//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Removes each shadow companion project from the solution and deletes its folder when
    /// the solution closes. Leaving stale companion projects/folders around between sessions
    /// has been a real, confusing source of test failures in this feature's own development
    /// (half-regenerated files, leftover references from an earlier attempt). Regeneration
    /// is already proven idempotent -- ShadowDesignerBridge.TryOpen recreates the companion
    /// project on demand -- so cleaning it up on close is safe: nothing is lost, it's purely
    /// auto-generated output.
    ///
    /// Subclasses the existing Microsoft.VisualStudio.Project.SolutionListener base
    /// (ProjectBase\SolutionListener.cs) rather than implementing IVsSolutionEvents by hand
    /// -- it already provides correct advise/unadvise lifecycle and E_NOTIMPL defaults for
    /// every method in the IVsSolutionEvents family.
    ///
    /// TWO separate hooks are needed, not one, because of a real ordering bug found in
    /// testing: removing the project from the solution has to happen BEFORE VS's own
    /// "save changes?" prompt writes the .sln (otherwise the saved .sln still references a
    /// project whose folder gets deleted moments later, leaving a dangling reference the
    /// user hits on next open) -- OnQueryCloseSolution is the earliest close-related event,
    /// firing before that save. Deleting the folder itself still has to wait until
    /// OnAfterCloseSolution, by which point every project (including the companion) has
    /// fully unloaded, so there's no risk of deleting files still locked by an active
    /// project node.
    /// </summary>
    internal sealed class ShadowDesignerCleanup : SolutionListener
    {
        private static readonly object _lock = new object();
        private static ShadowDesignerCleanup _instance;

        // Companion .csproj full paths seen this session -- removed from the solution (in
        // OnQueryCloseSolution) and then deleted from disk (in OnAfterCloseSolution).
        private static readonly HashSet<string> _companionCsprojPaths =
            new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        private readonly IVsSolution _vsSolution;

        private ShadowDesignerCleanup(IServiceProvider serviceProvider) : base(serviceProvider)
        {
            var solutionService = serviceProvider.GetService(typeof(SVsSolution));
            if (solutionService == null)
            {
                throw new InvalidOperationException("Could not obtain the IVsSolution service.");
            }
            _vsSolution = (IVsSolution)solutionService;
            Init();
        }

        /// <summary>
        /// Registers a companion project for cleanup on solution close, lazily advising
        /// solution events the first time this is called. Safe to call every time
        /// ShadowDesignerBridge.TryOpen succeeds -- idempotent (HashSet).
        /// </summary>
        public static void Track(string companionCsprojPath)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_instance == null)
            {
                lock (_lock)
                {
                    if (_instance == null)
                    {
                        var package = XSharpProjectPackage.XInstance;
                        if (package == null)
                        {
                            return;
                        }
                        _instance = new ShadowDesignerCleanup(package);
                    }
                }
            }
            _companionCsprojPaths.Add(companionCsprojPath);
        }

        public override int OnQueryCloseSolution(object reserved, ref int cancel)
        {
            if (_vsSolution != null)
            {
                foreach (string csprojPath in _companionCsprojPaths)
                {
                    try
                    {
                        // EnvDTE's Solution.Remove/ProjectItem.Remove both throw
                        // NullReferenceException on a project nested inside a Solution Folder
                        // when called from this particular solution-closing callback -- use
                        // the lower-level IVsSolution API instead (what VS's own "Remove"
                        // command uses internally), which handles nested/virtual projects
                        // correctly here.
                        CloseProjectElement(csprojPath);
                    }
                    catch
                    {
                        // Best-effort -- if removal fails here, the folder still gets deleted
                        // below on full close, which is the more visible half of this cleanup
                        // anyway; a stale .sln reference is a lesser, recoverable annoyance.
                    }
                }
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Finds the IVsHierarchy for the project at csprojPath (searching every project in
        /// the solution, including ones nested in Solution Folders) and asks IVsSolution to
        /// close/remove it directly -- bypasses the EnvDTE object model entirely, which is
        /// what makes this reliable for a nested project during OnQueryCloseSolution.
        /// </summary>
        private void CloseProjectElement(string csprojPath)
        {
            Guid enumFlags = Guid.Empty;
            int hr = _vsSolution.GetProjectEnum((uint)__VSENUMPROJFLAGS.EPF_ALLINSOLUTION, ref enumFlags, out IEnumHierarchies hierarchies);
            if (ErrorHandler.Failed(hr) || hierarchies == null)
            {
                return;
            }

            var buffer = new IVsHierarchy[1];
            while (hierarchies.Next(1, buffer, out uint fetched) == VSConstants.S_OK && fetched == 1)
            {
                var hierarchy = buffer[0];
                if (hierarchy is IVsProject vsProject &&
                    ErrorHandler.Succeeded(vsProject.GetMkDocument(VSConstants.VSITEMID_ROOT, out string mkDocument)) &&
                    string.Equals(mkDocument, csprojPath, StringComparison.OrdinalIgnoreCase))
                {
                    _vsSolution.CloseSolutionElement(0, hierarchy, 0);
                    return;
                }
            }
        }

        public override int OnAfterCloseSolution(object reserved)
        {
            // Confirmed via diagnostic logging (twice) that the folder is still locked well
            // beyond a ~1 second bounded retry at the moment this event fires ("The process
            // cannot access the file '...ShadowDesigner' because it is being used by another
            // process" -- the lock is on the directory handle itself, not a specific file
            // inside it), presumably the out-of-process Designer host taking longer than that
            // to fully exit. Rather than block VS's solution-close UI for the ~10+ seconds
            // that could take, retry on a background thread instead -- costs nothing (this is
            // best-effort cleanup either way) and gives the external process realistic time to
            // release its handle.
            foreach (string csprojPath in _companionCsprojPaths)
            {
                string dir = Path.GetDirectoryName(csprojPath);
                if (string.IsNullOrEmpty(dir))
                {
                    continue;
                }
                // Intentionally fire-and-forget: best-effort background cleanup, nothing to
                // await it against.
                _ = System.Threading.Tasks.Task.Run(() => DeleteWithRetry(dir));
            }
            _companionCsprojPaths.Clear();
            return VSConstants.S_OK;
        }

        private static void DeleteWithRetry(string dir)
        {
            const int maxAttempts = 20;
            const int delayMs = 1000;
            for (int attempt = 1; attempt <= maxAttempts; attempt++)
            {
                try
                {
                    if (!Directory.Exists(dir))
                    {
                        return;
                    }
                    Directory.Delete(dir, recursive: true);
                    ShadowDesignerBridge.DiagLog($"ShadowDesignerCleanup: deleted {dir} (attempt {attempt})");
                    return;
                }
                catch (Exception ex)
                {
                    if (attempt == maxAttempts)
                    {
                        ShadowDesignerBridge.DiagLog($"ShadowDesignerCleanup: FAILED to delete {dir} after {maxAttempts} attempts: {ex}");
                        // Best-effort cleanup only -- regeneration is idempotent, so a
                        // leftover folder just gets overwritten fresh next time, not a
                        // failure worth surfacing to the user.
                        return;
                    }
                    System.Threading.Thread.Sleep(delayMs);
                }
            }
        }
    }
}
