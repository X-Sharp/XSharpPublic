//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.LanguageServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using XSharp.LanguageService.Commands;
using XSharp.LanguageService.FindReferences;
using XSharp.LanguageService.GoToImplementation;
using XSharp.Settings;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.ForeignSource
{
    /// <summary>
    /// The source of a symbol that X# read from the output DLL of a C# or VB project of the solution, found through
    /// VS's Roslyn workspace. Any failure gives no locations: the callers then show the generated header as before.
    /// </summary>
    internal static class ForeignNavigator
    {
        private const int TimeoutMilliseconds = 3000;

        /// <summary>The declarations of <paramref name="element"/> (the type itself or one of its members). UI thread.</summary>
        internal static IReadOnlyList<ForeignLocation> Find(XPETypeSymbol type, XPESymbol element)
        {
            var none = Array.Empty<ForeignLocation>();
            try
            {
                if (type == null || element == null)
                    return none;
                var key = ForeignSymbolKey.From(type, element);
                var assembly = (element as XPEMemberSymbol)?.IsExtension == true
                    ? ((XPEMemberSymbol)element).DeclaringTypeSym?.Assembly ?? type.Assembly
                    : type.Assembly;
                var projectFile = ForeignProjects.FindProjectFile(assembly?.FileName);
                if (projectFile == null)
                    return none;
                var found = FindInWorkspace(projectFile, key);
                XSharpGotoDefinition.WriteOutputMessage($"ForeignNavigator: {projectFile} {key.MemberDocId ?? key.TypeDocId} -> {found.Count} location(s)");
                return found;
            }
            catch (Exception ex)
            {
                // also a Roslyn assembly that cannot be loaded (an older VS): it fails when FindInWorkspace is compiled
                Logger.Exception(ex, "ForeignNavigator.Find failed");
                return none;
            }
        }

        /// <summary>
        /// All Roslyn work lives here, not in <see cref="Find"/>: when a Roslyn assembly cannot be loaded, the JIT fails
        /// while compiling this method, which happens inside Find's try, and F12 falls back to the generated header.
        /// </summary>
        [MethodImpl(MethodImplOptions.NoInlining)]
        private static IReadOnlyList<ForeignLocation> FindInWorkspace(string projectFile, ForeignSymbolKey key)
        {
            var workspace = Workspace();
            if (workspace == null)
                return Array.Empty<ForeignLocation>();
            var projects = workspace.CurrentSolution.Projects
                .Where(p => string.Equals(p.FilePath, projectFile, StringComparison.OrdinalIgnoreCase)).ToList();
            if (projects.Count == 0)
                return Array.Empty<ForeignLocation>();
            var compilations = ThreadHelper.JoinableTaskFactory.Run(() => CompilationsAsync(projects));
            // generated sources (source generators) have virtual paths that cannot be opened
            return ForeignSymbolResolver.Resolve(compilations, key, File.Exists);
        }

        /// <summary>Opens the declaration in its editor (the C# or VB editor).</summary>
        internal static void Open(ForeignLocation location)
        {
            XDocuments.Open(location.FilePath, location.StartLine, location.StartColumn, false);
        }

        /// <summary>Several declarations (partial classes): the Find All References list, as Go To Implementation shows them.</summary>
        internal static void Present(IReadOnlyList<ForeignLocation> locations, IXSymbol target, string title)
        {
            ThreadHelper.JoinableTaskFactory.RunAsync(async () =>
            {
                try
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    var hits = locations.Select(l => DeclarationHit.Create(l.FilePath, ProjectName(l.FilePath), l.StartLine, l.StartColumn,
                        LineText(l.FilePath, l.StartLine), l.Name, l.ContainerName)).ToList();
                    var service = await VS.GetRequiredServiceAsync<SVsFindAllReferences, IFindAllReferencesService>();
                    var window = service.StartSearch(title);
                    var source = new ReferencesTableDataSource(target, await FindReferencesCommand.CreateTextFormatAsync());
                    window.Manager.AddSource(source, ReferencesTableDataSource.Columns);
                    source.Add(hits);
                    source.Complete();
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "ForeignNavigator.Present failed");
                }
            }).FileAndForget("XSharp/ForeignNavigator/Present");
        }

        private static VisualStudioWorkspace Workspace()
        {
            var componentModel = ServiceProvider.GlobalProvider.GetService(typeof(SComponentModel)) as IComponentModel;
            return componentModel?.GetService<VisualStudioWorkspace>();
        }

        private static async Task<IReadOnlyList<Compilation>> CompilationsAsync(IList<Microsoft.CodeAnalysis.Project> projects)
        {
            using (var cts = new CancellationTokenSource(TimeoutMilliseconds))
            {
                var tasks = projects.Select(p => p.GetCompilationAsync(cts.Token)).ToList();
                var all = Task.WhenAll(tasks);
                await Task.WhenAny(all, Task.Delay(TimeoutMilliseconds)).ConfigureAwait(false);
                return tasks.Where(t => t.Status == TaskStatus.RanToCompletion && t.Result != null).Select(t => t.Result).ToList();
            }
        }

        private static string LineText(string filePath, int line)
        {
            try
            {
                return File.ReadLines(filePath).Skip(line).FirstOrDefault() ?? "";
            }
            catch (Exception)
            {
                return "";
            }
        }

        private static string ProjectName(string filePath)
        {
            var workspace = Workspace();
            var documentId = workspace?.CurrentSolution.GetDocumentIdsWithFilePath(filePath).FirstOrDefault();
            return documentId == null ? "" : workspace.CurrentSolution.GetProject(documentId.ProjectId)?.Name ?? "";
        }
    }
}
