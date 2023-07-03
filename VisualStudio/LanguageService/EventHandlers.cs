//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using XSharpModel;


namespace XSharp.LanguageService
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    public class ModelScannerEvents 
    {
        private static ILogger Logger => XSolution.Logger;
        static string solutionName = "";
        static bool solutionHasXSharpProjects = false;
        static ModelScannerEvents()
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnAfterBackgroundSolutionLoadComplete += SolutionEvents_OnAfterBackgroundSolutionLoadComplete;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.SolutionEvents.OnBeforeCloseProject += SolutionEvents_OnBeforeCloseProject;
                VS.Events.SolutionEvents.OnAfterRenameProject += SolutionEvents_OnAfterRenameProject;

                VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;


                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;
                VS.Events.BuildEvents.SolutionConfigurationChanged += BuildEvents_SolutionConfigurationChanged;
                VS.Events.BuildEvents.ProjectBuildStarted += BuildEvents_ProjectBuildStarted;
                VS.Events.BuildEvents.ProjectBuildDone += BuildEvents_ProjectBuildDone;
                VS.Events.BuildEvents.ProjectCleanStarted += BuildEvents_ProjectCleanStarted;
                VS.Events.BuildEvents.ProjectCleanDone += BuildEvents_ProjectCleanDone;
                VS.Events.BuildEvents.ProjectConfigurationChanged += BuildEvents_ProjectConfigurationChanged;

                _ = await VS.Commands.InterceptAsync(KnownCommands.File_CloseSolution, CloseDesignerWindows);
                _ = await VS.Commands.InterceptAsync(KnownCommands.File_Exit, CloseDesignerWindows);

                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                if (sol is Solution)
                {
                    SolutionEvents_OnAfterOpenSolution(sol);
                }
            });
        }

        private static void SolutionEvents_OnBeforeOpenProject(string obj)
        {
            if (IsXSharpProject(obj))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"SolutionEvents_OnBeforeOpenProject {obj}");
            }
        }

        private static void BuildEvents_ProjectConfigurationChanged(Project obj)
        {
            if (IsXSharpProject(obj.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"BuildEvents_ProjectConfigurationChanged {obj.Name}");
            }
        }

        private static void BuildEvents_SolutionConfigurationChanged()
        {
            if (solutionHasXSharpProjects)
            {
                Logger.Information("BuildEvents_SolutionConfigurationChanged");
            }
        }

        private static void BuildEvents_ProjectCleanDone(ProjectBuildDoneEventArgs obj)
        {
            if (IsXSharpProject(obj.Project.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"BuildEvents_ProjectCleanDone {obj.Project.Name} {obj.IsSuccessful}");
            }
        }

        private static void BuildEvents_ProjectCleanStarted(Project obj)
        {
            if (IsXSharpProject(obj.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"BuildEvents_ProjectCleanStarted {obj.Name}");
            }
        }

        private static void BuildEvents_ProjectBuildDone(ProjectBuildDoneEventArgs obj)
        {
            if (IsXSharpProject(obj.Project.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"BuildEvents_ProjectBuildDone {obj.Project.Name} {obj.IsSuccessful}");
            }
        }

        private static void BuildEvents_ProjectBuildStarted(Project obj)
        {
            if (IsXSharpProject(obj.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.Information($"BuildEvents_ProjectBuildStarted {obj.Name}");
            }
        }

        private static void BuildEvents_SolutionBuildCancelled()
        {
            Logger.Information("BuildEvents_SolutionBuildCancelled");
            // Start or Resume the model walker
            XSharpModel.ModelWalker.Start();
        }

        private static void BuildEvents_SolutionBuildDone(bool result)
        {
            if (solutionHasXSharpProjects)
            {
                XSolution.Logger.Information($"BuildEvents_SolutionBuildDone: {result}");
                // Start or Resume the model walker
                XSharpModel.ModelWalker.Start();
            }
        }

        private static void BuildEvents_SolutionBuildStarted(object sender, EventArgs e)
        {
            if (solutionHasXSharpProjects)
            {
                XSolution.Logger.Information("BuildEvents_SolutionBuildStarted");
                if (XSharpModel.ModelWalker.IsRunning)
                {
                    // Do not walk while building
                    XSharpModel.ModelWalker.Suspend();
                }
            }
        }

        public static void Start()
        {

        }
        private static CommandProgression CloseDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            SaveDesignerWindows();
            return CommandProgression.Continue;
        }

        static bool IsXSharpProject(string fileName)
        {
            return String.Equals(Path.GetExtension(fileName), ".xsproj",StringComparison.OrdinalIgnoreCase);
        }

        private static void SolutionEvents_OnAfterOpenProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                solutionHasXSharpProjects = true;
                XSolution.Logger.SingleLine();
                XSolution.Logger.Information("After Opening project: " + project?.FullPath ?? "");
                XSolution.Logger.SingleLine();
            }
        }

        private static void SolutionEvents_OnAfterRenameProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                solutionHasXSharpProjects = true;
                Logger.SingleLine();
                Logger.Information("Renamed project: " + project?.FullPath ?? "");
                Logger.SingleLine();
            }
        }

        private static void SolutionEvents_OnBeforeCloseProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Before Closing project: " + project?.FullPath ?? "");
                Logger.SingleLine();
            }
            SolutionItem parent = project.Parent;
            while (parent.Parent != null)
            {
                parent = parent.Parent;
            }
            solutionHasXSharpProjects = HasXSharpProjects(parent);
        }

        private static void ShellEvents_ShutdownStarted()
        {
            Logger.SingleLine();
            Logger.Information("Shutdown VS");
            Logger.SingleLine();
            XSolution.IsClosing = true;
            XSolution.IsShuttingDown = true;
        }


        private static void SolutionEvents_OnAfterBackgroundSolutionLoadComplete()
        {
            XSolution.WriteOutputMessage("SolutionEvents_OnAfterBackgroundSolutionLoadComplete");
            ThreadHelper.ThrowIfNotOnUIThread();
            RestoreDesignerWindows();
        }

        private static void SaveDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var files = GetAllDesignerWindows(false);
            XDatabase.SaveOpenDesignerFiles(files);
        }
        private static void CloseAllDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetAllDesignerWindows(true);
        }

        private static void RestoreDesignerWindows()
        {
            if (XSolution.Projects.Count == 0)
            {
                return;
            }
            if (XSolution.Projects.Count == 1 && XSolution.Projects.First() == XSolution.OrphanedFilesProject)
            {
                return;
            }

            ThreadHelper.ThrowIfNotOnUIThread();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                // Read open designerfiles from before
                var files = XDatabase.GetOpenDesignerFiles();
                var selection = await VS.Solutions.GetActiveItemsAsync();
                if (files.Count > 0)
                {
                    Logger.SingleLine();
                    Logger.Information("Start restoring windows in [Design] mode");
                    Logger.SingleLine();
                    CloseAllDesignerWindows();

                    foreach (var file in files)
                    {
                        try
                        {
                            Logger.SingleLine();
                            Logger.Information("Restoring " + file);
                            Logger.SingleLine();
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            VsShellUtilities.OpenDocument(ServiceProvider.GlobalProvider, file, VSConstants.LOGVIEWID_Designer, out _, out _, out _);

                            Logger.SingleLine();
                            Logger.Information("Restored " + file);
                            Logger.SingleLine();
                        }
                        catch (Exception e)
                        {
                            Logger.DoubleLine();
                            Logger.Exception(e, "Restoring [Design] mode windows");
                            Logger.DoubleLine();
                        }
                    }
                    var sel = selection.Where(s => s.Type == SolutionItemType.PhysicalFile).FirstOrDefault();
                    if (sel != null)
                    {
                        _ = await VS.Documents.OpenAsync(sel.FullPath);
                    }
                    Logger.SingleLine();
                    Logger.Information("End restoring windows in [Design] mode");
                    Logger.SingleLine();
                }
            });
        }

        private static List<string> GetAllDesignerWindows(bool close = false)
        {
            var files = new List<string>();
            Logger.Information("Start enumerating designer windows for X# source files");

            ThreadHelper.ThrowIfNotOnUIThread();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var activeDoc = await VS.Windows.GetCurrentWindowAsync();
                var documents = await VS.Windows.GetAllDocumentWindowsAsync();
                foreach (var doc in documents.ToArray())
                {
                    var caption = doc.Caption;
                    if (caption.EndsWith("]"))
                    {
                        string docName = "";
                        var field = doc.GetType().GetField("_frame", BindingFlags.Instance | BindingFlags.NonPublic);
                        if (field != null)
                        {
                            dynamic _frame = field.GetValue(doc);
                            docName = _frame.EffectiveDocumentMoniker;
                            var type = XFileTypeHelpers.GetFileType(docName);
                            if (type != XFileType.SourceCode)
                                continue;
                            string capt = _frame.EditorCaption;
                            if ( capt == null || !capt.EndsWith("]"))
                                continue;
                            
                        }
#if !DEV17
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
#endif
                        if (string.IsNullOrEmpty(docName) && doc is IVsWindowFrame frame)
                        {
                            frame.GetProperty((int)__VSFPROPID.VSFPROPID_pszMkDocument, out var objdocName);
                            if (objdocName is string fileName)
                            {
                                docName = fileName;
                            }
                        }
                        if (!string.IsNullOrEmpty(docName))
                        {
                            await doc.CloseFrameAsync(FrameCloseOption.NoSave);
                            files.Add(docName);
                        }
                    }
                }
            });
            Logger.Information("End enumerating designer windows");
            return files;
        }


        private static void DocumentEvents_Closed(string document)
        {
            // Remove document from OrphanedFilesProject
            // So it can be opened in normal project afterwards
            // when possible
            if (document != null)
            {
                var xfile = XSolution.FindFile(document);
                if (xfile != null && xfile.Project.Name == OrphanedFilesProject.OrphanName)
                {
                    Logger.Information("DocumentEvents_Closed file in MiscFiles project " + document);
                    XSolution.OrphanedFilesProject.RemoveFile(document);
                }
            }
        }


        private static void SolutionEvents_OnBeforeOpenSolution(string solutionFileName)
        {
            XSharp.LanguageService.Logger.ActivateWhenNeeded();
            Logger.DoubleLine();
            Logger.Information("Opening Solution: " + solutionFileName ?? "");
            Logger.SingleLine();
            solutionName = solutionFileName;
        }



        private static bool HasXSharpProjects (SolutionItem obj)
        {
            foreach (var child in obj.Children)
            {
                if (child is Project && IsXSharpProject(child.FullPath))
                    return true;
                if (child is SolutionFolder)
                    return HasXSharpProjects(child);

            }
            return false;
        }

        /// <summary>
        /// Called at load time when solution has finished opening.
        /// </summary>
        /// <param name="pUnkReserved">reserved</param>
        /// <param name="fNewSolution">true if this is a new solution</param>
        /// <returns></returns>
        ///
        private static void SolutionEvents_OnAfterOpenSolution(SolutionItem obj)
        {
            solutionHasXSharpProjects = HasXSharpProjects(obj);
            if (! solutionHasXSharpProjects)
            {
                return;
            }
            string file = "";
            if (obj is Solution sol)
            {
                file = sol.FullPath;
                if (!string.IsNullOrEmpty(file))
                {
                    XSolution.Open(file);
                    solutionName = file;
                }

            }

            Logger.SingleLine();
            Logger.Information("Opened Solution: " + file);
            Logger.DoubleLine();

            ModelWalker.Suspend();
        }


        private static void SolutionEvents_OnBeforeCloseSolution()
        {
            Logger.SingleLine();
            Logger.Information("Closing solution: " + solutionName);
            Logger.SingleLine();
            XSharpXMLDocTools.Close();
            XSolution.IsClosing = true;
            XSolution.Close();
            // close OUR documents that are opened in design mode.
            return;
        }

        private static void SolutionEvents_OnAfterCloseSolution()
        {
            if (! String.IsNullOrEmpty(solutionName ))
            {
                Logger.DoubleLine();
                Logger.Information("Closed solution: " + solutionName);
                Logger.DoubleLine();
                solutionName = "";
            }
            XSolution.IsClosing = false;
        }

    }
}


