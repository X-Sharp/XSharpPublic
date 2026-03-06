using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using XSharp.LanguageService;
using XSharp.Settings;

using XSharpModel;

using CVT=Community.VisualStudio.Toolkit;
namespace XSharp.Project
{
    internal class XSharpShellEvents
    {
        static ILogger Logger => XSettings.Logger;

        public int BuildEvents_SolutionBuildFinished { get; private set; }
        static bool hasEnvironmentvariable = false;
        static XSharpShellEvents()
        {
            hasEnvironmentvariable = !string.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
            if (!hasEnvironmentvariable)
            {
                VS.MessageBox.ShowWarning("The environment variable 'XSharpMsBuildDir' is missing. \rSome projects may have problems loading. \rPlease run the XSharp setup program again.");
            }
        }

        internal XSharpShellEvents()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnAfterBackgroundSolutionLoadComplete += SolutionEvents_OnAfterBackgroundSolutionLoadComplete;
                VS.Events.SolutionEvents.OnAfterMergeSolution += SolutionEvents_OnAfterMergeSolution;
                VS.Events.SolutionEvents.OnBeforeUnloadProject += SolutionEvents_OnBeforeUnloadProject;

                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.SolutionEvents.OnAfterLoadProject += SolutionEvents_OnAfterLoadProject;
                VS.Events.SolutionEvents.OnBeforeCloseProject += SolutionEvents_OnBeforeCloseProject;
                VS.Events.SolutionEvents.OnAfterRenameProject += SolutionEvents_OnAfterRenameProject;
                //VS.Events.SolutionEvents.OnBeforeUnloadProject += SolutionEvents_OnBeforeUnloadProject;

                //VS.Events.SolutionEvents.OnAfterOpenFolder += SolutionEvents_OnAfterOpenFolder;
                //VS.Events.SolutionEvents.OnBeforeCloseFolder += SolutionEvents_OnBeforeCloseFolder;

                VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
                //VS.Events.DocumentEvents.Opened += DocumentEvents_Opened;
                //VS.Events.DocumentEvents.Saved += DocumentEvents_Saved;
                //VS.Events.DocumentEvents.BeforeDocumentWindowShow += DocumentEvents_BeforeDocumentWindowShow;
                //VS.Events.DocumentEvents.AfterDocumentWindowHide += DocumentEvents_AfterDocumentWindowHide;

                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                //VS.Events.ShellEvents.EnvironmentColorChanged   += ShellEvents_EnvironmentColorChanged;
                //VS.Events.ShellEvents.ShellAvailable += ShellEvents_ShellAvailable;

                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;

                VS.Events.BuildEvents.SolutionConfigurationChanged += BuildEvents_SolutionConfigurationChanged;
                VS.Events.BuildEvents.ProjectConfigurationChanged += BuildEvents_ProjectConfigurationChanged; ;
                VS.Events.BuildEvents.ProjectBuildDone += BuildEvents_ProjectBuildDone;
                VS.Events.BuildEvents.ProjectBuildStarted += BuildEvents_ProjectBuildStarted;
                VS.Events.BuildEvents.ProjectCleanDone += BuildEvents_ProjectCleanDone;
                VS.Events.BuildEvents.ProjectCleanStarted += BuildEvents_ProjectCleanStarted;


                _ = await VS.Commands.InterceptAsync(KnownCommands.File_CloseSolution, CloseDesignerWindows);
                _ = await VS.Commands.InterceptAsync(KnownCommands.File_Exit, CloseDesignerWindows);
                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                if (sol is Solution)
                {
                    SolutionEvents_OnAfterOpenSolution(sol);
                }

            });

        }



        #region Build Events
        private void BuildEvents_ProjectCleanStarted(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Clean started: " + project.Name);
        }

        private void BuildEvents_ProjectCleanDone1(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Build done: " + args.Project.Name + " " + args.IsSuccessful);
        }

        private void BuildEvents_ProjectCleanDone(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Clean done: " + args.Project.Name + " " + args.IsSuccessful); ;
        }

        private void BuildEvents_ProjectBuildStarted(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Build Started: " + project.Name);
        }

        private void BuildEvents_ProjectBuildDone(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Build Done: " + args.Project.Name + " " + args.IsSuccessful);
        }

        private void BuildEvents_ProjectConfigurationChanged(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Configuration changed: " + project.Name);
        }

        private void BuildEvents_SolutionConfigurationChanged()
        {
            Logger.SingleLine();
            Logger.Information("Solution Configuration changed");
        }
        #endregion
        private void DocumentEvents_Opened(string doc)
        {
            Logger.Information("Opened document: " + doc ?? "");
        }

        #region Solution Events
        string solutionName = "";
        private void SolutionEvents_OnAfterMergeSolution()
        {
            Logger.SingleLine();
            Logger.Information("After Merge solution");
        }
        private void SolutionEvents_OnBeforeOpenSolution(string solutionFileName)
        {
            Logger.SingleLine();
            Logger.Information("Opening Solution: " + solutionFileName ?? "");
            Logger.SingleLine();
            solutionName = solutionFileName;
            if (!string.IsNullOrEmpty(solutionName) && File.Exists(solutionName))
            {
                XSolution.Open(solutionName);
            }
        }

        private void SolutionEvents_OnAfterOpenSolution(Solution solution)
        {
            Logger.SingleLine();
            Logger.Information("Opened Solution: " + solution?.FullPath ?? "");
            Logger.SingleLine();
            solutionName = solution?.FullPath;
            foreach (var project in XSharpProjectNode.AllProjects)
            {
                if (project.HasIncompleteReferences)
                {
                    project.FixReferences();
                }
            }
            XSolution.AfterOpen();
        }
        private void SolutionEvents_OnAfterCloseSolution()
        {
            XSolution.IsClosing = false;
            Logger.SingleLine();
            Logger.Information("Closed solution: " + solutionName);
            Logger.SingleLine();
            solutionName = "";
            XSharpXMLDocTools.Close();
        }

        private void SolutionEvents_OnBeforeCloseSolution()
        {
            XSolution.IsClosing = true;
            XSolution.Close();
            // close OUR documents that are opened in design mode.
            if (!XSolution.HasProjects)
            {
                return;
            }
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var frames = await VS.Windows.GetAllDocumentWindowsAsync();
                if (frames != null)
                {
                    foreach (var frame in frames.ToList())
                    {
                        if (frame.Caption.EndsWith("]"))
                        {
                            // no need to save here. VS has shown a dialog with the dirty files already
                            await frame.CloseFrameAsync(FrameCloseOption.NoSave);
                        }
                    }
                }
            });

            Logger.SingleLine();
            Logger.Information("Closing solution: " + solutionName);
            Logger.SingleLine();
#if !DEV17
            XSharpProjectFactory.InvalidProjectFiles.Clear();
#endif
        }

        private void SolutionEvents_OnAfterBackgroundSolutionLoadComplete()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            RestoreDesignerWindows();
            RestoreStartupProject();
        }

        #endregion
        #region Project Events
        private void SolutionEvents_OnBeforeOpenProject(string projectFileName)
        {
            Logger.SingleLine();
            var ext = Path.GetExtension(projectFileName);
            if (!string.IsNullOrEmpty(ext))
                Logger.Information("Opening project: " + projectFileName ?? "");
            else
                Logger.Information("Opening folder: " + projectFileName ?? "");
            Logger.SingleLine();
            if (IsXSharpProject(projectFileName))
            {
                checkProjectFile(projectFileName);
            }
        }
        private void SolutionEvents_OnAfterOpenProject(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Opened project: " + project.FullPath ?? "");
            Logger.SingleLine();
        }
        private void SolutionEvents_OnAfterLoadProject(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Loaded project: " + project.FullPath ?? "");
            Logger.SingleLine();
        }

        private void SolutionEvents_OnBeforeUnloadProject(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Unloading project: " + project.FullPath ?? "");
            Logger.SingleLine();
        }

        private void SolutionEvents_OnBeforeCloseProject(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Closing project: " + project.FullPath ?? "");
            Logger.SingleLine();
        }

        private void SolutionEvents_OnAfterRenameProject(CVT.Project project)
        {
            Logger.SingleLine();
            Logger.Information("Renamed project: " + project?.FullPath ?? "");
            Logger.SingleLine();
        }
        bool IsXSharpProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            return string.Equals(Path.GetExtension(fileName), ".xsproj", StringComparison.OrdinalIgnoreCase);
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        const string MsTestGuid = @"{3AC096D0-A1C2-E12C-1390-A8335801FDAB};";

        private void checkProjectFile(string fileName)
        {
            if (fileName != null && fileName.ToLower().EndsWith("xsproj") && File.Exists(fileName))
            {
                string xml = File.ReadAllText(fileName);
                bool oldProject = xml.IndexOf("<Project Sdk", StringComparison.OrdinalIgnoreCase) == -1;
                // In VS 2022 and earlier we need to fix the casing of the new text
                if (oldProject)
                {
                    var original = Path.ChangeExtension(fileName, ".original");
                    bool changed = false;
                    if (hasEnvironmentvariable)
                    {
                        var pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                        if (pos >= 0)
                        {
                            while (pos > 0)
                            {
                                xml = xml.Substring(0, pos) + newText + xml.Substring(pos + oldText.Length);
                                pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                            }
                            DeleteFileSafe(original);
                            File.Copy(fileName, original);
                            DeleteFileSafe(fileName);
                            File.WriteAllText(fileName, xml);
                            changed = true;
                        }
                    }
                    var testpos = xml.IndexOf(MsTestGuid, StringComparison.OrdinalIgnoreCase);
                    if (testpos >= 0)
                    {
                        var left = xml.Substring(0, testpos);
                        var right = xml.Substring(testpos + MsTestGuid.Length);
                        if (!changed)
                        {
                            DeleteFileSafe(original);
                            File.Copy(fileName, original);
                        }
                        xml = left + right;
                        DeleteFileSafe(fileName);
                        File.WriteAllText(fileName, xml);
                        changed = true;
                    }
                    if (changed)
                    {
                        Logger.SingleLine();
                        Logger.Information("==> Project must be upgraded: " + fileName);
                        Logger.SingleLine();

                        XSharpProjectNode.ChangedProjectFiles.Add(fileName, original);
                    }
                }
#if !DEV17
                else
                {
                    XSharpProjectFactory.InvalidProjectFiles.Add(fileName.ToLower());
                    return;
                }
#endif

            }
        }

        public static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (File.Exists(fileName))
                {
                    File.SetAttributes(fileName, FileAttributes.Normal);
                    File.Delete(fileName);

                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "DeleteFileSafe");
                return false;
            }
            return true;

        }
        #endregion


        #region Save / Restore Windows
        private void RestoreStartupProject()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (!XSolution.HasProjects)
                return;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var projects = XDatabase.GetStartupProjects();
                if (projects.Count > 0)
                {
                    var allProjects = await VS.Solutions.GetAllProjectsAsync();
                    foreach (var projPath in projects)
                    {
                        var proj = allProjects.Where(p => string.Equals(p.FullPath, projPath, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
                        if (proj != null)
                        {
                            var dte = (EnvDTE.DTE)await VS.GetRequiredServiceAsync<EnvDTE.DTE, EnvDTE.DTE>();
                            dte.Solution.SolutionBuild.StartupProjects = proj.FullPath;
                            Logger.SingleLine();
                            Logger.Information("Restored Startup Project: " + proj.FullPath);
                            Logger.SingleLine();
                        }
                    }
                }
            });
        }
        private CommandProgression CloseDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            SaveDesignerWindows();
            return CommandProgression.Continue;
        }

        private void SaveDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            SaveActiveStartupProject();
            var files = GetAllDesignerWindows(false);
            XDatabase.SaveOpenDesignerFiles(files);
        }
        private void CloseAllDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetAllDesignerWindows(true);
        }
        private List<string> GetAllDesignerWindows(bool close = false)
        {
            var files = new List<string>();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var documents = await VS.Windows.GetAllDocumentWindowsAsync();
                foreach (var doc in documents.ToArray())
                {
                    var caption = doc.Caption;
                    if (caption.EndsWith("]"))
                    {
                        string docName = "";
                        // use reflection to get the _frame field which has the full URL to the file
                        // and please note that we do not check for "[Design]" because that can (will)
                        // be translated in localized versions of VS, for example [Entwurf] in German.
                        var field = doc.GetType().GetField("_frame", BindingFlags.Instance | BindingFlags.NonPublic);
                        if (field != null)
                        {
                            dynamic _frame = field.GetValue(doc);
                            try
                            {
                                docName = _frame.EffectiveDocumentMoniker;
                            }
                            catch (Exception)
                            {
                                ;
                            }
                            if (string.IsNullOrEmpty(docName))
                            {
                                try
                                {
                                    docName = _frame.DocumentMoniker;
                                }
                                catch (Exception)
                                {
                                    ;
                                }
                            }
                            if (string.IsNullOrEmpty(docName))
                            {
                                continue;
                            }

                            var type = XFileTypeHelpers.GetFileType(docName);
                            if (type != XFileType.SourceCode)
                                continue;
                            string capt = _frame.EditorCaption;
                            if (capt == null || !capt.EndsWith("]"))
                                continue;

                        }

                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
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
            return files;
        }
        private void RestoreDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (!XSolution.HasProjects)
                return;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                Logger.SingleLine();
                Logger.Information("Start restoring windows in [Design] mode");
                Logger.SingleLine();
                var files = XDatabase.GetOpenDesignerFiles();
                if (files.Count > 0)
                {
                    CloseAllDesignerWindows();
                }
                var selection = await VS.Solutions.GetActiveItemsAsync();
                if (files.Count > 0)
                {
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
        #endregion
        #region Save/Restore Startup Project
        private void SaveActiveStartupProject()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var project = await GetStartupProjectAsync();
                if (project != null)
                {
                    Logger.SingleLine();
                    Logger.Information("Active Startup Project: " + project.FullPath);
                    Logger.SingleLine();
                    var prj = new List<string> { project.FullPath };
                    XDatabase.SaveStartuprojects(prj);
                }
            });
        }
        internal static async Task<CVT.Project> GetStartupProjectAsync()
        {
            EnvDTE.DTE dte = null;
            EnvDTE100.Solution4 sol4 = null;
            EnvDTE.SolutionBuild build = null;
            object startupprojects = null;
            await ThreadHelper.JoinableTaskFactory.RunAsync(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                dte = await VS.GetRequiredServiceAsync<EnvDTE.DTE, EnvDTE.DTE>();
                if (dte != null)
                    sol4 = dte.Solution as EnvDTE100.Solution4;
                if (sol4 != null)
                    build = sol4.SolutionBuild;
                if (build != null)
                {
                    startupprojects = build.StartupProjects;
                }

            });

            if (startupprojects != null)
            {
                var projects = await VS.Solutions.GetAllProjectsAsync();
                var projectList = startupprojects as Array;
                foreach (string prjName in projectList)
                {
                    string prjFileName = Path.GetFileName(prjName);
                    foreach (var prj in projects)
                    {
                        var fileName = Path.GetFileName(prj.FullPath);
                        if (fileName.Equals(prjFileName, StringComparison.OrdinalIgnoreCase))
                        {
                            return prj;
                        }
                    }
                }
            }
            return null;

        }
        #endregion
        #region DocumentEvents
        private void DocumentEvents_Closed(string document)
        {
            // Remove document from OrphanedFilesProject
            // So it can be opened in normal project afterwards
            // when possible
            if (!IsXSharpProject(document))
            {
                Logger.Information("Languageservice.DocumentEvents_Closed " + document ?? "(none)");
                var xfile = XSolution.FindFile(document);
                if (xfile != null && xfile.Project.Name == OrphanedFilesProject.OrphanName)
                {
                    XSolution.OrphanedFilesProject.RemoveFile(document);
                }
            }
        }

        #endregion

        #region Shell Events
        private void ShellEvents_ShutdownStarted()
        {
            XSolution.IsClosing = true;
            XSolution.IsShuttingDown = true;
            XSolution.Close();

            Logger.SingleLine();
            Logger.Information("Shutdown VS");
            Logger.SingleLine();
        }
        #endregion


        #region Build Events
        public bool IsBuilding => building;
        bool building;
        private void BuildEvents_SolutionBuildStarted(object sender, EventArgs e)
        {
            building = true;
        }
        private void BuildEvents_SolutionBuildDone(bool result)
        {
            building = false;
        }
        private void BuildEvents_SolutionBuildCancelled()
        {
            building = false;
        }
        #endregion

    }
}
