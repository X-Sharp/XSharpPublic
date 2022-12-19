using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using System;
using XSharpModel;
using System.Collections;
using Task = System.Threading.Tasks.Task;
using System.Collections.Generic;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    [Command(PackageIds.idConvertXSharpRuntime)]
    internal sealed class CommandConvertXsRuntime : BaseCommand<CommandConvertXsRuntime>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }

        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
            if (Command.Visible)
            {
                var refs = await GetAssemblyReferencesAsync();
                bool isVulcan = false;
                if (refs != null)
                {
                    foreach (var asmref in refs)
                    {
                        if (asmref.FullName.Contains("Vulcan"))
                        {
                            isVulcan = true;
                            break;
                        }
                    }
                }
                Command.Visible = isVulcan;
            }
        }
        async System.Threading.Tasks.Task<List<XAssembly>> GetAssemblyReferencesAsync()
        {
            var project = await VS.Solutions.GetActiveProjectAsync();
            var path = project.FullPath;
            var xsproject = XSolution.FindProject(path);
            if (xsproject != null)
            {
                return xsproject.AssemblyReferences;
            }
            return null;
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var toDelete = new List<string>();
            var toAdd = new List<string>();
            foreach (var asm in await GetAssemblyReferencesAsync())
            {
                bool delete = true;
                switch (System.IO.Path.GetFileName(asm.FileName).ToLower())
                {
                    case VulcanRT:
                        toAdd.Add(XSharpCore);
                        break;
                    case VulcanRTFuncs:
                        toAdd.Add(XSharpRT);
                        toAdd.Add(XSharpVO);
                        toAdd.Add(XSharpRDD);
                        toAdd.Add(XSharpMacroCompiler);
                        break;
                    case VulcanVoSystem:
                        toAdd.Add(VoSystem);
                        break;
                    case VulcanVoConsole:
                        toAdd.Add(VoConsole);
                        break;
                    case VulcanVoRdd:
                        toAdd.Add(VoRdd);
                        break;
                    case VulcanVoSql:
                        toAdd.Add(VoSql);
                        break;
                    case VulcanVoGui:
                        toAdd.Add(VoGui);
                        break;
                    case VulcanVoInet:
                        toAdd.Add(VoInet);
                        break;
                    case VulcanVoWin32:
                        toAdd.Add(VoWin32);
                        break;
                    case VulcanVoReport:
                        toAdd.Add(VoReport);
                        break;
                    default:
                        delete = false;
                        break;
                }
                if (delete)
                {
                    toDelete.Add(asm.FileName);
                }
            }
            if (toDelete.Count == 0 || toAdd.Count == 0)
            {
                await VS.MessageBox.ShowErrorAsync("No Vulcan assemblies found in the project");
                return;
            }
            var project = await VS.Solutions.GetActiveProjectAsync();
            var path = project.FullPath;
            var xsproject = XSolution.FindProject(path);
            var projectNode = (XSharpProjectNode)xsproject.ProjectNode;
            var refContainer = projectNode.GetReferenceContainer() as XSharpReferenceContainerNode;
            // Delete Vulcan items from the project
            foreach (var item in toDelete)
            {
                
                var refnode = refContainer.FindChild(item);
                if (refnode != null)
                {
                    refnode.Remove(false);
                    xsproject.RemoveAssemblyReference(item);
                }
            }
            // Add X# items to the project. Make sure to not add items that already exist.
            foreach (var item in toAdd)
            {
                var refnode = refContainer.FindChild(item);
                if (refnode == null)
                {
                    refnode = new XSharpAssemblyReferenceNode(projectNode, item);
                    refContainer.AddChild(refnode);
                }
            }
            // Let MsBuild figure out where the assemblies are
            projectNode.Build(MsBuildTarget.ResolveAssemblyReferences);
        }
        #region Assembly names
        internal const string VulcanRT = "vulcanrt.dll";
        internal const string VulcanRTFuncs = "vulcanrtfuncs.dll";
        internal const string VulcanVoGui = "vulcanvoguiclasses.dll";
        internal const string VulcanVoSystem = "vulcanvosystemclasses.dll";
        internal const string VulcanVoRdd = "vulcanvorddclasses.dll";
        internal const string VulcanVoSql = "vulcanvosqlclasses.dll";
        internal const string VulcanVoConsole = "vulcanvoconsoleclasses.dll";
        internal const string VulcanVoWin32 = "vulcanvowin32apilibrary.dll";
        internal const string VulcanVoInet = "vulcanvointernetclasses.dll";
        internal const string VulcanVoReport = "vulcanvoreportclasses.dll";

        internal const string XSharpCore = "XSharp.Core";
        internal const string XSharpRT = "XSharp.RT";
        internal const string XSharpVO = "XSharp.VO";
        internal const string XSharpRDD = "XSharp.Rdd";
        internal const string XSharpMacroCompiler = "XSharp.MacroCompiler";
        internal const string VoGui = "VOGUIClasses";
        internal const string VoSystem = "VOSystemClasses";
        internal const string VoRdd = "VORDDClasses";
        internal const string VoSql = "VOSQLClasses";
        internal const string VoConsole = "VOConsoleClasses";
        internal const string VoWin32 = "VOWin32APILibrary";
        internal const string VoInet = "VOInternetClasses";
        internal const string VoReport = "VOReportClasses";
        #endregion Assembly names
    }
}
