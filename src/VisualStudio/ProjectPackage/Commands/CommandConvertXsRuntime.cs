using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using Task = System.Threading.Tasks.Task;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Shell.Interop;
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
                var project = await VS.Solutions.GetActiveProjectAsync();
                var refs = project.References.Select(r => r.VsReference.FullPath);
                bool isVulcan = false;
                if (refs != null)
                {
                    foreach (var asmref in refs)
                    {
                        if (asmref.Contains("Vulcan"))
                        {
                            isVulcan = true;
                            break;
                        }
                    }
                }
                Command.Visible = isVulcan;
            }
        }
       
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var toDelete = new List<string>();
            var toAdd = new List<string>();
            var project = await VS.Solutions.GetActiveProjectAsync();
            var refs = project.References.Select(r => r.VsReference.FullPath);
            foreach (var asm in refs)
            {
                bool delete = true;
                var name = System.IO.Path.GetFileName(asm).ToLower();
                switch (name)
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
                    toDelete.Add(asm);
                }
            }
            if (toDelete.Count == 0 || toAdd.Count == 0)
            {
                await VS.MessageBox.ShowErrorAsync("No Vulcan assemblies found in the project");
                return;
            }
            // Delete Vulcan items from the project
            var referencesToDelete = new List<Reference>();
            foreach (var item in toDelete)
            {
                foreach (var node in project.References)
                {
                    if (string.Compare(node.VsReference.FullPath, item, StringComparison.OrdinalIgnoreCase) == 0)
                    {
                        referencesToDelete.Add(node);
                    }
                }
            }
            await project.References.RemoveAsync(referencesToDelete.ToArray());
            await project.References.AddAsync(toAdd.ToArray());
            await VS.MessageBox.ShowAsync("The project was converted to the X# runtime successfully. Please recompile and test.",
                icon: OLEMSGICON.OLEMSGICON_INFO,
                buttons : OLEMSGBUTTON.OLEMSGBUTTON_OK);
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
