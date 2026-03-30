using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Project;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;

using XSharp.Settings;

using XSharpModel;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{

    [Command(PackageIds.idEditProjectFile)]
    internal sealed class CommandEditProjectFile : BaseCommand<CommandEditProjectFile>
    {
        static Dictionary<string, string> tempProjectFiles;
        static string tempProjectDir;
        static CommandEditProjectFile()
        {
            tempProjectDir = Path.Combine(Path.GetTempPath(), "XSharp.Intellisense");
            Directory.CreateDirectory(tempProjectDir);
            tempProjectFiles = new Dictionary<string, string>();
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            var project = await VS.Solutions.GetActiveProjectAsync();
            VS.Events.DocumentEvents.Saved += DocumentEvents_Saved;
            VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
            VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;


            var projectNode = XSharpProjectNode.FindProject(project.FullPath);
            if (projectNode != null && projectNode.QueryEditProjectFile(true))
            {
                projectNode.BuildProject.Save();
                byte[] projectData = File.ReadAllBytes(project.FullPath);
                Directory.CreateDirectory(tempProjectDir);
                var fileName = Path.GetFileNameWithoutExtension(project.FullPath) + ".xml";
                var file = Path.Combine(tempProjectDir, fileName);
                File.WriteAllBytes(file, projectData);
                tempProjectFiles[project.FullPath] = file;
                await VS.Documents.OpenAsync(file);
            }

        }

        private void ShellEvents_ShutdownStarted()
        {
            foreach (var item in tempProjectFiles)
            {
                if (File.Exists(item.Value))
                {
                    File.Delete(item.Value);
                }
            }
        }

        private void DocumentEvents_Saved(string obj)
        {
            var text = File.ReadAllText(obj);
            foreach (var item in tempProjectFiles)
            {
                if (item.Value == obj)
                {
                    var project = XSharpProjectNode.FindProject(item.Key);
                    if (project != null)
                    {
                        File.WriteAllText(item.Key, text);
                        project.BuildProject.Xml.ReloadFrom(item.Key, false, null);
                        project.BuildProject.ReevaluateIfNecessary();

                    }
                }
            }

        }

        private void DocumentEvents_Closed(string obj)
        {
            foreach (var item in tempProjectFiles)
            {
                if ( item.Value == obj)
                {

                    //File.Delete(item.Value);
                    tempProjectFiles.Remove(item.Key);
                    VS.Events.DocumentEvents.Closed -= DocumentEvents_Closed;
                    VS.Events.DocumentEvents.Saved -= DocumentEvents_Saved;
                    break;

                }
            }
        }


    }
}
