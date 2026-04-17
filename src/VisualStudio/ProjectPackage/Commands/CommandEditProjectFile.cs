using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{

    [Command(PackageIds.idEditProjectFile)]
    internal sealed class CommandEditProjectFile : BaseCommand<CommandEditProjectFile>
    {
        static Dictionary<string, string> tempProjectFiles; // key is the project file, value is the temp file
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

                    tempProjectFiles.Remove(item.Key);
                    VS.Events.DocumentEvents.Closed -= DocumentEvents_Closed;
                    VS.Events.DocumentEvents.Saved -= DocumentEvents_Saved;
                    File.Delete(item.Value);
                    break;

                }
            }
        }
        static FieldInfo field = null;
        static Guid guidXmlEditor = new Guid("FA3CD31E-987B-443A-9B81-186104E8DAC1");
        static Guid guidProjectDesigner = new Guid("04b8ab82-a572-4fef-95ce-5222444b6b64");
        internal static void CloseProjectEditWindows()
        {
            var toClose = new List<WindowFrame>();
            if (field == null)
            {
                field = typeof(WindowFrame).GetField("_frame", BindingFlags.Instance | BindingFlags.NonPublic);
            }
            if (field == null)
                return;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var documents = await VS.Windows.GetAllDocumentWindowsAsync();
                foreach (var doc in documents.ToArray())
                {
                    string docName = "";
                    if (doc.Editor == guidProjectDesigner)
                    {
                        dynamic _frame = field.GetValue(doc);
                        try
                        {
                            docName = _frame.EffectiveDocumentMoniker;
                        }
                        catch (Exception)
                        {
                            docName = "";
                        }
                        if (XSharpModel.XProject.IsXSharpProject(docName))

                            toClose.Add(doc);
                    }
                    if (doc.Editor == guidXmlEditor)
                    {
                        dynamic _frame = field.GetValue(doc);
                        try
                        {
                            docName = _frame.EffectiveDocumentMoniker;
                        }
                        catch (Exception)
                        {
                            docName = "";
                        }
                        if (!string.IsNullOrEmpty(docName))
                        {
                            if (tempProjectFiles.Values.Contains(docName))
                            {
                                toClose.Add(doc);
                            }
                        }
                    }
                }
                foreach (var frame in toClose)
                {
                    await frame.CloseFrameAsync(FrameCloseOption.SaveIfDirty);
                }
            });

            return;
        }


    }
}
