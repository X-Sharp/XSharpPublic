using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;

using System;
using System.Collections.Concurrent;
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
        static ConcurrentDictionary<string, string> tempProjectFiles; // key is the project xmlFile, value is the temp xmlFile
        static string tempProjectDir;
        static volatile bool _eventsSubscribed = false;
        static CommandEditProjectFile()
        {
            tempProjectDir = Path.Combine(Path.GetTempPath(), "XSharp.Intellisense");
            Directory.CreateDirectory(tempProjectDir);
            tempProjectFiles = new ConcurrentDictionary<string, string>(StringComparer.OrdinalIgnoreCase);
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
            // Unsubscribe before subscribing to prevent duplicate registrations
            // if ExecuteAsync is called multiple times.
            if (!_eventsSubscribed)
            {
                VS.Events.DocumentEvents.Saved += DocumentEvents_Saved;
                VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
                _eventsSubscribed = true;
            }


            var projectNode = XSharpProjectNode.FindProject(project.FullPath);
            if (projectNode != null && projectNode.QueryEditProjectFile(true))
            {
#if DEV17
                projectNode.ClearSdkProjectReferences();
#endif
                projectNode.SetProjectFileDirty(false);
                Directory.CreateDirectory(tempProjectDir);
                var prjFile = project.FullPath;
                var fileName = Path.GetFileNameWithoutExtension(project.FullPath) + ".xml";
                var xmlFile = Path.Combine(tempProjectDir, fileName);
                projectNode.BuildProject.Xml.Save(xmlFile);
                
                projectNode.BuildProject.FullPath = prjFile;
                tempProjectFiles[prjFile] = xmlFile;
                await VS.Documents.OpenAsync(xmlFile);
                var open = await VS.Documents.IsOpenAsync(xmlFile);
                if (!open)
                {
                    await VS.Documents.OpenViaProjectAsync(xmlFile);
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
            // Find the matching key without modifying the collection during iteration.
            string keyToRemove = null;
            foreach (var item in tempProjectFiles)
            {
                if (item.Value == obj)
                {
                    keyToRemove = item.Key;
                    break;
                }
            }
            if (keyToRemove != null && tempProjectFiles.TryRemove(keyToRemove, out _))
            {
                File.Delete(obj);
                // VS document events and commands all execute on the UI thread, so no other
                // thread can insert a new entry between TryRemove and IsEmpty.
                if (tempProjectFiles.IsEmpty)
                {
                    VS.Events.DocumentEvents.Closed -= DocumentEvents_Closed;
                    VS.Events.DocumentEvents.Saved -= DocumentEvents_Saved;
                    _eventsSubscribed = false;
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
