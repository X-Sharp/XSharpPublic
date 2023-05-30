using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Generic;
using System.IO;
using XSharpModel;
using static System.Windows.Forms.AxHost;
using File = System.IO.File;

namespace XSharp.LanguageService
{
    class XSharpGotoDefinition
    {

        internal static void Goto(IXSymbol element, ITextView TextView, CompletionState state)
        {
            if (state == CompletionState.Constructors && element is IXTypeSymbol xtype)
            {
                // when the cursor is before a "{" then goto the constructor and not the type
                var ctors = xtype.GetConstructors();
                if (ctors.Length > 0)
                {
                    element = ctors[0];
                }
            }

            if (element is XSourceEntity source)
            {
                source.OpenEditor();
            }
            else if (element is XPETypeSymbol petype)
            {
                GotoSystemType(TextView, petype, petype);

            }
            else if (element is XPEMemberSymbol pemember)
            {
                var petype2 = pemember.Parent as XPETypeSymbol;
                GotoSystemType(TextView, petype2, pemember);
            }
            else
            {
                VS.MessageBox.Show("Cannot navigate to the symbol under the cursor", "",
                    icon: OLEMSGICON.OLEMSGICON_INFO,
                    buttons: OLEMSGBUTTON.OLEMSGBUTTON_OK);
            }
        }
        internal static void GotoDefn(ITextView TextView)
        {
            try
            {
                var result = TextView.GetSymbolUnderCursor(out var state,out _, out _);
                //
                ThreadHelper.ThrowIfNotOnUIThread();
                if (result.Count > 0)
                {
                    Goto(result[0], TextView, state);
                    return;
                }
            }
            catch (Exception ex)
            {
                XSettings.LogException(ex, "Goto failed");
            }
            finally
            {
                ModelWalker.Resume();
            }
        }

        private static void DeleteFolderRecursively(DirectoryInfo directory, bool IncludeFiles)
        {
            // Scan all files in the current path
            if (IncludeFiles)
            {
                foreach (FileInfo file in directory.GetFiles())
                {
                    file.Attributes &= ~FileAttributes.ReadOnly;
                    file.Delete();
                }
            }

            DirectoryInfo[] subDirectories = directory.GetDirectories();

            // Scan the directories in the current directory and call this method
            // again to go one level into the directory tree
            foreach (DirectoryInfo subDirectory in subDirectories)
            {
                DeleteFolderRecursively(subDirectory, true);
                subDirectory.Attributes &= ~FileAttributes.ReadOnly;

                subDirectory.Delete();
            }
        }

        static string WorkFolder = null;
        static Stream Semaphore = null;
        const string semName = "XSharp.Busy";
        private static XAssembly asmName = null;
        private static string LookupXml(IXSymbol key)
        {
            return XSharpXMLDocMember.GetDoc(asmName, key);
        }

        internal static XSourceEntity FindSystemElement(XPETypeSymbol petype, XPESymbol element)
        {
            if (element is XPEMemberSymbol mem && mem.IsExtension)
            {
                petype = mem.DeclaringTypeSym;
            }
            var xFile = CreateFileForSystemType(petype, element);
            return FindElementInFile(xFile, petype, element);
        }

        private static void GotoSystemType(ITextView TextView, XPETypeSymbol petype, XPESymbol element)
        {
            var entity = FindSystemElement(petype, element);
            if (entity == null || entity.File == null)
                return;
            var xFile = entity.File;
            var file = TextView.TextBuffer.GetFile();
            // Copy references to the Orphan file project so type lookup works as expected
            var orphProject = XSolution.OrphanedFilesProject;
            var project = file.Project;
            if (project != orphProject)
            {
                orphProject.ClearAssemblyReferences();
                foreach (var asm in project.AssemblyReferences)
                {
                    orphProject.AddAssemblyReference(asm.FileName);
                }
            }
            // Note that the line numbers in the list are 0 based. OpenDocument also wants a 0 based line number
            XSettings.OpenDocument(xFile.FullPath, entity.Range.StartLine, 0, true);

        }

        private static XSourceEntity FindElementInFile(XFile file, XPETypeSymbol petype, XSymbol element)
        {
            var walker = new SourceWalker(file, false);
            walker.Parse(false);
            var entities = walker.EntityList;
            if (petype.IsFunctionsClass)
            {
                foreach (var entity in entities)
                {
                    if (entity.Name == element.Name)
                    {
                        return entity;
                    }
                }
            }
            else
            {
                foreach (var entity in entities)
                {
                    if (element is IXTypeSymbol && entity is IXTypeSymbol)
                        return entity;
                    if (entity.Name == element.Name)
                    {
                        if (entity is IXMemberSymbol m1 && m1.IsExtension && element is IXMemberSymbol m2)
                        {
                            if (m1.ParameterList == m2.ParameterList)
                                return entity;
                        }
                        if (entity.FullName == element.FullName)
                            return entity;
                        break;
                    }
                }
            }
            return null;
        }
        private static XFile CreateFileForSystemType(XPETypeSymbol petype, XPESymbol element)
        {
            asmName = petype.Assembly;
            bool mustCreate = false;
            if (Semaphore == null)
            {
                // we create a semaphore file in the workfolder to make sure that if 2 copies of VS are running
                // that we will not delete the files from the other copy
                var tempFolder = XSolution.TempFolder;
                var semFile = Path.Combine(tempFolder, semName);
                // clean up files from previous run
                if (Directory.Exists(tempFolder))
                {
                    if (File.Exists(semFile))
                    {
                        try
                        {
                            File.Delete(semFile);
                            DeleteFolderRecursively(new DirectoryInfo(tempFolder), false);
                        }
                        catch
                        {
                            // if deletion fails, other copy of VS is running, so do not delete the folder
                        }
                    }
                }
                if (!Directory.Exists(tempFolder))
                {
                    Directory.CreateDirectory(tempFolder);
                }
                WorkFolder = tempFolder;
                if (!File.Exists(semFile))
                    Semaphore = File.Create(semFile);
            }
            var ns = petype.Namespace + "." + petype.Assembly.Version;
            ns = petype.Assembly.DisplayName+"\\"+ns;
            var name = petype.Name;
            var nspath = Path.Combine(WorkFolder, ns);
            if (!Directory.Exists(nspath))
            {
                Directory.CreateDirectory(nspath);
            }
            var fileName = petype.Name;
            foreach (var c in Path.GetInvalidFileNameChars())
            {
                fileName = fileName.Replace(c, '_');
            }
            var temp = Path.Combine(nspath, fileName) + ".prg";
            var fi = new FileInfo(temp);
            mustCreate = !fi.Exists || fi.Length < 10;
            if (mustCreate)
            {
                VS.StatusBar.ShowMessageAsync("Generating reference source for " + petype.FullName).FireAndForget();
                VS.StatusBar.StartAnimationAsync(StatusAnimation.General).FireAndForget();
                var aLines = XClassCreator.Create(petype, LookupXml);
                File.WriteAllLines(temp, aLines, System.Text.Encoding.UTF8);
                File.SetAttributes(temp, FileAttributes.ReadOnly);
                VS.StatusBar.ClearAsync().FireAndForget();
                VS.StatusBar.EndAnimationAsync(StatusAnimation.General).FireAndForget();
            }
            var xFile = XSolution.AddOrphan(temp);
            return xFile;
            }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage(strMessage);
            }
        }
    }
}

