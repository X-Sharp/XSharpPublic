using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Generic;
using System.IO;
using XSharpModel;
using File = System.IO.File;

namespace XSharp.LanguageService
{
    class XSharpGotoDefinition
    {
        internal static void GotoDefn(ITextView TextView)
        {
            try
            {
                if (XSettings.DisableGotoDefinition)
                    return;
                var file = TextView.TextBuffer.GetFile();
                if (file == null || file.XFileType != XFileType.SourceCode)
                    return;
                WriteOutputMessage("CommandFilter.GotoDefn()");
                ModelWalker.Suspend();


                var snapshot = TextView.TextBuffer.CurrentSnapshot;

                // We don't want to lex the buffer. So get the tokens from the last lex run
                // and when these are too old, then simply bail out
                var tokens = TextView.TextBuffer.GetTokens();
                if (tokens != null)
                {
                    if (tokens.SnapShot.Version != snapshot.Version)
                        return;
                }
                string currentNS = TextView.FindNamespace();
                var location = TextView.FindLocation();
                var state = CompletionState.General | CompletionState.Types | CompletionState.Namespaces;
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream, out state);

                // LookUp for the BaseType, reading the TokenList (From left to right)
                var result = new List<IXSymbol>();

                result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state, out var notProcessed));
                //
                Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
                if (result.Count > 0)
                {
                    var element = result[0];
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
                    return;
                }
                //
                if (tokenList.Count > 1)
                {
                    // try again with just the last element in the list
                    var token = tokenList[tokenList.Count - 1];
                    tokenList.Clear();
                    tokenList.Add(token);
                    location = location.With(currentNS);
                    result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state, out notProcessed));
                }
                if (result.Count > 0)
                {
                    var element = result[0];
                    if (element is XSourceEntity source)
                        source.OpenEditor();
                    //else
                    //{
                    //    openInObjectBrowser(element.FullName);
                    //}
                    //return;

                }

            }
            catch (Exception ex)
            {
                WriteOutputMessage("Goto failed: ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                ModelWalker.Resume();
            }
        }

        private static void DeleteFolderRecursively(DirectoryInfo directory)
        {
            // Scan all files in the current path
            foreach (FileInfo file in directory.GetFiles())
            {
                file.Attributes &= ~FileAttributes.ReadOnly;
                file.Delete();
            }

            DirectoryInfo[] subDirectories = directory.GetDirectories();

            // Scan the directories in the current directory and call this method 
            // again to go one level into the directory tree
            foreach (DirectoryInfo subDirectory in subDirectories)
            {
                DeleteFolderRecursively(subDirectory);
                subDirectory.Attributes &= ~FileAttributes.ReadOnly;

                subDirectory.Delete();
            }
        }

        static string WorkFolder = null;
        static Stream Semaphore = null;
        const string folderName = "XSharp.Intellisense";
        const string semName = "XSharp.Busy";
        private static XAssembly asmName = null;
        private static string LookupXml(IXSymbol key)
        {
            return XSharpXMLDocMember.GetDoc(asmName, key);
        }
        private static void GotoSystemType(ITextView TextView, XPETypeSymbol petype, XPESymbol element)
        {
            asmName = petype.Assembly;
            bool mustCreate = false;
            if (Semaphore == null)
            {
                // we create a semaphore file in the workfolder to make sure that if 2 copies of VS are running
                // that we will not delete the files from the other copy
                var tempFolder = Path.GetTempPath();
                tempFolder = Path.Combine(tempFolder, folderName);
                var semFile = Path.Combine(tempFolder, semName);
                // clean up files from previous run
                if (Directory.Exists(tempFolder))
                {
                    if (File.Exists(semFile))
                    {
                        try
                        {
                            File.Delete(semFile);
                            DeleteFolderRecursively(new DirectoryInfo(tempFolder));
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
                Semaphore = File.Create(semFile);
            }
            var ns = petype.Namespace + "." + petype.Assembly.Version;
            var name = petype.Name;
            var nspath = Path.Combine(WorkFolder, ns);
            if (!Directory.Exists(nspath))
            {
                Directory.CreateDirectory(nspath);
            }
            var temp = Path.Combine(nspath, petype.Name) + ".prg";
            mustCreate = !File.Exists(temp);
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
            var walker = new SourceWalker(xFile, false);
            walker.Parse(false);
            var entities = walker.EntityList;
            var line = 1;

            if (petype.IsFunctionsClass)
            {
                foreach (var entity in entities)
                {
                    if (entity.Prototype == element.Prototype)
                    {
                        line = entity.Range.StartLine + 1;
                        break;
                    }
                }
            }
            else
            {
                foreach (var entity in entities)
                {
                    if (entity.FullName == element.FullName)
                    {
                        line = entity.Range.StartLine + 1;
                        break;
                    }
                }

            }
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
            XSettings.OpenDocument(temp, line, 1, true);

        }


        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage(strMessage);
            }
        }
    }
}

