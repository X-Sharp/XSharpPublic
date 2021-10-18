//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Design.Serialization;
using System;
using System.CodeDom;
using System.IO;
using System.Security.Permissions;
using System.Text;
using XSharp.CodeDom;
using System.CodeDom.Compiler;
using Microsoft;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{

    public class XDocDataTextReader : DocDataTextReader
    {
        public CodeTypeDeclaration ClassName;
        public XDocDataTextReader(DocData docData, CodeTypeDeclaration classname) :base(docData)
        {
            ClassName = classname;
        }
    }
    // Moved "special" VisualStudio  CodeDomProvider
    [PermissionSet(SecurityAction.InheritanceDemand, Name = "FullTrust"), PermissionSet(SecurityAction.LinkDemand, Name = "FullTrust")]
    public class VSXSharpCodeDomProvider : XSharpCodeDomProvider
    {
        private XSharpFileNode _fileNode;

        // The parameterless constructor is called by the WPF designer ?

        public VSXSharpCodeDomProvider(XSharpFileNode fileNode)
        {
            _fileNode = fileNode;
            _projectNode = fileNode.ProjectMgr as XSharpProjectNode;
        }

        #region helper functions

        private bool IsFormSubType
        {
            get { return _fileNode.HasDesigner; }
        }

        private string GetFilePath()
        {
            return Path.Combine(Path.GetDirectoryName(_fileNode.GetMkDocument()), _fileNode.FileName);
        }

        private string GetDesignerFilePath()
        {
            return Path.Combine(Path.GetDirectoryName(_fileNode.GetMkDocument()),
                Path.GetFileNameWithoutExtension(_fileNode.FileName) +
                    ".Designer.prg");
        }

        #endregion


        #region Parser implementation

        XCodeCompileUnit ToXCodeCompileUnit(CodeCompileUnit unit)
        {
            if (unit is XCodeCompileUnit xccu)
                return xccu;
            return new XCodeCompileUnit(unit);
        }
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            XCodeCompileUnit compileUnit = null;
            //
            string mainFilePath = GetFilePath();
            // Are we are from the Designer ?
            if (codeStream is DocDataTextReader)
            {
                this.FileName = mainFilePath;
                // Do the parse
                // If the TextReader is a DocDataTextReader, we should be running from VisualStudio, called by the designer
                // So, we will guess the FileName to check if we have a .Designer.Prg file at the same place.
                // If so, we will have to handle both .prg to produce two CodeCompileUnit, then we will merge the result into one, with markers in it
                // so we can split again when the Designer is willing to save. ( See GenerateCodeFromCompileUnit )
                if (codeStream is DocDataTextReader)
                {
                    // Anyway, we have that source, just parse it.

                    WriteOutputMessage("Start Parse " + this.FileName);
                    compileUnit = ToXCodeCompileUnit(base.Parse(codeStream));
                    WriteOutputMessage("End Parse " + this.FileName);
                    // Now, we should check if we have a partial Class inside, if so, that's a Candidate for .Designer.prg
                    CodeNamespace nameSpace;
                    CodeTypeDeclaration className;
                    if (XSharpCodeDomHelper.HasPartialClass(compileUnit, out nameSpace, out className))
                    {
                        // Ok, so get the Filename, to get the .Designer.prg
                        DocDataTextReader ddtr = codeStream as DocDataTextReader;
                        DocData dd = ((IServiceProvider)ddtr).GetService(typeof(DocData)) as DocData;
                        Assumes.Present(dd);
                        string prgFileName = dd.Name;
                        // Build the Designer FileName
                        string designerPrgFile = XSharpCodeDomHelper.BuildDesignerFileName(prgFileName);
                        if (!string.IsNullOrEmpty(designerPrgFile) && File.Exists(designerPrgFile))
                        {
                            // Ok, we have a candidate !!!
                            DocData docdata = new DocData(ddtr, designerPrgFile);
                            DocDataTextReader reader = new XDocDataTextReader(docdata, className);
                            // so parse
                            WriteOutputMessage("Start Parse " + designerPrgFile);
                            var designerCompileUnit = ToXCodeCompileUnit(base.Parse(reader));
                            designerCompileUnit.FileName = designerPrgFile;
                            WriteOutputMessage("End Parse " + designerPrgFile);
                            // Now we have Two CodeCompileUnit, we must merge them
                            WriteOutputMessage("Start merge compile Units " + this.FileName);
                            var mergedCompileUnit = XSharpCodeDomHelper.MergeCodeCompileUnit(compileUnit, designerCompileUnit);
                            WriteOutputMessage("End merge compile Units " + this.FileName);
                            return mergedCompileUnit;
                        }
                    }
                }
                else
                {
                    var unit = base.Parse(codeStream);
                    if (unit is XCodeCompileUnit xccu)
                        compileUnit = xccu;
                    else
                        compileUnit = new XCodeCompileUnit(unit);
                }

            }
            //
            return compileUnit;
        }


        #endregion

        private void CopyClassProperties(CodeTypeDeclaration source, CodeTypeDeclaration target)
        {
            target.Name = source.Name;
            target.Attributes = source.Attributes;
        }
        // Called by the WinForm designer at save time
        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
            // Does that CodeCompileUnit comes from a "Merged" unit ?
            if (compileUnit is XMergedCodeCompileUnit mergedUnit)
            {
                // Retrieve the Form Class
                CodeTypeDeclaration combinedClass = XSharpCodeDomHelper.FindDesignerClass(compileUnit);
                // and retrieve the filename of the prg file
                string prgFileName = mergedUnit.FileName;
                // Build the Designer FileName
                // Retrieve Both CodeCompileUnit
                var formCCU = mergedUnit.FormUnit;
                var designCCU = mergedUnit.DesignerUnit;
                string designerPrgFile = designCCU.FileName;
                var formMembers = new CodeTypeMemberCollection(formCCU.Members);
                foreach (CodeTypeMember m in formMembers)
                {
                    m.SetWritten(false);
                }
                // suppress generating the "generated code" header in the form.prg
                formCCU.GenerateHeader = false;

                CodeTypeDeclaration formClass = formCCU.GetFirstClass();
                CodeTypeDeclaration designClass = designCCU.GetFirstClass();
                // Now, remove the members
                CopyClassProperties(combinedClass, formClass);
                CopyClassProperties(combinedClass, designClass);
                combinedClass.IsPartial = true;
                formClass.Members.Clear();
                designClass.Members.Clear();
                // Now, split the members
                // And make sure no members are deleted
                foreach (CodeTypeMember ctm in combinedClass.Members)
                {
                    // Was it a member that we have found in the original merged CodeCompileUnits ?
                    if (ctm is IXCodeObject xco)
                    {
                        if (ctm.GetFromDesigner())
                        {
                            // Comes from the Designer.prg file
                            // so go back to Designer.prg
                            designClass.Members.Add(ctm);
                            ctm.SetWritten(true);
                        }
                        else
                        {
                            // Comes from the original Form file
                            formClass.Members.Add(ctm);
                            foreach (CodeTypeMember member in formMembers)
                            {
                                if (member == ctm)
                                {
                                    ctm.SetWritten(true);
                                    formMembers.Remove(ctm);
                                    break;
                                }
                            }
                        }
                    }
                    else
                    {
                        // This must be a member generated by the Designer !
                        // So we will move Methods to the Form and all others to the Designer
                        if (ctm is CodeMemberMethod)
                        {
                            formClass.Members.Add(ctm);
                            ctm.SetWritten(true);
                        }
                        else
                        {
                            designClass.Members.Add(ctm);
                            ctm.SetWritten(true);
                        }
                    }
                }

                // Check for members that are not written
                foreach (CodeTypeMember member in formMembers)
                {
                    if (!member.WasWritten())
                    {
                        formClass.Members.Add(member);
                    }
                }

                // now, we must save both CodeCompileUnit
                // The received TextWriter is pointing to the Form
                // so we must create our own TextWriter for the Designer
                // First, let's make in Memory
                String generatedSource;
                MemoryStream inMemory = new MemoryStream();
                StreamWriter designerStream = new StreamWriter(inMemory, Encoding.UTF8);
                //
                // Backup original Form file and Form.Designer file
                //
                if (XSharpModel.XSettings.FormEditorMakeBackupFiles)
                {
                    if (File.Exists(prgFileName))
                    {
                        var bak = Path.ChangeExtension(prgFileName, ".bak");
                        Utilities.CopyFileSafe(prgFileName, bak);
                    }
                    if (File.Exists(designerPrgFile))
                    {
                        var bak = Path.ChangeExtension(designerPrgFile, ".bak");
                        Utilities.CopyFileSafe(designerPrgFile, bak);
                    }
                }

                base.GenerateCodeFromCompileUnit(designCCU, designerStream, options);
                // and force Flush
                designerStream.Flush();
                // Reset and read to String
                inMemory.Position = 0;
                StreamReader reader = new StreamReader(inMemory, Encoding.UTF8, true);
                generatedSource = reader.ReadToEnd();
                generatedSource = this._projectNode.SynchronizeKeywordCase(generatedSource, prgFileName);
                Encoding realencoding = reader.CurrentEncoding;
                reader.Close();
                designerStream.Close();

                XSharpFileNode node = _fileNode.FindChild(designerPrgFile) as XSharpFileNode;
                bool done = false;
                if (node != null )
                {
                    // assign the source to the open buffer when possible
                    if (node.DocumentSetText(generatedSource))
                    {
                        // then use automation to save the file, because that is much easier
                        // since we do not have to worry about the docdata etc.
                        var oaFile = (OAXSharpFileItem)node.GetAutomationObject();
                        oaFile.Save(designerPrgFile);
                        done = true;
                    }
                }
                if (! done)
                {
                    // File is not open in editor, so write to disk
                    designerStream = new StreamWriter(designerPrgFile, false, realencoding);
                    designerStream.Write(generatedSource);
                    designerStream.Flush();
                    designerStream.Close();
                }
                // The problem here, is that we "may" have some new members, like EvenHandlers, and we need to update their position (line/col)
                XSharpCodeParser parser = new XSharpCodeParser(_projectNode, formClass);
                parser.FileName = designerPrgFile;
                CodeCompileUnit resultDesigner = parser.Parse(generatedSource);
                CodeTypeDeclaration resultClass = XSharpCodeDomHelper.FindDesignerClass(resultDesigner);
                // just to be sure...
                if (resultClass != null)
                {
                    // Now push all elements from resultClass to designClass
                    designClass.Members.Clear();
                    foreach (CodeTypeMember ctm in resultClass.Members)
                    {
                        ctm.SetFromDesigner(true);
                        designClass.Members.Add(ctm);
                    }
                }
                // Ok,we MUST do the same thing for the Form file
                base.GenerateCodeFromCompileUnit(formCCU, writer, options);
                // BUT, the writer is hold by the Form Designer, don't close  it !!
                writer.Flush();
                // Now, we must re-read it and parse again
                IServiceProvider provider = (DocDataTextWriter)writer;
                DocData docData = (DocData)provider.GetService(typeof(DocData));
                DocDataTextReader ddtr = new DocDataTextReader(docData);
                // Retrieve
                generatedSource = ddtr.ReadToEnd();
                var newsource = this._projectNode.SynchronizeKeywordCase(generatedSource, prgFileName);

                if (string.Compare(newsource, generatedSource) != 0)
                {
                    // get DocDataTextWriter and update the source after the case has been synchronized
                    generatedSource = newsource;
                    DocDataTextWriter dtw = new DocDataTextWriter(docData);
                    dtw.Write(generatedSource);
                    dtw.Flush();
                }
                // Don't forget to set the name of the file where the source is...
                parser.FileName = prgFileName;
                resultDesigner = parser.Parse(generatedSource);
                resultClass = resultDesigner.GetFirstClass();
                // just to be sure...
                if (resultClass != null)
                {
                    // Now push all elements from resultClass to formClass
                    formClass.Members.Clear();
                    foreach (CodeTypeMember ctm in resultClass.Members)
                    {
                        ctm.SetFromDesigner(false);
                        formClass.Members.Add(ctm);
                    }
                }
                // Ok, it should be ok....
                // We have updated the file and the types that are stored inside each CCU that have been merged in compileUnit
                //XSharpCodeDomHelper.MergeCodeCompileUnit(compileUnit, formCCU, designCCU);
                // And update...
                combinedClass.Members.Clear();
                combinedClass.Members.AddRange(designClass.Members);
                combinedClass.Members.AddRange(formClass.Members);
            }
            else
            {
                var xcompileUnit = ToXCodeCompileUnit(compileUnit);
                // suppress generating the "generated code" header
                if (writer is  DocDataTextWriter)       // Form Editor
                {
                    compileUnit.SetNoHeader();
                }
                base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
                writer.Flush();
                // Designer gave us these informations
                // Now, we must re-read it and parse again
                if (writer is DocDataTextWriter)
                {
                    CodeTypeDeclaration formClass = compileUnit.GetFirstClass();
                    IServiceProvider provider = (DocDataTextWriter)writer;
                    DocData docData = (DocData)provider.GetService(typeof(DocData));
                    DocDataTextReader ddtr = new DocDataTextReader(docData);
                    // Retrieve
                    string generatedSource = ddtr.ReadToEnd();
                    XSharpCodeParser parser = new XSharpCodeParser(_projectNode);
                    parser.FileName = xcompileUnit.FileName;
                    generatedSource = _projectNode.SynchronizeKeywordCase(generatedSource, parser.FileName);
                    CodeCompileUnit resultCcu = parser.Parse(generatedSource);
                    CodeTypeDeclaration resultClass = resultCcu.GetFirstClass();
                    // just to be sure...
                    if (resultClass != null)
                    {
                        // Now push all elements from resultClass to formClass
                        formClass.Members.Clear();
                        foreach (CodeTypeMember ctm in resultClass.Members)
                        {
                            formClass.Members.Add(ctm);
                        }
                    }
                }

            }
        }
        void WriteOutputMessage(string msg)
        {
            XSharpModel.XSolution.WriteOutputMessage("XCodeDom: " + msg);
        }
    }
}
