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
            
            this.FileName = mainFilePath;
            // Do the parse
            // If the TextReader is a DocDataTextReader, we should be running from VisualStudio, called by the designer
            // So, we will guess the FileName to check if we have a .Designer.Prg file at the same place.
            // If so, we will have to handle both .prg to produce two CodeCompileUnit, then we will merge the result into one, with markers in it
            // so we can split again when the Designer is willing to save. ( See GenerateCodeFromCompileUnit )
            // Anyway, we have that source, just parse it.

            WriteOutputMessage("Start Parse " + this.FileName);
            compileUnit = ToXCodeCompileUnit(base.Parse(codeStream));
            WriteOutputMessage("End Parse " + this.FileName);
            // Now, we should check if we have a partial Class inside, if so, that's a Candidate for .Designer.prg
            if (XSharpCodeDomHelper.HasPartialClass(compileUnit, out _, out var className))
            {
                if (codeStream is DocDataTextReader ddtr)
                {
                    // Ok, so get the Filename, to get the .Designer.prg
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
                else
                {
                    if (compileUnit is XCodeCompileUnit xccu)
                        compileUnit = xccu;
                    else
                        compileUnit = new XCodeCompileUnit(compileUnit);
                }
            }
            return compileUnit;
        }


        #endregion

        private void CopyClassProperties(CodeTypeDeclaration source, CodeTypeDeclaration target)
        {
            target.Name = source.Name;
            target.Attributes = source.Attributes;
        }
        private void SaveSource(string filename, string source, Encoding encoding, bool SaveToDisk = true)
        {
            source = this._projectNode.SynchronizeKeywordCase(source, filename);

            XSharpFileNode node = _fileNode.FindChild(filename) as XSharpFileNode;
            bool done = false;
            if (node != null)
            {
                // assign the source to the open buffer when possible
                if (node.DocumentSetText(source))
                {
                    // then use automation to save the file, because that is much easier
                    // since we do not have to worry about the docdata etc.
                    var oaFile = (OAXSharpFileItem)node.GetAutomationObject();
                    oaFile.Save(filename);
                    done = true;
                }
            }
            if (!done && SaveToDisk)
            {
                // File is not open in editor, so write to disk
                var designerStream = new StreamWriter(filename, false, encoding);
                designerStream.Write(source);
                designerStream.Flush();
                designerStream.Close();
            }
        }

        private void UpdateUserDataForClass(CodeTypeDeclaration type, string source, string fileName, CodeTypeDeclaration mainClass)
        {
            XSharpCodeParser parser = new XSharpCodeParser(_projectNode, mainClass);
            parser.FileName = fileName;
            CodeCompileUnit tempUnit = parser.Parse(source);
            CodeTypeDeclaration tempClass = XSharpCodeDomHelper.FindDesignerClass(tempUnit, mainClass);
            if (tempClass != null)
            {
                tempClass.UpdateClassMemberUserData(type);
            }
        }

        private string GetDocDataSource(DocDataTextWriter dtw)
        {
            var prop = dtw.GetType().GetProperty("DocData", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            if (prop == null)
            {
                throw new ApplicationException("CCU DocData property is null");
            }
            DocData docData = (DocData)prop.GetValue(dtw);
            DocDataTextReader ddtr = new DocDataTextReader(docData);
            // Retrieve
            return ddtr.ReadToEnd();
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
                bool mustWriteForm = formCCU.MustWrite;
                var designCCU = mergedUnit.DesignerUnit;
                // make sure namespaces are the same
                // C# does that too.
                if (mergedUnit.DesignerNamespace != null && mergedUnit.FormNamespace != null)
                {
                    mergedUnit.DesignerNamespace.Name = mergedUnit.FormNamespace.Name;
                }

                string designerPrgFile = designCCU.FileName;
                var originalFormMembers = new CodeTypeMemberCollection(formCCU.Members);
                foreach (CodeTypeMember m in originalFormMembers)
                {
                    m.SetWritten(false);
                }
                // suppress generating the "generated code" header in the form.prg
                formCCU.GenerateHeader = false;
                designCCU.GenerateHeader = true;
                CodeTypeDeclaration formClass = formCCU.GetFirstClass();
                CodeTypeDeclaration designClass = designCCU.GetFirstClass();
                // Now, remove the members
                CopyClassProperties(combinedClass, formClass);
                foreach (CodeTypeMember member in formClass.Members)
                {
                    member.SetFromDesigner(false);
                }
                if (designClass != null)
                {
                    CopyClassProperties(combinedClass, designClass);
                    foreach (CodeTypeMember member in designClass.Members)
                    {
                        member.SetFromDesigner(true);
                    }
                    designClass.Members.Clear();
                }
                combinedClass.IsPartial = true;
                formClass.Members.Clear();
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
                            // so write back to Designer.prg
                            designClass.Members.Add(ctm);
                        }
                        else
                        {
                            // Comes from the original Form file
                            formClass.Members.Add(ctm);
                            foreach (CodeTypeMember member in originalFormMembers)
                            {
                                if (ctm == member || member.SourceEquals(ctm))
                                {
                                    member.SetWritten(true);
                                    originalFormMembers.Remove(member);
                                    break;
                                }
                            }
                        }
                        ctm.SetWritten(true);
                    }
                    else
                    {
                        // Not an IXCodeObject, so this must be a member generated by the Designer !
                        // We will move Methods to the Form and all others to the Designer
                        if (ctm is CodeMemberMethod)
                        {
                            formClass.Members.Add(ctm);
                            mustWriteForm = true;
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
                foreach (CodeTypeMember member in originalFormMembers)
                {
                    if (!member.WasWritten())
                    {
                        formClass.Members.Add(member);
                    }
                }

                //
                // Backup original Form file and Form.Designer file
                //
                if (XSharpModel.XCustomEditorSettings.BackupFormFiles)
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
                // now, we must save both CodeCompileUnit
                // The received TextWriter is pointing to the Form
                // so we must create our own TextWriter for the Designer
                // First, let's make in Memory
                String generatedSource;
                MemoryStream memStream = new MemoryStream();

                StreamWriter stream = new StreamWriter(memStream, Encoding.UTF8);
                base.GenerateCodeFromCompileUnit(designCCU, stream, options);
                // and force Flush
                stream.Flush();
                // Reset and read to String
                memStream.Position = 0;
                StreamReader reader = new StreamReader(memStream, Encoding.UTF8, true);
                generatedSource = reader.ReadToEnd();
                Encoding realencoding = reader.CurrentEncoding;
                reader.Close();
                stream.Close();
                SaveSource(designerPrgFile, generatedSource, realencoding);

                // We "may" have some new members, like EvenHandlers,
                // and we need to update their position (line/col)
                UpdateUserDataForClass(combinedClass, generatedSource, designerPrgFile, formClass);
                // Do the same thing for the Form file
                if (mustWriteForm)
                {
                    base.GenerateCodeFromCompileUnit(formCCU, writer, options);
                }
                else
                {
                    writer.Write(formCCU.GetSourceCode());
                }
                // BUT, the writer is hold by the Form Designer, don't close  it !!
                writer.Flush();
                // Now, we must re-read it and parse again
                if (writer is DocDataTextWriter dtw)
                {
                    generatedSource = GetDocDataSource(dtw);
                    UpdateUserDataForClass(combinedClass, generatedSource, prgFileName, formClass);
                }
            }
            else
            {
                var xcompileUnit = ToXCodeCompileUnit(compileUnit);
                // suppress generating the "generated code" header
                if (writer is DocDataTextWriter)       
                {
                    // The Form Editor is a DocDataTextWriter
                    // other editors are different writer and should generate the Header
                    xcompileUnit.SetNoHeader();
                }
                base.GenerateCodeFromCompileUnit(xcompileUnit, writer, options);
                writer.Flush();
                // Designer gave us these informations
                // Now, we must re-read it and parse again
                if (writer is DocDataTextWriter dtw)
                {
                    // The Form Editor is a DocDataTextWriter
                    CodeTypeDeclaration formClass = xcompileUnit.GetFirstClass();
                    string generatedSource = GetDocDataSource(dtw);
                    UpdateUserDataForClass(formClass, generatedSource, xcompileUnit.FileName, formClass);
                }

            }
        }
        void WriteOutputMessage(string msg)
        {
            XSharpModel.XSolution.WriteOutputMessage("XCodeDom: " + msg);
        }
    }
}
