//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Design.Serialization;
using System;
using System.CodeDom;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Permissions;
using System.Text;
using System.Threading.Tasks;
using XSharp.CodeDom;
using System.CodeDom.Compiler;

namespace XSharp.Project
{

    // Moved "special" VisualStudio  CodeDomProvider
    [PermissionSet(SecurityAction.InheritanceDemand, Name = "FullTrust"), PermissionSet(SecurityAction.LinkDemand, Name = "FullTrust")]
    public class VSXSharpCodeDomProvider : XSharpCodeDomProvider
    {
        private XSharpFileNode _fileNode;

        // The parameterless constructor is called by the WPF designer ?

        public VSXSharpCodeDomProvider(XSharpFileNode fileNode)
        {
            _fileNode = fileNode;
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
                    ".Designer" + XSharpConstants.FileExtension1);
        }

        #endregion


        #region Parser implementation

        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit compileUnit = null;
            //
            string mainFilePath = GetFilePath();
            // Are we are from the Designer ?
            if (codeStream is DocDataTextReader)
            {
                this.FileName = mainFilePath;
                // Do the parse
#if DESIGNERSUPPORT
                // If the TextReader is a DocDataTextReader, we should be running from VisualStudio, called by the designer
                // So, we will guess the FileName to check if we have a .Designer.Prg file at the same place.
                // If so, we will have to handle both .prg to produce two CodeCompileUnit, then we will merge the result into one, with markers in it
                // so we can split again when the Designer is willing to save. ( See GenerateCodeFromCompileUnit )
                if (codeStream is DocDataTextReader)
                {
                    // Anyway, we have that source, just parse it.
                    compileUnit = base.Parse(codeStream);
                    // Now, we should check if we have a partial Class inside, if so, that's a Candidate for .Designer.prg
                    CodeNamespace nameSpace;
                    CodeTypeDeclaration className;
                    if (XSharpCodeDomHelper.HasPartialClass(compileUnit, out nameSpace, out className))
                    {
                        // Ok, so get the Filename, to get the .Designer.prg
                        DocDataTextReader ddtr = codeStream as DocDataTextReader;
                        DocData dd = ((IServiceProvider)ddtr).GetService(typeof(DocData)) as DocData;
                        String prgFileName = dd.Name;
                        // Build the Designer FileName
                        String designerPrgFile = XSharpCodeDomHelper.BuildDesignerFileName(prgFileName);
                        if (File.Exists(designerPrgFile))
                        {
                            // Ok, we have a candidate !!!
                            DocData docdata = new DocData((IServiceProvider)ddtr, designerPrgFile);
                            DocDataTextReader reader = new DocDataTextReader(docdata);
                            // so parse
                            CodeCompileUnit designerCompileUnit = base.Parse(reader);
                            CodeCompileUnit mergedCompileUnit = null;
                            // Now we have Two CodeCompileUnit, we must merge them
                            mergedCompileUnit = XSharpCodeDomHelper.MergeCodeCompileUnit(compileUnit, designerCompileUnit);
                            mergedCompileUnit.UserData[XSharpCodeConstants.USERDATA_HASDESIGNER] = true;
                            mergedCompileUnit.UserData[XSharpCodeConstants.USERDATA_FILENAME] = prgFileName;
                            // Save CCU for GenerateCode operation, it will be faster and easier than to recreate it
                            mergedCompileUnit.UserData[XSharpCodeConstants.USERDATA_CCU_FORM] = compileUnit;
                            mergedCompileUnit.UserData[XSharpCodeConstants.USERDATA_CCU_DESIGNER] = designerCompileUnit;
                            return mergedCompileUnit;
                        }
                    }
                }
                else
#endif
                {
                    compileUnit = base.Parse(codeStream);
                }

            }
            //
            return compileUnit;
        }


        #endregion

        // Called by the WinForm designer at save time
        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
#if DESIGNERSUPPORT
            // Does that CodeCompileUnit comes from a "Merged" unit ?
            if (compileUnit.UserData.Contains(XSharpCodeConstants.USERDATA_HASDESIGNER))
            {
                // Retrieve the Form Class
                CodeNamespace designerNamespace;
                CodeTypeDeclaration designerClass = XSharpCodeDomHelper.FindDesignerClass(compileUnit, out designerNamespace);
                // and retrieve the filename of the prg file
                String prgFileName = (string)compileUnit.UserData[XSharpCodeConstants.USERDATA_FILENAME];
                // Build the Designer FileName
                String designerPrgFile = XSharpCodeDomHelper.BuildDesignerFileName(prgFileName);
                // Retrieve Both CodeCompileUnit
                CodeCompileUnit formCCU = (CodeCompileUnit)compileUnit.UserData[XSharpCodeConstants.USERDATA_CCU_FORM];
                CodeCompileUnit designCCU = (CodeCompileUnit)compileUnit.UserData[XSharpCodeConstants.USERDATA_CCU_DESIGNER];
                //
                CodeNamespace formNamespace;
                CodeTypeDeclaration formClass = XSharpCodeDomHelper.FindFirstClass(formCCU, out formNamespace);
                CodeNamespace designNamespace;
                CodeTypeDeclaration designClass = XSharpCodeDomHelper.FindFirstClass(designCCU, out designNamespace);
                // Now, remove the members
                formClass.Members.Clear();
                designClass.Members.Clear();
                // Now, split the members
                foreach (CodeTypeMember ctm in designerClass.Members)
                {
                    // Was it a member that we have found in the original merged CodeCompileUnits ?
                    if (ctm.UserData.Contains(XSharpCodeConstants.USERDATA_FROMDESIGNER))
                    {
                        if ((bool)ctm.UserData[XSharpCodeConstants.USERDATA_FROMDESIGNER])
                        {
                            // Comes from the Designer.prg file
                            // so go back to Designer.prg
                            designClass.Members.Add(ctm);
                        }
                        else
                        {
                            // Comes from the original Form file
                            formClass.Members.Add(ctm);
                        }
                    }
                    else
                    {
                        // This must be a member generated by the Designer !
                        // So we will move Methods to the Form and all others to the Designer
                        if (ctm is CodeMemberMethod)
                        {
                            formClass.Members.Add(ctm);
                        }
                        else
                        {
                            designClass.Members.Add(ctm);
                        }
                    }
                }
                // now, we must save both CodeCompileUnit
                // The received TextWriter is pointing to the Form
                // so we must create our own TextWriter for the Designer
                // First, let's make in Memory
                String generatedSource;
                MemoryStream inMemory = new MemoryStream();
                StreamWriter designerStream = new StreamWriter(inMemory, System.Text.Encoding.Default);
                // 
                base.GenerateCodeFromCompileUnit(designCCU, designerStream, options);
                // and force Flush
                designerStream.Flush();
                // Reset and read to String
                inMemory.Position = 0;
                StreamReader reader = new StreamReader(inMemory, System.Text.Encoding.Default);
                generatedSource = reader.ReadToEnd();
                reader.Close();
                designerStream.Close();
                // and now write the "real" file
                designerStream = new StreamWriter(designerPrgFile, false, System.Text.Encoding.Default);
                designerStream.Write(generatedSource);
                designerStream.Flush();
                designerStream.Close();
                // The problem here, is that we "may" have some new members, like EvenHandlers, and we need to update their position (line/col)
                XSharpCodeParser parser = new XSharpCodeParser();
                parser.TabSize = XSharpCodeDomProvider.TabSize;
                parser.FileName = designerPrgFile;
                CodeCompileUnit resultDesigner = parser.Parse(generatedSource);
                CodeNamespace resultNamespace;
                CodeTypeDeclaration resultClass = XSharpCodeDomHelper.FindDesignerClass(resultDesigner, out resultNamespace);
                // just to be sure...
                if (resultClass != null)
                {
                    // Now push all elements from resultClass to designClass
                    designClass.Members.Clear();
                    foreach (CodeTypeMember ctm in resultClass.Members)
                    {
                        ctm.UserData[XSharpCodeConstants.USERDATA_FROMDESIGNER] = true;
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
                // Don't forget to set the name of the file where the source is... 
                parser.FileName = prgFileName;
                resultDesigner = parser.Parse(generatedSource);
                resultClass = XSharpCodeDomHelper.FindFirstClass(resultDesigner, out resultNamespace);
                // just to be sure...
                if (resultClass != null)
                {
                    // Now push all elements from resultClass to formClass
                    formClass.Members.Clear();
                    foreach (CodeTypeMember ctm in resultClass.Members)
                    {
                        ctm.UserData[XSharpCodeConstants.USERDATA_FROMDESIGNER] = false;
                        formClass.Members.Add(ctm);
                    }
                }
                // Ok, it should be ok....
                // We have updated the file and the types that are stored inside each CCU that have been merged in compileUnit
                //XSharpCodeDomHelper.MergeCodeCompileUnit(compileUnit, formCCU, designCCU);
                // And update...
                designerClass.Members.Clear();
                foreach (CodeTypeMember m in designClass.Members)
                {
                    designerClass.Members.Add(m);
                }
                foreach (CodeTypeMember m in formClass.Members)
                {
                    designerClass.Members.Add(m);
                }
            }
            else
#endif
            {
                base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
                writer.Flush();
                // Designer gave us these informations
                CodeNamespace formNamespace;
                CodeTypeDeclaration formClass = XSharpCodeDomHelper.FindFirstClass(compileUnit, out formNamespace);
                // Now, we must re-read it and parse again
                IServiceProvider provider = (DocDataTextWriter)writer;
                DocData docData = (DocData)provider.GetService(typeof(DocData));
                DocDataTextReader ddtr = new DocDataTextReader(docData);
                // Retrieve 
                string generatedSource = ddtr.ReadToEnd();
                XSharpCodeParser parser = new XSharpCodeParser();
                parser.TabSize = XSharpCodeDomProvider.TabSize;
                CodeCompileUnit resultCcu = parser.Parse(generatedSource);
                CodeNamespace resultNamespace;
                CodeTypeDeclaration resultClass = XSharpCodeDomHelper.FindFirstClass(resultCcu, out resultNamespace);
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
}
