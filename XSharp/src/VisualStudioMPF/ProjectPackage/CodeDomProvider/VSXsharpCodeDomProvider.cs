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
                    ".Designer" +XSharpConstants.FileExtension1);
        }

        #endregion


        #region Parser implementation

        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit ccu = null;
            //
            string mainFilePath = GetFilePath();
            // Are we are from the Designer ?
            if (codeStream is DocDataTextReader)
            {
                this.FileName = mainFilePath;
                // Do the parse
                ccu = base.Parse(codeStream);
            }
            //
            return ccu;
        }


        #endregion

        public override void GenerateCodeFromCompileUnit(CodeCompileUnit compileUnit, TextWriter writer, CodeGeneratorOptions options)
        {
            base.GenerateCodeFromCompileUnit(compileUnit, writer, options);
            writer.Flush();
            // Retrieve the original elements
            CodeNamespace originNamespace;
            CodeTypeDeclaration originClass = XSharpCodeDomHelper.FindFirstClass(compileUnit, out originNamespace);
            // and see, what has been generated
            CodeCompileUnit resultCcu;
            CodeNamespace resultNamespace;
            CodeTypeDeclaration resultClass=null;
            // The access to the document is stored in UserData
            RelatedDocDataCollection docCollection;
            docCollection = compileUnit.UserData[typeof(RelatedDocDataCollection)] as RelatedDocDataCollection;
            if (docCollection != null)
            {
                // a forEach, but just to grab the first
                foreach (DocData docData in docCollection)
                {
                    DocDataTextReader textReader = new DocDataTextReader(docData);
                    XSharpCodeParser parser = new XSharpCodeParser();
                    parser.FileName = this.FileName;
                    resultCcu = parser.Parse(textReader);
                    // Retrieve the first type declaration
                    resultClass = XSharpCodeDomHelper.FindFirstClass(compileUnit, out resultNamespace);
                    break;
                }
                // just to be sure...
                if ((originClass != null) && (resultClass != null))
                {
                    // Now push all elements from resultClass to formClass
                    originClass.Members.Clear();
                    foreach (CodeTypeMember ctm in resultClass.Members)
                    {
                        originClass.Members.Add(ctm);

                    }
                }
            }
        }
    }
}
