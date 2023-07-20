//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using EnvDTE;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.CodeDom.Compiler;
using System.IO;
using System.Text;
using XSharp.CodeDom;

namespace XSharp.Project.FileCodeModel
{
    public class XSharpFileCodeModel : FileCodeModelBase
    {

        private FileNode _fileNode;
        private CodeDomProvider _codeDomProvider;


        public XSharpFileCodeModel(EnvDTE.ProjectItem projectItem, FileNode fileNode)
                : base(projectItem)
        {
            _fileNode = fileNode;
            _codeDomProvider = GetCodeDomProvider();
            forceInit = true;
        }

        internal bool forceInit;

        private CodeDomProvider GetCodeDomProvider()
        {
            if (_fileNode is XSharpFileNode)
                return ((XSharpFileNode)_fileNode).CodeDomProvider;
            else
            {
                // 
                using (ServiceProvider sp = new ServiceProvider(_fileNode.OleServiceProvider))
                {
                    IVSMDCodeDomProvider smdProvider = sp.GetService(typeof(Microsoft.VisualStudio.Shell.Interop.SVSMDCodeDomProvider)) as IVSMDCodeDomProvider;

                    if (null == smdProvider)
                        return null;

                    return smdProvider.CodeDomProvider as CodeDomProvider;
                }
            }
            //return null;
        }

        protected override void Initialize()
        {
            if (_fileNode is XSharpFileNode xfileNode)
            {
                if (forceInit || (CompileUnit == null))
                {
                    var xproject = xfileNode.ProjectMgr as XSharpProjectNode;
                    bool isOpen = false;
                    var text = xproject.DocumentGetText(xfileNode.Url, ref isOpen);
                    if (!isOpen)
                    {
                        text = File.ReadAllText(xfileNode.Url);
                    }
                    StringReader sr = new StringReader(text);
                    CompileUnit = _codeDomProvider.Parse(sr);
                    sr.Close();
                    forceInit = false;
                }
            }
        }

        internal override void FlushChanges()
        {
            bool contextAlreadyOpenned = DTE.UndoContext.IsOpen;
            if (!contextAlreadyOpenned)
                DTE.UndoContext.Open("Undo Code Merge", false);

            try
            {
                if (_fileNode is XSharpFileNode xfileNode)
                {
                    String generatedSource;
                    MemoryStream memStream = new MemoryStream();

                    StreamWriter stream = new StreamWriter(memStream, Encoding.UTF8);
                    _codeDomProvider.GenerateCodeFromCompileUnit(CompileUnit, stream, new CodeGeneratorOptions());
                    // and force Flush
                    stream.Flush();
                    // Reset and read to String
                    memStream.Position = 0;
                    StreamReader reader = new StreamReader(memStream, Encoding.UTF8, true);
                    generatedSource = reader.ReadToEnd();
                    Encoding realencoding = reader.CurrentEncoding;
                    reader.Close();
                    stream.Close();
                    SaveSource(xfileNode.Url, generatedSource, realencoding);
                    Initialize();
                }
            }
            finally
            {
                if (!contextAlreadyOpenned)
                    DTE.UndoContext.Close();
            }
        }

        private void SaveSource(string filename, string source, Encoding encoding, bool SaveToDisk = true)
        {
            if (_fileNode is XSharpFileNode xfileNode)
            {
                if (xfileNode.ProjectMgr is IProjectTypeHelper _projectNode)
                    _projectNode.SynchronizeKeywordCase(source, filename);


                bool done = false;
                // assign the source to the open buffer when possible
                if (xfileNode.DocumentSetText(source))
                {
                    // then use automation to save the file, because that is much easier
                    // since we do not have to worry about the docdata etc.
                    var oaFile = (OAXSharpFileItem)xfileNode.GetAutomationObject();
                    oaFile.Save(filename);
                    done = true;
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
        }
    }
}
