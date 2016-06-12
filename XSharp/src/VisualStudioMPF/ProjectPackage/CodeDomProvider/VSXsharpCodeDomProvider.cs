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

    }
}
