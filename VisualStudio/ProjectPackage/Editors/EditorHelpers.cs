using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    static internal class EditorHelpers
    {
        internal static bool IsVulcanFileNode(string fileName)
        {
            object itemNode = GetItemNode(fileName);
            if (itemNode != null)
            {
                Type type = itemNode.GetType();
                var asm = type.Assembly.GetName().Name;
                return asm.IndexOf("vulcan", StringComparison.OrdinalIgnoreCase) == 0;
            }
            return false;
        }
        private static object GetItemNode(string filename)
        {
            EnvDTE80.DTE2 dte = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(EnvDTE.DTE)) as EnvDTE80.DTE2;
            var projectitem = dte.Solution.FindProjectItem(filename);
            return projectitem;
        }

        public static String GetDocumentFileName(ITextBuffer TextBuffer)
        {
            String fileName = "";
            ITextDocument textDoc;
            var rc = TextBuffer.Properties.TryGetProperty<ITextDocument>(typeof(ITextDocument), out textDoc);
            if (rc == true)
            {
                fileName = textDoc.FilePath;
            }
            return fileName;
        }
    }
}
