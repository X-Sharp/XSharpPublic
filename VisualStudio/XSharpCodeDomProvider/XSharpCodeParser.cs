using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using LanguageService.CodeAnalysis.XSharp;
using System.Diagnostics;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;
using LanguageService.SyntaxTree;

namespace XSharp.CodeDom
{
    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }
    public class XSharpCodeParser : CodeParser
    {
        IProjectTypeHelper _projectNode;
        public XSharpCodeParser(IProjectTypeHelper projectNode)
        {
            this.FileName = "";
            this.TabSize = 1;
            _projectNode = projectNode;
        }
        public string FileName { get; set; }

        public int TabSize { get; set; }

        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit ccu;
            if (codeStream == null)
            {
                ccu = new CodeCompileUnit();
            }
            else
            {
                String codeFile;
                codeFile = codeStream.ReadToEnd();
                ccu = Parse(codeFile);
            }
            return ccu;
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="source"></param>
        /// <returns></returns>
        public CodeCompileUnit Parse(string source)
        {
            XCodeCompileUnit ccu = new XCodeCompileUnit();
            //
            try
            {
                // Tab replace, in order to have the good position of Members (Line/col)
                String TabSpace = new String(' ', TabSize);
                source = source.Replace("\t", TabSpace);
                //
                var reporter = new ErrorIgnorer();
                ITokenStream tokenStream;
                LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.SourceContext xtree;
                bool ok = XSharp.Parser.VsParser.Parse(source, this.FileName, _projectNode.ParseOptions, reporter, out tokenStream, out xtree);

                // We need to d 2 steps here:
                // 1 - Scan for the fields , so we know the difference between fields and properties when we perform step 2
                // 2 - Scan for the rest. We pass the list of fields to the tree discover code so it "knows" about all fields

                var discoverFields = new XSharpFieldsDiscover(_projectNode);
                discoverFields.SourceCode = source;
                discoverFields.CurrentFile = this.FileName;

                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                walker.Walk(discoverFields, xtree);
                // now the discoverFields class should contain a Dictionary with <context, FieldList>
                var discover = new XSharpClassDiscover(_projectNode);
                discover.FieldList = discoverFields.FieldList;
                discover.SourceCode = source;
                discover.CurrentFile = this.FileName;
                //
                walker.Walk(discover, xtree);
                //
                ccu = discover.CodeCompileUnit;
                ccu.UserData[XSharpCodeConstants.USERDATA_FILENAME] = this.FileName;
                ccu.UserData[XSharpCodeConstants.USERDATA_CODE]     = source;
            }
            catch ( Exception ex )
            {
                if (System.Diagnostics.Debugger.IsAttached)
                    Debug.WriteLine(ex.Message + Environment.NewLine + ex.StackTrace);
            }
            //
            return ccu;
        }

    }
}
