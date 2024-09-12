using System;
using System.Collections.Generic;
using System.CodeDom;
using System.Collections;
using System.CodeDom.Compiler;
using System.IO;
using System.Drawing;
using System.Diagnostics;
using static XSharp.Parser.VsParser;
using LanguageService.SyntaxTree;
using XSharp.Parser;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
using XSharp.Settings;

namespace XSharp.CodeDom
{

    public class ErrorListener : IErrorListener
    {
        public class XError
        {
            public string FileName { get; set; }
            public LinePositionSpan Span { get; set; }
            public string ErrorCode { get; set; }
            public string Message { get; set; }
            public object[] Args { get; set; }

        }
        #region IErrorListener
        private List<XError> errors = new List<XError>();
        private List<XError> warnings = new List<XError>();

        internal List<XError> Errors { get => errors; set => errors = value; }
        internal List<XError> Warnings { get => warnings; set => warnings = value; }

        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            errors.Add(new XError { FileName = fileName, Span = span, ErrorCode = errorCode, Message = message, Args = args });
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            warnings.Add(new XError { FileName = fileName, Span = span, ErrorCode = errorCode, Message = message, Args = args });
        }
        #endregion
    }
    public class XSharpCodeParser : CodeParser
    {
        XProject _projectNode;
        public XSharpCodeParser(XProject projectNode)
        {
            this.FileName = "";
            _projectNode = projectNode;
        }
        public XSharpCodeParser(XProject projectNode, CodeTypeDeclaration formClass)
        {
            this.FileName = "";
            _projectNode = projectNode;
            typeInMainFile = formClass;
        }
        public string FileName { get; set; }

        CodeTypeDeclaration typeInMainFile = null;
        public override CodeCompileUnit Parse(TextReader codeStream)
        {
            CodeCompileUnit ccu;
            if (codeStream == null)
            {
                ccu = new XCodeCompileUnit();
            }
            else
            {
                string codeFile;
                codeFile = codeStream.ReadToEnd();
                var field = codeStream.GetType().GetField("ClassName");
                if (field != null)
                {
                    typeInMainFile = field.GetValue(codeStream) as CodeTypeDeclaration;
                }
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
                var reporter = new ErrorListener();
                ITokenStream tokenStream;
                LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParserRuleContext xtree;
                bool ok = XSharp.Parser.VsParser.Parse(source, this.FileName, (XSharpParseOptions)_projectNode.ParseOptions, reporter, out tokenStream, out xtree, out _);
                if (reporter.Errors.Count > 0)
                {
                    foreach (var error in reporter.Errors)
                    {
                        XSettings.Logger.Debug($"Parser error: Line {error.Span.Line} column {error.Span.Column}, Code {error.ErrorCode}, {error.Message} ");
                    }
                }
                if (reporter.Warnings.Count > 0)
                {
                    foreach (var error in reporter.Warnings)
                    {
                        XSettings.Logger.Debug($"Parser warning: Line {error.Span.Line} column {error.Span.Column}, Code {error.ErrorCode}, {error.Message} ");
                    }
                }

                // We need to d 2 steps here:
                // 1 - Scan for the fields , so we know the difference between fields and properties when we perform step 2
                // 2 - Scan for the rest. We pass the list of fields to the tree discover code so it "knows" about all fields

                var discoverFields = new XSharpFieldsDiscover(_projectNode, typeInMainFile);
                discoverFields.SourceCode = source;
                discoverFields.CurrentFile = this.FileName;

                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                walker.Walk(discoverFields, xtree);
                // now the discoverFields class should contain a Dictionary with <context, FieldList>
                var discover = new XSharpClassDiscover(_projectNode, typeInMainFile);
                discover.FieldList = discoverFields.FieldList;
                discover.SourceCode = source;
                discover.CurrentFile = this.FileName;
                //
                walker.Walk(discover, xtree);
                //
                ccu = discover.CodeCompileUnit;
                var firstType = ccu.GetFirstClass();
                if (firstType != null)
                {
                    // save a copy of the member list to the CCU
                    // Sort them first
                    ccu.Members = Helpers.SortMembers(firstType.Members); 
                }
                // save file name & original source
                ccu.FileName = this.FileName;
                ccu.Source = source;
            }
            catch (Exception ex)
            {
                if (System.Diagnostics.Debugger.IsAttached)
                    Debug.WriteLine(ex.Message + Environment.NewLine + ex.StackTrace);
            }
            //
            return ccu;
        }
       
    }
}
