﻿using System;
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
            _projectNode = projectNode;
        }
        public XSharpCodeParser(IProjectTypeHelper projectNode, CodeTypeDeclaration formClass)
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
                var reporter = new ErrorIgnorer();
                ITokenStream tokenStream;
                LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParserRuleContext xtree;
                bool ok = XSharp.Parser.VsParser.Parse(source, this.FileName, _projectNode.ParseOptions, reporter, out tokenStream, out xtree, out _);

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
                    ccu.Members = firstType.Members;
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
