//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
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

namespace XSharp.CodeDom
{
    public class XSharpCodeParser : CodeParser
    {

        public XSharpCodeParser()
        {
            this.FileName = "";
            this.TabSize = 1;       
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
            CodeCompileUnit ccu = new CodeCompileUnit();
            //
            try
            {
                // Tab replace, in order to have the good position of Memebers (Line/col)
                String TabSpace = new String(' ', TabSize);
                source = source.Replace("\t", TabSpace);
                //
                LanguageService.CodeAnalysis.SyntaxTree tree = XSharpSyntaxTree.ParseText(source);
                var syntaxRoot = tree.GetRoot();
                // Get the antlr4 parse tree root
                var xtree = ((LanguageService.CodeAnalysis.XSharp.Syntax.CompilationUnitSyntax)syntaxRoot).XSource;
                //
                var discover = new XSharpTreeDiscover();
                discover.SourceCode = source;
                discover.CurrentFile = this.FileName;
                //
                var walker = new LanguageService.SyntaxTree.Tree.ParseTreeWalker();
                walker.Walk(discover, xtree);
                //
                ccu = discover.CodeCompileUnit;
            }
            catch ( Exception ex )
            {
                Debug.WriteLine(ex.Message + Environment.NewLine + ex.StackTrace);
            }
            //
            return ccu;
        }

    }
}
