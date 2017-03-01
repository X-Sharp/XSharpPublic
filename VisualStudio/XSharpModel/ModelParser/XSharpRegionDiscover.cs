//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.SyntaxTree.Tree;
using Microsoft.VisualStudio.Text;

namespace XSharpModel
{
    partial class XSharpModelRegionDiscover : XSharpBaseListener
    {
        internal List<ClassificationSpan> tags = new List<ClassificationSpan>();
        public ITextSnapshot Snapshot { get; set; }
        public bool BuildRegionTags { get; internal set; }

        private bool _reInitModel;
        private bool _buildModel;
        public bool BuildModel
        {
            get
            {
                return _buildModel;
            }

            internal set
            {
                _buildModel = value;
                _reInitModel = value;
            }
        }

        internal IClassificationType xsharpIdentifierType;
        internal IClassificationType xsharpBraceOpenType;
        internal IClassificationType xsharpBraceCloseType;
        internal IClassificationType xsharpRegionStartType;
        internal IClassificationType xsharpRegionStopType;


        public void RegionExitEveryRule([NotNull] ParserRuleContext context)
        {
            try
            {
                if ((context is XSharpParser.Namespace_Context) ||
                    (context is XSharpParser.Class_Context) ||
                    (context is XSharpParser.Structure_Context) ||
                    (context is XSharpParser.Interface_Context) ||
                    (context is XSharpParser.Enum_Context) ||
                    (context is XSharpParser.PropertyContext) ||
                    (context is XSharpParser.PropertyAccessorContext))
                {
                    // already done
                    // BEGIN         NAMESPACE .... END NAMESPACE 
                    //
                    TagRegion(context, context.ChildCount - 2);
                }
                else if ((context is XSharpParser.FunctionContext) ||
                        (context is XSharpParser.ProcedureContext) ||
                        (context is XSharpParser.MethodContext) ||
                        (context is XSharpParser.ClsctorContext) ||
                        (context is XSharpParser.ClsdtorContext))
                {
                    // Put a region up to the end of the Entity
                    TagRegion(context, context.ChildCount - 1);
                }
                else if (context is XSharpParser.IdentifierContext)
                {
                    LanguageService.SyntaxTree.IToken sym = context.Start;
                    // Add tag for Keyword that is used as Identifier
                    if (XSharpLexer.IsKeyword(sym.Type))
                    {
                        TextSpan tokenSpan;
                        tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                        tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpIdentifierType));
                    }
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("RegionExitEveryRule : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        private void TagRegion(ParserRuleContext context, int endChild)
        {
            var endToken = context.GetChild(endChild);
            if (endToken is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
            {
                LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)endToken).Symbol;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
            }
            else if (endToken is LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext)
            {
                XSharpParser.StatementBlockContext lastTokenInContext = endToken as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
            }
        }

    }
}
