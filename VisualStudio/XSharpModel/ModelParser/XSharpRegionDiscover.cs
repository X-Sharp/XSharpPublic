//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using Microsoft.VisualStudio.Text.Classification;
using LanguageService.SyntaxTree.Tree;
using Microsoft.VisualStudio.Text;

namespace XSharpModel
{
    partial class XSharpModelRegionDiscover : XSharpBaseListener
    {
        internal List<ClassificationSpan> tags = new List<ClassificationSpan>();
        public ITextSnapshot Snapshot { get; set; }
        public bool BuildRegionTags { get; internal set; }
        public bool BuildLocals
        {
            get
            {
                return _buildLocals;
            }

            internal set
            {
                _buildLocals = value;
            }
        }
        private bool _reInitModel;
        private bool _buildModel;
        private bool _buildLocals = true;

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

        internal IClassificationType xsharpRegionStartType;
        internal IClassificationType xsharpRegionStopType;


        public void RegionExitEveryRule([NotNull] ParserRuleContext context)
        {
            // we can probably move most of this to the Enter and Exit methods
            // for each type and member rule as well.
            try
            {
                if ((context is XSharpParser.Namespace_Context) ||
                    (context is XSharpParser.Class_Context) ||
                    (context is XSharpParser.Interface_Context) ||
                    (context is XSharpParser.Structure_Context) ||
                    (context is XSharpParser.Enum_Context) ||
                    (context is XSharpParser.PropertyContext) ||
                    (context is XSharpParser.PropertyAccessorContext) ||
                    (context is XSharpParser.Event_Context) ||
                    (context is XSharpParser.EventAccessorContext) )
                {
                    // already done
                    // BEGIN         NAMESPACE .... END NAMESPACE 
                    // use -2 because these rules all end with "END <something>"
                    TagRegion(context, context.ChildCount - 2);     
                }
                else if ((context is XSharpParser.VostructContext) ||
                    (context is XSharpParser.VounionContext) )
                {
                    TagRegion(context, context.ChildCount-1);
                }
                else if (context is XSharpParser.StatementBlockContext)
                {
                    TagRegion(context.Parent, context.Parent.ChildCount-1);
                }
                else if (context is XSharpParser.SwitchStmtContext)
                {
                    TagRegion(context, context.ChildCount - 2);
                }
                else if (context is XSharpParser.CaseStmtContext)
                {
                    TagRegion(context, context.ChildCount - 2);
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("RegionExitEveryRule : Error Walking {0}, at {1}/{2} : " + ex.Message, this.File.Name, context.Start.Line, context.Start.Column);
            }
        }

        private void TagRegion(RuleContext _context, int endChild)
        {
            try
            {
                var context = _context as XSharpParserRuleContext;
                var endToken = context.GetChild(endChild);
                if (endToken is TerminalNodeImpl)
                {
                    IToken sym = ((TerminalNodeImpl)endToken).Symbol;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
                }
                else if (endToken is XSharpParser.StatementBlockContext)
                {
                    XSharpParser.StatementBlockContext lastTokenInContext = endToken as XSharpParser.StatementBlockContext;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
                }
                else if (endToken is ParserRuleContext)
                {
                    var lastTokenInContext = endToken as ParserRuleContext;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                    tags.Add(tokenSpan.ToClassificationSpan(Snapshot, xsharpRegionStopType));
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine("Tagregion failed: " + e.Message);
            }
        }
    }
}
