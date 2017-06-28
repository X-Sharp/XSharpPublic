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
using Microsoft.VisualStudio.Text;
using System.Collections.Immutable;

namespace XSharpColorizer
{
    partial class XSharpRegionDiscover : XSharpBaseListener
    {
        private readonly List<ClassificationSpan> _regionTags;
        private readonly ITextSnapshot _snapshot;
        private bool _hasPositionalKeyword = false;
        public bool HasPositionalKeyword => _hasPositionalKeyword;
 
        public XSharpRegionDiscover(ITextSnapshot snapshot) : base()
        {
            // To store intermediate declarations
            this._regionTags = new List<ClassificationSpan>();
            _snapshot = snapshot;
            _hasPositionalKeyword = false;

        }
        internal IClassificationType xsharpRegionStartType;
        internal IClassificationType xsharpRegionStopType;
 
        #region Entities with END ...
        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitPropertyAccessor([NotNull] XSharpParser.PropertyAccessorContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitEvent_([NotNull] XSharpParser.Event_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }

        public override void ExitEventAccessor([NotNull] XSharpParser.EventAccessorContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        #endregion
        #region Entities without End ..
        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            TagRegion(context, context.ChildCount - 1);
        }
        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            TagRegion(context, context.ChildCount - 1);
        }
        #endregion
 
        #region Statements with block
        public override void ExitStatementBlock([NotNull] XSharpParser.StatementBlockContext context)
        {
            TagRegion(context.Parent, context.Parent.ChildCount - 1);
        }
        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        #endregion

        public override void ExitIdentifier([NotNull] XSharpParser.IdentifierContext context)
        {
            XSharpToken token = null;
            // Mark the Lexer token as ID for Keywords used as Identifier
            // so it gets colored properly in the editor
            if (context.VnToken != null)
            {
                token = context.VnToken.Token as XSharpToken;
            }
            else if (context.XsToken != null)
            {
                token = context.XsToken.Token as XSharpToken;
            }
            if (token != null)
            {
                _hasPositionalKeyword = true;
                token.Original.Type = XSharpLexer.ID;
            }
        }

        private void TagRegion(RuleContext _context, int endChild)
        {
            try
            {
                var context = _context as XSharpParserRuleContext;
                var endToken = context.GetChild(endChild);
                if (endToken is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
                {
                    LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)endToken).Symbol;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                }
                else if (endToken is LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext)
                {
                    XSharpParser.StatementBlockContext lastTokenInContext = endToken as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(lastTokenInContext.Stop.StopIndex - 1, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                }
                else if (endToken is ParserRuleContext)
                {
                    var lastTokenInContext = endToken as ParserRuleContext;
                    var tokenSpan = new TextSpan(context.Start.StartIndex, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                    tokenSpan = new TextSpan(lastTokenInContext.Stop.StartIndex - 1, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                }
            }
            catch (Exception e)
            {
                XSharpClassifier.Debug("Tagregion failed: " + e.Message);
            }
        }

        public IImmutableList<ClassificationSpan>  GetRegionTags()
        {
            return _regionTags.ToImmutableList();
        }
    }
}
