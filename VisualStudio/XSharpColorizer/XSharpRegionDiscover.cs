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
            if (!(context.Parent is XSharpParser.IfElseBlockContext) &&
                !(context.Parent is XSharpParser.CaseBlockContext) &&
                !(context.Parent is XSharpParser.TryStmtContext) &&
                !(context.Parent is XSharpParser.CatchBlockContext) &&
                !(context.Parent is XSharpParser.PropertyAccessorContext))
            {
                TagRegion(context.Parent, context.Parent.ChildCount - 1);
            }
            else if (context.Parent is XSharpParser.IfElseBlockContext)
            {
                var ctxt = context.Parent as XSharpParser.IfElseBlockContext;
                //
                if (ctxt.ElseIfBlock != null)
                {
                    // we have Count >= 3
                    // 0 : BinaryExpr
                    // 1 : EOS
                    // 2 : StatementBlock
                    //TagRegion(ctxt.ElseIfBlock, 2);
                    //
                    //// Search the ELSEIF block, if Any
                    int i = 0;
                    LanguageService.SyntaxTree.Tree.IParseTree token = null;
                    for (i = 0; i < ctxt.ChildCount; i++)
                    {
                        token = ctxt.GetChild(i);
                        String tokenText = token.GetText().ToUpper();
                        if (tokenText == "ELSEIF")
                        {
                            break;
                        }
                        else
                            token = null;
                    }
                    //
                    if (token is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
                    {
                        LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)token).Symbol;
                        var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                        _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                        //
                        var endToken = ctxt.ElseIfBlock.GetChild(2);
                        XSharpParser.StatementBlockContext lastTokenInContext = endToken as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                        tokenSpan = new TextSpan(lastTokenInContext.Stop.StopIndex - 1, 1);
                        _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                    }
                }
            }
        }
        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            TagRegion(context, context.ChildCount - 2);
        }
        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            // we have Count == 7
            // 0 : DO
            // 1 : CASE
            // 2 : Eos
            // 3 : CaseBlock
            // 4 : END
            // 5 : CASE
            // 6 : Eos
            TagRegion(context, context.ChildCount - 2);
        }

        public override void ExitCaseBlock([NotNull] XSharpParser.CaseBlockContext context)
        {
            // we have Count == 4
            // 0 : CASE
            // 1 : BinaryExpr
            // 2 : EOS
            // 3 : StatementBlock
            // 4 : CaseBlock <- Only if we have another CASE after

            // OR

            // 0 : OTHERWISE
            // 1 : EOS
            // 2 : StatementBlock

            int endIndex = 3;
            var startToken = context.GetChild(0);
            String token = startToken.GetText().ToUpper();
            if (token == "OTHERWISE")
            {
                endIndex = 2;
            }
            TagRegion(context, endIndex);
        }

        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            TagRegion(context, context.ChildCount - 2);
            //
            try
            {
                XSharpParser.StatementBlockContext elseBlock = null;
                XSharpParser.IfElseBlockContext stmt = null;
                if (context.IfStmt.ElseBlock != null)
                {
                    elseBlock = context.IfStmt.ElseBlock;
                    stmt = context.IfStmt;
                }
                else if (context.IfStmt.ElseIfBlock != null)
                {
                    if (context.IfStmt.ElseIfBlock.ElseBlock != null)
                    {
                        elseBlock = context.IfStmt.ElseIfBlock.ElseBlock;
                        stmt = context.IfStmt.ElseIfBlock;
                    }
                }
                //
                if (elseBlock != null)
                {
                    // Search the ELSE block, if Any
                    int i = 0;
                    LanguageService.SyntaxTree.Tree.IParseTree token = null;
                    for (i = 0; i < stmt.ChildCount; i++)
                    {
                        token = stmt.GetChild(i);
                        String tokenText = token.GetText().ToUpper();
                        if (tokenText == "ELSE")
                        {
                            break;
                        }
                        else
                            token = null;
                    }
                    //
                    if (token is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
                    {
                        LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)token).Symbol;
                        var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                        _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                        //
                        XSharpParser.StatementBlockContext lastTokenInContext = elseBlock as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                        tokenSpan = new TextSpan(lastTokenInContext.Stop.StopIndex - 1, 1);
                        _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                    }
                }
            }
            catch (Exception e)
            {
                XSharpClassifier.Debug("Tagregion failed: " + e.Message);
            }
        }

        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            TagRegion(context, context.ChildCount - 1);
            if (context.FinBlock != null)
            {
                // Search the ELSE block, if Any
                int i = 0;
                LanguageService.SyntaxTree.Tree.IParseTree token = null;
                for (i = 0; i < context.ChildCount; i++)
                {
                    token = context.GetChild(i);
                    String tokenText = token.GetText().ToUpper();
                    if (tokenText == "FINALLY")
                    {
                        break;
                    }
                    else
                        token = null;
                }
                //
                if (token is LanguageService.SyntaxTree.Tree.TerminalNodeImpl)
                {
                    LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)token).Symbol;
                    var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                    //
                    XSharpParser.StatementBlockContext lastTokenInContext = context.FinBlock as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.StatementBlockContext;
                    tokenSpan = new TextSpan(lastTokenInContext.Stop.StopIndex - 1, 1);
                    _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
                }
            }
        }

        public override void ExitCatchBlock([NotNull] XSharpParser.CatchBlockContext context)
        {
            // Here will have all the informations that are AFTER the CATCH keyword, but we need to include the keyword
            XSharpParser.TryStmtContext tryStmt = (XSharpParser.TryStmtContext)context.Parent;
            // Search the CatchBlock
            int i = 0;
            LanguageService.SyntaxTree.Tree.IParseTree token = null;
            for (i = 0; i < tryStmt.ChildCount; i++)
            {
                token = tryStmt.GetChild(i);
                if (token == context)
                {
                    // Ok, we found the current CatchBlock, so the Catch is the previous one
                    if ( i>0)
                    {
                        token = tryStmt.GetChild(i-1);
                        break;
                    }
                }
            }
            //
            if ( token != null )
            {
                LanguageService.SyntaxTree.IToken sym = ((LanguageService.SyntaxTree.Tree.TerminalNodeImpl)token).Symbol;
                var tokenSpan = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex + 1);
                _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStartType));
                //
                XSharpParser.CatchBlockContext lastTokenInContext = context as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.CatchBlockContext;
                tokenSpan = new TextSpan(lastTokenInContext.Stop.StopIndex - 1, 1);
                _regionTags.Add(tokenSpan.ToClassificationSpan(_snapshot, xsharpRegionStopType));
            }
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
                    // Attribute on top of Function/Method/... ?
                    if ( context.Start.Text == "[")
                    {
                        // Skip it
                        int newStart = getStatementForAttribute(_snapshot, context.Start.StartIndex);
                        tokenSpan = new TextSpan(newStart, 1);
                    }
                    //
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

        public IImmutableList<ClassificationSpan> GetRegionTags()
        {
            return _regionTags.ToImmutableList();
        }

        private int getStatementForAttribute(ITextSnapshot snapshot, int start)
        {
            char car;
            int currentPos = start;
            int pos = 0;
            char[] newLine = Environment.NewLine.ToCharArray();
            bool attributeOpen = true;
            bool continueNextLine = false;
            //
            do
            {
                car = snapshot[currentPos];
                // Open/Close Attribute ?
                if ( car == '[' )
                {
                    attributeOpen = true;
                }
                //
                if ( attributeOpen )
                {
                    if (car == ']')
                    {
                        attributeOpen = false;
                    }
                    // Skip all chars inside attribute def
                    currentPos++;
                }
                else
                {
                    // Semi Colon ?
                    if (car == ';')
                    {
                        continueNextLine = true;
                    }
                    else
                    {
                        // NewLine ?
                        if (car == newLine[pos])
                        {
                            if (pos == newLine.Length - 1)
                            {
                                if (!continueNextLine)
                                {
                                    break;
                                }
                                else
                                {
                                    pos = 0;
                                    continueNextLine = false;
                                }
                            }
                            else
                                pos++;
                        }
                        else
                        {
                            // No Space / Tab
                            if ((car != ' ') && (car != 9))
                            {
                                // Bye
                                start = currentPos;
                                break;
                            }
                        }
                    }
                    
                    // move to next
                    currentPos++;
                }
                //
            } while (currentPos < snapshot.Length);
            //
            return start;
        }
    }
}
