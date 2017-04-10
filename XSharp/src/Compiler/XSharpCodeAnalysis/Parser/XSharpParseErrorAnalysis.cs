/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpParseErrorAnalysis : XSharpBaseListener
    {
        private XSharpParser _parser;
        private List<ParseErrorData> _parseErrors;

        private void checkMissingToken(IToken l, IToken r, ParserRuleContext context)
        {
            if (l != null && r == null)
            {
                ErrorCode err = ErrorCode.ERR_SyntaxError;
                object par = null;
                switch (l.Type)
                {
                    case XSharpLexer.LPAREN:
                        err = ErrorCode.ERR_CloseParenExpected;
                        break;
                    case XSharpLexer.LCURLY:
                        err = ErrorCode.ERR_RbraceExpected;
                        break;
                    case XSharpLexer.LBRKT:
                        err = ErrorCode.ERR_SyntaxError;
                        par = ']';
                        break;
                }
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = l;
                ParseErrorData errdata;
                if (par != null)
                    errdata = new ParseErrorData(anchor, err, par);
                else
                    errdata = new ParseErrorData(anchor, err);
                _parseErrors.Add(errdata);
            }
        }

        private void checkMissingKeyword(IToken endToken, ParserRuleContext context, string msg)
        {
            if (endToken == null)
            {
                var err = ErrorCode.ERR_SyntaxError;
                IToken anchor = context.Stop;
                if (anchor == null)
                    anchor = context.Start;
                var errdata = new ParseErrorData(anchor, err, msg);
                _parseErrors.Add(errdata);
            }
            return ;
        }

        public XSharpParseErrorAnalysis(XSharpParser parser, List<ParseErrorData> parseErrors)
        {
            _parser = parser;
            _parseErrors = parseErrors;
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
            if (node.Symbol.TokenIndex == -1)
            {
                _parseErrors.Add(new ParseErrorData(node, ErrorCode.ERR_SyntaxError, node));
            }
        }
        public override void ExitEveryRule([NotNull] ParserRuleContext ctxt)
        {
            var context = ctxt as XSharpParserRuleContext;
            if (context.exception != null)
                _parseErrors.Add(new ParseErrorData(context, ErrorCode.ERR_SyntaxError, context));
        }

        // Check for missing end keywords for statement blocks
        public override void ExitWhileStmt([NotNull] XSharpParser.WhileStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END[DO]");
        }
        public override void ExitForStmt([NotNull] XSharpParser.ForStmtContext context)
        {
            checkMissingKeyword(context.e, context, "NEXT");
        }
        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            checkMissingKeyword(context.e, context, "NEXT");
        }
        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END[IF]");
        }
        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            checkMissingKeyword(context.CaseStmt?.Start, context, "CASE or OTHERWISE");
            checkMissingKeyword(context.e, context, "END[CASE]");
        }
        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END [TRY]");
        }
        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
             checkMissingKeyword(context.e, context, "END [SWITCH]");
        }
        public override void ExitSeqStmt([NotNull] XSharpParser.SeqStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END SEQUENCE");
        }

        public override void ExitBlockStmt([NotNull] XSharpParser.BlockStmtContext context)
        {
            checkMissingKeyword(context.e, context, "END [" + context.Key.Text + "]");
        }
    }
}
