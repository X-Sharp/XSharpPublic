//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpErrorStrategy : DefaultErrorStrategy
    {
        internal XSharpErrorStrategy() : base()
        {

        }
        protected internal override void ReportUnwantedToken(Parser recognizer)
        {
            if (InErrorRecoveryMode(recognizer))
            {
                return;
            }
            BeginErrorCondition(recognizer);
            IToken t = recognizer.CurrentToken;
            string tokenName = GetTokenErrorDisplay(t);
            IntervalSet expecting = GetExpectedTokens(recognizer);
            string msg;
            if (expecting.Count <= 4)
            {
                msg = "unexpected input " + tokenName + " expecting " + expecting.ToString(recognizer.Vocabulary);
            }
            else
            {
                msg = "unexpected input " + tokenName;
                string missing = null;
                // unexpected closing tokens often indicate a missing opening token
                switch (t.Type)
                {
                    case XSharpParser.RPAREN:
                        missing = "'('" ;
                        break;
                    case XSharpParser.RCURLY:
                        missing = "'{'";
                        break;
                    case XSharpParser.RBRKT:
                        missing = "'['";
                        break;
                    case XSharpParser.EOS:
                        missing = " closing ')' or '}'";
                        break;
                }
                if (!String.IsNullOrEmpty(missing))
                {
                    msg += Missing(missing);
                }
            }
            recognizer.NotifyErrorListeners(t, msg, null);
        }
        protected internal override void ReportInputMismatch(Parser recognizer, InputMismatchException e)
        {
            IntervalSet expecting = GetExpectedTokens(recognizer);
            string msg;
            if (expecting.Count <= 4)
            {
                msg = "mismatched input " + GetTokenErrorDisplay(e.OffendingToken) + " expecting " + expecting.ToString(recognizer.Vocabulary);
            }
            else
            {
                msg = "mismatched input " + GetTokenErrorDisplay(e.OffendingToken) ;
                if (e.OffendingToken.Type == XSharpParser.EOS)
                {
                    msg += Missing("closing ')' or '}'");
                }
            }
            NotifyErrorListeners(recognizer, msg, e);
        }
        protected internal override void ReportNoViableAlternative(Parser recognizer, NoViableAltException e)
        {
            ITokenStream tokens = ((ITokenStream)recognizer.InputStream);
            string input;
            if (tokens != null)
            {
                if (e.StartToken.Type == TokenConstants.Eof)
                {
                    input = "<EOF>";
                }
                else
                {
                    input = e.OffendingToken.Text;
                }
            }
            else
            {
                input = "<unknown input>";
            }
            string msg;
            char firsttoken = (Char) 0;
            char lasttoken = (Char)0;
            int count = 0;
            foreach (var c in input)
            {
                switch (c)
                {
                    case '[':
                    case '(':
                    case '{':
                        if (firsttoken == 0)
                        {
                            firsttoken = c;
                        }
                        count += 1;
                        break;
                    case ']':
                    case ')':
                    case '}':
                        lasttoken = c;
                        count -= 1;
                        break;
                }
            }
            if (e.OffendingToken.Type == XSharpLexer.EOS)
            {
                string eos;
                string missing  = " token";
                if (count > 0)
                {
                    switch (firsttoken)
                    {
                        case '(':
                            missing = "closing ')'";
                            break;
                        case '{':
                            missing = "closing '}'";
                            break;
                        case '[':
                            missing = "closing ']'";
                            break;
                    }
                }
                else if (count < 0)
                {
                    switch (lasttoken)
                    {
                        case ')':
                            missing = "opening '('";
                            break;
                        case '}':
                            missing = "opening '{'";
                            break;
                        case ']':
                            missing = "opening '['";
                            break;
                    }
                }
                if (e.OffendingToken.Text == "\r\n")
                    eos = "CRLF";
                else if (e.OffendingToken.Text == ";")
                    eos = "';'";
                else
                    eos = "End of Statement";
                msg = "unexpected " + eos +  Missing(missing);
            }
            else
            {
                if (input.Length > 50)  
                    input = input.Substring(0, 50);      
                msg = "unexpected input " + EscapeWSAndQuote(input);
            }
            NotifyErrorListeners(recognizer, msg, e);
        }
        protected internal override string EscapeWSAndQuote(string s)
        {
            //		if ( s==null ) return s;
            s = s.Replace("\r\n", "CRLF");
            s = s.Replace("\n", "LINEFEED");
            s = s.Replace("\r", "RETURN");
            s = s.Replace("\t", "TAB");
            return "'" + s + "'";
        }
        protected string Missing (string missing)
        {
            return ", are you missing a "+missing+" ?";
        }
        protected internal override void ConsumeUntil(Parser recognizer, IntervalSet set)
        {
            //Console.WriteLine("consumeUntil("+set.ToString()+")");
            int ttype = ((ITokenStream)recognizer.InputStream).La(1);
            while (ttype != TokenConstants.Eof && !set.Contains(ttype) && ttype != XSharpLexer.EOS)
            {
                var t = recognizer.Consume();
                //Console.WriteLine("consumeuntil:" + t.StartIndex.ToString()+":"+t.Text);
                ttype = ((ITokenStream)recognizer.InputStream).La(1);
            }
        }
        int nestedLevel = 0; 
        public override void Recover(Parser recognizer, RecognitionException e)
        {
            base.Recover(recognizer, e);
            if (nestedLevel < 25)
            {
                try
                {
                    nestedLevel++;
                    if (recognizer.Context is XSharpParser.StatementBlockContext ctx)
                    {
                        ConsumeToEos(recognizer);
                        ctx._Stmts.Add(new XSharpParser.NopStmtContext(new XSharpParser.StatementContext()));
                        var p = (recognizer as XSharpParser);
                        EndErrorCondition(recognizer);
                        {
                            p.State = ctx.invokingState;

                            var stmtBlk = p.statementBlock();
                            if (stmtBlk?._Stmts?.Count > 0)
                            {
                                foreach (var s in stmtBlk._Stmts)
                                {
                                    s.parent = ctx;
                                    ctx._Stmts.Add(s);
                                }
                            }
                        }
                    }
                }
                finally
                {
                    nestedLevel--;
                }
            }
        }
        public override void Sync(Parser recognizer)
        {
            base.Sync(recognizer);
        }
        public override IToken RecoverInline(Parser recognizer)
        {
            return base.RecoverInline(recognizer);
        }
        void ConsumeToEos(Parser recognizer)
        {
            var inp = ((ITokenStream)recognizer.InputStream);
            int ttype = inp.La(1);
            while (ttype != TokenConstants.Eof && ttype != XSharpLexer.EOS)
            {
                var t = recognizer.Consume();
                ttype = inp.La(1);
            }
            while (ttype == XSharpLexer.EOS)
            {
                var t = recognizer.Consume();
                ttype = inp.La(1);
            }
        }
    }
}
