//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Antlr4.Runtime;
using System;
using System.Collections.Generic;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    internal class XSharpTokenFactory : ITokenFactory
    {
        internal static readonly ITokenFactory Instance = new XSharpTokenFactory();
        private XSharpTokenFactory() { }
        public virtual XSharpToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
        {
            var t = new XSharpToken(source, type, channel, start, stop);
            t.Line = line;
            t.Column = charPositionInLine;
            //t.SourceFileName = source.Item1.SourceName;
            if (text != null)
            {
                t.Text = text;
            }
            return t;
        }

        IToken ITokenFactory.Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
        {
            return Create(source, type, text, channel, start, stop, line, charPositionInLine);
        }

        public virtual XSharpToken Create(int type, string text)
        {
            return new XSharpToken(type, text);
        }
        IToken ITokenFactory.Create(int type, string text)
        {
            return Create(type, text);
        }
    }
    public class XSharpListTokenSource : ListTokenSource
    {
        public XSharpListTokenSource(XSharpLexer lexer, IList<IToken> tokens)
           : base(tokens)
        {
            this.TokenFactory = lexer.TokenFactory;
        }
        public XSharpListTokenSource(XSharpLexer lexer, IList<IToken> tokens, string sourceName)
           : base(tokens,  sourceName)
        {
            this.TokenFactory = lexer.TokenFactory;
        }
    }
}
