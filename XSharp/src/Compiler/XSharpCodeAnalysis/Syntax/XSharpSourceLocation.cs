//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.IO;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{

    /// <summary>
    /// A program location in source code.
    /// </summary>
    internal sealed class XSharpSourceLocation : Location, IEquatable<XSharpSourceLocation>
    {
        private readonly TextSpan _sourceSpan;
        private readonly FileLinePositionSpan _lineSpan;
        private readonly SyntaxTree _syntaxTree;

        void GetSpans(IToken start, IToken end, out FileLinePositionSpan lineSpan, out TextSpan sourceSpan)
        {
            if (start.Type == XSharpLexer.EOF && end.Type == XSharpLexer.EOF)
            {
                var lp = new LinePosition(0, 0);
                sourceSpan = new TextSpan(start.StartIndex, 0);
                lineSpan = new FileLinePositionSpan(start.InputStream.SourceName, new LinePositionSpan(lp, lp));
                return;
            }
            var lp1 = new LinePosition(start.Line - 1, start.Column);
            var lp2 = new LinePosition(end.Line - 1, end.Column + end.Text.Length - 1);
            var width = end.StopIndex - start.StartIndex;
            sourceSpan = new TextSpan(start.StartIndex, width);
            lineSpan = new FileLinePositionSpan(start.InputStream.SourceName, new LinePositionSpan(lp1, lp2));
        }

        internal XSharpSourceLocation(IToken token, SyntaxTree syntaxTree)
        {
            _syntaxTree = syntaxTree;
            GetSpans(token, token, out _lineSpan, out _sourceSpan);
        }
        internal XSharpSourceLocation(XSharpParserRuleContext context, SyntaxTree syntaxTree)
        {
            _syntaxTree = syntaxTree;
            GetSpans(context.Start, context.Stop, out _lineSpan, out _sourceSpan);
        }
        public override SyntaxTree SourceTree => _syntaxTree;
        public override TextSpan SourceSpan => _sourceSpan;
        public override FileLinePositionSpan GetLineSpan() => _lineSpan;
        public override FileLinePositionSpan GetMappedLineSpan() => _lineSpan;
        public override LocationKind Kind => LocationKind.SourceFile;
        public override bool Equals(object obj) => this.Equals(obj as XSharpSourceLocation);
        public bool Equals(XSharpSourceLocation obj)
        {
            if (ReferenceEquals(obj, this))
            {
                return true;
            }

            return obj != null
                && _sourceSpan == obj._sourceSpan
                && _lineSpan.Equals(obj._lineSpan);
        }

        public override int GetHashCode()
        {
            return Hash.Combine(_lineSpan.GetHashCode(), _sourceSpan.GetHashCode());
        }
    }
}
