//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
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
        private readonly XSharpParserRuleContext _context;

        internal XSharpSourceLocation(XSharpParserRuleContext context)
        {
            var start = context.Start as XSharpToken;
            var stop = context.Stop as XSharpToken;
            if (start.SourceSymbol != null)
                start = start.SourceSymbol;
            if (stop.SourceSymbol != null)
                stop = stop.SourceSymbol;

            if (stop.Type == XSharpLexer.Eof)
            {
                var cu = context.CompilationUnit;
                if (cu != null)
                {
                    var stream = cu.XTokens as BufferedTokenStream;
                    var tokens = stream.GetTokens();
                    var last = tokens.Count - 1;
                    if (tokens[last].Type == XSharpLexer.Eof && last > 0)
                    {
                        stop = (XSharpToken) tokens[last - 1];
                    }
                }
            }
            var width = stop.StopIndex - start.StartIndex;
            var lp1 = new LinePosition(start.Line - 1, start.Column);
            var lp2 = new LinePosition(stop.Line - 1, stop.Column + stop.Text.Length - 1);
            _context = context;
            _sourceSpan = new TextSpan(start.StartIndex, width);
            _lineSpan = new FileLinePositionSpan(context.SourceFileName, new LinePositionSpan(lp1, lp2));

        }


        public override TextSpan SourceSpan
        {
            get
            {
                return _sourceSpan;
            }
        }

        public override FileLinePositionSpan GetLineSpan()
        {
            return _lineSpan;
        }

        public override FileLinePositionSpan GetMappedLineSpan()
        {
            return _lineSpan;
        }

        public override LocationKind Kind
        {
            get
            {
                return LocationKind.SourceFile;
            }
        }

        public override bool Equals(object obj)
        {
            return this.Equals(obj as XSharpSourceLocation);
        }

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
