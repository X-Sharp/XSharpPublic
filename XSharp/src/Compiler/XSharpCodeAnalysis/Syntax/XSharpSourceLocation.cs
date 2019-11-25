//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
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
            var lp1 = new LinePosition(context.Start.Line - 1, context.Start.Column);
            var lp2 = new LinePosition(context.Stop.Line - 1, context.Stop.Column + context.FullWidth - 1);
            _context = context;
            _sourceSpan = new TextSpan(context.Start.StartIndex, context.FullWidth);
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
