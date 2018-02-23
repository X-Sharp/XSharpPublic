/*
   Copyright 2016-2018 XSharp B.V.

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
