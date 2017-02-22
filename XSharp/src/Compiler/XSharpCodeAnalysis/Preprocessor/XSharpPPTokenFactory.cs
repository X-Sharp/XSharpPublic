using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpPPTokenFactory : ITokenFactory
    {

        protected internal readonly bool copyText;
        internal static readonly ITokenFactory Default = new XSharpPPTokenFactory(false);
        internal XSharpPPTokenFactory() : this(false)
        {

        }
        internal XSharpPPTokenFactory(bool copyText) 
        {
            this.copyText = copyText;
        }

        public virtual PPToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
        {
            PPToken t = new PPToken(source, type, channel, start, stop);
            t.Line = line;
            t.Column = charPositionInLine;
            if (text != null)
            {
                t.Text = text;
            }
            else
            {
                if (copyText && source.Item2 != null)
                {
                    t.Text = source.Item2.GetText(Interval.Of(start, stop));
                }
            }
            return t;
        }

        IToken ITokenFactory.Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
        {
            return Create(source, type, text, channel, start, stop, line, charPositionInLine);
        }

        public virtual PPToken Create(int type, string text)
        {
            return new PPToken(type, text);
        }

        IToken ITokenFactory.Create(int type, string text)
        {
            return Create(type, text);
        }
    }
}
