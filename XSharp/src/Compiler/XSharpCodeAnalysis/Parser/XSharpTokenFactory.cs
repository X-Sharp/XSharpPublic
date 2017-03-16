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
    internal class XSharpTokenFactory : ITokenFactory
    {

        protected internal readonly bool copyText;
        internal static readonly ITokenFactory Default = new XSharpTokenFactory(false);
        internal XSharpTokenFactory() : this(false)
        {

        }
        internal XSharpTokenFactory(bool copyText) 
        {
            this.copyText = copyText;
        }

        public virtual XSharpToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
        {
            XSharpToken t = new XSharpToken(source, type, channel, start, stop);
            t.Line = line;
            t.Column = charPositionInLine;
            t.SourceFileName = source.Item1.SourceName;
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

        public virtual XSharpToken Create(int type, string text)
        {
            return new XSharpToken(type, text);
        }

        IToken ITokenFactory.Create(int type, string text)
        {
            return Create(type, text);
        }
    }
}
