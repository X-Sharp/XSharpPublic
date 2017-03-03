using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    public class XSharpToken : CommonToken, Microsoft.CodeAnalysis.IMessageSerializable
    {
        internal string SourceFileName;
        internal string MappedFileName;
        internal int MappedLine = -1;
        internal XSharpToken SourceSymbol;
        private XSharpToken _original = null;

        internal XSharpToken(IToken t) : base(t)
        {
            if (t is XSharpToken && t != this)
            {
                _original = ((XSharpToken)t).Original;
            }
        }
        internal XSharpToken(IToken t, int type, string text) : base(t)
        {
            Type = type;
            Text = text;
        }
        internal XSharpToken(int type, string text) : base(type, text)
        {
        }
        internal XSharpToken(int type) : base(type)
        {
        }
        internal XSharpToken(Tuple<ITokenSource, ICharStream> source, int type, int channel, int start, int stop) :
            base(source, type, channel, start, stop)
        {

        }

        internal XSharpToken Original
        {
            get
            {
                if (_original != null)
                    return _original;
                return this;
            }
        }

    }
}
