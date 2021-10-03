//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    [DebuggerDisplay("{DebuggerString(),nq} ")]
    public class XSharpToken : CommonToken, IFormattable
    {
        //internal string SourceFileName;
        internal string MappedFileName;
        internal int OriginalChannel;
        private int _originalTokenIndex = -1; 
        internal int MappedLine = -1;
        internal XSharpToken SourceSymbol;
        private XSharpToken _original = null;
        public string XmlComments
        {
            get
            {
                if (HasTrivia)
                {
                    var sb = new StringBuilder();
                    foreach (var t in Trivia)
                    {
                        if (t.Channel == XSharpLexer.XMLDOCCHANNEL)
                        {
                            sb.AppendLine(t.Text.Trim());
                        }
                    }
                    return sb.ToString();
                }
                return "";

            }

        }
        internal string DebuggerString()
        {
            var result = XSharpLexer.DefaultVocabulary.GetDisplayName(Type) + " " + Text.Trim();
            if (Channel != Lexer.DefaultTokenChannel)
            {
                result += " (" + Channel.ToString() + ")";
            }
            return result;
        }
        public bool HasXmlComments => HasTrivia && Trivia.Any(t => t.Channel == XSharpLexer.XMLDOCCHANNEL);
        public IList<XSharpToken> Trivia { get; set; } = null;
        public bool HasTrivia => Trivia?.Count > 0;
        public string TextWithTrivia
        {
            get
            {
                return TriviaAsText + Text;
            }
        }
        public string TriviaAsText
        {
            get
            {
                if (HasTrivia)
                {
                    var sb = new StringBuilder();
                    foreach (var tr in Trivia)
                    {
                        sb.Append(tr.Text);
                    }
                    return sb.ToString();
                }
                return string.Empty;
            }
        }
        public bool IsTrivia => Channel == TokenConstants.HiddenChannel || Channel == XSharpLexer.XMLDOCCHANNEL;
        public bool CanHaveTrivia => Channel == TokenConstants.DefaultChannel || Channel == XSharpLexer.PREPROCESSORCHANNEL;
        private void copyToken(XSharpToken token)
        {
            type = token.Type;
            text = token.Text;
            line = token.Line;
            Column = token.Column;
            source = token.source;
            StartIndex = token.StartIndex;
            StopIndex = token.StopIndex;
            Trivia = token.Trivia;
        }
        internal XSharpToken(IToken t) : base(t)
        {
            if (t is XSharpToken xt && t != this)
            {
                copyToken(xt);
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
        internal XSharpToken(int type, XSharpToken token) : base(type)
        {
            copyToken(token);
            this.type = type;
            SourceSymbol = token;

        }
        internal XSharpToken(int type, string text, XSharpToken token) : base(type, text)
        {
            copyToken(token);
            this.type = type;
            this.text = text;
            SourceSymbol = token;
        }

        internal XSharpToken(Tuple<ITokenSource, ICharStream> source, int type, int channel, int start, int stop) :
            base(source, type, channel, start, stop)
        {
            OriginalChannel = channel;
        }

        public XSharpToken Original
        {
            get
            {
                // There could be several replacements, so walk up the tree
                var org = _original;
                while (org != null && org.Original != null && org.Original != org)
                {
                    org = org.Original;
                }
                if (org != null)
                {
                    return org;
                }
                return this;
            }
            set
            {
                _original = value;
            }
        }
        public string SourceName
        {
            get
            {
                return Original.TokenSource?.SourceName;
            }
        }
        public override int Line
        {
            get
            {
                var org = this.Original;
                if (org == this)
                    return base.Line;
                return org.Line;
            }
        }
        public int FullWidth
        {
            get
            {
                if (StopIndex > StartIndex)
                    return StopIndex + StartIndex + 1;
                return 1;
            }
        }
        public int Position => StartIndex;
        public override int TokenIndex
        {
            get
            {
                return base.TokenIndex;
            }
            set
            {
                base.TokenIndex = value;
                if (_originalTokenIndex == -1)
                {
                    _originalTokenIndex = value;
                }
            }
        }

        public int OriginalTokenIndex => _originalTokenIndex;

        public string ToString(string format, IFormatProvider formatProvider)
        {
            return this.Text;
        }
    }
}
