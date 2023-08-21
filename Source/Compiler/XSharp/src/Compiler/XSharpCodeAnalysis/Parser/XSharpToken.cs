//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Antlr4.Runtime;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{

    public class XSharpPPToken : XSharpToken
    {

        readonly XSharpToken sourceSymbol;

        internal XSharpPPToken(int type, string text, XSharpToken token, bool defaultChannel = false) : base(token, type, text)
        {
            this.sourceSymbol = token;
            if (defaultChannel)
                Channel = XSharpLexer.DefaultTokenChannel;
        }
        internal XSharpPPToken(XSharpToken token, XSharpToken source, bool defaultChannel = false) : base(token)
        {
            this.sourceSymbol = source;
            if (defaultChannel)
                Channel = XSharpLexer.DefaultTokenChannel;
        }
        internal override XSharpToken SourceSymbol => sourceSymbol;
    }


    [DebuggerDisplay("{DebuggerString(),nq} ")]
    public class XSharpToken : CommonToken, IFormattable
    {
        //internal string SourceFileName;
        public bool FromInclude { get; set; } = false;
        internal int OriginalChannel;
        private int _originalTokenIndex = -1;
        private XSharpToken _original = null;

        #region Wrappers for compatibility with Macro compiler
        internal string Value

        {
            get => Text;
            set => Text = value;
        }
        internal int Start => Position;
        internal int End => Position + Text.Length;
        #endregion
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
        public string TextWithTrivia => TriviaAsText + Text;
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
            Type = token.Type;
            Text = token.Text;
            Line = token.Line;
            Column = token.Column;
            source = token.source;
            StartIndex = token.StartIndex;
            StopIndex = token.StopIndex;
            CopyCustomFields(token);
        }

        private void CopyCustomFields(XSharpToken token)
        {
            Trivia = token.Trivia;
            FromInclude = token.FromInclude;
        }

        internal XSharpToken(IToken t) : base(t)
        {
            if (t is XSharpToken xt && t != this)
            {
                CopyCustomFields(xt);
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
        public string SourceName => Original.TokenSource?.SourceName;
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
        public int FullWidth => Text != null ? Text.Length : 1;
        public int Position => StartIndex;
        public override int TokenIndex
        {
            get => base.TokenIndex;
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

        /// <summary>
        /// When the token comes from a UDC then this is the UDC node
        /// </summary>
        internal virtual XSharpToken SourceSymbol => null;

        public string ToString(string format, IFormatProvider formatProvider)
        {
            return this.Text;
        }
        internal void CopyTokenSource(XSharpToken token)
        {
            this.source = token.source;
        }
#if VSPARSER
        public XSharpToken UDCLocation {get; set;}
        public byte UDCType { get; set; }
#endif
    }
}
