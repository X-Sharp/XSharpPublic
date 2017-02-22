/*
   Copyright 2016-2017 XSharp B.V.

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
using System.Collections.Generic;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;


namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    /// <summary>
    /// Base class for rule tokens
    /// </summary>
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal abstract class PPRuleToken
    {
        protected PPToken _token;
        protected PPTokenType _type;
        internal string Key { get { return _token.Text; } }
        internal PPToken Token { get { return _token; } }
        internal int Index { get; set; }

        internal bool IsOptional()
        {
            return _type.IsOptional();
        }
        internal bool Matched { get { return _type.IsMatched(); } set { _type |= PPTokenType.Matched; } }
        internal bool IsMarker { get { return (_type & PPTokenType.TypeMask) != PPTokenType.Token; } }
        internal PPTokenType RuleTokenType { get { return _type.GetTokenType(); } set { _type = value; } }
        internal string GetDebuggerDisplay()
        {
            return _type.GetTokenType().ToString() + " " + SyntaxText;
        }

        internal string SyntaxText
        {

            get
            {
                var type = _type.GetTokenType();
                switch (type)
                {
                    case PPTokenType.Token:
                        return Key;
                    case PPTokenType.MatchRegular:
                        return "<" + Key + ">";
                    case PPTokenType.MatchList:
                        return "<" + Key + ",...>";
                    case PPTokenType.MatchExtended:
                        return "<(" + Key + ")>";
                    case PPTokenType.MatchRestricted:
                        return "<" + Key + ":...>";
                    case PPTokenType.MatchWild:
                        return "<*" + Key + "*>";
                    case PPTokenType.ResultRegular:
                        return "<" + Key + ">";
                    case PPTokenType.ResultDumbStringify:
                        return "#<" + Key + ">";
                    case PPTokenType.ResultNormalStringify:
                        return "<\"" + Key + "\">";
                    case PPTokenType.ResultSmartStringify:
                        return "<(" + Key + ")>";
                    case PPTokenType.ResultBlockify:
                        return "<{" + Key + "}>";
                    case PPTokenType.ResultLogify:
                        return "<." + Key + ".>";
                }
                return "<" + Key + ">";
            }
        }

        internal PPRuleToken(PPToken token, PPTokenType type)
        {
            _token = token;
            _type = type;
            Index = -1;
        }
    }

    /// <summary>
    /// UDC Match tokens.
    /// Can be a normal literal token or a special Match Marker
    /// Match markers do not have to have a Result marker.
    /// </summary>

    internal class PPMatchToken : PPRuleToken
    {
        internal bool HasChildren { get { return _children?.Length > 0; } }
        protected PPToken[] _children = null;
        // Restricted and Optional Markers may have more than one token
        // For restricted tokens this contains the list of possible match values
        internal PPToken[] Children { get { return _children; } set { _children = value; } }
        // For optional tokens this contains the list of tokens inside the option block
        internal PPMatchToken[] _elements = null;
        internal PPMatchToken[] Elements { get { return _elements; } set { _elements = value; } }
        internal PPMatchToken(PPToken token, PPTokenType type) : base(token, type)
        {

        }

    }

    /// <summary>
    /// UDC Result tokens.
    /// Can be a normal literal token or a special Result Marker
    /// For result markers we set the property Matched to indicate that a matching 
    /// Match marker is found. Unmatched Result markers make a UDC invalid
    /// </summary>
    internal class PPResultToken : PPRuleToken
    {
        internal PPMatchToken MatchMarker;
        internal PPResultToken(PPToken token, PPTokenType type, bool nested) : base(token, type)
        {
            if (nested)
            {
                type |= PPTokenType.Nested;
            }
            MatchMarker = null;
        }
    }

}
