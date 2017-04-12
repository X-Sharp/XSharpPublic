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
        #region Fields
        protected XSharpToken _token;
        protected PPTokenType _type;
        protected string _key;
        #endregion
        #region Properties
        internal string Key { get { return _key; } }
        internal XSharpToken Token { get { return _token; } }
        internal int Index { get; set; }
        internal bool IsMarker { get { return (PPTokenType)((int)_type & 0x0F) != PPTokenType.Token; } }
        internal bool IsOptional { get { return _type.IsOptional(); } }
        internal bool IsToken { get { return _type == PPTokenType.Token; } }
        internal bool IsRepeat { get; set; }
        internal PPTokenType RuleTokenType { get { return _type.GetTokenType(); } set { _type = value; } }

        internal string SyntaxText
        {

            get
            {
                var type = _type.GetTokenType();
                string sResult;
                switch (type)
                {
                    case PPTokenType.Token:
                        sResult = Key;
                        break;
                    case PPTokenType.MatchRegular:
                        sResult = "<" + Key + ">";
                        break;
                    case PPTokenType.MatchList:
                        sResult = "<" + Key + ",...>";
                        break;
                    case PPTokenType.MatchExtended:
                        sResult = "<(" + Key + ")>";
                        break;
                    case PPTokenType.MatchRestricted:
                        sResult = "<" + Key + ":...>";
                        break;
                    case PPTokenType.MatchWild:
                        sResult = "<*" + Key + "*>";
                        break;
                    case PPTokenType.ResultRegular:
                        sResult = "<" + Key + ">";
                        break;
                    case PPTokenType.ResultDumbStringify:
                        sResult = "#<" + Key + ">";
                        break;
                    case PPTokenType.ResultNormalStringify:
                        sResult = "<\"" + Key + "\">";
                        break;
                    case PPTokenType.ResultSmartStringify:
                        sResult = "<(" + Key + ")>";
                        break;
                    case PPTokenType.ResultBlockify:
                        sResult = "<{" + Key + "}>";
                        break;
                    case PPTokenType.ResultLogify:
                        sResult = "<." + Key + ".>";
                        break;
                    default:
                        sResult = "<" + Key + ">";
                        break;
                }
                if (this.IsOptional)
                {
                    sResult = '[' + sResult + ']';
                }
                return sResult;
            }
        }


        #endregion
        internal string GetDebuggerDisplay()
        {
            return _type.GetTokenType().ToString() + " " + SyntaxText;
        }

        internal PPRuleToken(XSharpToken token, PPTokenType type)
        {
            _token = token;
            _key = token.Text;
            _type = type;
            Index = -1;
            IsRepeat = false;
        }
    }

    /// <summary>
    /// UDC Match tokens.
    /// Can be a normal literal token or a special Match Marker
    /// Match markers do not have to have a Result marker.
    /// </summary>

    internal class PPMatchToken : PPRuleToken
    {
        #region Properties
       // Restricted and Optional Markers may have more than one token
        // For restricted tokens this contains the list of possible match values
        // For List and Repeated match markers the Tokens list contains the list of 
        // tokens that may the end of the list
        internal XSharpToken[] Tokens { get; set; }
        // For optional tokens this contains the list of tokens inside the option block
        internal PPMatchToken[] Children { get; set; }
       #endregion
        internal PPMatchToken(XSharpToken token, PPTokenType type) : base(token, type)
        {
            Children = null;
        }
        internal PPMatchToken(XSharpToken token, PPTokenType type, string key) : this(token, type)
        {
            _key = key;
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
        #region Properties
        internal PPMatchToken MatchMarker { get; set; }
        internal PPResultToken[] OptionalElements { get; set; }
        #endregion
        internal PPResultToken(XSharpToken token, PPTokenType type) : base(token, type)
        {
            MatchMarker = null;
        }
        internal PPResultToken(XSharpToken token, PPTokenType type, string key) : this(token, type)
        {
            _key = key;
        }

    }

}
