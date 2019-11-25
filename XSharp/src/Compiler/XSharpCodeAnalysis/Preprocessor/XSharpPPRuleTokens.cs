//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
        internal bool IsMarker
        {
            get
            {
                switch (_type)
                {
                    case PPTokenType.Token:
                    case PPTokenType.None:
                        return false;
                    default:
                        return true;
                }
            }
        }
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
        internal int Index { get; set; } // Index in the flattened matchmarker list
        #endregion
        internal PPMatchToken(XSharpToken token, PPTokenType type) : base(token, type)
        {
            Children = null;
            Index = -1;
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
