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
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;


namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    enum PPUDCType : byte
    {
        None,
        Define = 1,                 // #define
        Command = 2,                 // #command 
        Translate = 3,               // #translate 
        XCommand = 4,                // #xcommand
        XTranslate = 5,              // #xtranslate
    }
    enum PPTokenType : byte
    {
        None = 0,                          //                      0000 0000
        // Normal Token, part of UDC but with no special meaning
        Token = 1,                         //                      0000 0001
        MatchRegular = 2,                  // <idMarker>           0000 0010
        MatchList = 3,                     // <idMarker,...>       0000 0011
        MatchRestricted = 4,               // <idMarker:word list> 0000 0100
        MatchWild = 5,                     // <*idMarker*>         0000 0101
        MatchExtended = 6,                 // <(idMarker)>         0000 0110
        MatchOptional = 7,                 // [......]             0000 0111

        ResultRegular = 9,                 // <idMarker>           0000 1001
        ResultDumbStringify = 10,          // #<idMarker>          0000 1010
        ResultNormalStringify = 11,        // <"idMarker">         0000 1011
        ResultSmartStringify = 12,         // <(idMarker)>         0000 1100
        ResultBlockify = 13,               // <{idMarker}>         0000 1101
        ResultLogify = 14,                 // <.idMarker.>         0000 1110
        ResultOptional = 15,               // [....]               0000 1111

    }
    internal class PPErrorMessages : List<PPErrorMessage>
    {

    }
  
    internal class PPErrorMessage
    {
        IToken _token;          // location to link error to
        String _message;        // message
        internal IToken Token { get { return _token; } }
        internal String Message { get { return _message; } }
        internal PPErrorMessage(IToken token, String message)
        {
            _token = token;
            _message = message;
        }
    }


    [DebuggerDisplay("{GetCount()}")]
    internal class PPRules : List<PPRule>
    {
        internal PPRules(): base()
        {
        }
        internal int GetCount()
        {
            return base.Count;
        }
    }
    /// <summary>
    /// This class is a dictionary of 'First tokens' in a PP Rule with the matching rules
    /// New rules are inserted at the top of the list, so they get preference over existing
    /// rules.
    /// </summary>
    internal class PPRuleDictionary
    {
        Dictionary<String, PPRules> _rules;
        internal PPRuleDictionary()
        {
            _rules = new Dictionary<string, PPRules>(StringComparer.OrdinalIgnoreCase);
        }

        internal int Count
        {
            get
            {
                int result = 0;
                foreach (var r in _rules)
                {
                    result += r.Value.Count;
                }
                return result;
            }
        }

        internal void Add(PPRule rule)
        {
            // find element that matches the first token and insert at the front of the list
            // so rules defined later override rules defined first
            string key = rule.LookupKey;
            PPRules list;
            if (_rules.ContainsKey(key))
            {
                list = _rules[key];
            }
            else
            {
                list = new PPRules();
                _rules.Add(key, list);
            }
            list.Insert(0, rule);
        }
  
        internal PPRule FindMatchingRule(IList<XSharpToken> tokens, out PPMatchRange[] matchInfo)
        {
            matchInfo = null;
            if (tokens?.Count >= 0)
            {
                var firsttoken = tokens[0];
                var key = firsttoken.Text;
                while (true)
                {
                    if (_rules.ContainsKey(key))
                    {
                        // try to find the first rule in the list that matches our tokens
                        var rules = _rules[key];
                        foreach (var rule in rules)
                        {
                            if (rule.Matches(tokens, out matchInfo))
                            {
                                return rule;
                            }
                        }
                    }
                    if (key.Length <= 4)
                        return null;
                    key = key.Substring(0, 4);
                }

            }
            return null;
        }
    }

    /// <summary>
    /// This struct holds the start and end location of the tokens in the source 
    /// that match a match marker in a UDC
    /// Some special meanings are for:
    /// _start = 0 : Empty
    /// _start = -1: Missing optional token
    /// _start = -2: Token
    /// It may also hold a list of MatchRanges, which is the case for List markers
    /// or Repeated markers
    /// </summary>
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal struct PPMatchRange
    {
        #region Fields
        private int _length;
        private int _start;
        private bool _token;
        private IList<PPMatchRange> _children ;
        #endregion
        #region Properties
        internal bool IsList { get { return _children != null; } }
        internal int Start { get { return _start; } }
        internal int Length { get { return _length; } }
        internal int MatchCount
        {
            get
            {
                if (Empty)
                    return 0;
                if (_children == null)
                    return 1;
                else
                    return _children.Count;

            }
        }
        internal int End
        {
            get
            {
                if (_children?.Count > 0)
                    return _children[_children.Count - 1].End;
                else
                    return _start + _length - 1;
            }
        }
        internal IList<PPMatchRange> Children { get { return _children; } }

        internal bool Empty
        {
            get { return !_token && _length == 0; }
        }
        internal bool IsToken
        {
            get { return _token; }
        }

        internal void SetPos(int start, int end)
        {
            _token = false;
            if (Empty)
            {
                _start = start;
                _length = end - start + 1;
                _children = null;
            }
            else
            {
                _children = new List<PPMatchRange>();
                _children.Add(Create(Start, End));
                _children.Add(Create(start, end));
                _length = end - Start + 1;
            }
        }
        internal void SetToken(int pos)
        {
            if (Empty)
            {
                _token = true;
                _start = pos;
                _length = 1;
                _children = null;
            }
            else
            {
                _token = true;
                _children = new List<PPMatchRange>();
                _children.Add(Token(_start));
                _children.Add(Token(pos));
            }
        }
        internal void  SetSkipped()
        {
            _start = -1;
            _length = 0;
            _token = false;
            _children = null;
        }

        #endregion
        #region Constructors
        private static PPMatchRange Token(int pos)
        {
            return new PPMatchRange() { _token = true, _start = pos, _length = 1, _children = null };
        }
        private static PPMatchRange Create( int start, int end)
        {
            return new PPMatchRange() { _start = start, _length = end - start +1, _children = null};
        }
        #endregion
        internal string GetDebuggerDisplay()
        {
            if (_token)
            {
                if (_children != null)
                    return $"Token ({Children.Count}) {Start},{End}";
                else
                    return $"Token ({Start})";
            }
            if (_start == 0 && _length == 0)
                return "Empty";
            if (_start == -1 && _length == 0 )
                return "Skipped Optional marker";
            if (_children != null)
                return $"List ({Children.Count}) {Start},{End}";
            else
                return $"{Start},{End}"; 
        }
    }
    /// <summary>
    /// This class is used to monitor recursion for #Translate and #command in the Preprocessor
    /// </summary>
    internal class PPUsedRules
    {
        class PPUsedRule
        {
            PPRule _rule;
            IList<XSharpToken> _tokens;
            internal PPUsedRule(PPRule rule, IList<XSharpToken> tokens)
            {
                _rule = rule;
                _tokens = tokens;
            }
            internal bool isDuplicate(PPRule rule, IList<XSharpToken> tokens)
            {
                if (_rule == rule && _tokens.Count == tokens.Count)
                {
                    for (int i = 0; i < tokens.Count; i++)
                    {
                        var t1 = tokens[i];
                        var t2 = _tokens[i];
                        if (t1.Text != t2.Text)
                        {
                            return false;
                        }
                    }
                    return true;
                }
                return false;
            }
        }
        List<PPUsedRule> _list;
        XSharpPreprocessor _pp;
        int _maxDepth;
        internal PPUsedRules(XSharpPreprocessor pp, int maxDepth)
        {
            _list = new List<PPUsedRule>();
            _pp = pp;
            _maxDepth = maxDepth;
        }
        /// <summary>
        /// Check for recursion, and add the rule to the list of rules that have been used
        /// </summary>
        /// <param name="rule"></param>
        /// <param name="tokens"></param>
        /// <returns>True when the rule with the same tokens list is found in the list</returns>
        internal bool HasRecursion(PPRule rule, IList<XSharpToken> tokens)
        {
            // check to see if this is already there
            if (_list.Count == _maxDepth)
            {
                _pp.addParseError(new ParseErrorData(tokens[0], ErrorCode.ERR_PreProcessorRecursiveRule, rule.Name));
                return true;
            }
            foreach (var item in _list)
            {
                if (item.isDuplicate(rule, tokens))
                {
                    _pp.addParseError(new ParseErrorData(tokens[0], ErrorCode.ERR_PreProcessorRecursiveRule, rule.Name));
                    return true;
                }
            }
            _list.Add(new PPUsedRule(rule, tokens));
            return false;
        }
        internal int Count => _list.Count;
    }
}

