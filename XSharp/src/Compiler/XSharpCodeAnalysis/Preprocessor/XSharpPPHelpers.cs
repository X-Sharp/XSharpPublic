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
    enum PPUDCType : byte
    {
        None,
        Define = 1,                 // #define
        Command = 2,                 // #command 
        Translate = 3,               // #translate 
        XCommand = 4,                // #xcommand
        XTranslate = 5,              // #xtranslate
    }
    [Flags]
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
        // unused 7                                                0000 0111

        //ResultMask = 8,                                          0000 1000    
        ResultRegular = 9,                 // <idMarker>           0000 1001
        ResultDumbStringify = 10,          // #<idMarker>          0000 1010
        ResultNormalStringify = 11,        // <"idMarker">         0000 1011
        ResultSmartStringify = 12,         // <(idMarker)>         0000 1100
        ResultBlockify = 13,               // <{idMarker}>         0000 1101
        ResultLogify = 14,                 // <.idMarker.>         0000 1110
        // TypeMask = 0x0F,                //                      0000 1111
        
        //
        //Overlay flags
        // 
        Repeated = 16,                      // 0001 0000
        Optional = 32,                      // 0010 0000
        Nested = 64,                        // 0100 0000
        Matched = 128,                      // 1000 0000    used for result tokens to mark that they have a match
        // Masks to filter out the bits
        ResultMask = 8,                      // 0000 1000    // Result markers have bit 4 set
        TypeMask = 0x0F,                     // 0000 1111
        OverlayMask = 0xF0,                  // 1111 0000
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
        internal void Add(PPRule rule)
        {
            // find element that matches the first token and insert at the front of the list
            // so rules defined later override rules defined first
            string key = rule.Key;
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
        internal PPRules Find(string key)
        {
            // Find a list of rules that start with the same first token
            PPRules list;
            if (_rules.ContainsKey(key))
                list = _rules[key];
            else
                list = null;
            return list;
        }

        internal PPRule FindMatchingRule(IList<PPToken> tokens, out PPMatchRange[] matchInfo)
        {
            PPRule result = null;
            matchInfo = null;
            if (tokens?.Count != 0)
            {
                var firsttoken = tokens[0];
                var rules = Find(firsttoken.Text);
                if (rules?.Count > 0)
                {
                    // try to find the first rule in the list that matches our tokens
                    foreach (var rule in rules)
                    {
                        if (rule.Matches(tokens, out matchInfo))
                        {
                            result = rule;
                            break;
                        }
                    }
                }
            }
            return result;
        }
    }

    /// <summary>
    /// This struct holds the start and end location of the tokens in the source 
    /// that match a match marker in a UDC
    /// Some special meanings are for:
    /// _start = 0 : Empty
    /// _start = -1: Missing optional token
    /// _start = -2: Token
    /// </summary>
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal struct PPMatchRange
    {
        private int _length;
        private int _start;
        internal int Start { get { return _start; } }
        internal int End { get { return _start + _length -1; } }
        internal static PPMatchRange Create( int start, int end)
        {
            return new PPMatchRange() { _start = start, _length = end - start +1};
        }
        internal bool Empty()
        {
            return _start == 0 && _length == 0;
        }
        internal static PPMatchRange Optional()
        {
            return new PPMatchRange() { _start = -1, _length = 0};
        }
        internal static PPMatchRange Token()
        {
            return new PPMatchRange() { _start = -2, _length = 0 };
        }
        internal string GetDebuggerDisplay()
        {
            if (_start == 0 && _length == 0)
                return "Empty";
            if (_start == -1 )
                return "Skipped Optional marker";
            if (_start == -2 )
                return "Token";
            else
                return _start.ToString() + "," + End.ToString();

        }
    }


}

