using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService.Formatting
{
    [Flags]
    internal enum FormattingFlags
    {
        Type = 1 << 0,
        Member = 1 << 1,
        Statement = 1 << 2,
        /// <summary>
        /// Does the statement have Middle keywords
        /// </summary>
        Middle = 1 << 4, 
        /// <summary>
        /// Does the statement have CASE / OTHERWISE labels and need to follow the indent CASE setting
        /// </summary>
        Case = 1 << 5,
        Preprocessor = 1 << 6,
        OptionalEnd = 1 << 7,
        End = 1 << 8

    }
    /// <summary>
    /// This type is used to store information about keyword pairs that are used during formatting
    /// Each entry has a start token and an stop token and flags that indicate the type of pair
    /// The flags entry also has a member that indicates that END without further keyword is allowd
    /// and that the END is optional.
    /// </summary>

    internal struct FormattingRule
    {
        internal XToken Start { get; private set; } 
        internal XToken Stop { get; private set; }
        internal FormattingFlags Flags { get; private set; }
        internal bool isEmpty => Start.isEmpty || Stop.isEmpty;

        private readonly static IDictionary<XToken, XToken> _aliases;
        private readonly static IDictionary<XToken, List<XToken>> _middleKeywords;
        private readonly static IDictionary<XToken, FormattingRule> _rulesByStart;

        internal static FormattingRule GetStartRule(XToken token)
        {
            if (_rulesByStart.ContainsKey(token))
                return _rulesByStart[token];
            return default;
        }
  
        /// <summary>
        /// Return all the middle tokens that can map a single start token
        /// </summary>
        /// <returns>List of tokens</returns>
        internal static IReadOnlyDictionary<XToken, XToken> MiddleKeywords()
        {
            var tokens = new Dictionary<XToken, XToken>();
            // Entities
            foreach (var item in _middleKeywords)
            {
                if (item.Value.Count == 1)
                {
                    tokens.Add(item.Key, item.Value[0]);
                }

            }
            return tokens;

        }
        /// <summary>
        /// Return all start tokens from the rules list
        /// </summary>
        /// <returns>List of tokens</returns>

        internal static IList<XToken> IndentKeywords() 
        {
            var tokens = new List<XToken>();
            // Entities
            foreach (var item in _rulesByStart)
            {
                tokens.Add(item.Key);
            }
            return tokens.ToArray();
        }


        /// <summary>
        /// Return all tokens that are the start of a Member
        /// </summary>
        /// <returns>List of tokens</returns>
        internal static IList<XToken> MemberKeywords()
        {
            var tokens = new List<XToken>();
            // Entities
            foreach (var item in _rulesByStart)
            {
                if (item.Value.Flags.HasFlag(FormattingFlags.Member))
                    tokens.Add(item.Key);
            }
            return tokens.ToArray();
        }

        /// <summary>
        /// Return all tokens that can be closed with "just" an END
        /// </summary>
        /// <returns>List of tokens</returns>
        internal static IList<XToken> AllowEndKeywords()
        {
            var tokens = new List<XToken>();
            foreach (var item in _rulesByStart)
            {
                if (item.Value.Flags.HasFlag(FormattingFlags.End))
                {
                    tokens.Add(item.Key);
                }
            }
            return tokens.ToArray(); 
        }
        /// <summary>
        /// Return end keywords that can match more than one start keyword
        /// </summary>
        /// <returns></returns>
        internal static IReadOnlyDictionary<XToken, XToken> EndKeywords()
        {
            var tokens = new Dictionary<XToken, XToken>();
            // Entities
            foreach (var item in _rulesByStart)
            {
                tokens.Add(item.Value.Stop, item.Key);

            }
            return tokens;
        }
        /// <summary>
        /// Return all the middle tokens that can map more than one start token
        /// </summary>
        /// <returns>List of tokens</returns>

        internal static IReadOnlyDictionary<XToken, IList<XToken>> SpecialMiddleKeywords()
        {
            var tokens = new Dictionary<XToken, IList<XToken>>();
            foreach (var item in _middleKeywords)
            {
                if (item.Value.Count >= 1)
                {
                    tokens.Add(item.Key, item.Value);
                }
            }
            return tokens;
        }
        #region Keywords. These are used below to fill the tables. Each token is in fact a single integer

        // types
        static readonly XToken Begin_Namespace = new XToken(XKeyword.Begin, XKeyword.Namespace);
        static readonly XToken End_Namespace = new XToken(XKeyword.End, XKeyword.Namespace);

        static readonly XToken Class = new XToken(XKeyword.Class);
        static readonly XToken End_Class = new XToken(XKeyword.End, XKeyword.Class);
        static readonly XToken Define_Class = new XToken(XKeyword.Define, XKeyword.Class);
        static readonly XToken Enddefine = new XToken(XKeyword.Enddefine);
        static readonly XToken End_Define = new XToken(XKeyword.End, XKeyword.Define);
        static readonly XToken Structure = new XToken(XKeyword.Structure);
        static readonly XToken End_Structure = new XToken(XKeyword.End, XKeyword.Structure);
        static readonly XToken Interface = new XToken(XKeyword.Interface);
        static readonly XToken End_Interface = new XToken(XKeyword.End, XKeyword.Interface);
        static readonly XToken Enum = new XToken(XKeyword.Enum);
        static readonly XToken End_Enum = new XToken(XKeyword.End, XKeyword.Enum);
        static readonly XToken VoStruct = new XToken(XKeyword.VoStruct);
        static readonly XToken End_VoStruct = new XToken(XKeyword.End, XKeyword.VoStruct);
        static readonly XToken Union = new XToken(XKeyword.Union);
        static readonly XToken End_Union = new XToken(XKeyword.End, XKeyword.Union);

        // members
        static readonly XToken Function = new XToken(XKeyword.Function);
        static readonly XToken End_Function = new XToken(XKeyword.End, XKeyword.Function);

        static readonly XToken Procedure = new XToken(XKeyword.Procedure);
        static readonly XToken End_Procedure = new XToken(XKeyword.End, XKeyword.Procedure);

        static readonly XToken Access = new XToken(XKeyword.Access);
        static readonly XToken End_Access = new XToken(XKeyword.End, XKeyword.Access);

        static readonly XToken Assign = new XToken(XKeyword.Assign);
        static readonly XToken End_Assign = new XToken(XKeyword.End, XKeyword.Assign);

        static readonly XToken Method = new XToken(XKeyword.Method);
        static readonly XToken End_Method = new XToken(XKeyword.End, XKeyword.Method);

        static readonly XToken Property = new XToken(XKeyword.Property);
        static readonly XToken End_Property = new XToken(XKeyword.End, XKeyword.Property);

        static readonly XToken Constructor = new XToken(XKeyword.Constructor);
        static readonly XToken End_Constructor = new XToken(XKeyword.End, XKeyword.Constructor);

        static readonly XToken Destructor = new XToken(XKeyword.Destructor);
        static readonly XToken End_Destructor = new XToken(XKeyword.End, XKeyword.Destructor);

        static readonly XToken Operator = new XToken(XKeyword.Operator);
        static readonly XToken End_Operator = new XToken(XKeyword.End, XKeyword.Operator);

        static readonly XToken Event = new XToken(XKeyword.Event);
        static readonly XToken End_Event = new XToken(XKeyword.End, XKeyword.Event);

        static readonly XToken Get = new XToken(XKeyword.Get);
        static readonly XToken End_Get = new XToken(XKeyword.End, XKeyword.Get);

        static readonly XToken Set = new XToken(XKeyword.Set);
        static readonly XToken End_Set = new XToken(XKeyword.End, XKeyword.Set);

        static readonly XToken Init = new XToken(XKeyword.Init);
        static readonly XToken End_Init = new XToken(XKeyword.End, XKeyword.Init);

        static readonly XToken Add = new XToken(XKeyword.Add);
        static readonly XToken End_Add = new XToken(XKeyword.End, XKeyword.Add);

        static readonly XToken Remove = new XToken(XKeyword.Remove);
        static readonly XToken End_Remove = new XToken(XKeyword.End, XKeyword.Remove);

        // statements
        static readonly XToken Do_Case = new XToken(XKeyword.Do, XKeyword.Case);
        static readonly XToken Case = new XToken(XKeyword.Case);
        static readonly XToken Otherwise = new XToken(XKeyword.Otherwise);
        static readonly XToken Endcase = new XToken(XKeyword.Endcase);
        static readonly XToken End_Case = new XToken(XKeyword.End, XKeyword.Case);
        static readonly XToken For = new XToken(XKeyword.For);
        static readonly XToken ForEach = new XToken(XKeyword.Foreach);
        static readonly XToken Next = new XToken(XKeyword.Next);
        static readonly XToken End_For = new XToken(XKeyword.End, XKeyword.For);
        static readonly XToken While = new XToken(XKeyword.While);
        static readonly XToken Do_While = new XToken(XKeyword.Do, XKeyword.While);
        static readonly XToken Enddo = new XToken(XKeyword.Enddo);
        static readonly XToken End_Do = new XToken(XKeyword.End, XKeyword.Do);
        static readonly XToken End_While = new XToken(XKeyword.End, XKeyword.While);
        static readonly XToken If = new XToken(XKeyword.If);
        static readonly XToken Else = new XToken(XKeyword.Else);
        static readonly XToken Elseif = new XToken(XKeyword.Elseif);
        static readonly XToken Endif = new XToken(XKeyword.Endif);
        static readonly XToken End_If = new XToken(XKeyword.End, XKeyword.If);
        static readonly XToken Repeat = new XToken(XKeyword.Repeat);
        static readonly XToken Until = new XToken(XKeyword.Until);
        static readonly XToken Try = new XToken(XKeyword.Try);
        static readonly XToken Catch = new XToken(XKeyword.Catch);
        static readonly XToken Finally = new XToken(XKeyword.Finally);
        static readonly XToken End_Try = new XToken(XKeyword.End, XKeyword.Try);

        static readonly XToken Text = new XToken(XKeyword.Text);
        static readonly XToken Endtext = new XToken(XKeyword.Endtext);
        static readonly XToken End_Text = new XToken(XKeyword.End, XKeyword.Text);
        static readonly XToken Begin_Sequence = new XToken(XKeyword.Begin, XKeyword.Sequence);
        static readonly XToken Recover = new XToken(XKeyword.Recover);
        static readonly XToken End_Sequence = new XToken(XKeyword.End, XKeyword.Sequence);
        static readonly XToken Switch = new XToken(XKeyword.Switch);
        static readonly XToken Do_Switch = new XToken(XKeyword.Do, XKeyword.Switch);
        static readonly XToken Begin_Switch = new XToken(XKeyword.Begin, XKeyword.Switch);
        static readonly XToken End_Switch = new XToken(XKeyword.End, XKeyword.Switch);
        static readonly XToken With = new XToken(XKeyword.With);
        static readonly XToken End_With = new XToken(XKeyword.End, XKeyword.With);


        // Blocks
        static readonly XToken Begin_Fixed = new XToken(XKeyword.Begin, XKeyword.Fixed);
        static readonly XToken End_Fixed = new XToken(XKeyword.End, XKeyword.Fixed);
        static readonly XToken Begin_Using = new XToken(XKeyword.Begin, XKeyword.Using);
        static readonly XToken End_Using = new XToken(XKeyword.End, XKeyword.Using);
        static readonly XToken Begin_Scope = new XToken(XKeyword.Begin, XKeyword.Scope);
        static readonly XToken End_Scope = new XToken(XKeyword.End, XKeyword.Scope);
        static readonly XToken Begin_Checked = new XToken(XKeyword.Begin, XKeyword.Checked);
        static readonly XToken End_Checked = new XToken(XKeyword.End, XKeyword.Checked);
        static readonly XToken Begin_Unchecked = new XToken(XKeyword.Begin, XKeyword.Unchecked);
        static readonly XToken End_Unchecked = new XToken(XKeyword.End, XKeyword.Unchecked);
        static readonly XToken Begin_Lock = new XToken(XKeyword.Begin, XKeyword.Lock);
        static readonly XToken End_Lock = new XToken(XKeyword.End, XKeyword.Lock);



        // preprocessor
        static readonly XToken PP_If = new XToken(XKeyword.PP_if);
        static readonly XToken PP_Ifdef = new XToken(XKeyword.PP_ifdef);
        static readonly XToken PP_Ifndef = new XToken(XKeyword.PP_ifndef);
        static readonly XToken PP_Else = new XToken(XKeyword.PP_else);
        static readonly XToken PP_Endif = new XToken(XKeyword.PP_endif);
        static readonly XToken PP_Region = new XToken(XKeyword.PP_region);
        static readonly XToken PP_Endregion = new XToken(XKeyword.PP_endregion);

        #endregion

        static FormattingRule()
        {
            // This table contains unique Start and Stop values
            // symbols that share a stop code are merged by the Alias table
            // So FOR .. NEXT and FOREACH .. NEXT are combined in the For Next rule
            // Rules that can also end with END have the FormattingFlags.End Set
            // and rules with an optional END clause havd the OptionalEnd flag set
            var rules = new List<FormattingRule>
            {
                // Namespace
                new FormattingRule{Start = Begin_Namespace, Stop = End_Namespace, Flags = FormattingFlags.Type },
                // Blocks
                new FormattingRule{Start = If, Stop = Endif, Flags = FormattingFlags.Statement | FormattingFlags.End| FormattingFlags.Middle},
                new FormattingRule{Start = For, Stop = Next, Flags = FormattingFlags.Statement },
                new FormattingRule{Start = Do_Case, Stop = Endcase, Flags = FormattingFlags.Statement | FormattingFlags.Case | FormattingFlags.Middle| FormattingFlags.End},
                new FormattingRule{Start = While, Stop = Enddo, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Switch, Stop = End_Switch, Flags = FormattingFlags.Statement | FormattingFlags.Case | FormattingFlags.Middle | FormattingFlags.End },
                new FormattingRule{Start = Repeat, Stop = Until, Flags = FormattingFlags.Statement },
                new FormattingRule{Start = Try, Stop = End_Try, Flags = FormattingFlags.Statement | FormattingFlags.End | FormattingFlags.Middle},
                new FormattingRule{Start = Begin_Sequence, Stop = End_Sequence, Flags = FormattingFlags.Statement | FormattingFlags.End | FormattingFlags.Middle},
                new FormattingRule{Start = With, Stop = End_With, Flags = FormattingFlags.Statement | FormattingFlags.End},
                new FormattingRule{Start = Text, Stop = Endtext, Flags = FormattingFlags.Statement | FormattingFlags.End},
                
                // BEGIN .. END
                new FormattingRule{Start = Begin_Checked, Stop = End_Checked, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Begin_Fixed, Stop = End_Fixed, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Begin_Lock, Stop = End_Lock, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Begin_Scope, Stop = End_Scope, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Begin_Using, Stop = End_Using, Flags = FormattingFlags.Statement | FormattingFlags.End },
                new FormattingRule{Start = Begin_Unchecked, Stop = End_Unchecked, Flags = FormattingFlags.Statement | FormattingFlags.End },

                // Types
                new FormattingRule{Start = Class, Stop = End_Class, Flags = FormattingFlags.Type  },
                new FormattingRule{Start = Define_Class, Stop = Enddefine, Flags = FormattingFlags.Type  },
                new FormattingRule{Start = Interface, Stop = End_Interface, Flags = FormattingFlags.Type  },
                new FormattingRule{Start = Structure, Stop = End_Structure, Flags = FormattingFlags.Type  },
                new FormattingRule{Start = Enum, Stop = End_Enum, Flags = FormattingFlags.Type |  FormattingFlags.End },
                new FormattingRule{Start = VoStruct, Stop = End_VoStruct, Flags = FormattingFlags.Type | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Union, Stop = End_Union, Flags = FormattingFlags.Type | FormattingFlags.OptionalEnd   },

                // Members with mandatory end
                new FormattingRule{Start = Property, Stop = End_Property, Flags = FormattingFlags.Member | FormattingFlags.End },
                new FormattingRule{Start = Event, Stop = End_Event, Flags = FormattingFlags.Member | FormattingFlags.End  },
                new FormattingRule{Start = Get, Stop = End_Get, Flags = FormattingFlags.Member | FormattingFlags.End  },
                new FormattingRule{Start = Set, Stop = End_Set, Flags = FormattingFlags.Member | FormattingFlags.End  },
                new FormattingRule{Start = Init, Stop = End_Init, Flags = FormattingFlags.Member | FormattingFlags.End  },
                new FormattingRule{Start = Add, Stop = End_Add, Flags = FormattingFlags.Member | FormattingFlags.End  },
                new FormattingRule{Start = Remove, Stop = End_Remove, Flags = FormattingFlags.Member | FormattingFlags.End  },

                // Members with optional end. Most of them require END + Keyword
                new FormattingRule{Start = Constructor, Stop = End_Constructor, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Destructor, Stop = End_Destructor, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Method, Stop = End_Method, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Access, Stop = End_Access, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Assign, Stop = End_Assign, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Operator, Stop = End_Operator, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Function, Stop = End_Function, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },
                new FormattingRule{Start = Procedure, Stop = End_Procedure, Flags = FormattingFlags.Member | FormattingFlags.OptionalEnd  },

                // Preprocessor
                new FormattingRule{Start = PP_Region, Stop = PP_Endregion, Flags = FormattingFlags.Preprocessor },
                new FormattingRule{Start = PP_Ifdef, Stop = PP_Endif, Flags = FormattingFlags.Preprocessor | FormattingFlags.Middle  },



            };
            // tokens that are remapped to other tokens For example END IF is mapped to ENDIF and FOREACH is mapped to FOR
            // The second value in the table must be present in the Rules table as Start or Stop rule
            _aliases = new Dictionary<XToken, XToken>
            {
                {End_If, Endif},
                {End_For, Next},
                {ForEach, For},
                {Do_While, While },
                {Do_Switch, Switch },
                {Begin_Switch, Switch },
                {End_Do, Enddo },
                {End_While, Enddo },
                {End_Case, Endcase },
                {PP_Ifndef, PP_Ifdef},
                {PP_If, PP_Ifdef},
                {End_Text, Endtext },
                {End_Define, Enddefine},
            };

            // Keywords that are 'middle' parts of a statement, with the start token they need to be aligned with
            _middleKeywords = new Dictionary<XToken, List<XToken>>
            {
                { Catch, new List<XToken> { Begin_Sequence, Try } },
                { Finally, new List<XToken> { Begin_Sequence, Try } },
                { Case, new List<XToken> { Do_Case, Switch } },
                { Otherwise, new List<XToken> { Do_Case, Switch } },
                { Else, new List<XToken> { If } },
                { Elseif, new List<XToken> { If } },
                { PP_Else, new List<XToken> { PP_Ifdef } },
                { Recover, new List<XToken> { Begin_Sequence} },
            };

            // Validate the tables when debugging
            ValidateRuleTables(rules);

            _rulesByStart = new Dictionary<XToken, FormattingRule>();

            foreach (var item in rules)
            {
                _rulesByStart.Add(item.Start, item);
            }

        }

        static internal XToken TranslateToken(XToken keyword)
        {
            if (_aliases.ContainsKey(keyword))
                return _aliases[keyword];
            return keyword;
        }

        [Conditional("DEBUG")]
        static void ValidateRuleTables(List<FormattingRule> rules)
        {
            var startKeywords = new Dictionary<XToken, FormattingRule>();
            var endKeywords = new Dictionary<XToken, FormattingRule>();
            foreach (var rule in rules)
            {
                if (!startKeywords.ContainsKey(rule.Start))
                    startKeywords.Add(rule.Start, rule);
                else
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Start Keyword {rule.Start} double defined");
                if (!endKeywords.ContainsKey(rule.Stop))
                    endKeywords.Add(rule.Stop, rule);
                else

                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Stop Keyword {rule.Stop} double defined");
            }
            foreach (var item in _aliases)
            {
                if (startKeywords.ContainsKey(item.Key))
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Alias {item.Key} defined as start keyword");
                if (endKeywords.ContainsKey(item.Key))
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Alias {item.Key} defined as end keyword");
                if (!startKeywords.ContainsKey(item.Value) && ! endKeywords.ContainsKey(item.Value))
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Alias alternative {item.Value} not defined as start or end keyword");
            }
            foreach (var item in _middleKeywords)
            {
                if (startKeywords.ContainsKey(item.Key))
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Middle keyword {item.Key} defined as start keyword");
                if (endKeywords.ContainsKey(item.Key))
                    Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Middle keyword {item.Key} defined as end keyword");
                foreach (var first in item.Value)
                {
                    if (!startKeywords.ContainsKey(first))
                        Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Opening keyword {first} for middle keyword {item.Key} not defined as start keyword");
                    var rule = startKeywords[first];
                    if (! rule.Flags.HasFlag(FormattingFlags.Middle))
                        Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Formatting rule {first} for middle keyword {item.Key} does not have the Middle Flag set");
                }
            }
        }

    }
}
