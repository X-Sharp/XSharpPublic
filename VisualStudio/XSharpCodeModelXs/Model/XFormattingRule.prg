// XKeywordPairs.prg
// Created by    : robert
// Creation Date : 4/14/2022 4:18:52 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text
using System.Diagnostics

BEGIN NAMESPACE XSharpModel

    /// <summary>
    /// Flags that describe formatting rules
    /// </summary>
    [Flags];
    enum XFormattingFlags
        /// <summary>
        /// Type block.
        /// </summary>

        member @@Type := 1 << 0
        /// <summary>
        /// Member Block
        /// </summary>
        member @@Member := 1 << 1
        /// <summary>
        /// Statement block
        /// </summary>
        member @@Statement := 1 << 2
        /// <summary>
        /// Does the statement have Middle keywords
        /// </summary>
        member @@HasMiddle := 1 << 4
        /// <summary>
        /// Does the statement have CASE / OTHERWISE labels and need to follow the indent CASE setting
        /// </summary>
        member @@Case := 1 << 5
        /// <summary>
        /// block is a preprocessor block
        /// </summary>
        member @@Preprocessor := 1 << 6
        /// <summary>
        /// End keyword is optional
        /// </summary>
        member @@OptionalEnd := 1 << 7
        /// <summary>
        /// End without second keyword is allowed
        /// </summary>
        member @@End := 1 << 8

        /// <summary>
        /// Namespace block
        /// </summary>
        member @@Namespace := 1 << 9
    end Enum

    /// <summary>
    /// This type is used to store information about keyword pairs that are used during formatting
    /// Each entry has a start token and an stop token and flags that indicate the type of pair
    /// The flags entry also has a member that indicates that END without further keyword is allowd
    /// and that the END is optional.
    /// </summary>

    CLASS XFormattingRule
        #region Static Fields
        // types
        static initonly @@Begin_Namespace := XKeyword{XTokenType.Begin, XTokenType.Namespace} as XKeyword
        static initonly @@End_Namespace := XKeyword{XTokenType.End, XTokenType.Namespace}as XKeyword

        static initonly @@Class := XKeyword{XTokenType.Class} as XKeyword
        static initonly @@End_Class := XKeyword{XTokenType.End, XTokenType.Class} as XKeyword
        static initonly @@Define_Class := XKeyword{XTokenType.Define, XTokenType.Class} as XKeyword
        static initonly @@Enddefine := XKeyword{XTokenType.Enddefine} as XKeyword
        static initonly @@End_Define := XKeyword{XTokenType.End, XTokenType.Define} as XKeyword
        static initonly @@Structure := XKeyword{XTokenType.Structure} as XKeyword
        static initonly @@End_Structure := XKeyword{XTokenType.End, XTokenType.Structure} as XKeyword
        static initonly @@Interface := XKeyword{XTokenType.Interface} as XKeyword
        static initonly @@End_Interface := XKeyword{XTokenType.End, XTokenType.Interface} as XKeyword
        static initonly @@Enum := XKeyword{XTokenType.Enum} as XKeyword
        static initonly @@End_Enum := XKeyword{XTokenType.End, XTokenType.Enum} as XKeyword
        static initonly @@VoStruct := XKeyword{XTokenType.Vostruct} as XKeyword
        static initonly @@End_VoStruct := XKeyword{XTokenType.End, XTokenType.Vostruct} as XKeyword
        static initonly @@Union := XKeyword{XTokenType.Union} as XKeyword
        static initonly @@End_Union := XKeyword{XTokenType.End, XTokenType.Union} as XKeyword


        // members


        static initonly @@Local_Function := XKeyword{XTokenType.Local, XTokenType.Function} as XKeyword
        static initonly @@Function := XKeyword{XTokenType.Function} as XKeyword
        static initonly @@End_Function := XKeyword{XTokenType.End, XTokenType.Function} as XKeyword

        static initonly @@Local_Procedure := XKeyword{XTokenType.Local, XTokenType.Procedure} as XKeyword
        static initonly @@Procedure := XKeyword{XTokenType.Procedure} as XKeyword
        static initonly @@End_Procedure := XKeyword{XTokenType.End, XTokenType.Procedure} as XKeyword

        static initonly @@Access := XKeyword{XTokenType.Access} as XKeyword
        static initonly @@End_Access := XKeyword{XTokenType.End, XTokenType.Access} as XKeyword

        static initonly @@Assign := XKeyword{XTokenType.Assign} as XKeyword
        static initonly @@End_Assign := XKeyword{XTokenType.End, XTokenType.Assign} as XKeyword

        static initonly @@Method := XKeyword{XTokenType.Method} as XKeyword
        static initonly @@End_Method := XKeyword{XTokenType.End, XTokenType.Method} as XKeyword

        static initonly @@Property := XKeyword{XTokenType.Property} as XKeyword
        static initonly @@End_Property := XKeyword{XTokenType.End, XTokenType.Property} as XKeyword

        static initonly @@Constructor := XKeyword{XTokenType.Constructor} as XKeyword
        static initonly @@End_Constructor := XKeyword{XTokenType.End, XTokenType.Constructor} as XKeyword

        static initonly @@Destructor := XKeyword{XTokenType.Destructor} as XKeyword
        static initonly @@End_Destructor := XKeyword{XTokenType.End, XTokenType.Destructor} as XKeyword

        static initonly @@Operator := XKeyword{XTokenType.Operator} as XKeyword
        static initonly @@End_Operator := XKeyword{XTokenType.End, XTokenType.Operator} as XKeyword

        static initonly @@Event := XKeyword{XTokenType.Event} as XKeyword
        static initonly @@End_Event := XKeyword{XTokenType.End, XTokenType.Event} as XKeyword

        static initonly @@Get := XKeyword{XTokenType.Get} as XKeyword
        static initonly @@End_Get := XKeyword{XTokenType.End, XTokenType.Get} as XKeyword

        static initonly @@Set := XKeyword{XTokenType.Set} as XKeyword
        static initonly @@End_Set := XKeyword{XTokenType.End, XTokenType.Set} as XKeyword

        static initonly @@Init := XKeyword{XTokenType.Init} as XKeyword
        static initonly @@End_Init := XKeyword{XTokenType.End, XTokenType.Init} as XKeyword

        static initonly @@Add := XKeyword{XTokenType.Add} as XKeyword
        static initonly @@End_Add := XKeyword{XTokenType.End, XTokenType.Add} as XKeyword

        static initonly @@Remove := XKeyword{XTokenType.Remove} as XKeyword
        static initonly @@End_Remove := XKeyword{XTokenType.End, XTokenType.Remove} as XKeyword

        // statements
        static initonly @@Do_Case := XKeyword{XTokenType.Do, XTokenType.Case} as XKeyword
        static initonly @@Case := XKeyword{XTokenType.Case} as XKeyword
        static initonly @@Otherwise := XKeyword{XTokenType.Otherwise} as XKeyword
        static initonly @@Endcase := XKeyword{XTokenType.Endcase} as XKeyword
        static initonly @@End_Case := XKeyword{XTokenType.End, XTokenType.Case} as XKeyword
        static initonly @@For := XKeyword{XTokenType.For} as XKeyword
        static initonly @@ForEach := XKeyword{XTokenType.Foreach} as XKeyword
        static initonly @@Next := XKeyword{XTokenType.Next} as XKeyword
        static initonly @@End_For := XKeyword{XTokenType.End, XTokenType.For} as XKeyword
        static initonly @@While := XKeyword{XTokenType.While} as XKeyword
        static initonly @@Do_While := XKeyword{XTokenType.Do, XTokenType.While} as XKeyword
        static initonly @@Enddo := XKeyword{XTokenType.Enddo} as XKeyword
        static initonly @@End_Do := XKeyword{XTokenType.End, XTokenType.Do} as XKeyword
        static initonly @@End_While := XKeyword{XTokenType.End, XTokenType.While} as XKeyword
        static initonly @@If := XKeyword{XTokenType.If} as XKeyword
        static initonly @@Else := XKeyword{XTokenType.Else} as XKeyword
        static initonly @@Elseif := XKeyword{XTokenType.Elseif} as XKeyword
        static initonly @@Endif := XKeyword{XTokenType.Endif} as XKeyword
        static initonly @@End_If := XKeyword{XTokenType.End, XTokenType.If} as XKeyword
        static initonly @@Repeat := XKeyword{XTokenType.Repeat} as XKeyword
        static initonly @@Until := XKeyword{XTokenType.Until} as XKeyword
        static initonly @@Try := XKeyword{XTokenType.Try} as XKeyword
        static initonly @@Catch := XKeyword{XTokenType.Catch} as XKeyword
        static initonly @@Finally := XKeyword{XTokenType.Finally} as XKeyword
        static initonly @@End_Try := XKeyword{XTokenType.End, XTokenType.Try} as XKeyword

        static initonly @@Text := XKeyword{XTokenType.Text} as XKeyword
        static initonly @@Endtext := XKeyword{XTokenType.Endtext} as XKeyword
        static initonly @@End_Text := XKeyword{XTokenType.End, XTokenType.Text} as XKeyword
        static initonly @@Begin_Sequence := XKeyword{XTokenType.Begin, XTokenType.Sequence} as XKeyword
        static initonly @@Recover := XKeyword{XTokenType.Recover} as XKeyword
        static initonly @@End_Sequence := XKeyword{XTokenType.End, XTokenType.Sequence} as XKeyword
        static initonly @@Switch := XKeyword{XTokenType.Switch} as XKeyword
        static initonly @@Do_Switch := XKeyword{XTokenType.Do, XTokenType.Switch} as XKeyword
        static initonly @@Begin_Switch := XKeyword{XTokenType.Begin, XTokenType.Switch} as XKeyword
        static initonly @@End_Switch := XKeyword{XTokenType.End, XTokenType.Switch} as XKeyword
        static initonly @@With := XKeyword{XTokenType.With} as XKeyword
        static initonly @@End_With := XKeyword{XTokenType.End, XTokenType.With} as XKeyword


        // Blocks
        static initonly @@Begin_Fixed := XKeyword{XTokenType.Begin, XTokenType.Fixed} as XKeyword
        static initonly @@End_Fixed := XKeyword{XTokenType.End, XTokenType.Fixed} as XKeyword
        static initonly @@Begin_Using := XKeyword{XTokenType.Begin, XTokenType.Using} as XKeyword
        static initonly @@End_Using := XKeyword{XTokenType.End, XTokenType.Using} as XKeyword
        static initonly @@Begin_Scope := XKeyword{XTokenType.Begin, XTokenType.Scope} as XKeyword
        static initonly @@End_Scope := XKeyword{XTokenType.End, XTokenType.Scope} as XKeyword
        static initonly @@Begin_Checked := XKeyword{XTokenType.Begin, XTokenType.Checked} as XKeyword
        static initonly @@End_Checked := XKeyword{XTokenType.End, XTokenType.Checked} as XKeyword
        static initonly @@Begin_Unchecked := XKeyword{XTokenType.Begin, XTokenType.Unchecked} as XKeyword
        static initonly @@End_Unchecked := XKeyword{XTokenType.End, XTokenType.Unchecked} as XKeyword
        static initonly @@Begin_Lock := XKeyword{XTokenType.Begin, XTokenType.Lock} as XKeyword
        static initonly @@End_Lock := XKeyword{XTokenType.End, XTokenType.Lock} as XKeyword


        // preprocessor
        static initonly @@PP_If := XKeyword{XTokenType.PP_if} as XKeyword
        static initonly @@PP_Ifdef := XKeyword{XTokenType.PP_ifdef} as XKeyword
        static initonly @@PP_Ifndef := XKeyword{XTokenType.PP_ifndef} as XKeyword
        static initonly @@PP_Else := XKeyword{XTokenType.PP_else} as XKeyword
        static initonly @@PP_Endif := XKeyword{XTokenType.PP_endif} as XKeyword
        static initonly @@PP_Region := XKeyword{XTokenType.PP_region} as XKeyword
        static initonly @@PP_Endregion := XKeyword{XTokenType.PP_endregion} as XKeyword
        static initonly @@PP_Text  := XKeyword{XTokenType.PP_text} as XKeyword
        static initonly @@PP_EndText := XKeyword{XTokenType.PP_endtext} as XKeyword


        static initonly _aliases        as IDictionary<XKeyword, XKeyword>
        static initonly _middleKeywords as IDictionary<XKeyword, List<XKeyword>>
        static initonly _rulesByStart   as IDictionary<XKeyword, XFormattingRule>
        static initonly _endKeywords    as IDictionary<XKeyword, XFormattingRule>

        #endregion

        public PROPERTY Start as XKeyword GET PRIVATE SET
        public PROPERTY Stop  as XKeyword GET PRIVATE SET
        public property Flags as XFormattingFlags GET PRIVATE SET



        PRIVATE CONSTRUCTOR(start as XKeyword, stop as XKeyword, flags as XFormattingFlags)
            Start := start
            Stop  := stop
            Flags := flags
            RETURN

        #region Static Constructor that builds the tables
            STATIC CONSTRUCTOR()
            var rules := List<XFormattingRule>{}
                rules:Add(XFormattingRule{@@Begin_Namespace,  @@End_Namespace, XFormattingFlags.Namespace })
                // Types
                rules:Add(XFormattingRule{@@Class, @@End_Class, XFormattingFlags.Type  })
                rules:Add(XFormattingRule{@@Define_Class, @@Enddefine, XFormattingFlags.Type  })
                rules:Add(XFormattingRule{@@Interface, @@End_Interface, XFormattingFlags.Type  })
                rules:Add(XFormattingRule{@@Structure, @@End_Structure, XFormattingFlags.Type  })
                rules:Add(XFormattingRule{@@Enum, @@End_Enum, XFormattingFlags.Type |  XFormattingFlags.End })
                rules:Add(XFormattingRule{@@VoStruct, @@End_VoStruct, XFormattingFlags.Type | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Union, @@End_Union, XFormattingFlags.Type | XFormattingFlags.OptionalEnd   })
                // Blocks
                rules:Add(XFormattingRule{@@If,               @@Endif,        XFormattingFlags.Statement | XFormattingFlags.End | XFormattingFlags.HasMiddle})
                rules:Add(XFormattingRule{@@For,              @@Next,         XFormattingFlags.Statement })
                rules:Add(XFormattingRule{@@Do_Case,          @@Endcase,      XFormattingFlags.Statement | XFormattingFlags.Case | XFormattingFlags.HasMiddle| XFormattingFlags.End})
                rules:Add(XFormattingRule{@@While,            @@Enddo,        XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Switch,           @@End_Switch,   XFormattingFlags.Statement | XFormattingFlags.Case | XFormattingFlags.HasMiddle | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Repeat,           @@Until,        XFormattingFlags.Statement })
                rules:Add(XFormattingRule{@@Try,              @@End_Try,      XFormattingFlags.Statement | XFormattingFlags.End | XFormattingFlags.HasMiddle})
                rules:Add(XFormattingRule{@@Begin_Sequence,   @@End_Sequence, XFormattingFlags.Statement | XFormattingFlags.End | XFormattingFlags.HasMiddle})
                rules:Add(XFormattingRule{@@With,             @@End_With,     XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Text,             @@Endtext,      XFormattingFlags.Statement | XFormattingFlags.End})
                // begin end
                rules:Add(XFormattingRule{@@Begin_Checked,    @@End_Checked, XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Begin_Fixed,      @@End_Fixed, XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Begin_Lock,       @@End_Lock, XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Begin_Scope,      @@End_Scope, XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Begin_Using,      @@End_Using, XFormattingFlags.Statement | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Begin_Unchecked,  @@End_Unchecked, XFormattingFlags.Statement | XFormattingFlags.End })

                // Members with mandatory end
                rules:Add(XFormattingRule{@@Property, @@End_Property, XFormattingFlags.Member | XFormattingFlags.End })
                rules:Add(XFormattingRule{@@Event, @@End_Event, XFormattingFlags.Member | XFormattingFlags.End  })

                // Accessors are treated like a statement
                // and they have an optional end because they can be a single like

                rules:Add(XFormattingRule{@@Get, @@End_Get, XFormattingFlags.Statement | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Set, @@End_Set, XFormattingFlags.Statement | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Init, @@End_Init, XFormattingFlags.Statement | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Add, @@End_Add, XFormattingFlags.Statement | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Remove, @@End_Remove, XFormattingFlags.Statement | XFormattingFlags.OptionalEnd  })

                // Members with optional end. Most of them require END + Keyword
                rules:Add(XFormattingRule{@@Constructor, @@End_Constructor, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Destructor, @@End_Destructor, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Method, @@End_Method, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Access, @@End_Access, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Assign, @@End_Assign, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Operator, @@End_Operator, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Function, @@End_Function, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })
                rules:Add(XFormattingRule{@@Procedure, @@End_Procedure, XFormattingFlags.Member | XFormattingFlags.OptionalEnd  })

                // Preprocessor
                rules:Add(XFormattingRule{@@PP_Region, @@PP_Endregion, XFormattingFlags.Preprocessor })
                rules:Add(XFormattingRule{@@PP_Ifdef, @@PP_Endif, XFormattingFlags.Preprocessor | XFormattingFlags.HasMiddle  })
                rules:Add(XFormattingRule{@@PP_Text, @@PP_EndText, XFormattingFlags.Preprocessor  })


                // tokens that are remapped to other tokens For example END IF is mapped to ENDIF and FOREACH is mapped to FOR
                // The second value in the table must be present in the Rules table as Start or Stop rule
                // ValidateToken (which is only called in debug mode) checks this.
                _aliases := Dictionary<XKeyword, XKeyword>{} {;
                    {@@End_If, @@Endif},;
                    {@@End_For, @@Next},;
                    {@@ForEach, @@For},;
                    {@@Do_While, @@While },;
                    {@@Do_Switch, @@Switch },;
                    {@@Begin_Switch, @@Switch },;
                    {@@End_Do, @@Enddo },;
                    {@@End_While, @@Enddo },;
                    {@@End_Case, @@Endcase },;
                    {@@PP_Ifndef, @@PP_Ifdef},;
                    {@@PP_If, @@PP_Ifdef},;
                    {@@End_Text, @@Endtext },;
                    {@@End_Define, @@Enddefine},;
                    {@@Local_Function, @@Function },;
                    {@@Local_Procedure, @@Procedure};
                    }

            // Keywords that are 'middle' parts of a statement, with the start tokens they need to be aligned with
            // this has to be the start token in the rules table, so not an alias
             _middleKeywords := Dictionary<XKeyword, List<XKeyword>> {} {;
                { @@Catch, List<XKeyword>{} { @@Begin_Sequence, @@Try } }, ;
                { @@Finally, List<XKeyword>{} { @@Begin_Sequence, @@Try } },;
                { @@Case, List<XKeyword>{} { @@Do_Case, @@Switch } },;
                { @@Otherwise, List<XKeyword>{} { @@Do_Case, @@Switch } },;
                { @@Else, List<XKeyword>{} { @@If } },;
                { @@Elseif, List<XKeyword>{} { @@If } },;
                { @@PP_Else, List<XKeyword>{} { @@PP_Ifdef } },;
                { @@Recover, List<XKeyword>{} { @@Begin_Sequence} } ;
                }

            ValidateRules(rules)

            _rulesByStart := Dictionary<XKeyword, XFormattingRule>{}
            _endKeywords  := Dictionary<XKeyword, XFormattingRule>{}
            foreach var item in rules
                _rulesByStart:Add(item:Start, item)
                _endKeywords:Add(item:Stop, item)
            next
            foreach var item in _aliases
                if _endKeywords:ContainsKey(item:Value) .and. ! _endKeywords:ContainsKey(item:Key)
                    _endKeywords:Add(item:Key, _endKeywords[item:Value])
                endif
            next

        #endregion

        [Conditional("DEBUG")];
        PRIVATE STATIC METHOD ValidateRules(rules as List<XFormattingRule>) AS VOID
            var startKeywords := Dictionary<XKeyword, XFormattingRule>{}
            var endKeywords := Dictionary<XKeyword, XFormattingRule>{}
            foreach var rule in rules
                if (!startKeywords:ContainsKey(rule:Start))
                    startKeywords[rule:Start] := rule
                else
                    XSettings.ShowMessageBox(i"Start Keyword {rule.Start} double defined")
                endif
                if !endKeywords.ContainsKey(rule.Stop)
                    endKeywords[rule.Stop] :=  rule
                else
                    XSettings.ShowMessageBox(i"Stop Keyword {rule.Stop} double defined")
                endif
            next
            foreach var item in _aliases
                if startKeywords.ContainsKey(item:Key)
                    XSettings.ShowMessageBox(i"Alias {item.Key} defined as start keyword")
                endif
                if (endKeywords.ContainsKey(item:Key))
                    XSettings.ShowMessageBox(i"Alias {item.Key} defined as end keyword")
                endif
                if (!startKeywords.ContainsKey(item:Value) && ! endKeywords.ContainsKey(item:Value))
                    XSettings.ShowMessageBox(i"Alias alternative {item.Value} not defined as start or end keyword")
                endif
            next
            foreach var item in _middleKeywords
                // middle keywords may not be start or end
                // and the list of start keywords must not contain items that we do not know
                if (startKeywords.ContainsKey(item:Key))
                    XSettings.ShowMessageBox(i"Middle keyword {item.Key} defined as start keyword")
                endif
                if (endKeywords.ContainsKey(item.Key))
                    XSettings.ShowMessageBox(i"Middle keyword {item.Key} defined as end keyword")
                endif
                foreach var first in item.Value

                    if (!startKeywords.ContainsKey(first))
                        XSettings.ShowMessageBox(i"Opening keyword {first} for middle keyword {item.Key} not defined as start keyword")
                    endif
                    var rule := startKeywords[first]
                    if (! rule:Flags:HasFlag(XFormattingFlags.HasMiddle))
                        XSettings.ShowMessageBox(i"Formatting rule {first} for middle keyword {item.Key} does not have the Middle Flag set")
                    endif
                next
            next


        #region public methods


        OVERRIDE METHOD ToString() AS STRING
            RETURN SELF:Start:ToString() + " "+SELF:Stop:ToString()

        PUBLIC STATIC METHOD TranslateToken(token as XKeyword ) AS XKeyword
            if _aliases:ContainsKey(token)
                return _aliases[token]
            endif
            return token

        PUBLIC STATIC METHOD GetStartRule( token as XKeyword) AS XFormattingRule
            if _aliases:ContainsKey(token)
                token := _aliases[token]
            endif
            if _rulesByStart.ContainsKey(token)
                return _rulesByStart[token]
            endif
            return DEFAULT(XFormattingRule)

        PUBLIC STATIC METHOD IsStartKeyword(token as XKeyword) AS LOGIC
            if _aliases:ContainsKey(token)
                token := _aliases[token]
            endif
            return _rulesByStart.ContainsKey(token)
        PUBLIC STATIC METHOD IsStartKeyword(token as XKeyword, withFlags as XFormattingFlags) AS LOGIC
            if _aliases:ContainsKey(token)
                token := _aliases[token]
            endif
            if _rulesByStart.ContainsKey(token)
                var rule := _rulesByStart[token]
                return rule:Flags:HasFlag(XFormattingFlags.Statement)
            endif
            return false

        PUBLIC STATIC METHOD IsEntityKeyword(token as XKeyword) AS LOGIC
            if _aliases:ContainsKey(token)
                token := _aliases[token]
            endif
            if _rulesByStart.ContainsKey(token)
                var rule := _rulesByStart[token]
                return rule:Flags:HasFlag(XFormattingFlags.Member) .or. rule:Flags:HasFlag(XFormattingFlags.Type)
            endif
            return false



        PUBLIC STATIC METHOD IsMiddleKeyword(token as XKeyword) AS LOGIC
            RETURN _middleKeywords:ContainsKey(token)

        PUBLIC STATIC METHOD IsEndKeyword(token as XKeyword) AS LOGIC
            RETURN _endKeywords:ContainsKey(token)


        /// <summary>
        /// Return all the middle tokens that can map a single start token
        /// </summary>
        /// <returns>List of tokens</returns>
        PUBLIC STATIC METHOD MiddleKeywords() as IReadOnlyDictionary<XKeyword, XKeyword>

            var tokens := Dictionary<XKeyword, XKeyword>{}
            foreach var item in _middleKeywords
                if item.Value.Count == 1
                    tokens.Add(item.Key, item.Value[0])
                endif
            next
            return tokens
        /// <summary>
        /// Return all start tokens from the rules list
        /// </summary>
        /// <returns>List of tokens</returns>
        PUBLIC STATIC METHOD IndentKeywords()  AS IList<XKeyword>
            var tokens := List<XKeyword>{}
            foreach var item in _rulesByStart
                tokens:Add(item:Key)
            next
            return tokens:ToArray()



        /// <summary>
        /// Return all tokens that are the start of a Member
        /// </summary>
        /// <returns>List of tokens</returns>
        PUBLIC static METHOD MemberKeywords() as IList<XKeyword>
            var tokens := List<XKeyword>{}
            // Entities
            foreach var item in _rulesByStart
                if (item:Value:Flags:HasFlag(XFormattingFlags.Member))
                    tokens:Add(item:Key)
                endif
            next

            foreach var item in _aliases
                if tokens.Contains(item:Value)
                    tokens.Add(item:Key)
                endif
            next

            return tokens:ToArray()


        /// <summary>
        /// Return all tokens that can be closed with "just" an END
        /// </summary>
        /// <returns>List of tokens</returns>
        public static method AllowEndKeywords() as IList<XKeyword>
            var tokens := List<XKeyword>{}
            foreach var item in _rulesByStart
                if item.Value.Flags.HasFlag(XFormattingFlags.End)
                    tokens.Add(item:Key)
                endif
            next
            return tokens:ToArray()


        /// <summary>
        /// Return end keywords that can match more than one start keyword
        /// </summary>
        /// <returns></returns>
        public static method EndKeywords() as IReadOnlyDictionary<XKeyword, XKeyword>
            var tokens := Dictionary<XKeyword, XKeyword>{}
            foreach var item in _rulesByStart
                tokens:Add(item:Value:Stop, item:Key)
            next
            return tokens



        /// <summary>
        /// Return all the middle tokens that can map more than one start token
        /// </summary>
        /// <returns>List of tokens</returns>
        public static method SpecialMiddleKeywords() as IReadOnlyDictionary<XKeyword, IList<XKeyword>>
            var tokens := Dictionary<XKeyword, IList<XKeyword>>{}
            foreach var item in _middleKeywords
                if (item:Value:Count >= 1)

                    tokens:Add(item:Key, item:Value)
                endif
            next
            return tokens

        #endregion



    END CLASS
END NAMESPACE // XSharpModel.Model
