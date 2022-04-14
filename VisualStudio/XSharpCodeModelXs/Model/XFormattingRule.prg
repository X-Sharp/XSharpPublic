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
        static initonly @@Begin_Namespace := XToken{XKeyword.Begin, XKeyword.Namespace} as XToken
        static initonly @@End_Namespace := XToken{XKeyword.End, XKeyword.Namespace}as XToken

        static initonly @@Class := XToken{XKeyword.Class} as XToken
        static initonly @@End_Class := XToken{XKeyword.End, XKeyword.Class} as XToken
        static initonly @@Define_Class := XToken{XKeyword.Define, XKeyword.Class} as XToken
        static initonly @@Enddefine := XToken{XKeyword.Enddefine} as XToken
        static initonly @@End_Define := XToken{XKeyword.End, XKeyword.Define} as XToken
        static initonly @@Structure := XToken{XKeyword.Structure} as XToken
        static initonly @@End_Structure := XToken{XKeyword.End, XKeyword.Structure} as XToken
        static initonly @@Interface := XToken{XKeyword.Interface} as XToken
        static initonly @@End_Interface := XToken{XKeyword.End, XKeyword.Interface} as XToken
        static initonly @@Enum := XToken{XKeyword.Enum} as XToken
        static initonly @@End_Enum := XToken{XKeyword.End, XKeyword.Enum} as XToken
        static initonly @@VoStruct := XToken{XKeyword.Vostruct} as XToken
        static initonly @@End_VoStruct := XToken{XKeyword.End, XKeyword.Vostruct} as XToken
        static initonly @@Union := XToken{XKeyword.Union} as XToken
        static initonly @@End_Union := XToken{XKeyword.End, XKeyword.Union} as XToken


        // members


        static initonly @@Local_Function := XToken{XKeyword.Local, XKeyword.Function} as XToken
        static initonly @@Function := XToken{XKeyword.Function} as XToken
        static initonly @@End_Function := XToken{XKeyword.End, XKeyword.Function} as XToken

        static initonly @@Local_Procedure := XToken{XKeyword.Local, XKeyword.Procedure} as XToken
        static initonly @@Procedure := XToken{XKeyword.Procedure} as XToken
        static initonly @@End_Procedure := XToken{XKeyword.End, XKeyword.Procedure} as XToken

        static initonly @@Access := XToken{XKeyword.Access} as XToken
        static initonly @@End_Access := XToken{XKeyword.End, XKeyword.Access} as XToken

        static initonly @@Assign := XToken{XKeyword.Assign} as XToken
        static initonly @@End_Assign := XToken{XKeyword.End, XKeyword.Assign} as XToken

        static initonly @@Method := XToken{XKeyword.Method} as XToken
        static initonly @@End_Method := XToken{XKeyword.End, XKeyword.Method} as XToken

        static initonly @@Property := XToken{XKeyword.Property} as XToken
        static initonly @@End_Property := XToken{XKeyword.End, XKeyword.Property} as XToken

        static initonly @@Constructor := XToken{XKeyword.Constructor} as XToken
        static initonly @@End_Constructor := XToken{XKeyword.End, XKeyword.Constructor} as XToken

        static initonly @@Destructor := XToken{XKeyword.Destructor} as XToken
        static initonly @@End_Destructor := XToken{XKeyword.End, XKeyword.Destructor} as XToken

        static initonly @@Operator := XToken{XKeyword.Operator} as XToken
        static initonly @@End_Operator := XToken{XKeyword.End, XKeyword.Operator} as XToken

        static initonly @@Event := XToken{XKeyword.Event} as XToken
        static initonly @@End_Event := XToken{XKeyword.End, XKeyword.Event} as XToken

        static initonly @@Get := XToken{XKeyword.Get} as XToken
        static initonly @@End_Get := XToken{XKeyword.End, XKeyword.Get} as XToken

        static initonly @@Set := XToken{XKeyword.Set} as XToken
        static initonly @@End_Set := XToken{XKeyword.End, XKeyword.Set} as XToken

        static initonly @@Init := XToken{XKeyword.Init} as XToken
        static initonly @@End_Init := XToken{XKeyword.End, XKeyword.Init} as XToken

        static initonly @@Add := XToken{XKeyword.Add} as XToken
        static initonly @@End_Add := XToken{XKeyword.End, XKeyword.Add} as XToken

        static initonly @@Remove := XToken{XKeyword.Remove} as XToken
        static initonly @@End_Remove := XToken{XKeyword.End, XKeyword.Remove} as XToken

        // statements
        static initonly @@Do_Case := XToken{XKeyword.Do, XKeyword.Case} as XToken
        static initonly @@Case := XToken{XKeyword.Case} as XToken
        static initonly @@Otherwise := XToken{XKeyword.Otherwise} as XToken
        static initonly @@Endcase := XToken{XKeyword.Endcase} as XToken
        static initonly @@End_Case := XToken{XKeyword.End, XKeyword.Case} as XToken
        static initonly @@For := XToken{XKeyword.For} as XToken
        static initonly @@ForEach := XToken{XKeyword.Foreach} as XToken
        static initonly @@Next := XToken{XKeyword.Next} as XToken
        static initonly @@End_For := XToken{XKeyword.End, XKeyword.For} as XToken
        static initonly @@While := XToken{XKeyword.While} as XToken
        static initonly @@Do_While := XToken{XKeyword.Do, XKeyword.While} as XToken
        static initonly @@Enddo := XToken{XKeyword.Enddo} as XToken
        static initonly @@End_Do := XToken{XKeyword.End, XKeyword.Do} as XToken
        static initonly @@End_While := XToken{XKeyword.End, XKeyword.While} as XToken
        static initonly @@If := XToken{XKeyword.If} as XToken
        static initonly @@Else := XToken{XKeyword.Else} as XToken
        static initonly @@Elseif := XToken{XKeyword.Elseif} as XToken
        static initonly @@Endif := XToken{XKeyword.Endif} as XToken
        static initonly @@End_If := XToken{XKeyword.End, XKeyword.If} as XToken
        static initonly @@Repeat := XToken{XKeyword.Repeat} as XToken
        static initonly @@Until := XToken{XKeyword.Until} as XToken
        static initonly @@Try := XToken{XKeyword.Try} as XToken
        static initonly @@Catch := XToken{XKeyword.Catch} as XToken
        static initonly @@Finally := XToken{XKeyword.Finally} as XToken
        static initonly @@End_Try := XToken{XKeyword.End, XKeyword.Try} as XToken

        static initonly @@Text := XToken{XKeyword.Text} as XToken
        static initonly @@Endtext := XToken{XKeyword.Endtext} as XToken
        static initonly @@End_Text := XToken{XKeyword.End, XKeyword.Text} as XToken
        static initonly @@Begin_Sequence := XToken{XKeyword.Begin, XKeyword.Sequence} as XToken
        static initonly @@Recover := XToken{XKeyword.Recover} as XToken
        static initonly @@End_Sequence := XToken{XKeyword.End, XKeyword.Sequence} as XToken
        static initonly @@Switch := XToken{XKeyword.Switch} as XToken
        static initonly @@Do_Switch := XToken{XKeyword.Do, XKeyword.Switch} as XToken
        static initonly @@Begin_Switch := XToken{XKeyword.Begin, XKeyword.Switch} as XToken
        static initonly @@End_Switch := XToken{XKeyword.End, XKeyword.Switch} as XToken
        static initonly @@With := XToken{XKeyword.With} as XToken
        static initonly @@End_With := XToken{XKeyword.End, XKeyword.With} as XToken


        // Blocks
        static initonly @@Begin_Fixed := XToken{XKeyword.Begin, XKeyword.Fixed} as XToken
        static initonly @@End_Fixed := XToken{XKeyword.End, XKeyword.Fixed} as XToken
        static initonly @@Begin_Using := XToken{XKeyword.Begin, XKeyword.Using} as XToken
        static initonly @@End_Using := XToken{XKeyword.End, XKeyword.Using} as XToken
        static initonly @@Begin_Scope := XToken{XKeyword.Begin, XKeyword.Scope} as XToken
        static initonly @@End_Scope := XToken{XKeyword.End, XKeyword.Scope} as XToken
        static initonly @@Begin_Checked := XToken{XKeyword.Begin, XKeyword.Checked} as XToken
        static initonly @@End_Checked := XToken{XKeyword.End, XKeyword.Checked} as XToken
        static initonly @@Begin_Unchecked := XToken{XKeyword.Begin, XKeyword.Unchecked} as XToken
        static initonly @@End_Unchecked := XToken{XKeyword.End, XKeyword.Unchecked} as XToken
        static initonly @@Begin_Lock := XToken{XKeyword.Begin, XKeyword.Lock} as XToken
        static initonly @@End_Lock := XToken{XKeyword.End, XKeyword.Lock} as XToken


        // preprocessor
        static initonly @@PP_If := XToken{XKeyword.PP_if} as XToken
        static initonly @@PP_Ifdef := XToken{XKeyword.PP_ifdef} as XToken
        static initonly @@PP_Ifndef := XToken{XKeyword.PP_ifndef} as XToken
        static initonly @@PP_Else := XToken{XKeyword.PP_else} as XToken
        static initonly @@PP_Endif := XToken{XKeyword.PP_endif} as XToken
        static initonly @@PP_Region := XToken{XKeyword.PP_region} as XToken
        static initonly @@PP_Endregion := XToken{XKeyword.PP_endregion} as XToken
        static initonly @@PP_Text  := XToken{XKeyword.PP_text} as XToken
        static initonly @@PP_EndText := XToken{XKeyword.PP_endtext} as XToken


        static initonly _aliases        as IDictionary<XToken, XToken>
        static initonly _middleKeywords as IDictionary<XToken, List<XToken>>
        static initonly _rulesByStart   as IDictionary<XToken, XFormattingRule>
        static initonly _endKeywords    as IDictionary<XToken, XFormattingRule>

        #endregion

        public PROPERTY Start as XToken GET PRIVATE SET
        public PROPERTY Stop  as XToken GET PRIVATE SET
        public property Flags as XFormattingFlags GET PRIVATE SET



        PRIVATE CONSTRUCTOR(start as XToken, stop as XToken, flags as XFormattingFlags)
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
                _aliases := Dictionary<XToken, XToken>{} {;
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
             _middleKeywords := Dictionary<XToken, List<XToken>> {} {;
                { @@Catch, List<XToken>{} { @@Begin_Sequence, @@Try } }, ;
                { @@Finally, List<XToken>{} { @@Begin_Sequence, @@Try } },;
                { @@Case, List<XToken>{} { @@Do_Case, @@Switch } },;
                { @@Otherwise, List<XToken>{} { @@Do_Case, @@Switch } },;
                { @@Else, List<XToken>{} { @@If } },;
                { @@Elseif, List<XToken>{} { @@If } },;
                { @@PP_Else, List<XToken>{} { @@PP_Ifdef } },;
                { @@Recover, List<XToken>{} { @@Begin_Sequence} } ;
                }

            ValidateRules(rules)

            _rulesByStart := Dictionary<XToken, XFormattingRule>{}
            _endKeywords  := Dictionary<XToken, XFormattingRule>{}
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
            var startKeywords := Dictionary<XToken, XFormattingRule>{}
            var endKeywords := Dictionary<XToken, XFormattingRule>{}
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
                    if (! rule.Flags.HasFlag(XFormattingFlags.HasMiddle))
                        XSettings.ShowMessageBox(i"Formatting rule {first} for middle keyword {item.Key} does not have the Middle Flag set")
                    endif
                next
            next


        #region public methods


        OVERRIDE METHOD ToString() AS STRING
            RETURN SELF:Start:ToString() + " "+SELF:Stop:ToString()

        PUBLIC STATIC METHOD TranslateToken(token as XToken ) AS XToken
            if _aliases:ContainsKey(token)
                return _aliases[token]
            endif
            return token

        PUBLIC STATIC METHOD GetStartRule( token as XToken) AS XFormattingRule
            if _aliases:ContainsKey(token)
                token := _aliases[token]
            endif
            if _rulesByStart.ContainsKey(token)
                return _rulesByStart[token]
            endif
            return DEFAULT(XFormattingRule)

        PUBLIC STATIC METHOD IsMiddleKeyword(token as XToken) AS LOGIC
            RETURN _middleKeywords:ContainsKey(token)

        PUBLIC STATIC METHOD IsEndKeyword(token as XToken) AS LOGIC
            RETURN _endKeywords:ContainsKey(token)


        /// <summary>
        /// Return all the middle tokens that can map a single start token
        /// </summary>
        /// <returns>List of tokens</returns>
        PUBLIC STATIC METHOD MiddleKeywords() as IReadOnlyDictionary<XToken, XToken>

            var tokens := Dictionary<XToken, XToken>{}
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
        PUBLIC STATIC METHOD IndentKeywords()  AS IList<XToken>
            var tokens := List<XToken>{}
            foreach var item in _rulesByStart
                tokens:Add(item:Key)
            next
            return tokens:ToArray()



        /// <summary>
        /// Return all tokens that are the start of a Member
        /// </summary>
        /// <returns>List of tokens</returns>
        PUBLIC static METHOD MemberKeywords() as IList<XToken>
            var tokens := List<XToken>{}
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
        public static method AllowEndKeywords() as IList<XToken>
            var tokens := List<XToken>{}
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
        public static method EndKeywords() as IReadOnlyDictionary<XToken, XToken>
            var tokens := Dictionary<XToken, XToken>{}
            foreach var item in _rulesByStart
                tokens:Add(item:Value:Stop, item:Key)
            next
            return tokens



        /// <summary>
        /// Return all the middle tokens that can map more than one start token
        /// </summary>
        /// <returns>List of tokens</returns>
        public static  method SpecialMiddleKeywords() as IReadOnlyDictionary<XToken, IList<XToken>>
            var tokens := Dictionary<XToken, IList<XToken>>{}
            foreach var item in _middleKeywords
                if (item:Value:Count >= 1)

                    tokens:Add(item:Key, item:Value)
                endif
            next
            return tokens

        #endregion



    END CLASS
END NAMESPACE // XSharpModel.Model
