//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------
USING System
USING System.Collections.Generic
USING System.Collections
USING System.Text
USING System.Linq
USING System.Diagnostics
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
USING STATIC XSharp.Parser.VsParser
USING LanguageService.CodeAnalysis.Text
USING XSharp.Parser
#pragma options("az", on)
BEGIN NAMESPACE XSharpModel

STATIC CLASS FormattingExtensions

    STATIC METHOD IsNamespace(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.Namespace) != 0

    STATIC METHOD IsType(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, _OR(XFormattingFlags.Type, XFormattingFlags.Namespace)) != 0

    STATIC METHOD IsEntity(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, _OR(XFormattingFlags.Type, XFormattingFlags.Namespace, XFormattingFlags.Member)) != 0

    STATIC METHOD IsNested(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.Nested) != 0

    STATIC METHOD IsMiddle(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.Middle) != 0

    STATIC METHOD IsJump(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.Jump) != 0

    STATIC METHOD IsCase(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.Case) != 0


    STATIC METHOD IsSingleLine(SELF flags AS XFormattingFlags) AS LOGIC
        RETURN _AND(flags, XFormattingFlags.SingleLine) != 0
END CLASS

/// <summary>
/// Flags that describe formatting rules
/// </summary>
[Flags];
ENUM XFormattingFlags
    MEMBER @@None := 0

    /// <summary>
    /// Namespace block
    /// </summary>
    MEMBER @@Namespace := 1 << 0
    /// <summary>
    /// Type block.
    /// </summary>
    MEMBER @@Type := 1 << 1
    /// <summary>
    /// Member Block
    /// </summary>
    MEMBER @@Member := 1 << 2
    /// <summary>
    /// Statement block
    /// </summary>
    MEMBER @@Statement := 1 << 3
    /// <summary>
    /// Does the statement have Middle keywords
    /// </summary>
    MEMBER @@Middle := 1 << 4
    /// <summary>
    /// Does the statement have CASE / OTHERWISE labels and need to follow the indent CASE setting
    /// </summary>
    MEMBER @@Case := 1 << 5
    /// <summary>
    /// block is a preprocessor block
    /// </summary>
    MEMBER @@Preprocessor := 1 << 6
    /// <summary>
    /// End keyword is optional
    /// </summary>
    MEMBER @@OptionalEnd := 1 << 7
    /// <summary>
    /// Single line is also allowed (PROPERTY, EVENT, ADD etc)
    /// </summary>
    MEMBER @@SingleLine := 1 << 8
    /// <summary>
    /// Does the rule allow to end with single END keyword
    /// </summary>
    MEMBER @@End := 1 << 9
    /// <summary>
    /// (Combined with type) Can the type be nested
    /// </summary>
    MEMBER @@Nested := 1 << 10
    /// <summary>
    ///  Token is an accessor
    /// </summary>
    MEMBER @@Accessor := 1 << 11
    /// <summary>
    ///  Token is an Jump Statement (like EXIT or LOOP)
    /// </summary>
    MEMBER @@Jump := 1 << 12
END ENUM

/// <summary>
/// This type is used to store information about keyword pairs that are used during formatting
/// Each entry has a start token and an stop token and flags that indicate the type of pair
/// The flags entry also has a member that indicates that END without further keyword is allowd
/// and that the END is optional.
/// </summary>

CLASS XFormattingRule
#region Static Fields
    // types
    STATIC INITONLY _rulesByStart   AS IDictionary<XKeyword, List<XFormattingRule>>
    STATIC INITONLY _rulesByEnd     AS IDictionary<XKeyword, List<XFormattingRule>>
    STATIC INITONLY _rulesByMiddle  AS IDictionary<XKeyword, List<XFormattingRule>>
    STATIC INITONLY _rulesByJump    AS IDictionary<XKeyword, List<XFormattingRule>>
    STATIC INITONLY _synonyms       AS IDictionary<XKeyword, List<XKeyword> >
    STATIC INITONLY _singleKeywords AS BitArray
    STATIC INITONLY _jumpTargets    AS IDictionary<XKeyword, LOGIC>

#endregion

    PUBLIC PROPERTY Start AS XKeyword AUTO GET PRIVATE SET
    PUBLIC PROPERTY Stop  AS XKeyword AUTO GET PRIVATE SET
    PUBLIC PROPERTY Flags AS XFormattingFlags AUTO GET PRIVATE SET

    INTERNAL CONSTRUCTOR(start AS XKeyword, stop AS XKeyword, flags AS XFormattingFlags)
        Start := start
        Stop  := stop
        Flags := flags
        RETURN

#region Static Constructor that builds the tables
    STATIC CONSTRUCTOR()
        _singleKeywords := BitArray{XSharpLexer.LAST, TRUE}
        _jumpTargets    := Dictionary<XKeyword, LOGIC>{}
        VAR source := XSharpModel.Formatting.XFormattingRules.Rules
        VAR reader := RulesReader{source}
        VAR rules := reader:ReadRules()

        //ValidateRules(rules)

        _rulesByStart  := Dictionary<XKeyword, List<XFormattingRule>>{}
        _rulesByEnd    := Dictionary<XKeyword, List<XFormattingRule>>{}
        _rulesByMiddle := Dictionary<XKeyword, List<XFormattingRule>>{}
        _rulesByJump   := Dictionary<XKeyword, List<XFormattingRule>>{}
        _synonyms      := Dictionary<XKeyword, List<XKeyword> >{}
        FOREACH VAR item IN rules
            VAR Start := item:Start
            VAR Stop  := item:Stop
            VAR isEnd := Stop:IsEnd .AND. Stop:IsSingle
            IF !_rulesByStart:ContainsKey(Start)
                _rulesByStart [Start] := List<XFormattingRule>{}
            ENDIF
            _rulesByStart [Start]:Add(item)
            IF isEnd
                VAR first := _rulesByStart [Start]:First()
                first:Flags |= XFormattingFlags.End
            ENDIF
            // rules with the middle flag do not go to the _rulesByEnd collection
            IF item:Flags:HasFlag(XFormattingFlags.Middle)
                IF !_rulesByMiddle:ContainsKey(Stop)
                    _rulesByMiddle [Stop] := List<XFormattingRule>{}
                ENDIF
                _rulesByMiddle [Stop]:Add(item)
            ELSEIF item:Flags:HasFlag(XFormattingFlags.Jump)
                IF !_rulesByJump:ContainsKey(Stop)
                    _rulesByJump [Stop] := List<XFormattingRule>{}
                ENDIF
                _rulesByJump [Stop]:Add(item)
                AddJumpTarget(item:Start)

            ELSE
                IF !_rulesByEnd:ContainsKey(Stop)
                    _rulesByEnd [Stop] := List<XFormattingRule>{}
                ENDIF
                _rulesByEnd [Stop]:Add(item)
                IF isEnd
                    VAR first := _rulesByEnd [Stop]:First()
                    first:Flags |= XFormattingFlags.End
                ENDIF
            ENDIF
            AddSingleKeyword(item:Start)
            AddSingleKeyword(item:Stop)
        NEXT
        // walk the end rules list for synonyms such as ENDCASE/END CASE, ENDIF and END IF, ENDDO and END DO and END WHILE
        FOREACH VAR item IN _rulesByEnd
            VAR key := item:Key
            VAR first := TRUE
            FOREACH VAR rule IN item:Value
                VAR kwstart := rule:Start
                VAR startrules := _rulesByStart[kwstart]
                IF startrules:Count > 1
                    FOREACH VAR startrule IN startrules
                        IF !startrule:Stop:Equals(key) .AND. ! startrule:Flags:HasFlag(XFormattingFlags.Middle)
                            IF first
                                _synonyms:Add(key, List<XKeyword>{})
                                first := FALSE
                            ENDIF
                            IF ! _synonyms[key]:Contains(startrule:Stop)
                                _synonyms[key]:Add(startrule:Stop)
                            ENDIF
                        ENDIF
                    NEXT
                ENDIF
            NEXT
        NEXT
        RETURN

    PRIVATE STATIC METHOD AddJumpTarget(kw AS XKeyword) AS VOID
        IF ! _jumpTargets:ContainsKey(kw)
            _jumpTargets:Add(kw, TRUE)
        ENDIF
    PRIVATE STATIC METHOD AddSingleKeyword(kw AS XKeyword) AS VOID
        IF kw:Kw2 == XTokenType.None .AND. kw:Kw1 != XTokenType.End
            _singleKeywords:Set( (INT) kw:Kw1, TRUE)
        ELSE
            _singleKeywords:Set( (INT) kw:Kw1, FALSE)
        ENDIF

#endregion

#region public methods
   PUBLIC STATIC METHOD IsJumpTarget(kw AS XKeyword) AS LOGIC
       RETURN _jumpTargets:ContainsKey(kw)

    PUBLIC STATIC METHOD IsSingleKeyword(token AS LONG) AS LOGIC
        RETURN _singleKeywords:Get( token)

    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Start:ToString() + " " + SELF:Stop:ToString()

    PUBLIC STATIC METHOD GetFirstRuleByStart( token AS XKeyword) AS XFormattingRule
        VAR rules := GetStartRules(token)
        IF (rules != NULL)
            RETURN rules:FirstOrDefault()
        ENDIF
        RETURN DEFAULT(XFormattingRule)

    PUBLIC STATIC METHOD GetFirstRuleByMiddle( token AS XKeyword) AS XFormattingRule
        VAR rules := GetMiddleRules(token)
        IF (rules != NULL)
            RETURN rules:FirstOrDefault()
        ENDIF
        RETURN DEFAULT(XFormattingRule)

    PUBLIC STATIC METHOD GetFirstRuleByEnd( token AS XKeyword) AS XFormattingRule
        VAR rules := GetEndRules(token)
        IF (rules != NULL)
            RETURN rules:FirstOrDefault()
        ENDIF
        RETURN DEFAULT(XFormattingRule)

    PUBLIC STATIC METHOD GetStartRules( token AS XKeyword) AS IList<XFormattingRule>
        IF _rulesByStart.ContainsKey(token)
            RETURN _rulesByStart[token]
        ENDIF
        RETURN NULL

    PUBLIC STATIC METHOD GetMiddleRules( token AS XKeyword) AS IList<XFormattingRule>
        IF _rulesByMiddle.ContainsKey(token)
            RETURN _rulesByMiddle[token]
        ENDIF
        RETURN NULL

    PUBLIC STATIC METHOD GetJumpRules( token AS XKeyword) AS IList<XFormattingRule>
        IF _rulesByJump.ContainsKey(token)
            RETURN _rulesByJump[token]
        ENDIF
        RETURN NULL

    PUBLIC STATIC METHOD GetEndRules( token AS XKeyword) AS IList<XFormattingRule>
        IF _rulesByEnd.ContainsKey(token)
            RETURN _rulesByEnd[token]
        ENDIF
        RETURN NULL

    PUBLIC STATIC METHOD IsSynonym(token1 AS XKeyword, token2 AS XKeyword) AS LOGIC
        IF _synonyms:ContainsKey(token1)
            RETURN _synonyms[token1]:Contains(token2)
        ENDIF
        RETURN FALSE




    PUBLIC STATIC METHOD IsStartKeyword(token AS XKeyword) AS LOGIC
        RETURN _rulesByStart.ContainsKey(token)


    PUBLIC STATIC METHOD IsGlobalEntity(token AS XKeyword) AS LOGIC
        SWITCH token:Kw1
        CASE XTokenType.Function
        CASE XTokenType.Procedure
        CASE XTokenType.Vostruct
        CASE XTokenType.Union
        CASE XTokenType.Define
        CASE XTokenType.Global
            RETURN TRUE
        END SWITCH
        RETURN FALSE



    PRIVATE STATIC METHOD GetFlags(token AS XKeyword) AS XFormattingFlags
        IF _rulesByStart.ContainsKey(token)
            VAR rule := _rulesByStart[token]:First()
            RETURN rule:Flags
        ENDIF
        RETURN XFormattingFlags.None

    PUBLIC STATIC METHOD IsEntity(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):IsEntity()

    PUBLIC STATIC METHOD IsType(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):IsType()

    PUBLIC STATIC METHOD IsNamespace(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):IsNamespace()

    PUBLIC STATIC METHOD IsMember(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):HasFlag(XFormattingFlags.Member)

    PUBLIC STATIC METHOD IsAccessor(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):HasFlag(XFormattingFlags.Accessor)

    PUBLIC STATIC METHOD IsSingleLineEntity(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):HasFlag(XFormattingFlags.SingleLine)

    PUBLIC STATIC METHOD IsStatement(token AS XKeyword) AS LOGIC
        RETURN GetFlags(token):HasFlag(XFormattingFlags.Statement)

    PUBLIC STATIC METHOD IsNestedType(token AS XKeyword) AS LOGIC
        VAR flags := GetFlags(token)
        RETURN flags:IsType() .AND. flags:IsNested()

    PUBLIC STATIC METHOD IsMiddleKeyword(token AS XKeyword) AS LOGIC
        RETURN _rulesByMiddle:ContainsKey(token)

    PUBLIC STATIC METHOD IsJumpKeyword(token AS XKeyword) AS LOGIC
        RETURN _rulesByJump:ContainsKey(token)

    PUBLIC STATIC METHOD IsEndKeyword(token AS XKeyword) AS LOGIC
        RETURN _rulesByEnd:ContainsKey(token)

    PUBLIC STATIC METHOD IsEndOfType(token AS XKeyword) AS LOGIC
        IF _rulesByEnd:ContainsKey(token)
            VAR rule := XFormattingRule.GetFirstRuleByEnd(token)
            RETURN rule != NULL .AND. rule:Flags:HasFlag(XFormattingFlags.Type)
        ENDIF
        RETURN FALSE
    /// <summary>
    /// Return all start tokens from the rules list
    /// </summary>
    /// <returns>List of tokens</returns>
    PUBLIC STATIC METHOD IndentKeywords()  AS IList<XKeyword>
        VAR tokens := List<XKeyword>{}
        FOREACH VAR item IN _rulesByStart
            tokens:Add(item:Key)
        NEXT
        RETURN tokens:ToArray()

    /// <summary>
    /// Return all end tokens from the rules list
    /// </summary>
    /// <returns>List of tokens</returns>
    PUBLIC STATIC METHOD OutdentKeywords()  AS IList<XKeyword>
        VAR tokens := List<XKeyword>{}
        FOREACH VAR item IN _rulesByEnd
            tokens:Add(item:Key)
        NEXT
        RETURN tokens:ToArray()


    /// <summary>
    /// Return all tokens that are the start of a Member
    /// </summary>
    /// <returns>List of tokens</returns>
    PUBLIC STATIC METHOD MemberKeywords() AS IList<XKeyword>
        VAR tokens := List<XKeyword>{}
        // Entities
        FOREACH VAR item IN _rulesByStart
            VAR rule := item:Value:First()
            IF (rule:Flags:HasFlag(XFormattingFlags.Member))
                tokens:Add(item:Key)
            ENDIF
        NEXT
        RETURN tokens:ToArray()

    /// <summary>
    /// Return all tokens that can be closed with "just" an END
    /// </summary>
    /// <returns>List of tokens</returns>
    PUBLIC STATIC METHOD AllowEndKeywords() AS IList<XKeyword>
        VAR tokens := List<XKeyword>{}
        FOREACH VAR item IN _rulesByEnd
            FOREACH VAR rule IN item:Value
                IF ! tokens:Contains(rule:Start) .AND. rule:Stop:IsSingle .AND. rule:Stop:IsEnd
                    tokens:Add(rule:Start)
                ENDIF
            NEXT
        NEXT
        RETURN tokens:ToArray()

    /// <summary>
    /// Return end keywords that can match more than one start keyword
    /// </summary>
    /// <returns></returns>
    PUBLIC STATIC METHOD EndKeywords() AS IList<XKeyword>
        VAR tokens := List<XKeyword>{}
        FOREACH VAR item IN _rulesByEnd
            IF item:Value:Count > 1
                tokens:Add(item:Key)
            ENDIF
        NEXT
        RETURN tokens


#endregion

END CLASS

CLASS RulesReader IMPLEMENTS VsParser.IErrorListener
    PROTECT cSource AS STRING
    PROTECTED Flags AS XFormattingFlags
    PROTECTED Rules AS List<XFormattingRule>
    PROTECTED lexer AS XSharpLexer

    CONSTRUCTOR(cText AS STRING)
        SELF:cSource := cText
        SELF:Rules := List<XFormattingRule>{}

    METHOD ReadRules () AS List<XFormattingRule>
        LOCAL stream := NULL AS ITokenStream
        TRY
            lexer := XSharpLexer.Create(cSource, "rules.txt", XSharpParseOptions.Default)
            XSharp.Parser.VsParser.Lex(cSource, "rules.txt", XSharpParseOptions.Default, SELF, OUT stream, OUT VAR includeFiles)
            VAR bufferedStream := (BufferedTokenStream) stream
            VAR tokens := bufferedStream:GetTokens()
            VAR line := List<IToken>{}
            FOREACH VAR token  IN tokens
                // Read a line
                IF token:Channel != 0 .AND. token:Channel != XSharpLexer.PREPROCESSORCHANNEL
                    LOOP
                ENDIF
                IF token:Type != XSharpLexer.EOS
                    line:Add(token)
                    LOOP
                ENDIF
                // we have reached the end of the line
                SELF:ProcessLine(line)
                line:Clear()
            NEXT


        CATCH
            Rules:Clear()
            RETURN Rules
        END TRY
        SELF:ValidateRules()
        RETURN Rules

    METHOD ValidateRules() AS VOID
        FOREACH VAR rule IN Rules
            FOREACH VAR rule2 IN Rules
                IF rule != rule2
                    IF rule:Start:Equals(rule2:Start) .AND. rule:Stop:Equals(rule2:Stop)
                        XSettings.ShowMessageBox(i"Duplicate rule found: {rule:Start} {rule:End}")
                    ENDIF
                ENDIF
            NEXT
        NEXT


    METHOD TokensAsString(line AS IList<IToken>) AS STRING
        VAR tokenstring := ""
        FOREACH VAR token IN line
            tokenstring += token:Text+" "
        NEXT
        RETURN tokenstring:TrimEnd()

    METHOD GetFlags(name AS STRING) AS XFormattingFlags
        LOCAL flags AS XFormattingFlags
        name := name:ToUpper():Trim()
        SWITCH name
        CASE "NAMESPACE"
            flags := XFormattingFlags.Namespace
        CASE "TYPE"
            flags := XFormattingFlags.Type
        CASE "MEMBER"
            flags := XFormattingFlags.Member
        CASE "STATEMENT"
            flags := XFormattingFlags.Statement
        CASE "PREPROCESSOR"
            flags := XFormattingFlags.Preprocessor
        CASE "NESTED"
            flags := XFormattingFlags.Nested
        CASE "OPTIONAL"
            flags := XFormattingFlags.OptionalEnd
        CASE "CASE"
            flags := XFormattingFlags.Case
        CASE "MIDDLE"
            flags := XFormattingFlags.Middle
        CASE "SINGLELINE"
            flags := XFormattingFlags.SingleLine
        CASE "ACCESSOR"
            flags := XFormattingFlags.Accessor
        CASE "JUMP"
            flags := XFormattingFlags.Jump
        OTHERWISE
            XSettings.ShowMessageBox(i"Incorrect option {name} found")
            flags := XFormattingFlags.None
        END SWITCH
        RETURN flags

    METHOD ProcessFlags(line AS IList<IToken>) AS VOID
        // Change the type of the line
        SELF:Flags := GetFlags(line[1]:Text)
        RETURN

    METHOD SplitPairs(line AS IList<IToken>, tStart AS IList<IToken>, tEnd AS IList<IToken>, tOptions AS IList<IToken>) AS LOGIC
        LOCAL section AS INT

        section := 1
        LOCAL hasQuestion := FALSE AS LOGIC
        FOREACH VAR token IN line
            SWITCH token:Type
            CASE XSharpLexer.COMMA
                section += 1
                IF section > 3
                    XSettings.ShowMessageBox(i"Line found with > 3 sections")
                ENDIF
            OTHERWISE
                IF token:Type == XSharpLexer.QMARK
                    hasQuestion := TRUE
                ENDIF
                IF section == 1
                    tStart:Add(token)
                ELSEIF section == 2
                    tEnd:Add(token)
                ELSEIF section == 3
                    tOptions:Add(token)
                ENDIF
            END SWITCH
        NEXT
        RETURN hasQuestion

    METHOD ProcessKeywordPairs(line AS IList<IToken>) AS VOID
        VAR tStart := List<IToken>{}
        VAR tEnd   := List<IToken>{}
        VAR tOptions := List<IToken>{}
        LOCAL rule AS XFormattingRule
        LOCAL kwStart AS XKeyword
        LOCAL kwEnd AS XKeyword
        LOCAL kwFlags AS XFormattingFlags
        VAR hasQuestion := SplitPairs(line, tStart, tEnd, tOptions)
        kwFlags := GetOptions(tOptions)
        IF hasQuestion
            VAR startindex := tStart:IndexOf(tStart:Find({t => t:Type ==XSharpLexer.QMARK}))
            VAR endindex   := tEnd:IndexOf(tEnd:Find({t => t:Type ==XSharpLexer.QMARK}))
            DO CASE
            CASE startindex > 0 .AND.  endindex > 0
                // Only one of the keywords should have a qmark
                Debug.Assert(tStart:Count == 3)
                Debug.Assert(tEnd:Count == 3)
                Debug.Assert(startindex == 1 .OR. startindex == 2)
                Debug.Assert(endindex == 1 .OR. endindex == 2)
                // both index should be either 1 (A ? B) or 2 (A B ?)
                kwStart := GetKeyword(tStart, startindex, FALSE)
                kwEnd   := GetKeyword(tEnd, endindex, FALSE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, FALSE)
                kwEnd   := GetKeyword(tEnd, endindex, TRUE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, TRUE)
                kwEnd   := GetKeyword(tEnd, endindex, FALSE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, TRUE)
                kwEnd   := GetKeyword(tEnd, endindex, TRUE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

            CASE startindex > 0
                Debug.Assert(tStart:Count == 3)
                // index should be either 1 (A ? B) or 2 (A B ?)
                Debug.Assert(startindex == 1 .OR. startindex == 2)
                kwEnd   := GetKeyword(tEnd)
                kwStart := GetKeyword(tStart, startindex, FALSE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
                kwStart := GetKeyword(tStart, startindex, TRUE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
            CASE endindex > 0
                Debug.Assert(tEnd:Count == 3)
                // index should be either 1 (A ? B) or 2 (A B ?)
                Debug.Assert(endindex == 1 .OR. endindex == 2)
                kwStart   := GetKeyword(tStart)
                kwEnd   := GetKeyword(tEnd, endindex, FALSE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
                kwEnd   := GetKeyword(tEnd, endindex, TRUE)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
            OTHERWISE
                XSettings.ShowMessageBox("Error in line: "+TokensAsString(line))
            END CASE
        ELSE
            kwStart := GetKeyword(tStart)
            kwEnd   := GetKeyword(tEnd)
            rule := XFormattingRule{kwStart, kwEnd, kwFlags}
            SELF:Rules:Add(rule)
        ENDIF
        RETURN
    METHOD ProcessLine(line AS IList<IToken>) AS VOID
        IF line:Count == 0
            RETURN
        ENDIF
        IF line:Count >= 3 .AND. line[0]:Type == XSharpLexer.LBRKT .AND. line[2]:Type == XSharpLexer.RBRKT
            ProcessFlags(line)
            RETURN
        ENDIF
        ProcessKeywordPairs(line)

        RETURN

    PRIVATE METHOD GetTokentype(token AS IToken) AS LONG
        LOCAL type := token:Type AS LONG
        IF type == XSharpLexer.ID
            IF lexer:KwIds:ContainsKey(token:Text)
                type := lexer:KwIds[token:Text]
            ELSE
                SWITCH token:Text:ToUpper()
                CASE "ENDDEFINE"
                    type := XSharpLexer.ENDDEFINE
                CASE "ENDFOR"
                    type := XSharpLexer.NEXT
                CASE "ENDCLASS"
                    type := XSharpLexer.ENDCLASS
                OTHERWISE
                    XSettings.ShowMessageBox("Unknown Keyword : "+token:Text)
                END SWITCH
            ENDIF
        ELSEIF type == XSharpLexer.SYMBOL_CONST
            VAR text  := token:Text:ToUpper()
            IF lexer:SymPPIds:ContainsKey(text)
                type := lexer:SymPPIds[text]
            ENDIF
        ENDIF
        RETURN type


    PRIVATE METHOD GetKeyword(tokens AS IList<IToken>, qmark AS INT, delete AS LOGIC) AS XKeyword
        LOCAL kw AS XKeyword
        Debug.Assert(tokens:Count == 3)
        Debug.Assert(qmark == 1 .OR. qmark == 2)
        Debug.Assert(tokens[qmark]:Type == XSharpLexer.QMARK)
        IF delete
            IF qmark == 1
                kw := XKeyword{GetTokentype(tokens[2])}
            ELSE
                kw := XKeyword{GetTokentype(tokens[0])}
            ENDIF
        ELSE
            IF qmark == 1
                kw := XKeyword{GetTokentype(tokens[0]), GetTokentype(tokens[2])}
            ELSE
                kw := XKeyword{GetTokentype(tokens[0]), GetTokentype(tokens[1])}
            ENDIF
        ENDIF
        RETURN kw

    PRIVATE METHOD GetKeyword(tokens AS IList<IToken>) AS XKeyword
        LOCAL kw AS XKeyword
        IF tokens:Count == 1
            kw := XKeyword{GetTokentype(tokens[0])}
        ELSEIF tokens:Count == 2
            kw := XKeyword{GetTokentype(tokens[0]),GetTokentype(tokens[1])}
        ELSE
            kw := XKeyword{GetTokentype(tokens[0]),GetTokentype(tokens[1])}
            XSettings.ShowMessageBox("Keyword has more than 2 tokens: "+TokensAsString(tokens))
        ENDIF
        RETURN kw

    PRIVATE METHOD GetOptions(tOptions AS IList<IToken>) AS XFormattingFlags
        LOCAL kwFlags AS XFormattingFlags
        kwFlags := SELF:Flags
        FOREACH VAR token IN tOptions
            kwFlags |= GetFlags(token:Text)
        NEXT
        RETURN kwFlags



    VIRTUAL METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        RETURN
    VIRTUAL METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        RETURN


END CLASS


END NAMESPACE // XSharpModel.Model




