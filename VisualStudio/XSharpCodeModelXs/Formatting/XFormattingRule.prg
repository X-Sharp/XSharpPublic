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
using System.Diagnostics
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
    STATIC METHOD IsType(SELF flags as XFormattingFlags) AS LOGIC
    return _AND(flags, _OR(XFormattingFlags.Type, XFormattingFlags.Namespace)) != 0

    STATIC METHOD IsEntity(SELF flags as XFormattingFlags) AS LOGIC
    return _AND(flags, _OR(XFormattingFlags.Type, XFormattingFlags.Namespace, XFormattingFlags.Member)) != 0

    END CLASS

    /// <summary>
    /// Flags that describe formatting rules
    /// </summary>
    [Flags];
    enum XFormattingFlags
    member @@None := 0

    /// <summary>
    /// Namespace block
    /// </summary>
    member @@Namespace := 1 << 0


    /// <summary>
    /// Type block.
    /// </summary>
    member @@Type := 1 << 1
    /// <summary>
    /// Member Block
    /// </summary>
    member @@Member := 1 << 2
    /// <summary>
    /// Statement block
    /// </summary>
    member @@Statement := 1 << 3
    /// <summary>
    /// Does the statement have Middle keywords
    /// </summary>
    member @@Middle := 1 << 4
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
    /// Single line is also allowed (PROPERTY, EVENT, ADD etc)
    /// </summary>
    member @@SingleLine := 1 << 8

    /// <summary>
    /// Does the rule allow to end with single END keyword
    /// </summary>
    member @@End := 1 << 8
    /// <summary>
    /// (Combined with type) Can the type be nested
    /// </summary>
    member @@Nested := 1 << 9
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
    static initonly _rulesByStart   as IDictionary<XKeyword, List<XFormattingRule>>
    static initonly _rulesByEnd     as IDictionary<XKeyword, List<XFormattingRule>>
    static initonly _rulesByMiddle  as IDictionary<XKeyword, List<XFormattingRule>>
    static initonly _synonyms       as IDictionary<XKeyword, List<XKeyword> >
    static initonly _singleKeywords as BitArray

    #endregion

    PUBLIC PROPERTY Start as XKeyword AUTO GET PRIVATE SET
    public PROPERTY Stop  as XKeyword AUTO GET PRIVATE SET
    public property Flags as XFormattingFlags AUTO GET PRIVATE SET



    INTERNAL CONSTRUCTOR(start as XKeyword, stop as XKeyword, flags as XFormattingFlags)
    Start := start
    Stop  := stop
    Flags := flags
    RETURN

    #region Static Constructor that builds the tables
    STATIC CONSTRUCTOR()
    _singleKeywords := BitArray{XSharpLexer.LAST, TRUE}
    var source := XSharpModel.Formatting.XFormattingRules.Rules
    var reader := RulesReader{source}
    var rules := reader:ReadRules()

    //ValidateRules(rules)

    _rulesByStart  := Dictionary<XKeyword, List<XFormattingRule>>{}
    _rulesByEnd    := Dictionary<XKeyword, List<XFormattingRule>>{}
    _rulesByMiddle := Dictionary<XKeyword, List<XFormattingRule>>{}
    _synonyms      := Dictionary<XKeyword, List<XKeyword> >{}
    foreach var item in rules
        var Start := item:Start
        var Stop  := item:Stop
        var isEnd := Stop:IsEnd .and. Stop:IsSingle
        if !_rulesByStart:ContainsKey(Start)
            _rulesByStart [Start] := List<XFormattingRule>{}
        endif
        _rulesByStart [Start]:Add(item)
        if isEnd
            var first := _rulesByStart [Start]:First()
            first:Flags |= XFormattingFlags.End
        endif
        // rules with the middle flag do not go to the _rulesByEnd collection
        if item:Flags:HasFlag(XFormattingFlags.Middle)
            if !_rulesByMiddle:ContainsKey(Stop)
                _rulesByMiddle [Stop] := List<XFormattingRule>{}
            endif
            _rulesByMiddle [Stop]:Add(item)
        else
            if !_rulesByEnd:ContainsKey(Stop)
                _rulesByEnd [Stop] := List<XFormattingRule>{}
            endif
            _rulesByEnd [Stop]:Add(item)
            if isEnd
                var first := _rulesByEnd [Stop]:First()
                first:Flags |= XFormattingFlags.End
            endif
        endif
        AddSingleKeyword(item:Start)
        AddSingleKeyword(item:Stop)
    next
    // walk the end rules list for synonyms such as ENDCASE and END CASE
    foreach var item in _rulesByEnd
        var key := item:Key
        var first := TRUE
        foreach var rule in item:Value
            var kwstart := rule:Start
            var startrules := _rulesByStart[kwstart]
            if startrules:Count > 1
                foreach var startrule in startrules
                    if !startrule:Stop:Equals(key) .and. ! startrule:Flags:HasFlag(XFormattingFlags.Middle)
                        if first
                            _synonyms:Add(key, List<XKeyword>{})
                            first := false
                        endif
                        if ! _synonyms[key]:Contains(startrule:Stop)
                            _synonyms[key]:Add(startrule:Stop)
                        endif
                    endif
                next
            endif
         next
    next
    return

    private static method AddSingleKeyword(kw as XKeyword) as void
    if kw:Kw2 == XTokenType.None .and. kw:Kw1 != XTokenType.End
        _singleKeywords:Set( (int) kw:Kw1, TRUE)
    Else
        _singleKeywords:Set( (int) kw:Kw1, FALSE)
    endif

    #endregion

    #region public methods


    PUBLIC STATIC METHOD IsSingleKeyword(token as long) AS LOGIC
        return _singleKeywords:Get( token)

    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Start:ToString() + " " + SELF:Stop:ToString()

    PUBLIC STATIC METHOD GetFirstRuleByStart( token as XKeyword) AS XFormattingRule
        var rules := GetStartRules(token)
        if (rules != null)
            return rules:FirstOrDefault()
        endif
        return DEFAULT(XFormattingRule)

    PUBLIC STATIC METHOD GetFirstRuleByMiddle( token as XKeyword) AS XFormattingRule
        var rules := GetMiddleRules(token)
        if (rules != null)
            return rules:FirstOrDefault()
        endif
        return DEFAULT(XFormattingRule)

     PUBLIC STATIC METHOD GetFirstRuleByEnd( token as XKeyword) AS XFormattingRule
        var rules := GetEndRules(token)
        if (rules != null)
            return rules:FirstOrDefault()
        endif
        return DEFAULT(XFormattingRule)



    PUBLIC STATIC METHOD GetStartRules( token as XKeyword) AS IList<XFormattingRule>
        if _rulesByStart.ContainsKey(token)
            return _rulesByStart[token]
        endif
        return null

    PUBLIC STATIC METHOD GetMiddleRules( token as XKeyword) AS IList<XFormattingRule>
        if _rulesByMiddle.ContainsKey(token)
            return _rulesByMiddle[token]
        endif
        return null

    PUBLIC STATIC METHOD GetEndRules( token as XKeyword) AS IList<XFormattingRule>
        if _rulesByEnd.ContainsKey(token)
            return _rulesByEnd[token]
        endif
        return null

    PUBLIC STATIC METHOD IsSynonym(token1 as XKeyword, token2 as XKeyword) AS LOGIC
        if _synonyms:ContainsKey(token1)
            return _synonyms[token1]:Contains(token2)
        endif
        return false




    PUBLIC STATIC METHOD IsStartKeyword(token as XKeyword) AS LOGIC
    return _rulesByStart.ContainsKey(token)


    PUBLIC STATIC METHOD IsGlobalEntity(token as XKeyword) AS LOGIC
    SWITCH token:Kw1
        CASE XTokenType.Function
        CASE XTokenType.Procedure
        CASE XTokenType.Vostruct
        CASE XTokenType.Union
        CASE XTokenType.Define
        CASE XTokenType.Global
            return true
    END SWITCH
    RETURN FALSE



    PRIVATE STATIC METHOD GetFlags(token as XKeyword) AS XFormattingFlags
    if _rulesByStart.ContainsKey(token)
        var rule := _rulesByStart[token]:First()
        return rule:Flags
    endif
    return XFormattingFlags.None

    PUBLIC STATIC METHOD IsEntity(token as XKeyword) AS LOGIC
    RETURN GetFlags(token):IsEntity()

    PUBLIC STATIC METHOD IsType(token as XKeyword) AS LOGIC
    RETURN GetFlags(token):IsType()

    PUBLIC STATIC METHOD IsMember(token as XKeyword) AS LOGIC
    RETURN GetFlags(token):HasFlag(XFormattingFlags.Member)

    PUBLIC STATIC METHOD IsSingleLineEntity(token as XKeyword) AS LOGIC
    RETURN GetFlags(token):HasFlag(XFormattingFlags.SingleLine)

    PUBLIC STATIC METHOD IsStatement(token as XKeyword) AS LOGIC
    RETURN GetFlags(token):HasFlag(XFormattingFlags.Statement)

    PUBLIC STATIC METHOD IsNestedType(token as XKeyword) AS LOGIC
    var flags := GetFlags(token)
    RETURN flags:IsType() .and. flags:HasFlag(XFormattingFlags.Nested)

    PUBLIC STATIC METHOD IsMiddleKeyword(token as XKeyword) AS LOGIC
    RETURN _rulesByMiddle:ContainsKey(token)

    PUBLIC STATIC METHOD IsEndKeyword(token as XKeyword) AS LOGIC
    RETURN _rulesByEnd:ContainsKey(token)


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
    /// Return all end tokens from the rules list
    /// </summary>
    /// <returns>List of tokens</returns>
    PUBLIC STATIC METHOD OutdentKeywords()  AS IList<XKeyword>
    var tokens := List<XKeyword>{}
    foreach var item in _rulesByEnd
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
        var rule := item:Value:First()
        if (rule:Flags:HasFlag(XFormattingFlags.Member))
            tokens:Add(item:Key)
        endif
    next
    return tokens:ToArray()

    /// <summary>
    /// Return all tokens that can be closed with "just" an END
    /// </summary>
    /// <returns>List of tokens</returns>
    public static method AllowEndKeywords() as IList<XKeyword>
    var tokens := List<XKeyword>{}
    foreach var item in _rulesByEnd
        foreach var rule in item:Value
            if ! tokens:Contains(rule:Start) .and. rule:Stop:IsSingle .and. rule:Stop:IsEnd
                tokens:Add(rule:Start)
            endif
        next
    next
    return tokens:ToArray()

    /// <summary>
    /// Return end keywords that can match more than one start keyword
    /// </summary>
    /// <returns></returns>
    public static method EndKeywords() as IList<XKeyword>
    var tokens := List<XKeyword>{}
    foreach var item in _rulesByEnd
        if item:Value:Count > 1
            tokens:Add(item:Key)
        endif
    next
    return tokens


    #endregion

    END CLASS

    CLASS RulesReader IMPLEMENTS VsParser.IErrorListener
    PROTECT cSource as STRING
    PROTECTED Flags as XFormattingFlags
    PROTECTED Rules as List<XFormattingRule>
    PROTECTED lexer as XSharpLexer

    CONSTRUCTOR(cText as STRING)
    SELF:cSource := cText
    SELF:Rules := List<XFormattingRule>{}

    METHOD ReadRules () AS List<XFormattingRule>
    LOCAL stream := NULL AS ITokenStream
    TRY
        lexer := XSharpLexer.Create(cSource, "rules.txt", XSharpParseOptions.Default)
        XSharp.Parser.VsParser.Lex(cSource, "rules.txt", XSharpParseOptions.Default, SELF, OUT stream, OUT VAR includeFiles)
        var bufferedStream := (BufferedTokenStream) stream
        var tokens := bufferedStream:GetTokens()
        var line := List<IToken>{}
        FOREACH var token  in tokens
            // Read a line
            if token:Channel != 0 .and. token:Channel != XSharpLexer.PREPROCESSORCHANNEL
                LOOP
            endif
            if token:Type != XSharpLexer.EOS
                line:Add(token)
                loop
            endif
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
    FOREACH var rule in Rules
        FOREACH VAR rule2 in Rules
            if rule != rule2
                if rule:Start:Equals(rule2:Start) .and. rule:Stop:Equals(rule2:Stop)
                    XSettings.ShowMessageBox(i"Duplicate rule found: {rule:Start} {rule:End}")
                endif
            endif
        NEXT
    NEXT


    METHOD TokensAsString(line as IList<IToken>) AS STRING
    var tokenstring := ""
    foreach var token in line
        tokenstring += token:Text+" "
    next
    return tokenstring:TrimEnd()

    METHOD GetFlags(name as STRING) AS XFormattingFlags
    local flags as XFormattingFlags
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
        otherwise
            XSettings.ShowMessageBox(i"Incorrect option {name} found")
            flags := XFormattingFlags.None
    END SWITCH
    return flags

    METHOD ProcessFlags(line as IList<IToken>) AS VOID
    // Change the type of the line
    SELF:Flags := GetFlags(line[1]:Text)
    RETURN

    METHOD SplitPairs(line as IList<IToken>, tStart AS IList<IToken>, tEnd AS IList<IToken>, tOptions AS IList<IToken>) AS LOGIC
    local section as int

    section := 1
    local hasQuestion := FALSE as LOGIC
    foreach var token in line
        switch token:Type
            case XSharpLexer.COMMA
                section += 1
                if section > 3
                    XSettings.ShowMessageBox(i"Line found with > 3 sections")
                endif
            otherwise
                if token:Type == XSharpLexer.QMARK
                    hasQuestion := TRUE
                endif
                if section == 1
                    tStart:Add(token)
                elseif section == 2
                    tEnd:Add(token)
                elseif section == 3
                    tOptions:Add(token)
                endif
        end switch
    next
    return hasQuestion

    METHOD ProcessKeywordPairs(line as IList<IToken>) AS VOID
    var tStart := List<IToken>{}
    var tEnd   := List<IToken>{}
    var tOptions := List<IToken>{}
    local rule as XFormattingRule
    local kwStart as XKeyword
    local kwEnd as XKeyword
    local kwFlags as XFormattingFlags
    var hasQuestion := SplitPairs(line, tStart, tEnd, tOptions)
    kwFlags := GetOptions(tOptions)
    if hasQuestion
        var startindex := tStart:IndexOf(tStart:Find({t => t:Type ==XSharpLexer.QMARK}))
        var endindex   := tEnd:IndexOf(tEnd:Find({t => t:Type ==XSharpLexer.QMARK}))
        DO CASE
            CASE startindex > 0 .and.  endindex > 0
                // Only one of the keywords should have a qmark
                Debug.Assert(tStart:Count == 3)
                Debug.Assert(tEnd:Count == 3)
                Debug.Assert(startindex == 1 .or. startindex == 2)
                Debug.Assert(endindex == 1 .or. endindex == 2)
                // both index should be either 1 (A ? B) or 2 (A B ?)
                kwStart := GetKeyword(tStart, startindex, false)
                kwEnd   := GetKeyword(tEnd, endindex, false)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, false)
                kwEnd   := GetKeyword(tEnd, endindex, true)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, true)
                kwEnd   := GetKeyword(tEnd, endindex, false)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

                kwStart := GetKeyword(tStart, startindex, true)
                kwEnd   := GetKeyword(tEnd, endindex, true)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)

            CASE startindex > 0
                Debug.Assert(tStart:Count == 3)
                // index should be either 1 (A ? B) or 2 (A B ?)
                Debug.Assert(startindex == 1 .or. startindex == 2)
                kwEnd   := GetKeyword(tEnd)
                kwStart := GetKeyword(tStart, startindex, false)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
                kwStart := GetKeyword(tStart, startindex, true)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
            CASE endindex > 0
                Debug.Assert(tEnd:Count == 3)
                // index should be either 1 (A ? B) or 2 (A B ?)
                Debug.Assert(endindex == 1 .or. endindex == 2)
                kwStart   := GetKeyword(tStart)
                kwEnd   := GetKeyword(tEnd, endindex, false)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
                kwEnd   := GetKeyword(tEnd, endindex, true)
                rule := XFormattingRule{kwStart, kwEnd, kwFlags}
                SELF:Rules:Add(rule)
            OTHERWISE
                XSettings.ShowMessageBox("Error in line: "+TokensAsString(line))
        end case
    else
        kwStart := GetKeyword(tStart)
        kwEnd   := GetKeyword(tEnd)
        rule := XFormattingRule{kwStart, kwEnd, kwFlags}
        SELF:Rules:Add(rule)
    endif
    RETURN
    METHOD ProcessLine(line as IList<IToken>) AS VOID
    if line:Count == 0
        return
    endif
    if line:Count >= 3 .and. line[0]:Type == XSharpLexer.LBRKT .and. line[2]:Type == XSharpLexer.RBRKT
        ProcessFlags(line)
        RETURN
    endif
    ProcessKeywordPairs(line)

    RETURN

    PRIVATE METHOD GetTokentype(token as IToken) AS LONG
    local type := token:Type as LONG
    IF type == XSharpLexer.ID
        if lexer:KwIds:ContainsKey(token:Text)
            type := lexer:KwIds[token:Text]
        else
            switch token:Text:ToUpper()
                case "ENDDEFINE"
                    type := XSharpLexer.ENDDEFINE
                case "ENDFOR"
                    type := XSharpLexer.NEXT
                case "ENDCLASS"
                    type := XSharpLexer.ENDCLASS
                otherwise
                    XSettings.ShowMessageBox("Unknown Keyword : "+token:Text)
            end switch
        endif
    elseif type == XSharpLexer.SYMBOL_CONST
        var text  := token:Text:ToUpper()
        if lexer:SymPPIds:ContainsKey(text)
            type := lexer:SymPPIds[text]
        endif
    endif
    return type


    PRIVATE METHOD GetKeyword(tokens as IList<IToken>, qmark as int, delete as logic) AS XKeyword
    local kw as XKeyword
    Debug.Assert(tokens:Count == 3)
    Debug.Assert(qmark == 1 .or. qmark == 2)
    Debug.Assert(tokens[qmark]:Type == XSharpLexer.QMARK)
    if delete
        if qmark == 1
            kw := XKeyword{GetTokentype(tokens[2])}
        else
            kw := XKeyword{GetTokentype(tokens[0])}
        endif
    else
        if qmark == 1
            kw := XKeyword{GetTokentype(tokens[0]), GetTokentype(tokens[2])}
        else
            kw := XKeyword{GetTokentype(tokens[0]), GetTokentype(tokens[1])}
        endif
    endif
    return kw

    PRIVATE METHOD GetKeyword(tokens as IList<IToken>) AS XKeyword
    local kw as XKeyword
    if tokens:Count == 1
        kw := XKeyword{GetTokentype(tokens[0])}
    elseif tokens:Count == 2
        kw := XKeyword{GetTokentype(tokens[0]),GetTokentype(tokens[1])}
    else
        kw := XKeyword{GetTokentype(tokens[0]),GetTokentype(tokens[1])}
        XSettings.ShowMessageBox("Keyword has more than 2 tokens: "+TokensAsString(tokens))
    endif
    return kw

    PRIVATE METHOD GetOptions(tOptions as IList<IToken>) AS XFormattingFlags
    local kwFlags as XFormattingFlags
    kwFlags := SELF:Flags
    FOREACH VAR token in tOptions
        kwFlags |= GetFlags(token:Text)
    NEXT
    return kwFlags



    VIRTUAL METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
    RETURN
    VIRTUAL METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
    RETURN


    END CLASS


END NAMESPACE // XSharpModel.Model




