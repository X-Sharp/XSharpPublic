// XLexer.prg
// Created by    : robert
// Creation Date : 1/6/2021 3:37:34 PM
// Created for   :
// WorkStation   : NYX

USING System
USING System.Collections.Generic
USING System.Globalization
BEGIN NAMESPACE XSharp.Parsers
CLASS XSqlLexer INHERIT XLexer
    STATIC aKeywords AS Dictionary<STRING, XTokenType>
    STATIC CONSTRUCTOR
        aKeywords := Dictionary<STRING, XTokenType>{StringComparer.OrdinalIgnoreCase}
        FOR VAR i := XTokenType.FIRST_KEYWORD+1 TO XTokenType.LAST_KEYWORD-1
            VAR name := System.Enum.GetName(TYPEOF(XTokenType), i)
            aKeywords:Add(name, i)

        NEXT
        RETURN

    CONSTRUCTOR(source AS STRING )
        SUPER(source)

    OVERRIDE METHOD NextToken() AS XToken
        LOCAL start AS INT
        LOCAL t AS XTokenType
        LOCAL ch AS XChannel
        LOCAL strValue AS STRING
        LOCAL ws := "" AS STRING
        IF !SELF:Eoi()
            REPEAT
                start := SELF:_index
                t	:= XTokenType.UNRECOGNIZED
                ch := XChannel.Default
                strValue := NULL
                VAR c := SELF:La()
                strValue := c:ToString()
                IF c < 128
                    t := TokenAttr.specialTable[c]
                    SELF:Consume()
                ELSE
                    IF SELF:ExpectIdStart()
                        t := XTokenType.ID
                    ELSE
                        SELF:Consume()
                    ENDIF
                ENDIF
                IF t == XTokenType.ID
                    // check for string prefixes
                    IF c == c'n' .OR. c == c'N'						// SQL only has Single quoted strings.
                        IF SELF:La() == c'\''
                            SELF:Consume()
                            t := XTokenType.STRING_CONST
                        ENDIF
                    ENDIF
                ENDIF

                SWITCH t
                CASE XTokenType.ID
                    DO WHILE ExpectIdChar()
                        NOP
                    ENDDO
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)
                    // check for keyword when Source does not start with "@@"
                CASE XTokenType.CHAR_CONST
                    t := XTokenType.CHAR_CONST
                    IF SELF:La() == c'\\' .AND. SELF:La(3) == c'\''
                        SELF:Consume(3)
                    ELSE
                        DO WHILE !SELF:Reach(c'\'')
                            NOP
                        ENDDO
                        IF !SELF:Expect(c'\'')
                            t := XTokenType.INVALID_STRING_CONST
                        ENDIF
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)
                    t := XTokenType.STRING_CONST
                    WHILE !SELF:Reach(c'\'')
                        NOP
                    END WHILE
                    IF !SELF:Expect(c'\'')
                        t := XTokenType.INVALID_STRING_CONST
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)

                CASE XTokenType.DQUOTE
                    t := XTokenType.ID
                    DO WHILE !SELF:Reach(c'"')
                        NOP
                    ENDDO
                    IF !SELF:Expect(c'"')
                        t := XTokenType.INCOMPLETE_ID
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)
                CASE XTokenType.LBRKT
                    t := XTokenType.ID
                    DO WHILE !SELF:Reach(c']')
                        NOP
                    ENDDO
                    IF !SELF:Expect(c']')
                        t := XTokenType.INCOMPLETE_ID
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)

                CASE XTokenType.ADDROF
                    IF SELF:Expect(c'@')
                        t := XTokenType.ATAT
                    ENDIF

                CASE XTokenType.COLON
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_OP
                    ENDIF

                CASE XTokenType.PIPE
                    IF SELF:Expect(c'|')
                        t := XTokenType.PIPEPIPE
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_BITOR
                        ENDIF
                    ENDIF
                CASE XTokenType.AMP
                    IF SELF:Expect(c'&')
                        t := XTokenType.AMPAMP
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_BITAND
                        ENDIF
                    ENDIF
                CASE XTokenType.MINUS
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_SUB
                    ELSEIF SELF:Expect(c'-')
                        t := XTokenType.SL_COMMENT
                        ch := XChannel.Hidden
                        DO WHILE !SELF:ReachEol()
                            NOP
                        ENDDO
                    ENDIF
                CASE XTokenType.PLUS
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_ADD
                    ENDIF
                CASE XTokenType.LT
                    IF SELF:Expect(c'<')
                        t := XTokenType.LSHIFT
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_LSHIFT
                        ENDIF
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.LTE
                            IF SELF:Expect(c'>')
                                t := XTokenType.NULLSAFE_EQ
                            ENDIF
                        ELSE
                            IF SELF:Expect(c'>')
                                t := XTokenType.NEQ
                            ENDIF
                        ENDIF
                    ENDIF
                CASE XTokenType.GT
                    IF SELF:Expect(c'=')
                        t := XTokenType.GTE
                    ELSE
                        IF SELF:Expect(c'>', c'=')
                            t := XTokenType.ASSIGN_RSHIFT
                        ENDIF
                    ENDIF
                CASE XTokenType.MOD
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_MOD
                    ENDIF
                CASE XTokenType.DIV
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_DIV
                    ELSE
                        IF SELF:Expect(c'*')
                            t := XTokenType.ML_COMMENT
                            ch := XChannel.Hidden
                            DO WHILE !SELF:Reach(c'*', c'/')
                                NOP
                            ENDDO
                            SELF:Expect(c'*', c'/')
                        ENDIF
                    ENDIF
                CASE XTokenType.EXP
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_EXP
                    ENDIF

                CASE XTokenType.TILDE
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_XOR
                    ENDIF
                CASE XTokenType.EQ
                    IF SELF:Expect(c'=')
                        t := XTokenType.EEQ
                    ELSE
                        IF SELF:Expect(c'>')
                            t := XTokenType.UDCSEP
                        ENDIF
                    ENDIF
                CASE XTokenType.EXCLAMATIONMARK
                    IF SELF:Expect(c'=')
                        t := XTokenType.NEQ
                    ENDIF
                CASE XTokenType.QMARK
                    IF SELF:Expect(c'?')
                        t := XTokenType.QQMARK
                    ENDIF
                CASE XTokenType.NL
                    IF c == c'\r'
                        SELF:Expect(c'\n')
                    ENDIF
                    t := XTokenType.WS
                    ch := XChannel.Hidden
                    ws := SELF:_Source:Substring(start, SELF:_index - start)

                CASE XTokenType.WS
                    ch := XChannel.Hidden
                    DO WHILE SELF:ExpectAny(c' ', c'\t')
                        NOP
                    ENDDO
                    ws := SELF:_Source:Substring(start, SELF:_index - start)
                CASE XTokenType.DOT
                    IF SELF:La() >= c'0' .AND. SELF:La() <= c'9'
                        t := XTokenType.NUMERIC_CONST	// Handle later
                    ELSEIF SELF:La(2) == c'.'
                        IF SELF:ExpectAny(c'F', c'f', c'N', c'n')
                            SELF:Consume()	// eat the closing dot
                            t := XTokenType.FALSE_CONST
                            strValue := "FALSE"
                        ELSE
                            IF SELF:ExpectAny(c'T', c't', c'Y', c'y')
                                SELF:Consume() // eat the closing dot
                                t := XTokenType.TRUE_CONST
                                strValue := "TRUE"
                            ELSE
                                IF SELF:Expect(c'.')
                                    SELF:Consume()// eat the closing dot
                                    t := XTokenType.ELLIPSIS
                                ENDIF
                                strValue := "..."
                            ENDIF
                        ENDIF
                    ELSE
                        IF SELF:La(3) == c'.'
                            IF SELF:ExpectLower("or")
                                SELF:Consume() // eat the closing dot
                                t := XTokenType.LOGIC_OR
                            ENDIF
                        ELSE
                            IF SELF:La(4) == c'.'
                                IF SELF:ExpectLower("and")
                                    SELF:Consume() // eat the closing dot
                                    t := XTokenType.LOGIC_AND
                                ELSE
                                    IF SELF:ExpectLower("not")
                                        SELF:Consume()// eat the closing dot
                                        t := XTokenType.LOGIC_NOT
                                    ELSE
                                        IF SELF:ExpectLower("xor")
                                            SELF:Consume() // eat the closing dot
                                            t := XTokenType.LOGIC_XOR
                                        ENDIF
                                    ENDIF
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF

                CASE XTokenType.POUND							// # as start of line comment
                    IF SELF:_lastToken == XTokenType.NL
                        t  := XTokenType.SL_COMMENT
                        ch := XChannel.Hidden
                        DO WHILE !SELF:ReachEol()
                            NOP
                        ENDDO
                    ENDIF

                CASE XTokenType.MULT
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_MUL
                    ELSE
                        IF SELF:Expect(c'*')
                            t := XTokenType.EXP
                            IF SELF:Expect(c'=')
                                t := XTokenType.ASSIGN_EXP
                            ENDIF
                        ENDIF
                    ENDIF
                CASE XTokenType.SEMI
                    ch := XChannel.Hidden
                END SWITCH
                IF t == XTokenType.NUMERIC_CONST
                    IF c == c'0' .AND. SELF:ExpectAny(c'X', c'x')
                        // Hex Notation
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:ExpectRange(c'A', c'F') .OR. SELF:ExpectRange(c'a', c'f')
                            NOP
                        ENDDO
                    ELSE
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:ExpectAny(c'.',c'e',c'E')
                            NOP
                        ENDDO
                    ENDIF
                    strValue:= SELF:_Source:Substring(start, SELF:_index - start)
                    IF strValue:Contains(".") .AND. strValue:Replace(".",""):Length == strValue:Length -2
                        t := XTokenType.DATE_CONST
                    ENDIF
                ENDIF
                IF ch == XChannel.Default
                    IF t == XTokenType.ID
                        var kw := LookupKeyword(strValue)
                        IF kw != XTokenType.UNRECOGNIZED
                            t := kw
                        ENDIF
                    ENDIF
                    SELF:_lastToken := t
                    VAR Token := XToken{SELF, t, start, SELF:_index - start, strValue, ch}
                    Token:Leadingws := ws
                    ws := ""
                    RETURN Token
                ENDIF
            UNTIL SELF:Eoi()

        ENDIF
        IF !SELF:_hasEos
            SELF:_hasEos := TRUE
            RETURN XToken{SELF, XTokenType.EOS, SELF:_index, SELF:_index, NULL, XChannel.Default}
        ENDIF
        RETURN NULL
    OVERRIDE METHOD LookupKeyword(strValue AS STRING) AS XTokenType
        IF aKeywords:TryGetValue(strValue, OUT VAR number )
            RETURN number
        ENDIF
        RETURN XTokenType.UNRECOGNIZED
END CLASS
END NAMESPACE
