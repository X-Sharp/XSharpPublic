

USING System.Collections.Generic
USING System.Globalization

BEGIN NAMESPACE XSharp.Parsers
CLASS XBaseLexer INHERIT XLexer
    CONSTRUCTOR(source AS STRING )
        SUPER(source)

    OVERRIDE METHOD NextToken() AS XToken
        LOCAL start AS INT
        LOCAL t AS XTokenType
        LOCAL st AS XTokenType
        LOCAL ch AS XChannel
        LOCAL strValue AS STRING
        IF !SELF:Eoi()
            REPEAT
                start := SELF:_index
                t := XTokenType.UNRECOGNIZED
                st := XTokenType.UNRECOGNIZED
                ch := XChannel.Default
                strValue := NULL
                VAR c := SELF:La()
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
                    IF c == c'c' .OR. c == c'C'
                        IF SELF:La() == c'"' .OR. SELF:La() == c'\''
                            SELF:Consume()
                            t := XTokenType.CHAR_CONST
                        ENDIF
                    ELSEIF c == c'e' .OR. c == c'E'
                        IF SELF:La() == c'"'
                            SELF:Consume()
                            t := XTokenType.STRING_CONST
                        ELSEIF (SELF:La() == c'i' .OR. SELF:La() == c'I') .AND. SELF:La(2) == c'"'
                            SELF:Consume(2)
                            t := XTokenType.STRING_CONST
                        ENDIF
                    ELSEIF c == c'i' .OR. c == c'I'
                        IF SELF:La() == c'"'
                            SELF:Consume()
                            t := XTokenType.STRING_CONST
                        ELSE
                            IF (SELF:La() == c'e' .OR. SELF:La() == c'E') .AND. SELF:La(2) == c'"'
                                SELF:Consume(2)
                                t := XTokenType.STRING_CONST
                            ENDIF
                        ENDIF
                    ENDIF
                ELSEIF t == XTokenType.ADDROF
                    IF SELF:Expect(c'@')
                        t := XTokenType.ID
                        SELF:Consume()
                    ENDIF
                ELSEIF t == XTokenType.DQUOTE
                    t := XTokenType.STRING_CONST
                ENDIF

                SWITCH t
                CASE XTokenType.ID
                    DO WHILE ExpectIdChar()
                        NOP
                    ENDDO
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)

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

                CASE XTokenType.STRING_CONST
                    DO WHILE !SELF:Reach(c'"')
                        NOP
                    ENDDO
                    IF !SELF:Expect(c'"')
                        t := XTokenType.INVALID_STRING_CONST
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)

                CASE XTokenType.SQUOTE
                    t := XTokenType.STRING_CONST
                    DO WHILE !SELF:Reach(c'\'')
                        NOP
                    ENDDO
                    IF !SELF:Expect(c'\'')
                        t := XTokenType.INVALID_STRING_CONST
                    ENDIF
                    strValue := SELF:_Source:Substring(start, SELF:_index - start)

                CASE XTokenType.LCURLY
                    IF SELF:Expect(c'^')
                        t := XTokenType.DATETIME_CONST
                        DO WHILE !SELF:Reach(c'}')
                            NOP
                        ENDDO
                        IF !SELF:Expect(c'}')
                            t := XTokenType.INVALID_STRING_CONST
                        ENDIF
                        strValue := SELF:_Source:Substring(start, SELF:_index - start)
                    ENDIF
                CASE XTokenType.COLON
                    IF SELF:Expect(c':')
                        t := XTokenType.COLONCOLON
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_OP
                        ENDIF
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
                        IF SELF:AllowOldStyleComments
                            t := XTokenType.SL_COMMENT
                            ch := XChannel.Hidden
                            SELF:ConsumeToEol()
                        ELSE
                            t := XTokenType.AMPAMP
                        ENDIF
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_BITAND
                        ENDIF
                    ENDIF
                CASE XTokenType.MINUS
                    IF SELF:Expect(c'>')
                        t := XTokenType.ALIAS
                    ELSE
                        IF SELF:Expect(c'-')
                            t := XTokenType.DEC
                        ELSE
                            IF SELF:Expect(c'=')
                                t := XTokenType.ASSIGN_SUB
                            ENDIF
                        ENDIF
                    ENDIF
                CASE XTokenType.PLUS
                    IF SELF:Expect(c'+')
                        t := XTokenType.INC
                    ELSE
                        IF SELF:Expect(c'=')
                            t := XTokenType.ASSIGN_ADD
                        ENDIF
                    ENDIF
                  CASE XTokenType.DIV
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_DIV
                    ELSE
                        IF SELF:Expect(c'/')
                            t := XTokenType.SL_COMMENT
                            ch := XChannel.Hidden
                            if SELF:Expect(c'/')
                                t := XTokenType.DOC_COMMENT
                            endif
                            SELF:ConsumeToEol()
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
                    ENDIF
               CASE XTokenType.MOD
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_MOD
                    ENDIF
                CASE XTokenType.EXP
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_EXP
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
               CASE XTokenType.TILDE
                    IF SELF:Expect(c'=')
                        t := XTokenType.ASSIGN_XOR
                    ENDIF
               CASE XTokenType.MULT
                    IF SELF:_lastToken == XTokenType.NL
                        t  := XTokenType.SL_COMMENT
                        ch := XChannel.Hidden
                        SELF:ConsumeToEol()
                    ENDIF
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

                CASE XTokenType.QMARK
                    IF SELF:Expect(c'?')
                        t := XTokenType.QQMARK
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
                CASE XTokenType.NL
                    IF c == c'\r'
                        SELF:Expect(c'\n')
                    ENDIF
                CASE XTokenType.WS
                    ch := XChannel.Hidden
                    DO WHILE SELF:ExpectAny(c' ', c'\t')
                        NOP
                    ENDDO
                CASE XTokenType.DOT
                    IF SELF:La() >= c'0' .AND. SELF:La() <= c'9'
                        t := XTokenType.NUMERIC_CONST	// Handle later
                    ELSEIF SELF:La(2) == c'.'
                        IF SELF:ExpectAny(c'F', c'f', c'N', c'n')
                            SELF:Consume()	// eat the closing dot
                            t := XTokenType.FALSE_CONST
                        ELSE
                            IF SELF:ExpectAny(c'T', c't', c'Y', c'y')
                                SELF:Consume() // eat the closing dot
                                t := XTokenType.TRUE_CONST
                            ELSE
                                IF SELF:Expect(c'.')
                                    SELF:Consume()// eat the closing dot
                                    t := XTokenType.ELLIPSIS
                                ENDIF
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

                CASE XTokenType.DOLLAR
                    IF SELF:La() == c'.' .OR. (SELF:La() >= c'0' .AND. SELF:La() <= c'9')
                        t := XTokenType.NUMERIC_CONST
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:Expect(c'_')
                            NOP
                        ENDDO
                        IF SELF:Expect(c'.')
                            DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:Expect(c'_')
                                NOP
                            ENDDO
                        ENDIF
                        IF SELF:Lb() == c'_'
                            t := XTokenType.INVALID_NUMBER
                        ENDIF
                        strValue := SELF:_Source:Substring(start, SELF:_index - start):Replace("_", "")
                    ENDIF
                 CASE XTokenType.LBRKT
                    IF SELF:AllowSingleQuotedStrings .AND. ;
                            SELF:_lastToken != XTokenType.ID .AND. ;
                            SELF:_lastToken != XTokenType.RPAREN .AND. ;
                            SELF:_lastToken != XTokenType.RCURLY .AND. ;
                            SELF:_lastToken != XTokenType.RBRKT
                        t := XTokenType.STRING_CONST
                        DO WHILE !SELF:Reach(c']')
                            NOP
                        END
                        IF !SELF:Expect(c']')
                            t := XTokenType.INVALID_STRING_CONST
                        ENDIF
                        strValue := SELF:_Source:Substring(start, SELF:_index - start)
                    ENDIF
                CASE XTokenType.POUND
                    IF SELF:ExpectIdStart()
                        t := XTokenType.SYMBOL_CONST
                        DO WHILE SELF:ExpectIdChar()
                            NOP
                        ENDDO
                        strValue := SELF:_Source:Substring(start, SELF:_index - start)
                    ENDIF
                CASE XTokenType.SEMI
                    // Detect End of line or comments after SEMI. In that case it is a statement continuation character
                    DO WHILE SELF:ExpectAny(c' ', c'\t')
                        NOP
                    ENDDO
                    IF SELF:Expect(c'/', c'/')
                        // ; Followed by //
                        t  := XTokenType.LINE_CONT
                        ch := XChannel.Hidden
                        SELF:ConsumeToEol()
                    ELSE
                        IF SELF:AllowOldStyleComments .AND. SELF:Expect(c'&', c'&')
                            // ; Followed by &&
                            t := XTokenType.LINE_CONT
                            ch := XChannel.Hidden
                            SELF:ConsumeToEol()
                        ENDIF
                    ENDIF
                    IF SELF:ExpectEol()
                        IF t == XTokenType.SEMI
                            t := XTokenType.LINE_CONT
                        ENDIF
                        ch := XChannel.Hidden
                    ENDIF
                    IF (t == XTokenType.SEMI) .AND. SELF:_index > start + 1
                        // Statement delimiter character
                        SELF:Rewind(start + 1)
                    ENDIF
                CASE XTokenType.NUMERIC_CONST
                    IF c == c'0' .AND. SELF:ExpectAny(c'X', c'x')
                        // Hex Notation
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:ExpectRange(c'A', c'F') .OR. SELF:ExpectRange(c'a', c'f') .OR. SELF:Expect(c'_')
                            NOP
                        ENDDO
                        IF SELF:Lb() == c'_'
                            t := XTokenType.INVALID_NUMBER
                        ENDIF
                        SELF:ExpectAny(c'U', c'u', c'L', c'l')
                    ELSEIF c == c'0' .AND. SELF:ExpectAny(c'B', c'b')
                        DO WHILE SELF:ExpectRange(c'0', c'1')
                            NOP
                        ENDDO
                        SELF:ExpectAny(c'U', c'u')
                    ELSEIF c == c'0' .AND. SELF:ExpectAny(c'H', c'h')
                        t := XTokenType.BINARY_CONST
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:ExpectRange(c'A', c'F') .OR. SELF:ExpectRange(c'a', c'f') .OR. SELF:Expect(c'_')
                            NOP
                        ENDDO
                        IF SELF:Lb() == c'_'
                            t := XTokenType.INVALID_NUMBER
                        ENDIF
                    ELSE
                        DO WHILE SELF:ExpectRange(c'0', c'9') .OR. SELF:ExpectAny(c'_',c'.',c'e',c'E')
                            NOP
                        ENDDO
                        IF SELF:Lb() == c'_'
                            t := XTokenType.INVALID_NUMBER
                        ENDIF
                        SELF:ExpectAny(c'U', c'u', c'L', c'l')
                        SELF:ExpectAny(c'S', c's', c'D', c'd')
                    ENDIF
                    strValue:= SELF:_Source:Substring(start, SELF:_index - start):Replace(e"_", "")
                END SWITCH
                IF ch == XChannel.Default
                    IF t == XTokenType.ID
                        st := LookupKeyword(strValue)
                        IF st != XTokenType.UNRECOGNIZED
                            t := XTokenType.KEYWORD
                        ELSE
                            st := XTokenType.ID
                        ENDIF
                    ENDIF
                    SELF:_lastToken := t
                    RETURN XToken{SELF, t, start, SELF:_index - start, strValue, ch}
                ENDIF
            UNTIL SELF:Eoi()

        ENDIF
        IF !SELF:_hasEos
            SELF:_hasEos := TRUE
            RETURN XToken{SELF, XTokenType.EOS, SELF:_index, SELF:_index, NULL, XChannel.Default}
        ENDIF
        RETURN NULL
    OVERRIDE METHOD LookupKeyword(strValue AS STRING) AS XTokenType
        RETURN XTokenType.UNRECOGNIZED
END CLASS
END NAMESPACE
