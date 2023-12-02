// XLexer.prg


USING System.Collections.Generic
USING System.Globalization


BEGIN NAMESPACE XSharp.Parsers
ABSTRACT CLASS XLexer
    PROTECTED _Source AS STRING

    PROTECTED _index := 0 AS LONG
    PROTECTED _lastToken := XTokenType.NL AS XTokenType
    PROTECTED _hasEos := TRUE AS LOGIC
    PROTECTED AllowOldStyleComments AS LOGIC
    PROTECTED AllowSingleQuotedStrings AS LOGIC
    CONSTRUCTOR(source AS STRING )
        SELF:_Source := source
        AllowOldStyleComments := TRUE
        AllowSingleQuotedStrings := TRUE


    PROTECTED METHOD Lb() AS CHAR
        RETURN IIF((SELF:_index > 0) , SELF:_Source[SELF:_index - 1] , c'\0')


    PROTECTED METHOD La() AS CHAR
        RETURN IIF((SELF:_index < SELF:_Source:Length) , SELF:_Source[SELF:_index] , c'\0')


    PROTECTED METHOD La(n AS LONG ) AS CHAR
        RETURN IIF((SELF:_index + n - 1 < SELF:_Source:Length) , SELF:_Source[SELF:_index + n - 1] , c'\0')


    PROTECTED METHOD InRange(c AS CHAR , first AS CHAR , last AS CHAR ) AS LOGIC
        RETURN c >= first .AND. c <= last


    PROTECTED METHOD Eoi() AS LOGIC
        RETURN SELF:_index >= SELF:_Source:Length


    PROTECTED METHOD Consume() AS VOID
        SELF:_index++


    PROTECTED METHOD Consume(n AS LONG ) AS VOID
        SELF:_index += n


    PROTECTED METHOD Rewind(pos AS LONG ) AS VOID
        SELF:_index := pos


    PROTECTED METHOD Expect(c AS CHAR ) AS LOGIC
        IF SELF:La() == c
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD Expect(c1 AS CHAR , c2 AS CHAR ) AS LOGIC
        IF SELF:La() == c1 .AND. SELF:La(2) == c2
            SELF:Consume(2)
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD ExpectEol() AS LOGIC
        LOCAL r AS LOGIC
        r := FALSE
        IF SELF:La() == c'\r'
            SELF:Consume()
            r := TRUE
        ENDIF
        IF SELF:La() == c'\n'
            SELF:Consume()
            r := TRUE
        ENDIF
        RETURN r


    PROTECTED METHOD ExpectAny(c1 AS CHAR , c2 AS CHAR ) AS LOGIC
        LOCAL c3 AS CHAR
        c3 := SELF:La()
        IF c3 == c1 .OR. c3 == c2
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE

    PROTECTED METHOD ExpectAny(c1 AS CHAR , c2 AS CHAR , c3 AS CHAR) AS LOGIC
        LOCAL c4 AS CHAR
        c4 := SELF:La()
        IF c4 == c1 .OR. c4 == c2 .OR. c4 == c3
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE

    PROTECTED METHOD ExpectAny(c1 AS CHAR , c2 AS CHAR , c3 AS CHAR , c4 AS CHAR ) AS LOGIC
        LOCAL c5 AS CHAR
        c5 := SELF:La()
        IF c5 == c1 .OR. c5 == c2 .OR. c5 == c3 .OR. c5 == c4
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD ExpectRange(c1 AS CHAR , c2 AS CHAR ) AS LOGIC
        IF SELF:InRange(SELF:La(), c1, c2)
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE

     // copied from the Roslyn C# lexer

    PROTECTED STATIC METHOD IsLetterChar(cat AS UnicodeCategory ) AS LOGIC
        // letter-character:
        //   A Unicode character of classes Lu, Ll, Lt, Lm, Lo, or Nl
        //   A Unicode-escape-sequence representing a character of classes Lu, Ll, Lt, Lm, Lo, or Nl

       switch cat
            case UnicodeCategory.UppercaseLetter
            case UnicodeCategory.LowercaseLetter
            case UnicodeCategory.TitlecaseLetter
            case UnicodeCategory.ModifierLetter
            case UnicodeCategory.OtherLetter
            case UnicodeCategory.LetterNumber
                return true
		end switch
        RETURN FALSE

    // copied from the Roslyn C# lexer

    PUBLIC STATIC METHOD IsIdentifierPartCharacter(ch AS CHAR ) AS LOGIC
            // identifier-part-character:
            //   letter-character
            //   decimal-digit-character
            //   connecting-character
            //   combining-character
            //   formatting-character

        //
        IF ch < c'a'    // '\u0061'
            IF ch < c'A' // '\u0041'
                RETURN ch >= c'0' ;  // '\u0030'
                    .AND. ch <= c'9'		// '\u0039'
            ENDIF
            RETURN ch <= c'Z' ; // '\u005A'
                .OR. ch == c'_'  // '\u005F'
        ENDIF
        IF ch <= c'z'			// '\u007A'
            RETURN TRUE
        ENDIF
        IF ch <= c'\u007f'	// max ASCII
            RETURN FALSE
        ENDIF
        VAR cat := CharUnicodeInfo.GetUnicodeCategory(ch)
        RETURN IsLetterChar(cat) ;
            .OR. cat == UnicodeCategory.DecimalDigitNumber ;
            .OR. cat == UnicodeCategory.ConnectorPunctuation ;
            .OR. cat == UnicodeCategory.NonSpacingMark;
            .OR. cat == UnicodeCategory.SpacingCombiningMark;
            .OR. (ch > c'\u007f' .AND. cat == UnicodeCategory.Format)

    // copied from the Roslyn C# lexer
    PUBLIC STATIC METHOD IsIdentifierStartCharacter(ch AS CHAR ) AS LOGIC
        // identifier-start-character:
        //   letter-character
        //   _ (the underscore character U+005F)
        IF ch < c'a'    // '\u0061'
            IF ch < c'A' // '\u0041'
                RETURN FALSE
            ENDIF
            RETURN ch <= c'Z' ; // '\u005A'
                .OR. ch == c'_'  // '\u005F'
        ENDIF
        IF ch <= c'z'			// '\u007A'
            RETURN TRUE
        ENDIF
        IF ch <= c'\u007f'	// max ASCII
            RETURN FALSE
        ENDIF
        RETURN IsLetterChar(CharUnicodeInfo.GetUnicodeCategory(ch))


    PROTECTED METHOD ExpectIdStart() AS LOGIC
        LOCAL c AS CHAR
        c := SELF:La()
        IF IsIdentifierStartCharacter(c)
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD ExpectIdChar() AS LOGIC
        LOCAL c AS CHAR
        c := SELF:La()
        IF IsIdentifierPartCharacter(c)
            SELF:Consume()
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD ExpectLower(s AS STRING ) AS LOGIC
        IF Char.ToLower(SELF:La()) == s[0]
            FOR VAR i := 1 TO s:Length
                IF Char.ToLower(SELF:La(i)) != s[i - 1]
                    RETURN FALSE
                ENDIF
            NEXT
            SELF:Consume(s:Length)
            RETURN TRUE
        ENDIF
        RETURN FALSE


    PROTECTED METHOD Reach(c AS CHAR ) AS LOGIC
        IF !SELF:Eoi() .AND. SELF:La() != c
            SELF:Consume()
            RETURN FALSE
        ENDIF
        RETURN TRUE


    PROTECTED METHOD ReachEsc(c AS CHAR ) AS LOGIC
        LOCAL esc AS LOGIC
        IF !SELF:Eoi() .AND. SELF:La() != c
            esc := SELF:La() == c'\\'
            SELF:Consume()
            IF !SELF:Eoi() .AND. esc
                SELF:Consume()
            ENDIF
            RETURN FALSE
        ENDIF
        RETURN TRUE

    PROTECTED METHOD ConsumeToEol() AS VOID
        DO WHILE !SELF:ReachEol()
            NOP
        ENDDO
        RETURN
    PROTECTED METHOD ReachEol() AS LOGIC
        LOCAL c AS CHAR
        IF !SELF:Eoi()
            c := SELF:La()
            IF c != c'\r'  .AND. c != c'\n'
                SELF:Consume()
                RETURN FALSE
            ENDIF
        ENDIF
        RETURN TRUE


    PROTECTED METHOD Reach(c1 AS CHAR , c2 AS CHAR ) AS LOGIC
        IF !SELF:Eoi()
            IF SELF:La() != c1 .OR. SELF:La(2) != c2
                SELF:Consume()
                RETURN FALSE
            ENDIF
        ENDIF
        RETURN TRUE


    METHOD Reset() AS VOID
        SELF:_lastToken := XTokenType.NL
        SELF:_index := 0


    METHOD AllTokens() AS IList<XToken>
        LOCAL t AS XToken
        VAR i := List<XToken>{}
        DO WHILE (t := SELF:NextToken()) != NULL
            i:Add(t)
        ENDDO
        RETURN i


    METHOD GetText(t AS XToken ) AS STRING
        RETURN SELF:_Source:Substring(t:Start, t:Length)

    ABSTRACT METHOD NextToken AS XToken
    ABSTRACT METHOD LookupKeyword(strKeyword AS STRING) AS XTokenType

END CLASS
END NAMESPACE


