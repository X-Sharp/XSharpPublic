// TokenType.prg
// Created by    : robert
// Creation Date : 1/6/2021 3:44:29 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.Parsers
ENUM XTokenType
    MEMBER UNRECOGNIZED := -1
#region Operators
    MEMBER FIRST_OPERATOR := 0
    MEMBER LT := 1
    MEMBER LTE
    MEMBER GT
    MEMBER GTE
    MEMBER EQ
    MEMBER EEQ
    MEMBER DOLLAR
    MEMBER NEQ
    MEMBER POUND
    MEMBER BACKTICK
    MEMBER NULLSAFE_EQ
    MEMBER INC
    MEMBER DEC
    MEMBER PLUS
    MEMBER MINUS
    MEMBER DIV
    MEMBER MOD
    MEMBER EXP
    MEMBER LSHIFT
    MEMBER RSHIFT
    MEMBER TILDE
    MEMBER MULT
    MEMBER QQMARK
    MEMBER QMARK
    MEMBER EXCLAMATIONMARK
    MEMBER ASSIGN_OP
    MEMBER ASSIGN_ADD
    MEMBER ASSIGN_SUB
    MEMBER ASSIGN_EXP
    MEMBER ASSIGN_MUL
    MEMBER ASSIGN_DIV
    MEMBER ASSIGN_MOD
    MEMBER ASSIGN_BITAND
    MEMBER ASSIGN_BITOR
    MEMBER ASSIGN_LSHIFT
    MEMBER ASSIGN_RSHIFT
    MEMBER ASSIGN_XOR
    MEMBER COMMA
    MEMBER AMP
    MEMBER ADDROF
    MEMBER BACKSLASH
    MEMBER ELLIPSIS
    MEMBER UDCSEP
    MEMBER SQUOTE
    MEMBER DQUOTE
    MEMBER LAST_OPERATOR
#endregion
#region Pairs
    MEMBER FIRST_PAIR
    MEMBER LPAREN
    MEMBER RPAREN
    MEMBER LCURLY
    MEMBER RCURLY
    MEMBER LBRKT
    MEMBER RBRKT
    MEMBER LAST_PAIR
#endregion
#region Separators and special characters
    MEMBER FIRST_SEPARATOR
    MEMBER COLON
    MEMBER DOT
    MEMBER PIPE
    MEMBER ALIAS
    MEMBER COLONCOLON
    MEMBER PIPEPIPE
    MEMBER AMPAMP
    MEMBER ATAT
    MEMBER LAST_SEPARATOR
#endregion
#region Logical
    MEMBER LOGIC_OR
    MEMBER LOGIC_AND
    MEMBER LOGIC_NOT
    MEMBER LOGIC_XOR
#endregion
#region Constants
    MEMBER FIRST_CONSTANT
    MEMBER FALSE_CONST
    MEMBER TRUE_CONST
    MEMBER NUMERIC_CONST
    MEMBER DATE_CONST
    MEMBER DATETIME_CONST
    MEMBER SYMBOL_CONST
    MEMBER CHAR_CONST
    MEMBER STRING_CONST
    MEMBER INVALID_STRING_CONST
    MEMBER INVALID_NUMBER
    MEMBER BINARY_CONST
    MEMBER LAST_CONSTANT					// End of Constants
#endregion
#region Identifiers
    MEMBER ID
    MEMBER INCOMPLETE_ID                    // SQL ID that starts with DQuote but has no end
    MEMBER KEYWORD
#endregion
#region Comments
    MEMBER DOC_COMMENT
    MEMBER SL_COMMENT
    MEMBER ML_COMMENT
#endregion
#region Special
    MEMBER LINE_CONT					// Only for languages that have line continuation.
    MEMBER SEMI
    MEMBER WS
    MEMBER NL							// \r and \n
    MEMBER EOS							// End of Statement. Could be semi colon or CRLF for XBase
    MEMBER EOF
    MEMBER LAST := EOF+1
#endregion

#region SQL Keywords          // Only relevant keywords that delimit groups
    MEMBER FIRST_KEYWORD
    MEMBER @@AS
    MEMBER ALTER
    MEMBER AUTOINC
    MEMBER CHECK
	MEMBER CODEPAGE
    MEMBER COLLATE
    MEMBER CREATE
    MEMBER CURSOR
    MEMBER DEFAULT
    MEMBER DELETE
    MEMBER DROP
	MEMBER ERROR
    MEMBER FOREIGN
    MEMBER FROM
    MEMBER HAVING
    MEMBER INDEX
    MEMBER INSERT
    MEMBER INTO
    MEMBER IS
    MEMBER JOIN
    MEMBER KEY
	MEMBER NOCPTRANS
	MEMBER NOT
    MEMBER NULL
    MEMBER ORDER
    MEMBER PRIMARY
    MEMBER REFERENCES
    MEMBER SELECT
    MEMBER SET
    MEMBER TABLE
    MEMBER TO
    MEMBER TRIGGER
    MEMBER UNION
	MEMBER UNIQUE
    MEMBER UPDATE
    MEMBER VALUES
    MEMBER VIEW
    MEMBER WHERE
    MEMBER LAST_KEYWORD
#endregion

END ENUM


STATIC CLASS TokenAttr
    STATIC INITONLY specialTable AS XTokenType[]
    STATIC CONSTRUCTOR
        specialTable := XTokenType[]{128}
        FOR VAR i := 1 TO specialTable:Length
            specialTable[i] := XTokenType.UNRECOGNIZED
        NEXT
        specialTable[c'('] := XTokenType.LPAREN
        specialTable[c')'] := XTokenType.RPAREN
        specialTable[c'{'] := XTokenType.LCURLY
        specialTable[c'}'] := XTokenType.RCURLY
        specialTable[c'['] := XTokenType.LBRKT
        specialTable[c']'] := XTokenType.RBRKT
        specialTable[c':'] := XTokenType.COLON // :: :=
        specialTable[c','] := XTokenType.COMMA
        specialTable[c'\\']:= XTokenType.BACKSLASH
        specialTable[c'|'] := XTokenType.PIPE // || |=
        specialTable[c'&'] := XTokenType.AMP // && &=
        specialTable[c'@'] := XTokenType.ADDROF // @@id
        specialTable[c'-'] := XTokenType.MINUS // -> -- -=
        specialTable[c'+'] := XTokenType.PLUS // ++ +=
        specialTable[c'/'] := XTokenType.DIV // // /// /* /=
        specialTable[c'%'] := XTokenType.MOD // %=
        specialTable[c'^'] := XTokenType.EXP // ^=
        specialTable[c'<'] := XTokenType.LT // << <<= <= <>
        specialTable[c'>'] := XTokenType.GT // >> >>= >=
        specialTable[c'~'] := XTokenType.TILDE // ~= ~"..."
        specialTable[c'*'] := XTokenType.MULT // *comment *= ** **=
        specialTable[c'?'] := XTokenType.QMARK // ??
        specialTable[c'='] := XTokenType.EQ // == =>
        specialTable[c'$'] := XTokenType.DOLLAR
        specialTable[c'!'] := XTokenType.EXCLAMATIONMARK // !=
        specialTable[c';'] := XTokenType.SEMI // ;\n
        specialTable[c'.'] := XTokenType.DOT // .F. .T. .N. .Y. ... .OR. .AND. .NOT. .XOR.
        specialTable[c'\r'] := XTokenType.NL // \r\r\r...
        specialTable[c'\n'] := XTokenType.NL // \n\n\n...
        specialTable[c'\t'] := XTokenType.WS // ...
        specialTable[c' '] := XTokenType.WS // ...
        specialTable[c'\v'] := XTokenType.WS // ...
        specialTable[c'\f'] := XTokenType.WS // ...
        specialTable[c'#'] := XTokenType.POUND // #sym
        specialTable[c'\''] := XTokenType.SQUOTE
        specialTable[c'"'] := XTokenType.DQUOTE
        specialTable[c'_'] := XTokenType.ID
        specialTable[c'`'] := XTokenType.BACKTICK
        FOR VAR i := (INT) c'A' TO (INT) c'Z'
            specialTable[i] := XTokenType.ID
        NEXT
        FOR VAR i := (INT) c'a' TO (INT) c'z'
            specialTable[i] := XTokenType.ID
        NEXT
        FOR VAR i := (INT) c'0' TO (INT) c'9'
            specialTable[i] := XTokenType.NUMERIC_CONST
        NEXT
        RETURN
END CLASS
END NAMESPACE
