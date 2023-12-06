// Token.prg
// Created by    : robert
// Creation Date : 1/6/2021 3:48:19 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text
BEGIN NAMESPACE XSharp.Parsers
CLASS XToken
    PRIVATE _lexer    as XLexer
    PUBLIC PROPERTY Leadingws AS STRING AUTO GET SET
    PUBLIC PROPERTY HasTrivia AS LOGIC GET !String.IsNullOrEmpty(Leadingws)
	PUBLIC PROPERTY Type	AS XTokenType AUTO GET SET
	PUBLIC PROPERTY Channel	AS XChannel AUTO GET SET
	PUBLIC PROPERTY Start	AS LONG AUTO GET SET
	PUBLIC PROPERTY Length	AS LONG AUTO GET SET
	PUBLIC PROPERTY StrValue  AS STRING AUTO GET SET

	CONSTRUCTOR(lexer as XLexer, type AS XTokenType, start AS LONG , length AS LONG , strvalue AS STRING , channel AS XChannel )
		SELF:Type	 := type
		SELF:Start	 := start
		SELF:Length	 := length
		SELF:Channel := channel
        SELF:StrValue:= strvalue
        SELF:_lexer  := lexer


	PUBLIC OVERRIDE METHOD ToString() AS STRING
		RETURN IIF((!String.IsNullOrEmpty(SELF:StrValue)) , SELF:StrValue , SELF:Type:ToString() )

    PUBLIC PROPERTY Text as STRING
        GET
            IF _lexer == null
                return ""
            endif
            Return _lexer:GetText(SELF)
        END GET
    END PROPERTY


END CLASS

ENUM XChannel
	MEMBER Default := 1
	MEMBER Hidden  := 2

END ENUM
END NAMESPACE
