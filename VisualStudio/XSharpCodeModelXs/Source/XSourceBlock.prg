//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XBlockChild
        PROPERTY Token    AS IToken   AUTO GET PRIVATE SET
        PROPERTY Keyword AS XKeyword AUTO GET PRIVATE SET
        CONSTRUCTOR (keyword AS XKeyword,token AS IToken)
            SELF:Token := token
            SELF:Keyword := keyword
        METHOD DebuggerDisplay() AS STRING
            RETURN SELF:Token:Text+" ("+Token:Line:ToString()+")"
    END CLASS


    [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XSourceBlock
        PROPERTY XKeyword AS XKeyword AUTO GET PRIVATE SET
        PROPERTY Token    AS IToken  AUTO GET PRIVATE SET
        PROPERTY Children AS IList<XBlockChild> AUTO GET PRIVATE SET
        PROPERTY Text     AS STRING GET Token:ToString()
        PROPERTY Last     AS XBlockChild GET IIF(Closed, SELF:Children:Last(), XBlockChild{XKeyword, Token})
        PROPERTY Closed   AS LOGIC GET Children?:Count > 0
        PROPERTY CanJump  AS LOGIC AUTO GET PRIVATE SET

        CONSTRUCTOR(xt as XKeyword, token as IToken )
            SELF:XKeyword   := xt
            SELF:Token    := token
            SELF:Children := List<XBlockChild>{}
            SELF:CanJump  := XFormattingRule.IsJumpTarget(xt)

        PROPERTY Valid    AS LOGIC
            GET
                RETURN SELF:Closed
            END GET
        END PROPERTY

        METHOD DebuggerDisplay() AS STRING
            LOCAL res AS STRING
            res := SELF:Text
            IF SELF:Closed
                FOREACH VAR child IN SELF:Children
                    res += " "+child:Token:Text
                NEXT
                res += " ("+Token:Line:ToString()
                res += "-"+Last:Token:Line:ToString()+")"
            ELSE
                res += " ("+Token:Line:ToString() +")"
            ENDIF
            RETURN res

    END CLASS
END NAMESPACE
