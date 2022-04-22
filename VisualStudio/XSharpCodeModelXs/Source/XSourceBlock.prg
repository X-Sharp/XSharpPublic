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
    CLASS XSourceBlock
        PROPERTY XKeyword AS XKeyword AUTO GET PRIVATE SET
        PROPERTY Token    AS XSharpToken  AUTO GET PRIVATE SET
        PROPERTY Children AS IList<XSourceBlock> AUTO GET PRIVATE SET
        PROPERTY Text     AS STRING GET Token:ToString()
        PROPERTY Last     AS XSourceBlock GET IIF(Closed, SELF:Children:Last(), SELF)
        PROPERTY Closed   AS LOGIC  GET Children?:Count > 0

        CONSTRUCTOR(xt as XKeyword, token as XSharpToken )
            SELF:XKeyword   := xt
            SELF:Token    := token
            SELF:Children := List<XSourceBlock>{}

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
                    res += " "+child:Text
                NEXT
                res += " ("+Token:Line:ToString()
                res += "-"+Last:Token:Line:ToString()+")"
            ELSE
                res += " ("+Token:Line:ToString() +")"
            ENDIF
            RETURN res

    END CLASS
END NAMESPACE
