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
    CLASS XBlock
        PROPERTY Token1   AS IToken AUTO GET PRIVATE SET
        PROPERTY Token2   AS IToken AUTO GET PRIVATE SET
        PROPERTY Children AS IList<XBlock> AUTO GET PRIVATE SET
        PROPERTY Text     AS STRING GET IIF (Token1 != Token2, Token1:Text+" "+Token2:Text, Token1:Text)
        PROPERTY Type     AS INT    GET Token1:Type
        PROPERTY Type2    AS INT    GET Token2:Type
        PROPERTY Last     AS XBlock GET IIF(Closed, SELF:Children:Last(), SELF)
        PROPERTY Closed   AS LOGIC  GET Children:Count > 0
        
        CONSTRUCTOR(token1 AS IToken,token2 AS IToken)
            SELF:Token1   := token1
            SELF:Token2   := token2
            SELF:Children := List<XBlock>{}
            
        PROPERTY Valid    AS LOGIC
            GET
                IF SELF:Closed
                    VAR last := SELF:Last
                    SWITCH SELF:Type
                    CASE XSharpLexer.FOR
                    CASE XSharpLexer.FOREACH
                        RETURN last:Type == XSharpLexer.NEXT .OR. (last:Type == XSharpLexer.END .AND. last:Type2 == XSharpLexer.FOR)
                    
                    CASE XSharpLexer.IF
                        RETURN last:Type == XSharpLexer.ENDIF .OR. last:Type == XSharpLexer.END
                    
                    CASE XSharpLexer.DO
                        SWITCH SELF:Token2:Type
                            CASE XSharpLexer.WHILE
                                RETURN last:Type == XSharpLexer.ENDDO .OR. last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type .OR. Last:Type2 == XSharpLexer.EOS)
                            CASE XSharpLexer.CASE
                                RETURN last:Type == XSharpLexer.ENDCASE .OR. (last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type2 .OR. Last:Type2 == XSharpLexer.EOS))
                            CASE XSharpLexer.SWITCH
                                RETURN last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type2 .OR. Last:Type2 == XSharpLexer.EOS)
                         END SWITCH
                    CASE XSharpLexer.SWITCH
                        RETURN last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type .OR. Last:Type2 == XSharpLexer.EOS)
                    CASE XSharpLexer.TRY
                        RETURN last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type .OR. Last:Type2 == XSharpLexer.EOS)
                    CASE XSharpLexer.BEGIN
                        RETURN last:Type == XSharpLexer.END .AND. (SELF:Type2 == Last:Type2 .OR. Last:Type2 == XSharpLexer.EOS)
                    CASE XSharpLexer.REPEAT
                        RETURN last:Type == XSharpLexer.UNTIL
                    CASE XSharpLexer.WITH
                        RETURN last:Type == XSharpLexer.END .AND. (Last:Type2 == SELF:Type .OR. Last:Type2 == XSharpLexer.EOS)
                    CASE XSharpLexer.TEXT
                        RETURN last:Type == XSharpLexer.ENDTEXT
                    CASE XSharpLexer.PP_REGION
                        RETURN last:Type == XSharpLexer.PP_ENDREGION
                    CASE XSharpLexer.PP_IFDEF
                    CASE XSharpLexer.PP_IFNDEF
                        RETURN last:Type == XSharpLexer.PP_ENDIF
                    END SWITCH
                ENDIF
                RETURN FALSE
            END GET
        END PROPERTY

        METHOD DebuggerDisplay() AS STRING
            LOCAL res AS STRING
            res := SELF:Text
            IF SELF:Closed
                FOREACH VAR child IN SELF:Children
                    res += " "+child:Text
                NEXT
                res += " ("+Token1:Line:ToString() 
                res += "-"+Last:Token1:Line:ToString()+")"
            ELSE
                res += " ("+Token1:Line:ToString() +")"
            ENDIF
            RETURN res
                
    END CLASS
END NAMESPACE
