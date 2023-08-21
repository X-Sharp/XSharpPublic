//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Collections.Generic
USING System.Collections
USING System.Text
USING System.Text.RegularExpressions
USING System.IO
USING System.Diagnostics
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING XSharp.Parser
USING LanguageService.CodeAnalysis.Text


BEGIN NAMESPACE XSharpModel
    /// <summary>
    /// The XSharpTokenList class. This class is used to process a list of tokens
    /// without having to worry about the end of the list. You can safely access next items
    /// through the La.. and Lt.. properties and methods.
    /// </summary>
    CLASS XSharpTokenList
        PRIVATE _list  AS IList<IToken>
        PRIVATE _lastReadToken AS IToken
        PRIVATE _count  AS INT
        PRIVATE _index  AS INT
        PROPERTY La1 AS INT => SELF:La(1)
        PROPERTY La2 AS INT => SELF:La(2)
        PROPERTY La3 AS INT => SELF:La(3)
        PROPERTY Lt1 AS IToken => SELF:Lt(1)
        PROPERTY Lt2 AS IToken => SELF:Lt(2)
        PROPERTY Lt3 AS IToken => SELF:Lt(3)

        PROPERTY LastReadToken as IToken => _lastReadToken
        PROPERTY FirstOrDefault as IToken => _list:FirstOrDefault()
        PROPERTY LastOrDefault  as IToken => _list:LastOrDefault()

    CONSTRUCTOR(list as IList<IToken>)
        _list  := list
        _count := list:Count
        _index := 0
        _lastReadToken     := list:FirstOrDefault()
         RETURN

    METHOD Reset as Void
        _index := 0

    METHOD La(nToken AS LONG) AS LONG
         nToken += _index-1
         IF (nToken < _count)
            VAR t := _list[nToken]
            RETURN t:Type
         ENDIF
         RETURN XSharpLexer.Eof

     METHOD Lt(nToken AS LONG) AS IToken
         nToken += _index-1
         IF (nToken < _list:Count)
            RETURN _list[nToken]
         ENDIF
         RETURN NULL

      PUBLIC METHOD Eoi() AS LOGIC
         RETURN _index >= _list:Count

      PUBLIC METHOD Eos() AS LOGIC
         RETURN SELF:La1 == XSharpLexer.EOS .OR. SELF:Eoi()

      PUBLIC METHOD PushBack() AS VOID
         _index-= 1
         RETURN

      PUBLIC METHOD Consume() AS VOID
         SELF:SaveLastToken()
         _index+= 1
         RETURN


      METHOD SaveLastToken() AS VOID
         IF ! SELF:Eoi() .AND. SELF:La1 != XSharpLexer.EOS
            _lastReadToken := _list[_index]
         ENDIF

      METHOD ConsumeAndGet() AS IToken
         if ! SELF:Eoi()
            VAR t := _list[_index]
            _lastReadToken := t
            _index+= 1
            RETURN t
         endif
         return NULL

      METHOD ConsumeAndGetText() AS STRING
        if ! SELF:Eoi()
           VAR t := _list[_index]
           _lastReadToken := t
           _index+= 1
           var result := t:Text
           if result:StartsWith("@@")
                result := result:Substring(2)
           endif
           return result
        endif
        RETURN ""

      METHOD ConsumeUntilEndToken(nType as LONG) AS VOID
            SELF:ConsumeUntilEndToken(nType, OUT VAR _)
      METHOD ConsumeUntilEndToken(nType as LONG, endToken OUT IToken) AS LOGIC
          LOCAL nested := 0 as LONG
          endToken := NULL
          DO WHILE ! SELF:Eoi()
                var nextType := self:La1
                switch nextType
                CASE XSharpLexer.LPAREN
                CASE XSharpLexer.LCURLY
                CASE XSharpLexer.LBRKT
                    nested += 1
                CASE XSharpLexer.RPAREN
                CASE XSharpLexer.RCURLY
                CASE XSharpLexer.RBRKT
                    nested -= 1
                END SWITCH
                var t := SELF:ConsumeAndGet()
                if nextType == nType .and. nested <= 0
                    endToken := t
                    RETURN TRUE
                ENDIF
         ENDDO
          RETURN FALSE

      METHOD ConsumeAndGetAny(nTypes PARAMS LONG[]) AS IToken
         IF nTypes:Contains(SELF:La1)
            RETURN SELF:ConsumeAndGet()
         ENDIF
         RETURN NULL
      METHOD Matches(nTypes PARAMS LONG[]) AS LOGIC
         IF nTypes:Contains(SELF:La1)
            RETURN TRUE
         ENDIF
         RETURN FALSE

      METHOD Expect(nType AS LONG) AS LOGIC
         IF SELF:La1 == nType
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE

      METHOD ExpectAny(nTypes PARAMS LONG[]) AS LOGIC
         IF nTypes:Contains(SELF:La1)
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE

      METHOD ExpectAndGet(nType AS LONG, t OUT IToken) AS LOGIC
         IF SELF:La1 == nType
            t := SELF:ConsumeAndGet()
            RETURN TRUE
         ENDIF
         t := NULL
         RETURN FALSE
      METHOD ReadLine() AS VOID
         DO WHILE ! SELF:Eos()
            SELF:Consume()
         ENDDO
         DO WHILE SELF:La1 == XSharpLexer.EOS
            SELF:Consume() // Consume the EOS
         ENDDO
         RETURN
      METHOD ExpectOnThisLine(nType as LONG) AS LOGIC
         local nIndex := 1 AS LONG
         do while nIndex < _count
           local nToken := SELF:La(nIndex) AS LONG
           switch nToken
            case XSharpLexer.EOS
            case XSharpLexer.EOF
                return false
            otherwise
                if (nToken == nType)
                    return true
                endif
            end switch
            nIndex +=1
         enddo
         return false



      METHOD Contains(nType as LONG) AS LOGIC
           var counter := _index
           do while counter < _count
                if _list[counter]:Type == nType
                    return true
                endif
                counter += 1
            enddo
            return false
    END CLASS
END NAMESPACE // XSharpModel.Parser
