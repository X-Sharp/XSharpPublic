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


BEGIN NAMESPACE XSharp.Parsers 
    /// <summary>
    /// The XSharpTokenList class. This class is used to process a list of tokens
    /// without having to worry about the end of the list. You can safely access next items
    /// through the La.. and Lt.. properties and methods.
    /// </summary>
    CLASS XTokenList
        PRIVATE _list  AS IList<XToken>
        PRIVATE _lastReadToken AS XToken
        PRIVATE _count  AS INT
        PRIVATE _index  AS INT
        PROPERTY La1 AS XTokenType => SELF:La(1)
        PROPERTY La2 AS XTokenType => SELF:La(2)
        PROPERTY La3 AS XTokenType => SELF:La(3)
        PROPERTY Lt1 AS XToken => SELF:Lt(1)
        PROPERTY Lt2 AS XToken => SELF:Lt(2)
        PROPERTY Lt3 AS XToken => SELF:Lt(3)

        PROPERTY LastReadToken as XToken => _lastReadToken
        PROPERTY FirstOrDefault as XToken => _list:FirstOrDefault()
        PROPERTY LastOrDefault  as XToken => _list:LastOrDefault()

    CONSTRUCTOR(list as IList<XToken>)
        _list  := list
        _count := list:Count
        _index := 0
        _lastReadToken     := list:FirstOrDefault()
         RETURN

    METHOD Reset as Void
        _index := 0

    METHOD La(nToken AS LONG) AS XTokenType
         nToken += _index-1
         IF (nToken < _count)
            VAR t := _list[nToken]
            RETURN t:Type
         ENDIF
         RETURN XTokenType.EOF

     METHOD Lt(nToken AS LONG) AS XToken
         nToken += _index-1
         IF (nToken < _list:Count)
            RETURN _list[nToken]
         ENDIF
         RETURN NULL

      PUBLIC METHOD Eoi() AS LOGIC
         RETURN _index >= _list:Count

      PUBLIC METHOD Eos() AS LOGIC
         RETURN SELF:La1 == XTokenType.EOS .OR. SELF:Eoi()

      PUBLIC METHOD PushBack() AS VOID
         _index-= 1
         RETURN

      PUBLIC METHOD Consume() AS VOID
         SELF:SaveLastToken()
         _index+= 1
         RETURN


      METHOD SaveLastToken() AS VOID
         IF ! SELF:Eoi() .AND. SELF:La1 != XTokenType.EOS
            _lastReadToken := _list[_index]
         ENDIF

      METHOD ConsumeAndGet() AS XToken
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

      METHOD ConsumeUntilEndToken(nType as XTokenType) AS VOID
            SELF:ConsumeUntilEndToken(nType, OUT VAR _)
      METHOD ConsumeUntilEndToken(nType as XTokenType, endToken OUT XToken) AS LOGIC
          LOCAL nested := 0 as LONG
          endToken := NULL
          DO WHILE ! SELF:Eoi()
                var nextType := self:La1
                switch nextType
                CASE XTokenType.LPAREN
                CASE XTokenType.LCURLY
                CASE XTokenType.LBRKT
                    nested += 1
                CASE XTokenType.RPAREN
                CASE XTokenType.RCURLY
                CASE XTokenType.RBRKT
                    nested -= 1
                END SWITCH
                var t := SELF:ConsumeAndGet()
                if nextType == nType .and. nested <= 0
                    endToken := t
                    RETURN TRUE
                ENDIF
         ENDDO
          RETURN FALSE

      METHOD ConsumeAndGetAny(nTypes PARAMS XTokenType[]) AS XToken
         IF nTypes:Contains(SELF:La1)
            RETURN SELF:ConsumeAndGet()
         ENDIF
            RETURN NULL


      METHOD Matches(nType AS XTokenType) AS LOGIC
         RETURN SELF:La1 == nType

      METHOD Matches(nTypes PARAMS XTokenType[]) AS LOGIC
         IF nTypes:Contains(SELF:La1)
            RETURN TRUE
         ENDIF
         RETURN FALSE

     METHOD Matches(cText AS STRING) AS LOGIC
        IF String.Compare(SELF:Lt1:Text, cText, StringComparison.OrdinalIgnoreCase) == 0
           RETURN TRUE
        ENDIF
        RETURN FALSE

      METHOD Expect(nType AS XTokenType) AS LOGIC
         IF SELF:Matches(nType)
            SELF:Consume()
            RETURN TRUE
         ENDIF
            RETURN FALSE

      METHOD Expect(cText AS STRING) AS LOGIC
         IF Matches(cText)
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE


      METHOD ExpectAny(nTypes PARAMS XTokenType[]) AS LOGIC
         IF SELF:Matches(nTypes)
            SELF:Consume()
            RETURN TRUE
         ENDIF
         RETURN FALSE

      METHOD ExpectAndGet(nType AS XTokenType, t OUT XToken) AS LOGIC
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
         DO WHILE SELF:La1 == XTokenType.EOS
            SELF:Consume() // Consume the EOS
         ENDDO
         RETURN
      METHOD ExpectOnThisLine(nType as XTokenType) AS LOGIC
         local nIndex := 1 AS LONG
         do while nIndex < _count
           local nToken := SELF:La(nIndex) AS LONG
           switch nToken
            case XTokenType.EOS
            case XTokenType.EOF
                return false
            otherwise
                if (nToken == nType)
                    return true
                endif
            end switch
            nIndex +=1
         enddo
         return false



      METHOD Contains(nType as XTokenType) AS LOGIC
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
