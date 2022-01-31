//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Text


INTERNAL STATIC CLASS TransformHelpers
    [Flags] ;
    ENUM TransformPictures
        MEMBER None        := 0
        MEMBER Left        := 1
        MEMBER Credit      := 2
        MEMBER @@Date      := 4
        MEMBER British     := 8
        MEMBER NonTemplate := 16
        MEMBER Debit       := 32
        MEMBER ZeroBlank   := 64
        MEMBER ParenLeft   := 128
        MEMBER ParenRight  := 256
        MEMBER Upper       := 512
        MEMBER YesNo       := 1024
    END ENUM

    STATIC METHOD SplitPict(cSayPicture AS STRING, cPic OUT STRING, cFunc OUT STRING) AS LOGIC
        LOCAL iFuncLen  AS INT
        cPic := cFunc := ""
        IF cSayPicture:Length > 1 .AND. cSayPicture[0]  == c'@'
            iFuncLen := cSayPicture:IndexOf(" ") -1
            IF iFuncLen < 0
                // No space delimiter so we assume the whole length
                iFuncLen := cSayPicture:Length-1
                cFunc    := cSayPicture:Substring(1):ToUpper()
            ELSE
                cPic  := cSayPicture:Substring(iFuncLen+2):Trim()
                cFunc := cSayPicture:Substring(1, iFuncLen):ToUpper()
            ENDIF
        ELSE
            cPic := cSayPicture
        ENDIF
        RETURN cFunc:Contains("R")


    STATIC METHOD UnformatC(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS STRING PASCAL
        LOCAL lRInsert			:= FALSE AS LOGIC
        LOCAL cFunc 			:= ""	AS STRING
        LOCAL wValueLen 		:= 0	AS LONG
        LOCAL wPictureLen		:= 0	AS LONG
        LOCAL w 				:= 0	AS LONG
        LOCAL wValueIdx 		:= 0	AS LONG
        LOCAL cRet				:= ""	AS STRING
        LOCAL cPic				AS STRING
        LOCAL cString			AS STRING
        wValueLen := cValue:Length
        lRInsert	:= SplitPict(cSayPicture, OUT cPic, OUT cFunc)
        cSayPicture := cFunc // get rid of warning
        cSayPicture := cPic
        wPictureLen 	:= cSayPicture:Length
        IF lRInsert
            VAR sb := StringBuilder{}
            wValueIdx := 0
            FOR w := 0 TO wPictureLen-1
                wValueIdx += 1
                IF wValueIdx > wValueLen
                    EXIT
                ENDIF
                IF TransformHelpers.IsPictureLiteral(c'C', cSayPicture[ w])
                    LOOP
                ENDIF
                sb:Append(cValue[ wValueIdx-1])
            NEXT
            IF wValueIdx < wValueLen
                sb:Append(cValue:Substring( wValueIdx+1))
            ENDIF
            cString := sb:ToString()
        ELSE
            cString := cValue
        ENDIF
        IF Empty(cString) .AND. lNullable
            cRet := ""
        ELSE
            cRet := cString
        ENDIF

        RETURN cRet


    STATIC METHOD UnformatD(cValue AS STRING, cSayPicture AS STRING, lNullable   AS LOGIC)    AS DATE PASCAL
        LOCAL dRet				AS DATE

        SplitPict(cSayPicture, OUT VAR cTempValue, OUT VAR cFunc)
        cTempValue := AllTrim(cValue)

        LOCAL cFormat			AS STRING
        IF cFunc:Contains("E")
            cFormat := IIF ( SetCentury(), "dd/mm/yyyy","dd/mm/yy")
        ELSE
            cFormat := GetDateFormat()
        ENDIF
        dRet 	:= CToD(cTempValue, cFormat)
        IF Empty(dRet) .AND. lNullable
            dRet := NULL_DATE
        ENDIF

        RETURN dRet



    STATIC METHOD UnformatN(cValue AS STRING, cSayPicture AS STRING, lNullable   AS LOGIC)    AS USUAL PASCAL
        LOCAL lNegative 			:= FALSE AS LOGIC
        LOCAL lDecimalFound 		:= FALSE AS LOGIC
        LOCAL cFunc 				AS STRING
        LOCAL cChar 				AS CHAR
        LOCAL cNumString			:= NULL_STRING	AS STRING
        LOCAL cDecimal				:= c'\0' AS CHAR
        LOCAL cTempValue			:= "" AS STRING
        LOCAL cPic					:= "" AS STRING
        LOCAL wNegSignCnt			AS DWORD
        LOCAL wValueLen 			AS LONG
        LOCAL wPictureLen			AS LONG
        LOCAL wValueIdx 			AS LONG
        LOCAL wPictureIdx			AS LONG
        LOCAL wValDecPos			AS LONG
        LOCAL wPicDecPos			AS LONG
        LOCAL uRetVal				:= NIL AS USUAL
        LOCAL nDecSave				AS DWORD

        SplitPict(cSayPicture, OUT cPic, OUT cFunc)

        cTempValue := AllTrim(cValue)
        wValueLen  := cTempValue:Length

        IF cFunc:Contains("X") .AND. cTempValue:EndsWith("DB")
            lNegative	:= TRUE
            cTempValue	:= cTempValue:Substring(0, wValueLen-2):Trim()
            wValueLen	:= cTempValue:Length
        ELSEIF cFunc:Contains("C") .AND. cTempValue:EndsWith("CR")
            cTempValue := cTempValue:Substring(0, wValueLen-2):Trim()
            wValueLen := cTempValue:Length
        ENDIF
        IF (cFunc:Contains(")") .OR. cFunc:Contains("(")) ;
            .AND. cTempValue:StartsWith("(") .AND. cTempValue:EndsWith(")")
            lNegative := TRUE
            cTempValue := cTempValue:Substring(1, wValueLen-2)
            wValueLen := cTempValue:Length
        ENDIF
        IF cFunc:Contains("D")
            LOCAL aMDY[3,2]	AS INT[,]
            LOCAL wTemp1	AS INT
            LOCAL wTemp2	AS INT
            LOCAL cDateFormat := "" AS STRING
            LOCAL cDate 	  := "" AS STRING
            aMDY  := INT[,]{3,2}
            cDateFormat:=GetDateFormat()
            aMDY[1,1] := cDateFormat:IndexOf("MM")
            aMDY[1,2] :=2
            aMDY[2,1] := cDateFormat:IndexOf("DD")
            aMDY[2,2] :=2
            aMDY[3,1] := cDateFormat:IndexOf("YYYY")
            IF aMDY[3,1] == -1
                aMDY[3,1] := cDateFormat:IndexOf("YY")
                aMDY[3,2] :=2
            ELSE
                aMDY[3,2] :=4
            ENDIF
            LOCAL w AS DWORD
            FOR w:=1 UPTO 2
                LOCAL i AS DWORD
                FOR i:=1 UPTO 2
                    IF aMDY[i,1]>aMDY[i+1,1]
                        wTemp1:=aMDY[i+1,1]
                        wTemp2:=aMDY[i+1,2]
                        aMDY[i+1,1]:=aMDY[i,1]
                        aMDY[i+1,2]:=aMDY[i,2]
                        aMDY[i,1]:=wTemp1
                        aMDY[i,2]:=wTemp2
                    ENDIF
                NEXT
            NEXT
            FOR w:=1 UPTO 3
                cDate += cValue:Substring(aMDY[w,1],aMDY[w,2])
            NEXT
            uRetVal := Val(cDate)
            RETURN uRetVal
        ENDIF
        IF (wNegSignCnt := Occurs("-",cTempValue)) > 0
            IF wNegSignCnt > Occurs("-",cPic)
                lNegative := TRUE
            ENDIF
        ENDIF

        IF cPic==""
            uRetVal := Abs( Val(cTempValue) )
        ELSE
            cDecimal := (CHAR) SetDecimalSep()

            wPictureLen := cPic:Length
            wValDecPos  := cTempValue:IndexOf(cDecimal)+1
            IF wValDecPos == 0
                wValDecPos := wValueLen+1
            ENDIF

            wPicDecPos := cPic:IndexOf(".")+1

            IF wPicDecPos == 0
                wPicDecPos := wPictureLen+1
            ENDIF

            IF wValDecPos > wPicDecPos
                wValueIdx := wValDecPos-wPicDecPos
                wPictureIdx := 1
            ELSE
                wPictureIdx := wPicDecPos-wValDecPos+1
                wValueIdx := 0
            ENDIF
            FOR VAR w:=wPictureIdx TO wPictureLen
                wValueIdx += 1
                IF wValueIdx > wValueLen
                    EXIT
                ENDIF
                cChar := cTempValue[(INT) wValueIdx-1]
                IF cChar == cDecimal .AND. ! lDecimalFound
                    IF Empty(cNumString) .AND. lNullable
                        uRetVal := NIL
                    ELSE
                        uRetVal := Val(cNumString)
                    ENDIF
                    cNumString := ""
                    lDecimalFound := TRUE
                ENDIF
                IF !TransformHelpers.IsPictureLiteral(c'N', cPic[(INT) w-1])
                    IF Char.IsDigit(cChar)
                        cNumString += cChar:ToString()
                    ELSEIF cChar == c'-'
                        lNegative := TRUE
                    ENDIF
                ENDIF
            NEXT
            IF wValueIdx < wValueLen
                cNumString +=  cTempValue:Substring(wValueIdx, wValueLen-wValueIdx )
            ENDIF

            IF lDecimalFound
                VAR w := SLen(cNumString)
                IF w > 0
                    nDecSave	:= SetDecimal(w)
                    IF !(Empty(cNumString) .AND. lNullable)
                        uRetVal += Val(cNumString) / (10**w)
                    ENDIF
                    SetDecimal(nDecSave)
                ENDIF
            ELSE
                IF Empty(cNumString) .AND. lNullable
                    uRetVal := NIL
                ELSE
                    uRetVal := Val(cNumString)
                ENDIF
            ENDIF
        ENDIF
        IF lNegative
            uRetVal := -uRetVal
        ENDIF
        RETURN uRetVal


    STATIC METHOD UnformatL(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS LOGIC PASCAL
        LOCAL cTempValue    := "" AS STRING
        LOCAL cChar         := "" AS STRING
        LOCAL nValueLen     := 0  AS INT
        LOCAL nValIdx       := 0  AS INT
        LOCAL cPic          := "" AS STRING
        LOCAL cFunc         := "" AS STRING
        LOCAL lRInsert	    := FALSE AS LOGIC
        LOCAL nPictureLen	:= 0  AS INT
        LOCAL n, nTemp		:= 0  AS INT
        LOCAL lRet          := FALSE AS LOGIC

        cTempValue := AllTrim(cValue)
        nValueLen  := cTempValue:Length
        SplitPict(cSayPicture, OUT cPic, OUT cFunc)
        lRInsert   := cFunc:Contains("R")

        IF Empty(cTempValue) .AND. lNullable
            lRet := .F.
        ELSE
            LOCAL aTemp AS STRING[]
            aTemp    := STRING[]{5}
            IF Instr("L", cPic) .OR. Instr("L", cFunc)
                aTemp[1] := "TRUE"
                aTemp[2] := ".T."
                aTemp[3] := "T"
                aTemp[4] := "YES"
                aTemp[5] := "Y"
            ELSE
                aTemp[1] := __CavoStr(VOErrors.RT_MSG_LONG_TRUE)
                aTemp[2] := ".T."
                aTemp[3] := __CavoStr(VOErrors.RT_MSG_SHORT_TRUE)
                aTemp[4] := __CavoStr(VOErrors.RT_MSG_LONG_YES)
                aTemp[5] := __CavoStr(VOErrors.RT_MSG_SHORT_YES)
            ENDIF

            IF cPic == ""
                nTemp := Array.IndexOf(aTemp,Upper(cTempValue) )+1
                IF nTemp > 0
                    lRet := .T.
                ENDIF
            ELSE
                nPictureLen := cPic:Length
                IF lRInsert
                    nValIdx := 0
                    FOR n := 1 TO nPictureLen
                        nValIdx += 1
                        IF nValIdx > nValueLen
                            EXIT
                        ENDIF
                        IF Instr(cPic:Substring(n-1,1),"YL")
                            cChar := cValue:Substring(nValIdx-1,1)
                            nTemp := Array.IndexOf(aTemp, Upper(cChar) )+1
                            IF nTemp > 0
                                lRet := .T.
                            ENDIF
                            EXIT
                        ENDIF
                    NEXT
                ELSE
                    cChar := cValue:Substring(0,1)
                    nTemp := Array.IndexOf( aTemp, Upper(cChar) )+1
                    IF nTemp > 0
                        lRet := .T.
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

        RETURN lRet






    STATIC METHOD MergeValueAndTemplate(cType AS CHAR, cValue AS STRING, cTemplate AS STRING, nPictures AS TransformPictures) AS STRING
        LOCAL result := cTemplate:ToCharArray() AS CHAR[]
        LOCAL nSrc   := 0 AS INT
        LOCAL nTempl := 0 AS INT
        LOCAL nDest  := __ARRAYBASE__ AS INT
        LOCAL nSrcLen := cValue:Length AS INT
        LOCAL nDestLen:= cTemplate:Length AS INT
        LOCAL lBritish := nPictures:HasFlag(TransformPictures:British) AS LOGIC
        DO WHILE nSrc < nSrcLen .AND. nDest -__ARRAYBASE__ < nDestLen
            VAR templChar := cTemplate[nTempl++]
            VAR srcChar   := cValue[nSrc]
            IF IsPictureLiteral(cType, templChar)
                IF cType == c'N' .AND. templChar == c'.'
                    // Decimal separator ?
                    result[nDest++] := IIF(lBritish, c',', (CHAR) RuntimeState.DecimalSep)
                    nSrc++

                ELSEIF cType == c'N' .AND. templChar == c',' .AND. nDest > 0
                    // Thousand separator ?
                    LOCAL cLast := result[nDest-1] AS CHAR
                    IF Char.IsDigit(cLast)
                        result[nDest++] := IIF(lBritish, c'.', (CHAR) RuntimeState.ThousandSep)
                    ELSEIF cLast == c'-' .OR. cLast == c'+'
                        // overwrite the + or minus sign with a space and set the sign at the place of the comma
                        result[nDest-1] := c' '
                        result[nDest] := cLast
                        nDest++
                    ELSE
                        result[nDest++] := cLast
                    ENDIF
                ELSEIF cType == c'L' .AND. templChar == c'Y'
                    result[nDest++] := TransformHelpers.GetLogicLiteral(srcChar == c'T', cType == c'Y')
                    nSrc++
                ELSE
                    // Normal template literal. For numeric or @R pictures no change to the src pointer
                    // otherwise increase src pointer
                    result[nDest++] := templChar
                    IF nPictures:HasFlag(TransformPictures:NonTemplate) .OR. cType == c'N'
                        NOP
                    ELSE
                        nSrc++
                    ENDIF
                ENDIF

            ELSE
                // Non template char
                IF templChar == c'Y' .OR. templChar == c'y'
                    IF srcChar == c'T'
                        result[nDest++] := TransformHelpers.GetLogicLiteral(TRUE, TRUE)
                    ELSE
                        result[nDest++] := TransformHelpers.GetLogicLiteral(FALSE, TRUE)
                    ENDIF
                    nSrc++
                ELSEIF srcChar == c' ' .AND. (templChar == c'*' .OR. templChar == c'$')
                    result[nDest++] := templChar
                    nSrc++
                ELSE
                    IF lBritish .AND. cType == c'N' .AND. srcChar == c'.'
                        result[nDest++] := c','
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransformPictures:Upper) .OR. templChar == c'!'
                        result[nDest++] := Char.ToUpper(srcChar)
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransformPictures:ZeroBlank) .AND. cType == c'N' .AND. srcChar == c'0'
                        LOCAL lHasDig := FALSE AS LOGIC
                        FOR VAR x := __ARRAYBASE__ TO nDest
                            IF Char.IsDigit(result[x])
                                lHasDig := TRUE
                                EXIT
                            ENDIF
                        NEXT
                        IF lHasDig
                            result[nDest++] := srcChar
                        ELSE
                            result[nDest++] := c' '
                        ENDIF
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransformPictures:YesNo) .AND. cType == c'L'
                        result[nDest++] := TransformHelpers.GetLogicLiteral(srcChar == c'T', TRUE)
                        nSrc++
                    ELSE
                        result[nDest++] := srcChar
                        nSrc++
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
        // any remaining templ chars can be copied to the end of the string
//      IF nPictures:HasFlag(TransformPictures:NonTemplate) .OR. cType == c'N'
            DO WHILE nTempl < nDestLen .AND. nDest <= nDestLen
                VAR templChar := cTemplate[nTempl++]
                IF IsPictureLiteral(cType,templChar)
                    result[nDest++] := templChar
                ELSE
                    result[nDest++] := c' '
                ENDIF
            ENDDO
//      ENDIF

        RETURN System.String{result}

    STATIC METHOD TransformS(cValue AS STRING, cPicture AS STRING) AS STRING
        LOCAL cTemplate AS STRING
        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT VAR nPicFunc )
        IF String.IsNullOrEmpty(cTemplate)
            cTemplate := System.String{c'#', cValue:Length}
        ENDIF
        RETURN TransformHelpers.MergeValueAndTemplate(c'C', cValue, cTemplate, nPicFunc)

    STATIC METHOD TransformL( lValue AS LOGIC, cPicture AS STRING ) AS STRING
        LOCAL cTemplate AS STRING

        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT VAR nPicFunc )

        IF cTemplate == ""  // for VO compatiblity, an empty picture string returns T or F
            IF nPicFunc:HasFlag(TransformPictures.YesNo)
                cTemplate := "Y"
            ELSE
                cTemplate := "L"
            ENDIF
        ENDIF
        RETURN TransformHelpers.MergeValueAndTemplate(c'L', IIF(lValue, "T", "F"), cTemplate, nPicFunc)




    STATIC METHOD GetLogicLiteral(lValue AS LOGIC, lYesNo AS LOGIC) AS CHAR
        // Get Literal from the string tables
        LOCAL cReturn AS STRING
        IF lYesNo
            IF lValue
                cReturn := __CavoStr(VOErrors.RT_MSG_SHORT_YES)
            ELSE
                cReturn := __CavoStr(VOErrors.RT_MSG_SHORT_NO)
            ENDIF
        ELSE
            IF lValue
                cReturn := __CavoStr(VOErrors.RT_MSG_SHORT_TRUE)
            ELSE
                cReturn := __CavoStr(VOErrors.RT_MSG_SHORT_FALSE)
            ENDIF
        ENDIF
        RETURN cReturn[0]

    STATIC METHOD TransformD( dValue AS DATE, cPicture AS STRING ) AS STRING
        LOCAL cValue   AS STRING
        LOCAL cTemplate AS STRING
        cTemplate := TransformHelpers.ParseTemplate(cPicture, OUT VAR nPicFunc )
        IF nPicFunc:HasFlag(TransformPictures.British)
           cTemplate := IIF(SetCentury(), "DD/MM/YYYY", "DD/MM/YY")
        ELSEIF String.IsNullOrEmpty(cTemplate)
            cTemplate := GetDateFormat()
        ENDIF
        cTemplate := cTemplate:Replace("D","9"):Replace("M","9"):Replace("Y","9")
        cValue := DToC(dValue)
        RETURN TransformHelpers.MergeValueAndTemplate(c'D', cValue, cTemplate, nPicFunc)

        // check if the character is a valid literal character or a template character
    STATIC METHOD IsPictureLiteral(cType AS CHAR, cChar AS CHAR) AS LOGIC
        SWITCH Char.ToUpper(cType)
            CASE c'D'
            CASE c'N'
                SWITCH Char.ToUpper(cChar)
                CASE c'9'
                CASE c'#'
                CASE c'*'
                CASE c'$'
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
            END SWITCH
            CASE c'L'
                SWITCH Char.ToUpper(cChar)
                CASE c'L'
                CASE c'#'
                CASE c'Y'
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
                END SWITCH

            CASE c'C'
            OTHERWISE
                SWITCH Char.ToUpper(cChar)
                CASE c'9'
                CASE c'#'
                CASE c'A'
                CASE c'L'
                CASE c'N'
                CASE c'X'
                CASE c'Y'
                CASE c'!'
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
            END SWITCH
        END SWITCH
    STATIC METHOD TransformN( nValue AS FLOAT, cPicture AS STRING, lIsInt AS LOGIC ) AS STRING
        //
        // Note: A,N,X,L and Y template chars are treated as normal letters in VO for numeric pictures
        //
        LOCAL cTemplate         AS STRING
        LOCAL cOrigTemplate     AS STRING
        LOCAL cReturn           AS STRING
        LOCAL lWhole            AS LOGIC
        LOCAL nWhole, nDecimal	AS INT
        LOCAL nLen              AS INT

        LOCAL lIsFloat := FALSE AS LOGIC
        nWhole := nDecimal := 0
        lIsFloat := ! lIsInt

        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT VAR nPicFunc )
        cOrigTemplate := cTemplate

        IF  nPicFunc:HasFlag( TransformPictures.Date )
            cTemplate := GetDateFormat():ToUpper():Replace('D' , '#'):Replace('M' , '#'):Replace('Y' , '#')
        ENDIF

        // when no template is provided, created one the way VO does
        IF cTemplate:Length == 0
            cTemplate := System.String{c'#' , IIF(nValue < 10000000000 , 10 , 20) }
            IF lIsFloat
                IF nValue:Decimals != 0
                    cTemplate += "." + System.String{c'9' , nValue:Decimals}
                END IF
            END IF
        ENDIF

        // Convert the arithmetic chars of the VO style template into the .NET format string
        lWhole := TRUE

        FOREACH VAR cChar IN cTemplate
            IF !IsPictureLiteral(c'N', cChar)
                IF lWhole
                    nWhole ++
                ELSE
                    nDecimal ++
                ENDIF
            ELSEIF cChar == c'.'
                IF lWhole
                    lWhole := FALSE
                ELSE
                    nDecimal ++ // multiple dots don't make sense (although VO somehow allows them)
                END IF
            ELSE
                // ','
                NOP
            ENDIF
        NEXT
        LOCAL nLength AS INT
        nLength := nWhole
        IF nDecimal > 0
            nLength += nDecimal + 1
        ENDIF

        // check for overflow
        LOCAL fTemp AS FLOAT
        LOCAL cTemp AS STRING
        fTemp := Round(nValue , nDecimal)

        DO CASE
        CASE fTemp == 0.0
            cTemp		:= Str2(fTemp , (DWORD) nWhole )
        CASE fTemp < 0.0
            IF nWhole <= 1
                cTemp   := "*"
            ELSE
                fTemp	:= -fTemp:CastToInt64()
                cTemp	:= Str2(fTemp , (DWORD) nWhole - 1  )
            ENDIF
        OTHERWISE
                fTemp := fTemp:CastToInt64()
            cTemp := Str2(fTemp , (DWORD) nWhole )
        END CASE
        IF cTemp:IndexOf('*') > -1	// overflow error
            cReturn := Replicate("*", (DWORD) nLength)
            IF nDecimal > 0
                cReturn := Stuff(cReturn,  (DWORD)(nLength-nDecimal),1,".")
            ENDIF
        ELSE

            IF lIsInt .AND. nDecimal == 0
                cReturn := ConversionHelpers.FormatNumber((INT64) nValue, nLength, nDecimal)
            ELSE
                cReturn := ConversionHelpers.FormatNumber((REAL8) nValue, nLength, nDecimal)
            ENDIF

            IF nPicFunc:HasFlag(TransformPictures.ZeroBlank )
                IF nValue == 0
                    IF cOrigTemplate:Length != 0
                       cReturn := Space((DWORD) cOrigTemplate:Length)
                    ELSE
                       cReturn := Space((DWORD) cReturn:Length)
                    ENDIF
                    RETURN cReturn
                ELSEIF lIsFloat .AND. Math.Abs( (REAL8) nValue) < 1.0
                    VAR x := cReturn:IndexOf('.')
                    IF x == -1
                        cReturn := Space((DWORD) cReturn:Length)
                    ELSE
                        IF cReturn:IndexOf("-") != -1 .AND. x >= 2
                            cReturn := Space( (DWORD) x - 2) + "- " + cReturn:Substring(x)
                        ELSE
                            cReturn := Space( (DWORD) x) + cReturn:Substring(x)
                        ENDIF
                    END IF
                ENDIF
            ENDIF
        ENDIF
        // Map result string back to the original template
        cReturn := MergeValueAndTemplate( c'N', cReturn, cTemplate, nPicFunc)
        // add special functions
        IF nValue < 0
            IF  nPicFunc:HasFlag( TransformPictures.ParenLeft )
                cReturn := "(" + cReturn:Substring(1) + ")" // ugly, but so is VO here, too :)
            ELSEIF  nPicFunc:HasFlag(  TransformPictures.ParenRight )
                cTemp := cReturn:Trim()
                nLen  := cReturn:Length - cTemp:Length
                IF nLen == 0
                    cTemp := cTemp:Substring(1)
                ELSE
                    nLen --
                END IF
                cTemp := "(" + cTemp + ")"
                cReturn := cTemp:PadLeft(cReturn:Length,c' ')
            END IF
        END IF

        IF nValue > 0
            IF  nPicFunc:HasFlag( TransformPictures.Credit )
                cReturn := cReturn + " "+ __CavoStr(VOErrors.RT_MSG_CREDIT)
            ENDIF
        ELSEIF nValue < 0
            IF  nPicFunc:HasFlag( TransformPictures.Debit )
                cReturn := cReturn + " "+ __CavoStr(VOErrors.RT_MSG_DEBIT)
            ENDIF
        ENDIF

        IF  nPicFunc:HasFlag( TransformPictures.Left )  .AND. cReturn[0] == c' '
            nLen	:= cReturn:Length
            cReturn := cReturn:TrimStart():PadRight( nLen,c' ')
        ENDIF

        RETURN cReturn

    PRIVATE STATIC METHOD ParseTemplate( cPicture AS STRING, nPicFunc OUT TransformPictures ) AS STRING
        LOCAL cTemplate AS STRING
        LOCAL done := FALSE AS LOGIC
        nPicFunc  := TransformPictures.None

        IF cPicture:Length > 1 .AND. cPicture[0] == c'@'
            VAR nIndex := cPicture:IndexOf(" ")
            IF nIndex > 0
                cTemplate := cPicture:Substring(nIndex+1)
                cPicture  := cPicture:Substring(1, nIndex-1)
            ELSE
                cTemplate := ""
                cPicture  := cPicture:Substring(1)
            ENDIF
            FOREACH cChar AS CHAR IN cPicture
                SWITCH cChar
                CASE c'B' ; CASE c'b'
                     nPicFunc |= TransformPictures.Left
                CASE c'C' ; CASE c'c'
                     nPicFunc |= TransformPictures.Credit
                CASE c'D' ; CASE c'd'
                     nPicFunc |= TransformPictures.Date
                CASE c'E' ; CASE c'e'
                     nPicFunc |= TransformPictures.British
                CASE c'R' ; CASE c'r'
                     nPicFunc |= TransformPictures.NonTemplate
                CASE c'X' ; CASE c'x'
                     nPicFunc |= TransformPictures.Debit
                CASE c'Z' ; CASE c'z'
                    nPicFunc |= TransformPictures.ZeroBlank
                CASE c'('
                    nPicFunc |= TransformPictures.ParenLeft
                CASE c')'
                    nPicFunc |= TransformPictures.ParenRight
                CASE c'!'
                    nPicFunc |= TransformPictures.Upper
                CASE c'Y'; CASE c'y'
                    nPicFunc |= TransformPictures.YesNo
                CASE c' '
                CASE c'\t'
                    done := TRUE
                OTHERWISE
                    //cTemplate += cChar:ToString()
                    NOP // ignore non function chars in the function part
                END SWITCH
                IF done
                    EXIT
                ENDIF
            NEXT
        ELSE
            cTemplate := cPicture
        ENDIF

        RETURN cTemplate

        END CLASS

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform( uValue AS DATE, cSayPicture AS STRING ) AS STRING
    RETURN TransformHelpers.TransformD(uValue, cSayPicture)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform( uValue AS LOGIC, cSayPicture AS STRING ) AS STRING
    RETURN TransformHelpers.TransformL(uValue, cSayPicture)

// /// <summary>Convert any value into a formatted string.</summary>
//FUNCTION Transform( nValue AS LONG, cPicture AS STRING ) AS STRING
//    RETURN TransformHelpers.TransformN( nValue, cPicture, TRUE)
//
// /// <summary>Convert any value into a formatted string.</summary>
//FUNCTION Transform( nValue AS INT64, cPicture AS STRING ) AS STRING
//    RETURN TransformHelpers.TransformN( nValue, cPicture, TRUE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform( uValue AS FLOAT, cSayPicture AS STRING ) AS STRING
    RETURN TransformHelpers.TransformN( uValue, cSayPicture, FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform(uValue AS STRING, cSayPicture AS STRING) AS STRING
    RETURN TransformHelpers.TransformS(uValue, cSayPicture)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform(uValue AS SYMBOL, cSayPicture AS STRING) AS STRING
    // VO Always return an empty string for Transform on a symbol
    RETURN ""


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform( uValue AS USUAL, cSayPicture AS STRING ) AS STRING
    LOCAL ret AS USUAL
    SWITCH uValue:_usualType
    CASE __UsualType.Float
    CASE __UsualType.Decimal
    CASE __UsualType.Currency
        ret := TransformHelpers.TransformN( (FLOAT) uValue, cSayPicture , FALSE)
    CASE __UsualType.Int64
    CASE __UsualType.Long
        ret := TransformHelpers.TransformN( (INT64) uValue, cSayPicture , TRUE)
	CASE __UsualType.Date
    CASE __UsualType.DateTime
        ret := TransformHelpers.TransformD( (DATE) uValue, cSayPicture )
    CASE __UsualType.Logic
        ret := TransformHelpers.TransformL( (LOGIC) uValue, cSayPicture )
    CASE __UsualType.String
    CASE __UsualType.Psz
        ret := TransformHelpers.TransformS( (STRING) uValue,  cSayPicture )
    CASE __UsualType.Void
    CASE __UsualType.Symbol
        ret := ""
    OTHERWISE
        IF uValue:IsObject .AND. IsMethod( uValue, #Transform )
            ret := Send( uValue, "Transform" , cSayPicture )
        ELSE
            THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue),  "Invalid argument type"  ,1)
        ENDIF
    END SWITCH
    RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/unformat/*" />
FUNCTION Unformat( 	cFormatString	AS STRING,  cSayPicture AS STRING, cType AS STRING)	AS USUAL PASCAL
LOCAL uRetVal		AS USUAL
LOCAL lNullable     AS LOGIC

IF SLen(cType) == 2
    lNullable := Right(cType, 1) == "0"
ELSE
    lNullable := FALSE
ENDIF

cType		:= Upper(Left(cType, 1))
cSayPicture := Upper( cSayPicture )
SWITCH cType[0]
CASE c'N'
    uRetVal := TransformHelpers.UnformatN(cFormatString, cSayPicture, lNullable)
CASE c'C'
    uRetVal := TransformHelpers.UnformatC(cFormatString, cSayPicture, lNullable)
CASE c'L'
    uRetVal := TransformHelpers.UnformatL(cFormatString, cSayPicture, lNullable)
CASE c'D'
    uRetVal := TransformHelpers.UnformatD(cFormatString, cSayPicture, lNullable)
OTHERWISE
    uRetVal := NIL
END SWITCH

RETURN uRetVal





