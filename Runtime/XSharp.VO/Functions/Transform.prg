//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text


INTERNAL STATIC CLASS TransFormHelpers
    [Flags] ;
    ENUM TransformPictures
        None        := 0
        Left        := 1
        Credit      := 2
        @@Date      := 4
        British     := 8
        NonTemplate := 16
        Debit       := 32
        ZeroBlank   := 64
        ParenLeft   := 128
        ParenRight  := 256
        Upper       := 512
        YesNo       := 1024
    END ENUM

    STATIC METHOD SplitPict(cSayPicture AS STRING, cPic OUT STRING, cFunc OUT STRING) AS LOGIC
        LOCAL iFuncLen  AS INT
        cPic := cFunc := ""
        IF cSayPicture:Length > 1 .AND. cSayPicture[0]  == '@'
            iFuncLen := cSayPicture:IndexOf(" ") -1
            IF iFuncLen < 0
                // No space delimiter so we assume the whole length
                iFuncLen := cSayPicture:Length-1
                cFunc    := cSayPicture:SubString(1):ToUpper()
            ELSE
                cPic  := cSayPicture:SubString(iFuncLen+2):Trim()
                cFunc := cSayPicture:SubString(1, iFuncLen):ToUpper()
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
        lRInsert	:= SplitPict(cSayPicture, OUT cPic, REF cFunc)
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
                IF TransFormHelpers.IsPictureLiteral('C', cSayPicture[ w]) 
                    LOOP
                ENDIF
                sb:Append(cValue[ wValueIdx-1])
            NEXT
            IF wValueIdx < wValueLen
                sb:Append(cValue:SubString( wValueIdx+1))
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
        LOCAL cFunc 			AS STRING
        LOCAL cTempValue		AS STRING
        LOCAL dRet				AS DATE
        
        SplitPict(cSayPicture, OUT cTempValue, OUT cFunc)
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
        LOCAL cDecimal				:= '\0' AS CHAR
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
            cTempValue	:= cTempValue:SubString(0, wValueLen-2):Trim()
            wValueLen	:= cTempValue:Length
        ELSEIF cFunc:Contains("C") .AND. cTempValue:EndsWith("CR")
            cTempValue := cTempValue:SubString(0, wValueLen-2):Trim()
            wValueLen := cTempValue:Length
        ENDIF
        IF (cFunc:Contains(")") .OR. cFunc:Contains("(")) ;
            .AND. cTempValue:StartsWith("(") .AND. cTempValue:EndsWith(")")
            lNegative := TRUE
            cTempValue := cTempValue:SubString(1, wValueLen-2)
            wValueLen := cTempValue:Length
        ENDIF
        IF cFunc:Contains("D")
            LOCAL aMDY[3,2]	AS ARRAY
            LOCAL wTemp1	AS DWORD
            LOCAL wTemp2	AS DWORD
            LOCAL cDateFormat := "" AS STRING
            LOCAL cDate 	  := "" AS STRING
            
            cDateFormat:=GetDateFormat()
            aMDY[1,1] := cDateformat:IndexOf("MM")
            aMDY[1,2] :=2
            aMDY[2,1] := cDateformat:IndexOf("DD")
            aMDY[2,2] :=2
            aMDY[3,1] := cDateformat:IndexOf("YYYY")
            IF aMDY[3,1] == -1
                aMDY[3,1] := cDateformat:IndexOf("YY")
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
                cDate += cValue:SubString(aMDY[w,1],aMDY[w,2])
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
                IF cChar == cDecimal .AND. ! lDecimalfound
                    IF Empty(cNumString) .AND. lNullable
                        uRetVal := NIL
                    ELSE
                        uRetVal := Val(cNumString)
                    ENDIF
                    cNumString := ""
                    lDecimalfound := TRUE
                ENDIF
                IF !TransFormHelpers.IsPictureLiteral('N', cPic[(INT) w-1])
                    IF Char.IsDigit(cChar)
                        cNumString += cChar
                    ELSEIF cChar == '-'
                        lNegative := TRUE
                    ENDIF
                ENDIF
            NEXT
            IF wValueIdx < wValueLen
                cNumString +=  cTempValue:SubString(wValueIdx, wValueLen-wValueIdx )
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
            uRetval := -uRetVal
        ENDIF
        RETURN uRetVal
        
        
    STATIC METHOD UnformatL(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS LOGIC PASCAL
        LOCAL cTempValue    := "" AS STRING
        LOCAL cChar         := "" AS STRING
        LOCAL nValueLen     := 0  AS DWORD
        LOCAL nValIdx       := 0  AS DWORD
        LOCAL cPic          := "" AS STRING
        LOCAL cFunc         := "" AS STRING
        LOCAL lRInsert	    := FALSE AS LOGIC
        LOCAL nPictureLen	:= 0  AS DWORD
        LOCAL n, nTemp		:= 0  AS DWORD
        LOCAL lRet          := FALSE AS LOGIC
        
        cTempValue := AllTrim(cValue)
        nValueLen  := SLen(cTempValue)
        SplitPict(cSayPicture, OUT cPic, REF cFunc)
        lRInsert   := cFunc:Contains("R")
        
        IF Empty(cTempValue) .AND. lNullable
            lRet := .F.
        ELSE
            LOCAL aTemp AS ARRAY
            aTemp    := ArrayCreate(5)
            IF Instr("L", cPic) .OR. Instr("L", cFunc)
                aTemp[1] := "TRUE"
                aTemp[2] := ".T."
                aTemp[3] := "T"
                aTemp[4] := "YES"
                aTemp[5] := "Y"
            ELSE
                aTemp[1] := SetLiteral(VOErrors.RT_MSG_Long_True)
                aTemp[2] := ".T."
                aTemp[3] := SetLiteral(VOErrors.RT_MSG_Short_True)
                aTemp[4] := SetLiteral(VOErrors.RT_MSG_Long_Yes)
                aTemp[5] := SetLiteral(VOErrors.RT_MSG_Short_Yes)
            ENDIF
            
            IF cPic == ""
                nTemp := AScanExact( aTemp, Upper(cTempValue) )
                IF nTemp > 0
                    lRet := .T.
                ENDIF
            ELSE
                nPictureLen := SLen(cPic)
                IF lRInsert
                    nValIdx := 0
                    FOR n := 1 TO nPictureLen
                        nValIdx += 1
                        IF nValIdx > nValueLen
                            EXIT
                        ENDIF
                        IF Instr(SubStr(cPic,n,1),"YL")
                            cChar := SubStr(cValue,nValIdx,1)
                            nTemp := AScanExact( aTemp, Upper(cChar) )
                            IF nTemp > 0
                                lRet := .T.
                            ENDIF
                            EXIT
                        ENDIF
                    NEXT
                ELSE
                    cChar := SubStr(cValue,1,1)
                    nTemp := AScanExact( aTemp, Upper(cChar) )
                    IF nTemp > 0
                        lRet := .T.
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        
        RETURN lRet
        
        
        
        
        
        
    STATIC METHOD MergeValueAndTemplate(cType AS CHAR, cValue AS STRING, cTemplate AS STRING, nPictures AS TransFormPictures) AS STRING
        LOCAL result := cTemplate:ToCharArray() AS CHAR[]
        LOCAL nSrc   := 0 AS INT
        LOCAL nTempl := 0 AS INT
        LOCAL nDest  := __ARRAYBASE__ AS INT
        LOCAL nSrcLen := cValue:Length AS INT
        LOCAL nDestLen:= cTemplate:Length AS INT
        LOCAL lBritish := nPictures:HasFlag(TransFormPictures:British) AS LOGIC
        DO WHILE nSrc < nSrcLen .AND. nDest -__ARRAYBASE__ < nDestLen 
            VAR templChar := cTemplate[nTempl++]
            VAR srcChar   := cValue[nSrc]
            IF IsPictureLiteral(cType, templChar)
                IF cType == 'N' .AND. templChar == '.'
                    // Decimal separator ?
                    result[nDest++] := IIF(lBritish, ',', (CHAR) RuntimeState.DecimalSep)
                    nSrc++
                    
                ELSEIF cType == 'N' .AND. templChar == ',' .AND. nDest > 0
                    // Thousand separator ?
                    LOCAL cLast := result[nDest-1] AS CHAR
                    IF Char.IsDigit(cLast)
                        result[nDest++] := IIF(lBritish, '.', (CHAR) RuntimeState.ThousandSep)
                    ELSEIF cLast == '-' .OR. cLast == '+'
                        // overwrite the + or minus sign with a space and set the sign at the place of the comma
                        result[nDest-1] := ' '
                        result[nDest] := cLast
                        nDest++
                    ELSE
                        result[nDest++] := cLast
                    ENDIF
                ELSEIF cType == 'L' .AND. templChar == 'Y'
                    result[nDest++] := TransformHelpers.GetLogicLiteral(srcChar == 'T', cType == 'Y')
                    nSrc++
                ELSE
                    // Normal template literal. For numeric or @R pictures no change to the src pointer
                    // otherwise increase src pointer
                    result[nDest++] := templChar
                    IF nPictures:HasFlag(TransformPictures:NonTemplate) .OR. cType == 'N'
                        NOP
                    ELSE
                        nSrc++
                    ENDIF
                ENDIF
                
            ELSE
                // Non template char
                IF templChar == 'Y' .OR. templChar == 'y'
                    IF srcChar == 'T'
                        result[nDest++] := TransformHelpers.GetLogicLiteral(TRUE, TRUE)
                    ELSE
                        result[nDest++] := TransformHelpers.GetLogicLiteral(FALSE, TRUE)
                    ENDIF
                    nSrc++
                ELSEIF srcChar == ' ' .AND. (templChar == '*' .OR. templChar == '$')
                    result[nDest++] := templChar
                    nSrc++
                ELSE
                    IF lBritish .AND. cType == 'N' .AND. srcChar == '.'
                        result[nDest++] := ','
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransformPictures:Upper) .OR. templChar == '!'
                        result[nDest++] := Char.ToUpper(srcChar)
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransFormPictures:ZeroBlank) .AND. cType == 'N' .AND. srcChar == '0'
                        LOCAL lHasDig := FALSE AS LOGIC
                        FOR VAR x := 0 TO nDest
                            IF Char.IsDigit(result[x])
                                lHasDig := TRUE
                                EXIT
                            ENDIF
                        NEXT
                        IF lHasDig
                            result[nDest++] := srcChar
                        ELSE
                            result[nDest++] := ' '
                        ENDIF
                        nSrc++
                    ELSEIF nPictures:HasFlag(TransFormPictures:YesNo) .AND. cType == 'L'
                        result[nDest++] := TransformHelpers.GetLogicLiteral(srcChar == 'T', TRUE)
                        nSrc++
                    ELSE
                        result[nDest++] := srcChar
                        nSrc++
                    ENDIF
                ENDIF  
            ENDIF
        ENDDO
        // any remaining templ chars can be copied to the end of the string
        IF nPictures:HasFlag(TransFormPictures:NonTemplate) .OR. cType == 'N'
            DO WHILE nTempl < nDestLen .AND. nDest <= nDestLen - __ARRAYBASE__
                VAR templChar := cTemplate[nTempl++]
                IF IsPictureLiteral(cType,templChar)
                    result[nDest++] := templChar
                ELSE
                    result[nDest++] := ' '
                ENDIF
            ENDDO
        ENDIF
        
        RETURN System.String{result}
        
    STATIC METHOD TransformS(cValue AS STRING, cPicture AS STRING) AS STRING
        LOCAL nPicFunc AS TransformPictures
        LOCAL cTemplate AS STRING
        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
        IF String.IsNullOrEmpty(cTemplate)
            cTemplate := System.String{'#', cValue:Length}
        ENDIF
        RETURN TransformHelpers.MergeValueAndTemplate('C', cValue, cTemplate, nPicFunc)
        
    STATIC METHOD TransformL( lValue AS LOGIC, cPicture AS STRING ) AS STRING
        LOCAL nPicFunc AS TransformPictures
        LOCAL cTemplate AS STRING
        
        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
        
        IF cTemplate == ""  // for VO compatiblity, an empty picture string returns T or F
            IF nPicFunc:HasFlag(TransformPictures.YesNo)
                cTemplate := "Y"
            ELSE
                cTemplate := "L"
            ENDIF
        ENDIF
        RETURN TransformHelpers.MergeValueAndTemplate('L', IIF(lValue, "T", "F"), cTemplate, nPicFunc)
        
        
        

    STATIC METHOD GetLogicLiteral(lValue AS LOGIC, lYesNo AS LOGIC) AS CHAR
        // Get Literal from the string tables
        LOCAL cReturn AS STRING
        IF lYesNo
            IF lValue
                cReturn := SetLiteral(VOErrors.RT_MSG_SHORT_YES)
            ELSE
                cReturn := SetLiteral(VOErrors.RT_MSG_SHORT_NO)
            ENDIF
        ELSE
            IF lValue
                cReturn := SetLiteral(VOErrors.RT_MSG_SHORT_TRUE)
            ELSE
                cReturn := SetLiteral(VOErrors.RT_MSG_SHORT_FALSE)
            ENDIF
        ENDIF
        RETURN cReturn[0]
        
    STATIC METHOD TransformD( dValue AS DATE, cPicture AS STRING ) AS STRING
        LOCAL nPicFunc AS TransformPictures
        LOCAL cValue   AS STRING
        LOCAL cTemplate AS STRING
        cTemplate := TransformHelpers.ParseTemplate(cPicture, OUT nPicFunc )
        IF nPicFunc:HasFlag(TransFormPictures.British)
           cTemplate := IIF(SetCentury(), "DD/MM/YYYY", "DD/MM/YY") 
        ELSEIF String.IsNullOrEmpty(cTemplate)
            cTemplate := GetDateFormat()
        ENDIF
        cTemplate := cTemplate:Replace("D","9"):Replace("M","9"):Replace("Y","9")
        cValue := DToC(dValue)
        RETURN TransformHelpers.MergeValueAndTemplate('D', cValue, cTemplate, nPicFunc)
        
        // check if the character is a valid literal character or a template character
    STATIC METHOD IsPictureLiteral(cType AS CHAR, cChar AS CHAR) AS LOGIC 
        SWITCH Char.ToUpper(cType)
            CASE 'D'
            CASE 'N'
                SWITCH Char.ToUpper(cChar)
                CASE '9'
                CASE '#'
                CASE '*'
                CASE '$' 
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
            END SWITCH
            CASE 'L'
                SWITCH Char.ToUpper(cChar)
                CASE 'L'
                CASE '#'
                CASE 'Y'
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
                END SWITCH
                
            CASE 'C'
            OTHERWISE
                SWITCH Char.ToUpper(cChar)
                CASE '9'
                CASE '#'
                CASE 'A'
                CASE 'L'
                CASE 'N'
                CASE 'X'
                CASE 'Y'
                CASE '!'
                    RETURN FALSE
                OTHERWISE
                    RETURN TRUE
            END SWITCH
        END SWITCH
    STATIC METHOD TransformN( nValue AS FLOAT, cPicture AS STRING, lIsInt AS LOGIC ) AS STRING
        //
        // Note: A,N,X,L and Y template chars are treated as normal letters in VO for numeric pictures
        //
        LOCAL cOriginalTemplate AS STRING
        LOCAL nPicFunc          AS TransformPictures
        LOCAL cTemplate         AS STRING
        LOCAL cReturn           AS STRING
        LOCAL lWhole            AS LOGIC
        LOCAL nWhole, nDecimal	AS INT
        LOCAL nLen              AS INT
        
        LOCAL lIsFloat := FALSE AS LOGIC
        nWhole := nDecimal := 0
        lIsFloat := ! lIsInt
        
        
        cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
        
        
        IF  nPicFunc:HasFlag( TransformPictures.Date ) 
            cTemplate := GetDateFormat():ToUpper():Replace('D' , '#'):Replace('M' , '#'):Replace('Y' , '#')
        ENDIF
        
        // when no template is provided, created one the way VO does
        IF cTemplate:Length == 0
            cTemplate := System.String{'#' , IIF(nValue < 10000000000 , 10 , 20) }
            IF lIsFloat
                IF nValue:Decimals != 0
                    cTemplate += "." + System.String{'9' , nValue:Decimals}
                END IF
            END IF
            cOriginalTemplate := cTemplate
        ENDIF
        
        cOriginalTemplate := cTemplate
        
        // Convert the arithmetic chars of the VO style template into the .NET format string
        lWhole := TRUE
        
        FOREACH VAR cChar IN cTemplate
            IF !IsPictureLiteral('N', cChar)
                IF lWhole
                    nWhole ++
                ELSE
                    nDecimal ++
                ENDIF
            ELSEIF cChar == '.'
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
                    cTemp	:= Str2(fTemp , (DWORD) nWhole  )
            ENDIF
        OTHERWISE
                fTemp := fTemp:CastToInt64()
            cTemp := Str2(fTemp , (DWORD) nWhole )
        END CASE
        IF cTemp:IndexOf('*') > -1	// overflow error
            cReturn := Replicate("*", (DWORD) nLength)
            IF nDecimal > 0
                cReturn := Stuff(cReturn, (DWORD) nLength-nDecimal,1,".")
            ENDIF
        ELSE
        
            IF lIsInt .AND. nDecimal == 0
                cReturn := ConversionHelpers.FormatNumber((INT64) nValue, nLength, nDecimal)
            ELSE
                cReturn := ConversionHelpers.FormatNumber((REAL8) nValue, nLength, nDecimal)
            ENDIF
            
            IF nPicFunc:HasFLag(TransformPictures.ZeroBlank ) 
                IF nValue == 0
                    cReturn := Space(cReturn:Length)
                    
                ELSEIF lIsFloat .AND. Math.Abs(nValue:Value) < 1.0
                    VAR x := cReturn:IndexOf('.')
                    IF x == -1
                        cReturn := Space(cReturn:Length)
                    ELSE
                        IF cReturn:IndexOf("-") != -1 .AND. x >= 2
                            cReturn := Space( x - 2) + "- " + cReturn:Substring(x)
                        ELSE
                            cReturn := Space( x) + cReturn:Substring(x)
                        ENDIF
                    END IF
                ENDIF
            ENDIF
        ENDIF	
        // Map result string back to the original template
        cReturn := MergeValueAndTemplate( 'N', cReturn, cTemplate, nPicFunc)
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
                cReturn := cTemp:PadLeft(cReturn:Length,' ')
            END IF
        END IF
        
        IF nValue > 0
            IF  nPicFunc:HasFlag( TransformPictures.Credit ) 
                // TODO: should these be localized?
                cReturn := cReturn + " CR"
            ENDIF
        ELSEIF nValue < 0
            IF  nPicFunc:HasFlag( TransformPictures.Debit ) 
                // TODO: should these be localized?
                cReturn := cReturn + " DB"
            ENDIF
        ENDIF
        
        IF  nPicFunc:HasFlag( TransformPictures.Left )  .AND. cReturn[0] == ' '
            nLen	:= cReturn:Length
            cReturn := cReturn:TrimStart():PadRight( nLen,' ')
        ENDIF
        
        RETURN cReturn
        
    PRIVATE STATIC METHOD ParseTemplate( cPicture AS STRING, nPicFunc OUT TransformPictures ) AS STRING
        LOCAL cTemplate AS STRING
        LOCAL done := FALSE AS LOGIC
        nPicFunc  := TransformPictures.None
        
        IF cPicture:Length > 1 .AND. cPicture[0] == '@'
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
        CASE 'B' ; CASE 'b'
                nPicFunc |= TransformPictures.Left
        CASE 'C' ; CASE 'c'
                nPicFunc |= TransformPictures.Credit
        CASE 'D' ; CASE 'd'
                nPicFunc |= TransformPictures.Date
        CASE 'E' ; CASE 'e'
                nPicFunc |= TransformPictures.British
        CASE 'R' ; CASE 'r'
                nPicFunc |= TransformPictures.NonTemplate
        CASE 'X' ; CASE 'x'
                nPicFunc |= TransformPictures.Debit
        CASE 'Z' ; CASE 'z'
                    nPicFunc |= TransformPictures.ZeroBlank
                CASE '('
                    nPicFunc |= TransformPictures.ParenLeft
                CASE ')'
                    nPicFunc |= TransformPictures.ParenRight
                CASE '!'
                nPicFunc |= TransformPictures.Upper
        CASE 'Y'; CASE 'y'
                    nPicFunc |= TransformPictures.YesNo
                CASE ' '
                CASE '\t'
                    done := TRUE
                OTHERWISE
                    cTemplate += cChar:ToString()            
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
        
        
FUNCTION Transform( dValue AS DATE, cPicture AS STRING ) AS STRING
    RETURN TransFormHelpers.TransformD(dValue, cPicture)	
    
FUNCTION Transform( lValue AS LOGIC, cPicture AS STRING ) AS STRING
    RETURN TransFormHelpers.TransformL(lValue, cPicture)
    
FUNCTION Transform( nValue AS LONG, cPicture AS STRING ) AS STRING
    RETURN TransFormHelpers.TransformN( nValue, cPicture, TRUE)
    
FUNCTION Transform( nValue AS INT64, cPicture AS STRING ) AS STRING
    RETURN TransFormHelpers.TransformN( nValue, cPicture, TRUE)
    
FUNCTION Transform( nValue AS FLOAT, cPicture AS STRING ) AS STRING
    RETURN TransFormHelpers.TransformN( nValue, cPicture, FALSE)
    
FUNCTION Transform(cValue AS STRING, cPicture AS STRING) AS STRING
    RETURN TransFormHelpers.TransformS(cValue, cPicture)
    
FUNCTION Transform( uValue AS USUAL, cPicture AS STRING ) AS STRING
    LOCAL ret AS USUAL
    SWITCH uValue:_UsualType
CASE __UsualType.Float
    CASE __UsualType.Decimal
        ret := TransformHelpers.TransformN( uValue, cPicture , FALSE)
    CASE __UsualType.Int64
    CASE __UsualType.Long
        ret := TransformHelpers.TransformN( uValue, cPicture , TRUE)
CASE __UsualType.Date
    CASE __UsualType.DateTime
        ret := TransformHelpers.TransformD( uValue, cPicture )
    CASE __UsualType.Logic
        ret := TransformHelpers.TransformL( uValue, cPicture )
    CASE __UsualType.String
    CASE __UsualType.Psz
        ret := TransformHelpers.TransformS( uValue, cPicture )
    CASE __UsualType.Void
        ret := ""
    OTHERWISE
            IF uValue:IsObject && IsMethod( uValue, #Transform )
                ret := Send( uValue, "Transform" , cPicture )
            ELSE
                THROW Error.ArgumentError( __ENTITY__, NAMEOF(uValue),  "Invalid argument type"  ,1)
        ENDIF
    END SWITCH
    RETURN ret
    
    
FUNCTION Unformat( 	cValue	AS STRING,  cSayPicture AS STRING, cType AS STRING)	AS USUAL PASCAL
LOCAL uRetVal		AS USUAL
LOCAL lNullable     AS LOGIC

IF SLen(CType) == 2
    lNullable := Right(cType, 1) == "0"
ELSE
    lNullable := FALSE
ENDIF

cType		:= Upper(Left(cType, 1))
cSayPicture := Upper( cSayPicture )
SWITCH cType[0]
CASE 'N'
    uRetVal := TransFormHelpers.UnformatN(cValue, cSayPicture, lNullable)
CASE 'C'
    uRetVal := TransFormHelpers.UnformatC(cValue, cSayPicture, lNullable)
CASE 'L'
    uRetVal := TransFormHelpers.UnformatL(cValue, cSayPicture, lNullable)
CASE 'D'
    uRetVal := TransFormHelpers.UnformatD(cValue, cSayPicture, lNullable)
OTHERWISE
    uRetVal := NIL
END SWITCH

RETURN uRetVal





