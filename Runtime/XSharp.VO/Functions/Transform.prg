//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text


INTERNAL STATIC CLASS UnformatHelpers

	STATIC METHOD SplitPict(cSayPicture AS STRING, cPic OUT STRING, cFunc OUT STRING) AS LOGIC
		LOCAL iFuncLen  AS INT
		LOCAL lRInsert	:= FALSE AS LOGIC
		cPic := cFunc := ""
		IF cSayPicture:Length > 1 .AND. cSayPicture[0]  == '@'
			iFuncLen := INT(At(" ",cSayPicture))
			IFuncLen -= 2
			IF iFuncLen < 0
				iFuncLen := INT(SLen(cSayPicture))-1	
			ENDIF
			cPic  := AllTrim( SubStr(cSayPicture,iFuncLen+2) )
			cFunc := Upper( SubStr(cSayPicture,2,iFuncLen) )
			
			IF Instr("R",cFunc)
				lRInsert := TRUE
			ENDIF
		ENDIF
		RETURN lRInsert
		
		
	STATIC METHOD UnformatC(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS STRING PASCAL
		LOCAL lRInsert			:= FALSE AS LOGIC
		LOCAL cFunc 			:= ""	AS STRING
		LOCAL wValueLen 		:= 0	AS DWORD
		LOCAL wPictureLen		:= 0	AS DWORD
		LOCAL w 				:= 0	AS DWORD
		LOCAL wValueIdx 		:= 0	AS DWORD
		LOCAL wPictureIdx		:= 0	AS DWORD
		LOCAL cRet				:= ""	AS STRING
		LOCAL cPic				:= ""   AS STRING
		LOCAL cString			AS STRING
		wPictureIdx := 1
		SplitPict(cSayPicture, REF cPic, REF cFunc)
		lRInsert	:= cFunc:Contains("R")
		cSayPicture := cPic
		wPictureLen 	:= SLen(cSayPicture)
		IF lRInsert
			VAR sb := StringBuilder{}
			wValueIdx := 0
			FOR w :=wPictureIdx TO wPictureLen
				wValueIdx += 1
				IF wValueIdx > wValueLen
					EXIT
				ENDIF
				IF "ANX9#YL!":IndexOf(cSayPicture[(INT) w-1]) < 0
					LOOP
				ENDIF
				sb:Append(cValue[(INT) wValueIdx-1])
			NEXT
			IF wValueIdx < wValueLen
				sb:Append(SubStr2( cValue, wValueIdx+1))
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
		LOCAL cChar 				AS STRING
		LOCAL cNumString			:= NULL_STRING	AS STRING
		LOCAL cDecimal				:= "" AS STRING
		LOCAL cTempValue			:= "" AS STRING
		LOCAL cPic					:= "" AS STRING
		LOCAL wNegSignCnt			AS DWORD
		LOCAL wValueLen 			AS DWORD
		LOCAL wPictureLen			AS DWORD
		LOCAL wValueIdx 			AS DWORD
		LOCAL wPictureIdx			AS DWORD
		LOCAL wValDecPos			AS DWORD
		LOCAL wPicDecPos			AS DWORD
		LOCAL uRetVal				:= NIL AS USUAL
		LOCAL nDecSave				AS DWORD
		
		SplitPict(cSayPicture, OUT cPic, OUT cFunc)
		
		cTempValue := AllTrim(cValue)
		wValueLen  := SLen(cTempValue)
		
		IF Instr("X",cFunc) .AND. SubStr3(cTempValue,wValueLen-1,2) == "DB"
			lNegative	:= TRUE
			cTempValue	:= AllTrim( SubStr3(cTempValue,1,wValueLen-2) )
			wValueLen	:= SLen(cTempValue)
		ENDIF
		IF ( Instr(")",cFunc) .OR. Instr("(",cFunc) )	;
		.AND. SubStr3(cTempValue,1,1) == "(" 		;
		.AND. SubStr3(cTempValue,wValueLen,1) == ")"
			lNegative := TRUE
			cTempValue := AllTrim( SubStr3(cTempValue,2,SLen(cTempValue)-2) )
			wValueLen := SLen(cTempValue)
		ELSEIF Instr("C",cFunc) .AND. SubStr3(cTempValue,wValueLen-1,2) == "CR"
			cTempValue := AllTrim( SubStr3(cTempValue,1,wValueLen-2) )
			wValueLen := SLen(cTempValue)
		ENDIF
		IF Instr("D", cFunc)
			LOCAL aMDY[3,2]	AS ARRAY
			LOCAL wTemp1	AS DWORD
			LOCAL wTemp2	AS DWORD
			LOCAL cDateFormat := "" AS STRING
			LOCAL cDate 	  := "" AS STRING
			
			cDateFormat:=GetDateFormat()
			aMDY[1,1] :=At("MM",cDateformat)
			aMDY[1,2] :=2
			aMDY[2,1] :=At("DD",cDateformat)
			aMDY[2,2] :=2
			IF Instr("YYYY",cDateformat)
				aMDY[3,1] :=At("YYYY",cDateformat)
				aMDY[3,2] :=4
			ELSE
				aMDY[3,1] :=At("YY",cDateformat)
				aMDY[3,2] :=2
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
				cDate += SubStr3(cValue,aMDY[w,1],aMDY[w,2])
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
			cDecimal := Chr( SetDecimalSep() )
			
			wPictureLen := SLen(cPic)
			wValDecPos  := At(cDecimal,cTempValue)
			IF wValDecPos == 0
				wValDecPos := wValueLen+1
			ENDIF
			
			wPicDecPos := At(".",cPic)
			
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
				cChar := SubStr3(cTempValue,wValueIdx,1)
				IF cChar == cDecimal .AND. ! lDecimalfound
					IF Empty(cNumString) .AND. lNullable
						uRetVal := NIL
					ELSE
						uRetVal := Val(cNumString)
					ENDIF
					cNumString := ""
					lDecimalfound := TRUE
				ENDIF
				IF Instr(SubStr3(cPic,w,1),"9#$*")
					IF IsDigit(cChar)
						cNumString += cChar
					ELSEIF cChar == "-"
						lNegative := TRUE
					ENDIF
				ENDIF
			NEXT
			IF wValueIdx < wValueLen
				cNumString += SubStr3( cTempValue, wValueIdx+1, wValueLen-wValueIdx )
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
		
		cPic := cSayPicture
		
		cTempValue := AllTrim(cValue)
		nValueLen  := SLen(cTempValue)
		SplitPict(cSayPicture, REF cPic, REF cFunc)
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
		
		
		END	CLASS
		
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

	STATIC METHOD TransformS(cValue AS STRING, cPicture AS STRING) AS STRING
		LOCAL nPicFunc AS TransformPictures
		LOCAL cTemplate AS STRING
		LOCAL cReturn AS STRING
		LOCAL nLen AS INT
		
		cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
		
		IF nPicFunc:HasFLag( TransformPictures.Upper ) 
			cValue := cValue:ToUpper()
		ENDIF
		
		nLen := cTemplate:Length
		IF nLen == 0
			cReturn := cValue
		ELSE
			cValue := PadR(cValue,nLen)
			LOCAL sb AS StringBuilder
			LOCAL nPos := 0  AS INT
			sb := StringBuilder{}
			FOREACH cChar AS CHAR IN cTemplate
				SWITCH Char.ToUpper(cChar)
					CASE '!'
						sb:Append(Char.ToUpper(cValue[nPos]))
						nPos++
					CASE 'A' ; CASE 'N' ; CASE 'X' ; CASE '9'; CASE '#'
						sb:Append(cValue[nPos])
						nPos++
					OTHERWISE
						sb:Append(cChar)
						IF ! nPicFunc:hasFlag(TransformPictures.NonTemplate)
							npos++
						ENDIF
					END SWITCH
				NEXT
				cReturn := sb:ToString()
			ENDIF
		RETURN cReturn
	
	STATIC METHOD TransformL( lValue AS LOGIC, cPicture AS STRING ) AS STRING
		LOCAL nPicFunc AS TransformPictures
		LOCAL cTemplate AS STRING
		LOCAL cReturn AS STRING
		LOCAL sReturn AS StringBuilder
		LOCAL cChar AS CHAR
		LOCAL nLen AS INT
		LOCAL lDone AS LOGIC
		
		cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
		
		IF cTemplate == ""  // for VO compatiblity, an empty picture string returns T or F
			IF nPicFunc:HasFlag(TransformPictures.YesNo)
				cTemplate := "Y"
			ELSE
				cTemplate := "L"
			ENDIF
		ENDIF
		
		IF nPicFunc:HasFlag(TransformPictures.NonTemplate)	// @R
			nLen    := cTemplate:Length
			sReturn := StringBuilder{ nLen }
			
			nLen--
			lDone := FALSE
			FOREACH c AS CHAR IN cTemplate
				SWITCH c
				CASE 'L' ; CASE 'l'
					sReturn:Append( IIF( lDone, " ", GetLogicLiteral(lValue, FALSE) ) )
					lDone := TRUE
				CASE 'Y' ; CASE 'y'	
					sReturn:Append( IIF( lDone, " ", GetLogicLiteral(lValue, TRUE) ) )
					lDone := TRUE
				OTHERWISE
					sReturn:Append( c )
				END SWITCH
			NEXT
			cReturn := sReturn:ToString()
			
		ELSE
			// No @R so simply convert
			cChar := cTemplate[0]
			IF cChar == 'Y' .OR. cChar == 'y' .OR. (nPicFunc:HasFlag(TransformPictures.YesNo) .AND. (cChar == 'L' .OR. cChar == 'l'))
				cReturn := GetLogicLiteral(lValue, TRUE)
			ELSEIF cChar == 'L' .OR. cChar == 'l'
				cReturn := GetLogicLiteral(lValue, FALSE)
			ELSE
				cReturn := cChar:ToString()
			ENDIF
			
		ENDIF
		RETURN cReturn


	
	STATIC METHOD GetLogicLiteral(lValue AS LOGIC, lYesNo AS LOGIC) AS STRING
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
		RETURN cReturn

	STATIC METHOD TransformD( dValue AS DATE, cPicture AS STRING ) AS STRING
		LOCAL nPicFunc AS TransformPictures
		LOCAL cReturn AS STRING
		LOCAL dt AS DateTime
		LOCAL cFormat AS STRING
		
		TransformHelpers.ParseTemplate(cPicture, OUT nPicFunc )
		
		DO CASE
			CASE nPicFunc:HasFlag(TransformPictures.Date)
				cReturn := DToC(dValue)
				
			CASE nPicFunc:HasFlag(TransformPictures.British)
				dt := dValue:ToDateTime()
				IF SetCentury()
					cFormat := "dd'/'MM'/'yyyy"
				ELSE
					cFormat := "dd'/'MM'/'yy"
				ENDIF
				
				cReturn := dt:ToString(cFormat)
				
			OTHERWISE // VO returns empty string with @R, but lets ignore it and return the same thing for every template type
				cReturn := DToC(dValue)
				
		ENDCASE
		
		RETURN cReturn

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
		LOCAL cThous, cDec       AS CHAR
		LOCAL lForceEuropean 	AS LOGIC
		LOCAL nLen              AS INT
		
		LOCAL x                 AS INT
		LOCAL lIsFloat := FALSE AS LOGIC
		nWhole := nDecimal := 0
		lIsFloat := ! lIsInt
		
		cDec	:= (CHAR) RuntimeState.DecimalSep
		cThous	:= (CHAR) RuntimeState.ThousandSep
		
		cTemplate := TransformHelpers.ParseTemplate( cPicture, OUT nPicFunc )
		
		lForceEuropean :=  nPicFunc:HasFLag( TransformPictures.British ) 
		
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
		SWITCH cChar
			CASE '9' ; CASE '#' ; CASE '*' ; CASE '$'
				IF lWhole
					nWhole ++
				ELSE
					nDecimal ++
				ENDIF
					
			CASE '.'
				IF lWhole
					lWhole := FALSE
				ELSE
					nDecimal ++ // multiple dots don't make sense (although VO somehow allows them)
				END IF
			OTHERWISE
				// What else ?
				NOP
			END SWITCH
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

			IF lIsInt
				cReturn := ConversionHelpers.FormatNumber((INT64) nValue, nLength, nDecimal)
			ELSE
				cReturn := ConversionHelpers.FormatNumber((REAL8) nValue, nLength, nDecimal)
			ENDIF
			
			IF nPicFunc:HasFLag(TransformPictures.ZeroBlank ) 
				IF nValue == 0
					cReturn := Space(cReturn:Length)
					
				ELSEIF lIsFloat .AND. Math.Abs(nValue:Value) < 1.0
					x := cReturn:IndexOf(cDec)
					IF x == -1
						cReturn := Space(cReturn:Length)
					ELSE
						IF cReturn:IndexOf("-") != -1 .AND. x >= 2
							cReturn := Space( x - 2) + "- " + cReturn:Substring(x)
						ELSE
							cReturn := Space( x) + cReturn:Substring(x)
						ENDIF
					END IF
				END IF
			ENDIF
		ENDIF	
		// Map result string back to the original template
		
		LOCAL nMap		        AS INT
		LOCAL sb                AS StringBuilder
		nLen	:= cOriginalTemplate:Length - 1
		sb		:= StringBuilder{nLen + 1}
		nMap	:= cReturn:Length - 1
		
		FOR x := nLen DOWNTO 0
			VAR cChar := cOriginalTemplate[x]
				SWITCH cChar
				CASE '9' ; CASE '#' ; CASE '*' ; CASE '$' ; CASE '.'
					IF nMap >= 0
						IF cChar == '.'
							IF lForceEuropean 
								sb:Insert(0 , ',')
							ELSE
								sb:Insert(0, cDec)
							ENDIF
						ELSE
							sb:Insert(0 , cReturn[nMap])
						END IF
					ELSE
						IF cChar == '*' .OR. cChar == '$'
							sb:Insert(0, cChar)
						ELSE
							sb:Insert(0 , ' ')
						ENDIF
					END IF
					nMap --
					
				CASE ','
					IF nMap >= 0
						IF nMap == 0 .AND. cReturn[nMap] == '-'
							sb:Insert(0 , cReturn[nMap])
							nMap --
						ELSE
							IF lForceEuropean
								sb:Insert(0 , '.')
							ELSE
								sb:Insert(0 , cThous)
							END IF
						END IF
					ELSE
						sb:Insert(0 , ' ')
					END IF
					
				OTHERWISE
				
					sb:Insert(0 , cChar)
					
				END SWITCH
			NEXT
		cReturn := sb:ToString()
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
		cTemplate := ""
		nPicFunc  := TransformPictures.None
		
		IF cPicture:Length > 1 .AND. cPicture[0] == '@'
			VAR nIndex := cPicture:IndexOf(" ")
			IF nIndex > 0
				cTemplate := cPicture:Substring(nIndex+1)
				cPicture  := cPicture:Substring(1, nIndex-1)
			ELSE
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
		uRetVal := UnformatHelpers.UnformatN(cValue, cSayPicture, lNullable)
	CASE 'C'
		uRetVal := UnformatHelpers.UnformatC(cValue, cSayPicture, lNullable)
	CASE 'L'
		uRetVal := UnformatHelpers.UnformatL(cValue, cSayPicture, lNullable)
	CASE 'D'
		uRetVal := UnformatHelpers.UnformatD(cValue, cSayPicture, lNullable)
	OTHERWISE
		uRetVal := NIL
	END SWITCH

	RETURN uRetVal





