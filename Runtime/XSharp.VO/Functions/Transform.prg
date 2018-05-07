//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Text
[Flags] ;
INTERNAL ENUM TransformPictures
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
end enum

STATIC FUNCTION _TransformTemplate( cPicture AS STRING, nPicFunc OUT TransformPictures ) AS STRING
   LOCAL cTemplate AS STRING
   local done := false as logic
   local nPos := 0 as int
   cTemplate := ""
   nPicFunc  := TransformPictures.None

   if cPicture:Length > 1 .and. cPicture[0] == '@'
	var nIndex := cPicture:IndexOf(" ")
      if nIndex > 0
		cTemplate := cPicture:Substring(nIndex+1)
		cPicture  := cPicture:Substring(1, nIndex-1)
	  else
		cPicture  := cPicture:Substring(1)
	  endif
      foreach cChar as char in cPicture
		nPos ++
		SWITCH cChar
         case 'B' ; case 'b'
            nPicFunc |= TransformPictures.Left
         case 'C' ; case 'c'
            nPicFunc |= TransformPictures.Credit
         case 'D' ; case 'd'
            nPicFunc |= TransformPictures.Date
         case 'E' ; case 'e'
            nPicFunc |= TransformPictures.British
         case 'R' ; case 'r'
            nPicFunc |= TransformPictures.NonTemplate
         case 'X' ; case 'x'
            nPicFunc |= TransformPictures.Debit
         case 'Z' ; case 'z'
            nPicFunc |= TransformPictures.ZeroBlank
         CASE '('
            nPicFunc |= TransformPictures.ParenLeft
         CASE ')'
            nPicFunc |= TransformPictures.ParenRight
         CASE '!'
            nPicFunc |= TransformPictures.Upper
         CASE 'Y'; case 'y'
            nPicFunc |= TransformPictures.YesNo
		 case ' '
		 case '\t'
			done := true
		 otherwise
			cTemplate += cChar:ToString()            
         END SWITCH
		 if done
			exit
		endif
      next
   ELSE
      cTemplate := cPicture
   ENDIF

   RETURN cTemplate

FUNCTION Transform( dValue AS DATE, cPicture AS STRING ) AS STRING
   LOCAL nPicFunc AS TransformPictures
   LOCAL cReturn AS STRING
   LOCAL dt AS DateTime
   LOCAL cFormat AS STRING

   _TransformTemplate( cPicture, oUT nPicFunc )

   DO CASE
   CASE nPicFunc == TransformPictures.Date
      cReturn := DToC(dValue)

   CASE nPicFunc == TransformPictures.British
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


FUNCTION Transform( lValue AS LOGIC, cPicture AS STRING ) AS STRING
   LOCAL nPicFunc AS TransformPictures
   LOCAL cTemplate AS STRING
   LOCAL cReturn AS STRING
   LOCAL sReturn AS StringBuilder
   LOCAL cChar AS Char
   LOCAL nLen AS INT
   LOCAL lDone AS LOGIC

   cTemplate := _TransformTemplate( cPicture, OUT nPicFunc )

   IF cTemplate == ""  // for VO compatiblity, an empty picture string returns T or F
      IF nPicFunc == TransformPictures.YesNo
         cTemplate := "Y"
      ELSE
         cTemplate := "L"
      ENDIF
   ENDIF
   
   IF nPicFunc == TransformPictures.NonTemplate

      nLen := cTemplate:Length
      sReturn := StringBuilder{ nLen }
   
      nLen--
	  lDone := FALSE
      FOREACH c as char in cTemplate
         SWITCH c
         CASE 'L' ; case 'l'
            sReturn:Append( IIF( lDone, ' ', IIF( lValue, 'T', 'F') ) )
            lDone := TRUE
         CASE 'Y' ; case 'y'	
            sReturn:Append( IIF( lDone, ' ', IIF( lValue, 'Y', 'N' ) ) )
            lDone := TRUE
         OTHERWISE
            sReturn:Append( c )
         END switch
      NEXT
      cReturn := sReturn:ToString()

   ELSE
   
      cChar := cTemplate[0]
      IF cChar == 'Y' .or. cChar == 'y' .or. (nPicFunc == TransformPictures.YesNo .and. (cChar == 'L' .or. cChar == 'l'))
         cReturn := IIF( lValue, "Y", "N" )
      ELSEIF cChar == 'L' .or. cChar == 'l'
         cReturn := IIF( lValue, "T", "F" )
      ELSE
         cReturn := cChar:ToString()
      ENDIF

   ENDIF

   RETURN cReturn
   


INTERNAL FUNCTION TransformNumeric( nValue AS USUAL, cPicture AS STRING ) AS STRING
   //
   // Note: A,N,X,L and Y template chars are treated as normal letters in VO for numeric pictures
   //
   LOCAL cOriginalTemplate AS STRING
   LOCAL nPicFunc          AS TransformPictures
   LOCAL sb                AS StringBuilder
   LOCAL cTemplate         AS STRING
   LOCAL cReturn           AS STRING
   LOCAL lWhole            AS LOGIC
   LOCAL nWhole, nDecimal	AS INT
   LOCAL cThous, cDec       AS Char
   LOCAL lForceEuropean 	AS LOGIC
   LOCAL cChar             AS Char
   LOCAL cTemp             AS STRING
   LOCAL nLen              AS INT
   LOCAL nMap		        AS INT
   LOCAL x                 AS INT
   LOCAL lIsInt64 := false         AS LOGIC
   LOCAL lIsFloat := false          AS LOGIC
   nWHole := nDecimal := 0
   lIsInt64 := UsualType(nValue) == INT64
   IF .not. lIsInt64
      nValue := (FLOAT)nValue // Cpc 2014-06-14 build 303, for backwards compatibility with previous builds were nValue was typed as FLOAT
      lIsFloat := TRUE // Could add specific support for INT32 in the future
   END IF

   cDec := (Char) (word) RuntimeState.DecimalSep
   cThous := (Char)(Word) RuntimeState.ThousandSep

   cTemplate := _TransformTemplate( cPicture, OUT nPicFunc )

   lForceEuropean := ( nPicFunc & TransformPictures.British ) == TransformPictures.British

   IF ( nPicFunc & TransformPictures.Date ) == TransformPictures.Date
      cTemplate := GetDateFormat():ToUpper():Replace('D' , '#'):Replace('M' , '#'):Replace('Y' , '#')
   ENDIF

    // when no template is provided, created one the way VO does
   IF cTemplate:Length == 0
      cTemplate := System.String{'#' , iif(nValue < 10000000000 , 10 , 20) }
      IF lIsFloat
         IF ((FLOAT)nValue):Decimals != 0
            cTemplate += "." + System.String{'9' , ((FLOAT)nValue):Decimals}
         END IF
      END IF
      cOriginalTemplate := cTemplate
   ENDIF

   cOriginalTemplate := cTemplate

   // Convert the arithmetic chars of the VO style template into the .NET format string
   lWhole := TRUE
   nLen   := cTemplate:Length - 1

   FOR x := 0 UPTO nLen
      cChar := cTemplate[x]
      DO CASE

      CASE cChar == '9' .or. cChar == '#' .or. cChar == '*' .or. cChar == '$'
         IF lWhole
            nWhole ++
         ELSE
            nDecimal ++
         ENDIF

      CASE cChar == '.'
         IF lWhole
            lWhole := FALSE
         ELSE
            nDecimal ++ // multiple dots don't make sense (although VO somehow allows them)
         END IF

      ENDCASE
   NEXT

   sb := StringBuilder{nWhole + nDecimal + 1}
   IF nWhole != 0
      sb:Append('#' , nWhole - 1)
      sb:Append('0')
   END IF
   IF .not. lWhole
      sb:Append('.')
   END IF
   IF nDecimal != 0
      sb:Append('0' , nDecimal)
   END IF
   cTemplate := sb:ToString()

   // check for overflow
   LOCAL lOverflow AS LOGIC
   LOCAL fTemp AS FLOAT
   fTemp := Round(nValue , nDecimal)
   
   DO CASE
   CASE fTemp == 0.0
   	  cTemp := Str(fTemp , nWhole , 0)
   	  lOverflow := cTemp:IndexOf('*') > -1
   CASE fTemp < 0.0
   	  IF nWhole <= 1
         lOverflow := TRUE
   	  ELSE
         fTemp := -fTemp:CastToInt64()
         cTemp := Str(fTemp , nWhole - 1 , 0)
         lOverflow := cTemp:IndexOf('*') > -1
   	  ENDIF
   OTHERWISE
   	  fTemp := fTemp:CastToInt64()
   	  cTemp := Str(fTemp , nWhole , 0)
   	  lOverflow := cTemp:IndexOf('*') > -1
   END CASE
   IF lOverflow
      cReturn := cTemplate:Replace('#' , '*'):Replace('0' , '*')
   ELSE
      IF lIsInt64
         cReturn := ((INT64)nValue):ToString( cTemplate , StringHelpers.usCulture)
      ELSE
         cReturn := ((FLOAT)nValue):Value:ToString( cTemplate , StringHelpers.usCulture)
      ENDIF
   ENDIF


   IF .not. lOverflow .and. ( nPicFunc & TransformPictures.ZeroBlank ) == TransformPictures.ZeroBlank
      IF nValue == 0
         cReturn := System.String{' ' , cReturn:Length}
      ELSEIF lIsFloat .and. Math.Abs(((FLOAT)nValue):Value) < 1.0
         x := cReturn:IndexOf(cDec)
         IF x == -1
            cReturn := System.String{' ' , cReturn:Length}
         ELSE
            IF cReturn:IndexOf("-") != -1 .and. x >= 2
               cReturn := System.String{' ' , x - 2} + "- " + cReturn:Substring(x)
            ELSE
               cReturn := System.String{' ' , x} + cReturn:Substring(x)
            ENDIF
         END IF
      END IF
   ENDIF

   // Map result string back to the original template

   nLen := cOriginalTemplate:Length - 1
   sb	:= StringBuilder{nLen + 1}
   nMap := cReturn:Length - 1

   FOR x := nLen DOWNTO 0
      cChar := cOriginalTemplate[x]
      SWITCH cChar
      CASE '9' ; CASE '#' ; CASE '*' ; CASE '$' ; CASE '.'
         IF nMap >= 0
            IF lForceEuropean .and. cChar == '.'
               sb:Insert(0 , ',')
            ELSE
               sb:Insert(0 , cReturn[nMap])
            END IF
         ELSE
            IF cChar == '*' .or. cChar == '$'
               sb:Insert(0, cChar)
            ELSE
               sb:Insert(0 , ' ')
            ENDIF
         END IF
         nMap --

      CASE ','
         IF nMap >= 0
            IF nMap == 0 .and. cReturn[nMap] == '-'
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
      IF ( nPicFunc & TransformPictures.ParenLeft ) == TransformPictures.ParenLeft
         cReturn := "(" + cReturn:Substring(1) + ")" // ugly, but so is VO here, too :)
      ELSEIF ( nPicFunc & TransformPictures.ParenRight ) == TransformPictures.ParenRight
         cTemp := cReturn:Trim()
         nLen := cReturn:Length - cTemp:Length
         IF nLen == 0
            cTemp := cTemp:Substring(1)
         ELSE
            nLen --
         END IF
         cReturn := System.String{' ' , nLen} + "(" + cTemp + ")"
      END IF
   END IF

   IF nValue > 0
      IF ( nPicFunc & TransformPictures.Credit ) == TransformPictures.Credit
         // TODO: should these be localized?
         cReturn := cReturn + " CR"
      ENDIF
   ELSEIF nValue < 0
      IF ( nPicFunc & TransformPictures.Debit ) == TransformPictures.Debit
         // TODO: should these be localized?
         cReturn := cReturn + " DB"
      ENDIF
   ENDIF

   IF ( nPicFunc & TransformPictures.Left ) == TransformPictures.Left
      cReturn := cReturn:TrimStart():PadRight( cReturn:Length )
   ENDIF

RETURN cReturn

FUNCTION Transform( nValue AS FLOAT, cPicture AS STRING ) AS STRING
RETURN TransformNumeric( nValue, cPicture)

FUNCTION Transform(cValue AS STRING, cPicture AS STRING) AS STRING
   LOCAL nPicFunc AS TransformPictures
   LOCAL cTemplate AS STRING
   LOCAL cReturn AS STRING
   LOCAL nLen AS INT

   cTemplate := _TransformTemplate( cPicture, OUT nPicFunc )

   IF ( nPicFunc & TransformPictures.Upper ) == TransformPictures.Upper
      cValue := cValue:ToUpper()
   ENDIF

   nLen := cTemplate:Length
   IF nLen == 0
      cReturn := cValue
   ELSE
      cValue := PadR(cValue,nLen)
      local sb as StringBuilder
	  local nPos := 0  as int
	  sb := StringBuilder{}
      FOREACH cChar as char in cTemplate
         SWITCH Char.ToUpper(cChar)
         CASE '!'
            sb:Append(Char.ToUpper(cValue[nPos]))
            nPos++
         CASE 'A' ; CASE 'N' ; case 'X' ; case '9'; case '#'
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

FUNCTION Transform( uValue AS USUAL, cPicture AS STRING ) AS STRING
  LOCAL ret AS USUAL

   IF uValue:IsNumeric
      ret := TransformNumeric( uValue, cPicture )
   ELSEIF uValue:IsDate
      ret := Transform( (DATE) uValue, cPicture )
   ELSEIF uValue:IsLogic
      ret := Transform( (LOGIC) uValue, cPicture )
   ELSEIF uValue:IsString
      ret := Transform( (STRING) uValue, cPicture )
   ELSEIF uValue:IsNil
      ret := ""
   elseif uValue:IsObject && IsMethod( uValue, #Transform )
      ret := Send( uValue, "Transform" , cPicture )
   ELSE
      BREAK Error.ArgumentError( __ENTITY__, nameof(uValue),  "Invalid argument type"  ,1)
   ENDIF

   RETURN ret


//Todo: Implement unformat




INTERNAL FUNCTION UnformatSplitPict(cSayPicture AS STRING, cPic OUT STRING, cFunc OUT STRING) AS LOGIC
	LOCAL iFuncLen  AS INT
	LOCAL lRInsert	:= FALSE AS LOGIC
	cPic := cFunc := ""
	IF cSayPicture:Length > 1 .and. cSayPicture[0]  == '@'
		iFuncLen := INT(At(" ",cSayPicture))
		IFuncLen -= 2
		IF iFuncLen < 0
			iFuncLen := INT(SLen(cSayPicture))-1	// RvdH 031120
		ENDIF
		cPic  := AllTrim( SubStr(cSayPicture,iFuncLen+2) )
		cFunc := Upper( SubStr(cSayPicture,2,iFuncLen) )

		IF Instr("R",cFunc)
			lRInsert := TRUE
		ENDIF
	ENDIF
	RETURN lRInsert



FUNCTION Unformat( 	cValue	AS STRING,  cSayPicture AS STRING, cType AS STRING)	AS USUAL PASCAL
	LOCAL uRetVal		AS USUAL
	LOCAL lNullable     AS LOGIC

	IF (SLen(CType) == 2)
		lNullable := (Right(cType, 1) == "0")
	ELSE
		lNullable := FALSE
	ENDIF

	cType		:= Upper(Left(cType, 1))
	cSayPicture := Upper( cSayPicture )
	SWITCH cType
	CASE "N"
		uRetVal := __UnformatN(	cValue, cSayPicture, lNullable)

	CASE "C"
		uRetVal := __UnformatC(cValue, cSayPicture, lNullable)

	CASE "L"
		uRetVal := __UnformatL(	cValue, cSayPicture, lNullable)

	CASE "D"
		uRetVal := __UnformatD(	cValue, cSayPicture, lNullable)
	OTHERWISE
		uRetVal := NIL
	END SWITCH

	RETURN uRetVal


INTERNAL FUNCTION __UnformatC(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS STRING PASCAL

	LOCAL lRInsert			:= FALSE AS LOGIC
	LOCAL cFunc 			:= ""	AS STRING
	LOCAL wValueLen 		:= 0	AS DWORD
	LOCAL wPictureLen		:= 0	AS DWORD
	LOCAL w 				:= 0	AS DWORD
	LOCAL wValueIdx 		:= 0	AS DWORD
	LOCAL wPictureIdx		:= 0	AS DWORD
	LOCAL cRet				:= ""	AS STRING
	LOCAL cPic				:= ""   AS STRING
	LOCAL cString			as STRING
	wPictureIdx := 1
	UnformatSplitPict(cSayPicture, REF cPic, REF cFunc)
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
			IF "ANX9#YL!":IndexOf(cSayPicture[(int) w-1]) < 0
				LOOP
			ENDIF
			sb:Append(cValue[(int) wValueIdx-1])
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


INTERNAL FUNCTION __UnformatD(cValue AS STRING, cSayPicture AS STRING, lNullable   AS LOGIC)    AS DATE PASCAL
	LOCAL cFunc 			AS STRING
	LOCAL cTempValue		AS STRING
	LOCAL dRet				AS DATE

	UnformatSplitPict(cSayPicture, OUT cTempValue, OUT cFunc)
	cTempValue := AllTrim(cValue)

	LOCAL cFormat			AS STRING
	IF cFunc:Contains("E")
		cFormat := iif ( SetCentury(), "dd/mm/yyyy","dd/mm/yy")
	ELSE
		cFormat := GetDateFormat()
	ENDIF
	dRet 	:= CToD(cTempValue, cFormat)
	IF Empty(dRet) .AND. lNullable
		dRet := NULL_DATE
	ENDIF

	RETURN dRet


INTERNAL FUNCTION __UnformatN(cValue AS STRING, cSayPicture AS STRING, lNullable   AS LOGIC)    AS USUAL PASCAL
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

	UnformatSplitPict(cSayPicture, OUT cPic, OUT cFunc)

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
		cDecimal := CHR( SetDecimalSep() )

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


INTERNAL FUNCTION __UnformatL(cValue AS STRING, cSayPicture AS STRING, lNullable AS LOGIC) AS LOGIC PASCAL
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
	UnformatSplitPict(cSayPicture, REF cPic, REF cFunc)
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



