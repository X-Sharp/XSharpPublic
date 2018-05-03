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
   


STATIC FUNCTION TransformNumeric( nValue AS USUAL, cPicture AS STRING ) AS STRING
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

FUNCTION Transform( uValue AS USUAL, picture AS STRING ) AS STRING
  LOCAL ret AS USUAL

   IF uValue:IsNumeric
      ret := TransformNumeric( uValue, picture )
   ELSEIF uValue:IsDate
      ret := Transform( (DATE) uValue, picture )
   ELSEIF uValue:IsLogic
      ret := Transform( (LOGIC) uValue, picture )
   ELSEIF uValue:IsString
      ret := Transform( (STRING) uValue, picture )
   ELSEIF uValue:IsNil
      ret := ""
   elseif uValue:IsObject && IsMethod( uValue, #Transform )
	// todo
      //ret := Send( expr, #Transform, picture )
	  ret := ""
   ELSE
      BREAK Error.ArgumentError( __ENTITY__, nameof(uValue),  "Invalid argument type"  ,1)
   ENDIF

   RETURN ret


//Todo: Implement unformat
function Unformat( cString, cPicture, cType) as USUAL CLIPPER
	return NIL

