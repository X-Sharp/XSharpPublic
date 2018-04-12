using XSharp
using System.Text
using System.Globalization

/// <summary>
/// </summary>
/// <param name="x"></param>
/// <param name="nType"></param>
/// <returns>
/// </returns>
function Any2Usual(x as usual,nType as dword) as usual
	/// THROW NotImplementedException{}
	return NIL   
	
	
function AsHexString(uValue as usual) as string
	local result as string
	if IsString(uValue)
		result := "0x"+c2Hex( (string) uValue)
	elseif IsNumeric(uValue)
		result := String.Format("{0:X8}", (int64) uValue)
	else
		result := ""
	endif
	return result
	
	/// <summary>
	/// Convert a value to a right-padded string.
	/// </summary>
	/// <param name="u"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
function AsPadr(u as usual,dwLen as dword) as string
	/// THROW NotImplementedException{}
	return String.Empty   
	
	
	/// <summary>
	/// Convert a value to a string.
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
function AsString(u as usual) as string
	local result as string
	do case
		case u:IsString
			result := (string) u
		case u:IsNumeric
			result := Ntrim(u)
		case u:IsSymbol
			result := Symbol2String( (symbol) u)
		case u:IsDate
			result := DTOC( (date) u)
		otherwise
			result := u:ToString()
	endcase
	return result
	
	
	/// <summary>
	/// Convert a string or a Psz to a Symbol.
	/// </summary>
	/// <param name="u">The Usual holding a string or Psz</param>
	/// <returns>
	/// The Symbol representing the given string or Psz.
	/// </returns>
function AsSymbol(u as usual) as symbol
	return symbol{(string)u}   
	
	
	
	
	/// <summary>
	/// Create a descending order key value.
	/// </summary>
	/// <param name="uValue"></param>
	/// <returns>
	/// </returns>
function Descend(uValue as usual) as usual
	/// THROW NotImplementedException{}
	return NIL   
	
	/// <summary>
	/// </summary>
	/// <param name="uValue"></param>
	/// <returns>
	/// </returns>
function DescendA(uValue as usual) as usual
	/// THROW NotImplementedException{}
	return NIL   
	
	
	
	
	/// <summary>
	/// Convert a numeric expression to a left-trimmed string.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function NTrim(n as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   
	
	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
unsafe function Psz2Float(ptrBuff as ptr,nLen as int,nDec as int) as float
	throw NotImplementedException{}
	return 0   
	
	/// <summary>
	/// Convert a Psz to a Usual with a Psz tag.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
unsafe function Psz2Usual(ptrSource as ptr) as usual
	throw NotImplementedException{}
	return NIL   
	
	/// <summary>
	/// Pad character, numeric, and Date values with fill characters on the right.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nLen"></param>
	/// <param name="cPad"></param>
	/// <returns>
	/// </returns>
function Pad( uValue as usual, nLength as int ) as string
	return PadR( uValue, nLength, " " )
	
function Pad( uValue as usual, nLength as dword ) as string
	return PadR( uValue, (int) nLength, " " )
	
function Pad( uValue as usual, nLength as usual ) as string
	return PadR( uValue, (int) nLength, " " )
	
function Pad( uValue as usual, nLength as int, cFillStr as string ) as string
	return PadR( uValue, nLength, cFillStr )
	
function Pad( uValue as usual, nLength as dword, cFillStr as string ) as string
	return PadR( uValue, (int) nLength, cFillStr )
	
function Pad( uValue as usual, nLength as usual, cFillStr as string ) as string
	return PadR( uValue, (int) nLength, cFillStr )
	
	
	
	/// <summary>
	/// Pad character, numeric, and Date values with fill characters on the right.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nLen"></param>
	/// <param name="cPad"></param>
	/// <returns>
	/// </returns>
	
	
	/// <summary>
	/// Pad character, numeric, and Date values with fill characters on both the right and left.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nLen"></param>
	/// <param name="cPad"></param>
	/// <returns>
	/// </returns>
function PadC( uValue as usual, nLength as int ) as string
	return PadC( uValue, nLength, " " )
	
function PadC( uValue as usual, nLength as dword ) as string
	return PadC( uValue, (int) nLength, " " )
	
function PadC( uValue as usual, nLength as usual ) as string
	return PadC( uValue, (int) nLength, " " )
	
function PadC( uValue as usual, nLength as int, cFillStr as string ) as string
	if String.IsNullOrEmpty( cFillStr )
		cFillStr := " "
	endif
	
	local ret     as string
	local retlen  as int
	
	if uValue:isFloat
		ret := _Float2String( (float)uValue, RuntimeState.Digits ):TrimStart( <char>{ ' ' } )
	else
		ret := uValue:ToString()
	endif
	retlen := ret:Length
	
	if retlen > nLength
		ret := ret:Remove( nLength )
	else
		ret := ret:PadLeft( ( nLength - retlen ) / 2, cFillStr[0] ):PadRight( nLength, cFillStr[0] )
	endif
	
	return ret
	
function PadC( uValue as usual, nLength as dword, cFillStr as string ) as string
	return PadC( uValue, (int) nLength, cFillStr )
	
function PadC( uValue as usual, nLength as usual, cFillStr as string ) as string
	return PadC( uValue, (int) nLength, cFillStr )
	
	
	
	/// <summary>
	/// Pad character, numeric, and Date values with fill characters on the left.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nLen"></param>
	/// <param name="cPad"></param>
	/// <returns>
	/// </returns>
function PadL( uValue as usual, nLength as int ) as string
	return PadL( uValue, nLength, " " )
	
function PadL( uValue as usual, nLength as dword ) as string
	return PadL( uValue, (int) nLength, " " )
	
function PadL( uValue as usual, nLength as usual ) as string
	return PadL( uValue, (int) nLength, " " )
	
function PadL( uValue as usual, nLength as int, cFillStr as string ) as string
	if String.IsNullOrEmpty( cFillStr )
		cFillStr := " "
	endif
	local ret as string
	if uValue:IsFLoat
		ret := _Float2String( (float)uValue, RuntimeState.Digits ):TrimStart( <char>{ ' ' } )
	else
		ret := uValue:ToString()
	endif
	return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cFillStr[0] ) )
	
function PadL( uValue as usual, nLength as dword, cFillStr as string ) as string
	return PadL( uValue, (int) nLength, cFillStr )
	
function PadL( uValue as usual, nLength as usual, cFillStr as string ) as string
	return PadL( uValue, (int) nLength, cFillStr )
	
	
	/// <summary>
	/// Pad character, numeric, and Date values with fill characters on the right.
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="nLen"></param>
	/// <param name="cPad"></param>
	/// <returns>
	/// </returns>
	
function PadR( uValue as usual, nLength as int ) as string
	return PadR( uValue, nLength, " " )
	
function PadR( uValue as usual, nLength as dword ) as string
	return PadR( uValue, (int) nLength, " " )
	
function PadR( uValue as usual, nLength as usual ) as string
	return PadR( uValue, (int) nLength, " " )
	
function PadR( uValue as usual, nLength as dword, cFillStr as string ) as string
	return PadR( uValue, (int) nLength, cFillStr )
	
function PadR( uValue as usual, nLength as usual, cFillStr as string ) as string
	return PadR( uValue, (int) nLength, cFillStr )
	
function PadR( uValue as usual, nLength as int, cFillStr as string ) as string
	if String.IsNullOrEmpty( cFillStr )
		cFillStr := " "
	endif
	local ret as string
	// See comment on PadL()
	if uValue:IsFloat 
		ret := _Float2String( (float)uValue, RuntimeState.Digits ):TrimStart( <char>{ ' ' } )
	else
		ret := uValue:ToString()
	endif
	return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cFillStr[0] ) )
	
	
	
	/// <summary>
	/// Convert a numeric expression to a string.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
function Str(n as usual,nLen as usual,nDec as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


internal function _PadZero(cValue as STRING) AS STRING
	local iLen := 	cValue:Length as int
	Return cValue:TrimStart():PadLeft((int) iLen, '0')

	
	/// <summary>
	/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="iLen"></param>
	/// <param name="iDec"></param>
	/// <returns>
	/// </returns>
function StrZero(n as usual,iLen as int,iDec as int) as string
	IF ! ( n:IsNumeric )
      BREAK DataTypeError( __ENTITY__, nameof(n),1)
    ENDIF
	local cValue := Str3(n, (DWORD) iLen, (DWORD) iDec) as string
	return _PadZero(cValue)
	
/// <summary>
/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
/// </summary>
/// <param name="n"></param>
/// <param name="iLen"></param>
/// <returns>
/// </returns>
function StrZero(n as usual,iLen as int) as string
	IF ! ( n:IsNumeric )
      break DataTypeError( __ENTITY__, nameof(n),1)
	endif
	local cValue := Str2(n, (DWORD) iLen) as string
	return _padZero(cValue)
	

/// <summary>
/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function StrZero(n as usual) as string
	IF ! ( n:IsNumeric )
      BREAK DataTypeError( __ENTITY__, nameof(n),1)
    ENDIF
	local cValue := Str1(n) as string
	return _PadZero(cValue)
	
	/// <summary>
	/// Convert a number to a word.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
function ToWord(n as usual) as dword
	return (dword) n
	
	
/// <summary>
/// Convert an integer expression to a Psz.
/// </summary>
/// <param name="l"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param> 
/// <returns>
/// </returns>
function StrInt(l as long,dwLen as dword,dwDec as dword) as String
	return Str3( l, dwLen, dwDec) 

	/// <summary>
	/// Convert a long integer expression to a Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
function StrLong(l as long,dwLen as dword,dwDec as dword) as String
	return StrInt(l, dwLen, dwDec)
	
	/// <summary>
	/// Convert a Float expression to a Psz.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
function StrFloat(flSource as float,dwLen as dword,dwDec as dword) as String
	return Str3( flSource, dwLen, dwDec ) 
	
	#define MAXDIGITS               30
	#define MAXDECIMALS             15	
	
internal function _Float2String(nFloat as Float, nDigits as int) as string
return "" 
//	local dwLen as dword
//	local dwDec as dword
//	if RuntimeState.Fixed
//		dwDec := RuntimeState.Decimals
//	else
//		dwDec := nFloat:Decimals
//	endif
//	if !RuntimeState.DigitFixed .and. nFloat:Digits != 0
//		dwLen := nFloat:Digits
//	elseif nDigits == -1
//		dwLen := MAXDIGITS
//	else 
//		dwLen := (dword) nDigits
//	endif
//	if dwDec > 0 
//		dwLen += dwDec +1
//		dwLen := Math.Min(dwLen, MAXDIGITS)
//	endif
//	return _FLoatHelper(nFLoat, dwLen, DwDec, nDigits)

internal function _FLoatHelper(nFLoat as Float, dwLen as dword, dwDec as dword, nDigits as int) as string
	return ""	
/*
	
internal function _FLoatHelper(nFLoat as Float, dwLen as dword, dwDec as dword, nDigits as int) as string
	local nDotPos      as dword
	local nSize        as dword
	local uiDecDisplay as dword
	local ret          as string
	local sb           as StringBuilder
	
	uiDecDisplay := uiDec
	
	uiDec := Math.Min( MAXDECIMALS, uiDec )
	
	if uiLen > (dword) nDigits
		nSize := uiLen
	else
		nSize := uiLen + uiDec
		
		if uiDec != 0
			nSize++
		endif
	endif
	
	ret     := _Double2String( nFloat:Value, nSize, uiDec )
	nDotPos := (dword) ret:IndexOf( '.' ) + 1
	nSize   := (dword) ret:Length
	
	if nDotPos != 0
			if uiDec != 0
					//nTemp := nDotPos
					nDotPos--
					sb := StringBuilder{ ret }
					sb[(int) nDotPos] := '.'
					
					nDotPos += uiDec
					
					do while nDotPos > nSize
						if (int) nSize >= sb:Length
							sb:Append( '0' )
						else
							sb[(int) nSize] := '0'
						endif
						nSize++
					enddo
					ret := sb:ToString()
				nDotPos++
			else
				nSize := nDotPos - 1
				nDotPos := nSize
		endif
	else
		nDotPos := nSize
	endif
	// this happens when the # of decimal before the dot is larger than the result size
	if nDotPos > uiLen
		return string{ '*', (int) uiLen }
	endif
	
	nDotPos := (dword) ret:IndexOf( '.' ) + 1
	
	ret := FloatAdjust( ret, uiLen, uiDecDisplay )
	
	if  ret:IndexOf( '.' ) + 1 < nDotPos
		return string{ '*', (int) uiLen }
	endif
	
	if RuntimeState.Science
		ret := _Numeric2Scientific( ret, iStateDigits )
	else
		if RuntimeState.DecimalSep != (dword) '.' && ret:IndexOf( '.' ) > -1
			ret := ret:Replace( '.', (char) State.DecimalSep )
		endif
	endif
	
	return ret
	
	
internal function _Double2String( d as Double, nSize as dword, nDec as dword )
	local ret as string
	local iFormat as NumberFormatInfo
	iFormat := CultureInfo.GetCultureInfo("en-us"):NumberFormat
	if nDec > 0 
		local sb  as StringBuilder
		sb := StringBuilder{"0."}
		sb:Append('0', (int) nDec)
		ret := d:ToString(sb:ToString(), iFormat)
	else
		ret := d:ToString("0", iFormat)
	endif
	return ret


#define MIN_E  2

INTERNAL FUNCTION _Numeric2Scientific( cRet AS STRING, iStateDigits AS INT ) AS STRING
   LOCAL uiPos AS DWORD
   LOCAL uiDec AS DWORD
   LOCAL uiFloatPos AS DWORD
   LOCAL i AS DWORD
   LOCAL uiExpVal AS DWORD
   LOCAL uiDecPoint AS DWORD
   LOCAL lpbFloat AS StringBuilder 
   LOCAL uiTail AS DWORD
   LOCAL abLong AS STRING 
   LOCAL uiSize, uiLen AS DWORD
   LOCAL bNew AS Char
   LOCAL uiRound AS DWORD
   LOCAL fRound AS LOGIC
   LOCAL fDigitFound AS LOGIC
   LOCAL bSign AS Char
   LOCAL fSign AS LOGIC
   LOCAL cTemp AS StringBuilder //BYTE    cTemp[MAXDIGITS]
   LOCAL bSep AS DWORD

   LOCAL c AS Char
   LOCAL x, y, z AS INT

   uiSize := uiLen := (DWORD) pszRet:Length
   lpbFloat := StringBuilder{ pszRet } //lpbFloat = pszRet

   i := 0

   DO WHILE i < uiSize
      c := pszRet[(INT)i]
      IF c == 0x20
         uiLen--
         lpbFloat:Remove( 0, 1 )
      ELSEIF c == '-'
         uiLen--
         lpbFloat:Remove( 0, 1 )
         fSign := TRUE
      ELSEIF c == '.'
         uiDecPoint := i
      ENDIF

      IF c == bSep
         uiDecPoint := i
      ENDIF

      i++
   ENDDO

   IF uiLen == 0
      RETURN pszRet
   ENDIF

   cTemp := StringBuilder{ MAXDIGITS * 2 }
   cTemp:Append( 0, MAXDIGITS * 2 )           //	memset(cTemp, 0, MAXDIGITS)

   uiDec      := uiSize - uiDecPoint - 1
   uiDecPoint := uiLen  - uiDec - 1
   uiExpVal   := uiDecPoint - 1

   IF fSign
      cTemp[0] := '-'
      uiPos := 1
      uiLen++
   ELSE
      uiPos := 0
   ENDIF

   IF uiSize < iStateDigits
      uiSize := (DWORD) iStateDigits
   ENDIF

   cTemp[(INT)uiPos] := lpbFloat[0]

   uiPos++

   IF lpbFloat[0] == '0'
      uiExpVal  := 1
      bSign     := '-'
      uiLen    --
      lpbFloat:Remove( 0, Math.Min( 2, lpbFloat:Length ) ) // lpbFloat += 2

      DO WHILE lpbFloat:Length > 0 && uiLen != 0
         IF lpbFloat[0] == '0'
            lpbFloat:Remove( 0, 1 )
            uiLen--
            uiExpVal++
         ELSE
            fDigitFound := TRUE
            EXIT
         ENDIF
      ENDDO

      IF ! fDigitFound
         RETURN pszRet
      ENDIF

      cTemp[(INT)uiPos-1] := lpbFloat[0]
      uiDec := 0
      uiFloatPos := 1
      uiDecPoint := uiLen - 1
   ELSE
      uiFloatPos := 1
      bSign := '+'
   ENDIF

   cTemp[(INT)uiPos] := '.'

   uiPos++

   IF uiDec != 0
      x := (INT) uiPos
      y := (INT) uiFloatPos
      FOR z := 1 UPTO uiLen
         IF x < cTemp:Length && y < lpbFloat:Length
            cTemp[x++] := lpbFloat[y++]
         ELSE
            EXIT
         ENDIF
      NEXT

      uiPos := uiDecPoint + 1

      x := (INT) uiPos
      y := (INT) uiDecPoint + 1
      z := 1
      IF x < 0
         y -= x
         z -= x
         x := 0
      ENDIF

      FOR z := z UPTO uiDec
         IF x < cTemp:Length && y < lpbFloat:Length
          cTemp[x++] := lpbFloat[y++]
         ELSE
            EXIT
         ENDIF
      NEXT

      uiPos += uiDec
   ELSE
      // memcpy( &cTemp[uiPos], &lpbFloat[uiFloatPos], uiLen)
      x := (INT) uiPos
      y := (INT) uiFloatPos
      FOR z := 1 UPTO uiLen
         IF x < cTemp:Length && y < lpbFloat:Length
            cTemp[x++] := lpbFloat[y++]
         ELSE
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF (uiLen + MIN_E + 2) > uiSize
      //  Round up if necessary
      uiPos := uiSize - 2 - MIN_E
      uiTail := MIN_E

      IF cTemp[(INT) uiPos] > '4'
         fRound  := TRUE
         uiRound := uiPos - 1
      ELSE
         fRound := FALSE
      ENDIF

      DO WHILE fRound && uiRound != 0
         bNew := cTemp[(INT) uiRound]

         IF bNew != (BYTE) '.' 
            bNew += 1

            IF bNew <= '9'
               fRound := FALSE
            ELSE
               bNew := '0'
            ENDIF

            cTemp[(INT) uiRound] := bNew
            uiRound--
         ELSE
            fRound := FALSE
            cTemp[0] := '1'
            uiExpVal += 1
         ENDIF
      ENDDO
   ELSE
      //  All digits fit
      uiPos  := uiDecPoint + uiDec + 1
      uiTail := uiSize - (uiPos +2)
   ENDIF

   cTemp[(INT) uiPos]   := 'e'
   cTemp[(INT) uiPos+1] := bSign

   abLong := _StrLong( (INT) uiExpVal, uiTail, 0 )


   abLong := abLong:Replace( ' ', '0' )

   x := (INT) uiPos
   y := 0
   TRY
      FOR z := 1 UPTO uiTail
         cTemp[x++] := abLong[y++]
      NEXT
   CATCH
   END TRY

   RETURN cTemp:ToString( 0, Math.Min( cTemp:Length, (INT) ( uiPos + uiTail ) ) )
*/

/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Str1(f AS Float) AS STRING
   RETURN _FLoat2String( f, RuntimeState.Digits )
 

/// <summary>
/// Convert a numeric expression to a string of a specified length.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION Str2(f AS Float,dwLen AS DWORD) AS STRING
  IF dwLen == 0
      dwLen := (DWORD) RuntimeState.Digits
   ELSEIF dwLen  != UInt32.MaxValue
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF
   RETURN __VOStr3( f, dwLen, (DWORD) f:Decimals )
 

/// <summary>
/// Convert a numeric expression to a string of specific length and decimal places.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION Str3(f AS Float,dwLen AS DWORD,dwDec AS DWORD) AS STRING
   IF dwLen == 0
      dwLen := (DWORD) RuntimeState.Digits
   ENDIF

   IF dwDec == UInt32.MaxValue
      dwDec := (DWORD) f:Decimals
   ENDIF

   IF dwDec > 0 && dwLen != UInt32.MaxValue && ( dwLen < ( dwDec + 2 ) )
      RETURN STRING{ '*', (INT) dwLen }
   ELSE
      RETURN __VOStr3( f, dwLen, dwDec )
   ENDIF



/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToFloat(c AS STRING,dwRadix AS DWORD) AS Float
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToLong(c AS STRING,dwRadix AS DWORD) AS Float
	/// THROW NotImplementedException{}
RETURN 0   



INTERNAL FUNCTION __VOStr3( voflSource AS FLOAT, uiLen AS DWORD, uiDec AS DWORD ) AS STRING
   LOCAL uiLenNew       AS DWORD
   LOCAL fSignificant := false   AS LOGIC
   LOCAL fSign        := false   AS LOGIC
   LOCAL fBlank := TRUE AS LOGIC
   LOCAL ret            AS STRING
   LOCAL sb             AS StringBuilder
   LOCAL x          := 0    AS INT

   IF uiLen == UInt32.MaxValue
       fSignificant := TRUE
      uiLen := MAXDIGITS - uiDec - 1
    ENDIF

   uiLenNew := uiLen

   IF uiDec != 0
      uiLenNew := uiLen + uiDec + 1
   ENDIF

   IF voflSource:Decimals > uiDec
      voflSource := __MyRound( voflSource, uiDec )
   ENDIF

   ret := _FLoatHelper( voflSource, uiLenNew, uiDec, RuntimeState.Digits )

   IF ret[0] == '*'
      IF ret:Length > uiLen
         ret := ret:Substring( 0, (INT) uiLen )
      ENDIF
   ELSE
      sb := StringBuilder{ ret }

      IF fSignificant
         DO WHILE sb[0] == ' '
            sb:Remove( 0, 1 )
         ENDDO
      ELSE
         DO WHILE fBlank && ( uiLenNew > uiLen )
            DO CASE
            CASE sb[x] == ' '
               sb:Remove( 0, 1 )
               uiLenNew--

            CASE sb[x] == '-'
               fSign := TRUE
               sb:Remove( 0, 1 )
               uiLenNew--

            CASE sb[x] == '+'
               sb:Remove( 0, 1 )
               uiLenNew--

            OTHERWISE
               fBlank := FALSE
            ENDCASE
         ENDDO
      ENDIF

      IF fSign
         IF ! fBlank
            sb:Insert( 0, '-' )
         ELSE
            // We cannot simply insert the '-' chat because it will
            // replace the first char in the string
            //sb[0] := '-'
            // therefore try again when the # of decimals > 0
            // or else return all asterisks
            IF (uiDec > 0)
               RETURN __VOStr3(voflSource , uiLen , uiDec -1)
            ELSE
               RETURN StringBuilder{}:Insert( 0, "*", (INT) uiLen ):ToString()
            ENDIF
         ENDIF
      ENDIF

      ret := sb:ToString( 0, Math.Min( (INT) uiLen, sb:Length ) )

   ENDIF

   return ret


INTERNAL FUNCTION __MyRound( xd AS FLOAT, nDec AS DWORD ) AS Double
   LOCAL nDotPos  AS INT           // Position of Dot in the string
   LOCAL nCheck   AS INT           // Last digit to check
   LOCAL bTemp    AS Char          // character to test
   LOCAL fRoundUp := false AS LOGIC
   LOCAL fNeg     := false AS LOGIC
   LOCAL cTemp   AS STRING
   LOCAL ret      AS Double

   fNeg := xd < 0

   cTemp := _Double2String( xd, MAXDIGITS, nDec )

   IF ! String.IsNullOrEmpty( cTemp )
      nDotPos  := cTemp:IndexOf( '.' ) + 1

      IF nDotPos > 0
         nCheck := nDotPos + (INT) nDec

         IF nCheck < MAXDIGITS && nCheck < cTemp:Length
            bTemp := cTemp[nCheck]

            fRoundUp := ( bTemp >= '5' ) && ( bTemp <= '9' )
            cTemp := cTemp:Substring( 0, nCheck ) // cTemp[nCheck] := '\0'
         ENDIF
      ENDIF
   ELSE
      cTemp := "0"   
   ENDIF

   ret := Convert.ToDouble(cTemp,CultureInfo.GetCultureInfo("en-us"):NumberFormat)

   IF fRoundUp

      LOCAL roundVal  AS Double

      DO CASE
         CASE nDec ==  0 ; roundVal := 1.000000000000000E-0
         CASE nDec ==  1 ; roundVal := 1.000000000000000E-1
         CASE nDec ==  2 ; roundVal := 1.000000000000000E-2
         CASE nDec ==  3 ; roundVal := 1.000000000000000E-3
         CASE nDec ==  4 ; roundVal := 1.000000000000000E-4
         CASE nDec ==  5 ; roundVal := 1.000000000000000E-5
         CASE nDec ==  6 ; roundVal := 1.000000000000000E-6
         CASE nDec ==  7 ; roundVal := 1.000000000000000E-7
         CASE nDec ==  8 ; roundVal := 1.000000000000000E-8
         CASE nDec ==  9 ; roundVal := 1.000000000000000E-9
         CASE nDec == 10 ; roundVal := 1.000000000000000E-10
         CASE nDec == 11 ; roundVal := 1.000000000000000E-11
         CASE nDec == 12 ; roundVal := 1.000000000000000E-12
         CASE nDec == 13 ; roundVal := 1.000000000000000E-13
         CASE nDec == 14 ; roundVal := 1.000000000000000E-14
         case nDec == 15 ; roundVal := 1.000000000000000E-15
		 otherwise ; roundval := 0.0
      ENDCASE

      IF fNeg
         ret -= roundVal
      ELSE
         ret += roundVal
      ENDIF
   ENDIF

   RETURN ret


internal function _Double2String( d as Double, nSize as dword, nDec as dword )
	return ""