//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp
USING System.Text
USING System.Globalization
USING System.Collections.Generic

#define MAXDIGITS               30
#define MAXDECIMALS             15	

INTERNAL STATIC CLASS ConversionHelpers
	STATIC INTERNAL usCulture AS CultureInfo
	STATIC PRIVATE formatStrings AS Dictionary<INT, STRING>
	STATIC CONSTRUCTOR
		usCulture := CultureInfo{"en-US"} 
		formatStrings := Dictionary<INT, STRING>{}

	STATIC METHOD GetFormatString(nLen AS INT, nDec AS INT) AS STRING
		LOCAL nKey AS INT
		LOCAL cFormat AS STRING
		nKey := nLen * 100 + nDec
		IF formatStrings:ContainsKey(nKey)
			RETURN formatStrings[nKey]
		ENDIF
		IF nDec != 0
			cFormat := "0."
			cFormat := cFormat:PadRight(nDec+2, '0')	// 2 extra for the 0 + Dot
		ELSE
			cFormat := "0"
		ENDIF
		cFormat := cFormat:PadLeft(nLen, '#')
		cFormat := "{0," + nLen:ToString()+":"+cFormat+"}"
		formatStrings:Add(nKey, cFormat)
		RETURN cFormat

	STATIC METHOD FormatNumber(n AS REAL8, nLen AS INT, nDec AS INT) AS STRING
		LOCAL cFormat AS STRING
		LOCAL result AS STRING
		cFormat := GetFormatString(nLen, nDec)
		result := String.Format(usCulture, cFormat, n)
		IF result:Length > nLen
			result := Replicate("*", (DWORD) nLen)
		ENDIF
		RETURN result

	STATIC METHOD FormatNumber(n AS INT64, nLen AS INT, nDec AS INT) AS STRING
		LOCAL cFormat AS STRING
		LOCAL result AS STRING
		cFormat := GetFormatString(nLen, 0)
		result := String.Format(usCulture, cFormat, n)
		IF result:Length > nLen
			result := Replicate("*", (DWORD) nLen)
		ENDIF
		RETURN result


	STATIC METHOD AdjustDecimalSeparator(cString AS STRING) AS STRING
		IF cString:IndexOf(".") >= 0 
			VAR wSep   := SetDecimalSep()
			IF wSep != 46
				cString := cString:Replace('.', (char) wSep)
			ENDIF
		ENDIF
		RETURN cString

END CLASS

	/// <summary>
	/// Convert a value to a hexadecimal string.
	/// </summary>
	/// <param name="uValue">A string or Numeric value.</param>
	/// <returns>A string with the hex representation of the value
	/// </returns>
FUNCTION AsHexString(uValue AS USUAL) AS STRING
	LOCAL result AS STRING
	IF IsString(uValue)
		result := "0x"+c2Hex( (STRING) uValue)
	ELSEIF IsNumeric(uValue)
		result := String.Format("{0:X8}", (INT64) uValue)
	ELSE
		result := ""
	ENDIF
	RETURN result
	
	/// <summary>
	/// Convert a value to a right-padded string.
	/// </summary>
	/// <param name="u">The value to be converted.</param>
	/// <param name="dwLen">The length of the padded string.</param>
	/// <returns>A right-padded string of length dwLen containing the converted value.
	/// </returns>
FUNCTION AsPadr(u AS USUAL,dwLen AS DWORD) AS STRING
	RETURN PadR(AsString(u), dwLen)
	

/// <exclude />
FUNCTION _AsString(u AS USUAL) AS STRING
	RETURN	 AsString(u)

	
/// <summary>
/// Convert a value to a string.
/// </summary>
/// <param name="u">The value to be converted.</param>
/// <returns>A string containing the converted value.  Please note that this is NOT the same as the .Net ToString(). AsString() tries to emulate the VO Behaviour as correctly as possible.
/// </returns>
FUNCTION AsString(u AS USUAL) AS STRING
	LOCAL result AS STRING
	DO CASE
		CASE u:IsString
			result := (STRING) u
		CASE u:IsNumeric
			result := NTrim(u)
		CASE u:IsSymbol
			result := Symbol2String( (SYMBOL) u)
		CASE u:IsDate
			result := DToC( (DATE) u)
		CASE u:IsArray
			VAR aValue := (ARRAY) u
			//  {[0000000003]0x025400FC}
			IF aValue == NULL_ARRAY
				result := "{[0000000000]0x00000000}"
			ELSE
				VAR cHashCode := String.Format("{0:X8}", aValue:GetHashCode())
				result := "{["+STRING.Format("{0:D8}",aValue:Length)+"]0x"+cHashCode+"}"
			ENDIF

		CASE u:IsObject
			LOCAL oValue := u AS OBJECT
			IF oValue == NULL_OBJECT
				result := "{(0x0000)0x00000000} CLASS "
			ELSE
				VAR oType := oValue:GetType()
				VAR nSize := oType:GetFields():Length *4
				VAR cHashCode := String.Format("{0:X8}", oValue:GetHashCode())
				result := "{(0x"+String.Format("{0:X4}", nSize)+")0x"+cHashCode+"} CLASS " + oType:Name:ToUpperInvariant()
			ENDIF
		OTHERWISE
			result := u:ToString()
	ENDCASE
	RETURN result
	
	
	/// <summary>
	/// Convert a string or a Psz to a Symbol.
	/// </summary>
	/// <param name="u">The Usual holding a string or Psz</param>
	/// <returns>
	/// The Symbol representing the given string or Psz.
	/// </returns>
FUNCTION AsSymbol(u AS USUAL) AS SYMBOL
	RETURN SYMBOL{(STRING)u, TRUE}   
	
	
	/// <summary>
	/// Create a descending order key value.
	/// </summary>
	/// <param name="uValue"></param>
	/// <returns>
	/// </returns>
FUNCTION Descend(uValue AS USUAL) AS USUAL
	IF uValue:isString
		RETURN _descendingString( (STRING) uValue)
	ELSEIF uValue:IsLogic	
		RETURN ! (LOGIC) uValue
	ELSEIF uValue:IsLong
		RETURN 0 - (INT) uValue
	ELSEIF uValue:IsInt64
		RETURN 0 - (INT64) uValue
	ELSEIF uValue:IsFloat
		RETURN 0 - (FLOAT) uValue
	ELSEIF uValue:IsDate
		RETURN (DATE) (6364425 - (DWORD)(DATE) uValue )
	ENDIF
	RETURN uValue

INTERNAL FUNCTION _descendingString(s AS STRING) AS STRING
	VAR sb  := StringBuilder{s}
	VAR nlen := s:Length-1
	LOCAL i AS INT
	FOR i := 0 TO nlen
		IF sb[i] < 256
			sb[i] := (char) (255 - sb[i])
		ENDIF
	NEXT
	RETURN sb:ToString()

	
/// <summary>
/// Create a descending order key value. The parameter is also changed 
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
FUNCTION DescendA(uValue REF USUAL) AS USUAL
	uValue := Descend(uValue)
	RETURN uValue
	
	
	
/// <summary>
/// Convert a numeric expression to a left-trimmed string.
/// </summary>
/// <param name="n">A Usual with a numeric or date value</param>
/// <returns>
/// </returns>
FUNCTION NTrim(n AS USUAL) AS STRING
	LOCAL ret AS STRING
	SWITCH n:_UsualType
	CASE usualType.Int64
	CASE usualType.Long
      ret := ConversionHelpers.FormatNumber( (INT64) n, (INT) RuntimeState.Digits, 0):Trim()	
	CASE UsualType.Date
      ret := AsString( n )
    CASE UsualType.Float
	CASE UsualType.Decimal
      ret := ConversionHelpers.AdjustDecimalSeparator(_Str1(  (FLOAT) n )):Trim()
    OTHERWISE
      THROW Error.DataTypeError( __ENTITY__, nameof(n), 1, n)
   END SWITCH
   RETURN ret


/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION Pad( uValue AS USUAL, nLength AS INT, cPad := " " AS STRING ) AS STRING
	RETURN PadR( uValue, nLength, cPad )
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION Pad( uValue AS USUAL, nLength AS DWORD, cPad := " " AS STRING ) AS STRING
	RETURN PadR( uValue, (INT) nLength, cPad )
	

/// <summary>
/// Pad character, numeric, and Date values with fill characters on both the right and left.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION PadC( uValue AS USUAL, nLength AS INT, cPad := " " AS STRING ) AS STRING
	// If they send in an empty string then change to " "
	IF cPad == NULL .or. cPad :Length == 0
		cPad := " "
	ENDIF
	
	LOCAL ret     AS STRING
	LOCAL retlen  AS INT
	
	IF uValue:isNumeric
		ret := NTrim( uValue)
	ELSE
		ret := uValue:ToString()
	ENDIF
	retlen := ret:Length
	
	IF retlen > nLength
		ret := ret:Remove( nLength )
	ELSE
		ret := ret:PadLeft( ( nLength - retlen ) / 2, cPad[0] ):PadRight( nLength, cPad[0] )
	ENDIF
	
	RETURN ret

/// <summary>
/// Pad character, numeric, and Date values with fill characters on both the right and left.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION PadC( uValue AS USUAL, nLength AS DWORD, cPad := " " AS STRING ) AS STRING
	RETURN PadC( uValue, (INT) nLength, cPad )
	
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION PadL( uValue AS USUAL, nLength AS INT, cPad := " " AS STRING ) AS STRING
	// If they send in an empty string then change to " "
	IF cPad == NULL .or. cPad :Length == 0
		cPad := " "
	ENDIF
	LOCAL ret AS STRING
	IF uValue:IsNumeric
		ret := NTrim( uValue)
	ELSE
		ret := uValue:ToString()
	ENDIF
	RETURN iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cPad[0] ) )
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION PadL( uValue AS USUAL, nLength AS DWORD, cPad := " " AS STRING ) AS STRING
	RETURN PadL( uValue, (INT) nLength, cPad )
	

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cFillStr"></param>
/// <returns>
/// </returns>
FUNCTION PadR( uValue AS USUAL, nLength AS DWORD, cPad := " " AS STRING ) AS STRING
	RETURN PadR( uValue, (INT) nLength, cPad )

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
FUNCTION PadR( uValue AS USUAL, nLength AS INT, cPad := " " AS STRING ) AS STRING
	// If they send in an empty string then change to " "
	IF cPad == NULL .or. cPad:Length == 0
		cPad := " "
	ENDIF
	LOCAL ret AS STRING
	IF uValue:IsNumeric
		ret := NTrim( uValue)
	ELSE
		ret := uValue:ToString()
	ENDIF
	RETURN iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cPad[0] ) )
	
	 
	
/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="nLength"></param>
/// <param name="nDec"></param>
/// <returns>The string with always a decimal separator that matches the current SetDecimalSep() setting.</returns>
FUNCTION Str(n ,nLen ,nDec ) AS STRING CLIPPER
	IF PCount() < 1 .or. pCount() > 3
		RETURN ""
	ENDIF
	LOCAL result AS STRING
	result := _str3(n, nLen, nDec)
	RETURN ConversionHelpers.AdjustDecimalSeparator(result)


/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="nLength"></param>
/// <param name="nDec"></param>
/// <returns>The string with always a DOT as decimal separator.</returns>

FUNCTION _Str(n ,nLen ,nDec ) AS STRING CLIPPER
	IF PCount() > 0 .and. ! n:IsNumeric 
       THROW Error.DataTypeError( __ENTITY__, nameof(n),1, n, nLen, nDec)
    ENDIF
	SWITCH PCount()
	CASE 1
		RETURN _Str1( n)
	CASE 2
		IF ! nLen:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, nameof(nLen),2,n, nLen, nDec)
		ENDIF
		IF n:IsFloat
			RETURN _Str2(n, nLen)
		ELSE
			RETURN ConversionHelpers.FormatNumber((INT64) n, nLen,0)
		ENDIF
	CASE 3
		IF ! nDec:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, nameof(nDec),3,n, nLen, nDec)
		ENDIF
		IF n:IsFloat
			RETURN _Str3(n, nLen, nDec)
		ELSE
			RETURN ConversionHelpers.FormatNumber((INT64) n, nLen,nDec)
		ENDIF
	OTHERWISE
		RETURN ""
	END SWITCH
	

INTERNAL FUNCTION _PadZero(cValue AS STRING) AS STRING
	LOCAL iLen := 	cValue:Length AS INT
	RETURN cValue:TrimStart():PadLeft((INT) iLen, '0')

	
	/// <summary>
	/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
	/// </summary>
	/// <param name="n"></param>
	/// <param name="iLen"></param>
	/// <param name="iDec"></param>
	/// <returns>
	/// </returns>
FUNCTION StrZero(n AS USUAL,iLen AS INT,iDec AS INT) AS STRING
	IF ! ( n:IsNumeric )
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n, iLen, iDec)
    ENDIF
	LOCAL cValue := Str3(n, (DWORD) iLen, (DWORD) iDec) AS STRING
	RETURN _PadZero(cValue)
	
/// <summary>
/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
/// </summary>
/// <param name="n"></param>
/// <param name="iLen"></param>
/// <returns>
/// </returns>
FUNCTION StrZero(n AS USUAL,iLen AS INT) AS STRING
	IF ! ( n:IsNumeric )
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n, iLen)
	ENDIF
	LOCAL cValue := Str2(n, (DWORD) iLen) AS STRING
	RETURN _padZero(cValue)
	

/// <summary>
/// Convert a numeric expression to a string and pad it with leading zeroes instead of blanks.
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION StrZero(n AS USUAL) AS STRING
	IF ! ( n:IsNumeric )
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n)
    ENDIF
	LOCAL cValue := Str1(n) AS STRING
	RETURN _PadZero(cValue)
	
	/// <summary>
	/// Convert a number to a word.
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
FUNCTION ToWord(n AS USUAL) AS DWORD
	RETURN (DWORD) n
	
	
/// <summary>
/// Convert an integer expression to a Psz.
/// </summary>
/// <param name="l"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param> 
/// <returns>
/// </returns>
FUNCTION StrInt(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	RETURN Str3( l, dwLen, dwDec) 

	/// <summary>
	/// Convert a long integer expression to a Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
FUNCTION StrLong(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	RETURN StrInt(l, dwLen, dwDec)
	
	/// <summary>
	/// Convert a Float expression to a Psz.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
FUNCTION StrFloat(flSource AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	RETURN Str3( flSource, dwLen, dwDec ) 
	
	



/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Str1(f AS USUAL) AS STRING
	IF f:IsFloat
		RETURN ConversionHelpers.AdjustDecimalSeparator(_Str1( (FLOAT) f))
	ELSE
		RETURN ConversionHelpers.FormatNumber( (LONG) f, (INT) RuntimeState.Digits, 0):Trim()
	ENDIF
		
INTERNAL FUNCTION _Str1(f AS FLOAT) AS STRING
	VAR nDecimals := f:decimals
	VAR nDigits   := f:Digits
	VAR ltrim	  := FALSE
	IF nDecimals < 0
		nDecimals := (SHORT) RuntimeState.Decimals
	ENDIF
	IF nDigits <= 0
		nDigits := (SHORT) RuntimeState.Digits
		ltrim   := TRUE
	ENDIF
	VAR result := ConversionHelpers.FormatNumber(f, nDigits, nDecimals )
	IF (ltrim)
		result := result:TrimStart()
	ENDIF
	RETURN result
 

/// <summary>
/// Convert a numeric expression to a string of a specified length.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION Str2(f AS FLOAT,dwLen AS DWORD) AS STRING
	RETURN ConversionHelpers.AdjustDecimalSeparator(_Str2(f, dwLen))


INTERNAL FUNCTION _Str2(f AS FLOAT,dwLen AS DWORD) AS STRING
  IF dwLen == 0
      dwLen := (DWORD) RuntimeState.Digits
   ELSEIF dwLen  != UInt32.MaxValue
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF
   VAR nDecimals := f:decimals
	IF nDecimals < 0
		nDecimals := (SHORT) RuntimeState.Decimals
	ENDIF
   RETURN ConversionHelpers.FormatNumber(f, (INT) dwLen, nDecimals)
 

/// <summary>
/// Convert a numeric expression to a string of specific length and decimal places.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	RETURN ConversionHelpers.AdjustDecimalSeparator(_Str3(f, dwLen, dwDec))

FUNCTION _Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING

   IF dwLen == 0 .or. dwLen == UInt32.MaxValue
      dwLen := (DWORD) RuntimeState.Digits
   ELSE
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF

   IF dwDec == UInt32.MaxValue
      dwDec := (DWORD) f:Decimals
   ELSE
      dwDec := Math.Min( dwDec, MAXDECIMALS )
   ENDIF

   IF dwDec > 0 && dwLen != UInt32.MaxValue && ( dwLen < ( dwDec + 2 ) )
      RETURN STRING{ '*', (INT) dwLen }
   ENDIF
   RETURN ConversionHelpers.FormatNumber(f, (INT) dwLen, (INT) dwDec)


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToFloat(c AS STRING,dwRadix AS DWORD) AS FLOAT
	VAR wSep   := SetDecimalSep()
	LOCAL result AS FLOAT
	IF wSep != 46 // .
		c := c:Replace((char) wSep, '.')
	ENDIF
	TRY
		LOCAL r8 AS System.Double
		IF System.Double.TryParse(c, OUT r8)
			result := r8
		ELSE
			result := 0
		ENDIF
	CATCH
		result := 0
	END TRY
RETURN result





/// <summary>
/// Convert a string containing a numeric value to a numeric data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Val(cNumber AS STRING) AS USUAL
	cNumber := cNumber:Trim():ToUpper()
	IF String.IsNullOrEmpty(cNumber)
		RETURN 0
	ENDIF
	// find non numeric characters in cNumber and trim the field to that length
	VAR pos := 0
	VAR done := FALSE
	VAR hex  := FALSE
	VAR hasdec := FALSE
	VAR hasexp := FALSE
	VAR cDec := (CHAR) SetDecimalSep()
	IF cDec != '.'
		cNumber := cNumber:Replace(cDec, '.')
	ENDIF
	FOREACH VAR c IN cNumber
		SWITCH c
		CASE '0' 
		CASE '1'
		CASE '2'
		CASE '3'
		CASE '4'
		CASE '5'
		CASE '6'
		CASE '7'
		CASE '8'
		CASE '9'
		CASE '-'
		CASE '+'
			NOP
		CASE '.'
		CASE ','
			IF hasdec
				done := TRUE
			ELSE
				hasdec := TRUE
			ENDIF
		CASE 'A' 
		CASE 'B' 
		CASE 'C' 
		CASE 'D' 
		CASE 'F' 
			IF !hex
				done := TRUE
			ENDIF
		CASE 'E' 
			// exponentional notation only allowed if decimal separator was there
			IF hasdec
				hasexp := TRUE
			ELSE
				IF !hex
					done := TRUE
				ENDIF
			ENDIF
		CASE 'L'	// LONG result
		CASE 'U'	// DWORD result
			done := TRUE
		CASE 'X' 
			IF pos == 1
				hex := TRUE
			ELSE
				done := TRUE
			ENDIF	
		OTHERWISE
			done := TRUE
		END SWITCH
		IF done
			EXIT
		ENDIF
		pos += 1
	NEXT
	IF pos < cNumber:Length
		cNumber := cNumber:SubString(0, pos)
	ENDIF
	IF cNumber:IndexOfAny(<Char> {'.'}) > -1
		LOCAL r8Result := 0 AS REAL8
		IF cDec != '.'
			cNumber := cNumber:Replace(cDec, '.')
		ENDIF
		VAR style := NumberStyles.Number
		IF hasexp
			style |= NumberStyles.AllowExponent
		ENDIF
		IF System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, REF r8Result)
			RETURN r8Result
		ENDIF
	ELSE
		LOCAL iResult := 0 AS INT64
		LOCAL style AS NumberStyles
		IF hex
			cNumber := cNumber:Substring(2)
			style := NumberStyles.HexNumber 
		ELSE
			style := NumberStyles.Integer
		ENDIF
		IF System.Int64.TryParse(cNumber, style, ConversionHelpers.usCulture, REF iResult)
			IF iResult < Int32.MaxValue .and. iResult > int32.MinValue
				RETURN (INT) iResult
			ENDIF
			RETURN iResult
		ENDIF
	ENDIF
	RETURN 0
	
		

