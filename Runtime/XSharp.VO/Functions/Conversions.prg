//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using XSharp
using System.Text
using System.Globalization
using System.Collections.Generic

#define MAXDIGITS               30
#define MAXDECIMALS             15	

internal static class ConversionHelpers
	static internal usCulture as CultureInfo
	static private formatStrings as Dictionary<Int, String>
	static constructor
		usCulture := CultureInfo{"en-US"} 
		formatStrings := Dictionary<Int, String>{}

	static method GetFormatString(nLen as int, nDec as int) as string
		local nKey as int
		local cFormat as STRING
		nKey := nLen * 100 + nDec
		if formatStrings:ContainsKey(nKey)
			return formatStrings[nKey]
		endif
		if nDec != 0
			cFormat := "0."
			cFormat := cFormat:PadRight(nDec+2, '0')	// 2 extra for the 0 + Dot
		else
			cFormat := "0"
		ENDIF
		cFormat := cFormat:PadLeft(nLen, '#')
		cFormat := "{0," + nLen:ToString()+":"+cFormat+"}"
		formatStrings:Add(nKey, cFormat)
		return cFormat

	static method FormatNumber(n as real8, nLen as int, nDec as int) as string
		local cFormat as string
		local result as string
		cFormat := GetFormatString(nLen, nDec)
		result := String.Format(usCulture, cFormat, n)
		IF result:Length > nLen
			result := Replicate("*", (DWORD) nLen)
		endif
		return result

	static method FormatNumber(n as Int64, nLen as int, nDec as int) as string
		local cFormat as string
		local result as string
		cFormat := GetFormatString(nLen, 0)
		result := String.Format(usCulture, cFormat, n)
		IF result:Length > nLen
			result := Replicate("*", (DWORD) nLen)
		endif
		return result


	STATIC METHOD AdjustDecimalSeparator(cString AS STRING) AS STRING
		if cString:IndexOf(".") >= 0 
			var wSep   := SetDecimalSep()
			if wSep != 46
				cString := cString:Replace('.', (char) wSep)
			endif
		endif
		return cString

end class

	/// <summary>
	/// Convert a value to a hexadecimal string.
	/// </summary>
	/// <param name="uValue">A string or Numeric value.</param>
	/// <returns>A string with the hex representation of the value
	/// </returns>
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
	/// <param name="u">The value to be converted.</param>
	/// <param name="dwLen">The length of the padded string.</param>
	/// <returns>A right-padded string of length dwLen containing the converted value.
	/// </returns>
function AsPadr(u as usual,dwLen as dword) as string
	return PadR(AsString(u), dwLen)
	

/// <exclude />
function _AsString(u as Usual) as string
	return	 AsString(u)

	
/// <summary>
/// Convert a value to a string.
/// </summary>
/// <param name="u">The value to be converted.</param>
/// <returns>A string containing the converted value.  Please note that this is NOT the same as the .Net ToString(). AsString() tries to emulate the VO Behaviour as correctly as possible.
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
		case u:IsArray
			VAR aValue := (ARRAY) u
			//  {[0000000003]0x025400FC}
			IF aValue == NULL_ARRAY
				result := "{[0000000000]0x00000000}"
			ELSE
				var cHashCode := String.Format("{0:X8}", aValue:GetHashCode())
				result := "{["+STRING.Format("{0:D8}",aValue:Length)+"]0x"+cHashCode+"}"
			ENDIF

		case u:IsObject
			local oValue := u as OBJECT
			IF oValue == NULL_OBJECT
				result := "{(0x0000)0x00000000} CLASS "
			ELSE
				var oType := oValue:GetType()
				var nSize := oType:GetFields():Length *4
				var cHashCode := String.Format("{0:X8}", oValue:GetHashCode())
				result := "{(0x"+String.Format("{0:X4}", nSize)+")0x"+cHashCode+"} CLASS " + oType:Name:ToUpperInvariant()
			ENDIF
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
	return symbol{(string)u, TRUE}   
	
	
	/// <summary>
	/// Create a descending order key value.
	/// </summary>
	/// <param name="uValue"></param>
	/// <returns>
	/// </returns>
function Descend(uValue as usual) as usual
	if uValue:isString
		return _descendingString( (String) uValue)
	elseif uValue:IsLogic	
		return ! (LOGIC) uValue
	elseif uValue:IsLong
		return 0 - (int) uValue
	elseif uValue:IsInt64
		return 0 - (int64) uValue
	elseif uValue:IsFloat
		return 0 - (Float) uValue
	elseif uValue:IsDate
		return (date) (6364425 - (dword)(date) uValue )
	endif
	return uValue

internal function _descendingString(s as string) as STRING
	var sb  := StringBuilder{s}
	var nlen := s:Length-1
	local i as int
	for i := 0 to nlen
		if sb[i] < 256
			sb[i] := (char) (255 - sb[i])
		endif
	next
	return sb:ToString()

	
/// <summary>
/// Create a descending order key value. The parameter is also changed 
/// </summary>
/// <param name="uValue"></param>
/// <returns>
/// </returns>
function DescendA(uValue REF usual) as usual
	uValue := Descend(uValue)
	return uValue
	
	
	
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
	case usualType.Long
      ret := ConversionHelpers.FormatNumber( (int64) n, (int) RuntimeState.Digits, 0):Trim()	
	Case UsualType.Date
      ret := AsString( n )
    CASE UsualType.Float
	Case UsualType.Decimal
      ret := ConversionHelpers.AdjustDecimalSeparator(_Str1(  (float) n )):Trim()
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
function Pad( uValue as usual, nLength as int, cPad := " " as string ) as string
	return PadR( uValue, nLength, cPad )
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function Pad( uValue as usual, nLength as dword, cPad := " " as string ) as string
	return PadR( uValue, (int) nLength, cPad )
	

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
	if cPad == null .or. cPad :Length == 0
		cPad := " "
	endif
	
	local ret     as string
	local retlen  as int
	
	if uValue:isNumeric
		ret := Ntrim( uValue)
	else
		ret := uValue:ToString()
	endif
	retlen := ret:Length
	
	if retlen > nLength
		ret := ret:Remove( nLength )
	else
		ret := ret:PadLeft( ( nLength - retlen ) / 2, cPad[0] ):PadRight( nLength, cPad[0] )
	endif
	
	return ret

/// <summary>
/// Pad character, numeric, and Date values with fill characters on both the right and left.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadC( uValue as usual, nLength as dword, cPad := " " as string ) as string
	return PadC( uValue, (int) nLength, cPad )
	
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadL( uValue as usual, nLength as int, cPad := " " as string ) as string
	// If they send in an empty string then change to " "
	if cPad == null .or. cPad :Length == 0
		cPad := " "
	endif
	local ret as string
	if uValue:IsNumeric
		ret := Ntrim( uValue)
	else
		ret := uValue:ToString()
	endif
	return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cPad[0] ) )
	
/// <summary>
/// Pad character, numeric, and Date values with fill characters on the left.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadL( uValue as usual, nLength as dword, cPad := " " as string ) as string
	return PadL( uValue, (int) nLength, cPad )
	

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="uValue"></param>
/// <param name="nLength"></param>
/// <param name="cFillStr"></param>
/// <returns>
/// </returns>
function PadR( uValue as usual, nLength as dword, cPad := " " as string ) as string
	return PadR( uValue, (int) nLength, cPad )

/// <summary>
/// Pad character, numeric, and Date values with fill characters on the right.
/// </summary>
/// <param name="cSource"></param>
/// <param name="nLength"></param>
/// <param name="cPad"></param>
/// <returns>
/// </returns>
function PadR( uValue as usual, nLength as int, cPad := " " as string ) as string
	// If they send in an empty string then change to " "
	if cPad == null .or. cPad:Length == 0
		cPad := " "
	endif
	local ret as string
	if uValue:IsNumeric
		ret := Ntrim( uValue)
	else
		ret := uValue:ToString()
	endif
	return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cPad[0] ) )
	
	 
	
/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="nLength"></param>
/// <param name="nDec"></param>
/// <returns>The string with always a decimal separator that matches the current SetDecimalSep() setting.</returns>
function Str(n ,nLen ,nDec ) as string CLIPPER
	if PCount() < 1 .or. pCount() > 3
		return ""
	endif
	local result as string
	result := _str3(n, nLen, nDec)
	return ConversionHelpers.AdjustDecimalSeparator(result)


/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="nLength"></param>
/// <param name="nDec"></param>
/// <returns>The string with always a DOT as decimal separator.</returns>

function _Str(n ,nLen ,nDec ) as string CLIPPER
	IF PCount() > 0 .and. ! n:IsNumeric 
       THROW Error.DataTypeError( __ENTITY__, nameof(n),1, n, nLen, nDec)
    ENDIF
	switch PCount()
	case 1
		return _Str1( n)
	case 2
		if ! nLen:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, nameof(nLen),2,n, nLen, nDec)
		endif
		if n:IsFloat
			return _Str2(n, nLen)
		else
			return ConversionHelpers.FormatNumber((int64) n, nLen,0)
		endif
	case 3
		if ! nDec:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, nameof(nDec),3,n, nLen, nDec)
		endif
		if n:IsFloat
			return _Str3(n, nLen, nDec)
		else
			return ConversionHelpers.FormatNumber((int64) n, nLen,nDec)
		endif
	OTHERWISE
		return ""
	end switch
	

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
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n, iLen, iDec)
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
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n, iLen)
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
      THROW Error.DataTypeError( __ENTITY__, nameof(n),1,n)
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
	
	



/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Str1(f AS USUAL) AS STRING
	if f:IsFloat
		return ConversionHelpers.AdjustDecimalSeparator(_Str1( (float) f))
	ELSE
		return ConversionHelpers.FormatNumber( (long) f, (int) RuntimeState.Digits, 0):Trim()
	ENDIF
		
INTERNAL function _Str1(f as float) as string
	var nDecimals := f:decimals
	VAR nDigits   := f:Digits
	var ltrim	  := FALSE
	if nDecimals < 0
		nDecimals := (short) RuntimeState.Decimals
	ENDIF
	if nDigits <= 0
		nDigits := (short) RuntimeState.Digits
		ltrim   := true
	endif
	VAR result := ConversionHelpers.FormatNumber(f, nDigits, nDecimals )
	IF (ltrim)
		result := result:TrimStart()
	ENDIF
	return result
 

/// <summary>
/// Convert a numeric expression to a string of a specified length.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION Str2(f AS Float,dwLen AS DWORD) AS STRING
	return ConversionHelpers.AdjustDecimalSeparator(_Str2(f, dwLen))


INTERNAL FUNCTION _Str2(f AS Float,dwLen AS DWORD) AS STRING
  IF dwLen == 0
      dwLen := (DWORD) RuntimeState.Digits
   ELSEIF dwLen  != UInt32.MaxValue
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF
   var nDecimals := f:decimals
	if nDecimals < 0
		nDecimals := (short) RuntimeState.Decimals
	endif
   RETURN ConversionHelpers.FormatNumber(f, (int) dwLen, nDecimals)
 

/// <summary>
/// Convert a numeric expression to a string of specific length and decimal places.
/// </summary>
/// <param name="f"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION Str3(f AS Float,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	return ConversionHelpers.AdjustDecimalSeparator(_Str3(f, dwLen, dwDec))

function _Str3(f as float,dwLen as dword,dwDec as dword) as string

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
   endif
   return ConversionHelpers.FormatNumber(f, (int) dwLen, (int) dwDec)


/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="dwRadix"></param>
/// <returns>
/// </returns>
FUNCTION StrToFloat(c AS STRING,dwRadix AS DWORD) AS Float
	var wSep   := SetDecimalSep()
	local result as float
	if wSep != 46 // .
		c := c:Replace((char) wSep, '.')
	endif
	try
		local r8 as System.Double
		if System.Double.TryParse(c, out r8)
			result := r8
		else
			result := 0
		endif
	catch
		result := 0
	end try
RETURN result





/// <summary>
/// Convert a string containing a numeric value to a numeric data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Val(cNumber as string) as Usual
	cNumber := cNumber:Trim():ToUpper()
	IF String.IsNullOrEmpty(cNumber)
		RETURN 0
	ENDIF
	// find non numeric characters in cNumber and trim the field to that length
	VAR pos := 0
	VAR done := FALSE
	VAR hex  := FALSE
	VAR hasdec := FALSE
	var hasexp := false
	var cDec := (CHAR) SetDecimalSep()
	if cDec != '.'
		cNumber := cNumber:Replace(cDec, '.')
	ENDIF
	FOREACH var c IN cNumber
		SWITCH c
		CASE '0' 
		case '1'
		case '2'
		case '3'
		case '4'
		case '5'
		case '6'
		case '7'
		case '8'
		CASE '9'
		CASE '-'
		CASE '+'
			NOP
		CASE '.'
		CASE ','
			IF hasdec
				done := true
			else
				hasdec := TRUE
			endif
		CASE 'A' 
		CASE 'B' 
		CASE 'C' 
		CASE 'D' 
		CASE 'F' 
			IF !hex
				done := TRUE
			endif
		CASE 'E' 
			// exponentional notation only allowed if decimal separator was there
			if hasdec
				hasexp := true
			ELSE
				done := true
			endif
		CASE 'L'	// LONG result
		CASE 'U'	// DWORD result
			done := TRUE
		CASE 'X' 
			IF pos == 1
				hex := true
			else
				done := true
			endif	
		OTHERWISE
			done := TRUE
		end switch
		IF done
			EXIT
		endif
		pos += 1
	NEXT
	IF pos < cNumber:Length
		cNumber := cNumber:SubString(0, pos)
	endif
	IF cNumber:IndexOfAny(<Char> {'.'}) > -1
		local r8Result := 0 as Real8
		if cDec != '.'
			cNumber := cNumber:Replace(cDec, '.')
		ENDIF
		VAR style := NumberStyles.Number
		IF hasexp
			style |= NumberStyles.AllowExponent
		ENDIF
		IF System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, REF r8Result)
			RETURN r8Result
		endif
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
			return iResult
		endif
	ENDIF
	return 0
	
		

