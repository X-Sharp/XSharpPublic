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
			cFormat := cFormat:PadRight(nDec+2, c'0')	// 2 extra for the 0 + Dot
		ELSE
			cFormat := "0"
		ENDIF
		cFormat := cFormat:PadLeft(nLen, c'#')
		cFormat := "{0," + nLen:ToString()+":"+cFormat+"}"
		formatStrings:Add(nKey, cFormat)
		RETURN cFormat

	STATIC METHOD FormatNumber(n AS REAL8, nLen AS INT, nDec AS INT) AS STRING
		LOCAL cFormat AS STRING
		LOCAL result AS STRING
		cFormat := GetFormatString(nLen, nDec)
        // G17 returns all 17 relevant digits for a REAL8
		result := String.Format(usCulture, cFormat, n)
        IF result:EndsWith("0") .AND. nDec > 0 .AND. nLen > 15
            VAR cTemp := n:ToString("G17", usCulture)
            VAR parts := cTemp:Split(<CHAR>{c'.'}, StringSplitOptions.RemoveEmptyEntries)
            IF parts:Length > 1
                // If the G17 format contains a decimal part, fetch it.
                VAR cDec := parts[1+ __ARRAYBASE__]
                VAR cInt := parts[0+ __ARRAYBASE__]

				// could end up being represented in exp format. For example
				// Str3(70.00 - 65.01 - 4.99,16,2)
				// causes cTemp above to have a value of "-5.3290705182007514E-15"
	            LOCAL nExp AS INT
	            nExp := cDec:IndexOf('E')
	            IF nExp != -1
	            	IF cDec[nExp + 1] == '-'
	            		LOCAL cExp AS STRING
	            		cExp := System.String{'0', Int32.Parse(cDec:Substring(nExp + 2))}
	            		cDec := cExp + iif(cInt[0] == '-' , cInt:SubString(1) , cInt) + cDec
	            		cInt := iif(cInt[0] == '-' , "-0" , "0")
	            	END IF
	            END IF

                parts := result:Split(<CHAR>{c'.'}, StringSplitOptions.RemoveEmptyEntries)
                VAR cOldDec := parts[1+ __ARRAYBASE__]
                IF cDec:Length > cOldDec:Length
                    cDec := cDec:SubString(0, cOldDec:Length)
                ELSEIF cDec:Length < cOldDec:Length
                    cDec := cDec:PadRight(cOldDec:Length,c'0')
                ENDIF
                result := parts[0+ __ARRAYBASE__] + "." + cDec
            ENDIF
        ENDIF
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
				cString := cString:Replace(c'.', (CHAR) wSep)
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
		RETURN 0 - (__Float) uValue
	ELSEIF uValue:IsDate
		RETURN 5231808 - (DWORD)(DATE) uValue 
	ENDIF
	RETURN uValue

INTERNAL FUNCTION _descendingString(s AS STRING) AS STRING
	VAR sb  := StringBuilder{s}
	VAR nlen := s:Length-1
	LOCAL i AS INT
	FOR i := 0 TO nlen
		IF sb[i] < 256
			sb[i] := (CHAR) (255 - sb[i])
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
	CASE __usualType.Int64
	CASE __usualType.Long
      ret := ConversionHelpers.FormatNumber( (INT64) n, (INT) RuntimeState.Digits, 0):Trim()
	CASE __UsualType.Date
      ret := AsString( n )
    CASE __UsualType.Float
	CASE __UsualType.Decimal
      ret := ConversionHelpers.AdjustDecimalSeparator(_Str1(  (FLOAT) n )):Trim()
    OTHERWISE
      THROW Error.DataTypeError( __ENTITY__, NAMEOF(n), 1, n)
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
	IF cPad == NULL .OR. cPad :Length == 0
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
	IF cPad == NULL .OR. cPad :Length == 0
		cPad := " "
	ENDIF
	LOCAL ret AS STRING
	IF uValue:IsNumeric
		ret := NTrim( uValue)
	ELSE
		ret := uValue:ToString()
	ENDIF
	RETURN IIF( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cPad[0] ) )

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
	IF cPad == NULL .OR. cPad:Length == 0
		cPad := " "
	ENDIF
	LOCAL ret AS STRING
	IF uValue:IsNumeric
		ret := NTrim( uValue)
	ELSE
		ret := uValue:ToString()
	ENDIF
	RETURN IIF( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cPad[0] ) )



/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="uLen"></param>
/// <param name="uDec"></param>
/// <returns>The string with always a decimal separator that matches the current SetDecimalSep() setting.</returns>
FUNCTION Str(n ,uLen ,uDec ) AS STRING CLIPPER
	IF PCount() < 1 .OR. pCount() > 3
		RETURN ""
	ENDIF
	LOCAL result AS STRING
	LOCAL nLen AS DWORD
    LOCAL nDec AS DWORD
	IF IsNumeric(uLen)
        IF uLen < 0
	        nLen := System.UInt32.MaxValue
        ELSE
            nLen := (DWORD) uLen
        ENDIF
	ELSE
		nLen := System.UInt32.MaxValue
    ENDIF
    IF ! IsNumeric(uDec)
        nDec := UInt32.MaxValue
    ELSE
        IF uDec < 0
		    nDec := System.UInt32.MaxValue
        ELSE
            nDec := (DWORD) uDec
        ENDIF
    ENDIF
	result := _str3(n, nLen, nDec)
	RETURN ConversionHelpers.AdjustDecimalSeparator(result)


/// <summary>
/// Convert a numeric expression to a string.
/// </summary>
/// <param name="n"></param>
/// <param name="uLength"></param>
/// <param name="uDec"></param>
/// <returns>The string with always a DOT as decimal separator.</returns>

FUNCTION _Str(n ,uLen ,uDec ) AS STRING CLIPPER
    LOCAL nLen,  nDec AS LONG
    LOCAL dwLen, dwDec AS DWORD
	IF PCount() > 0 .AND. ! n:IsNumeric
       THROW Error.DataTypeError( __ENTITY__, NAMEOF(n),1, n, uLen, uDec)
    ENDIF
	SWITCH PCount()
	CASE 1
		RETURN _Str1( n)
	CASE 2
		IF ! uLen:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, NAMEOF(uLen),2,n, uLen, uDec)
        ENDIF
        nLen := uLen
        IF nLen < 0
            dwLen := System.UInt32.MaxValue
        ELSE
            dwLen := (DWORD) nLen
        ENDIF
		IF n:IsFloat
			RETURN _Str2(n, dwLen)
		ELSE
			RETURN ConversionHelpers.FormatNumber((INT64) n, nLen,0)
		ENDIF
	CASE 3
		IF ! uLen:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, NAMEOF(uLen),2,n, uLen, uDec)
		ENDIF
		IF ! uDec:IsNumeric
			THROW Error.DataTypeError( __ENTITY__, NAMEOF(uDec),3,n, uLen, uDec)
        ENDIF
        nLen := uLen
        nDec := uDec
 		IF n:IsLong
            nLen := uLen
            IF nLen < 0
                dwLen := System.UInt32.MaxValue
            ELSE
                dwLen := (DWORD) nLen
            ENDIF
            
            IF nDec < 0 .OR. RuntimeState.Fixed
                dwDec := XSharp.RuntimeState.Decimals
            ELSE
                dwDec := (DWORD) nDec
            ENDIF
            IF nLen < 0 .OR. RuntimeState.DigitsFixed
                dwLen := XSharp.RuntimeState.Digits
                IF nDec != 0
                    dwLen := dwLen + dwDec + 1
                ENDIF
            ENDIF
            RETURN _Str3(n, dwLen, dwDec)
        ELSE
            IF nLen < 0
                nLen := -1
            ENDIF
            IF nDec < 0
                nDec := -1
            ENDIF
			RETURN ConversionHelpers.FormatNumber((INT64) n, nLen,nDec)
		ENDIF
	OTHERWISE
		RETURN ""
	END SWITCH

// The following three functions are undocumented in Vulcan but sometimes used in user code
// They are the equivalent of the STR() functions but always return with digit decimal separator
// We route all three to the _Str() function that takes care of this already
/// <exclude/>
FUNCTION __Str(n AS USUAL) AS STRING
	RETURN _Str( n)

/// <exclude/>
FUNCTION __Str(n AS USUAL,nLen AS USUAL) AS STRING
	RETURN _Str( n, nLen)

/// <exclude/>
FUNCTION __Str(n AS USUAL,nLen AS USUAL, nDec AS USUAL) AS STRING
	RETURN _Str( n, nLen, nDec)


INTERNAL FUNCTION _PadZero(cValue AS STRING) AS STRING
	LOCAL iLen := 	cValue:Length AS INT
	RETURN cValue:TrimStart():PadLeft((INT) iLen, c'0')


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
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(n),1,n, iLen, iDec)
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
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(n),1,n, iLen)
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
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(n),1,n)
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
	IF nDecimals < 0 .OR. RuntimeState.Fixed
		nDecimals := (SHORT) RuntimeState.Decimals
	ENDIF
	IF nDigits <= 0 .OR. RuntimeState.DigitsFixed
		nDigits := (SHORT) RuntimeState.Digits
        IF nDecimals != 0
            nDigits += nDecimals +1
        ENDIF
	ENDIF
	VAR result := ConversionHelpers.FormatNumber(f, nDigits, nDecimals )
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
  IF dwLen == 0 .OR. RuntimeState.DigitsFixed
      dwLen := (DWORD) RuntimeState.Digits
   ELSEIF dwLen  != UInt32.MaxValue
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF
   VAR nDecimals := f:decimals
	IF nDecimals < 0 .OR. RuntimeState.DigitsFixed
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

/// <exclude/>
FUNCTION _Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING


   IF dwDec == UInt32.MaxValue
        IF RuntimeState.Fixed
            dwDec := (DWORD) RuntimeState.Decimals
        ELSE
            dwDec := (DWORD) f:Decimals
        ENDIF
   ELSE
      dwDec := Math.Min( dwDec, MAXDECIMALS )
   ENDIF
    
   IF dwLen == 0 .OR. dwLen == UInt32.MaxValue 
        IF dwDec > 0 
            dwLen := (DWORD) RuntimeState.Digits + dwDec +1
        ELSE
            dwLen := (DWORD) RuntimeState.Digits
        ENDIF
   ELSE
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF

 
   IF dwDec > 0 .AND. dwLen != UInt32.MaxValue .and. ( dwLen < ( dwDec + 2 ) )
      RETURN STRING{ c'*', (INT) dwLen }
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
		c := c:Replace((CHAR) wSep, c'.')
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
	RETURN _Val(cNumber)


/// <summary>
/// Convert an object containing a numeric value to a FLOAT
/// </summary>
/// <param name="oValue">Object containing the numeric value to convert.</param>
/// <returns>The value in the form of a float. </returns>
/// <exception cref='T:System.InvalidCastException'> Thrown when the parameter oValue cannot be converted to a FLOAT.</exception>
FUNCTION Object2Float(oValue AS OBJECT) AS FLOAT
    LOCAL typ := oValue:GetType() AS System.Type
    IF typ == typeof(FLOAT)
        RETURN (FLOAT) oValue
    ENDIF
    LOCAL tc := System.Type.GetTypeCode(typ) AS TypeCode
    SWITCH tc
    CASE System.TypeCode.SByte
        RETURN (System.SByte) oValue
    CASE System.TypeCode.Byte
        RETURN (System.Byte) oValue
    CASE System.TypeCode.Double
        RETURN (System.Double) oValue
    CASE System.TypeCode.Single
        RETURN (System.Single) oValue
    CASE System.TypeCode.UInt16
        RETURN (System.UInt16) oValue
    CASE System.TypeCode.UInt32
        RETURN (System.UInt32) oValue
    CASE System.TypeCode.UInt64
        RETURN (System.UInt64) oValue
    CASE System.TypeCode.Int16
        RETURN (System.Int16) oValue
    CASE System.TypeCode.Int32
        RETURN (System.Int32) oValue
    CASE System.TypeCode.Int64
        RETURN (System.Int64) oValue
    CASE System.TypeCode.Decimal
        RETURN (System.Decimal) oValue
    OTHERWISE
        THROW InvalidCastException{"Cannot convert from type "+typ:FullName+" to FLOAT"}
    END SWITCH


