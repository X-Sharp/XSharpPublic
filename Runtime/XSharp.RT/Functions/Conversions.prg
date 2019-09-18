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

    PRIVATE CONST NOCHAR := c'\0' AS CHAR
    STATIC METHOD NextChar(c AS STRING, nIndex REF INT) AS CHAR
        LOCAL cChar AS CHAR
        LOCAL lStart := nIndex == -1 AS LOGIC
        DO WHILE TRUE
            nIndex ++
            IF nIndex >= c:Length
                RETURN NOCHAR
            END IF
            cChar := c[nIndex]
            IF cChar == c'E'
                RETURN NOCHAR
            END IF
            IF cChar >= c'0' .AND. cChar <= c'9'
                IF cChar == c'0' .AND. lStart
                    nIndex ++
                    LOOP
                END IF
                RETURN cChar
            END IF
        END DO
    
    
    STATIC METHOD AdjustPrecision(cNum15 AS STRING, cNum17 AS STRING) AS STRING
        LOCAL cDiff15 := "0", cDiff17 := "0" AS STRING
        LOCAL cResult AS STRING
        LOCAL c15,c17 AS CHAR
        LOCAL n15,n17 AS INT
        LOCAL nMatch AS INT
        LOCAL lDiff  AS LOGIC
        n15 := n17 := -1
        nMatch := 0
        cResult := cNum15
        lDiff := FALSE
        DO WHILE TRUE
            c15 := NextChar(cNum15 , REF n15)
            c17 := NextChar(cNum17 , REF n17)
            IF c15 == NOCHAR .OR. c17 == NOCHAR
                IF lDiff
                    EXIT
                ELSE
                    RETURN cNum15
                END IF
            ELSEIF c15 == c17
                nMatch ++
            ELSE
                IF nMatch >= 14
                    IF nMatch < 17
                        lDiff := TRUE
                        cResult := cResult:Substring(0, n15) + c17:ToString() + cResult:Substring(n15 + 1)
                        nMatch ++
                        cDiff15 += c15:ToString()
                        cDiff17 += c17:ToString()
                    ELSE
                        EXIT
                    END IF
                ELSE
                    RETURN cNum15
                END IF
            END IF
        END DO
        
        // if the difference of the two numbers is the minimum one, then it was probably just a rounding issue in "G17" representation
        IF Math.Abs( Int32.Parse(cDiff15) - Int32.Parse(cDiff17) ) == 1
            RETURN cNum15
        END IF
    RETURN cResult

    STATIC METHOD FormatNumber(n AS REAL8, nLen AS INT, nDec AS INT) AS STRING
        LOCAL cFormat AS STRING
        LOCAL result AS STRING
        cFormat := GetFormatString(nLen, nDec)
        // G17 returns all 17 relevant digits for a REAL8
        // See https://docs.microsoft.com/en-us/dotnet/api/system.double.tostring?view=netframework-4.7.2
        result := String.Format(usCulture, cFormat, n)
        IF nLen > 15 .AND. result:Length >= 15
            result := AdjustPrecision(result, n:ToString("G17", usCulture))
        END IF
/*      IF result:EndsWith("0") .AND. nDec > 0 .AND. nLen > 15
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
        ENDIF*/
        IF result:Length > nLen
            LOCAL nSepIndex AS INT
            nSepIndex := result:IndexOf(usCulture:NumberFormat:NumberDecimalSeparator)
            IF nSepIndex != -1 .AND. nSepIndex <= nLen
                result := result:Substring(0, nLen)
            ELSE
                result := Replicate("*", (DWORD) nLen)
            END IF
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

    STATIC METHOD GetSignificantWholeDigits(r AS REAL8) AS INT
        LOCAL nRet := IIF(r < 0.0 , 1 , 0) AS INT
        r := Math.Floor(Math.Abs(r))
        DO WHILE r > 0.0
            nRet ++
            r := r /10.0
            r := Math.Floor(r)
        END DO
    RETURN nRet

END CLASS

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ashexstring/*" /> 
/// <seealso cref='M:XSharp.Core.Functions.C2Hex(System.String)' >C2Hex</seealso>
/// <seealso cref='M:XSharp.Core.Functions._C2Hex(System.String,System.Boolean)' >_C2Hex</seealso>
FUNCTION AsHexString(uValue AS USUAL) AS STRING
    LOCAL result AS STRING
    IF uValue:IsString
        result := _C2Hex( (STRING) uValue, TRUE)
    ELSEIF uValue:IsNumeric
        IF uValue:IsInt64
            result := String.Format("{0:X16}", (INT64) uValue)
        ELSE
            result := String.Format("{0:X8}", (INT) uValue)
        ENDIF
    ELSE
        result := ""
    ENDIF
    RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aspadr/*" />
FUNCTION AsPadr(uValue AS USUAL,wLen AS DWORD) AS STRING
    RETURN PadR(AsString(uValue), wLen)


/// <exclude />
FUNCTION _AsString(u AS USUAL) AS STRING
    RETURN	 AsString(u)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asstring/*" />
FUNCTION AsString(uValue AS USUAL) AS STRING
    LOCAL result AS STRING
    DO CASE
        CASE uValue:IsString
            result := (STRING) uValue
        CASE uValue:IsNumeric
            result := NTrim(uValue)
        CASE uValue:IsSymbol
            result := Symbol2String( (SYMBOL) uValue)
        CASE uValue:IsDate
            result := DToC( (DATE) uValue)
        CASE uValue:IsArray
            VAR aValue := (ARRAY) uValue
            //  {[0000000003]0x025400FC}
            IF aValue == NULL_ARRAY
                result := "{[0000000000]0x00000000}"
            ELSE
                VAR cHashCode := String.Format("{0:X8}", aValue:GetHashCode())
                result := "{["+STRING.Format("{0:D10}",aValue:Length)+"]0x"+cHashCode+"}"
            ENDIF

        CASE uValue:IsObject
            LOCAL oValue := uValue AS OBJECT
            IF oValue == NULL_OBJECT
                result := "{(0x0000)0x00000000} CLASS "
            ELSE
                VAR oType := oValue:GetType()
                VAR nSize := oType:GetFields():Length *4
                VAR cHashCode := String.Format("{0:X8}", oValue:GetHashCode())
                result := "{(0x"+String.Format("{0:X4}", nSize)+")0x"+cHashCode+"} CLASS " + oType:Name:ToUpperInvariant()
            ENDIF
        OTHERWISE
            result := uValue:ToString()
    ENDCASE
    RETURN result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/assymbol/*" />
FUNCTION AsSymbol(uValue AS USUAL) AS SYMBOL
    RETURN SYMBOL{(STRING)uValue, TRUE}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/descend/*" />
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/descend/*" />
FUNCTION DescendA(uValue REF USUAL) AS USUAL
    uValue := Descend(uValue)
    RETURN uValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ntrim/*" />
/// <seealso cref='M:XSharp.Core.Functions.LTrim(System.String)'>LTrim</seealso>
/// <seealso cref='M:XSharp.RT.Functions.Str(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)'>Str</seealso>
FUNCTION NTrim(nNum AS USUAL) AS STRING
    LOCAL ret AS STRING
    SWITCH nNum:_UsualType
    CASE __usualType.Int64
    CASE __usualType.Long
      ret := ConversionHelpers.FormatNumber( (INT64) nNum, (INT) RuntimeState.Digits, 0):Trim()
    CASE __UsualType.Date
      ret := AsString( nNum )
    CASE __UsualType.Float
    CASE __UsualType.Decimal
      ret := ConversionHelpers.AdjustDecimalSeparator(_Str1(  (FLOAT) nNum )):Trim()
    OTHERWISE
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(nNum), 1, nNum)
   END SWITCH
   RETURN ret


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pad/*" />
/// <seealso cref="M:XSharp.RT.Functions.PadR(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadC(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadL(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION Pad( uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING ) AS STRING
    RETURN PadR( uValue, nLength, cFillChar )

/// <inheritdoc cref="M:XSharp.RT.Functions.Pad(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION Pad( uValue AS USUAL, nLength AS DWORD, cFillChar := " " AS STRING ) AS STRING
    RETURN PadR( uValue, (INT) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padc/*" />
/// <seealso cref="M:XSharp.RT.Functions.PadR(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadL(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.Pad(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadC( uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING ) AS STRING
    // If they send in an empty string then change to " "
    IF cFillChar == NULL .OR. cFillChar :Length == 0
        cFillChar := " "
    ENDIF

    LOCAL ret     AS STRING
    LOCAL retlen  AS INT

    IF uValue:IsNil
        ret := ""
    ELSEIF uValue:isNumeric
        ret := NTrim( uValue)
    ELSE
        ret := uValue:ToString()
    ENDIF
    retlen := ret:Length

    IF retlen > nLength
        ret := ret:Remove( nLength )
    ELSE
        VAR leftSpace := Space((DWORD) ( nLength - retlen ) / 2)
        ret := leftSpace+ret
        ret := ret:PadRight( nLength, cFillChar[0] )
    ENDIF

    RETURN ret

/// <inheritdoc cref="M:XSharp.RT.Functions.PadC(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadC( uValue AS USUAL, nLength AS DWORD, cFillChar := " " AS STRING ) AS STRING
    RETURN PadC( uValue, (INT) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padl/*" />
/// <seealso cref="M:XSharp.RT.Functions.Pad(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadR(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadC(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadL( uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING ) AS STRING
    // If they send in an empty string then change to " "
    IF cFillChar == NULL .OR. cFillChar :Length == 0
        cFillChar := " "
    ENDIF
    LOCAL ret AS STRING
    IF uValue:IsNil
        ret := ""
    ELSEIF uValue:IsNumeric
        ret := NTrim( uValue)
    ELSE
        ret := uValue:ToString()
    ENDIF
    RETURN IIF( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cFillChar[0] ) )

/// <inheritdoc cref="M:XSharp.RT.Functions.PadL(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadL( uValue AS USUAL, nLength AS DWORD, cFillChar := " " AS STRING ) AS STRING
    RETURN PadL( uValue, (INT) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padr/*" />
/// <seealso cref="M:XSharp.RT.Functions.Pad(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadL(XSharp.__Usual,System.Int32,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.PadC(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadR( uValue AS USUAL, nLength AS DWORD, cFillChar := " " AS STRING ) AS STRING
    RETURN PadR( uValue, (INT) nLength, cFillChar )

/// <inheritdoc cref="M:XSharp.RT.Functions.PadR(XSharp.__Usual,System.Int32,System.String)" />
FUNCTION PadR( uValue AS USUAL, nLength AS INT, cFillChar := " " AS STRING ) AS STRING
    // If they send in an empty string then change to " "
    IF cFillChar == NULL .OR. cFillChar:Length == 0
        cFillChar := " "
    ENDIF
    LOCAL ret AS STRING
    IF uValue:IsNil
        ret := ""
    ELSEIF uValue:IsNumeric
        ret := NTrim( uValue)
    ELSE
        ret := uValue:ToString()
    ENDIF
    RETURN IIF( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cFillChar[0] ) )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str/*" />
/// <seealso cref="M:XSharp.RT.Functions.NTrim(XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str1(XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str2(XSharp.__Float,System.UInt32)" />
/// <seealso cref="M:XSharp.RT.Functions.Str3(XSharp.__Float,System.UInt32,System.UInt32)" />
/// <seealso cref="M:XSharp.RT.Functions.StrLong(System.Int32,System.UInt32,System.UInt32)" />
/// <seealso cref="M:XSharp.RT.Functions.StrFloat(XSharp.__Float,System.UInt32,System.UInt32)" />
/// <seealso cref="M:XSharp.Core.Functions.SetDigit(System.UInt32)" />
/// <seealso cref="M:XSharp.Core.Functions.SetDigitFixed(System.Boolean)" />
/// <seealso cref="M:XSharp.Core.Functions.SetDecimalSep(System.UInt32)" />
/// <seealso cref="M:XSharp.Core.Functions.SetScience(System.Boolean)" />
/// <seealso cref="M:XSharp.RT.Functions.Transform(XSharp.__Usual,System.String)" />
/// <seealso cref="M:XSharp.RT.Functions.Val(System.String)" />
FUNCTION Str(nNumber ,nLength ,nDecimals ) AS STRING CLIPPER
    IF PCount() < 1 .OR. pCount() > 3
        RETURN ""
    ENDIF

    // Handle integer values
    IF nNumber:IsInteger .and. (PCount() <= 2 .or. (PCount() == 3 .and. nDecimals:IsNumeric .and. nDecimals == 0) )
    	LOCAL cRet AS STRING
    	LOCAL nDigits AS INT

    	cRet := ((INT64)nNumber):ToString()

    	IF nLength:IsNumeric
    		IF nLength < 0
    			nDigits := cRet:Length
    		ELSE
	    		nDigits := nLength
    		END IF
    	ELSE
    		nDigits := (INT)RuntimeState.Digits
    	END IF

    	IF cRet:Length > nDigits
    		cRet := System.String{c'*' , nDigits}
    	ELSEIF cRet:Length < RuntimeState.Digits
    		cRet := cRet:PadLeft(nDigits)
    	END IF

    	RETURN cRet
    END IF

    LOCAL result AS STRING
    LOCAL nLen AS DWORD
    LOCAL nDec AS DWORD
    LOCAL lTrimSpaces := FALSE AS LOGIC
    IF nLength:IsNumeric
        IF nLength < 0
            nLen := System.UInt32.MaxValue - 1
            lTrimSpaces := TRUE
        ELSE
            nLen := (DWORD) nLength
        ENDIF
    ELSE
        nLen := System.UInt32.MaxValue
    ENDIF
    IF ! nDecimals:IsNumeric
        nDec := UInt32.MaxValue
    ELSE
        IF nDecimals < 0
            nDec := System.UInt32.MaxValue
        ELSE
            nDec := (DWORD) nDecimals
        ENDIF
    ENDIF
    result := _Str3(nNumber, nLen, nDec)
    IF lTrimSpaces
        result := result:TrimStart()
    END IF
    RETURN ConversionHelpers.AdjustDecimalSeparator(result)


/// <inheritdoc cref="M:XSharp.RT.Functions.Str(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
/// <returns>The returned string with always have a DOT as decimal separator.</returns>
FUNCTION _Str(nValue ,uLen ,uDec ) AS STRING CLIPPER
    LOCAL nLen,  nDec AS LONG
    LOCAL dwLen, dwDec AS DWORD
    IF PCount() > 0 .AND. ! nValue:IsNumeric
       THROW Error.DataTypeError( __FUNCTION__, NAMEOF(nValue),1, nValue, uLen, uDec)
    ENDIF
    SWITCH PCount()
    CASE 1
        RETURN _Str1( nValue)
    CASE 2
        IF ! uLen:IsNumeric
            THROW Error.DataTypeError( __FUNCTION__, NAMEOF(uLen),2,nValue, uLen, uDec)
        ENDIF
        nLen := uLen
        IF nLen < 0
            dwLen := System.UInt32.MaxValue
        ELSE
            dwLen := (DWORD) nLen
        ENDIF
        IF nValue:IsFloat
            RETURN _Str2(nValue, dwLen)
        ELSE
            RETURN ConversionHelpers.FormatNumber((INT64) nValue, nLen,0)
        ENDIF
    CASE 3
        IF ! uLen:IsNumeric
            THROW Error.DataTypeError( __FUNCTION__, NAMEOF(uLen),2,nValue, uLen, uDec)
        ENDIF
        IF ! uDec:IsNumeric
            THROW Error.DataTypeError( __FUNCTION__, NAMEOF(uDec),3,nValue, uLen, uDec)
        ENDIF
        nLen := uLen
        nDec := uDec
        IF nValue:IsFLoat
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
            VAR res := _Str3(nValue, dwLen, dwDec)
            IF nLen < 0
                res := res:TrimStart()
            ENDIF
            RETURN res
        ELSE
            IF nLen < 0
                nLen := 30
            ENDIF
            IF nDec < 0
                nDec := (INT) SetDecimal()
            ENDIF
            RETURN ConversionHelpers.FormatNumber((INT64) nValue, nLen,nDec)
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
FUNCTION StrZero(nNumber AS USUAL,nLength AS INT,nDecimals AS INT) AS STRING
    IF ! ( nNumber:IsNumeric )
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(nNumber),1,nNumber)
    ENDIF
    LOCAL cValue := Str3(nNumber, (DWORD) nLength, (DWORD) nDecimals) AS STRING
    RETURN _PadZero(cValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
FUNCTION StrZero(nNumber AS USUAL,nLength AS INT) AS STRING
    IF ! ( nNumber:IsNumeric )
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(nNumber),1,nNumber)
    ENDIF
    LOCAL cValue := Str2(nNumber, (DWORD) nLength) AS STRING
    RETURN _padZero(cValue)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
FUNCTION StrZero(nNumber AS USUAL) AS STRING
    IF ! ( nNumber:IsNumeric )
      THROW Error.DataTypeError( __FUNCTION__, NAMEOF(nNumber),1,nNumber)
    ENDIF
    LOCAL cValue := Str1(nNumber) AS STRING
    RETURN _PadZero(cValue)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/toword/*" />
FUNCTION ToWord(n AS USUAL) AS DWORD
    RETURN (DWORD) n


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strint/*" />
FUNCTION StrInt(liNumber AS LONG,dwLength AS DWORD,dwDecimals AS DWORD) AS STRING
    RETURN Str3( liNumber, dwLength, dwDecimals)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strlong/*" />
FUNCTION StrLong(liNumber AS LONG,dwLength AS DWORD,dwDecimals AS DWORD) AS STRING
    RETURN StrInt(liNumber, dwLength, dwDecimals)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strfloat/*" />
FUNCTION StrFloat(flSource AS FLOAT,dwLength AS DWORD,dwDecimals AS DWORD) AS STRING
    RETURN Str3( flSource, dwLength, dwDecimals )



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str1/*" />
FUNCTION Str1(fNumber AS USUAL) AS STRING
    IF fNumber:IsFloat
        RETURN ConversionHelpers.AdjustDecimalSeparator(_Str1( (FLOAT) fNumber))
    ELSE
        RETURN ConversionHelpers.FormatNumber( (LONG) fNumber, (INT) RuntimeState.Digits, 0):Trim()
    ENDIF

INTERNAL FUNCTION _Str1(f AS FLOAT) AS STRING
    VAR nDecimals := f:decimals
    VAR nDigits   := f:Digits

    SWITCH (Double)f
    CASE Double.NaN
        RETURN Double.NaN:ToString()
    CASE Double.PositiveInfinity
        RETURN Double.PositiveInfinity:ToString()
    CASE Double.NegativeInfinity
        RETURN Double.NegativeInfinity:ToString()
    END SWITCH
    
    IF nDecimals < 0 .OR. RuntimeState.Fixed
        nDecimals := (SHORT) RuntimeState.Decimals
    ENDIF
    IF nDigits <= 0 .OR. RuntimeState.DigitsFixed
        nDigits := (SHORT) RuntimeState.Digits
        IF ConversionHelpers.GetSignificantWholeDigits(f) > nDigits
            RETURN STRING{ c'*', (INT) nDigits}
        END IF
        IF nDecimals != 0
            nDigits += nDecimals +1
        ENDIF
    ENDIF
    VAR result := ConversionHelpers.FormatNumber(f, nDigits, nDecimals )
    RETURN result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str2/*" />
/// <returns>A string representation of the value.</returns>
/// <seealso cref="M:XSharp.RT.Functions.Str(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str1(XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str3(XSharp.__Float,System.UInt32,System.UInt32)" />
FUNCTION Str2(fNumber AS FLOAT,dwLength AS DWORD) AS STRING
    RETURN ConversionHelpers.AdjustDecimalSeparator(_Str2(fNumber, dwLength))


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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str3/*" />
/// <seealso cref="M:XSharp.RT.Functions.Str(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str1(XSharp.__Usual)" />
/// <seealso cref="M:XSharp.RT.Functions.Str2(XSharp.__Float,System.UInt32)" />
FUNCTION Str3(fNumber AS FLOAT,dwLength AS DWORD,dwDecimals AS DWORD) AS STRING
    RETURN ConversionHelpers.AdjustDecimalSeparator(_Str3(fNumber, dwLength, dwDecimals))

/// <inheritdoc cref="M:XSharp.RT.Functions.Str3(XSharp.__Float,System.UInt32,System.UInt32)" />
/// <returns>A string with DOT as decimal separator.</returns>
FUNCTION _Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING

   LOCAL lUnspecifiedDecimals := dwDec == UInt32.MaxValue AS LOGIC
   IF dwDec == UInt32.MaxValue
        IF RuntimeState.Fixed
            dwDec := (DWORD) RuntimeState.Decimals
        ELSE
            dwDec := (DWORD) f:Decimals
        ENDIF
   ELSE
      dwDec := Math.Min( dwDec, MAXDECIMALS )
   ENDIF
   
   // dwLen == UInt32.MaxValue     : Nil passed for 2nd param of Str()
   // dwLen == UInt32.MaxValue - 1 : Negative value for 2nd param of Str()
    
   IF dwLen == 0 .OR. dwLen == UInt32.MaxValue .or. dwLen == UInt32.MaxValue - 1
        IF dwDec > 0
        	LOCAL nSignificant AS INT
        	nSignificant := ConversionHelpers.GetSignificantWholeDigits(f)
            IF .not. dwLen == UInt32.MaxValue - 1
	            IF nSignificant > RuntimeState.Digits
	                RETURN STRING{ c'*', (INT) (RuntimeState.Digits + dwDec +1) } // VO's behavior...
	            END IF
            END IF
            IF dwLen == UInt32.MaxValue - 1
	            dwLen := (DWORD) nSignificant + dwDec + 2
            ELSE
	            dwLen := (DWORD) RuntimeState.Digits + dwDec +1
            END IF
        ELSE
            dwLen := (DWORD) RuntimeState.Digits
        ENDIF
   ELSE
      dwLen := Math.Min( dwLen, MAXDIGITS )
   ENDIF


 IF dwDec > 0 .AND. dwLen != UInt32.MaxValue .and. !lUnspecifiedDecimals .AND. ( dwLen < ( dwDec + 2 ) )
      RETURN STRING{ c'*', (INT) dwLen }
   ENDIF
   RETURN ConversionHelpers.FormatNumber(f, (INT) dwLen, (INT) dwDec)


/// <inheritdoc cref="M:XSharp.RT.Functions.Val(System.String)" />
/// <returns>The numeric value as a FLOAT.</returns>
FUNCTION StrToFloat(c AS STRING) AS FLOAT
    RETURN (FLOAT) Val(c)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/val/*" />
/// <seealso cref="M:XSharp.RT.Functions.Str(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
FUNCTION Val(cNumber AS STRING) AS USUAL
    RETURN _VOVal(AllTrim(cNumber))

//implements the quirks of VO's version of Val()
INTERNAL FUNCTION _VOVal(cNumber AS STRING) AS USUAL
    IF String.IsNullOrEmpty(cNumber)
      RETURN 0
    ENDIF
    cNumber := cNumber:Trim():ToUpper()
    // find non numeric characters in cNumber and trim the field to that length
    VAR pos := 0
    VAR done := FALSE
    VAR hex  := FALSE
    VAR hasdec := FALSE
    VAR hasexp := FALSE
    VAR lNoNumYet := TRUE
    VAR cPrev := (CHAR) ' '
    VAR cDec := (CHAR) RuntimeState.DecimalSep
    
    IF cDec != '.'
        cNumber := cNumber:Replace('.', cDec) // VO behavior...
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
            lNoNumYet := FALSE
        CASE '-'
        CASE '+'
            IF lNoNumYet
                lNoNumYet := TRUE
            ELSE
            	done := TRUE
            ENDIF
        CASE ' '
            IF .not. lNoNumYet
            	done := TRUE
            ENDIF
        CASE '.'
        CASE ','
            IF c == ',' .AND. cDec != ',' // Don't ask, VO...
                done := TRUE
            ELSEIF hasdec
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
            IF cPrev == '0' .and. !hex
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
        cPrev := c
    NEXT
    IF pos < cNumber:Length
        cNumber := cNumber:SubString(0, pos)
    ENDIF
    IF cNumber:IndexOf('-') == 0 .and. cNumber:Length > 2 .and. cNumber[1] == ' '
        cNumber := "-" + cNumber:Substring(1):Trim()
    END IF

    IF cNumber:IndexOfAny(<CHAR> {'.'}) > -1

        LOCAL r8Result := 0 AS REAL8
        IF cDec != '.'
            cNumber := cNumber:Replace(cDec, '.')
        ENDIF
        VAR style := NumberStyles.Number
        IF hasexp
            style |= NumberStyles.AllowExponent
        ENDIF
        IF System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT r8Result)
            RETURN __Float{ r8Result , cNumber:Length - cNumber:IndexOf('.') - 1}
        ENDIF

    ELSE

        LOCAL style AS NumberStyles
        IF hex

            style := NumberStyles.HexNumber

            LOCAL nHexTagPos AS INT
            nHexTagPos := cNumber:IndexOf("0x")
            IF nHexTagPos == -1
                nHexTagPos := cNumber:IndexOf("0X")
            ENDIF
            IF nHexTagPos != -1
                cNumber := cNumber:Substring(0, nHexTagPos) + cNumber:Substring(nHexTagPos + 2):Trim()
            ENDIF
            LOCAL lNegativeHex := FALSE AS LOGIC
            IF cNumber:IndexOf('-') == 0
            	cNumber := cNumber:Substring(1)
            	lNegativeHex := TRUE
            ELSEIF cNumber:IndexOf('+') == 0
            	cNumber := cNumber:Substring(1)
            END IF

            LOCAL iResult := 0 AS INT64
            System.Int64.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT iResult)
            IF lNegativeHex
                iResult := - iResult
            ENDIF
            IF Math.Abs(iResult) <= Int32.MaxValue
                RETURN (INT) iResult
            ELSE
                RETURN __Float{ (REAL8) iResult , 0 }
            ENDIF
           
        ELSE

            style := NumberStyles.Integer

            IF cNumber:Length <= 9 // yes, no matter if there's a sign char or not
                LOCAL iResult := 0 AS INT
                System.Int32.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT iResult)
                RETURN iResult
            ELSE
                LOCAL rResult := 0 AS REAL8
                System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, OUT rResult)
                RETURN __Float{rResult, 0}
            ENDIF

        ENDIF

    ENDIF
    RETURN 0


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




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2f/*" />
FUNCTION Bin2F(cFloat AS STRING) AS FLOAT
    LOCAL nDec AS WORD
    LOCAL val  AS REAL8
    IF SLen(cFloat) >= 12
        nDec := Bin2W(SubStr3(cFloat, 11,2))
        val  := Bin2Real8(SubStr3(cFloat, 1,8))
        RETURN FLOAT{val, 0, nDec}
    ENDIF
    RETURN 0.0
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/f2bin/*" />
FUNCTION F2Bin(fValue AS FLOAT) AS STRING
    RETURN Real82Bin(fValue:Value)+ e"\0\0" + W2Bin((WORD)fValue:Decimals)
    
