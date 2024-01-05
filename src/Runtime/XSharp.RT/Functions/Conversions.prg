//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp
using System.Text
using System.Globalization
using System.Collections.Generic
using System.Collections.Concurrent

#define MAXDIGITS               30
#define MAXDECIMALS             15

internal static class XSharp.ConversionHelpers
    static internal usCulture as CultureInfo
    static private formatStrings as ConcurrentDictionary<int, string>
    static constructor
        usCulture := CultureInfo{"en-US"}
        formatStrings := ConcurrentDictionary<int, string>{}

    static method GetFormatString(nLen as int, nDec as int) as string
        local nKey as int
        local cFormat as string
        nKey := nLen * 100 + nDec
        if formatStrings:TryGetValue(nKey, out var result)
            return result
        endif
        if nDec != 0
            cFormat := "0."
            cFormat := cFormat:PadRight(nDec+2, c'0')	// 2 extra for the 0 + Dot
        else
            cFormat := "0"
        endif
        cFormat := cFormat:PadLeft(nLen, c'#')
        cFormat := "{0," + nLen:ToString()+":"+cFormat+"}"
        formatStrings:TryAdd(nKey, cFormat)
        return cFormat

    private const NOCHAR := c'\0' as char
    static method NextChar(c as string, nIndex ref int) as char
        local cChar as char
        local lStart := nIndex == -1 as logic
        do while true
            nIndex ++
            if nIndex >= c:Length
                return NOCHAR
            end if
            cChar := c[nIndex]
            if cChar == c'E'
                return NOCHAR
            end if
            if cChar >= c'0' .and. cChar <= c'9'
                if cChar == c'0' .and. lStart
                    nIndex ++
                    loop
                end if
                return cChar
            end if
        end do


    static method AdjustPrecision(cNum15 as string, cNum17 as string) as string
        local cDiff15 := "0", cDiff17 := "0" as string
        local cResult as string
        local c15,c17 as char
        local n15,n17 as int
        local nMatch as int
        local lDiff  as logic
        n15 := n17 := -1
        nMatch := 0
        cResult := cNum15
        lDiff := false
        do while true
            c15 := NextChar(cNum15 , ref n15)
            c17 := NextChar(cNum17 , ref n17)
            if c15 == NOCHAR .or. c17 == NOCHAR
                if lDiff
                    exit
                else
                    return cNum15
                end if
            elseif c15 == c17
                nMatch ++
            else
                if nMatch >= 14
                    if nMatch < 17
                        lDiff := true
                        cResult := cResult:Substring(0, n15) + c17:ToString() + cResult:Substring(n15 + 1)
                        nMatch ++
                        cDiff15 += c15:ToString()
                        cDiff17 += c17:ToString()
                    else
                        exit
                    end if
                else
                    return cNum15
                end if
            end if
        end do

        // if the difference of the two numbers is the minimum one, then it was probably just a rounding issue in "G17" representation
        if Math.Abs( Int32.Parse(cDiff15) - Int32.Parse(cDiff17) ) == 1
            return cNum15
        end if
        return cResult

    static method FormatNumber(n as real8, nLen as int, nDec as int) as string
        local cFormat as string
        local result as string
        cFormat := GetFormatString(nLen, nDec)
        // G17 returns all 17 relevant digits for a REAL8
        // See https://docs.microsoft.com/en-us/dotnet/api/system.double.tostring?view=netframework-4.7.2
        result := String.Format(usCulture, cFormat, n)
        if nLen > 15 .and. result:Length >= 15
            result := AdjustPrecision(result, n:ToString("G17", usCulture))
        end if
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
        if result:Length > nLen
            local nSepIndex as int
            nSepIndex := result:IndexOf(usCulture:NumberFormat:NumberDecimalSeparator)
            if nSepIndex != -1 .and. nSepIndex <= nLen
                result := result:Substring(0, nLen)
            else
                result := Replicate("*", (dword) nLen)
            end if
        endif
        return result

    static method FormatNumber(n as int64, nLen as int, nDec as int) as string
        local cFormat as string
        local result as string
        cFormat := GetFormatString(nLen, 0)
        result := String.Format(usCulture, cFormat, n)
        if result:Length > nLen
            result := Replicate("*", (dword) nLen)
        endif
        return result


    static method AdjustDecimalSeparator(cString as string) as string
        if cString:IndexOf(".") >= 0
            var wSep   := SetDecimalSep()
            if wSep != 46
                cString := cString:Replace(c'.', (char) wSep)
            endif
        endif
        return cString

    static method GetSignificantWholeDigits(r as real8) as int
        local nRet := iif(r < 0.0 , 1 , 0) as int
        r := Math.Floor(Math.Abs(r))
        do while r > 0.0
            nRet ++
            r := r /10.0
            r := Math.Floor(r)
        end do
        return nRet

end class

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ashexstring/*" />
/// <seealso cref='C2Hex(System.String)' >C2Hex</seealso>
/// <seealso cref='_C2Hex(System.String,System.Boolean)' >_C2Hex</seealso>
function AsHexString(uValue as usual) as string
    local result as string
    if uValue:IsString
        result := _C2Hex( (string) uValue, true)
    elseif uValue:IsNumeric
        if uValue:IsInt64
            result := String.Format("{0:X16}", (int64) uValue)
        elseif uValue:IsLong
            result := String.Format("{0:X8}", (int) uValue)
        elseif uValue:IsFloat
            var flValue := (float) uValue
            var r8Value := flValue:Value
            if r8Value >= System.Int32.MinValue .and. r8Value <= System.Int32.MaxValue
                result := String.Format("{0:X8}", (int) uValue)
            elseif r8Value >= 0. .and. r8Value <= System.UInt32.MaxValue
                result := String.Format("{0:X8}", (dword) uValue)
            elseif r8Value >= System.Int64.MinValue .and. r8Value <= System.Int64.MaxValue
                result := String.Format("{0:X16}", (int64) uValue)
            elseif r8Value >= 0. .and. r8Value <= System.UInt64.MaxValue
                result := String.Format("{0:X16}", (uint64) uValue)
            else
                result := "********"
            endif
        else
            result := ""
        endif
    elseif uValue:IsPtr
        local i64 := (UIntPtr)uValue as UIntPtr
        local u64 := i64:ToUInt64() as uint64
        result := String.Format( iif( u64 > System.UInt32.MaxValue, "{0:X16}", "{0:X8}" ), u64 )
    else
        result := ""
    endif
    return result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aspadr/*" />
function AsPadr(uValue as usual,wLen as dword) as string
    return PadR(AsString(uValue), wLen)


/// <exclude />
function _AsString(u as usual) as string
    return	 AsString(u)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asstring/*" />
function AsString(uValue as usual) as string
    local result as string
    do case
    case uValue:IsString
        result := (string) uValue
    case uValue:IsNumeric
        result := NTrim(uValue)
    case uValue:IsSymbol
        result := Symbol2String( (symbol) uValue)
    case uValue:IsDate
        result := DToC( (date) uValue)
    case uValue:IsPtr
        local i64 := (UIntPtr)uValue as UIntPtr
        local u64 := i64:ToUInt64() as uint64
        result := String.Format( iif( u64 > System.UInt32.MaxValue, "0x{0:X16}", "0x{0:X8}" ), u64 )
    case uValue:IsArray
        var aValue := (array) uValue
        //  {[0000000003]0x025400FC}
        if aValue == null_array
            result := "{[0000000000]0x00000000}"
        else
            var cHashCode := String.Format("{0:X8}", aValue:GetHashCode())
            result := "{["+String.Format("{0:D10}",aValue:Length)+"]0x"+cHashCode+"}"
        endif

    case uValue:IsObject
        local oValue := uValue as object
        if oValue == null_object
            result := "{(0x0000)0x00000000} CLASS "
        else
            var oType := oValue:GetType()
            var nSize := oType:GetFields():Length *4
            var cHashCode := String.Format("{0:X8}", oValue:GetHashCode())
            result := "{(0x"+String.Format("{0:X4}", nSize)+")0x"+cHashCode+"} CLASS " + oType:Name:ToUpperInvariant()
        endif
    otherwise
        result := uValue:ToString()
    endcase
    return result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/assymbol/*" />
function AsSymbol(uValue as usual) as symbol
    if uValue:IsSymbol
        return (symbol) uValue
    endif
    return symbol{(string)uValue, true}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/descend/*" />
function Descend(uValue as usual) as usual
    if uValue:IsString
        return _descendingString( (string) uValue)
    elseif uValue:IsLogic
        return ! (logic) uValue
    elseif uValue:IsLong
        return 0 - (int) uValue
    elseif uValue:IsInt64
        return 0 - (int64) uValue
    elseif uValue:IsFloat
        return 0 - (__Float) uValue
    elseif uValue:IsDecimal
        return 0 - (decimal) uValue
    elseif uValue:IsCurrency
        return 0 - (__Currency) uValue
    elseif uValue:IsDate
        return 5231808 - (dword)(date) uValue
    endif
    return uValue

internal function _descendingString(s as string) as string
    local encoding as Encoding
    if RuntimeState.Ansi
        encoding := StringHelpers.WinEncoding
    else
        encoding := StringHelpers.DosEncoding
    endif
    local bytes := encoding:GetBytes( s ) as byte[]
    local nlen := bytes:Length as int
    for local i := 1 as int upto nlen
        if bytes[i] != 0
            bytes[i] := (byte) ( (int)256 - (int)bytes[i] )
        endif
    next
    return encoding:GetString( bytes )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/descend/*" />
function DescendA(uValue ref usual) as usual
    uValue := Descend(uValue)
    return uValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ntrim/*" />
function NTrim(nNum as usual) as string
    local ret as string
    switch nNum:_usualType
    case __UsualType.Int64
    case __UsualType.Long
        ret := ConversionHelpers.FormatNumber( (int64) nNum, (int) RuntimeState.Digits, 0):Trim()
    case __UsualType.Date
        ret := AsString( nNum )
    case __UsualType.Currency
        ret := ((__Currency) (nNum)):ToString()
    case __UsualType.Float
    case __UsualType.Decimal
        ret := ConversionHelpers.AdjustDecimalSeparator(_Str1(  (float) nNum )):Trim()
    otherwise
        throw Error.DataTypeError( __function__, nameof(nNum), 1, nNum)
    end switch
    return ret


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pad/*" />
function Pad( uValue as usual, nLength as int, cFillChar := " " as string ) as string
    return PadR( uValue, nLength, cFillChar )

/// <inheritdoc cref="Pad(XSharp.__Usual,System.Int32,System.String)" />
function Pad( uValue as usual, nLength as dword, cFillChar := " " as string ) as string
    return PadR( uValue, (int) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padc/*" />
function PadC( uValue as usual, nLength as int, cFillChar := " " as string ) as string
    // If they send in an empty string then change to " "
    if cFillChar == null .or. cFillChar :Length == 0
        cFillChar := " "
    endif

    local ret     as string
    local retlen  as int

    if uValue:IsNil
        ret := ""
    elseif uValue:IsNumeric
        ret := NTrim( uValue)
    else
        ret := uValue:ToString()
    endif
    retlen := ret:Length

    if retlen > nLength
        ret := ret:Remove( nLength )
    else
        var leftSpace := System.String{cFillChar[0], ( nLength - retlen ) / 2}
        ret := leftSpace+ret
        ret := ret:PadRight( nLength, cFillChar[0] )
    endif

    return ret

/// <inheritdoc cref="PadC(XSharp.__Usual,System.Int32,System.String)" />
function PadC( uValue as usual, nLength as dword, cFillChar := " " as string ) as string
    return PadC( uValue, (int) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padl/*" />
function PadL( uValue as usual, nLength as int, cFillChar := " " as string ) as string
    // If they send in an empty string then change to " "
    if cFillChar == null .or. cFillChar :Length == 0
        cFillChar := " "
    endif
    local ret as string
    if uValue:IsNil
        ret := ""
    elseif uValue:IsNumeric
        ret := NTrim( uValue)
    else
        ret := uValue:ToString()
    endif
    return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadLeft( nLength, cFillChar[0] ) )

/// <inheritdoc cref="PadL(XSharp.__Usual,System.Int32,System.String)" />
function PadL( uValue as usual, nLength as dword, cFillChar := " " as string ) as string
    return PadL( uValue, (int) nLength, cFillChar )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/padr/*" />
function PadR( uValue as usual, nLength as dword, cFillChar := " " as string ) as string
    return PadR( uValue, (int) nLength, cFillChar )

/// <inheritdoc cref="PadR(XSharp.__Usual,System.UInt32,System.String)" />
function PadR( uValue as usual, nLength as int, cFillChar := " " as string ) as string
    // If they send in an empty string then change to " "
    if cFillChar == null .or. cFillChar:Length == 0
        cFillChar := " "
    endif
    local ret as string
    if uValue:IsNil
        ret := ""
    elseif uValue:IsNumeric
        ret := NTrim( uValue)
    else
        ret := uValue:ToString()
    endif
    return iif( ret:Length > nLength, ret:Remove( nLength ), ret:PadRight( nLength, cFillChar[0] ) )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str/*" />
function Str(nNumber ,nLength ,nDecimals ) as string clipper
    if PCount() < 1 .or. pCount() > 3
        return ""
    endif

    // Handle integer values
    if nNumber:IsInteger .and. (PCount() <= 2 .or. (PCount() == 3 .and. nDecimals:IsNumeric .and. nDecimals == 0) )
        local cRet as string
        local nDigits as int

        cRet := ((int64)nNumber):ToString()

        if nLength:IsNumeric
            if nLength < 0
                nDigits := cRet:Length
            else
                nDigits := nLength
            end if
        else
            nDigits := (int)RuntimeState.Digits
        end if

        if cRet:Length > nDigits
            cRet := System.String{c'*' , nDigits}
        elseif cRet:Length < RuntimeState.Digits
            cRet := cRet:PadLeft(nDigits)
        end if

        return cRet
    elseif nNumber:IsFloat
        local oFloat as float
        oFloat := nNumber
        if oFloat:Digits != -1 .and. nLength:IsNil
            nLength := oFloat:Digits
        end if
    end if

    local result as string
    local nLen as dword
    local nDec as dword
    local lTrimSpaces := false as logic
    if nLength:IsNumeric
        if nLength < 0
            nLen := System.UInt32.MaxValue - 1
            lTrimSpaces := true
        else
            nLen := (dword) nLength
        endif
    else
        nLen := System.UInt32.MaxValue
    endif
    if ! nDecimals:IsNumeric
        nDec := UInt32.MaxValue
    else
        if nDecimals < 0
            nDec := System.UInt32.MaxValue
        else
            nDec := (dword) nDecimals
        endif
    endif
    result := _Str3(nNumber, nLen, nDec)
    if lTrimSpaces
        result := result:TrimStart()
    end if
    return ConversionHelpers.AdjustDecimalSeparator(result)


/// <inheritdoc cref="Str" />
/// <returns>The returned string with always have a DOT as decimal separator.</returns>
function _Str(nValue ,uLen ,uDec ) as string clipper
    local nLen,  nDec as long
    local dwLen, dwDec as dword
    if PCount() > 0 .and. ! nValue:IsNumeric
        throw Error.DataTypeError( __function__, nameof(nValue),1, nValue, uLen, uDec)
    endif
    switch PCount()
    case 1
        return _Str1( nValue)
    case 2
        if ! uLen:IsNumeric
            throw Error.DataTypeError( __function__, nameof(uLen),2,nValue, uLen, uDec)
        endif
        nLen := uLen
        if nLen < 0
            dwLen := System.UInt32.MaxValue
        else
            dwLen := (dword) nLen
        endif
        if nValue:IsFractional
            return _Str2(nValue, dwLen)
        else
            return ConversionHelpers.FormatNumber((int64) nValue, nLen,0)
        endif
    case 3
        if ! uLen:IsNumeric
            throw Error.DataTypeError( __function__, nameof(uLen),2,nValue, uLen, uDec)
        endif
        if ! uDec:IsNumeric
            throw Error.DataTypeError( __function__, nameof(uDec),3,nValue, uLen, uDec)
        endif
        nLen := uLen
        nDec := uDec
        if nValue:IsFractional
            nLen := uLen
            if nLen < 0
                dwLen := System.UInt32.MaxValue
            else
                dwLen := (dword) nLen
            endif

            if nDec < 0 .or. RuntimeState.Fixed
                dwDec := XSharp.RuntimeState.Decimals
            else
                dwDec := (dword) nDec
            endif
            if nLen < 0 .or. RuntimeState.DigitsFixed
                dwLen := XSharp.RuntimeState.Digits
                if nDec != 0
                    dwLen := dwLen + dwDec + 1
                endif
            endif
            var res := _Str3(nValue, dwLen, dwDec)
            if nLen < 0
                res := res:TrimStart()
            endif
            return res
        else
            if nLen < 0
                nLen := 30
            endif
            if nDec < 0
                nDec := (int) SetDecimal()
            endif
            return ConversionHelpers.FormatNumber((int64) nValue, nLen,nDec)
        endif
    otherwise
        return ""
    end switch

    // The following three functions are undocumented in Vulcan but sometimes used in user code
    // They are the equivalent of the STR() functions but always return with digit decimal separator
    // We route all three to the _Str() function that takes care of this already
/// <exclude/>
function __Str(n as usual) as string
    return _Str( n)

/// <exclude/>
function __Str(n as usual,nLen as usual) as string
    return _Str( n, nLen)

/// <exclude/>
function __Str(n as usual,nLen as usual, nDec as usual) as string
    return _Str( n, nLen, nDec)


internal function _PadZero(cValue as string) as string
    local iLen := 	cValue:Length as int
    cValue := cValue:TrimStart()
    if cValue:Length > 1 .and. cValue[0] == c'-'
        cValue := cValue:Substring(1)
        return "-" + cValue:PadLeft((int) iLen - 1, c'0')
    end if
    return cValue:PadLeft((int) iLen, c'0')


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
function StrZero(nNumber as usual,nLength as int,nDecimals as int) as string
    if ! ( nNumber:IsNumeric )
        throw Error.DataTypeError( __function__, nameof(nNumber),1,nNumber)
    endif
    local cValue := Str3(nNumber, (dword) nLength, (dword) nDecimals) as string
    return _PadZero(cValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
function StrZero(nNumber as usual,nLength as int) as string
    if ! ( nNumber:IsNumeric )
        throw Error.DataTypeError( __function__, nameof(nNumber),1,nNumber)
    endif
    local cValue := Str3(nNumber, (dword) nLength, 0) as string // Str3() and Str2() in VO have totally different behavior regarding decimal digits, rounding etc
    return _PadZero(cValue)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strzero/*" />
function StrZero(nNumber as usual) as string
    if ! ( nNumber:IsNumeric )
        throw Error.DataTypeError( __function__, nameof(nNumber),1,nNumber)
    endif
    local cValue := Str1(nNumber) as string
    return _PadZero(cValue)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/toword/*" />
function ToWord(n as usual) as dword
    return (dword) n


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strint/*" />
function StrInt(liNumber as long,dwLength as dword,dwDecimals as dword) as string
    return Str3( liNumber, dwLength, dwDecimals)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strlong/*" />
function StrLong(liNumber as long,dwLength as dword,dwDecimals as dword) as string
    return StrInt(liNumber, dwLength, dwDecimals)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strfloat/*" />
function StrFloat(flSource as float,dwLength as dword,dwDecimals as dword) as string
    return Str3( flSource, dwLength, dwDecimals )



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str1/*" />
function Str1(fNumber as usual) as string
    if fNumber:IsFloat
        return ConversionHelpers.AdjustDecimalSeparator(_Str1( (float) fNumber))
    else
        return ConversionHelpers.FormatNumber( (long) fNumber, (int) RuntimeState.Digits, 0):Trim()
    endif

internal function _Str1(f as float) as string
    var nDecimals := f:Decimals
    var nDigits   := f:Digits

    switch (Double)f
    case Double.NaN
        return Double.NaN:ToString()
    case Double.PositiveInfinity
        return Double.PositiveInfinity:ToString()
    case Double.NegativeInfinity
        return Double.NegativeInfinity:ToString()
    end switch

    if nDecimals < 0 .or. RuntimeState.Fixed
        nDecimals := (short) RuntimeState.Decimals
    endif
    if nDigits <= 0 .or. RuntimeState.DigitsFixed
        nDigits := (short) RuntimeState.Digits
        if ConversionHelpers.GetSignificantWholeDigits(f) > nDigits
            return string{ c'*', (int) nDigits}
        end if
        if nDecimals != 0
            nDigits += nDecimals +1
        endif
    endif
    var result := ConversionHelpers.FormatNumber(f, nDigits, nDecimals )
    return result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str2/*" />
/// <returns>A string representation of the value.</returns>
/// <seealso cref="Str" />
/// <seealso cref="Str1" />
/// <seealso cref="Str3" />
function Str2(fNumber as float,dwLength as dword) as string
    return ConversionHelpers.AdjustDecimalSeparator(_Str2(fNumber, dwLength))


internal function _Str2(f as float,dwLen as dword) as string
    local dwOriginal := dwLen as dword
    local lUnspecifiedLen := dwLen == UInt32.MaxValue as logic
    if dwLen == 0 .or. RuntimeState.DigitsFixed
        dwLen := (dword) RuntimeState.Digits
    elseif ! lUnspecifiedLen
        dwLen := Math.Min( (dword) dwLen, (dword) MAXDIGITS )
    endif
    var nDecimals := f:Decimals
    if nDecimals < 0 .or. RuntimeState.DigitsFixed
        nDecimals := (short) RuntimeState.Decimals
    endif
    var result :=  ConversionHelpers.FormatNumber(f, (int) dwLen, nDecimals)
    if SLen(result) < dwOriginal .and. ! lUnspecifiedLen
        result := result:PadLeft((int) dwOriginal)
    endif
    return result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/str3/*" />
/// <seealso cref="Str" />
/// <seealso cref="Str1" />
/// <seealso cref="Str2" />
function Str3(fNumber as float,dwLength as dword,dwDecimals as dword) as string
    return ConversionHelpers.AdjustDecimalSeparator(_Str3(fNumber, dwLength, dwDecimals))

/// <inheritdoc cref="Str3" />
/// <returns>A string with DOT as decimal separator.</returns>
function _Str3(f as float,dwLen as dword,dwDec as dword) as string
    local dwOriginal := dwLen as dword
    local lUnspecifiedDecimals  := dwDec == UInt32.MaxValue as logic
    local lUnspecifiedLen       := dwLen >= UInt32.MaxValue -1 as logic
    if lUnspecifiedDecimals
        if RuntimeState.Fixed
            dwDec := (dword) RuntimeState.Decimals
        else
            dwDec := (dword) f:Decimals
        endif
    else
        dwDec := Math.Min( (dword) dwDec, (dword) MAXDECIMALS )
    endif

    // dwLen == UInt32.MaxValue     : Nil passed for 2nd param of Str()
    // dwLen == UInt32.MaxValue - 1 : Negative value for 2nd param of Str()

    if dwLen == 0 .or. lUnspecifiedLen
        if dwDec > 0
            local nSignificant as int
            nSignificant := ConversionHelpers.GetSignificantWholeDigits(f)
            if .not. dwLen == UInt32.MaxValue - 1
                if nSignificant > RuntimeState.Digits
                    return string{ c'*', (int) (RuntimeState.Digits + dwDec +1) } // VO's behavior...
                end if
            end if
            if dwLen == UInt32.MaxValue - 1
                dwLen := (dword) nSignificant + dwDec + 2
            else
                dwLen := (dword) RuntimeState.Digits + dwDec +1
            end if
        else
            dwLen := (dword) RuntimeState.Digits
        endif
    else
        dwLen := Math.Min( (dword) dwLen, (dword) MAXDIGITS )
    endif


    if dwDec > 0 .and. dwLen != UInt32.MaxValue .and. !lUnspecifiedDecimals .and. ( dwLen < ( dwDec + 2 ) )
        return string{ c'*', (int) dwLen }
    endif
    var result :=  ConversionHelpers.FormatNumber(f, (int) dwLen, (int) dwDec)
    if SLen(result) < dwOriginal .and. ! lUnspecifiedLen
        result := result:PadLeft((int) dwOriginal)
    endif
    return result

/// <inheritdoc cref="Val" />
/// <returns>The numeric value as a FLOAT.</returns>
function StrToFloat(c as string) as float
    return (float) Val(c)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/val/*" />
function Val(cNumber as string) as usual
    local isCurrency as logic
    if String.IsNullOrEmpty(cNumber)
        return 0
    endif
    cNumber := AllTrim(cNumber)
    isCurrency := cNumber:StartsWith("$")
    if isCurrency
        cNumber := cNumber:Substring(1)
    endif
    var result := _VOVal(cNumber)
    if isCurrency
        return __Currency{ (real8) result }
    endif
    return result

    //implements the quirks of VO's version of Val()
internal function _VOVal(cNumber as string) as usual
    if String.IsNullOrEmpty(cNumber)
        return 0
    endif
    cNumber := cNumber:Trim():ToUpper()
    // find non numeric characters in cNumber and trim the field to that length
    var pos := 0
    var done := false
    var hex  := false
    var hasdec := false
    var hasexp := false
    var lNoNumYet := true
    var cPrev  := c' '
    var cDec   := (char) RuntimeState.DecimalSep

    if cDec != c'.'
        cNumber := cNumber:Replace(c'.', cDec) // VO behavior...
        cNumber := cNumber:Replace(cDec, c'.')
    endif
    foreach var c in cNumber
        switch c
        case c'0'
        case c'1'
        case c'2'
        case c'3'
        case c'4'
        case c'5'
        case c'6'
        case c'7'
        case c'8'
        case c'9'
            lNoNumYet := false
        case c'-'
        case c'+'
            if lNoNumYet
                lNoNumYet := true
            else
                done := true
            endif
        case c' '
            if .not. lNoNumYet
                done := true
            endif
        case c'.'
        case c','
            if c == c',' .and. cDec != c',' // Don't ask, VO...
                done := true
            elseif hasdec
                done := true
            else
                hasdec := true
            endif
        case c'A'
        case c'B'
        case c'C'
        case c'D'
        case c'F'
            if !hex
                done := true
            endif
        case c'E'
            // exponentional notation only allowed if decimal separator was there
            if hasdec
                hasexp := true
            else
                if !hex
                    done := true
                endif
            endif
        case c'L'	// LONG result
        case c'U'	// DWORD result
            done := true
        case c'X'
            if cPrev == c'0' .and. !hex
                hex := true
            else
                done := true
            endif
        otherwise
            done := true
        end switch
        if done
            exit
        endif
        pos += 1
        cPrev := c
    next
    if pos < cNumber:Length
        cNumber := cNumber:Substring(0, pos)
    endif
    if cNumber:IndexOf('-') == 0 .and. cNumber:Length > 2 .and. cNumber[1] == c' '
        cNumber := "-" + cNumber:Substring(1):Trim()
    end if

    if cNumber:IndexOfAny(<char> {c'.'}) > -1

        if cDec != c'.'
            cNumber := cNumber:Replace(cDec, c'.')
        endif
        var style := NumberStyles.Number
        if hasexp
            style |= NumberStyles.AllowExponent
        endif
        if System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, out var r8Result)
           if RuntimeState.Dialect == XSharpDialect.FoxPro
                return __Float{ r8Result }   // FoxPro Takes Decimals from settings
            else
                return __Float{ r8Result , cNumber:Length - cNumber:IndexOf(c'.') - 1}
           endif
        endif
    else

        local style as NumberStyles
        if hex

            style := NumberStyles.HexNumber

            local nHexTagPos as int
            nHexTagPos := cNumber:IndexOf("0x")
            if nHexTagPos == -1
                nHexTagPos := cNumber:IndexOf("0X")
            endif
            if nHexTagPos != -1
                cNumber := cNumber:Substring(0, nHexTagPos) + cNumber:Substring(nHexTagPos + 2):Trim()
            endif
            local lNegativeHex := false as logic
            if cNumber:IndexOf('-') == 0
                cNumber := cNumber:Substring(1)
                lNegativeHex := true
            elseif cNumber:IndexOf('+') == 0
                cNumber := cNumber:Substring(1)
            end if

            System.Int64.TryParse(cNumber, style, ConversionHelpers.usCulture, out var iResult)
            if lNegativeHex
                iResult := - iResult
            endif
            if Math.Abs(iResult) <= Int32.MaxValue
                return (int) iResult
            else
                return __Float{ (real8) iResult , 0 }
            endif

        else

            style := NumberStyles.Integer

            if cNumber:Length <= 9 // yes, no matter if there's a sign char or not
                System.Int32.TryParse(cNumber, style, ConversionHelpers.usCulture, out var iResult)
                return iResult
            else
                System.Double.TryParse(cNumber, style, ConversionHelpers.usCulture, out var rResult)
                return __Float{rResult, 0}
            endif

        endif
    endif
    return 0


/// <summary>
/// Convert an object containing a numeric value to a FLOAT
/// </summary>
/// <param name="oValue">Object containing the numeric value to convert.</param>
/// <returns>The value in the form of a float. </returns>
/// <exception cref='System.InvalidCastException'> Thrown when the parameter oValue cannot be converted to a FLOAT.</exception>
function Object2Float(oValue as object) as float
    local typ := oValue:GetType() as System.Type
    if typ == typeof(float)
        return (float) oValue
    endif
    local tc := System.Type.GetTypeCode(typ) as TypeCode
    switch tc
    case System.TypeCode.SByte
        return (System.SByte) oValue
    case System.TypeCode.Byte
        return (System.Byte) oValue
    case System.TypeCode.Double
        return (System.Double) oValue
    case System.TypeCode.Single
        return (System.Single) oValue
    case System.TypeCode.UInt16
        return (System.UInt16) oValue
    case System.TypeCode.UInt32
        return (System.UInt32) oValue
    case System.TypeCode.UInt64
        return (System.UInt64) oValue
    case System.TypeCode.Int16
        return (System.Int16) oValue
    case System.TypeCode.Int32
        return (System.Int32) oValue
    case System.TypeCode.Int64
        return (System.Int64) oValue
    case System.TypeCode.Decimal
        return (System.Decimal) oValue
    otherwise
        throw InvalidCastException{"Cannot convert from type "+typ:FullName+" to FLOAT"}
    end switch




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2f/*" />
function Bin2F(cFloat as string) as float
    local nDec as word
    local val  as real8
    if SLen(cFloat) >= 12
        nDec := Bin2W(SubStr3(cFloat, 11,2))
        val  := Bin2Real8(SubStr3(cFloat, 1,8))
        return float{val, 0, nDec}
    endif
    return 0.0


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/f2bin/*" />
function F2Bin(fValue as float) as string
    return Real82Bin(fValue:Value)+ e"\0\0" + W2Bin((word)fValue:Decimals)



