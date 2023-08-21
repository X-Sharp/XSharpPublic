//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <summary>
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION Float2Long(f AS USUAL) AS LONG
    LOCAL flValue AS FLOAT
    flValue := (FLOAT) f
    RETURN (LONG) flValue

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/abs/*" />
FUNCTION Abs(nValue AS USUAL) AS USUAL
    LOCAL uRet AS USUAL
    IF nValue:IsInt64
        uRet := Math.Abs( (INT64) nValue )
    ELSEIF nValue:IsDecimal
        uRet := Math.Abs( (Decimal) nValue)
    ELSEIF nValue:IsCurrency
        uRet := __Currency{Math.Abs( (Decimal) (__Currency) nValue)}
    ELSEIF nValue:IsInteger
        uRet := Math.Abs( (INT) nValue )
    ELSEIF nValue:IsFloat
        uRet := AbsFloat( nValue )
    ELSE
        THROW  Error.ArgumentError( __FUNCTION__, NAMEOF(nValue), "Argument not numeric",1)
    ENDIF
    RETURN uRet

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/acot/*" />
FUNCTION ACot(nNum AS USUAL) AS FLOAT
    RETURN 2 * Math.Atan(1) + Math.Atan( (REAL8) nNum)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atan/*" />
FUNCTION ATan(nNum AS USUAL) AS FLOAT
    RETURN Math.Atan((REAL8)nNum)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/acos/*" />
FUNCTION ACos(nExpression AS USUAL) AS FLOAT
    RETURN Math.Acos((REAL8) nExpression)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asin/*" />
FUNCTION ASin(nExpression AS USUAL) AS FLOAT
    RETURN Math.Asin((REAL8) nExpression)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atan2/*" />
FUNCTION Atan2(nY as USUAL, nX AS USUAL) AS FLOAT
    RETURN Math.Atan2((REAL8) nY, (REAL8) nX)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dtor/*" />
FUNCTION DToR(nExpression as USUAL) AS REAL8
    RETURN (Real8) nExpression / 180.0 * Math.PI

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rtod/*" />
FUNCTION RToD(nExpression as USUAL) AS REAL8
    RETURN ((REAL8) nExpression / Math.PI) * 180.0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pi/*" />
FUNCTION PI() AS REAL8
    RETURN Math.PI

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ceil/*" />
FUNCTION Ceil(nNumber AS USUAL) AS USUAL
    LOCAL result AS USUAL
    IF nNumber:IsInteger
        result := nNumber
    ELSEIF nNumber:IsDecimal .OR. nNumber:IsCurrency
        LOCAL d AS DECIMAL
        IF (nNumber:IsDecimal)
            d := (DECIMAL) nNumber
        ELSE
            d := (DECIMAL) (CURRENCY) nNumber
        ENDIF
        d := Math.Ceiling(d)
        IF d <= Int32.MaxValue .AND. d >= Int32.MinValue
            result := Convert.ToInt32(d)
        ELSEIF d <= Int64.MaxValue .AND. d >= Int64.MinValue
            result := Convert.ToInt64(d)
        ELSE
            result := d
        ENDIF

    ELSEIF nNumber:IsFloat
        LOCAL r8 AS REAL8
        r8 := (FLOAT) nNumber
        r8 := Math.Ceiling(r8)
        IF r8 <= Int32.MaxValue .AND. r8 >= Int32.MinValue
            result := Convert.ToInt32(r8)
        ELSEIF r8 <= Int64.MaxValue .AND. r8 >= Int64.MinValue
            result := Convert.ToInt64(r8)
        ELSE
            result := r8
        ENDIF
    ELSE
        THROW  Error.ArgumentError( __FUNCTION__, NAMEOF(nNumber), "Argument not numeric",1)
    ENDIF
    RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/cos/*" />
FUNCTION Cos(nNum AS USUAL) AS FLOAT
    RETURN Math.Cos((REAL8)nNum)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/cot/*" />
FUNCTION Cot(nNum AS USUAL) AS FLOAT
    RETURN 2 * Math.Atan( 1 ) + Math.Atan( (REAL8)nNum)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/exp/*" />
FUNCTION Exp(nExponent AS USUAL) AS FLOAT
    RETURN Math.Exp((REAL8)nExponent)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/floor/*" />
FUNCTION Floor(nNum AS USUAL) AS USUAL
    LOCAL result AS USUAL
    IF nNum:IsInteger
        result := nNum
    ELSEIF nNum:IsDecimal .OR. nNum:IsCurrency
        LOCAL d AS DECIMAL
        IF (nNum:IsDecimal)
            d := (DECIMAL) nNum
        ELSE
            d := (DECIMAL) (CURRENCY) nNum
        ENDIF
        d := Math.Floor(d)
        IF d <= Int32.MaxValue .AND. d >= Int32.MinValue
            result := Convert.ToInt32(d)
        ELSEIF d <= Int64.MaxValue .AND. d >= Int64.MinValue
            result := Convert.ToInt64(d)
        ELSE
            result := d
        ENDIF

    ELSEIF nNum:IsFloat
        LOCAL r8 AS REAL8
        r8 := (FLOAT) nNum
        r8 := Math.Floor(r8)
        IF r8 <= Int32.MaxValue .AND. r8 >= Int32.MinValue
            result := Convert.ToInt32(r8)
        ELSEIF r8 <= Int64.MaxValue .AND. r8 >= Int64.MinValue
            result := Convert.ToInt64(r8)
        ELSE
            result := r8
        ENDIF
    ELSE
        THROW  Error.ArgumentError( __FUNCTION__, NAMEOF(nNum), "Argument not numeric",1)
    ENDIF
    RETURN result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/integer/*" />
FUNCTION Integer(nValue AS USUAL) AS USUAL
    LOCAL result AS USUAL
    IF nValue:IsInteger
        result := nValue
    ELSEIF nValue:IsFractional
        IF nValue > 0.0
            result := Floor(nValue)
        ELSE
            result := Ceil(nValue)
        ENDIF
    ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nValue), "Argument not numeric",1)
    ENDIF
    RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/log/*" />
FUNCTION LOG(nValue AS USUAL) AS FLOAT
    RETURN Math.Log((REAL8)nValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/log10/*" />
FUNCTION Log10(nValue AS USUAL) AS FLOAT
    RETURN Math.Log10((REAL8)nValue)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pow/*" />
FUNCTION Pow(nBase AS USUAL,nExponent AS USUAL) AS FLOAT
    RETURN Math.Pow((REAL8)nBase, (REAL8)nExponent)


INTERNAL GLOBAL _randomGenerator AS Random
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rand/*" />
FUNCTION Rand(nSeed AS USUAL) AS FLOAT
    IF !nSeed:IsNumeric .OR. nSeed <= 0
        nSeed := (INT) DateTime.Now:Ticks
    ENDIF
    _randomGenerator := Random{ nSeed }
    RETURN _randomGenerator:NextDouble()
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rand/*" />
FUNCTION Rand() AS FLOAT
    IF _randomGenerator == NULL
        _randomGenerator := Random{ 100001 }
    ENDIF
    RETURN _randomGenerator:NextDouble()



    #define MAX_DECIMALS 15
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/round/*" />
FUNCTION Round(nNumber AS USUAL,siDecimals AS INT) AS USUAL
    LOCAL ret    AS USUAL
    LOCAL IsLong   AS LOGIC
    LOCAL IsInt64  AS LOGIC
    LOCAL r8     AS REAL8

    IF ! nNumber:IsNumeric
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nNumber), "Argument is not numeric")
    ENDIF

    // For Integers we could round the numbers
    // Round(12345, -1) -> 12350
    IsInt64 := nNumber:IsInt64
    IsLong  := nNumber:IsLong
    r8  := nNumber

    IF siDecimals > 0
        // Round after decimal point
        IF siDecimals > MAX_DECIMALS
            siDecimals := MAX_DECIMALS
        ENDIF

        r8 := (REAL8) Math.Round( (System.Decimal) r8, siDecimals, MidpointRounding.AwayFromZero )
    ELSE
        // Round before decimal point
        siDecimals := -siDecimals

        IF siDecimals > MAX_DECIMALS
            siDecimals := MAX_DECIMALS
        ENDIF

        r8 := r8 / ( 10 ^ siDecimals )
        r8 := Math.Round( r8, 0, MidpointRounding.AwayFromZero )
        r8 := r8 * ( 10 ^ siDecimals )

        IF r8 < 0
            IsLong	:= r8 >= Int32.MinValue
            IsInt64 := r8 >= Int64.MinValue
        ELSE
            IsLong  := r8 <= Int32.MaxValue
            IsInt64 := r8 <= Int64.MaxValue
        ENDIF
        siDecimals := 0
    ENDIF

    IF IsLong .OR. IsInt64
        siDecimals := 0
    ENDIF

    IF siDecimals == 0
        IF IsLong
            ret := (INT) r8
        ELSEIF IsInt64
            ret := (INT64) r8
        ELSE
            ret := FLOAT{r8, 0}
        ENDIF
    ELSE
        ret := FLOAT{r8, siDecimals}
    ENDIF

    RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/sin/*" />
FUNCTION Sin(nNum AS USUAL) AS FLOAT
    RETURN Math.Sin((REAL8)nNum)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/sqrt/*" />
FUNCTION SQrt(nNumber AS USUAL) AS FLOAT
    RETURN Math.Sqrt((REAL8)nNumber)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/tan/*" />
FUNCTION Tan(nNum AS USUAL) AS FLOAT
    RETURN Math.Tan((REAL8)nNum)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mod/*" />
FUNCTION Mod(nDividend AS USUAL, nDivisor AS USUAL) AS USUAL
local resType := __UsualType.Void as __UsualType
switch nDividend:_usualType
CASE __UsualType.Long
    switch nDivisor:_usualType
    CASE __UsualType.Long
        resType := __UsualType.Long
    CASE __UsualType.Int64
        resType := __UsualType.Int64
    CASE __UsualType.Float
        resType := __UsualType.Float
    CASE __UsualType.Currency
        resType := __UsualType.Currency
    CASE __UsualType.Decimal
        resType := __UsualType.Decimal
    end switch
CASE __UsualType.Int64
    switch nDivisor:_usualType
    CASE __UsualType.Long
    CASE __UsualType.Int64
        resType := __UsualType.Int64
    CASE __UsualType.Float
        resType := __UsualType.Float
    CASE __UsualType.Currency
        resType := __UsualType.Currency
    CASE __UsualType.Decimal
        resType := __UsualType.Decimal
    end switch
CASE __UsualType.Float
    switch nDivisor:_usualType
    CASE __UsualType.Long
    CASE __UsualType.Int64
    CASE __UsualType.Float
        resType := __UsualType.Float
    CASE __UsualType.Currency
        resType := __UsualType.Currency
    CASE __UsualType.Decimal
        resType := __UsualType.Decimal
    end switch
CASE __UsualType.Currency
    resType := __UsualType.Currency
CASE __UsualType.Decimal
    resType := __UsualType.Decimal
end switch

switch resType
CASE __UsualType.Long
    RETURN (LONG) nDividend % (LONG) nDivisor
CASE __UsualType.Int64
    RETURN (INT64) nDividend % (INT64) nDivisor
CASE __UsualType.Float
    RETURN (FLOAT) nDividend % (FLOAT) nDivisor
CASE __UsualType.Currency
    RETURN (CURRENCY) nDividend % (CURRENCY) nDivisor
CASE __UsualType.Decimal
    RETURN (DECIMAL) nDividend % (DECIMAL) nDivisor
end switch
IF !IsNumeric(nDividend)
    THROW Error.ArgumentError("Mod", nameof(nDividend),"nDividend must be numeric")
endif
THROW Error.ArgumentError("Mod", nameof(nDivisor),"nDivisor must be numeric")



