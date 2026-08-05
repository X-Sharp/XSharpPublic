//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VFPDocs.xml" path="Runtimefunctions/id/*" />
[FoxProFunction("ID", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Id( ) AS STRING
    RETURN Sys(0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/iscolor/*" />
[FoxProFunction("ISCOLOR", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsColor( ) AS LOGIC
    RETURN TRUE

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ismouse/*" />
[FoxProFunction("ISMOUSE", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsMouse( ) AS LOGIC
    RETURN TRUE

/// <include file="VFPDocs.xml" path="Runtimefunctions/nvl/*" />
[FoxProFunction("NVL", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION NVL( eExpression1, eExpression2) AS USUAL CLIPPER
    IF IsNil(eExpression1)
        RETURN eExpression2
    ENDIF
    RETURN eExpression1

/// <include file="VFPDocs.xml" path="Runtimefunctions/evl/*" />
[FoxProFunction("EVL", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION EVL( eExpression1 AS USUAL, eExpression2  AS USUAL) AS USUAL
    IF ! Empty(eExpression1)
        RETURN eExpression1
    ENDIF
    RETURN eExpression2

FUNCTION __FoxCast(expr AS USUAL, targetType AS STRING, nLen AS LONG, nDec AS LONG) AS USUAL
    LOCAL result := NIL AS USUAL
    LOCAL error := FALSE AS LOGIC

    // VFP accepts full type names (and Num) besides single-letter codes, and is case-insensitive
    targetType := Upper(targetType)

    SWITCH targetType
    CASE "W" // blob
    CASE "BLOB"
        // no width
        // no decimals
        IF IsBinary(expr)
            result := expr
        ELSEIF IsString(expr)
            result := BINARY{ (STRING) expr}
        ELSE
            error := TRUE
        ENDIF
    CASE "C" // Char
    CASE "CHAR"
    CASE "CHARACTER"
        // width applies, decimals disregarded; VFP default width is 1
        VAR str := ((OBJECT)expr):ToString()
        result := PadR(str, IIF(nLen == -1, 1, nLen))
    CASE "Y" // Currency
    CASE "CURRENCY"
        // no width
        // no decimals
        IF IsNumeric(expr)
            result := (CURRENCY) expr
        ELSE
            error := TRUE
        ENDIF
    CASE "D" // Date
    CASE "DATE"
        // no width
        // no decimals
        IF IsString(expr)
            result := CToD(expr)
        ELSEIF IsDate(expr) .OR. IsDateTime(expr)
            result := (DATE) expr
        ELSE
            error := TRUE
        ENDIF
    CASE "T" // DateTime
    CASE "DATETIME"
        // no width
        // no decimals
        IF IsString(expr)
            result := CToT(expr)
        ELSEIF IsDate(expr) .OR. IsDateTime(expr)
            result := (System.DateTime) expr
        ELSE
            error := TRUE
        ENDIF
    CASE "B" // Double
    CASE "DOUBLE"
        // nLen is # of decimals
        IF IsString(expr)
            expr := Val(expr)
        ENDIF
        IF IsNumeric(expr)
            result := (FLOAT) expr
            IF (nLen != -1 )
                result := FloatFormat(result,-1, nLen)
            ENDIF
        ELSE
            error := TRUE
        ENDIF
    CASE "N" // Numeric
    CASE "NUM"
    CASE "NUMERIC"
    CASE "F" // Float
    CASE "FLOAT"
        IF IsString(expr)
            expr := Val(expr)
        ENDIF
        IF IsNumeric(expr)
            result := (FLOAT) expr
            IF (nLen != -1 .OR. nDec != -1)
                result := FloatFormat(result,nLen, nDec)
            ENDIF
        ELSE
            error := TRUE
        ENDIF
    CASE "G" // General
    CASE "GENERAL"
        // no width
        // no decimals
        result := expr
    CASE "I" // Int
    CASE "INT"
    CASE "INTEGER"
        // no width
        // no decimals
        IF IsString(expr)
            expr := Val(expr)
        ENDIF
        IF IsNumeric(expr)
            result := (INT) expr
        ENDIF
    CASE "L" // Logic
    CASE "LOGICAL"
        // no width
        // no decimals
        IF IsString(expr)
            VAR cStr := (STRING) expr
            SWITCH cStr[0]
            CASE c't'
            CASE c'T'
            CASE c'y'
            CASE c'Y'
                result := TRUE
            OTHERWISE
                result := FALSE
            END SWITCH
        ELSEIF IsNumeric(expr)
            result := expr != 0
        ELSE
            error := TRUE
        ENDIF
    CASE "M" // Memo
    CASE "MEMO"
        // no width
        // no decimals
        result := ((OBJECT)expr):ToString()
    CASE "Q" // VarBinary
    CASE "VARBINARY"
        // no decimals
        IF IsString(expr)
            expr := BINARY{expr}
        ENDIF
        IF IsBinary(expr)
            IF nLen != -1
                VAR b := (BINARY) expr
                VAR str := (STRING) b
                IF str:Length > nLen
                    str := str:Substring(0, nLen)
                    b := BINARY{str}
                ENDIF
                result := b
            ELSE
                result := expr
            ENDIF
        ELSE
            error := TRUE
        ENDIF
    CASE "V" // VarChar
    CASE "VARCHAR"
        // no decimals
        VAR str := ((OBJECT)expr):ToString()
        IF nLen != -1 .AND. nLen < str:Length
            str := str:Substring(0, nLen)
        ENDIF
        result := str

    OTHERWISE
        // throw an error?
        error := TRUE
    END SWITCH
    IF error
        VAR	cMessage	:= VO_Sprintf(VOErrors.USUALCONVERSIONERR, ValType(expr), targetType)
        VAR err			:= Error{Gencode.EG_DATATYPE,"USUAL", cMessage}
        err:ArgNum		:= 1
        err:ArgType     := UsualType(expr)
        err:FuncSym		:= #CAST
        err:Args        := <OBJECT>{expr}
        THROW err
    ENDIF
    RETURN result

/// <include file="VfpDocs.xml" path="Runtimefunctions/isblank/*" />
    [FoxProFunction("ISBLANK", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
    FUNCTION IsBlank(eExpression AS USUAL) AS LOGIC
        IF eExpression == NIL .OR. eExpression == System.DBNull.Value
            RETURN FALSE
        ENDIF

        LOCAL dwType := UsualType(eExpression) AS DWORD

        SWITCH dwType
        CASE __UsualType.String
            RETURN String.IsNullOrWhiteSpace((STRING)eExpression)
        CASE __UsualType.Date
            RETURN (DATE)eExpression == NULL_DATE
        CASE __UsualType.Long
        CASE __UsualType.Float
        CASE __UsualType.Int64
        CASE __UsualType.Decimal
        CASE __UsualType.Currency
        CASE __UsualType.Logic
            RETURN FALSE
        END SWITCH

        RETURN FALSE
    ENDFUNC

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isnull/*" />
[FoxProFunction("ISNULL", FoxFunctionCategory.General, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IsNull(eExpression AS USUAL) AS LOGIC
    IF eExpression == NIL
        RETURN TRUE
    ENDIF

    RETURN System.Convert.IsDBNull(eExpression)
ENDFUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/imestatus/*" />
[FoxProFunction("IMESTATUS", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION ImeStatus(nExpression := 0 AS LONG) AS LONG
    RETURN 0
ENDFUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/ispen/*" />
[FoxProFunction("ISPEN", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION IsPen() AS LOGIC
    RETURN FALSE
ENDFUNC

/// <exclude/>
FUNCTION __VfpUnsupported(cCommand AS STRING) AS VOID
    // Silently absorb all unsupported VFP commands
    RETURN
