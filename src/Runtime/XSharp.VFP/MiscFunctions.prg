//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/iscolor/*" />
FUNCTION IsColor( ) AS LOGIC
    RETURN TRUE

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ismouse/*" />
FUNCTION IsMouse( ) AS LOGIC
    RETURN TRUE

/// <include file="VFPDocs.xml" path="Runtimefunctions/nvl/*" />
FUNCTION NVL( eExpression1, eExpression2) AS USUAL CLIPPER
    IF IsNil(eExpression1)
        RETURN eExpression2
    ENDIF
    RETURN eExpression1


/// <include file="VFPDocs.xml" path="Runtimefunctions/evl/*" />
FUNCTION EVL( eExpression1 AS USUAL, eExpression2  AS USUAL) AS USUAL
    IF ! Empty(eExpression1)
        RETURN eExpression1
    ENDIF
    RETURN eExpression2


//#translate CAST ( <expression> AS <type:W,C,Y,D,T,B,F,G,I,L,M,N,Q,V>)                   => __FoxCast(<expression>,<(type)>)
//#translate CAST ( <expression> AS <type:W,C,Y,D,T,B,F,G,I,L,M,N,Q,V>(<width> ) )        => __FoxCast(<expression>,<(type)>, <width>,-1)
//#translate CAST ( <expression> AS <type:W,C,Y,D,T,B,F,G,I,L,M,N,Q,V>(<width> ,<dec>) )  => __FoxCast(<expression>,<(type)>, <width>,<dec>)



FUNCTION __FoxCast(expr AS USUAL, targetType AS STRING, nLen AS LONG, nDec AS LONG) AS USUAL
    LOCAL result := NIL AS USUAL
    LOCAL error := FALSE AS LOGIC
    // This follows the FoxPro conversion rules from the CAST() function as close as possible
    SWITCH targetType
    CASE "W" // blob
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
        // no decimals
        result := PadR(expr,10)
    CASE "Y" // Currency
        // no width
        // no decimals
        IF IsNumeric(expr)
            result := (CURRENCY) expr
        ELSE
            error := TRUE
        ENDIF
    CASE "D" // Date
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
    CASE "F" // Float
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
        // no width
        // no decimals
        result := expr
    CASE "I" // Int
        // no width
        // no decimals
        IF IsString(expr)
            expr := Val(expr)
        ENDIF
        IF IsNumeric(expr)
            result := (INT) expr
        ENDIF
    CASE "L" // Logic
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
        // no width
        // no decimals
        result := ((OBJECT)expr):ToString()
    CASE "Q" // VarBinary
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

