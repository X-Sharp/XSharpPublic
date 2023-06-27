/// <include file="Harbour.xml" path="Runtimefunctions/hb_adel/*" />
FUNCTION Hb_ADel( aArray, nPos, lAutoSize ) AS ARRAY CLIPPER
    EnforceType(aArray, ARRAY)
    LOCAL aSource as ARRAY
    Default(REF nPos, 1) 
    Default(ref lAutoSize, FALSE)
    aSource := aArray
    ADel(aSource, 1) 
    IF lAutoSize
        ASize(aSource, ALen(aSource)-1) 
    ENDIF
    RETURN aSource 

/// <include file="Harbour.xml" path="Runtimefunctions/hb_ains/*" />
FUNCTION Hb_AIns( aArray, nPos, xValue, lAutoSize ) AS ARRAY CLIPPER
    LOCAL aSource as ARRAY
    EnforceType(aArray, ARRAY)
    aSource := aArray
    Default(REF nPos, 1)
    Default(ref lAutoSize, FALSE)
    IF lAutoSize
        ASize(aSource, ALen(aSource)+1)
    ENDIF
    AIns(aSource, nPos)
    aSource[nPos] := xValue
    RETURN aSource
