FUNCTION Hb_ADel( aArray, nPos, lAutoSize ) AS ARRAY CLIPPER
    EnforceType(aArray, ARRAY)
    Default(REF nPos, 1)
    Default(ref lAutoSize, FALSE)
    ADel(aArray, 1)
    IF lAutoSize
        ASize(aArray, ALen(aArray)-1)
    ENDIF
    RETURN aArray
