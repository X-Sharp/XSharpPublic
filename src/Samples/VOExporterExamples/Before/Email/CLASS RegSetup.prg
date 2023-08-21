CLASS RegSetup
    //  UH 03/01/2000
    //  Should be based on ...\CA Visual Objects Applications
    //  and therefore use
    //  SetRTRegString()
    //  QueryRTRegString()
    //  SetRTRegInt()
    //  QueryRTRegInt()
    //  DeleteRTRegKey()
    PROTECT cSubKey



CONSTRUCTOR(xKey)                                   
    Default(@xKey, "")

    IF SLen(xKey) == 0
        SELF:cSubKey := "EMail Sample"
    ELSE
        SELF:cSubKey := xKey
    ENDIF

    RETURN SELF



METHOD QueryInt      (cName)                     
    Default(@cName, "")
    RETURN QueryRTRegInt(SELF:cSubKey, cName)



METHOD QueryString   (cName)                     
    Default(@cName, "")
    RETURN QueryRTRegString(SELF:cSubKey, cName)



METHOD QueryValue    (cName)                     
    LOCAL xRet  AS USUAL
    LOCAL nRet  AS INT

    nRet := SELF:QueryInt(cName)

    IF nRet == 0
        xRet := SELF:QueryString(cName)
    ELSE
        xRet := nRet
    ENDIF

    RETURN xRet



METHOD SetInt        (cName, nValue)             
    Default(@cName, "")
    Default(@nValue, 0)
    RETURN SetRTRegInt(SELF:cSubKey, cName, nValue)



METHOD SetString     (cName, cValue)            
    Default(@cName, "")
    Default(@cValue, "")
    RETURN SetRTRegString(SELF:cSubKey, cName, cValue)




END CLASS
