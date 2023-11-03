FUNCTION Start() AS VOID
    ? DbUseArea(,"DBFCDX","C:\TEMP\INIFILE")
    ? RecCount()
    ? LastRec()
    ? DbSeek("BASE    CONNECT STRING")
    ? RecNo(), FieldGet(1), FieldGet(2)
    ? DbCloseArea()
    WAIT
