FUNCTION Start() AS VOID
    local pBuffer as PSZ
    local pFormat as PSZ
    pFormat := String2Psz("Example %s")
    pBuffer := MemAlloc(1000)
    wsprintf(pBuffer, pFormat, __arglist("abc"))
    ? Psz2String(pBuffer)
    MemFree(pBuffer)
    wait
    return



_DLL FUNCTION wsprintf(pszBuffer AS PSZ, pszFormat AS PSZ, ...) AS INT STRICT:USER32.wsprintfA
