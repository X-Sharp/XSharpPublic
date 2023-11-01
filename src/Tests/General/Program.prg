FUNCTION Start as VOID
    RddSetDefault("DBFCDX")
    ? DbCreate("TEst", {{"FLD1","C",10,0}})
    ? DbUseArea(TRUE, , "Test")
    ? OrdCreate("TEST","FLD1","FLD1")
    ? OrdSetFocus()
    ? OrdSetFocus()
    ? DbCloseArea()

    WAIT
    RETURN


FUNCTION MemWalker(pMem as IntPtr, nSize as DWORD) AS LOGIC
    ? "Walk", pMem, nSize
    RETURN TRUE
