FUNCTION Start as VOID
    LOCAL p AS BYTE PTR
    MemTrace(TRUE)
    p := MemAlloc(10)
    MemGrpEnum(1, MemWalker)
    p[1] := 42
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 8)
    MemGrpEnum(1, MemWalker)
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 10)
    MemGrpEnum(1, MemWalker)
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 1)
    MemGrpEnum(1, MemWalker)
    //? p, p[1], MemTotal()

    WAIT
    RETURN


FUNCTION MemWalker(pMem as IntPtr, nSize as DWORD) AS LOGIC
    ? "Walk", pMem, nSize
    RETURN TRUE
