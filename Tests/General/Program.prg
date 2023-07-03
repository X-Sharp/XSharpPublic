FUNCTION Start as VOID
    LOCAL p AS BYTE PTR
    #ifdef XSHARP_RT
    MemTrace(TRUE)
    #endif
    p := MemAlloc(10)
    #ifdef XSHARP_RT
    MemGrpEnum(1, MemWalker)
    #endif
    p[1] := 42
   ? p, p[1]
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 8)
   ? p, p[1]
    #ifdef XSHARP_RT
    MemGrpEnum(1, MemWalker)
    #endif
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 10)
   ? p, p[1]
    #ifdef XSHARP_RT
    MemGrpEnum(1, MemWalker)
    #endif
    //? p, p[1], MemTotal()
    p := MemRealloc(p, 1)
   ? p, p[1]
    #ifdef XSHARP_RT
    MemGrpEnum(1, MemWalker)
    #endif
    //? p, p[1], MemTotal()

    WAIT
    RETURN


FUNCTION MemWalker(pMem as IntPtr, nSize as DWORD) AS LOGIC
    ? "Walk", pMem, nSize
    RETURN TRUE
