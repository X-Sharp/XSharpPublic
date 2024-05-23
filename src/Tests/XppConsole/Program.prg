procedure Main()
#ifdef __XSHARP__
    SetNatDll("RUSSIAN.DLL")
    SetInternational(#CLIPPER)
    SetCollation(COLLAT_AMERICAN)
#endif
    ? asc("C"), asc("b"), "C" < "b"
    wait
    return
