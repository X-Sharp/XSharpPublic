// See https://github.com/X-Sharp/XSharpPublic/issues/586
// OVERRIDE and NEW should raise an error when not needed.
#pragma options("vo3", on)
FUNCTION Start( ) AS VOID
	XX{}:Funky()
	YY{}:Funky()
	YY{}:Funky2()
RETURN


CLASS Xx   

PUBLIC METHOD Funky() AS VOID
    ? "Funky XXXX"
    RETURN
    
END CLASS

CLASS Yy INHERIT Xx

PUBLIC OVERRIDE METHOD Funky() AS VOID
    ? "Funky YYYY"
    RETURN

PUBLIC OVERRIDE METHOD Funky2() AS VOID
    ? "Funky2 YYYY"
    RETURN

PUBLIC NEW METHOD Funky3() AS VOID
    ? "Funky3 YYYY"
    RETURN

END CLASS
