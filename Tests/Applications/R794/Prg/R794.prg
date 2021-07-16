// See https://github.com/X-Sharp/XSharpPublic/issues/586
// OVERRIDE and NEW should raise an error when not needed.

FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Hello x#!")
RETURN


CLASS Xx
END CLASS

CLASS Yy INHERIT Xx

PUBLIC OVERRIDE METHOD Funky() AS VOID
    System.Diagnostics.Debug.WriteLine("Funky YYYY")
    RETURN

END CLASS
