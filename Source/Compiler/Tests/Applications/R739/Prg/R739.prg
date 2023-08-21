//R739 The compiler does not like the 2 functions below 
// but it crashes in the generation of the error message.
FUNCTION Start( ) AS VOID
	Test()
RETURN


FUNCTION Test(cErr OUT STRING) AS VOID
    cErr := NULL_STRING
    RETURN
	
FUNCTION Test() AS VOID CLIPPER
    Test(OUT NULL)
    RETURN
