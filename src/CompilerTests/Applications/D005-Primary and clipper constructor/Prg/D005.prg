//Must report error class with both a primary and a clipper constructor
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Must report error")
RETURN

CLASS Test (a AS INT)
    PUBLIC CONSTRUCTOR(a) CLIPPER
END CLASS  
