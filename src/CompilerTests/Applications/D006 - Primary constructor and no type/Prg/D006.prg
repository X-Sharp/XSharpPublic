//Must report error class with no type on the last paremeter
//Works on VO dialect with /vo15
FUNCTION Start( ) AS VOID
	System.Console.WriteLine("Must report error")
RETURN

CLASS Test (a)
    STATIC x AS INT
END CLASS
