// error XS0710: Static classes cannot have instance constructors
// /vo16+
FUNCTION Start( ) AS VOID
	
RETURN

STATIC CLASS TestClass
END CLASS             

PRIVATE CLASS TestClass2 
	STATIC CONSTRUCTOR()
		
END CLASS
