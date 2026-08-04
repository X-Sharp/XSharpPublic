// 975. Failed to emit module problem with incorrect code and LOCAL PROCEDURE [#2031]
// https://github.com/X-Sharp/XSharpPublic/issues/2031

// VO dialect
FUNCTION Start( ) AS VOID
RETURN

CLASS TestClass
	METHOD Foo( o AS TestClass ) AS VOID
		
	LocProc( o )		

	LOCAL PROCEDURE LocProc( oo AS TestClass )		

	END PROCEDURE

	? doesnotexist

	o:Bar( )
	END METHOD

	METHOD Bar() CLIPPER
	RETURN NIL
END CLASS


