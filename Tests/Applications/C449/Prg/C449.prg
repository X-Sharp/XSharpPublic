// error XS7038: Failed to emit module 'C449'.
CLASS Control
	METHOD Font() CLIPPER
	RETURN NIL
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS Control
	o := Control{}

// this reports a correct error that Font() is a method (correct)
//	o:Font:Test := 0 // OK


/*
this reports:

warning XS1656: Cannot assign to 'Font' because it is a 'method'
error XS7038: Failed to emit module 

it should instead report the same error as above and no warning I think
*/
	o:Font := 123
RETURN

