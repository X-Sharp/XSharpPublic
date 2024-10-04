// 923. Naming conflicts with /allowdot-
/*
error XS0120: An object reference is required for the non-static field, method, or property 'testStruc.n'
error XS0117: 'VO.Bitmap' does not contain a definition for 'm'
*/


USING VO

CLASS VO.Bitmap
END CLASS

VOSTRUCT testStruc
	MEMBER n AS INT
	MEMBER m AS INT

#pragma options (allowdot, off)
FUNCTION Start( ) AS VOID
	LOCAL testStruc IS testStruc
	testStruc.n := 123 // error XS0120
	? testStruc.n // error XS0120
	
	LOCAL bitmap IS testStruc
	bitmap.m := 456 // error XS0117
	? bitmap.m // error XS0117
RETURN

CLASS TestClass
	PROPERTY System AS LOGIC AUTO
	CONSTRUCTOR()
		System.Diagnostics.Debug.WriteLine( "Hi" )
END CLASS
