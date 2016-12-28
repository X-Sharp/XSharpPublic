// 361. runtime problem, usual not passed correctly to the CLIPPER constructor
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	u := 123
	FileSpec{ u }:Test(u)
	u := "TEST"
	FileSpec{ u }:Test(u)

CLASS FileSpec
	CONSTRUCTOR(file) CLIPPER
	? "from constructor:", file
	METHOD Test(file) CLIPPER
	? "from method:", file
	RETURN NIL
END CLASS

