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
	IF ((UsualType(file) == INT) .and. (file != 123)) .or. ;
		((UsualType(file) == STRING) .and. ! file == "TEST") .or. ;
		(UsualType(file) != INT .and. UsualType(file) != STRING)
		THROW Exception{"Incorrect value passed to constructor"}
	END IF
	METHOD Test(file) CLIPPER
	? "from method:", file
	IF ((UsualType(file) == INT) .and. (file != 123)) .or. ;
		((UsualType(file) == STRING) .and. ! file == "TEST") .or. ;
		(UsualType(file) != INT .and. UsualType(file) != STRING)
		THROW Exception{"Incorrect value passed to constructor"}
	END IF
	RETURN NIL
END CLASS

