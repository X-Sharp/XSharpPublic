// R914 - Mark classes for static defines and functions as compilergenerated
// and also allow same named files in different folders by adding
// a hashcode of the path to the generated classname.
FUNCTION Start( ) AS VOID
	xAssert(Test1() == 1)
	xAssert(Test2() == 2)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
