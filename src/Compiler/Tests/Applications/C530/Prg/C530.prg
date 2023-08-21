// 530. error XS0122: 'TestStruc.Test()' is inaccessible due to its protection level
#pragma warnings(165, off) // unassigned

STRUCTURE TestStruc
	EXPORT n AS INT
	METHOD Test() AS INT
	RETURN 555
	METHOD AnotherTest() AS INT
	RETURN SELF:Test()
	PROPERTY MyProp AS INT AUTO

	STATIC PROPERTY SMyProp AS INT AUTO
	STATIC METHOD STest() AS INT
	RETURN 555
END STRUCTURE

FUNCTION Start( ) AS VOID
	LOCAL s AS TestStruc
	s:n := 123 // ok
	? s:n // ok
	? s:Test() // error
	s:MyProp := 456 // error
	? s:MyProp // error

	TestStruc.SMyProp := 1 // ok
	TestStruc.STest() // ok
RETURN

