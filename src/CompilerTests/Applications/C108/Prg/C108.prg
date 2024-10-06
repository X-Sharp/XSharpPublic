// 108. error XS1061: 'ITest' does not contain a definition for 'StaticMethod'


INTERFACE ITest
	METHOD Foo() AS VOID
END INTERFACE

CLASS Test
	STATIC METHOD StaticMethod() AS VOID
END CLASS

#pragma options (allowdot, on)
FUNCTION Start() AS VOID
LOCAL Test AS ITest
Test := NULL
Test.StaticMethod()
? (OBJECT) Test


#pragma options (allowdot, off)
FUNCTION Start2() AS VOID
LOCAL Test AS ITest
Test := NULL
Test.StaticMethod()
? (OBJECT) Test


