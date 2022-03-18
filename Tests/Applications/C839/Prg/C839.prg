// 839. Problem confusing a function/method call with array access
// /fox2+
// https://github.com/X-Sharp/XSharpPublic/issues/988
FUNCTION TestSomething(o1,o2)
? o1,o2
RETURN 123

FUNCTION Start() AS VOID
TestSomething()
TestSomething(1)   // error XS0103 with /undeclared- disabled, warning 9073 otherwise
TestSomething(1,2) // error XS0103 with /undeclared- disabled, warning 9073 otherwise

LOCAL o
o := TestSomething(1)

TestClass{}

CLASS TestClass
CONSTRUCTOR()
Test(1,2) // same here, and runtime error "Variable 'Test' does not exist" when compiled with /undeclared+
METHOD Test(a,b)
? a,b           
RETURN a+b
END CLASS
