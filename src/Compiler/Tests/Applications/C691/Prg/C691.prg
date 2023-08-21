// 690. Process is terminated due to StackOverflowException.
/*
Following causes the compiler to crash with an internal overflow, instead of reporting an error.
Very interestingly, this causes XIDE (intelisense) to crash as well, with the same error!
Apparently we are both doing somthing similar when trying to resolve this...
(so avoid invoking any intelisense while you may have this code open in XIDE)
*/
FUNCTION Start( ) AS VOID
	LOCAL obj AS OBJECT
	obj := TestClass{}
	VAR testclass := TestClass(obj) // StackOverflowException

RETURN
FUNCTION Start2( ) AS VOID
	LOCAL obj AS OBJECT
	obj := TestClass{}
	VAR testclass := (TestClass)obj // ok
RETURN

CLASS TestClass
END CLASS
