// 794. Compiler crash with circular const assignment
// Process is terminated due to StackOverflowException.
STATIC CLASS Class1
	PUBLIC CONST MyConst := Class1.MyConst AS STRING
END CLASS

FUNCTION Start( ) AS VOID
	? "no point, since it must not compile!"
RETURN
