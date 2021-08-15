// https://github.com/X-Sharp/XSharpPublic/issues/755
// 794. Compiler crash with circular const assignment
// Process is terminated due to StackOverflowException. 
DEFINE FOO := foo  +1
STATIC CLASS Class1          
	PUBLIC CONST MyConst := Class1.MyConst AS STRING
END CLASS

FUNCTION Start( ) AS VOID
	? "no point, since it must not compile!"   
	? Foo
RETURN
