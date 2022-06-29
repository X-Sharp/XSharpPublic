// 538. error XS9055: Codeblocks cannot be declared with the Lambda Expression Syntax.
// error XS0019: Operator '>=' cannot be applied to operands of type 'string' and 'string'
#pragma warnings(219, off) // assigned but not used
FUNCTION Start( ) AS VOID
LOCAL a AS ARRAY
a := {65,67,66}
ASort(a,,, {|x,y| Chr(x)>=Chr(y)})
ASort(a,,, {|x,y| Chr(x)<=Chr(y)})
? a[1] , a[2] , a[3]
IF a[2] != 66
	THROW Exception{"Incorrect result"}
END IF

a := { {|n| n + 1} } // error
AAdd(a , { 1, {|| 123} , 3 }) // error

LOCAL u AS USUAL
u := {|n| n + 1} // ok

RETURN

FUNCTION Test() CLIPPER
RETURN NIL

