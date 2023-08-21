// 256. error XS1977: Cannot use a lambda expression as an argument to a dynamically dispatched operation without first casting it to a delegate or expression tree type.
CLASS TestClass
METHOD Test(a,b)
? "tested"
RETURN NIL
END CLASS

FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := TestClass{}
u:Test( {|o| o:Test() } )

