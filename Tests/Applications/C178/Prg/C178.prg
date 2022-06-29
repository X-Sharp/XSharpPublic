// 178. error XS1660: Cannot convert lambda expression to type '__Usual' because it is not a delegate type
#pragma warnings(219, off) // assigned but never used
FUNCTION Start( ) AS VOID
LOCAL u AS USUAL
u := {|n|n+1}
SomeFunct({|a,b| a > b})

FUNCTION SomeFunct(u) CLIPPER
	? Eval(u,1,2)
	? Eval(u,2,1)
RETURN NIL

