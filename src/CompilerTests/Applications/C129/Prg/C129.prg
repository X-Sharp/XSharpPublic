// 129. Assertion failed at BetterConversionExpression...
// using REF for the last argument gets rid of the failed assertion
// XS9069: Argument 2 is passed with REF keyword but should be passed with the 'out' keyword
#pragma warnings(9069, off)
FUNCTION Start() AS VOID
LOCAL n,m,k AS INT
n := 1;m := 1;k := 1
? Math.DivRem( n, m, OUT k )
? k

