// 129. Assertion failed at BetterConversionExpression...
// using REF for the last argument gets rid of the failed assertion
FUNCTION Start() AS VOID
LOCAL n,m,k AS INT
n := 1;m := 1;k := 1
? Math.DivRem( n, m, k )

