// 129. Assertion failed at BetterConversionExpression...
// using REF for the last argument gets rid of the failed assertion
#pragma warnings(9071, off)   //  Parameter 3 needs a(n) 'Out' modifier. This modifier was automatically added.
FUNCTION Start() AS VOID
LOCAL n,m,k AS INT
n := 1;m := 1;k := 1
? Math.DivRem( n, m, k )
? k

