// CAUTION! Does not compile in vulcan either..need to find the original code that lead to this report
// Works in X#
// 188. Argument 1: cannot convert from 'Vulcan.__VOFloat' to 'float'
#pragma options("vo4", on)
#pragma warnings(9020, off)
FUNCTION Start() AS VOID
LOCAL f AS FLOAT
f := 1.0
Real4Func(f)

FUNCTION Real4Func(r AS REAL4) AS VOID
? r
