// 182. error XS0034: Operator '>' is ambiguous on operands of type 'uint' and 'int'

// /vo4+ enabled
#pragma warnings(9021, off) // conversion mey lead to loss of data
#pragma warnings(9020, off) // narrowing conversion mey lead to loss of data
FUNCTION Start( ) AS VOID
LOCAL d := 0 AS DWORD
LOCAL i := 1 AS LONG
? d > 0
? d != 1
? d < 2L
? d + 3
d += 4
d := d + 4
d += i
d := d + i
