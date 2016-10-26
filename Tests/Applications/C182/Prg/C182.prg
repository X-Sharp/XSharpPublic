// 182. error XS0034: Operator '>' is ambiguous on operands of type 'uint' and 'int'

// /vo4+ enabled
FUNCTION Start( ) AS VOID
LOCAL d := 0 AS DWORD
? d > 0
? d != 1
? d < 2L
? d + 3
d += 4 

