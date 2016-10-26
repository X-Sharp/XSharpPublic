// 95. error XS0034: Operator '>' is ambiguous on operands of type 'float' and 'double'
FUNCTION Start() AS VOID
LOCAL r4 AS REAL4
LOCAL r8 AS REAL8
r4 := 0.0
r8 := 0.0
? r4 > 0.5 // error XS0034
? r4 > r8  // no error here!

