// 168. error XS0034: Operator '+' is ambiguous on operands of type '__VOFloat' and 'double'
FUNCTION Start() AS VOID
LOCAL f := 1.0 AS FLOAT
LOCAL r := 1.0 AS REAL8
LOCAL s := 1.0 AS REAL4
LOCAL i := 1 AS INT
LOCAL w := 1 AS WORD
LOCAL dw := 1 AS DWORD
LOCAL b := 1 AS BYTE
f := f + 0.5 
f := f + r
f := f + s
f := f + i
f := f - w
f := f - dw
f := f - b
? f

