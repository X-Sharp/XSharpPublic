// 237. error XS0034: Operator '+' is ambiguous on operands of type 'int' and 'uint'
// witn /vo4+ enabled
FUNCTION Start() AS VOID
LOCAL d AS DWORD
LOCAL n AS INT
n := 1;d := 1
? _And(0x0000ffff,d)
? _And(0x0000ffff,n)

