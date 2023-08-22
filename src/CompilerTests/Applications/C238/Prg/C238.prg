// 238. error XS0034: Operator '+' is ambiguous on operands of type 'int' and 'uint'
// witn /vo4+ enabled
#define SomeDefine 6
FUNCTION Start() AS VOID
LOCAL d AS DWORD
d := 123
? SomeDefine + d

