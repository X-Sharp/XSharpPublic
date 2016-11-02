// 277. error XS0034: Operator '>>' is ambiguous on operands of type 'byte' and 'int'
FUNCTION Start() AS VOID
LOCAL b := 16 AS BYTE
b := b >> 2
? b

