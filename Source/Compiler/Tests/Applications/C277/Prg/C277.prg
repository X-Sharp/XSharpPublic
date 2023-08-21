// 277. error XS0034: Operator '>>' is ambiguous on operands of type 'byte' and 'int'
// in vulcan this compiles without /vo4. x# requires /vo4+
// we must either allow it in x# without /vo4, too, or document it as a known difference with vulcan
FUNCTION Start() AS VOID
LOCAL b := 16 AS BYTE
b := b >> 2
? b

