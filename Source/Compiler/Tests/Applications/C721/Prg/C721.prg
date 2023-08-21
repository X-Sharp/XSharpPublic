// any dialect
// error XS0034: Operator '+' is ambiguous on operands of type 'dword' and 'int'
FUNCTION Start() AS VOID
LOCAL d AS DWORD
d := (DWORD) 32*256*256*256+32*256*256+32*256+32
? d
