// 69. error XS0023: Operator '~' cannot be applied to operand of type 'bool'
FUNCTION Start() AS VOID
LOCAL lVar AS LOGIC
lVar := TRUE
IF !lVar
    NOP
END IF

