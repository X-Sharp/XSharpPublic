// 88. error XS0034: Operator '*' is ambiguous on operands of type 'int' and 'double'
FUNCTION Start() AS VOID
LOCAL n := 2 AS INT
n := (INT) (n * 42.5)
? n

