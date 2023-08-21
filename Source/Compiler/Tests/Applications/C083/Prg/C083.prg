// 83. error XS0034: Operator '*' is ambiguous on operands of type 'int' and 'double'
FUNCTION Start() AS VOID
LOCAL y AS INT
y := 1
y:= 1 - INT(y * 1.2)
? y
	
