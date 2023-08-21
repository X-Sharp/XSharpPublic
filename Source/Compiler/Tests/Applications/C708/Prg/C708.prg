// 708. Problem resolving identifier inside parens
FUNCTION Start() AS VOID
	LOCAL a AS STRING
	LOCAL b AS STRING
	b := "abc"
	// error XS0023: Operator '+' cannot be applied to operand of type 'string'
	// error XS0118: 'b' is a variable but is used like a type
	a := "123" + (b) + "456"
	? a
RETURN
