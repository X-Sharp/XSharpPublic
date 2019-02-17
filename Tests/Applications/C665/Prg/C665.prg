// 665. Compiler crash with incorrect method call
FUNCTION Start() AS VOID
	LOCAL c AS STRING
	c := c:Trim + "A"
	? c
RETURN
