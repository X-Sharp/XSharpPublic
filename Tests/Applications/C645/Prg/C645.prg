// 645. Compiler crash with usuals and decimals with the X# runtime
// happens when /vo4+ is enabled
FUNCTION Start() AS VOID
	LOCAL uUsual AS USUAL
	LOCAL lDecimal AS Decimal
	lDecimal := 1.23
	uUsual := lDecimal
	? lDecimal , uUsual
RETURN
