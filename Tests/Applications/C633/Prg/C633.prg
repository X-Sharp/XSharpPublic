// 633. error XS0121: The call is ambiguous between the following methods or properties: 'XSharp.VO.Functions.Transform(int, string)' and 'XSharp.VO.Functions.Transform(XSharp.__VOFloat, string)'
FUNCTION Start() AS VOID
	? Transform(Year(Today()) , "####")
	? Transform(123U , "####")
	? Transform(BYTE(123) , "####")
	? Transform(WORD(123) , "####")
	
	LOCAL uArray,u AS USUAL
	LOCAL aArray AS ARRAY
	uArray := aArray := {1,2,3}
	u := 2
	
	? AScan(uArray , u) // error
	? AScan(aArray , u) // ok
	? AScan(uArray , 123) // ok
	? AScan(aArray , 123) // ok
RETURN
