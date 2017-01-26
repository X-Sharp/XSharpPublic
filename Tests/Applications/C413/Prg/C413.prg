// 413. error XS0121: The call is ambiguous between the following methods or properties: 'Math.Round(real8, int)' and 'Math.Round(decimal, int)'
FUNCTION Start() AS VOID
	LOCAL f := 123.456 AS FLOAT
	? Math.Round(f,2)
	? Math.Round((FLOAT)1.2,2)
RETURN
