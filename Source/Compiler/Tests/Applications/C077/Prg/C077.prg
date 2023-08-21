// 77. incorrect warning XS0168: The variable 'n' is declared but never used
#pragma warnings(9101, off) // try without catch
FUNCTION Test() AS VOID
	LOCAL n AS INT
	TRY
		n := 1
		? n
	END TRY
RETURN
FUNCTION Start() AS VOID
Test()
