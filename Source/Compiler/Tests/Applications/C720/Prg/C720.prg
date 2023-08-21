// error XS0121: The call is ambiguous between the following methods or properties: 'Functions.Test(dword, dword)' and 'Functions.Test(int, int)'
// note that this must now be compiled in the VO dialect.
// in core dialect there is a problem.
FUNCTION Start() AS VOID
	Test(1 , SLen("")) // OK
	Test(1 , SLen("") - 2) // error XS0121
RETURN

FUNCTION Test(dw1 AS DWORD , dw2 AS DWORD) AS VOID
FUNCTION Test(int1 AS INT , int2 AS INT) AS VOID

