// 419. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.Test(__Array)' and 'Functions.Test(ArrayList)'
FUNCTION Start( ) AS VOID
	Test(NULL_ARRAY)
	Test(NULL_PSZ)
	Test(NULL_DATE)
	Test(NULL_CODEBLOCK)
RETURN

FUNCTION Test(a AS ARRAY) AS VOID
	? a == NULL_ARRAY
	TRY
		? a // vulcan does throw an exception here (object reference not set to instance of an object)
	CATCH e AS Exception
		? e:Message
	END TRY
FUNCTION Test(a AS System.Collections.ArrayList) AS VOID
FUNCTION Test(a AS System.AppDomain) AS VOID

FUNCTION Test(p AS PSZ) AS VOID
? "PSZ", p == NULL_PSZ
FUNCTION Test(d AS DATE) AS VOID
? "date", d == NULL_DATE
FUNCTION Test(c AS CODEBLOCK) AS VOID
? "codeblock", c == NULL_CODEBLOCK

