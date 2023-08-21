// 516. Not helpful enough parser error message with extra parenthesis
/*
Vulcan reports the descriptive message:
error VN2700: expecting end of statement, found ')'

In x# the parser errors make it very difficult to find the problem in the code:
error XS9002: Parser: unexpected input 'ctemp'
error XS1003: Syntax error, 'NEXT' expected
*/
FUNCTION Start( ) AS VOID
	LOCAL ni,nLen := 1 AS INT
	LOCAL aRet := {{"1","2"}} AS ARRAY
	LOCAL ctemp AS STRING
	FOR ni:=1 UPTO nLen
		ctemp:=aRet[ni,1])
	NEXT
RETURN
