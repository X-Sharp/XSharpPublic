// See https://github.com/X-Sharp/XSharpPublic/issues/1531
FUNCTION Start( ) AS VOID

	LOCAL cDbf AS STRING // error XS9091
	cDbf := "test"

	DIMENSION arr(5, 5) AS ARRAY
	arr[1,1] := 123
