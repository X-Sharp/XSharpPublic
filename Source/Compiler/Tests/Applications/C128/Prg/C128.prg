// 128. error: no viable alternative at input 'YIELD'
FUNCTION YieldTest() AS System.Collections.IEnumerable
	FOR LOCAL n := 1 AS INT UPTO 10
		YIELD RETURN n
		IF n > 5
			YIELD BREAK
		END IF
	NEXT
FUNCTION Start() AS VOID
 YieldTest()
RETURN
