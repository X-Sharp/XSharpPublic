// 893. Tuples
FUNCTION Start() AS VOID
	LOCAL t2 AS Tuple(INT, STRING)
	t2 := Tuple{123,"abc"}
	? t2
	? t2:Item1
	? t2:Item2
	xAssert(t2:Item1 == 123)
	xAssert(t2:Item2 == "abc")

	LOCAL t2b AS (INT,STRING)
	t2b := t2
	xAssert(t2b:Item1 == 123)
	xAssert(t2b:Item2 == "abc")
	
	LOCAL t3 AS (i1 AS INT, i2 AS LOGIC, i3 AS INT)
	t3 := GetTuple3(t2)
	xAssert(t3:i1 == 123)
	xAssert(t3:i2 == TRUE)
	xAssert(t3:i3 == 3)
	
	GetTupleByRef(OUT t2, REF t3)
	xAssert(t2:Item1 == 0)
	xAssert(t2:Item2 == "test")
	xAssert(t3:i1 == -1)
	xAssert(t3:i2 == FALSE)
	xAssert(t3:i3 == -2)
	
	LOCAL n := 1 AS INT
	LOCAL c := "a" AS STRING
	VAR t2c := Tuple{n,c}
	xAssert(t2c:n == 1)
	xAssert(t2c:c == "a")
	
	t2c:n := 555
	t2c:c := "555"
	(n,c) := t2c
	xAssert(n == 555)
	xAssert(c == "555")

RETURN


FUNCTION GetTuple3(t2 AS Tuple(INT,STRING) ) AS (INT,LOGIC,INT)
RETURN Tuple{t2:Item1, t2:Item2:Length == 3, t2:Item2:Length}

FUNCTION GetTupleByRef(t2 OUT TUPLE(INT, STRING), t3 REF (INT,LOGIC,INT) ) AS VOID
	t2 := TUPLE{0,"test"}
	t3 := TUPLE{-1,FALSE,-2}


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


