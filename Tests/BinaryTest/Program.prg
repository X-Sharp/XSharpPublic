// 742. Problem passing vars by reference when calling strongly typed methods late bound

// In the following code, there are no compiler errors, but the vars passed by reference do not get updated when returning to the caller code
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	u := TestClass{}
	LOCAL n AS INT   
	n := 1
	u:TestInt(REF n)   
	? n
	xAssert(n == 123)
	n := 1
	u:TestInt(@n)
	? n
	xAssert(n == 123)

	LOCAL l AS LOGIC
	l := FALSE
	u:TestLogic(REF l)
	? l
	xAssert(l == TRUE)
	l := FALSE
	u:TestLogic(@l)
	? l
	xAssert(l == TRUE)

	LOCAL c AS STRING
	c := ""
	u:TestString(REF c)
	? c
	xAssert(c == "changed")
	c := ""
	u:TestString(@c)
	? c
	xAssert(c == "changed")
    WAIT

RETURN

CLASS TestClass
	METHOD TestInt(n REF INT, c := "" AS STRING) AS VOID
	n := 123
	METHOD TestLogic(l REF LOGIC, d := NULL_DATE as Date) AS VOID
	l := TRUE
	METHOD TestString(c REF STRING, s := #somesymbol as symbol) AS VOID
	c := "changed"
	
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
