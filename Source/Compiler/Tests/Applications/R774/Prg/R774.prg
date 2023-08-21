// R774 - Strange arguments behaviour on methods with default parameters
// See https://github.com/X-Sharp/XSharpPublic/issues/629
CLASS MadnessClass

	METHOD TestMadness(a1 AS INT, a2 AS STRING, a3 :=NULL_DATE AS DATE, a4 := FALSE AS LOGIC, a5 := NIL AS USUAL) AS USUAL 

		? a1	// prints random number instead of 5
		? a2	// prints empty string instead of "test1"
		? a3	// empty date - ok
		? a4	// .F. - ok
		? a5	// test2 - ok
        xAssert(a3 == NULL_DATE)
	RETURN a5
	METHOD TestMadness2(a1 AS INT, a2 AS STRING, a3 := 2021.01.01 AS DATE, a4 := FALSE AS LOGIC, a5 := NIL AS USUAL) AS USUAL 

		? a1	// prints random number instead of 5
		? a2	// prints empty string instead of "test1"
		? a3	// empty date - ok
		? a4	// .F. - ok
		? a5	// test2 - ok
        xAssert(a3 == 2021.01.01)
	RETURN a5   
	METHOD Test3( a AS INT,  p:= "abc" AS PSZ, b AS INT) AS USUAL
	    ? p              
	    xAssert(Psz2String(p) == "abc")
	    RETURN p
	METHOD Test3a( p:= "abc" AS PSZ) AS USUAL
	    ? p
        xAssert(Psz2String(p) == "abc")
	    RETURN p

    METHOD Test4( a AS INT,  s:= #sym AS SYMBOL, b AS INT) AS USUAL
        ? s
    	xAssert(s == #Sym)
	    RETURN s

    METHOD Test4a( s:= #sym AS SYMBOL) AS USUAL
        ? s
    	xAssert(s == #Sym)
	    RETURN s

	METHOD RunTest() AS VOID STRICT

		? SELF:TestMadness(5, "test1",,, "test2")
		? SELF:TestMadness2(5, "test3",,, "test4")
		? SELF:Test3(1,,2)
		? SELF:Test3a()
		? SELF:Test4(3,,4)
		? SELF:Test4a()

	RETURN

END CLASS


FUNCTION Start() AS VOID STRICT

	MadnessClass{}:RunTest()



RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 
