// There is no warning reported by the compiler now for using REF instead of OUT
FUNCTION Start() AS VOID STRICT

	LOCAL x := "" AS STRING
	TestFuncOut(REF x)
//	TestFuncRef(OUT x) // this one does throw an error (not warning)
    ? x
	RETURN

FUNCTION TestFuncOut(x OUT STRING) AS VOID STRICT
	x := "test"
	RETURN
FUNCTION TestFuncRef(x REF STRING) AS VOID STRICT
	x := "test"
	RETURN
