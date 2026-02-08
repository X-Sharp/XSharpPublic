// 951. Macrocompiler does not work with types which implements interface
// https://github.com/X-Sharp/XSharpPublic/issues/1742
PUBLIC INTERFACE ITest
   PUBLIC METHOD TestMethod1(owin, udb, uParamsInit) AS USUAL CLIPPER
   PUBLIC METHOD TestMethod2(oWin := NIL AS USUAL, uDb := NIL AS USUAL, uParamsInit := NIL AS USUAL) AS USUAL
END INTERFACE

PUBLIC CLASS Test IMPLEMENTS ITest
	PUBLIC METHOD TestMethod1(owin, udb, uParamsInit) AS USUAL CLIPPER
		? oWin,udb,uParamsInit
	RETURN oWin * uDb
	
	PUBLIC METHOD TestMethod2(oWin := NIL AS USUAL, uDb := NIL AS USUAL, uParamsInit := NIL AS USUAL) AS USUAL
		? oWin,udb,uParamsInit
	RETURN oWin + uDb
END CLASS

FUNCTION GetTest() AS Test
	VAR o := Test{}
	RETURN o
END FUNCTION



FUNCTION Start() AS VOID STRICT
	VAR c := "{||GetTest():TestMethod1(2, 3)}"

	LOCAL ocb AS _CodeBlock

	ocb := &( c )

	? Eval(ocb)
	xAssert( Eval(ocb) == 6 )

	c := "{||GetTest():TestMethod2(3, 2)}"
	ocb := &( c )

	? Eval(ocb)
	xAssert( Eval(ocb) == 5 )

PROC xAssert(l AS LOGIC) AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

