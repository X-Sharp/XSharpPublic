// 779. Internal compiler error with Default(USUAL) used as argument
// https://github.com/X-Sharp/XSharpPublic/issues/664
#pragma warnings(9066, off) //   ambiguous
FUNCTION Start() AS VOID
LOCAL cb AS CODEBLOCK
cb := {|a| IsNil(a)}
? Eval(cb, NIL)
? Eval(cb, NIL , NIL , NIL)
? Eval(cb, Default(USUAL))
? Eval(cb, Default(USUAL),Default(USUAL))  // internal compiler error
? Eval(cb, Default(USUAL),Default(USUAL),Default(USUAL))  // internal compiler error

xAssert( Eval(cb, NIL) )
xAssert( Eval(cb, NIL , NIL , NIL) )
xAssert( Eval(cb, Default(USUAL) ) )
xAssert( Eval(cb, Default(USUAL) , Default(USUAL) ) )
xAssert( Eval(cb, Default(USUAL) , Default(USUAL) , Default(USUAL) ) )


CLASS OriginalReport
METHOD Eval(cbBlock AS USUAL) AS USUAL STRICT
RETURN Eval(cbBlock,Default(USUAL),Default(USUAL),Default(USUAL))

METHOD Eval(cbBlock AS USUAL, cbForBlock AS USUAL, cbWhileBlock AS USUAL, uScope AS USUAL) AS USUAL STRICT
RETURN Default(USUAL)
END CLASS


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
