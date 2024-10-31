// 926. Macro compiler cannot compile strongly typed codeblocks
// https://github.com/X-Sharp/XSharpPublic/issues/1591

FUNCTION Start() AS VOID
LOCAL cb AS CODEBLOCK

cb := MCompile( "{|a , b | a * b}" ) // OK
? Eval(cb ,3,4)
xAssert(Eval(cb ,3,4) == 12)

cb := MCompile( "{|a AS INT , b AS INT | a * b}" ) // Macrocompiler (1,5): error XM0100: Expected '|'
? Eval(cb ,4,5)
xAssert(Eval(cb ,4,5) == 20)

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
