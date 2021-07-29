// R797 Ensure typed define has the correct type
// https://github.com/X-Sharp/XSharpPublic/issues/705
STATIC DEFINE DWORD_DEFINE := 0x00000001 AS DWORD

FUNCTION Start() AS VOID STRICT

	LOCAL a AS OBJECT[]
	a := <OBJECT>{ DWORD_DEFINE }
	? a[1]:GetType():ToString() // displays System.Int32 instead System.UInt32
    xAssert(a[1]:GetType() == typeof(DWORD))
	RETURN
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN		



CLASS TextCompiler

	STATIC METHOD doSomething(cParam AS STRING, nParam := DWORD_DEFINE AS DWORD) AS VOID
	RETURN

END CLASS
