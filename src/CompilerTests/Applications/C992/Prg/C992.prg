// 992. [CallerLineNumber] for a call without arguments
// Every call without arguments got the line of the last call without arguments in the compilation
// (in a compiler server build even of another project): all of them shared one empty argument list.
USING System.Runtime.CompilerServices

GLOBAL gnLine AS INT

FUNCTION LineOf(n := 0 AS INT, [CallerLineNumber] nLine := 0 AS INT) AS INT
RETURN nLine

FUNCTION FileOf([CallerFilePath] cFile := "" AS STRING) AS STRING
RETURN cFile

PROCEDURE SetLine([CallerLineNumber] nLine := 0 AS INT)
	gnLine := nLine
RETURN

CLASS Caller
	METHOD LineOf([CallerMemberName] cMember := "" AS STRING, [CallerLineNumber] nLine := 0 AS INT) AS INT
		xAssert(cMember == "Typed" .or. cMember == "Untyped")
	RETURN nLine

	METHOD Typed() AS VOID
		xAssert(SELF:LineOf() == 24)
		xAssert(SELF:LineOf() == 25)

	METHOD Untyped(a, b) AS VOID CLIPPER
		IF a == 1
			xAssert(SELF:LineOf() == 29)
		ENDIF
END CLASS

CLASS Base
	PROPERTY Line AS INT AUTO
	CONSTRUCTOR([CallerLineNumber] nLine := 0 AS INT)
		SELF:Line := nLine
END CLASS

CLASS Derived INHERIT Base
	CONSTRUCTOR()
		SUPER()
END CLASS

FUNCTION Start() AS VOID
	xAssert(LineOf() == 45)
	xAssert(LineOf(1) == 46)
	xAssert(LineOf() == 47)
	Caller{}:Typed()
	Caller{}:Untyped(1, 2)
	xAssert(Derived{}:Line == 41)
	DO SetLine
	xAssert(gnLine == 51)
	xAssert(FileOf():EndsWith("C992.prg", StringComparison.OrdinalIgnoreCase))
	xAssert(LineOf() == 54)
RETURN

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
ENDIF
? "Assertion passed"
RETURN
