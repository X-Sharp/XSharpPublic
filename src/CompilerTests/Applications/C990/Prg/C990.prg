// 990. xsc crashes with IndexOutOfRangeException when an analyzer and an .editorconfig are passed
// and the compilation has more than one compiler generated syntax tree.
// The literal symbol below makes the compiler generate a tree for the symbol table, on top of the
// default tree for the VO dialect. The app is compiled with /analyzer (Bin\Analyzers\XSharpTestAnalyzer.dll)
// and /analyzerconfig (Applications\C990\.editorconfig), see the Switches in the test project.

FUNCTION Start( ) AS VOID
	LOCAL s AS SYMBOL
	s := #C990
	? s
	xAssert( Symbol2String(s) == "C990" )
RETURN

PROC xAssert(l AS LOGIC) AS VOID
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
