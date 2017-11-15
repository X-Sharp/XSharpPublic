USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID
	local cb1 as codeblock
	XSharp.MacroCompiler.Initialize()
	LOCAL i as INT
	FOR i := 1 to 100
	cb1 := MCompile("{|| 1+2 }")
	? eval(cb1)
	cb1 := MCompile("{||'Hello world'}")
	? eval(cb1)
	cb1 := MCompile("{||ToDay()}")
	? eval(cb1)
	cb1 := MCompile("{||MyToDay()}")
	? eval(cb1)
	NEXT

	
Function MyToday() as USUAL STRICT
RETURN Today()