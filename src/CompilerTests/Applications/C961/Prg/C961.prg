// 960. Late bound call uses a STRICT parent method instead of NEW CLIPPER in the child class [#1286]
// https://github.com/X-Sharp/XSharpPublic/issues/1286

CLASS Parent
	METHOD Foo() AS STRING STRICT
		? "foo parent strict"
	RETURN "foo parent strict"
	METHOD Bar() CLIPPER
		? "bar parent clipper"
	RETURN "bar parent clipper"
END CLASS

CLASS Child INHERIT Parent
	NEW METHOD Foo() CLIPPER
		? "new foo child clipper"
	RETURN "new foo child clipper"
	NEW METHOD Bar() AS STRING STRICT
		? "new bar child strict"
	RETURN "new bar child strict"
END CLASS

FUNCTION Start( ) AS VOID
	LOCAL u := Child{}
	u:Foo()
	u:Bar()

	// both calls should call the NEW methods in Child class
	xAssert( u:Foo() == "new foo child clipper")
	xAssert( u:Bar() == "new bar child strict")
	
PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
