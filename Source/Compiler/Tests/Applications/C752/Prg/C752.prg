// 752. error XS0656: Missing compiler required member 'Microsoft.CSharp.RuntimeBinder.Binder.IsEvent'
// lb+
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	u := Foo{}
	u:Bar += 1
	? u:Bar
	xAssert(u:Bar == 1)
	u:Bar -= 1
	u:BAr ++
	u:BAr ++
	xAssert(u:Bar == 2)
	u:c += "B"
	xAssert(u:c == "AB")
	
CLASS Foo
	EXPORT Bar AS INT
	EXPORT c := "A"
END CLASS


PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
