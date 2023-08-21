// 597. /vo16 does not apply ClipperCallingConvention attribute to constructors inherited from external library

USING System.Reflection
FUNCTION Start( ) AS VOID
	// ClipperCallingConvention applied:
	? TypeOf(Child):GetConstructors()[1]:GetCustomAttribute(TypeOf(Vulcan.Internal.ClipperCallingConventionAttribute))
	// ClipperCallingConvention not applied:
	? TypeOf(ChildDll):GetConstructors()[1]:GetCustomAttribute(TypeOf(Vulcan.Internal.ClipperCallingConventionAttribute))
	
	? CreateInstance("Parent")
	? CreateInstance("Parent" , 1, 2 ,3)

	gTest := 123
	? CreateInstance("Child")
	xAssert(gTest == NIL)
	? CreateInstance("Child" , 1, 2 ,3)
	xAssert(gTest == 1)

	// crash here, "Parameter count mismatch"
	? CreateInstance("ChildDll")
	xAssert(gTest == NIL)
	? CreateInstance("ChildDll" , 1, 2 ,3)
	xAssert(gTest == 1)
RETURN

CLASS Parent
	CONSTRUCTOR(a,b,c)
		? a,b,c
		gTest := a
	RETURN
END CLASS

// ok, CLIPPER constructor generated
CLASS Child INHERIT Parent
END CLASS

// error, generated constructor does not have ClipperCallingConvention attribute
CLASS ChildDll INHERIT ParentDll
END CLASS






PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

