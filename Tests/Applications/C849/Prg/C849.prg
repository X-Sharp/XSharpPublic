// 849. Problem passing NULL to function acceptin USUAL

// System.InvalidCastException
// Unable to cast object of type 'System.IntPtr' to type 'SomeClass'.

FUNCTION Start() AS VOID 
SetParent((OBJECT) NULL_OBJECT )
SetParent((OBJECT) NULL )
SetParent( NULL_OBJECT )
SetParent( NULL )

FUNCTION SetParent(p) AS VOID
LOCAL o AS SomeClass
o := p
xAssert(o == NULL)

LOCAL a AS System.Collections.ArrayList
a := p
xAssert(a == NULL)

CLASS SomeClass
END CLASS



PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN
