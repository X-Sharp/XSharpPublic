// 870. Default() function with NULL_OBJECT incompatibility with VO
// https://github.com/X-Sharp/XSharpPublic/issues/1119
FUNCTION Start() AS VOID
LOCAL u AS USUAL


u := NIL
Default(@u , "test")
? IsNil(u) //FALSE, OK
? u == NULL_OBJECT // FALSE OK, this actually causes VO to crash
xAssert( .not. IsNil(u) )


u := NULL_OBJECT

? IsNil(u) // TRUE
? u == NULL_OBJECT // TRUE
xAssert( IsNil(u) )
xAssert( u == NULL_OBJECT )

// this does not modify the value in VO, when it is a NULL_OBJECT
Default(@u , "test")

? IsNil(u) // TRUE again
? u == NULL_OBJECT // TRUE again
xAssert( IsNil(u) )
xAssert( u == NULL_OBJECT )


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
