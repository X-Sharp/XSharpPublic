// https://github.com/X-Sharp/XSharpPublic/issues/805
// does not work in RuntimeTests because macro compiler needs to be initialized
// correctly
FUNCTION Start() AS VOID
LOCAL ARRAY a(3)
PUBLIC ARRAY b(42)
? Type ( "a(1)" )
? Type ( "a[1]" )
xAssert(Type ( "a(1)" ) == "L")
xAssert(Type ( "a[1]" ) == "L")
// fill the array with dates
a := Today()
? Type ( "a(1)" )
? Type ( "a[1]" )
xAssert(Type ( "a(1)" ) == "D")
xAssert(Type ( "a[1]" ) == "D")

// 3 params should call the function
? Type ( "b(1,2,3)" )
// 2 params for single dimensional array: call the function
? Type ( "b(1,2)" )
// 1 params for single dimensional array: array element
? Type ( "b(1)" )
xAssert(Type ( "b(1)" ) == "L")          // 1 dim array, so array element
xAssert(Type ( "b(1,2)" ) == "C")        // 1 dim array, so function call
xAssert(Type ( "b(1,2,3)" ) == "C")      // 1 dim array, so function call

dimension b(6,7)
xAssert(Type ( "b(1)" ) == "L")            // 2 dim array, so array element
xAssert(Type ( "b(1,2)" ) == "L")          // 2 dim array, so array element
xAssert(Type ( "b(1,2,3)" ) == "C")        // 3 params so function call


RETURN


FUNCTION b(n) AS STRING
    RETURN AsString(n)



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

