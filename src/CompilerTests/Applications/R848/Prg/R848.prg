// https://github.com/X-Sharp/XSharpPublic/issues/1016
FUNCTION Start( ) AS VOID
    XAssert(testMe(1) == 1)
    XAssert(testMe("a",Today()) == 2)
    XAssert(testMe(NULL_PTR) == 1)
    XAssert(testMe(NULL_PSZ) == 1)
    XAssert(testMe(NULL_SYMBOL) == 1)
    XAssert(testMe(IntPtr.Zero) == 1)
    XAssert(testMe(null_obj) == 1)
    XAssert(testMe(null) == 1)
    XAssert(testMe() == 0)

FUNCTION TestMe (args) AS LONG CLIPPER        
    local i := 0 as long
    foreach var arg in _ARGS()
       ? "Arg", ++i, arg
    next
    ? PCount()
    return PCount()
    
    
    
PROC xAssert(l AS LOGIC) 
	IF .NOT. l
		THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	END IF
	? "Assertion passed"   
RETURN
