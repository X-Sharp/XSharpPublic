// Second Test for https://github.com/X-Sharp/XSharpPublic/issues/1243
#pragma options("vo16", on)
#pragma options("lb", on)
// #pragma options("xpp1", on) // unrecognized
#pragma warnings(9201, disable)

PROCEDURE main()
LOCAL o
    
    o := DerivedExample():NEW()
    
    ? o:ClassName()
    xAssert( o:ClassName() == "DerivedExample" )
    ? o:IsDerivedFrom("Example")
    xAssert( o:IsDerivedFrom("Example") )
    RETURN

CLASS DerivedExample FROM Example
endclass

CLASS Example
endclass

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
