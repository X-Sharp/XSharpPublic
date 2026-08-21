// R788 LOCAL and Dimension. 
// https://github.com/X-Sharp/XSharpPublic/issues/683
// https://github.com/X-Sharp/XSharpPublic/issues/2063
FUNCTION Start( ) AS VOID
    LOCAL aLocal
    // warning XS0168: The variable 'aLocal' is declared but never used

    // That's not correct, the dimension below should use the aLocal and turn in into an array (confirmed with VFP test)
    Dimension aLocal(10) 
    

    Dimension aPublic(10) 
    
    ? ALen(aLocal)
    xAssert(ALen(aLocal) == 10)

    ? ALen(aPublic)
    xAssert(ALen(aPublic) == 10)
    
    testnested()
RETURN

procedure testnested() AS VOID
	? ALen(aPublic) 

	? ALen(aLocal) // there should be an exception here, var is a local of the calling function

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
