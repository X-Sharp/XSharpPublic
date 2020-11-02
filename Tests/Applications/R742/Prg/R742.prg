// R742: Allow parenthesized index access. When both an array and a function exist then FoxPro calls the array


FUNCTION Start( ) AS VOID
    DIMENSION test[3] 
    DECLARE test2(3)  
    test := {42,43,44}
    test2 := {42,43,44} 
    xAssert(test[1] =42)
    xAssert(test(1) =42)
    xAssert(test2[1] =42)
    xAssert(test2(1) =42)    
    Tester{}:TestMethod()
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


FUNCTION test(nNum AS LONG)
    THROW Exception{"Function should not be called"}

FUNCTION test2(nNum AS LONG)
    THROW Exception{"Function should not be called"}
                                                                            

CLASS Tester
    PROTECTED aValues AS ARRAY
    METHOD TestMethod
        This.aValues := {1,2,3}
        xassert(This.aValues(1) == 1)
        xassert(This.aValues[1] == 1)
        RETURN  FALSE
END CLASS    
