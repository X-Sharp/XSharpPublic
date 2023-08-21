// R742: Allow parenthesized index access. When both an array and a function exist then FoxPro calls the array
// compile with /memvar /undeclared /fox2
#pragma warnings(9073, off) // undeclared, field or memvar
FUNCTION Start( ) AS VOID
    DoTest1()
    PUBLIC ARRAY Test(3)
    Test := 42 // Fill public with 42
    DoTest2()
    PUBLIC Test3
    Test3 := Today()
    PUBLIC Test3a
    Test3a := Today()
    DoTest3()
    Tester{}:TestMethod()
RETURN

FUNCTION DoTest1() AS VOID
    // These variables can be resolved at compile time
    // The compiler sees that test, test1 and test2 are foxpro arrays
    DIMENSION test[3]
    LOCAL ARRAY test1[3]
    DECLARE test2(3)
    test := 42              // Fill array with 42
    test1 := 43              // Fill array with 42
    test2 := 44             // Fill array with 43
    xAssert(test[1] ==42)
    test[1] = 42
    test(1) = 41
    xAssert(test(1) =41)
    test1(1) = 43
    xAssert(test1[1] ==43)
    xAssert(test1(1) ==43)
    xAssert(test2[1] ==44)
    xAssert(test2(1) ==44)

FUNCTION DoTest2 AS VOID
    // test[1] will be resolved at compile time to __VarGet("test")[1]
    xAssert(test[1] ==42)
    // Since Test is not declared locally it cannot be resolved at compile time
    // Therefore the call to test(1) must be resolved at runtime by calliong __FoxArrayAccess()
    xAssert(test(1) ==42)
    RETURN

FUNCTION DoTest3 AS VOID
    // Since Test is not declared locally it cannot be resolved at compile time
    xAssert(Test3(1) == 1)
    xAssertThrow({|| Test3a(1) == 1})    // This should throw an error.
    xAssert(Test3b(42) == 42)           // this is not a memory variable but a function
    RETURN



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

PROC xAssertThrow(cb AS CODEBLOCK)
LOCAL lOk AS LOGIC
TRY
    Eval(cb)
    lOk := FALSE
CATCH
    lOk := TRUE
END TRY

IF ! lOk
    THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
ENDIF
RETURN



FUNCTION test(nNum AS LONG)
    THROW Exception{"Function should not be called"}

FUNCTION test2(nNum AS LONG)
    THROW Exception{"Function should not be called"}

FUNCTION test3(nNum AS LONG)
    // this function will be called inside DoTest3()
    RETURN nNum

FUNCTION test3b(nNum AS LONG)
    // this function will be called inside DoTest3()
    RETURN nNum



CLASS Tester
    PROTECTED aValues AS ARRAY
    METHOD TestMethod
        This.aValues := {1,2,3}
        //xassert(This.aValues(1) == 1)
        xassert(This.aValues[1] == 1)
        RETURN  FALSE
END CLASS

