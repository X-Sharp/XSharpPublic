// 884. MemVarPut()/MemVarGet() incorrect visibility
// https://github.com/X-Sharp/XSharpPublic/issues/1223

GLOBAL gcMemVarName AS STRING

FUNCTION Start() AS VOID
DoTest_1()
?
DoTest_2()
?
DoTest_3()

?
DoTest_4()

PROCEDURE DoTest_1()
	? "No MEMVAR statement"
	gcMemVarName := "MyMemVar_1"
	CreateMemVarInAFunction_1( )

	TRY
		? "Memvar outside the function:"
		? MemVarGet( gcMemVarName ) // Should throw exception
		xAssert(FALSE)
	CATCH e AS XSharp.Error
	    xAssert(TRUE)
	END TRY


PROCEDURE CreateMemVarInAFunction_1( )
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_1()
PROCEDURE CheckInChild_1()
	TRY
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName ) // testing123
	CATCH e AS XSharp.Error
    	xAssert(FALSE)
	END TRY




PROCEDURE DoTest_2()
	gcMemVarName := "MyMemVar_2"
	? "using MEMVAR JustARandomMemvar"

	CreateMemVarInAFunction_2( )

	TRY
		? "Memvar outside the function:"
		? MemVarGet( gcMemVarName ) // Should throw exception
		xAssert(FALSE)
	CATCH e AS XSharp.Error
    	xAssert(TRUE)
	END TRY
PROCEDURE CreateMemVarInAFunction_2( )
	MEMVAR JustARandomMemvar
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_2()

PROCEDURE CheckInChild_2()
	TRY
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName )
		xAssert(TRUE)
	CATCH e AS XSharp.Error
	    XAssert(FALSE)
	END TRY



PROCEDURE DoTest_3()
	? "using MEMVAR MyMemVar"
	gcMemVarName := "MyMemVar_3"
	CreateMemVarInAFunction_3( )

	TRY
		? "Memvar outside the function:"
		?  MemVarGet( gcMemVarName ) // Should throw exception
		xAssert(FALSE)
	CATCH e AS XSharp.Error
	    XAssert(TRUE)
	END TRY

PROCEDURE CreateMemVarInAFunction_3( )
	MEMVAR MyMemVar_3
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_3()

PROCEDURE CheckInChild_3()
	TRY
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName )
		xAssert(TRUE)
	CATCH e AS XSharp.Error
    	xAssert(FALSE)
	END TRY

PROCEDURE DoTest_4()
	gcMemVarName := "MyMemVar_2"
	? "Create Memvar with codeblock"
	MacroTest()

	TRY
		? "Memvar outside the function:"
		? MemVarGet( gcMemVarName ) // Should throw exception
		xAssert(FALSE)
	CATCH e AS XSharp.Error
    	xAssert(TRUE)
	END TRY
    PUBLIC &gcMemVarName
	MacroTest()

	TRY
		? "Try again with Public "
		? MemVarGet( gcMemVarName )
		xAssert(TRUE)
	CATCH e AS XSharp.Error
    	xAssert(FALSE)
	END TRY


PROCEDURE MacroTest
    LOCAL oCodeBlock AS CODEBLOCK
    oCodeBlock := GetCodeBlock()
    oCodeBlock:Eval()
    ? MemVarGet( gcMemVarName ) // testinblock, OK

FUNCTION GetCodeBlock() AS CODEBLOCK
    RETURN {||MemVarPut(gcMemVarName,"testinblock")}


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		? "Incorrect result!!!!!!"
		THROW Exception{"Incorrect result"}
	END IF
RETURN

FUNCTION DoNothing() AS STRING
    // This should not generate a MemVarInit()
    RETURN "Nothing"
