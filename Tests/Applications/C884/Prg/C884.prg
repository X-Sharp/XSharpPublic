// 884. MemVarPut()/MemVarGet() incorrect visibility
// https://github.com/X-Sharp/XSharpPublic/issues/1223

GLOBAL gcMemVarName AS STRING

FUNCTION Start() AS VOID
DoTest_1()
?
DoTest_2()
?
DoTest_3()

PROCEDURE DoTest_1()
	? "No MEMVAR statement"
	gcMemVarName := "MyMemVar_1"
	CreateMemVarInAFunction_1( )

	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar outside the function:"
		? MemVarGet( gcMemVarName ) // testing123, exception in VO
		lException := FALSE
	END TRY
	xAssert(lException)

PROCEDURE CreateMemVarInAFunction_1( )
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_1()

PROCEDURE CheckInChild_1()
	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName ) // testing123, exception in VO
		lException := FALSE
	END TRY
	xAssert(lException)




PROCEDURE DoTest_2()
	gcMemVarName := "MyMemVar_2"
	? "using MEMVAR JustARandomMemvar"
	
	CreateMemVarInAFunction_2( )

	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar outside the function:"
		? MemVarGet( gcMemVarName ) // exception in both X# and VO
		lException := FALSE
	END TRY
	xAssert(lException)

PROCEDURE CreateMemVarInAFunction_2( )
	MEMVAR JustARandomMemvar
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_2()

PROCEDURE CheckInChild_2()
	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName ) // testing123, exception in VO
		lException := FALSE
	END TRY
	xAssert(lException)



PROCEDURE DoTest_3()
	? "using MEMVAR MyMemVar"
	gcMemVarName := "MyMemVar_3"
	CreateMemVarInAFunction_3( )

	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar outside the function:"
		?  MemVarGet( gcMemVarName ) // exception in both X# and VO
		lException := FALSE
	END TRY
	xAssert(lException)

PROCEDURE CreateMemVarInAFunction_3( )
	MEMVAR MyMemVar_3
	MemVarPut( gcMemVarName, "testing123" )
	? "Memvar inside the function it was created:"
	? MemVarGet( gcMemVarName ) // testing123, OK
	CheckInChild_3()

PROCEDURE CheckInChild_3()
	LOCAL lException := FALSE AS LOGIC
	TRY
		lException := TRUE
		? "Memvar in a child function:"
		? MemVarGet( gcMemVarName ) // testing123, exception in VO
		lException := FALSE
	END TRY
	xAssert(lException)




PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
//		THROW Exception{"Incorrect result"}
		? "Incorrect result!!!!!!"
	END IF
RETURN
