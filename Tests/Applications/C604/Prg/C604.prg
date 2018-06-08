// 604. iif() returns incorrect data types
FUNCTION Start() AS VOID
	LOCAL l := TRUE AS LOGIC
	
	? iif(l , 1L , 2L):GetType()
	xAssert(iif(l , 1L , 2L):GetType() == TypeOf(INT))
	xAssert(iif(l , -1L , -2L):GetType() == TypeOf(INT))
	
	? iif(l , INT(1) , INT(2)):GetType()
	xAssert(iif(l , INT(1) , INT(2)):GetType() == TypeOf(INT))
	xAssert(iif(l , INT(-1) , INT(2)):GetType() == TypeOf(INT))
	xAssert(iif(l , INT(1) , INT(-2)):GetType() == TypeOf(INT))

	? iif(l , 1U , 2U):GetType()
	xAssert(iif(l , 1U , 2U):GetType() == TypeOf(DWORD))
	xAssert(iif(l , 1U , 2000000U):GetType() == TypeOf(DWORD))

	? iif(l , (WORD)1 , (WORD)2):GetType()
	xAssert(iif(l , (WORD)1 , (WORD)2):GetType() == TypeOf(WORD))
	xAssert(iif(l , (WORD)256 , (WORD)2):GetType() == TypeOf(WORD))

	? iif(l , (BYTE)1 , (BYTE)2):GetType()
	xAssert(iif(l , (BYTE)1 , (BYTE)2):GetType() == TypeOf(BYTE))

	? iif(l , (INT64)1 , (INT64)2):GetType()
	xAssert(iif(l , (INT64)1 , (INT64)2):GetType() == TypeOf(INT64))

	? iif(l , (UInt64)1 , (UInt64)2):GetType()
	xAssert(iif(l , (UInt64)1 , (UInt64)2):GetType() == TypeOf(UInt64))
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

