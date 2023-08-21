// https://github.com/X-Sharp/XSharpPublic/issues/1013
// 848. Problem with casting string to PTR syntax
#pragma warnings(9068, off) // psz

FUNCTION Start() AS VOID
	LOCAL cTest AS STRING
	LOCAL pPtr AS PTR
	LOCAL pPsz AS PSZ

	cTest := "test"

	pPsz := PSZ(cTest) // OK
	? pPsz
	? Psz2String(pPsz)
	xAssert( Psz2String(pPsz) == "test" )

	pPsz := PSZ(_CAST, cTest) // OK
	? pPsz
	? Psz2String(pPsz)
	xAssert( Psz2String(pPsz) == "test" )


	pPtr := PTR(cTest) // // System.InvalidCastException
	? pPtr
	? Psz2String(pPtr)
	xAssert( Psz2String(pPtr) == "test" )

	pPtr := PTR(_CAST, cTest) // // System.InvalidCastException
	? pPtr
	? Psz2String(pPtr)
	xAssert( Psz2String(pPtr) == "test" )



	// Mixing PTR/PSZ here, this is stupid, but I think it shoudl also work?
	pPtr := PSZ(_CAST, cTest) // System.InvalidProgramException: CLR detected an invalid program
	xAssert( Psz2String(pPtr) == "test" )

	pPsz := PTR(_CAST, cTest) // System.InvalidCastException
	xAssert( Psz2String(pPsz) == "test" )

	xAssert( Psz2String(PTR(_CAST, cTest)) == "test" )


RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

