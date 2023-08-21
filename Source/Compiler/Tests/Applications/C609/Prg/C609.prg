// error XS7038: Failed to emit module 
// Error shown when /unsafe is not specified. If /unsafe is enabled, code compiles and runs correctly (?)
FUNCTION Start() AS VOID
	LOCAL DIM adim[4] AS BYTE
	LOCAL p AS PTR
	
	adim[1] := 1
	adim[2] := 1
	adim[3] := 0
	adim[4] := 0
	
	p := @adim
	? DWORD(p)
	xAssertEquals(DWORD(p) , 257)
	
	p := @adim[1]
	? BYTE(p)
	xAssertEquals(BYTE(p) , 1)
	


	LOCAL anet AS BYTE[]
	anet := <BYTE>{2,2,0,0}

	p := @anet
	? DWORD(p) // this is not the data apparently
//	xAssertEquals(DWORD(p) , 514)
	
	p := @anet[1]
	? DWORD(p)
	xAssertEquals(DWORD(p) , 514)
	? BYTE(p)
	xAssertEquals(BYTE(p) , 2)


	SplitTest("c:\test\tst.txt")	
RETURN

// original code:
FUNCTION SplitTest(cFile AS STRING) AS VOID
	
	LOCAL DIM abDrive[3] AS BYTE
	LOCAL DIM abDir[256] AS BYTE
	LOCAL DIM abName[256] AS BYTE
	LOCAL DIM abExt[256] AS BYTE
	
	SplitPath( String2Psz( cFile ), PSZ(_CAST, @abDrive[1]), PSZ(_CAST,@abDir[1]), PSZ(_CAST,@abName[1]), PSZ(_CAST,@abExt[1] ) )
	? PSZ(_CAST, @abDrive[1])
	? PSZ(_CAST, @abDir[1])
	? PSZ(_CAST, @abName[1])
	? PSZ(_CAST, @abExt[1])
RETURN

PROC xAssertEquals(o1 AS DWORD, o2 AS DWORD)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF


