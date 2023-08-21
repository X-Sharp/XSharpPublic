// 630. Problem calling function with PSZ(_CAST) param
//error XS1503: Argument 1: cannot convert from 'byte*' to 'string'
FUNCTION Start() AS VOID
	LOCAL pBuffer AS BYTE PTR
	LOCAL pz AS PSZ
	pBuffer := MemAlloc(10)
	pBuffer[1] := 65
	pBuffer[2] := 66
	pBuffer[3] := 67
	pBuffer[4] := 0

	pz := PSZ(_CAST, pBuffer + 1) // ok
	? pz
	xAssertEquals(Psz2String(pz) , "BC")
	Test( PSZ(_CAST, pBuffer + 1) ) // ok

	IsSpace( PSZ(_CAST, pBuffer + 1) ) // error XS1503: Argument 1: cannot convert from 'byte*' to 'string'
RETURN

FUNCTION Test(pz AS PSZ) AS VOID
? pz
xAssertEquals(Psz2String(pz) , "BC")


// original code:    
FUNCTION __GetNextAddress(cBuffer AS STRING, dwPosition REF DWORD) AS STRING STRICT
   //SE-070611 
    LOCAL dwPos          AS DWORD
    LOCAL dwEnd          AS DWORD
    LOCAL pBuffer        AS BYTE PTR
   
	dwPos   := dwPosition
	dwEnd   := SLen(cBuffer) 
	pBuffer := String2Psz(cBuffer)

	DO WHILE dwPos <= dwEnd .AND. IsSpace(PSZ(_CAST, pBuffer + dwPos - 1))
		++dwPos
	ENDDO

	// ...

RETURN NULL



PROC xAssertEquals(o1 AS STRING, o2 AS STRING)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF

