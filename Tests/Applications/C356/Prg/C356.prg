// 356. error XS1503: Argument 1: cannot convert from 'byte' to 'string'
FUNCTION Start() AS VOID

/*
must print the following. Note that in previous builds this compiled without errors, 
but didn't print correct values either
C:
\Test\
Testing
.dat
*/
__SplitPath()

?

// msut print "bcde"
LOCAL dwPosition AS DWORD
dwPosition := 2
__GetNextAddress("abcde" , dwPosition)

FUNCTION __SplitPath() AS VOID
	LOCAL DIM abDrive[100] AS BYTE
	LOCAL DIM abDir	 [100] AS BYTE
	LOCAL DIM abName [100] AS BYTE
	LOCAL DIM abExt  [100] AS BYTE
	
	LOCAL cString AS USUAL
	cString := "C:\Test\Testing.dat"

	SplitPath(cString, PSZ(_CAST, @abDrive[1]), PSZ(_CAST,@abDir[1]), PSZ(_CAST,@abName[1]), PSZ(_CAST,@abExt[1]))
	
	? Psz2String(PSZ(_CAST, @abDrive[1]))
	? Psz2String(PSZ(_CAST, @abDir[1]))
	? Psz2String(PSZ(_CAST, @abName[1]))
	? Psz2String(PSZ(_CAST, @abExt[1]))
	
	xAssertEquals( Psz2String(PSZ(_CAST, @abDrive[1])) , "C:")
	xAssertEquals( Psz2String(PSZ(_CAST, @abDir[1]))   , "\Test\")
	xAssertEquals( Psz2String(PSZ(_CAST, @abName[1]))  , "Testing")
	xAssertEquals( Psz2String(PSZ(_CAST, @abExt[1]))   , ".dat")
RETURN



CLASS WindowStyle
    STATIC METHOD ClassName(hWnd AS PTR) AS STRING
	    LOCAL pszName 	AS PSZ
	    pszName := PSZ(_CAST,MemAlloc(128))
	    ? pszName
	RETURN NULL
END CLASS

VOSTRUCT _WINWIN32_FIND_DATA
	MEMBER DIM cFileName[ 255 ] AS BYTE

VOSTRUCT _winCOPYDATASTRUCT
	MEMBER  dwData AS DWORD
	MEMBER  cbData AS DWORD
	MEMBER  lpData AS DWORD

FUNCTION Test() AS VOID
LOCAL pWin32_Find_Data	IS _WINWIN32_FIND_DATA
? Psz2String(PSZ(_CAST,@pWin32_Find_Data:cFilename))

LOCAL st AS _WinCOPYDATASTRUCT
LOCAL lParam AS LONG
st := (_WinCOPYDATASTRUCT PTR) (PTR(_CAST,lParam))
? st

FUNCTION __GetNextAddress(cBuffer AS STRING, dwPosition REF DWORD) AS STRING STRICT
    LOCAL dwPos          AS DWORD
    LOCAL pBuffer        AS BYTE PTR
   
	dwPos   := dwPosition
	pBuffer := String2Psz(cBuffer)

	? AsString(PSZ(_CAST, pBuffer + dwPos - 1))
	
	xAssertEquals(AsString(PSZ(_CAST, pBuffer + dwPos - 1)) , "bcde")
RETURN NULL



PROC xAssertEquals(o1 AS STRING, o2 AS STRING)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF

