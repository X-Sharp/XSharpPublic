// 545. error XS7036: There is no argument given that corresponds to the required formal parameter '__arglist' of 'Functions.local_wsprintf(IntPtr, IntPtr, __arglist)'

_DLL FUNCTION local_wsprintf(pszBuffer AS PSZ, pszFormat AS PSZ, ...) AS INT STRICT:USER32.wsprintfA

FUNCTION Start() AS VOID
	LOCAL DIM buffer[256] AS BYTE
	
//	calling the wsprintf() function defined in Win32APILibrary here throws a BadFormatException, even in vulcan. Why
//	calling it below works fine, this is strange. Let's see how it works in x#, when the compiler allows this to compile
//	? wsprintf(@buffer,String2Psz("ABC") ) // runtime exception when compiled with vulcan
//	? Psz2String(@buffer)
	
	? local_wsprintf(@buffer,String2Psz("ABC") )
	? Psz2String(@buffer)
	xAssert(Psz2String(@buffer) == "ABC")
	
	? local_wsprintf(@buffer,String2Psz("ABC %d") , 123 )
	? Psz2String(@buffer)
	xAssert(Psz2String(@buffer) == "ABC 123")
	
	? local_wsprintf(@buffer,String2Psz("ABC %d %d") , 123 , 456 )
	? Psz2String(@buffer)
	xAssert(Psz2String(@buffer) == "ABC 123 456")
	
//	calling the wsprintf() function defined in Win32APILibrary here works ok, also from vulcan
	? wsprintf(@buffer,String2Psz("ABC") )
	? Psz2String(@buffer)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

