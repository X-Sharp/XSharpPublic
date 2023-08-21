// This uses the Vulcan Dialect
// So a PSZ is assumed to start at 0
// so a copy at p+3 starts with the 4th character
FUNCTION Start as VOID
LOCAL p AS PSZ
LOCAL i AS DWORD

p := MemAlloc(100)
MemCopyString(p, "abc", 3) // ok
i := 3
MemCopyString(p + i, "de", 2) // it gives an incorrect result
xassert(Psz2String(p) == "abcde")
MemCopyString(p + 3, "de", 2) // it gives an incorrect result also   
xassert(Psz2String(p) == "abcde")   
xassert(p[0] == asc("a"))
? __DIALECT__
? Psz2String(p)
MemFree(p)

RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
