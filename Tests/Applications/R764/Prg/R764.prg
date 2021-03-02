// R 764 - new features in X# 2.8:
// USING statement
// NINT and NUINT types (native integer with 32 or 64 bits depending on platform)
USING System.IO
FUNCTION Start( ) AS VOID
    USING VAR reader := StringReader{"abcde"}
    USING IMPLIED reader2 := StringReader{"abcde"}    	
    LOCAL nInt1     AS NINT
    LOCAL nUInt1    AS NUINT
    ? sizeof(NINT)    
    ? sizeof(NUINT)
    nInt1  := -42
    nUInt1 := 42
    ? (LONG) nInt1
    ? (DWORD) nUInt1
RETURN
