// 290. File1.prg(7,17): error XS0121: The call is ambiguous between the following methods or properties: '__Psz.__Psz(string)' and '__Psz.__Psz(IntPtr)' 
// the error line reported  is line no 7, the LOCAL pszDir, while it should be obviously line 4
FUNCTION Test() AS VOID
LOCAL u AS USUAL
LOCAL p AS PSZ
p := PSZ(u) // should be here

FUNCTION Start() AS VOID
LOCAL pszDir AS PSZ // error reported here 

