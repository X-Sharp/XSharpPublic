// 275. error XS0121: The call is ambiguous between the following methods or properties: 
// 'Functions.Test(int)' and 'Functions.Test(dword)'
FUNCTION Test(n AS INT) AS VOID
? "int"
FUNCTION Test(d AS DWORD) AS VOID
? "dword"

FUNCTION Start() AS VOID
LOCAL b := 0 AS BYTE
Test(b)

