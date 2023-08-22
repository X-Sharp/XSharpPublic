// warning XS9021: Signed/unsigned conversions from 'dword' to 'int' may lead to loss of data or overflow errors
// warnings as errors to display as error
FUNCTION Start() AS VOID
LOCAL d := 0 AS DWORD
LOCAL n := 0 AS INT
d := (DWORD)n
n := (INT)d
? n,d 
