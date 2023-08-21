// 217. compiler crash
#pragma warnings(170, off) // unsasigned field
UNION TestUnion
MEMBER n AS DWORD
MEMBER i AS INT

FUNCTION Start() AS VOID
LOCAL u IS TestUnion
u:n := 123
? u:i
