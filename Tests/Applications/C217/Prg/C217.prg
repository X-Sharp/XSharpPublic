// 217. compiler crash
UNION TestUnion
MEMBER n AS DWORD
MEMBER i AS INT

FUNCTION Start() AS VOID
LOCAL u IS TestUnion
u:n := 123
? u:i
