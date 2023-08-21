// 234. error XS0037: Cannot convert null to '__Psz' because it is a non-nullable value type
#pragma warnings(165, off) // unsasigned local
FUNCTION TestPsz(p AS PSZ) AS VOID
? p
FUNCTION Start() AS VOID
TestPsz(NULL)


LOCAL ptrBuffer AS PTR
LOCAL nLen AS DWORD
? INT( WNetGetUser( NULL, ptrBuffer, @nLen ) )


FUNC WNetGetUser( lpName AS PSZ, lpUserName AS PSZ, lpnLength AS DWORD PTR) AS DWORD PASCAL
	? lpName, lpUserName, lpnLength
RETURN 0
