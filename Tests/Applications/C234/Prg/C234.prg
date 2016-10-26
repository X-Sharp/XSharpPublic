// 234. error XS0037: Cannot convert null to '__Psz' because it is a non-nullable value type
FUNCTION TestPsz(p AS PSZ) AS VOID
? p
FUNCTION Start() AS VOID
TestPsz(NULL)

