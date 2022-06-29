// 287. error XS0457: Ambiguous user defined conversions '__Psz.implicit operator __Psz(void*)' and '__Psz.implicit operator __Psz(byte*)' when converting from 'IntPtr*' to '__Psz'
// vulcan resolves call to the "string" overload
// note that the calls without "@" compile fine in x#, too
// vo7
#pragma warnings(165, off) // unassigned local
#pragma warnings(9071, off) // parameter needs a ref
FUNCTION mySplitPath(cPath AS STRING, cDrive REF STRING) AS VOID
? "string"
FUNCTION mySplitPath(pszPathName AS PSZ, pszDrive AS PSZ) AS VOID
? "PSZ"

FUNCTION Start() AS VOID
LOCAL u := "a" AS USUAL
LOCAL c AS STRING
mySplitPath(u, @c) // error XS0457:
mySplitPath(u, c) // OK
mySplitPath(u, REF c) // OK


