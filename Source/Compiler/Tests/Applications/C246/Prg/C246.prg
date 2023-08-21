// 246. error XS1503: Argument 1: cannot convert from 'void*' to 'ushort*'
// Should compile with /unsafe and /vo7.
#pragma warnings(165, off) // unsasigned local
FUNC WideCharToMultiByte(lpWideCharStr AS WORD PTR) AS VOID
FUNCTION Start() AS VOID
LOCAL p AS PTR
WideCharToMultiByte(p)

