// 246. error XS1503: Argument 1: cannot convert from 'void*' to 'ushort*'        
// Should compile with /unsafe and /vo7.
FUNC WideCharToMultiByte(lpWideCharStr AS WORD PTR) AS VOID
FUNCTION Start() AS VOID
LOCAL p AS PTR
WideCharToMultiByte(p)

