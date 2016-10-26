// 246. error XS1503: Argument 1: cannot convert from 'void*' to 'ushort*'
FUNC WideCharToMultiByte(lpWideCharStr AS WORD PTR) AS VOID
FUNCTION Start() AS VOID
LOCAL p AS PTR
WideCharToMultiByte(p)

