// 157. error XS1503: Argument 1: cannot convert from 'System.IntPtr' to 'void*'
#pragma warnings(165, off) // unassigned local
FUNCTION Start() AS VOID
LOCAL p AS IntPtr
FClose(p)

