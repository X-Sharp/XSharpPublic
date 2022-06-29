// 352. error XS0172: Type of conditional expression cannot be determined because 'IntPtr' and 'void*' implicitly convert to one another
#pragma warnings(165, off) // uniassigned local
FUNCTION Start() AS VOID
LOCAL h AS PTR
LOCAL l := FALSE AS LOGIC
? iif(l , NULL_PTR, h)

