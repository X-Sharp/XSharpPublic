// 236. error XS0034: Operator '==' is ambiguous on operands of type '__Psz' and 'IntPtr'
#pragma warnings(165, off) // unsasigned local
FUNCTION Start() AS VOID
LOCAL p AS PSZ
? p == NULL_PTR
? p = NULL_PTR
? p != NULL_PTR
? p > NULL_PTR
? p >= NULL_PTR
? " Compare NULL_PSZ with 0"
? p < 0
? p <= 0
? p > 0
? p >= 0
? p = 0
p := String2Psz("aaa")
? " Compare psz with value aaa  with 0"
? p < 0
? p <= 0
? p > 0
? p >= 0
? p = 0
? "all results are equal to but just as useless as Vulcan's results and VO's result"


