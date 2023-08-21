// 300. error XS0030: Cannot convert type 'string' to 'Vulcan.__Psz'
#pragma warnings(219, off) // variable is never used
FUNCTION Start() AS VOID
LOCAL p AS PSZ
LOCAL c := "123" AS STRING
p := PSZ(_CAST , "abc")
? p
