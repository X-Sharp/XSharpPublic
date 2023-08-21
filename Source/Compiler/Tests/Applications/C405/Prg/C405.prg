// 405. no error on Psz2String(_string)
// Vulcan produces an error
// We automatically convert a string to a PSZ in the compiler.
// the PSZ is never freed, so this is a memory leak.
#pragma warnings (9068, off) // psz
FUNCTION Start() AS VOID
LOCAL _string := "asd" AS STRING
// there should be an error here about using a STRING as argument:
_string := Psz2String(_string)
? _string

