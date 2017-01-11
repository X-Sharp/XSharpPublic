// 405. no error on Psz2String(_string)
FUNCTION Start() AS VOID
LOCAL _string := "asd" AS STRING
// there should be an error here about using a STRING as argument:
_string := Psz2String(_string)
? _string

