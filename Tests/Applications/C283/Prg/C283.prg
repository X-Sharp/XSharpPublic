// 283. error XS0121: The call is ambiguous between the following methods or properties: '__Usual.__Usual(__Usual)' and '__Usual.__Usual(object)'
// /vo10+
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := iif(TRUE,"1","2")
? c

