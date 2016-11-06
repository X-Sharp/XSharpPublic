// 250. error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('__Usual')
FUNCTION Start() AS VOID
LOCAL u := "a" AS USUAL
LOCAL DIM bErrMsg [10] AS BYTE
LOCAL o := 1 AS OBJECT
LOCAL p AS PTR
? @u
? @bErrMsg
? @o
p := @o
? p
p := @u
? p

