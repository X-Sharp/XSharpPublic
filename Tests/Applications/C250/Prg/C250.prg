// 250. error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('__Usual')
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL DIM bErrMsg [10] AS BYTE
? @u
? @bErrMsg

