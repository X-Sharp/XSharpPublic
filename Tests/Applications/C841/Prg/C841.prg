// 841. Failed to emit module error, with iif() - VO dialect
FUNCTION Start() AS VOID
LOCAL u
u := IIF(.t., {}, NULL)
u := IIF(.t., NULL, {})
u := IIF(.t., 456, NULL)
u := IIF(.t., NULL , 456)

