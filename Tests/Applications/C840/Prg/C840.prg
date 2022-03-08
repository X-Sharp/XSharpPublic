// 840. Failed to emit module error, with iif() - VFP dialect
FUNCTION Start() AS VOID
LOCAL u
u := IIF(.t., 123, .NULL.)
u := IIF(.t., {}, .NULL.)
u := IIF(.t., 456, NULL)

