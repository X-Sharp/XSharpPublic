// 840. Failed to emit module error, with iif() - VFP dialect
#pragma options("vo10", on)
FUNCTION Start() AS VOID
LOCAL u     
local cond := TRUE
u := IIF(.t., 123, .NULL.)
u := IIF(.t., {}, .NULL.)
u := IIF(.t., 456, NULL)
u := IIF(cond, 123, .NULL.)
u := IIF(cond, {}, .NULL.)
u := IIF(cond, 456, NULL)
? u
