// 840b. Failed to emit module error, with iif() - VFP dialect      
// https://github.com/X-Sharp/XSharpPublic/issues/989
#pragma options("vo10", off)
FUNCTION Start() AS VOID
LOCAL u     
LOCAL cond := TRUE
u := IIF(.t., 123, .NULL.)
u := IIF(.t., {}, .NULL.)
u := IIF(.t., 456, NULL)
u := IIF(cond, 123, .NULL.)
u := IIF(cond, {}, .NULL.)
u := IIF(cond, 456, NULL)
? u
RETURN 
