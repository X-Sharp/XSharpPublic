// 840. Failed to emit module error, with iif() - VFP dialect      
// https://github.com/X-Sharp/XSharpPublic/issues/989
FUNCTION Start() AS VOID
	Test1()
	Test2()

#pragma options("vo10", on)
PROCEDURE Test1()
LOCAL u     
LOCAL cond := TRUE
u := IIF(.t., 123, .NULL.)
u := IIF(.t., {}, .NULL.)
u := IIF(.t., 456, NULL)
u := IIF(cond, 123, .NULL.)
u := IIF(cond, {}, .NULL.)
u := IIF(cond, 456, NULL)
? u

#pragma options("vo10", off)
PROCEDURE Test2()
LOCAL u     
LOCAL cond := TRUE
u := IIF(.t., 123, .NULL.)
u := IIF(.t., {}, .NULL.)
u := IIF(.t., 456, NULL)
u := IIF(cond, 123, .NULL.)
u := IIF(cond, {}, .NULL.)
u := IIF(cond, 456, NULL)
? u

