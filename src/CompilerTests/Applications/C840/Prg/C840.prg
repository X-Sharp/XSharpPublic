// 840. Failed to emit module error, with iif() - VFP dialect      
// https://github.com/X-Sharp/XSharpPublic/issues/989

#pragma options("vo10", on)
FUNCTION Start() AS VOID
LOCAL u     
LOCAL cond := TRUE
u := IIF(.t., 123, .NULL.)
xAssert(u == 123)
u := IIF(.t., {1,2,3}, .NULL.)
xAssert(Len(u) == 3)
u := IIF(.t., 456, NULL)
xAssert(u == 456)
u := IIF(cond, 123, .NULL.)
u := IIF(cond, {}, .NULL.)
u := IIF(cond, 456, NULL)
xAssert(u == 456)

cond := FALSE
u := IIF(cond, 123, .NULL.)
? u
//xAssert(u == .NULL.) // doesn't work
u := IIF(cond, 456, NULL)
? u
xAssert(u == NULL)
RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
