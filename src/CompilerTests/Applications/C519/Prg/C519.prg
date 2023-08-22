// 519. error XS0266: Cannot implicitly convert type 'byte' to 'void*'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
LOCAL l := TRUE AS LOGIC

LOCAL p AS PTR
p := 1 //ok
p := 2 //ok
p := iif(l , 3 , 4) // error XS0266
? p
xAssert(p == 3)    



// code from bBrowser:
LOCAL pp AS PTR PTR
pp := MemAlloc(100)
? pp[2]
pp[2] := 123 // ok
pp[2] := iif(l , 100 ,200) // error XS0266
? pp[2]
xAssert(pp[2] == 100)

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

