// 765. Problem with accessing elements in multi dim arrays in VFP dialect
/*
Please keep /fox2 enabled as a test, this used to cause an ICE with this code with previous compiler versions
*/

/*
XSharp.Error
Bound error

Callstack :
XSharp.__Array.__Usual XSharp.__Array.__GetElement(System.Int32 index, System.Int32 index2)()   :  C:\xSharp\DevRt\Runtime\XSharp.RT\Types\Array.prg  :  240
XSharp.__Array.__Usual XSharp.__Array.get_Item(System.Int32 index, System.Int32 index2)()   :  C:\xSharp\DevRt\Runtime\XSharp.RT\Types\Array.prg  :  170
static System.Void C765.Exe.Functions.Start()()   :  C:\xSharp\Dev\Tests\Applications\C765\Prg\C765.prg  :  15
*/

#pragma warnings(9025, off) //   return statement
FUNCTION Start() AS VOID
	DIMENSION aDim1[10]
	aDim1[1] := 1
	aDim1[10] := "10"
	? aDim1[1]
	? aDim1[10]
	xAssert(aDim1[1] == 1)
	xAssert(aDim1[10] == "10")


	DIMENSION aDim2[3,4]
	aDim2[1,1] := 11 // runtime exception
	aDim2[3,4] := 34
	xAssert(aDim2[1,1] == 11)
	xAssert(aDim2[3,4] == 34)
    IF XSharp.RuntimeState.CompilerOptionFox2
	    TestFox2_on()
	ELSE
	?
	TestFox2_off()
	ENDIF
RETURN

//#pragma options ("fox2" , on)

PROCEDURE TestFox2_on()
? "Fox2+, LOCAL:"
LOCAL lon
DIMENSION lon[100]
lon = 123
? lon
? IsArray(lon) // TRUE
xAssert(IsArray(lon))
xAssert(lon[10] == 123)

? "Fox2+, PUBLIC:"
PUBLIC pon
DIMENSION pon[100]
pon = 123
? pon
? IsArray(pon) // TRUE
xAssert(IsArray(pon))
xAssert(pon[20] == 123)



PROCEDURE TestFox2_off()
? "Fox2-, LOCAL:"
LOCAL loff // works ok
DIMENSION loff[100]
loff = 123
? loff
? IsArray(loff) // TRUE, with PUBLIC it is FALSE <---
xAssert(.not. IsArray(loff))
xAssert(loff == 123)

? "Fox2-, PUBLIC:"
PUBLIC poff
DIMENSION poff[100]
poff = 123
? poff
? IsArray(poff) // FALSE, with LOCAL it is TRUE <---
xAssert(.not. IsArray(poff))
xAssert(poff == 123)

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

