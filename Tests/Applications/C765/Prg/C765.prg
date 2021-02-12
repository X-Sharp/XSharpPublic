// 766. Problem with accessing elements in multi dim arrays in VFP dialect
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
	xAssert(aDim1[1,1] == 11)
	xAssert(aDim1[3,4] == 34)
RETURN



PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN   

