// https://github.com/X-Sharp/XSharpPublic/issues/603
/*

    It should report 4 warnings for automatic PSZ conversion, but instead reports only 3
    The first warning, points to line 10, which is a LOCAL declaration.
    The other 2 warnings point correctly to the correct lines where PSZ() is used.

    This example also showed a problem in the runtime.
    The late bound call uObj:Something was not properly storing and retrieving the PSZ from within the usual.
    Inside the late bound code the return value from IVarGet(uObj, "something") was an object with a PSZ value.
    This was not correctly converted to a USUAL with the psz type
*/
#pragma warnings(9068, off) // psz
#pragma warnings(219, off) // assigned but not used
FUNCTION Start() AS VOID
TestClass{}:foo()
RETURN

CLASS TestClass
EXPORT something AS PSZ
CONSTRUCTOR()
SELF:something := StringAlloc("something")
DESTRUCTOR
    MemFree(something)
    RETURN

METHOD foo() AS VOID
LOCAL uObj := TestClass{} AS USUAL  // line 10, first warning incorrectly points here
LOCAL uPsz := String2Psz("abc") AS USUAL
LOCAL uTemp AS USUAL

uTemp := uObj:something

? PSZ( uObj:something)
? PSZ( uObj:something)

xAssert(PSZ( uObj:something) == "something")

? PSZ(uPsz) // line 16, correct
? PSZ(uPsz) // line 17, correct

xAssert(PSZ(uPsz) == "abc")


RETURN

END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
