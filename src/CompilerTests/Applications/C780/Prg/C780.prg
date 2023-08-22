// 780. AccessViolationException with FRead()

/*
System.AccessViolationException
Attempted to read or write protected memory. This is often an indication that other memory is corrupt.

Callstack :
String XSharp.__Usual.ToString()()
static String XSharp.__Usual.op_Implicit(XSharp.__Usual u)()
static String XSharp.RT.Functions.AsString(XSharp.__Usual uValue)()
static System.Void XSharp.RT.Functions.QOut(XSharp.__Usual uValueList)()
*/
#pragma warnings(165, off) //   unassigned local
// /vo7+
// https://github.com/X-Sharp/XSharpPublic/issues/686
FUNCTION Start( ) AS VOID
	LOCAL cFileName AS STRING
	LOCAL hHandle AS PTR
	LOCAL cText AS STRING
//	cFilename := "c:\test\abc.txt"
	cFilename := "abc.txt"

	System.IO.File.WriteAllText(cFileName , "ABCDE1234567890")
	hHandle := FOpen(cFileName)
	FRead(hHandle , REF cText , 5) // ABCDE, OK
	? cText
	xAssert(cText == "ABCDE")
	FRead(hHandle , @cText , 5) // System.AccessViolationException
	? cText
	xAssert(cText == "12345")
	FClose(hHandle)


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

