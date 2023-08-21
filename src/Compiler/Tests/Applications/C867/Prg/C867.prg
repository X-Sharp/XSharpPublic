// 867. Problem with late bound and overloads
// https://github.com/X-Sharp/XSharpPublic/issues/875
/*
Description :   Method is overloaded, Cannot determine the right overload to call.
Found 2 overloads
1. MyClassError:Go(Dummy AS Int32)
2. MyClassError:Go(text AS String)

Subsystem :     BASE
GenCode :       EG_AMBIGUOUSMETHOD Method is overloaded, Cannot determine the right overload to call.
FuncSym :       GO
Severity :      ES_ERROR
Can Default :   False
Can Retry :     False
Can Substitute :        False
Argument Number :       1
Argument :      MethodName
Arguments :     {MyClassError:Go}
Stack Trace :
 OOPHELPERS:FINDBESTOVERLOAD (Line: 0)
 OOPHELPERS:SENDHELPER (Line: 0)
 OOPHELPERS:DOSEND (Line: 0)
 __INTERNALSEND (Line: 0)
 START (Line: 11)
*/

USING System.Windows.Forms
#pragma options("lb", on)
FUNCTION Start() AS VOID STRICT
	LOCAL obj AS OBJECT

	obj := MyClassOk{}
	obj:Go(0)
	xAssert(obj:Go(123) == 123)
	xAssert(obj:Go() == 0)
	xAssert(obj:Go("test") == "test")

	obj := MyClassError{}
	obj:Go(0)
	xAssert(obj:Go(123) == 123)
	xAssert(obj:Go() == 0)
	xAssert(obj:Go("test") == "test")

	obj := CustomForm{}
	obj:Show()

RETURN

CLASS MyClassOK
	METHOD Go() AS INT STRICT
	RETURN 0
	METHOD Go(n AS INT) AS INT STRICT
	RETURN n
	METHOD Go(text AS STRING) AS STRING STRICT
	RETURN text
END CLASS

CLASS MyClassError
	METHOD Go(n AS INT) AS INT STRICT
	RETURN n
	METHOD Go(text AS STRING) AS STRING STRICT
	RETURN text
	METHOD Go() AS INT STRICT
	RETURN 0
END CLASS

CLASS CustomForm INHERIT Form
	NEW METHOD Show() AS VOID STRICT
	    xAssert(TRUE)
END CLASS


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
