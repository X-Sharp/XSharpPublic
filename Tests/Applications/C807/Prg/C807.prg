// 807. Case sensitivity of #defines with /cs+ and /vo8+
// https://github.com/X-Sharp/XSharpPublic/issues/816
// /cs+ /vo8+
#pragma warnings(9066, off) // ambiguous
FUNCTION Start() AS VOID STRICT
LOCAL n := 0 AS INT
#ifdef debug // does not print this
? "lower case"
n += 1
#endif
#ifdef DeBuG // does not print this
? "mixed case"
n += 10
#endif
#ifdef DEBUG // this one is printed
? "upper case"
n += 100
#endif
? DEBUG
xAssert(n == 100)
RETURN

FUNCTION Debug() AS VOID
RETURN

CLASS Test1
METHOD Debug() AS VOID
SELF:Debug()
Debug()
END CLASS

CLASS Test2
METHOD DeBuG() AS VOID
SELF:DeBuG()
DeBuG()
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

