// 989. [Net10] InvalidProgramException with _DLL FUNCTION #2097
// https://github.com/X-Sharp/XSharpPublic/issues/2097

_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA
_DLL FUNC GlobalAddAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.GlobalAddAtomA

GLOBAL gatomVOObjPtr AS DWORD

FUNCTION Start( ) AS VOID
	GetModuleHandle(String2Psz("COMCTL32.DLL"))
	gatomVOObjPtr 	:= GlobalAddAtom(String2Psz("__VOObjPtr"))	
	? gatomVOObjPtr
	xAssert(gatomVOObjPtr != 0)


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN nil
