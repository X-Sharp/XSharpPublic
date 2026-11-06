// 989. [Net10] InvalidProgramException with _DLL FUNCTION #2097
// https://github.com/X-Sharp/XSharpPublic/issues/2097

_DLL FUNC GetModuleHandle(lpModuleName AS PSZ) AS PTR PASCAL:KERNEL32.GetModuleHandleA
_DLL FUNC GlobalAddAtom(lpString AS PSZ) AS WORD PASCAL:KERNEL32.GlobalAddAtomA
using System.Runtime.InteropServices
using System.Security
using XSharp
using XSharp.Internal
GLOBAL gatomVOObjPtr AS DWORD

FUNCTION Start( ) AS VOID
	GetModuleHandle(String2Psz("COMCTL32.DLL"))
	gatomVOObjPtr 	:= GlobalAddAtom(String2Psz("__VOObjPtr"))	
	gatomVOObjPtr 	:= MyFunctions.GlobalAddAtom(String2Psz("__VOObjPtr"))	


static class MyFunctions
    [DllImport("KERNEL32", CharSet := CharSet.Auto, EntryPoint := "GlobalAddAtomA", ExactSpelling := true, SetLastError := true)];
    [SuppressUnmanagedCodeSecurity] ;
    EXTERN STATIC METHOD GlobalAddAtom([ActualType(typeof(__Psz))] pString as string)
end class

