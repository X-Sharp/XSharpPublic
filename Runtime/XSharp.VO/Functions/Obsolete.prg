//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// All Dynamic memory functions that are not supported or dummies
// And also all the OldSpace functions

[Obsolete( "'DynMemDump()' is not supported and always returns FALSE" )] ;
function	 DynMemDump(cFile as string,nOptions as dword) as logic
return false   

[Obsolete( "'DynCheckError()' is not supported and always returns 0" )] ;
function	 DynCheckError() as dword
return 0

[Obsolete( "'DynInfoFree()' is not supported and always returns 0" )] ;
function	 DynInfoFree() as dword
return 0

[Obsolete( "'DynInfoMax()' is not supported and always returns 0" )] ;
function	 DynInfoMax() as dword
return 0

[Obsolete( "'DynInfoSize()' is not supported and always returns 0" )] ;
function	 DynInfoSize() as dword
return 0

[Obsolete( "'DynInfoUsed()' is not supported and always returns 0" )] ;
function	 DynInfoUsed() as dword
return 0

[Obsolete( "'DynLock()' is not supported and has no effect" )] ;
function	 DynLock() as void
return

[Obsolete( "'DynProtect()' is not supported and always returns FALSE" )] ;
function	 DynProtect( lSet as logic ) as logic
return false

[Obsolete( "'DynShrink()' is not supported and always returns 0" )] ;
function	 DynShrink() as dword
return 0

[Obsolete( "'DynSize()' is not supported and always returns 0" )] ;
function	 DynSize() as dword
return 0

[Obsolete( "DynToOldSpace() is not supported and returns the original usual")] ; 
function	 DynToOldSpace( u as __Usual ) as __Usual
return u

[Obsolete( "DynToOldSpaceArray() is not supported and returns the original array")] ; 
function	 DynToOldSpaceArray( a as __Array ) as __Array
return a

[Obsolete( "DynToOldSpaceFloat() is not supported and returns the original float", true )] ; 
function	 DynToOldSpaceFloat( f as __VoFloat ) as __VoFloat
return f

[Obsolete( "DynToOldSpaceObject() is not supported and returns the original object" )] ; 
function	 DynToOldSpaceObject( o as object ) as object
return o

[Obsolete( "DynToOldSpaceString() is not supported and returns the original string" )] ; 
function	 DynToOldSpaceString( s as string ) as string
return s

[Obsolete( "'DynUnlock()' is not supported and has no effect" )] ;
function	 DynUnLock() as void
return


[Obsolete( "'IsOldSpaceFloat()' is not supported and always returns FALSE" )] ;
function	 IsOldSpaceFloat(f as __VOFloat) as logic
return false   


[Obsolete( "'IsOldSpaceString()' is not supported and always returns FALSE" )] ;
function	 IsOldSpaceString(c as string) as logic
return false 


[Obsolete( "'OldSpaceFreeFloat()' is not supported and has no effect")] ;
function	 OldSpaceFreeFloat(f as __VOFloat) as void
return

[Obsolete( "'RegisterKid()' is not supported and has no effect" )] ;
function	 RegisterKid(ptrKid as IntPtr,dwCount as dword,lItem as logic) as void
return

[Obsolete( "'FreeStaticObject()' is not supported and always returns FALSE" )] ;
function	 FreeStaticObject(o as object) as logic
return false   

[Obsolete( "'OldSpaceFree()' is not supported and has no effect" )] ;
function	 OldSpaceFree(u as __Usual) as void
return

[Obsolete( "'OldSpaceFreeObject()' is not supported and has no effect" )] ;
function	 OldSpaceFreeObject(o as object) as void
return


[Obsolete( "'IsOldSpaceArray()' is not supported and always returns FALSE" )] ;
function	 IsOldSpaceArray(a as __Array) as logic
return false   

[Obsolete( "'OldSpaceFreeArray()' is not supported and has no effect" )] ;
function	 OldSpaceFreeArray(a as __Array) as void
return 

[Obsolete( "'IsOldSpace()' is not supported and always returns FALSE" )] ;
function	 IsOldSpace(u as __Usual) as logic
return false   

[Obsolete( "'IsOldSpaceObject()' is not supported and always returns FALSE" )] ;
function	 IsOldSpaceObject(o as object) as logic
return false   

[Obsolete( "'MemCheckPtr()' is not supported and always returns TRUE" )] ;
unsafe	 function MemCheckPtr( pMemory as ptr, dwSize as dword ) as logic
return true


[Obsolete( "'SetKidStackSize()' is not supported and always returns 0" )] ;
function	 SetKidStackSize(dwBytes as dword) as dword
return 0   

[Obsolete( "'SetMaxDynSize()' is not supported and always returns 0" )] ;
function	 SetMaxDynSize(dwBytes as dword) as dword
return 0   

[Obsolete( "'SetMaxRegisteredAxitMethods()' is not supported and always returns 0" )] ;
function	 SetMaxRegisteredAxitMethods(dwCount as dword) as dword
return 0   

[Obsolete( "'SetMaxRegisteredKids()' is not supported and always returns 0" )] ;
function	 SetMaxRegisteredKids(dwCount as dword) as dword
return 0   

[Obsolete( "'SetMaxThreadDynSize()' is not supported and always returns 0" )] ;
function	 SetMaxThreadDynSize(dwBytes as dword) as dword
return 0   

[Obsolete( "'SetWipeDynSpace()' is not supported and always returns FALSE" )] ;
function	 SetWipeDynSpace(lWipe as logic) as logic
return false   


[Obsolete( "'PtrLen()' is not supported and always returns 0" )] ;
function	 PtrLen( lpv as IntPtr ) as dword
return 0

[Obsolete( "'PtrLenWrite()' is not supported and always returns 0" )] ;
function	 PtrLenWrite( lpv as IntPtr ) as dword
return 0


[Obsolete( "'ExitVOThread()' is not supported and has no effect" )] ;
function	 ExitVOThread(nRetVal as int) as void
return   


[Obsolete( "'WriteAtomTable()' is not supported always returns 0" )] ;
function	 WriteAtomTable(hf as dword) as dword
return 0  

[Obsolete( "'CreateVOThread()' is not supported always returns IntPtr.Zero" )] ;
function	 CreateVOThread(pSecAttr as IntPtr,nStackSize as dword,pFunc as IntPtr,pParam as IntPtr,dwFlags as dword,pdwID ref dword ) as IntPtr
return IntPtr.Zero

[Obsolete( "'TerminateVOThread()' is not supported always returns FALSE" )] ;
function	 TerminateVOThread(pH as IntPtr,dwCode as dword) as logic
return false   


[Obsolete( "'Usual2Variant()' is not supported always returns FALSE" )] ;
function	 Usual2Variant(pItem as IntPtr,pvarg as IntPtr) as logic
return false   

[Obsolete( "'Variant2Usual()' is not supported always returns FALSE" )] ;
function	 Variant2Usual(pvarg as IntPtr,pItem as IntPtr) as logic
return false 



[Obsolete( "'UnRegisterKid()' is not supported always returns FALSE" )];
function	 UnRegisterKid(ptrKid as IntPtr) as logic
return false   

[Obsolete( "'UnRegisterKid()' is not supported always returns 0" )];
function	 VOSendMessage(hwnd as IntPtr,nMsg as dword,dwParam as dword,lParam as long) as long
return 0	


[Obsolete( "'LabelJump()' is not supported and has no effect" )] ;
function	 LabelJump(uError as __Usual) as __Usual
return __Usual._NIL   

[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
function	 LabelPop() as void
return


[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
function	 LabelPush(ptrLabel as IntPtr) as void
return 

[Obsolete( "'IsVOString()' is not supported always returns FALSE" )];
function	 IsVOString(cString as string) as logic
return false   


[Obsolete( "'Multi2Wide()' is not supported always returns the original string" )];
function	 Multi2Wide(c as string) as string
return c

[Obsolete( "'Wide2Multi()' is not supported always returns the original string" )];
function	 Wide2Multi(cBstr as string) as string
return cBStr

[Obsolete( "'OldSpaceFreeString()' is not supported and has no effect" )] ;
function	 OldSpaceFreeString(c as string) as void
return

[Obsolete( "'DbcsNext()' is not supported and always returns NULL_PSZ" )] ;
function DbcsNext(pszSource as __Psz) as __Psz
return IntPtr.Zero

[Obsolete( "'DbcsPrev()' is not supported and always returns NULL_PSZ" )] ;
function DbcsPrev(pszSource as __Psz,pszCurr as __Psz) as __Psz
return IntPtr.Zero


[Obsolete( "'KillAtomTable()' is not supported and has no effect")] ;
function KillAtomTable() as void
return	

[Obsolete( "'LeaveCGCSection()' is not supported and has no effect")] ;
function	 LeaveCGCSection() as void
return

[Obsolete( "'EnterCGCSection()' is not supported and has no effect")] ;
function	 EnterCGCSection() as void
return  


[Obsolete( "'KidStackFree()' is not supported and always returns 0" )] ;
function KidStackFree() as dword
return 0   

[Obsolete( "'KidStackSize()' is not supported and always returns 0" )] ;
function KidStackSize() as dword
return 0   


[Obsolete( "'LongJmp()' is not supported and has no effect" )] ;
function LongJmp(strucMark as IntPtr,n as int) as void
return



[Obsolete( "'VOEnterCriticalSection()' is not supported and has no effect" )] ;
function	 VOEnterCriticalSection(lpCriticalSection as IntPtr) as void
return

[Obsolete( "'VOLeaveCriticalSection()' is not supported and has no effect" )] ;	
function	 VOLeaveCriticalSection(lpCriticalSection as IntPtr) as void
return



[Obsolete( "'AtomExit()' is not supported and always returns 0" )] ;
function	 AtomExit() as dword
return 0   

[Obsolete( "'AtomInit()' is not supported and always returns 0" )] ;
function AtomInit() as dword
return 0   

[Obsolete( "'AtomTableSetDirty()' is not supported and always returns FALSE" )] ;
function AtomTableGetDirty() as logic
return false   

[Obsolete( "'AtomTableSetDirty()' is not supported and always returns 0" )] ;
function AtomTableSetDirty(f as logic) as dword
return 0   



[Obsolete( "'AllocInstance()' is not supported and returns the same as CreateInstance" )] ;
function	 AllocInstance(symClassName as __Usual) as object
return CreateInstance(symClassName)

[Obsolete( "'AllocInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
function	 AllocInstanceStatic(symClassName as __Usual) as object
return CreateInstance(symClassName)






[Obsolete( "'CreateInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
function CreateInstanceStatic(symClassName as __Usual) as object
return CreateInstance(symClassName)

[Obsolete( "'MathCheck()' is not supported and always returns FALSE" )] ;
function MathCheck() as logic
return false   

[Obsolete( "'MathInit()' is not supported and has no effect" )] ;
function MathInit() as void
return

[Obsolete( "'MyDal()' is not supported and has no effect" )] ;
function MyDal() as void
return

[Obsolete( "'MyDalFloat()' is not supported and has no effect" )] ;
function MyDalFloat() as void
return

[Obsolete( "'MyDalFloatFSTP()' is not supported and has no effect" )] ;
function MyDalFloatFSTP() as void
return

[Obsolete( "'MyDalPtr()' is not supported and has no effect" )] ;
function MyDalPtr() as void
return



[Obsolete( "'DynAllocEnd()' is not supported and has no effect" )] ;
function DynAllocEnd() as void
return  

[Obsolete( "'DynAllocStart()' is not supported and always returns 0" )] ;
function DynAllocStart() as dword
return 0   


[Obsolete( "'DynCheckErrorInfo()' is not supported and always returns 0" )] ;
function DynCheckErrorInfo() as dword
return 0   



[Obsolete( "'DynCheckErrorInfo()' is not supported and always returns NULL_SYMBOL" )] ;
function	 DynCheckErrorSym() as __Symbol
return null_symbol   




[Obsolete( "'CreateGCDump()' is not supported and always returns 0" )] ;
function CreateGCDump(pDump as IntPtr,nValType as dword) as dword
return 0   



[Obsolete( "'DBToSB()' is not supported", true )] ;
function	 DBToSB(c as string) as string
return string.Empty 

[Obsolete( "'ToHira()' is not supported", true )] ;
function	 ToHira(c as string) as string
return String.Empty   

[Obsolete( "'ToJNum()' is not supported", true )] ;
function	 ToJNum(c as string) as string
return String.Empty   


[Obsolete( "'ToKata()' is not supported", true )] ;
function	 ToKata(c as string) as string
return String.Empty  


[Obsolete( "'IsKanji()' is not supported", true )] ;
FUNCTION IsKanji(c AS STRING) AS LOGIC
	RETURN FALSE   
