//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// All Dynamic memory functions that are not supported or dummies
// And also all the OldSpace functions

[Obsolete( "'DynMemDump()' is not supported and always returns FALSE" )] ;
function DynMemDump(cFile as string,nOptions as dword) as logic
    return false   

[Obsolete( "'DynCheckError()' is not supported and always returns 0" )] ;
function DynCheckError() as dword
    return 0

[Obsolete( "'DynInfoFree()' is not supported and always returns 0" )] ;
function DynInfoFree() as dword
    return 0

[Obsolete( "'DynInfoMax()' is not supported and always returns 0" )] ;
function DynInfoMax() as dword
    return 0

[Obsolete( "'DynInfoSize()' is not supported and always returns 0" )] ;
function DynInfoSize() as dword
    return 0

[Obsolete( "'DynInfoUsed()' is not supported and always returns 0" )] ;
function DynInfoUsed() as dword
    return 0

[Obsolete( "'DynLock()' is not supported and has no effect" )] ;
function DynLock() as void
    return

[Obsolete( "'DynProtect()' is not supported and always returns FALSE" )] ;
function DynProtect( lSet as logic ) as logic
    return false

[Obsolete( "'DynShrink()' is not supported and always returns 0" )] ;
function DynShrink() as dword
    return 0

[Obsolete( "'DynSize()' is not supported and always returns 0" )] ;
function DynSize() as dword
    return 0


[Obsolete( "DynStack2Array() is not supported and returns an empty array")] ; 
function DynStack2Array() as array
    return null_array   


[Obsolete( "DynToOldSpace() is not supported and returns the original usual")] ; 
function DynToOldSpace( u as usual ) as usual
    return u

[Obsolete( "DynToOldSpaceArray() is not supported and returns the original array")] ; 
function DynToOldSpaceArray( a as array ) as array
    return a

[Obsolete( "DynToOldSpaceFloat() is not supported and returns the original float", true )] ; 
function DynToOldSpaceFloat( f as float ) as float
    return f

[Obsolete( "DynToOldSpaceObject() is not supported and returns the original object" )] ; 
function DynToOldSpaceObject( o as object ) as object
    return o

[Obsolete( "DynToOldSpaceString() is not supported and returns the original string" )] ; 
function DynToOldSpaceString( s as string ) as string
    return s

[Obsolete( "'DynUnlock()' is not supported and has no effect" )] ;
function DynUnLock() as void
    return


[Obsolete( "'IsOldSpaceFloat()' is not supported and always returns FALSE" )] ;
function IsOldSpaceFloat(f as float) as logic
    return false   


[Obsolete( "'IsOldSpaceString()' is not supported and always returns FALSE" )] ;
function IsOldSpaceString(c as string) as logic
    return false 


[Obsolete( "'OldSpaceFreeFloat()' is not supported and has no effect")] ;
function OldSpaceFreeFloat(f as float) as void
    return



[Obsolete( "'ReDal()' is not supported and has no effect" )] ;
function ReDal() as void
    return



[Obsolete( "'RTExit()' is not supported and always returns 0" )] ;
function RTExit() as dword
    return 0   

[Obsolete( "'RegisterKid()' is not supported and has no effect" )] ;
function RegisterKid(ptrKid as IntPtr,dwCount as dword,lItem as logic) as void
    return

[Obsolete( "'FreeStaticObject()' is not supported and always returns FALSE" )] ;
function FreeStaticObject(o as object) as logic
    return false   


[Obsolete( "'Memory()' is not supported and always returns 0" )] ;
function Memory(iFunc as int) as dword
	return 0   

[Obsolete( "'OldSpaceFree()' is not supported and has no effect" )] ;
function OldSpaceFree(u as usual) as void
    return

[Obsolete( "'OldSpaceFreeObject()' is not supported and has no effect" )] ;
function OldSpaceFreeObject(o as object) as void
    return


[Obsolete( "'IsOldSpaceArray()' is not supported and always returns FALSE" )] ;
function IsOldSpaceArray(a as array) as logic
    return false   

[Obsolete( "'OldSpaceFreeArray()' is not supported and has no effect" )] ;
function OldSpaceFreeArray(a as array) as void
    return 

[Obsolete( "'IsOldSpace()' is not supported and always returns FALSE" )] ;
function IsOldSpace(u as usual) as logic
    return false   

[Obsolete( "'IsOldSpaceObject()' is not supported and always returns FALSE" )] ;
function IsOldSpaceObject(o as object) as logic
    return false   

[Obsolete( "'MemCheckPtr()' is not supported and always returns TRUE" )] ;
unsafe	 function MemCheckPtr( pMemory as ptr, dwSize as dword ) as logic
return true


[Obsolete( "'SetKidStackSize()' is not supported and always returns 0" )] ;
function SetKidStackSize(dwBytes as dword) as dword
    return 0   

[Obsolete( "'SetMaxDynSize()' is not supported and always returns 0" )] ;
function SetMaxDynSize(dwBytes as dword) as dword
    return 0   

[Obsolete( "'SetMaxRegisteredAxitMethods()' is not supported and always returns 0" )] ;
function SetMaxRegisteredAxitMethods(dwCount as dword) as dword
    return 0   

[Obsolete( "'SetMaxRegisteredKids()' is not supported and always returns 0" )] ;
function SetMaxRegisteredKids(dwCount as dword) as dword
    return 0   

[Obsolete( "'SetMaxThreadDynSize()' is not supported and always returns 0" )] ;
function SetMaxThreadDynSize(dwBytes as dword) as dword
    return 0   

[Obsolete( "'SetWipeDynSpace()' is not supported and always returns FALSE" )] ;
function SetWipeDynSpace(lWipe as logic) as logic
    return false   


[Obsolete( "'PtrLen()' is not supported and always returns 0" )] ;
function PtrLen( lpv as IntPtr ) as dword
    return 0

[Obsolete( "'PtrLenWrite()' is not supported and always returns 0" )] ;
function PtrLenWrite( lpv as IntPtr ) as dword
    return 0


[Obsolete( "'ExitVOThread()' is not supported and has no effect" )] ;
function ExitVOThread(nRetVal as int) as void
    return   


[Obsolete( "'WriteAtomTable()' is not supported always returns 0" )] ;
function WriteAtomTable(hf as dword) as dword
    return 0  

[Obsolete( "'CreateVOThread()' is not supported always returns IntPtr.Zero" )] ;
function CreateVOThread(pSecAttr as IntPtr,nStackSize as dword,pFunc as IntPtr,pParam as IntPtr,dwFlags as dword,pdwID ref dword ) as IntPtr
    return IntPtr.Zero


[Obsolete( "'StrEvaluate()' is not supported", true )] ;
function StrEvaluate( s as string ) as string
   return s

[Obsolete( "'TerminateVOThread()' is not supported always returns FALSE" )] ;
function TerminateVOThread(pH as IntPtr,dwCode as dword) as logic
    return false   


[Obsolete( "'Usual2Variant()' is not supported always returns FALSE" )] ;
function Usual2Variant(pItem as IntPtr,pvarg as IntPtr) as logic
    return false   

[Obsolete( "'Variant2Usual()' is not supported always returns FALSE" )] ;
function Variant2Usual(pvarg as IntPtr,pItem as IntPtr) as logic
    return false 



[Obsolete( "'UnRegisterKid()' is not supported always returns FALSE" )];
function UnRegisterKid(ptrKid as IntPtr) as logic
    return false   

[Obsolete( "'UnRegisterKid()' is not supported always returns 0" )];
function VOSendMessage(hwnd as IntPtr,nMsg as dword,dwParam as dword,lParam as long) as long
    return 0	


[Obsolete( "'LabelJump()' is not supported and has no effect" )] ;
function LabelJump(uError as usual) as usual
    return NIL   

[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
function LabelPop() as void
    return


[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
function LabelPush(ptrLabel as IntPtr) as void
    return 

[Obsolete( "'IsVOString()' is not supported always returns FALSE" )];
function IsVOString(cString as string) as logic
    return false   


[Obsolete( "'Multi2Wide()' is not supported always returns the original string" )];
function Multi2Wide(c as string) as string
    return c

[Obsolete( "'Wide2Multi()' is not supported always returns the original string" )];
function Wide2Multi(cBstr as string) as string
    return cBStr

[Obsolete( "'OldSpaceFreeString()' is not supported and has no effect" )] ;
function OldSpaceFreeString(c as string) as void
    return

[Obsolete( "'DbcsNext()' is not supported and always returns NULL_PSZ" )] ;
function DbcsNext(pszSource as psz) as psz
    return IntPtr.Zero

[Obsolete( "'DbcsPrev()' is not supported and always returns NULL_PSZ" )] ;
function DbcsPrev(pszSource as psz,pszCurr as psz) as psz
    return IntPtr.Zero


[Obsolete( "'KillAtomTable()' is not supported and has no effect")] ;
function KillAtomTable() as void
    return	

[Obsolete( "'LeaveCGCSection()' is not supported and has no effect")] ;
function LeaveCGCSection() as void
    return

[Obsolete( "'EnterCGCSection()' is not supported and has no effect")] ;
function EnterCGCSection() as void
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
function VOEnterCriticalSection(lpCriticalSection as IntPtr) as void
    return

[Obsolete( "'VOLeaveCriticalSection()' is not supported and has no effect" )] ;	
function VOLeaveCriticalSection(lpCriticalSection as IntPtr) as void
    return



[Obsolete( "'AtomExit()' is not supported and always returns 0" )] ;
function AtomExit() as dword
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
function AllocInstance(symClassName as usual) as object
    return CreateInstance(symClassName)

[Obsolete( "'AllocInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
function AllocInstanceStatic(symClassName as usual) as object
    return CreateInstance(symClassName)






[Obsolete( "'CreateInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
function CreateInstanceStatic(symClassName as usual) as object
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
function DynCheckErrorSym() as symbol
    return null_symbol   




[Obsolete( "'CreateGCDump()' is not supported and always returns 0" )] ;
function CreateGCDump(pDump as IntPtr,nValType as dword) as dword
    return 0   



[Obsolete( "'DBToSB()' is not supported", true )] ;
function DBToSB(c as string) as string
    return string.Empty 

[Obsolete( "'ToHira()' is not supported", true )] ;
function ToHira(c as string) as string
    return String.Empty   

[Obsolete( "'ToJNum()' is not supported", true )] ;
function ToJNum(c as string) as string
    return String.Empty   


[Obsolete( "'ToKata()' is not supported", true )] ;
function ToKata(c as string) as string
    return String.Empty  


[Obsolete( "'IsKanji()' is not supported", true )] ;
function IsKanji(c as string) as logic
    return false   

[Obsolete( "'WagnerInit()' is not supported and always returns 0" )] ;
function WagnerInit() as dword
    return     0   


[Obsolete( "'WagnerExit()' is not supported and always returns 0" )] ;
function WagnerExit() as dword
    return     0   


[Obsolete( "'Buffer()' is not supported, use MemAlloc() and MemFree() instead", true )] ;
FUNCTION Buffer( n AS DWORD ) AS STRING
  return null

[Obsolete( "'GetPrivPtr()' is not supported and always returns IntPtr.Zero" )] ;
FUNCTION GetPrivPtr() AS IntPtr
	RETURN IntPtr.Zero   

[Obsolete( "'GetStgServer()' is not supported and always returns ''" )] ;
FUNCTION GetStgServer(pStgRoot AS IntPtr,cSubStorage AS STRING) AS STRING
	RETURN String.Empty   

[Obsolete( "'FxOpen()' is not supported and always returns IntPtr.Zero" )] ;
function FxOpen(cFile as string,dwMode as dword,cPath as string) as IntPtr
	return IntPtr.Zero

[Obsolete( "'ErrorBuild()' is not supported and always returns an empty Error Object" )] ;
function ErrorBuild(pErrInfo as IntPtr) as XSharp.ERROR
	return  XSharp.Error{ES_ERROR}


[Obsolete( "'ErrorExec()' is not supported and always returns NIL" )] ;
function ErrorExec(pErrInfo as IntPtr) as Usual
return NIL   


[Obsolete( "'JNTOCMONTH()' is not supported and always returns ''" )] ;
FUNCTION JNTOCMONTH(wMonth AS WORD) AS STRING
	RETURN String.Empty   

[Obsolete( "'JNTOCYEAR()' is not supported and always returns ''" )] ;
FUNCTION JNTOCYEAR(wYear AS WORD) AS STRING
	RETURN String.Empty   


[Obsolete( "'CreateAtomTable()' is not supported and always returns 0" )] ;
FUNCTION CreateAtomTable(dwVS AS DWORD,dwStep AS DWORD) AS DWORD
RETURN 0   

[Obsolete( "'ReadAtomTable()' is not supported and always returns 0" )] ;
FUNCTION ReadAtomTable(hf AS DWORD) AS DWORD
RETURN 0   

[Obsolete( "'PszLenW()' is not supported and always returns 0" )] ;
function PszLenW(pszUnicode as Psz) as dword
	return 0

[Obsolete( "'ReleaseString()' is not supported and always returns NULL_PSZ" )] ;
function ReleaseString() AS Psz
	return null_psz

[Obsolete( "'AMemSize()' is not supported and always returns 0" )] ;
FUNCTION AMemSize(a AS Array) AS DWORD
	RETURN 0   

[Obsolete( "'APageCount()' is not supported and always returns 0" )] ;
FUNCTION APageCount(a AS Array) AS DWORD
	RETURN 0   


[Obsolete( "'ArrayGetPtr()' is not supported and always returns IntPtr.Zero" )] ;
function ArrayGetPtr(a as array,dwEl as dword) as intPtr
	return IntPtr.Zero


[Obsolete( "'IVarPutSuper()' is not supported and always returns the value that is set" )] ;
function IVarPutSuper(o as object,symIvar as Symbol,u as Usual,symClassName as Symbol) as Usual
	return u   

[Obsolete( "'IVarGetSuper()' is not supported and always returns NIL" )] ;
function IVarGetSuper(o as object,symIvar as Symbol,symClassName as Symbol) as Usual
	return NIL

