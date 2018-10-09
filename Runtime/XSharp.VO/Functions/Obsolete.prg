//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// All Dynamic memory functions that are not supported or dummies
// And also all the OldSpace functions

/// <exclude/>
[Obsolete( "'DynMemDump()' is not supported and always returns FALSE" )] ;
FUNCTION DynMemDump(cFile AS STRING,nOptions AS DWORD) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'DynCheckError()' is not supported and always returns 0" )] ;
FUNCTION DynCheckError() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynInfoFree()' is not supported and always returns 0" )] ;
FUNCTION DynInfoFree() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynInfoMax()' is not supported and always returns 0" )] ;
FUNCTION DynInfoMax() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynInfoSize()' is not supported and always returns 0" )] ;
FUNCTION DynInfoSize() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynInfoUsed()' is not supported and always returns 0" )] ;
FUNCTION DynInfoUsed() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynLock()' is not supported and has no effect" )] ;
FUNCTION DynLock() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'DynProtect()' is not supported and always returns FALSE" )] ;
FUNCTION DynProtect( lSet AS LOGIC ) AS LOGIC
	RETURN FALSE
	
/// <exclude/>
[Obsolete( "'DynShrink()' is not supported and always returns 0" )] ;
FUNCTION DynShrink() AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'DynSize()' is not supported and always returns 0" )] ;
FUNCTION DynSize() AS DWORD
	RETURN 0
	
	
/// <exclude/>
[Obsolete( "DynStack2Array() is not supported and returns an empty array")] ; 
FUNCTION DynStack2Array() AS ARRAY
	RETURN NULL_ARRAY   
	
	
/// <exclude/>
[Obsolete( "DynToOldSpace() is not supported and returns the original usual")] ; 
FUNCTION DynToOldSpace( u AS USUAL ) AS USUAL
	RETURN u
	
/// <exclude/>
[Obsolete( "DynToOldSpaceArray() is not supported and returns the original array")] ; 
FUNCTION DynToOldSpaceArray( a AS ARRAY ) AS ARRAY
	RETURN a
	
/// <exclude/>
[Obsolete( "DynToOldSpaceFloat() is not supported and returns the original float" )] ; 
FUNCTION DynToOldSpaceFloat( f AS FLOAT ) AS FLOAT
	RETURN f
	
/// <exclude/>
[Obsolete( "DynToOldSpaceObject() is not supported and returns the original object" )] ; 
FUNCTION DynToOldSpaceObject( o AS OBJECT ) AS OBJECT
	RETURN o
	
/// <exclude/>
[Obsolete( "DynToOldSpaceString() is not supported and returns the original string" )] ; 
FUNCTION DynToOldSpaceString( s AS STRING ) AS STRING
	RETURN s
	
/// <exclude/>
[Obsolete( "'DynUnlock()' is not supported and has no effect" )] ;
FUNCTION DynUnLock() AS VOID
	RETURN
	
	
/// <exclude/>
[Obsolete( "'IsOldSpaceFloat()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceFloat(f AS FLOAT) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'IsOldSpaceString()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceString(c AS STRING) AS LOGIC
	RETURN FALSE 
	
	
/// <exclude/>
[Obsolete( "'OldSpaceFreeFloat()' is not supported and has no effect")] ;
FUNCTION OldSpaceFreeFloat(f AS FLOAT) AS VOID
	RETURN
	
	
	
/// <exclude/>
[Obsolete( "'ReDal()' is not supported and has no effect" )] ;
FUNCTION ReDal() AS VOID
	RETURN
	
	
	
/// <exclude/>
[Obsolete( "'RTExit()' is not supported and always returns 0" )] ;
FUNCTION RTExit() AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'RegisterKid()' is not supported and has no effect" )] ;
FUNCTION RegisterKid(ptrKid AS IntPtr,dwCount AS DWORD,lItem AS LOGIC) AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'FreeStaticObject()' is not supported and always returns FALSE" )] ;
FUNCTION FreeStaticObject(o AS OBJECT) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'Memory()' is not supported and always returns 0" )] ;
FUNCTION Memory(iFunc AS INT) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'OldSpaceFree()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFree(u AS USUAL) AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'OldSpaceFreeObject()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFreeObject(o AS OBJECT) AS VOID
	RETURN
	
	
/// <exclude/>
[Obsolete( "'IsOldSpaceArray()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceArray(a AS ARRAY) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'OldSpaceFreeArray()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFreeArray(a AS ARRAY) AS VOID
	RETURN 
	
/// <exclude/>
[Obsolete( "'IsOldSpace()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpace(u AS USUAL) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'IsOldSpaceObject()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceObject(o AS OBJECT) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'MemCheckPtr()' is not supported and always returns TRUE" )] ;
UNSAFE	 FUNCTION MemCheckPtr( pMemory AS PTR, dwSize AS DWORD ) AS LOGIC
	RETURN	 TRUE
	
	
/// <exclude/>
[Obsolete( "'SetKidStackSize()' is not supported and always returns 0" )] ;
FUNCTION SetKidStackSize(dwBytes AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'SetMaxDynSize()' is not supported and always returns 0" )] ;
FUNCTION SetMaxDynSize(dwBytes AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'SetMaxRegisteredAxitMethods()' is not supported and always returns 0" )] ;
FUNCTION SetMaxRegisteredAxitMethods(dwCount AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'SetMaxRegisteredKids()' is not supported and always returns 0" )] ;
FUNCTION SetMaxRegisteredKids(dwCount AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'SetMaxThreadDynSize()' is not supported and always returns 0" )] ;
FUNCTION SetMaxThreadDynSize(dwBytes AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'SetWipeDynSpace()' is not supported and always returns FALSE" )] ;
FUNCTION SetWipeDynSpace(lWipe AS LOGIC) AS LOGIC
	RETURN FALSE   
	
	
/// <exclude/>
[Obsolete( "'PtrLen()' is not supported and always returns 0" )] ;
FUNCTION PtrLen( lpv AS IntPtr ) AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'PtrLenWrite()' is not supported and always returns 0" )] ;
FUNCTION PtrLenWrite( lpv AS IntPtr ) AS DWORD
	RETURN 0
	
	
/// <exclude/>
[Obsolete( "'ExitVOThread()' is not supported and has no effect" )] ;
FUNCTION ExitVOThread(nRetVal AS INT) AS VOID
	RETURN   
	
	
/// <exclude/>
[Obsolete( "'WriteAtomTable()' is not supported always returns 0" )] ;
FUNCTION WriteAtomTable(hf AS DWORD) AS DWORD
	RETURN 0  
	
/// <exclude/>
[Obsolete( "'CreateVOThread()' is not supported always returns IntPtr.Zero" )] ;
FUNCTION CreateVOThread(pSecAttr AS IntPtr,nStackSize AS DWORD,pFunc AS IntPtr,pParam AS IntPtr,dwFlags AS DWORD,pdwID REF DWORD ) AS IntPtr
	RETURN IntPtr.Zero
	
	
/// <exclude/>
[Obsolete( "'StrEvaluate()' is not supported" )] ;
FUNCTION StrEvaluate( s AS STRING ) AS STRING
	RETURN s
	
/// <exclude/>
[Obsolete( "'TerminateVOThread()' is not supported always returns FALSE" )] ;
FUNCTION TerminateVOThread(pH AS IntPtr,dwCode AS DWORD) AS LOGIC
	RETURN FALSE   
	
	
/// <exclude/>
[Obsolete( "'Usual2Variant()' is not supported always returns FALSE" )] ;
FUNCTION Usual2Variant(pItem AS IntPtr,pvarg AS IntPtr) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'Variant2Usual()' is not supported always returns FALSE" )] ;
FUNCTION Variant2Usual(pvarg AS IntPtr,pItem AS IntPtr) AS LOGIC
	RETURN FALSE 
	
	
	
/// <exclude/>
[Obsolete( "'UnRegisterKid()' is not supported always returns FALSE" )];
FUNCTION UnRegisterKid(ptrKid AS IntPtr) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'UnRegisterKid()' is not supported always returns 0" )];
FUNCTION VOSendMessage(hwnd AS IntPtr,nMsg AS DWORD,dwParam AS DWORD,lParam AS LONG) AS LONG
	RETURN 0	
	
	
/// <exclude/>
[Obsolete( "'LabelJump()' is not supported and has no effect" )] ;
FUNCTION LabelJump(uError AS USUAL) AS USUAL
	RETURN NIL   
	
/// <exclude/>
[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
FUNCTION LabelPop() AS VOID
	RETURN
	
	
/// <exclude/>
[Obsolete( "'LabelPush()' is not supported and has no effect" )] ;
FUNCTION LabelPush(ptrLabel AS IntPtr) AS VOID
	RETURN 
	
/// <exclude/>
[Obsolete( "'IsVOString()' is not supported always returns FALSE" )];
FUNCTION IsVOString(cString AS STRING) AS LOGIC
	RETURN FALSE   
	
	
/// <exclude/>
[Obsolete( "'Multi2Wide()' is not supported always returns the original string" )];
FUNCTION Multi2Wide(c AS STRING) AS STRING
	RETURN c
	
/// <exclude/>
[Obsolete( "'Wide2Multi()' is not supported always returns the original string" )];
FUNCTION Wide2Multi(cBstr AS STRING) AS STRING
	RETURN cBStr
	
/// <exclude/>
[Obsolete( "'OldSpaceFreeString()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFreeString(c AS STRING) AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'DbcsNext()' is not supported and always returns NULL_PSZ" )] ;
FUNCTION DbcsNext(pszSource AS PSZ) AS PSZ
	RETURN IntPtr.Zero
	
/// <exclude/>
[Obsolete( "'DbcsPrev()' is not supported and always returns NULL_PSZ" )] ;
FUNCTION DbcsPrev(pszSource AS PSZ,pszCurr AS PSZ) AS PSZ
	RETURN IntPtr.Zero
	
	
/// <exclude/>
[Obsolete( "'KillAtomTable()' is not supported and has no effect")] ;
FUNCTION KillAtomTable() AS VOID
	RETURN	
	
/// <exclude/>
[Obsolete( "'LeaveCGCSection()' is not supported and has no effect")] ;
FUNCTION LeaveCGCSection() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'EnterCGCSection()' is not supported and has no effect")] ;
FUNCTION EnterCGCSection() AS VOID
	RETURN  
	
	
/// <exclude/>
[Obsolete( "'KidStackFree()' is not supported and always returns 0" )] ;
FUNCTION KidStackFree() AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'KidStackSize()' is not supported and always returns 0" )] ;
FUNCTION KidStackSize() AS DWORD
	RETURN 0   
	
	
/// <exclude/>
[Obsolete( "'LongJmp()' is not supported and has no effect" )] ;
FUNCTION LongJmp(strucMark AS IntPtr,n AS INT) AS VOID
	RETURN
	
	
	
/// <exclude/>
[Obsolete( "'VOEnterCriticalSection()' is not supported and has no effect" )] ;
FUNCTION VOEnterCriticalSection(lpCriticalSection AS IntPtr) AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'VOLeaveCriticalSection()' is not supported and has no effect" )] ;	
FUNCTION VOLeaveCriticalSection(lpCriticalSection AS IntPtr) AS VOID
	RETURN
	
	
	
/// <exclude/>
[Obsolete( "'AtomExit()' is not supported and always returns 0" )] ;
FUNCTION AtomExit() AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'AtomInit()' is not supported and always returns 0" )] ;
FUNCTION AtomInit() AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'AtomTableSetDirty()' is not supported and always returns FALSE" )] ;
FUNCTION AtomTableGetDirty() AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'AtomTableSetDirty()' is not supported and always returns 0" )] ;
FUNCTION AtomTableSetDirty(f AS LOGIC) AS DWORD
	RETURN 0   
	
	
	
/// <exclude/>
[Obsolete( "'AllocInstance()' is not supported and returns the same as CreateInstance" )] ;
FUNCTION AllocInstance(symClassName AS USUAL) AS OBJECT
	RETURN CreateInstance(symClassName)
	
/// <exclude/>
[Obsolete( "'AllocInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
FUNCTION AllocInstanceStatic(symClassName AS USUAL) AS OBJECT
	RETURN CreateInstance(symClassName)
	
	
/// <exclude/>
[Obsolete( "'CreateInstanceStatic()' is not supported and returns the same as CreateInstance" )] ;
FUNCTION CreateInstanceStatic(args PARAMS USUAL[]) AS OBJECT 
	RETURN CreateInstance(args)
	
/// <exclude/>
[Obsolete( "'MathCheck()' is not supported and always returns FALSE" )] ;
FUNCTION MathCheck() AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'MathInit()' is not supported and has no effect" )] ;
FUNCTION MathInit() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'MyDal()' is not supported and has no effect" )] ;
FUNCTION MyDal() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'MyDalFloat()' is not supported and has no effect" )] ;
FUNCTION MyDalFloat() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'MyDalFloatFSTP()' is not supported and has no effect" )] ;
FUNCTION MyDalFloatFSTP() AS VOID
	RETURN
	
/// <exclude/>
[Obsolete( "'MyDalPtr()' is not supported and has no effect" )] ;
FUNCTION MyDalPtr() AS VOID
	RETURN
	
	
	
/// <exclude/>
[Obsolete( "'DynAllocEnd()' is not supported and has no effect" )] ;
FUNCTION DynAllocEnd() AS VOID
	RETURN  
	
/// <exclude/>
[Obsolete( "'DynAllocStart()' is not supported and always returns 0" )] ;
FUNCTION DynAllocStart() AS DWORD
	RETURN 0   
	
	
/// <exclude/>
[Obsolete( "'DynCheckErrorInfo()' is not supported and always returns 0" )] ;
FUNCTION DynCheckErrorInfo() AS DWORD
	RETURN 0   
	
	
	
/// <exclude/>
[Obsolete( "'DynCheckErrorInfo()' is not supported and always returns NULL_SYMBOL" )] ;
FUNCTION DynCheckErrorSym() AS SYMBOL
	RETURN NULL_SYMBOL   
	
	
	
	
/// <exclude/>
[Obsolete( "'CreateGCDump()' is not supported and always returns 0" )] ;
FUNCTION CreateGCDump(pDump AS IntPtr,nValType AS DWORD) AS DWORD
	RETURN 0   
	
	
	
/// <exclude/>
[Obsolete( "'DBToSB()' is not supported" )] ;
FUNCTION DBToSB(c AS STRING) AS STRING
	RETURN string.Empty 
	
/// <exclude/>
[Obsolete( "'ToHira()' is not supported" )] ;
FUNCTION ToHira(c AS STRING) AS STRING
	RETURN String.Empty   
	
/// <exclude/>
[Obsolete( "'ToJNum()' is not supported" )] ;
FUNCTION ToJNum(c AS STRING) AS STRING
	RETURN String.Empty   
	
	
/// <exclude/>
[Obsolete( "'ToKata()' is not supported" )] ;
FUNCTION ToKata(c AS STRING) AS STRING
	RETURN String.Empty  
	
	
/// <exclude/>
[Obsolete( "'IsKanji()' is not supported" )] ;
FUNCTION IsKanji(c AS STRING) AS LOGIC
	RETURN FALSE   
	
/// <exclude/>
[Obsolete( "'WagnerInit()' is not supported and always returns 0" )] ;
FUNCTION WagnerInit() AS DWORD
	RETURN     0   
	
	
/// <exclude/>
[Obsolete( "'WagnerExit()' is not supported and always returns 0" )] ;
FUNCTION WagnerExit() AS DWORD
	RETURN     0   
	
	
/// <exclude/>
[Obsolete( "'Buffer()' is not supported, use MemAlloc() and MemFree() instead" )] ;
FUNCTION Buffer( n AS DWORD ) AS STRING
	RETURN NULL
	
/// <exclude/>
[Obsolete( "'GetPrivPtr()' is not supported and always returns IntPtr.Zero" )] ;
FUNCTION GetPrivPtr() AS IntPtr
	RETURN IntPtr.Zero   
	
/// <exclude/>
[Obsolete( "'GetStgServer()' is not supported and always returns ''" )] ;
FUNCTION GetStgServer(pStgRoot AS IntPtr,cSubStorage AS STRING) AS STRING
	RETURN String.Empty   
	
/// <exclude/>
[Obsolete( "'FxOpen()' is not supported and always returns IntPtr.Zero" )] ;
FUNCTION FxOpen(cFile AS STRING,dwMode AS DWORD,cPath AS STRING) AS IntPtr
	RETURN IntPtr.Zero
	
/// <exclude/>
[Obsolete( "'ErrorExec()' is not supported and always returns NIL" )] ;
FUNCTION ErrorExec(pErrInfo AS IntPtr) AS USUAL
	RETURN	 NIL   
	
	
/// <exclude/>
[Obsolete( "'JNTOCMONTH()' is not supported and always returns ''" )] ;
FUNCTION JNTOCMONTH(wMonth AS WORD) AS STRING
	RETURN String.Empty   
	
/// <exclude/>
[Obsolete( "'JNTOCYEAR()' is not supported and always returns ''" )] ;
FUNCTION JNTOCYEAR(wYear AS WORD) AS STRING
	RETURN String.Empty   
	
	
/// <exclude/>
[Obsolete( "'CreateAtomTable()' is not supported and always returns 0" )] ;
FUNCTION CreateAtomTable(dwVS AS DWORD,dwStep AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'ReadAtomTable()' is not supported and always returns 0" )] ;
FUNCTION ReadAtomTable(hf AS DWORD) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'PszLenW()' is not supported and always returns 0" )] ;
FUNCTION PszLenW(pszUnicode AS PSZ) AS DWORD
	RETURN 0
	
/// <exclude/>
[Obsolete( "'ReleaseString()' is not supported and always returns NULL_PSZ" )] ;
FUNCTION ReleaseString() AS PSZ
	RETURN NULL_PSZ
	
/// <exclude/>
[Obsolete( "'AMemSize()' is not supported and always returns 0" )] ;
FUNCTION AMemSize(a AS ARRAY) AS DWORD
	RETURN 0   
	
/// <exclude/>
[Obsolete( "'APageCount()' is not supported and always returns 0" )] ;
FUNCTION APageCount(a AS ARRAY) AS DWORD
	RETURN 0   
	
	
/// <exclude/>
[Obsolete( "'ArrayGetPtr()' is not supported and always returns IntPtr.Zero" )] ;
FUNCTION ArrayGetPtr(a AS ARRAY,dwEl AS DWORD) AS intPtr
	RETURN IntPtr.Zero
	
	
/// <exclude/>
[Obsolete( "'IVarPutSuper()' is not supported and always returns the value that is set" )] ;
FUNCTION IVarPutSuper(o AS OBJECT,symIvar AS SYMBOL,u AS USUAL,symClassName AS SYMBOL) AS USUAL
	RETURN u   
	
/// <exclude/>
[Obsolete( "'IVarGetSuper()' is not supported and always returns NIL" )] ;
FUNCTION IVarGetSuper(o AS OBJECT,symIvar AS SYMBOL,symClassName AS SYMBOL) AS USUAL
	RETURN NIL
	
/// <exclude/>
[ObsoleteAttribute( "'SendClass()' is not supported and always returns NIL")] ;
FUNCTION SendClass( o , symMethod , symClassName ) AS USUAL CLIPPER
	RETURN NIL
	
/// <exclude/>
[ObsoleteAttribute( "'OClone()' is not supported and always returns NULL_OBJECT")] ;
FUNCTION OClone(o AS OBJECT) AS OBJECT
	RETURN NULL_OBJECT   
	
/// <exclude/>
[ObsoleteAttribute( "'OMemSize()' is not supported and always returns 0")] ;
FUNCTION OMemSize(o AS OBJECT) AS DWORD
	RETURN 0  
	
/// <exclude/>
[ObsoleteAttribute( "'PClone()' is not supported and always returns NIL")];
FUNCTION PClone(x AS USUAL) AS USUAL
	RETURN NIL   
	
/// <exclude/>
[ObsoleteAttribute( "'Bin2F()' is not supported and always returns 0")];
FUNCTION Bin2F(c AS STRING) AS FLOAT
	// VO stores the 8 bytes real8 + float header = 12 bytes.
	RETURN 0   
	
/// <exclude/>
[ObsoleteAttribute( "'F2Bin()' is not supported and always returns an empty string")];
FUNCTION F2Bin(f AS FLOAT) AS STRING
	// VO stores the 8 bytes real8 + float header = 12 bytes.
	RETURN String.Empty
	
	
/// <exclude/>
[ObsoleteAttribute( "'FloatNext()' is not supported and always returns 0")];
FUNCTION FloatNext(f AS FLOAT) AS FLOAT
	RETURN 0   
	
/// <exclude/>
[ObsoleteAttribute( "'Psz2Float()' is not supported and always returns 0")];
FUNCTION Psz2Float(ptrBuff AS IntPtr,nLen AS INT,nDec AS INT) AS FLOAT
	RETURN 0   
	
/// <exclude/>
[ObsoleteAttribute( "'Any2Usual()' is not supported and always returns NIL")];
FUNCTION Any2Usual(x AS USUAL,nType AS DWORD) AS USUAL
	RETURN NIL   
	
	
/// <exclude/>
[ObsoleteAttribute( "'StrToLong()' is not supported and always returns 0")];
FUNCTION StrToLong(c AS STRING,dwRadix AS DWORD) AS FLOAT
	RETURN 0   
	
	
/// <exclude/>
[ObsoleteAttribute( "'ObjAsPsz()' is not supported and always returns NULL_PSZ")];
FUNCTION ObjAsPsz(pObj AS IntPtr) AS PSZ
	RETURN NULL_PSZ
	
	
/// <exclude/>
[ObsoleteAttribute( "'Float2Str()' is not supported and always returns an empty string")];
FUNCTION Float2Str(ptrUsual AS IntPtr,dwLen AS DWORD,dwDec AS DWORD) AS STRING
	RETURN	 ""
	
/// <exclude/>
[Obsolete];
FUNCTION FunctionCount() AS DWORD
	RETURN	 0   
	
/// <exclude/>
[Obsolete];
FUNCTION FunctionList() AS ARRAY
	RETURN NULL_ARRAY   
	
	
/// <exclude/>
[Obsolete];
FUNCTION UnDeclareClass(symClass AS SYMBOL) AS INT
	RETURN 0   
	
/// <exclude/>
[Obsolete];
FUNCTION DeclareMethod(symClass AS SYMBOL,symMeth AS SYMBOL,nType AS DWORD,pFunc AS IntPTR,nArgs AS DWORD) AS INT
	RETURN 0   


/// <exclude/>
[Obsolete];
FUNCTION FunctionSym2Ptr(symFunc AS SYMBOL) AS IntPTR
	RETURN IntPtr.Zero
	
/// <exclude/>
[Obsolete] ;
FUNCTION ASendClass( a, symMethod, symClassName ) AS ARRAY CLIPPER
	RETURN a
	
/// <exclude/>
[Obsolete] ;
FUNCTION _CallClipFuncPtr(pFunc AS IntPtr,aArgs AS ARRAY) AS USUAL
	RETURN NIL   
	
/// <exclude/>
[Obsolete] ;
FUNCTION _Call(ptrFunc AS PTR) AS USUAL
	RETURN NIL   
	
/// <exclude/>
[Obsolete] ;
FUNCTION _AsPsz(u AS USUAL) AS PSZ
	RETURN NULL_PSZ

/// <exclude/>
[Obsolete] ;
FUNCTION AsPsz(u AS __Usual) AS PSZ
	RETURN NULL_PSZ

/// <exclude/>
[Obsolete] ;
FUNCTION ErrorCount(dw AS USUAL) AS DWORD
	RETURN 0  

/// <exclude/>
[Obsolete] ;
FUNCTION ErrorFunc(ptrFunc AS USUAL) AS USUAL
	RETURN NIL   

/// <exclude/>
[Obsolete] ;
FUNCTION ErrorMessageBox(pszText AS PSZ,pszCapt AS PSZ,dwB1 AS DWORD,dwB2 AS DWORD,dwB3 AS DWORD) AS DWORD
	RETURN 0  

/// <exclude/>
[Obsolete] ;
FUNCTION IsVOObject(oObject AS OBJECT) AS LOGIC
	RETURN FALSE   

/// <exclude/>
[Obsolete] ;
FUNCTION _ArrayGetPoly(a AS USUAL,n1 AS USUAL) AS USUAL
	RETURN NIL   

/// <exclude/>
[Obsolete] ;
FUNCTION _ArrayGetCollection(a AS USUAL,n1 AS USUAL) AS USUAL
	RETURN NIL   

/// <exclude/>
[Obsolete] ;
FUNCTION _ArrayPutCollection(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
	RETURN		 NIL   

/// <exclude/>
[Obsolete] ;
FUNCTION _ArrayPutPoly(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
	RETURN NIL  

/// <exclude/>
[Obsolete] ;
FUNCTION CompString() AS INT
	RETURN 0   

/// <exclude/>
[Obsolete] ;
FUNCTION SBTODB(c AS STRING) AS STRING
	RETURN String.Empty   

/// <exclude/>
[Obsolete];
FUNCTION SEvalA(cSource AS USUAL,block AS USUAL,nStart AS USUAL,nCount AS USUAL) AS STRING
	RETURN cSource

/// <exclude/>
[ObsoleteAttribute( "'SysObject()' is not supported", TRUE )] ;
FUNCTION SysObject() AS USUAL
   RETURN NIL


/// <exclude/>
[Obsolete];
FUNCTION MCSHORT(l AS USUAL) AS LOGIC
	RETURN FALSE  


/// <exclude/>
[Obsolete];
FUNCTION MPrepare(s AS STRING) AS STRING
	RETURN s

/// <exclude/>
[Obsolete];
FUNCTION ItemArrayGet(ptrAny AS IntPtr,dwElem AS DWORD) AS USUAL
RETURN NIL   

/// <exclude/>
[Obsolete];
FUNCTION ItemArrayNew(dwSize AS DWORD) AS USUAL
RETURN NIL   

/// <exclude/>
[Obsolete];
FUNCTION ItemArrayPut(px AS IntPtr,dw AS DWORD,pa AS IntPtr) AS IntPtr
RETURN IntPtr.Zero


/// <exclude/>
[Obsolete];
FUNCTION ItemCopyC(pszVal AS PSZ,ptrAny AS IntPtr,dwLen AS DWORD) AS DWORD
RETURN 0   

/// <exclude/>
[Obsolete];
FUNCTION ItemFreeC(pszVal AS PSZ) AS LOGIC
RETURN FALSE   

/// <exclude/>
[Obsolete];
FUNCTION ItemGetC(ptrAny AS IntPtr) AS PSZ
RETURN NULL_PSZ

/// <exclude/>
[Obsolete];
FUNCTION ItemGetDS(ptrAny AS IntPtr,pszDate AS PSZ) AS PSZ
RETURN NULL_PSZ

/// <exclude/>
[Obsolete];
FUNCTION ItemGetDL(ptrAny AS IntPtr) AS LONG
RETURN 0   

/// <exclude/>
[Obsolete];
FUNCTION ItemGetL(ptrAny AS IntPtr) AS LOGIC
RETURN FALSE   

/// <exclude/>
[Obsolete];
FUNCTION ItemGetND(ptrAny AS IntPtr) AS REAL8
RETURN 0   

/// <exclude/>
[Obsolete];
FUNCTION ItemGetNL(ptrAny AS IntPtr) AS LONG
RETURN 0   

/// <exclude/>
[Obsolete];
FUNCTION ItemGetNS(ptrAny AS IntPtr,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
RETURN NULL_PSZ

/// <exclude/>
[Obsolete];
FUNCTION ItemPutC(ptrAny AS IntPtr,pszVal AS PSZ) AS IntPtr
RETURN IntPtr.Zero

/// <exclude/>
[Obsolete];
FUNCTION ItemPutCL(ptrAny AS IntPtr,pszVal AS PSZ,uilen AS DWORD) AS IntPtr
RETURN IntPtr.Zero

/// <exclude/>
[Obsolete];
FUNCTION ItemPutDS(ptrAny AS IntPtr,pszDate AS PSZ) AS IntPtr
RETURN IntPtr.Zero

/// <exclude/>
[Obsolete];
FUNCTION ItemPutNS(ptrAny AS IntPtr,pszVal AS PSZ,dwLen AS DWORD,dwDec AS DWORD) AS IntPtr
RETURN IntPtr.Zero

/// <exclude/>
[Obsolete];
FUNCTION ItemPutDL(ptrAny AS IntPtr,lDate AS LONG) AS IntPtr
RETURN IntPtr.Zero

/// <esclude />
[Obsolete];
FUNCTION ItemPutL(ptrAny AS IntPtr,fVal AS LOGIC) AS IntPtr
RETURN IntPtr.Zero
/// <esclude />
[Obsolete];
FUNCTION ItemPutND(ptrAny AS IntPtr,r8 AS REAL8) AS IntPtr
RETURN IntPtr.Zero
/// <esclude />
[Obsolete];
FUNCTION ItemPutNL(ptrAny AS IntPtr,lNum AS LONG) AS IntPtr
RETURN IntPtr.Zero
/// <esclude />
[Obsolete];
FUNCTION ItemRegister(ptrAny AS IntPtr) AS LOGIC
RETURN FALSE   
/// <esclude />
[Obsolete];
FUNCTION ItemRelease(ptrAny AS IntPtr) AS LOGIC
RETURN FALSE   

/// <esclude />
[Obsolete];
FUNCTION ItemReturn(ptrAny AS IntPtr) AS USUAL
RETURN NIL   
/// <esclude />
[Obsolete];
FUNCTION ItemSize(ptrAny AS IntPtr) AS DWORD
RETURN 0   
/// <esclude />
[Obsolete];
FUNCTION ItemType(ptrAny AS IntPtr) AS DWORD
RETURN 0   

/// <esclude />
[Obsolete( "'GetDASPtr()' is not supported and always returns IntPtr.Zero" )] ;
FUNCTION GetDASPtr() AS IntPtr
	RETURN IntPtr.Zero   
	


