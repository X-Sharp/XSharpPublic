//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Vulcan
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __AccessPtr(o AS OBJECT,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __AccessPtrClass(symClassName AS __Symbol,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __AssignPtr(o AS OBJECT,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __AssignPtrClass(symClassName AS __Symbol,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __Break() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __CollectPublics() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ConvertResult(u AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="hInst"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __FreeGuruTab(hInst AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <param name="lpdwValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION __GetAtomProperty(atom AS __Symbol,atomPropName AS __Symbol,lpdwValue REF DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __GetAxitStack() AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __GetClassPtr(symClassName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __LastMethod() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __LastMethodPtr() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __MethodPtr(o AS OBJECT,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __MethodPtrClass(symClassName AS __Symbol,symMethodName AS __Symbol) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __NotifyUnload(pMod AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="lpO"></param>
	/// <param name="lpClassPtr"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __ObjectCastClassPtr(lpO AS OBJECT,lpClassPtr AS PTR) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __RemoveAtomProperty(atom AS __Symbol,atomPropName AS __Symbol) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <param name="dwValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION __SetAtomProperty(atom AS __Symbol,atomPropName AS __Symbol,dwValue AS DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwLevel"></param>
	/// <returns>
	/// </returns>
	FUNCTION __SetClassDebug(dwLevel AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="n"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION __Str(n AS __Usual,nLen AS __Usual,nDec AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION __Str3(f AS __VOFloat,dwLen AS DWORD,dwDec AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION __UpperPsz(pszSource AS __Psz) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _AddESP(dw AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION ___ArrayGetCollection(a AS __Usual,n1 AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION ___ArrayGetPoly(a AS __Usual,n1 AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION ___ArrayPutCollection(a AS __Usual,x AS __Usual,n1 AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION ___ArrayPutPoly(a AS __Usual,x AS __Usual,n1 AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _AsPsz(u AS __Usual) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _AsString(u AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="uError"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Break(uError AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _Call(ptrFunc AS PTR) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="symFunc"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION _CallClipFunc(symFunc AS __Symbol,aArgs AS __Array) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="pFunc"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _CallClipFuncPtr(pFunc AS PTR,aArgs AS __Array) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _D2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write information to the Debug Terminal Program
	/// </summary>
	/// <param name="pszOut"></param>
	/// <returns>
	/// </returns>
	FUNCTION _DebOut32(pszOut AS __Psz) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="dwVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _Dec(dwVal AS DWORD,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <param name="b"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _DecByte(b AS BYTE,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN    

	/// <summary>
	/// </summary>
	/// <param name="wVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _DecWord(wVal AS WORD,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <param name="pObj"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _DumpKid(pObj AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DW2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Check integrity of dynamic memory.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DynCheck() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DynCheckErrorBox() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DynLocks() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DynWipe() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="ptrCode"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Exec(ptrCode AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _ExecName() AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _F2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _FieldGet(nPos AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="xValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION _FieldPut(nPos AS DWORD,xValue AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return a pointer to the command line used to invoke the application.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetCmdLine() AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ  

	/// <summary>
	/// Return a constant specifying how the application window is shown.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetCmdShow() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Access the first parameter in a variable parameter list.
	/// </summary>
	/// <param name="pptrStart"></param>
	/// <param name="ptrFirst"></param>
	/// <param name="dwTypeFirst"></param>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _GetFirstParam(pptrStart AS __Usual,ptrFirst AS PTR,dwTypeFirst AS DWORD,dwType AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GETFPARAM(dw AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return the instance handle of an application or DLL.
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _GetInst() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GETMPARAM(dw AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetMRandID() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Access the remaining parameters in a variable parameter list.
	/// </summary>
	/// <param name="pptrStart"></param>
	/// <param name="dwType"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GetNextParam(pptrStart AS __Usual,dwType AS DWORD) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Return the previous instance handle of an application or DLL.
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _GetPrevInst() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetRTCount() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetRTNum() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetSelf() AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// </summary>
	/// <param name="dwRes"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GetStringDXAX(dwRes AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetSuper__Symbol() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSM(symVar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSMF(symVar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSMM(symVar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="dwVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _Hex(dwVal AS DWORD,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="b"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _HexByte(b AS BYTE,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="wVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _HexWord(wVal AS WORD,p AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _I2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <param name="lExit"></param>
	/// <param name="pszInfo"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IError(dw AS DWORD,lExit AS LOGIC,pszInfo AS __Psz) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetCollection(o AS __Usual,symIVar AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetSelfCollection(o AS __Usual,symIVar AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetSuperCollection(o AS __Usual,symIVar AS __Usual,symClass AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutCollection(o AS __Usual,symIVar AS __Usual,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutSelfCollection(o AS __Usual,symIVar AS __Usual,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutSuperCollection(o AS __Usual,symIVar AS __Usual,u AS __Usual,symClass AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _L2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="cWild"></param>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Like(cWild AS STRING,c AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _LowCase() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _MaxAS() AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _MClear() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="nSymVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _MxRelease(nSymVar AS __Usual) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="pszBuff"></param>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _NGet(pszBuff AS __Psz,dwPos AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="cSource"></param>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _NNextI(cSource AS STRING,dwPos AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _NPrevI(dwPos AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pszBuff"></param>
	/// <param name="dwPos"></param>
	/// <param name="b"></param>
	/// <returns>
	/// </returns>
	FUNCTION _NPut(pszBuff AS __Psz,dwPos AS DWORD,b AS BYTE) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION _nScanId(c AS STRING,dwLen AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Numeric2Scientific(p AS __Psz) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ  

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _NumMacro() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _NumPCode() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PCodeTable() AS DWORD[]
		/// THROW NotImplementedException{}
	RETURN NULL 

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PGenRef() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PGenSymRef(symVar AS __Symbol) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PopI87Real8() AS REAL8
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PopTempSelect() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="nAS"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PrivateCount(nAS AS __Usual) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nAS"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PrivateFirst(nAS AS __Usual) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PrivateNext() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _PrivatePtr() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="dwActivation"></param>
	/// <returns>
	/// </returns>
	FUNCTION _ProcEBP(dwActivation AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PublicFirst() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PublicNext() AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _PublicPtr() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushDWord(dw AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="r8"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushI87Real8(r8 AS REAL8) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="nArea"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushTempArea(nArea AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="sAlias"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushTempSelect(sAlias AS __Symbol) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="uSelect"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushTempUsual(uSelect AS __Usual) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushUsual(x AS __Usual) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _QOutPtr() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _Quit() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegCollNotifyEnd(ptrFunc AS PTR,lSet AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegCollNotifyStart(ptrFunc AS PTR,lSet AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pCollFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegisterCollect(pCollFunc AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// Register an exit routine to be called when the current application ends.
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegisterExit(ptrFunc AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrFlds"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegisterFields(ptrFlds AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="pWEPFunc"></param>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _RegisterWEP(pWEPFunc AS PTR,pMod AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nNew"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RestTempSelect(nNew AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="x"></param>
	/// <param name="symFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RTNumOverErr(x AS __Usual,symFunc AS __Symbol) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="cProg"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Run(cProg AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Scientific2Numeric(p AS __Psz) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethod"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SendClassParams(o AS OBJECT,symMethod AS __Symbol,aArgs AS __Array) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethod"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SendSelf(o AS __Usual,symMethod AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetDict(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetIntl(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="cNatDll"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetNatDLL(cNatDll AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _SetQOutPtr(ptrBuff AS PTR,dwLen AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="aAlias"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetTempAlias(aAlias AS __Symbol) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nSelect"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetTempArea(nSelect AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="psz1"></param>
	/// <param name="psz2"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SOrder(psz1 AS __Psz,psz2 AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrSField"></param>
	/// <param name="dwFcnt"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _SOrderInit(ptrSField AS PTR,dwFcnt AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSM(symVar AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSMF(symVar AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSMM(symVar AS __Symbol,u AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _Stack87Pos() AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="n"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Str(n AS __Usual,nLen AS __Usual,nDec AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="psz1"></param>
	/// <param name="psz2"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Tcmp(psz1 AS __Psz,psz2 AS __Psz,dwLen AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="strucError"></param>
	/// <returns>
	/// </returns>
	FUNCTION _TypeErrorGen(strucError AS __Usual) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _UnRegisterFields() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _UpCase() AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Val(c AS STRING) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _VODBErrInfoPtr() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _VODB__Symbols() AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _VOFreeLibrary(pMod AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// To safely use a .Dll that was created in <%APP%>.
	/// </summary>
	/// <param name="cDLL"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION _VOLoadLibrary(cDLL AS STRING) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _W2T() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nSelect"></param>
	/// <returns>
	/// </returns>
	FUNCTION _xappend(nSelect AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
end namespace