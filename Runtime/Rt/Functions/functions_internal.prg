//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __AccessPtr(o AS OBJECT,symMethodName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __AccessPtrClass(symClassName AS SYMBOL,symMethodName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __AssignPtr(o AS OBJECT,symMethodName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __AssignPtrClass(symClassName AS SYMBOL,symMethodName AS SYMBOL) AS IntPtr
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
	FUNCTION __ConvertResult(u AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="hInst"></param>
	/// <returns>
	/// </returns>
	FUNCTION __FreeGuruTab(hInst AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <param name="lpdwValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION __GetAtomProperty(atom AS SYMBOL,atomPropName AS SYMBOL,lpdwValue REF DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __GetAxitStack() AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __GetClassPtr(symClassName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __LastMethod() AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION __LastMethodPtr() AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __MethodPtr(o AS OBJECT,symMethodName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="symClassName"></param>
	/// <param name="symMethodName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __MethodPtrClass(symClassName AS SYMBOL,symMethodName AS SYMBOL) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	FUNCTION __NotifyUnload(pMod AS IntPtr) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="lpO"></param>
	/// <param name="lpClassPtr"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ObjectCastClassPtr(lpO AS OBJECT,lpClassPtr AS IntPtr) AS OBJECT
		/// THROW NotImplementedException{}
	RETURN NULL_OBJECT   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <returns>
	/// </returns>
	FUNCTION __RemoveAtomProperty(atom AS SYMBOL,atomPropName AS SYMBOL) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="atom"></param>
	/// <param name="atomPropName"></param>
	/// <param name="dwValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION __SetAtomProperty(atom AS SYMBOL,atomPropName AS SYMBOL,dwValue AS DWORD) AS LONG
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
	FUNCTION __Str(n AS USUAL,nLen AS USUAL,nDec AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="f"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION __Str3(f AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION __UpperPsz(pszSource AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ   

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
	FUNCTION _ArrayGetCollection(a AS USUAL,n1 AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION _ArrayGetPoly(a AS USUAL,n1 AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION _ArrayPutCollection(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="n1"></param>
	/// <returns>
	/// </returns>
	FUNCTION _ArrayPutPoly(a AS USUAL,x AS USUAL,n1 AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _AsPsz(u AS USUAL) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ

	/// <summary>
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _AsString(u AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="uError"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Break(uError AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Call(ptrFunc AS IntPtr) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symFunc"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION _CallClipFunc(symFunc AS SYMBOL,aArgs AS ARRAY) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="pFunc"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION _CallClipFuncPtr(pFunc AS IntPtr,aArgs AS ARRAY) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _D2T() AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Write information to the Debug Terminal Program
	/// </summary>
	/// <param name="pszOut"></param>
	/// <returns>
	/// </returns>
	FUNCTION _DebOut32(pszOut AS PSZ) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="dwVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Dec(dwVal AS DWORD,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <param name="b"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _DecByte(b AS BYTE,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN    

	/// <summary>
	/// </summary>
	/// <param name="wVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _DecWord(wVal AS WORD,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <param name="pObj"></param>
	/// <returns>
	/// </returns>
	FUNCTION _DumpKid(pObj AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _DW2T() AS FLOAT
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
	FUNCTION _Exec(ptrCode AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _ExecName() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _F2T() AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _FieldGet(nPos AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="xValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION _FieldPut(nPos AS DWORD,xValue AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return a pointer to the command line used to invoke the application.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetCmdLine() AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ  

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
	FUNCTION _GetFirstParam(pptrStart AS USUAL,ptrFirst AS IntPtr,dwTypeFirst AS DWORD,dwType AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GETFPARAM(dw AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the instance handle of an application or DLL.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetInst() AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GETMPARAM(dw AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

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
	FUNCTION _GetNextParam(pptrStart AS USUAL,dwType AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Return the previous instance handle of an application or DLL.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetPrevInst() AS IntPtr
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
	FUNCTION _GetStringDXAX(dwRes AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _GetSuperSymbol() AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSM(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSMF(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _GSMM(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="dwVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Hex(dwVal AS DWORD,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="b"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _HexByte(b AS BYTE,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="wVal"></param>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION _HexWord(wVal AS WORD,p AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _I2T() AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dw"></param>
	/// <param name="lExit"></param>
	/// <param name="pszInfo"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IError(dw AS DWORD,lExit AS LOGIC,pszInfo AS PSZ) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetCollection(o AS USUAL,symIVar AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetSelfCollection(o AS USUAL,symIVar AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarGetSuperCollection(o AS USUAL,symIVar AS USUAL,symClass AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutCollection(o AS USUAL,symIVar AS USUAL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutSelfCollection(o AS USUAL,symIVar AS USUAL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symIVar"></param>
	/// <param name="u"></param>
	/// <param name="symClass"></param>
	/// <returns>
	/// </returns>
	FUNCTION _IVarPutSuperCollection(o AS USUAL,symIVar AS USUAL,u AS USUAL,symClass AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _L2T() AS FLOAT
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
	FUNCTION _MxRelease(nSymVar AS USUAL) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="pszBuff"></param>
	/// <param name="dwPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION _NGet(pszBuff AS PSZ,dwPos AS DWORD) AS DWORD
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
	FUNCTION _NPut(pszBuff AS PSZ,dwPos AS DWORD,b AS BYTE) AS VOID
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
	FUNCTION _Numeric2Scientific(p AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ  

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
	FUNCTION _PGenSymRef(symVar AS SYMBOL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

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
	FUNCTION _PrivateCount(nAS AS USUAL) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nAS"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PrivateFirst(nAS AS USUAL) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PrivateNext() AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PrivatePtr() AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="dwActivation"></param>
	/// <returns>
	/// </returns>
	FUNCTION _ProcEBP(dwActivation AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PublicFirst() AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PublicNext() AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _PublicPtr() AS IntPtr
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
	FUNCTION _PushTempSelect(sAlias AS SYMBOL) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="uSelect"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushTempUsual(uSelect AS USUAL) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION _PushUsual(x AS USUAL) AS VOID
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
	FUNCTION _RegCollNotifyEnd(ptrFunc AS IntPtr,lSet AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RegCollNotifyStart(ptrFunc AS IntPtr,lSet AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pCollFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RegisterCollect(pCollFunc AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// Register an exit routine to be called when the current application ends.
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RegisterExit(ptrFunc AS IntPtr) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrFlds"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RegisterFields(ptrFlds AS IntPtr) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="pWEPFunc"></param>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	FUNCTION _RegisterWEP(pWEPFunc AS IntPtr,pMod AS IntPtr) AS LOGIC
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
	FUNCTION _RTNumOverErr(x AS USUAL,symFunc AS SYMBOL) AS VOID
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
	FUNCTION _Scientific2Numeric(p AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL_PSZ   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethod"></param>
	/// <param name="aArgs"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SendClassParams(o AS OBJECT,symMethod AS SYMBOL,aArgs AS ARRAY) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="o"></param>
	/// <param name="symMethod"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SendSelf(o AS USUAL,symMethod AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetDict(lSet AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetIntl(lSet AS USUAL) AS LOGIC
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
	FUNCTION _SetQOutPtr(ptrBuff AS IntPtr,dwLen AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="aAlias"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SetTempAlias(aAlias AS SYMBOL) AS DWORD
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
	FUNCTION _SOrder(psz1 AS PSZ,psz2 AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrSField"></param>
	/// <param name="dwFcnt"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SOrderInit(ptrSField AS IntPtr,dwFcnt AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSM(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSMF(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="symVar"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION _SSMM(symVar AS SYMBOL,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

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
	FUNCTION _Str(n AS USUAL,nLen AS USUAL,nDec AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="psz1"></param>
	/// <param name="psz2"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION _Tcmp(psz1 AS PSZ,psz2 AS PSZ,dwLen AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="strucError"></param>
	/// <returns>
	/// </returns>
	FUNCTION _TypeErrorGen(strucError AS USUAL) AS VOID
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
	FUNCTION _Val(c AS STRING) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _VODBErrInfoPtr() AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _VODBSymbols() AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <param name="pMod"></param>
	/// <returns>
	/// </returns>
	FUNCTION _VOFreeLibrary(pMod AS IntPtr) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// To safely use a .Dll that was created in <%APP%>.
	/// </summary>
	/// <param name="cDLL"></param>
	/// <returns>
	/// </returns>
	FUNCTION _VOLoadLibrary(cDLL AS STRING) AS IntPtr
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION _W2T() AS FLOAT
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