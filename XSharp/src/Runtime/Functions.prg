
USING System
BEGIN NAMESPACE XSharp.Runtime

        /// <summary>
        ///
        /// </summary>
        /// <param name="aRdds" ></ param > 
        /// <returns></returns>
        FUNCTION __AllocRddList(aRdds AS Array) AS _RDDLIST
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="oBlock" ></ param > 
        /// <returns></returns>
        FUNCTION __CanEval(oBlock AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="resid" ></ param > 
        /// <returns></returns>
        FUNCTION __CavoStr(resid AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aStruct" ></ param > 
        /// <param name="aNames" ></ param > 
        /// <param name="aMatch" ></ param > 
        /// <returns></returns>
        FUNCTION __DBFLEDIT(aStruct AS Array, aNames AS Array, aMatch AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="fieldName" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldGet(fieldName AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="fieldpos" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldGetNum(fieldpos AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="alias" ></ param > 
        /// <param name="fieldName" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldGetWa(alias AS string, fieldName AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="workarea" ></ param > 
        /// <param name="fieldpos" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldGetWaNum(workarea AS DWord, fieldpos AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="fieldName" ></ param > 
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldSet(fieldName AS string, value AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="fieldpos" ></ param > 
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldSetNum(fieldpos AS DWord, value AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="alias" ></ param > 
        /// <param name="fieldName" ></ param > 
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldSetWa(alias AS string, fieldName AS string, value AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="workarea" ></ param > 
        /// <param name="fieldpos" ></ param > 
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION __FieldSetWaNum(workarea AS DWord, fieldpos AS DWord, value AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="argnum" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION __GetFParam__(argnum AS DWord, args AS Usual[]) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION __InternalSend(oObject AS Usual, cName AS string,  args AS Usual[]) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="len" ></ param > 
        /// <returns></returns>
        FUNCTION __Mem2StringNoEncoding(p AS Psz, len AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="entity" ></ param > 
        /// <param name="line" ></ param > 
        /// <returns></returns>
        FUNCTION __pause(entity AS string, line AS Long) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION __popWorkarea() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="alias" ></ param > 
        /// <returns></returns>
        FUNCTION __pushWorkarea(alias AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="xDriver" ></ param > 
        /// <returns></returns>
        FUNCTION __RddList(xDriver AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="xDriver" ></ param > 
        /// <param name="aHidden" ></ param > 
        /// <returns></returns>
        FUNCTION __RddList(xDriver AS Usual, aHidden AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cmd" ></ param > 
        /// <returns></returns>
        FUNCTION __run(cmd AS string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <returns></returns>
        FUNCTION __Str(nNumber AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION __Str(nNumber AS Usual, nLength AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION __Str(nNumber AS Usual, nLength AS Usual, nDecimals AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION __Str3(nNumber AS Float, nLength AS DWord, nDecimals AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION __String2MemNoEncoding(s AS string) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <param name="r" ></ param > 
        /// <returns></returns>
        FUNCTION __StringCompare(l AS string, r AS string) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <param name="r" ></ param > 
        /// <returns></returns>
        FUNCTION __StringEquals(l AS string, r AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <param name="r" ></ param > 
        /// <returns></returns>
        FUNCTION __StringNotEquals(l AS string, r AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAlias" ></ param > 
        /// <param name="aNames" ></ param > 
        /// <param name="pJoinList" ></ param > 
        /// <returns></returns>
        FUNCTION __TargetFields(cAlias AS string, aNames AS Array, pJoinList REF _JOINLIST) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDbfName" ></ param > 
        /// <returns></returns>
        FUNCTION __UniqueAlias(cDbfName AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _accept() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="prompt" ></ param > 
        /// <returns></returns>
        FUNCTION _accept(prompt AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aStru" ></ param > 
        /// <returns></returns>
        FUNCTION _allocFieldNames(aStru AS Array) AS _FIELDNAMES
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION _AsString(u AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="expr" ></ param > 
        /// <returns></returns>
        FUNCTION _Break(expr AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="funcname" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION _CallClipFunc(funcname AS string, args AS Array) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile1" ></ param > 
        /// <param name=" cFile2" ></ param > 
        /// <param name=" cDriver" ></ param > 
        /// <param name=" lNew" ></ param > 
        /// <param name=" cAlias" ></ param > 
        /// <returns></returns>
        FUNCTION _DBCreate(cFile1, cFile2, cDriver, lNew, cAlias) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION _DebOut32(s AS string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION _DebOut32(p AS Psz) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION _DebOut32(u AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _DynCheck() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _ExecName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="fldNames" ></ param > 
        /// <returns></returns>
        FUNCTION _freeFieldNames(fldNames AS _FIELDNAMES) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _GetCmdLine() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _GetCmdShow() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pptrStart" ></ param > 
        /// <param name="ptrFirst" ></ param > 
        /// <param name="dwTypeFirst" ></ param > 
        /// <param name="dwType" ></ param > 
        /// <returns></returns>
        FUNCTION _GetFirstParam(pptrStart REF void PTR, ptrFirst AS void PTR, dwTypeFirst AS DWord, dwType AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pptrStart" ></ param > 
        /// <param name="dwType" ></ param > 
        /// <returns></returns>
        FUNCTION _GetNextParam(pptrStart REF void PTR, dwType AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _getPrevInst() AS System.IntPtr
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="resid" ></ param > 
        /// <returns></returns>
        FUNCTION _GetStringDXAX(resid AS VOStringResources) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="dwOffset" ></ param > 
        /// <returns></returns>
        FUNCTION _NGet(p AS Psz, dwOffset AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="dwOffset" ></ param > 
        /// <param name="b" ></ param > 
        /// <returns></returns>
        FUNCTION _NPut(p AS Psz, dwOffset AS DWord, b AS Byte) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _Quit() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrFunc" ></ param > 
        /// <returns></returns>
        FUNCTION _RegisterExit(ptrFunc AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION _SendClassParams(o AS Object, cName AS string, args AS Array) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _SetDict() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNewValue" ></ param > 
        /// <returns></returns>
        FUNCTION _SetDict(lNewValue AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _SetIntl() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION _SetIntl(lNewSetting AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _VODBErrInfoPtr() AS ErrorInfo
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="hModule" ></ param > 
        /// <returns></returns>
        FUNCTION _VOFreeLibrary(hModule AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="libPath" ></ param > 
        /// <returns></returns>
        FUNCTION _VOLoadLibrary(libPath AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION _wait() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="prompt" ></ param > 
        /// <returns></returns>
        FUNCTION _wait(prompt AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION AAdd(a AS Array, u AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION Abs(u AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="f" ></ param > 
        /// <returns></returns>
        FUNCTION AbsFloat(f AS Float) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION AbsInt(n AS Long) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION AbsLong(n AS Long) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION AbsReal4(n AS real4) AS real4
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION AbsReal8(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION AbsShort(n AS Short) AS Short
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION AClone(a AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION ACloneShallow(a AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uSource" ></ param > 
        /// <param name=" uTarget" ></ param > 
        /// <param name=" uStart" ></ param > 
        /// <param name=" uCount" ></ param > 
        /// <param name=" uTargetPos" ></ param > 
        /// <returns></returns>
        FUNCTION ACopy(uSource, uTarget, uStart, uCount, uTargetPos) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION ACot(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nPos" ></ param > 
        /// <returns></returns>
        FUNCTION ADel(a AS Array, nPos AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION AEval(aArray AS Array, cbBlock AS Codeblock) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION AEval(aArray AS Array, cbBlock AS Codeblock, nStart AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEval(aArray AS Array, cbBlock AS Codeblock, nStart AS Long, nCount AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEval(aArray AS Array, cbBlock AS Codeblock, nStart AS Usual, nCount AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalA(aArray AS Array, cbBlock AS Codeblock) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalA(aArray AS Array, cbBlock AS Codeblock, nStart AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalA(aArray AS Array, cbBlock AS Codeblock, nStart AS Long, nCount AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalA(aArray AS Array, cbBlock AS Codeblock, nStart AS Usual, nCount AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalOld(aArray AS Array, cbBlock AS Codeblock) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalOld(aArray AS Array, cbBlock AS Codeblock, nStart AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalOld(aArray AS Array, cbBlock AS Codeblock, nStart AS Long, nCount AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AEvalOld(aArray AS Array, cbBlock AS Codeblock, nStart AS Usual, nCount AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aNames" ></ param > 
        /// <param name=" aTypes" ></ param > 
        /// <param name=" aLens" ></ param > 
        /// <param name=" aDecs" ></ param > 
        /// <returns></returns>
        FUNCTION AFields(aNames, aTypes, aLens, aDecs) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name=" fill" ></ param > 
        /// <param name=" start" ></ param > 
        /// <param name=" stop" ></ param > 
        /// <returns></returns>
        FUNCTION AFill(a, fill, start, stop) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="seekVal" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <param name="nEnd" ></ param > 
        /// <returns></returns>
        FUNCTION AFind(a AS Array, seekVal AS Usual, nStart AS DWord, nCount AS DWord, nEnd AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nPos" ></ param > 
        /// <returns></returns>
        FUNCTION AIns(a AS Array, nPos AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aTarget" ></ param > 
        /// <returns></returns>
        FUNCTION ALen(aTarget AS Array) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSelect" ></ param > 
        /// <returns></returns>
        FUNCTION Alias(nSelect) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Alias0() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Alias0Sym() AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION AllTrim(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION AmPm(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Ansi2Oem(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Ansi2OemA(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszDest" ></ param > 
        /// <param name="pszSource" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION Ansi2OemBuff(pszDest AS Psz, pszSource AS Psz, dwCount AS DWord) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION AReplicate(u AS Usual, nSize AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ArrayBuild() AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayCreate(nSize AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayDeprotect(a AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayGet(a AS Array, nIndex AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwElement" ></ param > 
        /// <param name="aDim" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayInit(dwElement AS DWord, aDim REF Usual[]) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="_args" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayNew( _args AS Usual[]) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayProtect(a AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION ArrayPut(a AS Array, nIndex AS DWord, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <param name="uNewValue" ></ param > 
        /// <returns></returns>
        FUNCTION ArraySwap(a AS Array, nIndex AS DWord, uNewValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <returns></returns>
        FUNCTION Asc(cString AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <returns></returns>
        FUNCTION AscA(cString AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name=" x" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AScan(a, x, nStart, nCount) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="seekVal" ></ param > 
        /// <returns></returns>
        FUNCTION AScanBin(a AS Array, seekVal AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="seekVal" ></ param > 
        /// <returns></returns>
        FUNCTION AScanBinExact(a AS Array, seekVal AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name=" x" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nCount" ></ param > 
        /// <returns></returns>
        FUNCTION AScanExact(a, x, nStart, nCount) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION AscW(c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION ASend(a AS Array, cName AS string,  args AS Usual[]) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name=" symMethod" ></ param > 
        /// <param name=" symClassName" ></ param > 
        /// <returns></returns>
        FUNCTION ASendClass(a, symMethod, symClassName) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION AsHexString(u AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION ASize(a AS Array, nSize AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aArray" ></ param > 
        /// <param name=" startIndex" ></ param > 
        /// <param name=" nCount" ></ param > 
        /// <param name=" cbOrder" ></ param > 
        /// <returns></returns>
        FUNCTION ASort(aArray, startIndex, nCount, cbOrder) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION AsPsz(uValue AS Usual) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION AsString(u AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION AsSymbol(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION AsSymbol(p AS Psz) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION AsSymbol(s AS Symbol) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION AsSymbol(uValue AS Usual) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION At(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION At2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="nOffset" ></ param > 
        /// <returns></returns>
        FUNCTION At3(cSearch AS string, cTarget AS string, nOffset AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION ATail(a AS Array) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION ATan(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtC(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtC2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtCLine(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtCLine2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtLine(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION AtLine2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="aLookupTable" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION ATranslate(aLookupTable AS Array, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aLookupTable" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <param name="lSoftSeek" ></ param > 
        /// <returns></returns>
        FUNCTION ATranslate(aLookupTable AS Array, uValue AS Usual, lSoftSeek AS Logic) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="Value" ></ param > 
        /// <param name="Min" ></ param > 
        /// <param name="Max" ></ param > 
        /// <returns></returns>
        FUNCTION Between(Value AS Usual, Min AS Usual, Max AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2Date(c AS string) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2DW(c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2F(c AS string) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2I(c AS string) AS Short
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2L(c AS string) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2Logic(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2Ptr(c AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2Real4(c AS string) AS real4
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2Real8(c AS string) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Bin2W(c AS string) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nPointer" ></ param > 
        /// <param name=" cTargetFile" ></ param > 
        /// <param name=" nMode" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBDirectExport(nPointer, cTargetFile, nMode) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nPointer" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nCount" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBDirectGet(nPointer, nStart, nCount) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOldPointer" ></ param > 
        /// <param name=" cSourceFile" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBDirectImport(nOldPointer, cSourceFile) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOldPointer" ></ param > 
        /// <param name=" uBLOB" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBDirectPut(nOldPointer, uBLOB) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldPos" ></ param > 
        /// <param name=" cFileName" ></ param > 
        /// <param name=" nMode" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBExport(nFieldPos, cFileName, nMode) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldNo" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nLen" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBGet(nFieldNo, nStart, nLen) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldPos" ></ param > 
        /// <param name=" cFileName" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBImport(nFieldPos, cFileName) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION BLOBRootGet() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION BLOBRootLock() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="xblob" ></ param > 
        /// <returns></returns>
        FUNCTION BLOBRootPut(xblob) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION BLOBRootUnlock() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION BOF() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Buffer(n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION CanBreak() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Cast2Psz(s AS string) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION CDoW(d AS Date) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nValue" ></ param > 
        /// <returns></returns>
        FUNCTION Ceil(nValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION CharEven(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOdd" ></ param > 
        /// <param name="cEven" ></ param > 
        /// <returns></returns>
        FUNCTION CharMix(cOdd AS string, cEven AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION CharOdd(c AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION CharPos(c AS string, n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Chr(c AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION ChrA(c AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION ChrW(c AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ClassCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ClassList() AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION ClassName(o AS Object) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION ClassTree(o AS Object) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cClassName" ></ param > 
        /// <returns></returns>
        FUNCTION ClassTreeClass(cClassName AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION cls() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION CMonth(d AS Date) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Col() AS Short
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Collect() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION CollectCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION CollectForced() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s1" ></ param > 
        /// <param name="s2" ></ param > 
        /// <returns></returns>
        FUNCTION ConcatAtom(s1 AS Symbol, s2 AS Symbol) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s1" ></ param > 
        /// <param name="s2" ></ param > 
        /// <param name="s3" ></ param > 
        /// <returns></returns>
        FUNCTION ConcatAtom3(s1 AS Symbol, s2 AS Symbol, s3 AS Symbol) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nYear" ></ param > 
        /// <param name="nMonth" ></ param > 
        /// <param name="nDay" ></ param > 
        /// <returns></returns>
        FUNCTION ConDate(nYear AS DWord, nMonth AS DWord, nDay AS DWord) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nHour" ></ param > 
        /// <param name="nMinute" ></ param > 
        /// <param name="nSecond" ></ param > 
        /// <returns></returns>
        FUNCTION ConTime(nHour AS DWord, nMinute AS DWord, nSecond AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Cos(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Cot(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION CParamCount(cbBlock AS Codeblock) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pDump" ></ param > 
        /// <param name="nValType" ></ param > 
        /// <returns></returns>
        FUNCTION CreateGCDump(pDump AS void PTR, nValType AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cClassName" ></ param > 
        /// <returns></returns>
        FUNCTION CreateInstance(cClassName) AS Object
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pSecAttr" ></ param > 
        /// <param name="nStackSize" ></ param > 
        /// <param name="pFunc" ></ param > 
        /// <param name="pParam" ></ param > 
        /// <param name="dwFlags" ></ param > 
        /// <param name="pdwID" ></ param > 
        /// <returns></returns>
        FUNCTION CreateVOThread(pSecAttr AS void PTR, nStackSize AS DWord, pFunc AS void PTR, pParam AS void PTR, dwFlags AS DWord, pdwID AS DWord PTR) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sSource" ></ param > 
        /// <param name="sKey" ></ param > 
        /// <returns></returns>
        FUNCTION Crypt(sSource AS string, sKey AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSource" ></ param > 
        /// <param name="cKey" ></ param > 
        /// <param name="uiSourceLen" ></ param > 
        /// <param name="uiKeyLen" ></ param > 
        /// <returns></returns>
        FUNCTION Crypt4(cSource AS Byte PTR, cKey AS Byte PTR, uiSourceLen AS DWord, uiKeyLen AS DWord) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSource" ></ param > 
        /// <param name="cKey" ></ param > 
        /// <returns></returns>
        FUNCTION CryptA(cSource REF string, cKey AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDate" ></ param > 
        /// <returns></returns>
        FUNCTION CToD(cDate AS string) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDate" ></ param > 
        /// <param name="cDateFormat" ></ param > 
        /// <returns></returns>
        FUNCTION CToD(cDate AS string, cDateFormat AS string) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDate" ></ param > 
        /// <returns></returns>
        FUNCTION CToDAnsi(cDate AS string) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION CurDir() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDisk" ></ param > 
        /// <returns></returns>
        FUNCTION CurDir(cDisk AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION CurDrive() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION Date2Bin(d AS Date) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION Day(d AS Date) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Days(n AS real8) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <param name=" cDriver" ></ param > 
        /// <param name=" acRDDs" ></ param > 
        /// <returns></returns>
        FUNCTION DBApp(cFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest, cDriver, acRDDs) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" cDelim" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBAppDelim(cFile, cDelim, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBAppend() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lReleaseLocks" ></ param > 
        /// <returns></returns>
        FUNCTION DBAppend(lReleaseLocks AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBAppSDF(cFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" nPos" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION DBBlobInfo(nOrdinal, nPos, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBClearFilter() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION DBClearIndex(uOrder, cOrdBag) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBClearOrderCondition() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBClearRelation() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBCloseAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBCloseArea() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBCommit() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBCommitAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBContinue() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <param name=" cDriver" ></ param > 
        /// <param name=" acRDDs" ></ param > 
        /// <returns></returns>
        FUNCTION DBCopy(cFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest, cDriver, acRDDs) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" cDelim" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBCopyDelim(cFile, cDelim, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" acFields" ></ param > 
        /// <param name=" cbForCondition" ></ param > 
        /// <param name=" cbWhileCondition" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecord" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBCopySDF(cFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="aFields" ></ param > 
        /// <returns></returns>
        FUNCTION DBCopyStruct(cFile AS string, aFields AS Array) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION DBCopyXStruct(cFile AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name=" aStru" ></ param > 
        /// <param name=" cDriver" ></ param > 
        /// <param name=" lNew" ></ param > 
        /// <param name=" cAlias" ></ param > 
        /// <param name=" cDelim" ></ param > 
        /// <param name=" lJustOpen" ></ param > 
        /// <param name=" aHidden" ></ param > 
        /// <returns></returns>
        FUNCTION DBCreate(cName, aStru, cDriver, lNew, cAlias, cDelim, lJustOpen, aHidden) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name=" cExpr" ></ param > 
        /// <param name=" cobExpr" ></ param > 
        /// <param name=" lUnique" ></ param > 
        /// <returns></returns>
        FUNCTION DBCreateIndex(cName, cExpr, cobExpr, lUnique) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cName" ></ param > 
        /// <param name=" cExpr" ></ param > 
        /// <param name=" cobExpr" ></ param > 
        /// <param name=" lUnique" ></ param > 
        /// <returns></returns>
        FUNCTION DBCreateOrder(uOrder, cName, cExpr, cobExpr, lUnique) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBDelete() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION DBDeleteOrder(uOrder, cOrdBag) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBDriver() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uBlock" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecNo" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBEval(uBlock, uCobFor, uCobWhile, nNext, nRecNo, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBF() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" nPos" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION DBFieldInfo(nOrdinal, nPos, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="hfFrom" ></ param > 
        /// <param name=" cFile" ></ param > 
        /// <param name=" cFullPath" ></ param > 
        /// <returns></returns>
        FUNCTION DBFileCopy(hfFrom, cFile, cFullPath) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBFilter() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBGetSelect() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBGoBottom() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecId" ></ param > 
        /// <returns></returns>
        FUNCTION DBGoTo(uRecId AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBGoTop() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION DBInfo(nOrdinal, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAlias" ></ param > 
        /// <param name=" cFile" ></ param > 
        /// <param name=" aFields" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <returns></returns>
        FUNCTION DBJoin(cAlias, cFile, aFields, uCobFor) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSelect" ></ param > 
        /// <param name="struList" ></ param > 
        /// <returns></returns>
        FUNCTION DBJoinAppend(nSelect AS DWord, struList AS _JOINLIST) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uCobFor" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" uRecId" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBLocate(uCobFor, uCobWhile, nNext, uRecId, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xField" ></ param > 
        /// <returns></returns>
        FUNCTION DBMemoField(xField) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" cBagName" ></ param > 
        /// <param name=" uOrder" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION DBOrderInfo(nOrdinal, cBagName, uOrder, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBPack() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBRecall() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" uRecID" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION DBRecordInfo(nOrdinal, uRecID, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBReindex() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRelation" ></ param > 
        /// <returns></returns>
        FUNCTION DBRelation(nRelation) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecord" ></ param > 
        /// <returns></returns>
        FUNCTION DBRLock(uRecord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBRLockList() AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRelation" ></ param > 
        /// <returns></returns>
        FUNCTION DBRSelect(nRelation) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecID" ></ param > 
        /// <returns></returns>
        FUNCTION DBRUnlock(uRecID) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xValue" ></ param > 
        /// <param name=" lSoft" ></ param > 
        /// <param name=" lLast" ></ param > 
        /// <returns></returns>
        FUNCTION DBSeek(xValue, lSoft, lLast) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNew" ></ param > 
        /// <returns></returns>
        FUNCTION DBSelect(nNew) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="xValue" ></ param > 
        /// <returns></returns>
        FUNCTION DBSelectArea(xValue) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDriver" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetDriver(cDriver) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cbFilter" ></ param > 
        /// <param name=" cFilter" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetFilter(cbFilter, cFilter) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lFnd" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetFound(lFnd) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cIndex" ></ param > 
        /// <param name=" uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetIndex(cIndex, uOrder) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cBagname" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetOrder(uOrder, cBagname) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFor" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" lAll" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" uCobEval" ></ param > 
        /// <param name=" nStep" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecno" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <param name=" lDescending" ></ param > 
        /// <param name=" lAdditive" ></ param > 
        /// <param name=" lCurrent" ></ param > 
        /// <param name=" lCustom" ></ param > 
        /// <param name=" lNoOptimize" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetOrderCondition(cFor, uCobFor, lAll, uCobWhile, uCobEval, nStep, nStart, nNext, nRecno, lRest, lDescending, lAdditive, lCurrent, lCustom, lNoOptimize) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xAlias" ></ param > 
        /// <param name=" uCobKey" ></ param > 
        /// <param name=" cKey" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetRelation(xAlias, uCobKey, cKey) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBSetSelect() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSelect" ></ param > 
        /// <returns></returns>
        FUNCTION DBSetSelect(nSelect AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRecords" ></ param > 
        /// <returns></returns>
        FUNCTION DBSkip(nRecords) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" aFields" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRec" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBSort(cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBStruct() AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sAlias" ></ param > 
        /// <returns></returns>
        FUNCTION DBSymSelect(sAlias) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION DBToSB(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name=" bKey" ></ param > 
        /// <param name=" aFields" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRec" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <param name=" xDriver" ></ param > 
        /// <returns></returns>
        FUNCTION DBTotal(cFile, bKey, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, xDriver) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nTo" ></ param > 
        /// <param name=" aStru" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecNo" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <returns></returns>
        FUNCTION DBTrans(nTo, aStru, uCobFor, uCobWhile, nNext, nRecNo, lRest) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBUnlock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBUnlockAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAlias" ></ param > 
        /// <param name=" uCobKey" ></ param > 
        /// <param name=" lRand" ></ param > 
        /// <param name=" bReplace" ></ param > 
        /// <returns></returns>
        FUNCTION DBUpdate(cAlias, uCobKey, lRand, bReplace) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNew" ></ param > 
        /// <param name=" xDriver" ></ param > 
        /// <param name=" cName" ></ param > 
        /// <param name=" cAlias" ></ param > 
        /// <param name=" lShare" ></ param > 
        /// <param name=" lReadOnly" ></ param > 
        /// <param name=" aStru" ></ param > 
        /// <param name=" cDelim" ></ param > 
        /// <param name=" aHidden" ></ param > 
        /// <returns></returns>
        FUNCTION DBUseArea(lNew, xDriver, cName, cAlias, lShare, lReadOnly, aStru, cDelim, aHidden) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DBZap() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="_args" ></ param > 
        /// <returns></returns>
        FUNCTION DebOut( _args AS Usual[]) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION DebOut32(s AS string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION DebOut32(p AS Psz) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION DebOut32(u AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAttachment" ></ param > 
        /// <param name="hOutput" ></ param > 
        /// <returns></returns>
        FUNCTION DecodeBase64(cAttachment AS string, hOutput AS void PTR) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="uVar" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION Default(uVar REF Usual, uValue AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Deleted() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSubKey" ></ param > 
        /// <returns></returns>
        FUNCTION DeleteRTRegKey(cSubKey AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION Descend(uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION DirChange(c AS string) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <returns></returns>
        FUNCTION Directory(cFileSpec AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <param name="cAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION Directory(cFileSpec AS string, cAttributes AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION Directory(cFileSpec AS string, dwAttributes AS DWord) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <param name="uAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION Directory(cFileSpec AS string, uAttributes AS Usual) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION DirMake(c AS string) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION DirRemove(c AS string) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION DiskChange(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DiskFree() AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDrive" ></ param > 
        /// <returns></returns>
        FUNCTION DiskFree(cDrive AS string) AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nDrive" ></ param > 
        /// <returns></returns>
        FUNCTION DiskFree(nDrive AS DWord) AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DiskName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DiskSpace() AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDrive" ></ param > 
        /// <returns></returns>
        FUNCTION DiskSpace(cDrive AS string) AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nDrive" ></ param > 
        /// <returns></returns>
        FUNCTION DiskSpace(nDrive AS DWord) AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSymFunc" ></ param > 
        /// <returns></returns>
        FUNCTION DoError(nSymFunc AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSymFunc" ></ param > 
        /// <param name="nTries" ></ param > 
        /// <returns></returns>
        FUNCTION DoError(nSymFunc AS string, nTries AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DoEvents() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DOSError() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNewDosCode" ></ param > 
        /// <returns></returns>
        FUNCTION DOSError(nNewDosCode AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="strid" ></ param > 
        /// <returns></returns>
        FUNCTION DosErrString(strid AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION DoW(d AS Date) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION DToC(d AS Date) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION DToS(d AS Date) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION DW2Bin(n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynCheckError() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynInfoFree() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynInfoMax() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynInfoSize() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynInfoUsed() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynLock() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lSet" ></ param > 
        /// <returns></returns>
        FUNCTION DynProtect(lSet AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynShrink() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynSize() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION DynToOldSpace(u AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION DynToOldSpaceArray(a AS Array) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="f" ></ param > 
        /// <returns></returns>
        FUNCTION DynToOldSpaceFloat(f AS Float) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION DynToOldSpaceObject(o AS Object) AS Object
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION DynToOldSpaceString(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION DynUnLock() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cStart" ></ param > 
        /// <param name="cEnd" ></ param > 
        /// <returns></returns>
        FUNCTION ElapTime(cStart AS string, cEnd AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="x" ></ param > 
        /// <returns></returns>
        FUNCTION Empty(x AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION EmptyField(n AS Long) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION EmptyPSZ(p AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION EmptyRecord() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="@@type" ></ param > 
        /// <returns></returns>
        FUNCTION EmptyUsual(@@type AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lEnable" ></ param > 
        /// <returns></returns>
        FUNCTION EnableLBOptimizations(lEnable AS Logic) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="hInput" ></ param > 
        /// <param name="hOutput" ></ param > 
        /// <returns></returns>
        FUNCTION EncodeBase64(hInput AS void PTR, hOutput AS void PTR) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="refu" ></ param > 
        /// <returns></returns>
        FUNCTION EnforceNumeric(refu REF Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="refu" ></ param > 
        /// <param name="wType" ></ param > 
        /// <returns></returns>
        FUNCTION EnforceType(refu REF Usual, wType AS DWord) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION EOF() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ErrorBlock() AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cbNew" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorBlock(cbNew AS Codeblock) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="errinfo" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorBuild(errinfo AS ErrorInfo) AS Error
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="w" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorCount(w) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="e" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorDialog(e AS System.Exception) AS System.Windows.Forms.DialogResult
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="txt" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorDialog(txt AS string) AS System.Windows.Forms.DialogResult
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrError" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorExec(ptrError AS ErrorInfo) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrErrInfo" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorExec(ptrErrInfo AS void PTR) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrErrInfo" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorFunc(ptrErrInfo AS void PTR) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ErrorGen() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ErrorLevel() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION ErrorLevel(dwNewSetting AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ErrorNew() AS Error
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uiMsg" ></ param > 
        /// <returns></returns>
        FUNCTION ErrString(uiMsg AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="obj" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION Eval(obj AS Object,  args AS Usual[]) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uCodeBlock" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION Eval(uCodeBlock AS Usual,  args AS Usual[]) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cbCodeBlock" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION Eval(cbCodeBlock AS Codeblock,  args AS Usual[]) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="macroExpression" ></ param > 
        /// <returns></returns>
        FUNCTION Evaluate(macroExpression AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="macroExpression" ></ param > 
        /// <param name="voCompatibleSyntax" ></ param > 
        /// <returns></returns>
        FUNCTION Evaluate(macroExpression AS string, voCompatibleSyntax AS Logic) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lFull" ></ param > 
        /// <returns></returns>
        FUNCTION ExecName(lFull AS Logic) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nExitCode" ></ param > 
        /// <returns></returns>
        FUNCTION ExitVOThread(nExitCode AS Long) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Exp(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION F2Bin(n AS Float) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Fact(n AS DWord) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FAttr2String(dwAttributes AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FAttrib() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="uiOffset" ></ param > 
        /// <returns></returns>
        FUNCTION FChSize(pHandle AS void PTR, uiOffset AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="f" ></ param > 
        /// <returns></returns>
        FUNCTION FClone(f AS Float) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION FClose(p AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FCommit(pHandle AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSource" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION FCopy(cSource AS string, cTarget AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSource" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="lOverWrite" ></ param > 
        /// <returns></returns>
        FUNCTION FCopy(cSource AS string, cTarget AS string, lOverWrite AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION FCreate(cFile AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FCreate(cFile AS string, dwAttributes AS Long) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FCreate(cFile AS string, dwAttributes AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="uAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FCreate(cFile AS string, uAttributes AS Usual) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FCreate2(cFile AS string, dwAttributes AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FDate() AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FEof(pHandle AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION FErase(cFile AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FError() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION FError(n AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="sFile" ></ param > 
        /// <param name="wAttr" ></ param > 
        /// <returns></returns>
        FUNCTION FFCount(sFile AS string, wAttr AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszFile" ></ param > 
        /// <param name="wAttr" ></ param > 
        /// <returns></returns>
        FUNCTION FFCount(pszFile AS Psz, wAttr AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="sFile" ></ param > 
        /// <param name="wAttr" ></ param > 
        /// <returns></returns>
        FUNCTION FFirst(sFile AS string, wAttr AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszFile" ></ param > 
        /// <param name="wAttr" ></ param > 
        /// <returns></returns>
        FUNCTION FFirst(pszFile AS Psz, wAttr AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="lOffset" ></ param > 
        /// <param name="dwLength" ></ param > 
        /// <returns></returns>
        FUNCTION FFLock(pHandle AS void PTR, lOffset AS Long, dwLength AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FFlush(pHandle AS void PTR) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="lOffset" ></ param > 
        /// <param name="dwLength" ></ param > 
        /// <returns></returns>
        FUNCTION FFUnlock(pHandle AS void PTR, lOffset AS Long, dwLength AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FGets(pHandle AS void PTR) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FGets(pHandle AS void PTR, dwCount AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FGets2(pHandle AS void PTR, dwCount AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFieldName" ></ param > 
        /// <returns></returns>
        FUNCTION FieldBlock(cFieldName AS string) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sFieldName" ></ param > 
        /// <returns></returns>
        FUNCTION FieldBlockSym(sFieldName AS Symbol) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FieldGet() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nPos" ></ param > 
        /// <returns></returns>
        FUNCTION FieldGet(nPos AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="symAlias" ></ param > 
        /// <param name="symField" ></ param > 
        /// <returns></returns>
        FUNCTION FieldGetAlias(symAlias AS Symbol, symField AS Symbol) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="wa" ></ param > 
        /// <param name="symField" ></ param > 
        /// <returns></returns>
        FUNCTION FieldGetArea(wa AS DWord, symField AS Symbol) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="symField" ></ param > 
        /// <returns></returns>
        FUNCTION FieldGetSym(symField AS Symbol) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwPos" ></ param > 
        /// <returns></returns>
        FUNCTION FieldName(dwPos AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFieldName" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPos(cFieldName AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFieldName" ></ param > 
        /// <param name="workarea" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPos(cFieldName AS string, workarea AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFieldName" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPosSym(cFieldName AS Symbol) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nPos" ></ param > 
        /// <param name=" xValue" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPut(nPos, xValue) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="symAlias" ></ param > 
        /// <param name="symField" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPutAlias(symAlias AS Symbol, symField AS Symbol, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="wa" ></ param > 
        /// <param name="symField" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPutArea(wa AS DWord, symField AS Symbol, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="symField" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION FieldPutSym(symField AS Symbol, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwPos" ></ param > 
        /// <returns></returns>
        FUNCTION FieldSym(dwPos AS DWord) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFieldName" ></ param > 
        /// <param name="dwArea" ></ param > 
        /// <returns></returns>
        FUNCTION FieldWBlock(cFieldName AS string, dwArea AS DWord) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sFieldName" ></ param > 
        /// <param name="dwArea" ></ param > 
        /// <returns></returns>
        FUNCTION FieldWBlockSym(sFieldName AS Symbol, dwArea AS DWord) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <returns></returns>
        FUNCTION @@File(cFileSpec AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="f" ></ param > 
        /// <param name="iLen" ></ param > 
        /// <param name="iDec" ></ param > 
        /// <returns></returns>
        FUNCTION FloatFormat(f AS Float, iLen AS Long, iDec AS Long) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nValue" ></ param > 
        /// <returns></returns>
        FUNCTION Floor(nValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FNext() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION FOpen(cFile AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FOpen(cFile AS string, dwAttributes AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwAttributes" ></ param > 
        /// <returns></returns>
        FUNCTION FOpen2(cFile AS string, dwAttributes AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Found() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FPathName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION FPuts(pHandle AS void PTR, c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FPuts(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FPuts3(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Frac(n AS Float) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="refC" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FRead(pHandle AS void PTR, refC REF string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="ptrBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FRead3(pHandle AS void PTR, ptrBuffer AS void PTR, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="ptrBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <param name="lAnsi" ></ param > 
        /// <returns></returns>
        FUNCTION FRead4(pHandle AS void PTR, ptrBuffer AS void PTR, dwCount AS DWord, lAnsi AS Logic) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FReadLine(pHandle AS void PTR) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FReadLine(pHandle AS void PTR, dwCount AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FReadLine2(pHandle AS void PTR, dwCount AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FreadStr(pHandle AS void PTR, dwCount AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="refC" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FReadText(pHandle AS void PTR, refC REF string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="ptrBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FReadText3(pHandle AS void PTR, ptrBuffer AS void PTR, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSource" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION FRename(cSource AS string, cTarget AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FRewind(pHandle AS void PTR) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="lOffset" ></ param > 
        /// <returns></returns>
        FUNCTION FSeek(pHandle AS void PTR, lOffset AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="lOffset" ></ param > 
        /// <param name="dwOrigin" ></ param > 
        /// <returns></returns>
        FUNCTION FSeek(pHandle AS void PTR, lOffset AS Long, dwOrigin AS DWord) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="lOffset" ></ param > 
        /// <param name="dwOrigin" ></ param > 
        /// <returns></returns>
        FUNCTION FSeek3(pHandle AS void PTR, lOffset AS Long, dwOrigin AS DWord) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FSize() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FSize64() AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FTell(pHandle AS void PTR) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <returns></returns>
        FUNCTION FTell64(pHandle AS void PTR) AS Int64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION FTime() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION FWrite(pHandle AS void PTR, c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWrite(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="ptrBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWrite3(pHandle AS void PTR, ptrBuffer AS void PTR, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="ptrBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <param name="lAnsi" ></ param > 
        /// <returns></returns>
        FUNCTION FWrite4(pHandle AS void PTR, ptrBuffer AS void PTR, dwCount AS DWord, lAnsi AS Logic) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteLine(pHandle AS void PTR, c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteLine(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteLine3(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteText(pHandle AS void PTR, c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="c" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteText(pHandle AS void PTR, c AS string, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pHandle" ></ param > 
        /// <param name="pWriteBuffer" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION FWriteText3(pHandle AS void PTR, pWriteBuffer AS void PTR, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="dwMode" ></ param > 
        /// <param name="cPath" ></ param > 
        /// <returns></returns>
        FUNCTION FxOpen(cFile AS string, dwMode AS DWord, cPath AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetAMExt() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetCurPath() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetDateFormat() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetDefault() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="_args" ></ param > 
        /// <returns></returns>
        FUNCTION GetDefaultDir( _args AS Usual[]) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetDOSError() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION GetEnv(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uxFileAttr" ></ param > 
        /// <returns></returns>
        FUNCTION GetFAttr(uxFileAttr AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pTime" ></ param > 
        /// <returns></returns>
        FUNCTION GetFDate(pTime AS _WINFILETIME PTR) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileSpec" ></ param > 
        /// <returns></returns>
        FUNCTION GetFMask(cFileSpec AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pTime" ></ param > 
        /// <returns></returns>
        FUNCTION GetFTime(pTime AS _WINFILETIME PTR) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sFileName" ></ param > 
        /// <returns></returns>
        FUNCTION GetMimeType(sFileName AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetNatDll() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetNatDllHandle() AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetPMExt() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetTickCountLow() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION GetTimeSep() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION HardCR(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Header() AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="wValue" ></ param > 
        /// <returns></returns>
        FUNCTION HiByte(wValue AS Word) AS Byte
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ldwValue" ></ param > 
        /// <returns></returns>
        FUNCTION HiDWord(ldwValue AS UInt64) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwValue" ></ param > 
        /// <returns></returns>
        FUNCTION HiWord(dwValue AS DWord) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION I2Bin(n AS Short) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION InCollect() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION IndexCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION IndexExt() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION IndexHPLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION IndexHPLock(lNewSetting AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION IndexKey(uOrder) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION IndexOrd() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION InitSettings() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION InList(u AS Usual,  args AS Usual[]) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION InListExact(u AS Usual,  args AS Usual[]) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION InStr(cSearch AS string, cTarget AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nValue" ></ param > 
        /// <returns></returns>
        FUNCTION Integer(nValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsAccess(oObject AS Object, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsAlNum(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsAlNum(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsAlpha(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsAlpha(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsArray(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsAssign(oObject AS Object, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsBDigit(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsBDigit(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION IsBiDi() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsByRef(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsClass(cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name="cSuperName" ></ param > 
        /// <returns></returns>
        FUNCTION IsClassOf(cName AS string, cSuperName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsCodeBlock(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsDate(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsDigit(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsDigit(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrVar" ></ param > 
        /// <returns></returns>
        FUNCTION IsDynPtr(ptrVar AS void PTR) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsFloat(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsInstanceOf(oObject AS Object, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsInstanceOfUsual(o AS Usual, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsLogic(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsLong(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsLower(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsLower(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsMethod(o AS Object, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsMethodClass(c AS string, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IsMethodUsual(u AS Usual, cName AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsNil(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsNumeric(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsObject(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsOldSpace(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION IsOldSpaceArray(a AS Array) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="f" ></ param > 
        /// <returns></returns>
        FUNCTION IsOldSpaceFloat(f AS Float) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION IsOldSpaceObject(o AS Object) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsOldSpaceString(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsPtr(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsSpace(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsSpace(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsString(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION IsSymbol(u AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsUpper(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsUpper(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsXDigit(c AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION IsXDigit(c AS Psz) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="sInstanceVar" ></ param > 
        /// <returns></returns>
        FUNCTION IVarGet(oObject AS Object, sInstanceVar AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IVarGetInfo(o AS Object, cName AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="sInstanceVar" ></ param > 
        /// <returns></returns>
        FUNCTION IVarGetSelf(oObject AS Object, sInstanceVar AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION IVarList(o AS Object) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="sClassName" ></ param > 
        /// <returns></returns>
        FUNCTION IVarListClass(sClassName AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="sInstanceVar" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION IVarPut(oObject AS Object, sInstanceVar AS string, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="cName" ></ param > 
        /// <returns></returns>
        FUNCTION IVarPutInfo(o AS Object, cName AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name="sInstanceVar" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION IVarPutSelf(oObject AS Object, sInstanceVar AS string, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION L2Bin(n AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION LastRec() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Left(c AS string, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Len(s AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION Len(a AS Array) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION Len(uValue AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDefault" ></ param > 
        /// <param name=" nStringID" ></ param > 
        /// <param name=" xModule" ></ param > 
        /// <returns></returns>
        FUNCTION LoadResString(cDefault, nStringID, xModule) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="wValue" ></ param > 
        /// <returns></returns>
        FUNCTION LoByte(wValue AS Word) AS Byte
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION LockTries() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION LockTries(value AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ldwValue" ></ param > 
        /// <returns></returns>
        FUNCTION LoDWord(ldwValue AS UInt64) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Log(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Log10(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION Logic2Bin(l AS Logic) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Lower(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION LowerA(c REF string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwValue" ></ param > 
        /// <returns></returns>
        FUNCTION LoWord(dwValue AS DWord) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION LToC(value AS Logic) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION LTrim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION LUpdate() AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="wLow" ></ param > 
        /// <param name="wHigh" ></ param > 
        /// <returns></returns>
        FUNCTION MakeDWord(wLow AS Word, wHigh AS Word) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION MAKELANGID(p AS Word, s AS Word) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="lgid" ></ param > 
        /// <param name="srtid" ></ param > 
        /// <returns></returns>
        FUNCTION MAKELCID(lgid AS Word, srtid AS Word) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="wLow" ></ param > 
        /// <param name="wHigh" ></ param > 
        /// <returns></returns>
        FUNCTION MakeLong(wLow AS Word, wHigh AS Word) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="bLow" ></ param > 
        /// <param name="bHigh" ></ param > 
        /// <returns></returns>
        FUNCTION MakeWord(bLow AS Byte, bHigh AS Byte) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cExp" ></ param > 
        /// <param name="xValue" ></ param > 
        /// <returns></returns>
        FUNCTION MAssign(cExp AS string, xValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u1" ></ param > 
        /// <param name="u2" ></ param > 
        /// <returns></returns>
        FUNCTION Max(u1 AS Usual, u2 AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION MaxAtom() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <returns></returns>
        FUNCTION MBAllTrim(cString AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAt(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAt2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="nOffset" ></ param > 
        /// <returns></returns>
        FUNCTION MBAt3(cSearch AS string, cTarget AS string, nOffset AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAtC(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAtC2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAtLine(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBAtLine2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION MBLeft(c AS string, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION MBLen(s AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION MBLTrim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszC" ></ param > 
        /// <returns></returns>
        FUNCTION MBPszLen(pszC AS Psz) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBRat(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION MBRat2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="nOffset" ></ param > 
        /// <returns></returns>
        FUNCTION MBRAt3(cSearch AS string, cTarget AS string, nOffset AS Word) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION MBRight(c AS string, n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION MBRTrim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION MBSLen(c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cTarget" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nDelete" ></ param > 
        /// <param name="cInsert" ></ param > 
        /// <returns></returns>
        FUNCTION MBStuff(cTarget AS string, nStart AS DWord, nDelete AS DWord, cInsert AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nLength" ></ param > 
        /// <returns></returns>
        FUNCTION MBSubStr(c, nStart, nLength) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION MBSubStr2(c AS string, nStart AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION MBSubStr3(c AS string, nStart AS DWord, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION MBTrim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="macroExpression" ></ param > 
        /// <returns></returns>
        FUNCTION MCompile(macroExpression AS string) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="macroExpression" ></ param > 
        /// <param name="voCompatibleSyntax" ></ param > 
        /// <returns></returns>
        FUNCTION MCompile(macroExpression AS string, voCompatibleSyntax AS Logic) AS Codeblock
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION MCShort() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION MCShort(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pBuffer" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION Mem2String(pBuffer AS void PTR, nSize AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemAlloc(nSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemAtSpecial(ptrBuff AS Psz, dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="b" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemByte(ptrBuff AS void PTR, b AS Byte, dwCount AS DWord) AS Byte PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="wCount" ></ param > 
        /// <param name="wElSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemCAlloc(wCount AS DWord, wElSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="wBytes" ></ param > 
        /// <returns></returns>
        FUNCTION MemCheckPtr(p AS void PTR, wBytes AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="b" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemChr(ptrBuff AS void PTR, b AS Byte, dwCount AS DWord) AS Byte PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemClear(p AS void PTR, dwCount AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptr1" ></ param > 
        /// <param name="ptr2" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemComp(ptr1 AS void PTR, ptr2 AS void PTR, dwCount AS DWord) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pDestination" ></ param > 
        /// <param name="pSource" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemCopy(pDestination AS void PTR, pSource AS void PTR, nSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrDest" ></ param > 
        /// <param name="ptrSrc" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemCopyString(ptrDest AS void PTR, ptrSrc AS string, dwCount AS DWord) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="dw" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemDWord(ptrBuff AS void PTR, dw AS DWord, dwCount AS DWord) AS DWord PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION MemFree(p AS void PTR) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nGroup" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemGrpAlloc(nGroup AS DWord, nSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nGroup" ></ param > 
        /// <param name="wCount" ></ param > 
        /// <param name="wElSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemGrpCAlloc(nGroup AS DWord, wCount AS DWord, wElSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwGroup" ></ param > 
        /// <returns></returns>
        FUNCTION MemGrpClose(dwGroup AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION MemGrpOpen() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="i" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemInt(ptrBuff AS void PTR, i AS Long, dwCount AS DWord) AS Long PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pBuffer" ></ param > 
        /// <returns></returns>
        FUNCTION MemLen(pBuffer AS void PTR) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION MemLines(c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="i" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemLong(ptrBuff AS void PTR, i AS Long, dwCount AS DWord) AS Long PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="pDestination" ></ param > 
        /// <param name="pSource" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemMove(pDestination AS void PTR, pSource AS void PTR, nSize AS DWord) AS System.IntPtr
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name=" nLineLen" ></ param > 
        /// <param name=" nLineNum" ></ param > 
        /// <param name=" nTabSize" ></ param > 
        /// <param name=" lWrap" ></ param > 
        /// <returns></returns>
        FUNCTION MemoLine(cMemo, nLineLen, nLineNum, nTabSize, lWrap) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION MemoRead(cFile AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="iFunc" ></ param > 
        /// <returns></returns>
        FUNCTION Memory(iFunc AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cTarget" ></ param > 
        /// <param name=" cReplaceHardCR" ></ param > 
        /// <param name=" cReplaceSoftCR" ></ param > 
        /// <returns></returns>
        FUNCTION MemoTran(cTarget, cReplaceHardCR, cReplaceSoftCR) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="cContent" ></ param > 
        /// <returns></returns>
        FUNCTION MemoWrit(cFile AS string, cContent AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="pBuffer" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemRealloc(pBuffer AS void PTR, nSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pBuffer" ></ param > 
        /// <param name="b" ></ param > 
        /// <param name="nSize" ></ param > 
        /// <returns></returns>
        FUNCTION MemSet(pBuffer AS void PTR, b AS Byte, nSize AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="s" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemShort(ptrBuff AS void PTR, s AS Short, dwCount AS DWord) AS Short PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION MemTotal() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemUpper(ptrBuff AS void PTR, dwCount AS DWord) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <returns></returns>
        FUNCTION MemVarBlock(cVar AS string) AS Object
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <returns></returns>
        FUNCTION MemVarBlockSym(cVar AS Symbol) AS Object
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <returns></returns>
        FUNCTION MemVarGet(cVar AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION MemVarPut(cVar AS string, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION MemWalk() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrBuff" ></ param > 
        /// <param name="w" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION MemWord(ptrBuff AS void PTR, w AS Word, dwCount AS DWord) AS Word PTR
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION MethodList(o AS Object) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION MethodListClass(c AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="macro" ></ param > 
        /// <returns></returns>
        FUNCTION MExec(macro AS Codeblock) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u1" ></ param > 
        /// <param name="u2" ></ param > 
        /// <returns></returns>
        FUNCTION Min(u1 AS Usual, u2 AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name=" nLineLen" ></ param > 
        /// <param name=" nTabSize" ></ param > 
        /// <param name=" lWrap" ></ param > 
        /// <returns></returns>
        FUNCTION MLCount(cMemo, nLineLen, nTabSize, lWrap) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <returns></returns>
        FUNCTION MLCount1(cMemo AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name=" nLineLen" ></ param > 
        /// <param name=" nLineNum" ></ param > 
        /// <param name=" nColumn" ></ param > 
        /// <param name=" nTabSize" ></ param > 
        /// <param name=" lWrap" ></ param > 
        /// <returns></returns>
        FUNCTION MLcToPos(cMemo, nLineLen, nLineNum, nColumn, nTabSize, lWrap) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name="nLineNum" ></ param > 
        /// <returns></returns>
        FUNCTION MLine(cMemo AS Usual, nLineNum AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name="nLineNum" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <returns></returns>
        FUNCTION MLine(cMemo AS Usual, nLineNum AS Usual, nIndex REF DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name="nLineNum" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <returns></returns>
        FUNCTION MLine(cMemo AS Usual, nLineNum AS Usual, nIndex AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name="nLineNum" ></ param > 
        /// <param name="nIndex" ></ param > 
        /// <returns></returns>
        FUNCTION MLine3(cMemo AS string, nLineNum AS DWord, nIndex REF DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name=" nLineLen" ></ param > 
        /// <param name=" nLineNum" ></ param > 
        /// <param name=" nTabSize" ></ param > 
        /// <param name=" lWrap" ></ param > 
        /// <returns></returns>
        FUNCTION MLPos(cMemo, nLineLen, nLineNum, nTabSize, lWrap) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name="nLineNum" ></ param > 
        /// <returns></returns>
        FUNCTION MLPos2(cMemo AS string, nLineNum AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <param name="nBase" ></ param > 
        /// <returns></returns>
        FUNCTION @@Mod(n AS Usual, nBase AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lFull" ></ param > 
        /// <returns></returns>
        FUNCTION ModuleName(lFull AS Logic) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION Month(d AS Date) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cMemo" ></ param > 
        /// <param name=" nLineLen" ></ param > 
        /// <param name=" nPos" ></ param > 
        /// <param name=" nTabSize" ></ param > 
        /// <param name=" lWrap" ></ param > 
        /// <returns></returns>
        FUNCTION MPosToLc(cMemo, nLineLen, nPos, nTabSize, lWrap) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cStr" ></ param > 
        /// <returns></returns>
        FUNCTION Multi2Wide(cStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION NetErr() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION NetErr(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION NetName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION NewIndexLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lFlag" ></ param > 
        /// <returns></returns>
        FUNCTION NewIndexLock(lFlag AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION NewLocks() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION NoMethod() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION NToCDoW(n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION NToCMonth(n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION NTrim(uValue AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION Occurs(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION Occurs2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="nOffSet" ></ param > 
        /// <returns></returns>
        FUNCTION Occurs3(cSearch AS string, cTarget AS string, nOffSet AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Oem2Ansi(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszDest" ></ param > 
        /// <param name="pszSource" ></ param > 
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION Oem2AnsiBuff(pszDest AS Psz, pszSource AS Psz, dwCount AS DWord) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Oem2OAnsiA(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION OldSpaceFree(p AS Psz) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="a" ></ param > 
        /// <returns></returns>
        FUNCTION OldSpaceFreeArray(a AS Array) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lSet" ></ param > 
        /// <returns></returns>
        FUNCTION OleDateTimeAsDate(lSet) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION OOPTree(o AS Object) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION OOPTreeClass(c AS string) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION OrdBagExt() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION OrdBagName(uOrder AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFor" ></ param > 
        /// <param name=" uCobFor" ></ param > 
        /// <param name=" lAll" ></ param > 
        /// <param name=" uCobWhile" ></ param > 
        /// <param name=" uCobEval" ></ param > 
        /// <param name=" nStep" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nNext" ></ param > 
        /// <param name=" nRecNo" ></ param > 
        /// <param name=" lRest" ></ param > 
        /// <param name=" lDescending" ></ param > 
        /// <param name=" lAdditive" ></ param > 
        /// <param name=" lCurrent" ></ param > 
        /// <param name=" lCustom" ></ param > 
        /// <param name=" lNoOptimize" ></ param > 
        /// <returns></returns>
        FUNCTION OrdCondSet(cFor, uCobFor, lAll, uCobWhile, uCobEval, nStep, nStart, nNext, nRecNo, lRest, lDescending, lAdditive, lCurrent, lCustom, lNoOptimize) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name=" cOrder" ></ param > 
        /// <param name=" cExpr" ></ param > 
        /// <param name=" cobExpr" ></ param > 
        /// <param name=" lUnique" ></ param > 
        /// <returns></returns>
        FUNCTION OrdCreate(cName, cOrder, cExpr, cobExpr, lUnique) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <param name=" lDescend" ></ param > 
        /// <returns></returns>
        FUNCTION OrdDescend(xOrder, cOrdBag, lDescend) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdDestroy(uOrder, cOrdBag) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <param name=" cFor" ></ param > 
        /// <returns></returns>
        FUNCTION OrdFor(uOrder, cOrdBag, cFor) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrderBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdIsUnique(xOrder, cOrderBag) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKey(uOrder, cOrdBag) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <param name=" xVal" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKeyAdd(xOrder, cOrdBag, xVal) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKeyCount(xOrder, cOrdBag) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <param name=" xVal" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKeyDel(xOrder, cOrdBag, xVal) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nKeyNo" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKeyGoTo(nKeyNo) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdKeyNo(xOrder, cOrdBag) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION OrdKeyVal() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOrdBag" ></ param > 
        /// <param name=" uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION OrdListAdd(cOrdBag, uOrder) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOrdBag" ></ param > 
        /// <param name=" uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION OrdListClear(cOrdBag, uOrder) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION OrdListRebuild() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdName(uOrder, cOrdBag) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdNumber(uOrder, cOrdBag) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nScope" ></ param > 
        /// <param name=" xVal" ></ param > 
        /// <returns></returns>
        FUNCTION OrdScope(nScope, xVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uOrder" ></ param > 
        /// <param name=" cOrdBag" ></ param > 
        /// <returns></returns>
        FUNCTION OrdSetFocus(uOrder, cOrdBag) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAlias" ></ param > 
        /// <param name=" bKey" ></ param > 
        /// <param name=" cKey" ></ param > 
        /// <returns></returns>
        FUNCTION OrdSetRelation(cAlias, bKey, cKey) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION OrdSkipUnique(nCount) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION OS( args AS Object[]) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS Long, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS DWord, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION Pad(uValue AS Object, nLength AS Usual, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS Long, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS DWord, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadC(uValue AS Object, nLength AS Usual, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS Long, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS DWord, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadL(uValue AS Object, nLength AS Usual, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS Long, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS DWord, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uValue" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="cFillStr" ></ param > 
        /// <returns></returns>
        FUNCTION PadR(uValue AS Object, nLength AS Usual, cFillStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nBase" ></ param > 
        /// <param name="nExponent" ></ param > 
        /// <returns></returns>
        FUNCTION Pow(nBase AS real8, nExponent AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ProcFile() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcFile(activation AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcFile(activation AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcFile(activation AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ProcLine() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcLine(activation AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcLine(activation AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcLine(activation AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION ProcName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcName(activation AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcName(activation AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="activation" ></ param > 
        /// <returns></returns>
        FUNCTION ProcName(activation AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cText" ></ param > 
        /// <returns></returns>
        FUNCTION Proper(cText AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION ProperA(c REF string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION Psz2String(p AS Psz) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION Psz2Usual(p AS Psz) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION PszAlloc(p AS Psz) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION PszLen(p AS Psz) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Ptr2Bin(n AS void PTR) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lpv" ></ param > 
        /// <returns></returns>
        FUNCTION PtrLen(lpv AS void PTR) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="lpv" ></ param > 
        /// <returns></returns>
        FUNCTION PtrLenWrite(lpv AS void PTR) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION QOut() AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION QOut( o AS Usual[]) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION QOut(o AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION QQOut( o AS Usual[]) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION QQOut(o AS Usual) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSubKey" ></ param > 
        /// <param name="cKeyName" ></ param > 
        /// <returns></returns>
        FUNCTION QueryRTRegInt(cSubKey AS string, cKeyName AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSubKey" ></ param > 
        /// <param name="cKeyName" ></ param > 
        /// <returns></returns>
        FUNCTION QueryRTRegString(cSubKey AS string, cKeyName AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Rand() AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSeed" ></ param > 
        /// <returns></returns>
        FUNCTION Rand(nSeed AS Long) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSeed" ></ param > 
        /// <returns></returns>
        FUNCTION Rand(nSeed AS Usual) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION RAt(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION RAt2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <param name="nOffset" ></ param > 
        /// <returns></returns>
        FUNCTION RAt3(cSearch AS string, cTarget AS string, nOffset AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION RAtLine(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSearch" ></ param > 
        /// <param name="cTarget" ></ param > 
        /// <returns></returns>
        FUNCTION RAtLine2(cSearch AS string, cTarget AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RDDCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nType" ></ param > 
        /// <returns></returns>
        FUNCTION RDDCount(nType AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" xNewVal" ></ param > 
        /// <returns></returns>
        FUNCTION RDDInfo(nOrdinal, xNewVal) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RDDList() AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nType" ></ param > 
        /// <returns></returns>
        FUNCTION RDDList(nType AS Long) AS Array
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RDDName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cDriver" ></ param > 
        /// <returns></returns>
        FUNCTION RDDSetDefault(cDriver) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Real42Bin(n AS real4) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Real82Bin(n AS real8) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RecCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RecNo() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RecSize() AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION RegisterAxit(o AS Object) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name="count" ></ param > 
        /// <param name="isPoly" ></ param > 
        /// <returns></returns>
        FUNCTION RegisterKid(o AS Object, count AS Long, isPoly AS Logic) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Repli(c AS string, n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Replicate(c AS string, n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="red" ></ param > 
        /// <param name="green" ></ param > 
        /// <param name="blue" ></ param > 
        /// <returns></returns>
        FUNCTION RGB(red AS Byte, green AS Byte, blue AS Byte) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Right(c AS string, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION RLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="iDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION Round(nNumber AS Usual, iDecimals AS Long) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Row() AS Short
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION RTrim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SBToDB(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SClone(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Seconds() AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION Secs(c AS string) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="xValue" ></ param > 
        /// <returns></returns>
        FUNCTION Select(xValue) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="oObject" ></ param > 
        /// <param name=" cName" ></ param > 
        /// <returns></returns>
        FUNCTION Send(oObject, cName) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <param name=" symMethod" ></ param > 
        /// <param name=" symClassName" ></ param > 
        /// <returns></returns>
        FUNCTION SendClass(o, symMethod, symClassName) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSet" ></ param > 
        /// <param name=" uSetting" ></ param > 
        /// <returns></returns>
        FUNCTION @@Set(nSet, uSetting) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SetAMExt(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetAMPM() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION SetAMPM(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetAnsi() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION SetAnsi(d AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetBeep() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetBeep(n AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetCentury() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lDisplayCentury" ></ param > 
        /// <returns></returns>
        FUNCTION SetCentury(lDisplayCentury AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetCollation() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="newCollation" ></ param > 
        /// <returns></returns>
        FUNCTION SetCollation(newCollation AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetColor() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SetColor(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetCPU() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION SetCPU(nNewSetting AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDateCountry() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetDateCountry(n AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="newFormat" ></ param > 
        /// <returns></returns>
        FUNCTION SetDateFormat(newFormat AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDecimal() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetDecimal(n AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDecimalSep() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="sep" ></ param > 
        /// <returns></returns>
        FUNCTION SetDecimalSep(sep AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cPathSpec" ></ param > 
        /// <returns></returns>
        FUNCTION SetDefault(cPathSpec AS string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="_args" ></ param > 
        /// <returns></returns>
        FUNCTION SetDefaultDir( _args AS Usual[]) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDeleted() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION SetDeleted(d AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDigit() AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetDigit(n AS Long) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetDigitFixed() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION SetDigitFixed(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <param name="cValue" ></ param > 
        /// <param name="lAppend" ></ param > 
        /// <returns></returns>
        FUNCTION SetEnv(cVar AS string, cValue AS string, lAppend AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetEpoch() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="uiEpoch" ></ param > 
        /// <returns></returns>
        FUNCTION SetEpoch(uiEpoch AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="_args" ></ param > 
        /// <returns></returns>
        FUNCTION SetErrorLog( _args AS Usual[]) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetExact() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION SetExact(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetExclusive() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION SetExclusive(value AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <param name="d" ></ param > 
        /// <param name="cTime" ></ param > 
        /// <returns></returns>
        FUNCTION SetFDateTime(cFile AS string, d AS Date, cTime AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetFixed() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION SetFixed(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetFloatDelta() AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetFloatDelta(n AS Float) AS Float
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetHPLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION SetHPLock(value AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lUppercase" ></ param > 
        /// <returns></returns>
        FUNCTION SetImplicitString2SymbolUppercase(lUppercase AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetInternational() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SetInternational(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetKidStackSize(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetKidStackSpace(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name=" cNew" ></ param > 
        /// <param name=" lSetDefault" ></ param > 
        /// <returns></returns>
        FUNCTION SetLiteral(nOrdinal, cNew, lSetDefault) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="newType" ></ param > 
        /// <returns></returns>
        FUNCTION SetMacroCompiler(newType AS System.Type) AS System.Type
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetMath() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION SetMath(nNewSetting AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxDynSize(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxDynSpace(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxRegisteredAxitMethods(dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwCount" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxRegisteredKids(dwCount AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxThreadDynSize(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwBytes" ></ param > 
        /// <returns></returns>
        FUNCTION SetMaxThreadDynSpace(dwBytes AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cNewDLL" ></ param > 
        /// <returns></returns>
        FUNCTION SetNatDll(cNewDLL AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cPath" ></ param > 
        /// <returns></returns>
        FUNCTION SetPath(cPath AS string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SetPMExt(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRow" ></ param > 
        /// <param name="nCol" ></ param > 
        /// <returns></returns>
        FUNCTION SetPos(nRow AS Long, nCol AS Long) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetRddCodePage() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION SetRddCodePage(d AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSubKey" ></ param > 
        /// <param name="cKeyName" ></ param > 
        /// <param name="nKeyVal" ></ param > 
        /// <returns></returns>
        FUNCTION SetRTRegInt(cSubKey AS string, cKeyName AS string, nKeyVal AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cSubKey" ></ param > 
        /// <param name="cKeyName" ></ param > 
        /// <param name="cKeyVal" ></ param > 
        /// <returns></returns>
        FUNCTION SetRTRegString(cSubKey AS string, cKeyName AS string, cKeyVal AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetScience() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="l" ></ param > 
        /// <returns></returns>
        FUNCTION SetScience(l AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetSoftSeek() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION SetSoftSeek(value AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetThousandSep() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="sep" ></ param > 
        /// <returns></returns>
        FUNCTION SetThousandSep(sep AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SetTimeSep(n AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SetUnique() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="value" ></ param > 
        /// <returns></returns>
        FUNCTION SetUnique(value AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="iWipe" ></ param > 
        /// <returns></returns>
        FUNCTION SetWipeDynSpace(iWipe AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION SEval(cString AS string, cbBlock AS Codeblock) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION SEval(cString AS string, cbBlock AS Codeblock, nStart AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION SEval(cString AS string, cbBlock AS Codeblock, nStart AS Long, nCount AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION SEval(cString AS string, cbBlock AS Codeblock, nStart AS Usual, nCount AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <returns></returns>
        FUNCTION SEvalA(cString AS string, cbBlock AS Codeblock) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION SEvalA(cString AS string, cbBlock AS Codeblock, nStart AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION SEvalA(cString AS string, cbBlock AS Codeblock, nStart AS Long, nCount AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <param name="cbBlock" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nCount" ></ param > 
        /// <returns></returns>
        FUNCTION SEvalA(cString AS string, cbBlock AS Codeblock, nStart AS Usual, nCount AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="aTest" ></ param > 
        /// <param name=" cName" ></ param > 
        /// <returns></returns>
        FUNCTION ShowArray(aTest, cName) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="hWnd" ></ param > 
        /// <param name="cFileName" ></ param > 
        /// <param name="cTitle" ></ param > 
        /// <returns></returns>
        FUNCTION ShowBitmap(hWnd AS void PTR, cFileName AS string, cTitle AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Sin(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SoundEx(c AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Space(n AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cPath" ></ param > 
        /// <param name="cDrive" ></ param > 
        /// <param name="cDir" ></ param > 
        /// <param name="cFile" ></ param > 
        /// <param name="cExt" ></ param > 
        /// <returns></returns>
        FUNCTION SplitPath(cPath AS string, cDrive REF string, cDir REF string, cFile REF string, cExt REF string) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="pszPathName" ></ param > 
        /// <param name="pszDrive" ></ param > 
        /// <param name="pszDir" ></ param > 
        /// <param name="pszFile" ></ param > 
        /// <param name="pszExt" ></ param > 
        /// <returns></returns>
        FUNCTION SplitPath(pszPathName AS Psz, pszDrive AS Psz, pszDir AS Psz, pszFile AS Psz, pszExt AS Psz) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION SqRt(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION SToD(c AS string) AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="number" ></ param > 
        /// <param name=" digits" ></ param > 
        /// <param name=" decimals" ></ param > 
        /// <returns></returns>
        FUNCTION Str(number, digits, decimals) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <returns></returns>
        FUNCTION Str1(nNumber AS Float) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Str2(nNumber AS Float, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION Str3(nNumber AS Float, nLength AS DWord, nDecimals AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="hWnd" ></ param > 
        /// <param name="cFileName" ></ param > 
        /// <param name="cTitle" ></ param > 
        /// <returns></returns>
        FUNCTION StretchBitmap(hWnd AS void PTR, cFileName AS string, cTitle AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION StrEvaluate(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION StrFloat(nNumber AS Float, nLength AS DWord, nDecimals AS DWord) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION String2Atom(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION String2Mem(s AS string) AS void PTR
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION String2Psz(s AS string) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION String2Symbol(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL


        /// <summary>
        ///
        /// </summary>
        /// <param name="sz" ></ param > 
        /// <returns></returns>
        FUNCTION String2W(sz AS string) AS void PTR
        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION StringAlloc(s AS string) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION StrInt(nNumber AS Long, nLength AS DWord, nDecimals AS DWord) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION StrLong(nNumber AS Long, nLength AS DWord, nDecimals AS DWord) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="uTarget" ></ param > 
        /// <param name=" uSearch" ></ param > 
        /// <param name=" uReplace" ></ param > 
        /// <param name=" uStart" ></ param > 
        /// <param name=" uCount" ></ param > 
        /// <returns></returns>
        FUNCTION StrTran(uTarget, uSearch, uReplace, uStart, uCount) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <returns></returns>
        FUNCTION StrZero(nNumber AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION StrZero(nNumber AS Usual, nLength AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNumber" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <param name="nDecimals" ></ param > 
        /// <returns></returns>
        FUNCTION StrZero(nNumber AS Usual, nLength AS Long, nDecimals AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cTarget" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nDelete" ></ param > 
        /// <param name="cInsert" ></ param > 
        /// <returns></returns>
        FUNCTION Stuff(cTarget AS string, nStart AS DWord, nDelete AS DWord, cInsert AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nLength" ></ param > 
        /// <returns></returns>
        FUNCTION Subs(c, nStart, nLength) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name=" nStart" ></ param > 
        /// <param name=" nLength" ></ param > 
        /// <returns></returns>
        FUNCTION SubStr(c, nStart, nLength) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <returns></returns>
        FUNCTION SubStr2(c AS string, nStart AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="nStart" ></ param > 
        /// <param name="nLength" ></ param > 
        /// <returns></returns>
        FUNCTION SubStr3(c AS string, nStart AS DWord, nLength AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="b" ></ param > 
        /// <returns></returns>
        FUNCTION SwapByte(b AS Byte) AS Byte
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION SwapDWord(d AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="i" ></ param > 
        /// <returns></returns>
        FUNCTION SwapInt(i AS Long) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="i" ></ param > 
        /// <returns></returns>
        FUNCTION SwapLong(i AS Long) AS Long
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="qw" ></ param > 
        /// <returns></returns>
        FUNCTION SwapQWord(qw AS UInt64) AS UInt64
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SwapShort(s AS Short) AS Short
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="w" ></ param > 
        /// <returns></returns>
        FUNCTION SwapWord(w AS Word) AS Word
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Symbol2String(s AS Symbol) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SysAddAtom(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION SysAddAtom(p AS Psz) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SysAddAtomUpperA(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION SysAddAtomUpperA(p AS Psz) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SysFindAtom(s AS string) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="p" ></ param > 
        /// <returns></returns>
        FUNCTION SysFindAtom(p AS Psz) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION SysGetAtomName(s AS Symbol) AS Psz
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION SysObject() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="oSys" ></ param > 
        /// <returns></returns>
        FUNCTION SysObject(oSys AS Object) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION Tan(n AS real8) AS real8
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Time() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Time24() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Today() AS Date
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION ToHira(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION ToJNum(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION ToKata(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwFreq" ></ param > 
        /// <param name="dwDur" ></ param > 
        /// <returns></returns>
        FUNCTION Tone(dwFreq AS DWord, dwDur AS DWord) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION ToWord(u AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="lValue" ></ param > 
        /// <param name="cPicture" ></ param > 
        /// <returns></returns>
        FUNCTION Transform(lValue AS Logic, cPicture AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <param name="cPicture" ></ param > 
        /// <returns></returns>
        FUNCTION Transform(c AS string, cPicture AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="expr" ></ param > 
        /// <param name="picture" ></ param > 
        /// <returns></returns>
        FUNCTION Transform(expr AS Usual, picture AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="dValue" ></ param > 
        /// <param name="cPicture" ></ param > 
        /// <returns></returns>
        FUNCTION Transform(dValue AS Date, cPicture AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nValue" ></ param > 
        /// <param name="cPicture" ></ param > 
        /// <returns></returns>
        FUNCTION Transform(nValue AS Float, cPicture AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Trim(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION TruePath(cFile AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSeconds" ></ param > 
        /// <returns></returns>
        FUNCTION TString(nSeconds AS Long) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="seconds" ></ param > 
        /// <returns></returns>
        FUNCTION TString(seconds AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSeconds" ></ param > 
        /// <returns></returns>
        FUNCTION TString(nSeconds AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSeconds" ></ param > 
        /// <returns></returns>
        FUNCTION TString(nSeconds AS Float) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cString" ></ param > 
        /// <returns></returns>
        FUNCTION @@Type(cString) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nType" ></ param > 
        /// <returns></returns>
        FUNCTION TypeString(nType AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cValue" ></ param > 
        /// <param name="cSayPicture" ></ param > 
        /// <param name="cType" ></ param > 
        /// <returns></returns>
        FUNCTION Unformat(cValue AS string, cSayPicture AS string, cType AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION UnregisterAxit(o AS Object) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="o" ></ param > 
        /// <returns></returns>
        FUNCTION UnRegisterKid(o AS Object) AS void
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="s" ></ param > 
        /// <returns></returns>
        FUNCTION Upper(s AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="c" ></ param > 
        /// <returns></returns>
        FUNCTION UpperA(c REF string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Used() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION UsualType(u AS Usual) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION UsualVal(u AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cInput" ></ param > 
        /// <param name="hOutput" ></ param > 
        /// <returns></returns>
        FUNCTION UUDecodeLine(cInput AS string, hOutput AS void PTR) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="cFileName" ></ param > 
        /// <returns></returns>
        FUNCTION UUEncFile(cFileName AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="str" ></ param > 
        /// <returns></returns>
        FUNCTION Val(str AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="u" ></ param > 
        /// <returns></returns>
        FUNCTION ValType(u AS Usual) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <returns></returns>
        FUNCTION VarGet(cVar AS string) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cVar" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION VarPut(cVar AS string, uValue AS Usual) AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION Version() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="format" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION VO_Sprintf(format AS string,  args AS Object[]) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="format" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION VO_Sprintf(format AS DWord,  args AS Object[]) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="format" ></ param > 
        /// <param name="args" ></ param > 
        /// <returns></returns>
        FUNCTION VO_Sprintf(format AS Usual,  args AS Object[]) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nArea" ></ param > 
        /// <returns></returns>
        FUNCTION VODBAlias(nArea AS DWord) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="nArea" ></ param > 
        /// <returns></returns>
        FUNCTION VODBAliasSym(nArea AS DWord) AS Symbol
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="lReleaseLocks" ></ param > 
        /// <returns></returns>
        FUNCTION VODBAppend(lReleaseLocks AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name="nPos" ></ param > 
        /// <param name="ptrRet" ></ param > 
        /// <returns></returns>
        FUNCTION VODBBlobInfo(nOrdinal AS DWord, nPos AS DWord, ptrRet REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBBof() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBBuffRefresh() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBClearFilter() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBClearLocate() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBClearRelation() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBClearScope() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBCloseAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBCloseArea() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBCommit() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBCommitAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBContinue() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name="aStru" ></ param > 
        /// <param name="rddName" ></ param > 
        /// <param name="lNew" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="cDelim" ></ param > 
        /// <param name="lKeep" ></ param > 
        /// <param name="lJustOpen" ></ param > 
        /// <returns></returns>
        FUNCTION VODBCreate(cName AS string, aStru AS Array, rddName AS string, lNew AS Logic, cAlias AS string, cDelim AS string, lKeep AS Logic, lJustOpen AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name="aStru" ></ param > 
        /// <param name="rddType" ></ param > 
        /// <param name="lNew" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="cDelim" ></ param > 
        /// <param name="lKeep" ></ param > 
        /// <param name="lJustOpen" ></ param > 
        /// <returns></returns>
        FUNCTION VODBCreate(cName AS string, aStru AS Array, rddType AS System.Type, lNew AS Logic, cAlias AS string, cDelim AS string, lKeep AS Logic, lJustOpen AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cName" ></ param > 
        /// <param name="aStru" ></ param > 
        /// <param name="rddList" ></ param > 
        /// <param name="lNew" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="cDelim" ></ param > 
        /// <param name="lKeep" ></ param > 
        /// <param name="lJustOpen" ></ param > 
        /// <returns></returns>
        FUNCTION VODBCreate(cName AS string, aStru AS Array, rddList AS _RDDLIST, lNew AS Logic, cAlias AS string, cDelim AS string, lKeep AS Logic, lJustOpen AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBDelete() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBDeleted() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBEof() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uBlock" ></ param > 
        /// <param name="uCobFor" ></ param > 
        /// <param name="uCobWhile" ></ param > 
        /// <param name="nNext" ></ param > 
        /// <param name="nRecNo" ></ param > 
        /// <param name="lRest" ></ param > 
        /// <returns></returns>
        FUNCTION VODBEval(uBlock AS Usual, uCobFor AS Usual, uCobWhile AS Usual, nNext AS Usual, nRecNo AS Usual, lRest AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="dwFieldPos" ></ param > 
        /// <param name="uRetVal" ></ param > 
        /// <returns></returns>
        FUNCTION VODBFieldGet(dwFieldPos AS DWord, uRetVal REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="kInfoType" ></ param > 
        /// <param name="wFieldPos" ></ param > 
        /// <param name="uRetVal" ></ param > 
        /// <returns></returns>
        FUNCTION VODBFieldInfo(kInfoType AS DWord, wFieldPos AS DWord, uRetVal REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldPos" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION VODBFieldPut(nFieldPos AS DWord, uValue AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldPos" ></ param > 
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION VODBFileGet(nFieldPos AS DWord, cFile AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nFieldPos" ></ param > 
        /// <param name="cFile" ></ param > 
        /// <returns></returns>
        FUNCTION VODBFilePut(nFieldPos AS DWord, cFile AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBFilter() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBFLock() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBFound() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="dbsci" ></ param > 
        /// <returns></returns>
        FUNCTION VODBGetScope(dbsci AS DBSCOPEINFO) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBGetSelect() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBGoBottom() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecID" ></ param > 
        /// <returns></returns>
        FUNCTION VODBGoTo(uRecID AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBGoTop() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="kInfoType" ></ param > 
        /// <param name="retVal" ></ param > 
        /// <returns></returns>
        FUNCTION VODBInfo(kInfoType AS DWord, retVal REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nSelect" ></ param > 
        /// <param name="jList" ></ param > 
        /// <returns></returns>
        FUNCTION VODBJoinAppend(nSelect AS DWord, jList AS _JOINLIST) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBLastRec() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="uCobFor" ></ param > 
        /// <param name="uCobWhile" ></ param > 
        /// <param name="nNext" ></ param > 
        /// <param name="uRecId" ></ param > 
        /// <param name="lRest" ></ param > 
        /// <returns></returns>
        FUNCTION VODBLocate(uCobFor AS Usual, uCobWhile AS Usual, nNext AS Long, uRecId AS Usual, lRest AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBOrdBagExt() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="ptrCondInfo" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdCondSet(ptrCondInfo AS DBORDERCONDINFO) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cBagName" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <param name="cExpr" ></ param > 
        /// <param name="uCobExpr" ></ param > 
        /// <param name="lUnique" ></ param > 
        /// <param name="ordCondInfo" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdCreate(cBagName AS string, uOrder AS Usual, cExpr AS string, uCobExpr AS Usual, lUnique AS Logic, ordCondInfo AS DBORDERCONDINFO) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cBagName" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdDestroy(cBagName AS string, uOrder AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name="cBagName" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <param name="retval" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrderInfo(nOrdinal AS DWord, cBagName AS string, uOrder AS Usual, retval REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOrdBag" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdListAdd(cOrdBag AS string, uOrder AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOrdBag" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdListClear(cOrdBag AS string, uOrder AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBOrdListRebuild() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cOrdBag" ></ param > 
        /// <param name="uOrder" ></ param > 
        /// <param name="previousOrder" ></ param > 
        /// <returns></returns>
        FUNCTION VODBOrdSetFocus(cOrdBag AS string, uOrder AS Usual, previousOrder REF string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBPack() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRDDCount() AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRddType" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRDDCount(nRddType AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name="ptrRet" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRDDInfo(nOrdinal AS Long, ptrRet REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRDDList() AS string[]
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="rddList" ></ param > 
        /// <param name="nRDDType" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRDDList(rddList AS _RDDLIST, nRDDType AS DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRDDName() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cNewSetting" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRDDSetDefault(cNewSetting AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRecall() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRecno() AS Usual
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBRecordGet() AS Byte[]
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nOrdinal" ></ param > 
        /// <param name="uRecID" ></ param > 
        /// <param name="uValue" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRecordInfo(nOrdinal AS DWord, uRecID AS Usual, uValue REF Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="@@record" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRecordPut(@@record AS Byte[]) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cRddName" ></ param > 
        /// <param name="oType" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRegisterRDD(cRddName AS string, oType AS System.Type) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRelation" ></ param > 
        /// <param name="sRelation" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRelation(nRelation AS DWord, sRelation REF string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecId" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRLock(uRecId AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nPos" ></ param > 
        /// <returns></returns>
        FUNCTION VODBRSelect(nPos AS DWord) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="xValue" ></ param > 
        /// <param name="lSoft" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSeek(xValue AS Usual, lSoft AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="xValue" ></ param > 
        /// <param name="lSoft" ></ param > 
        /// <param name="lLast" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSeek(xValue AS Usual, lSoft AS Logic, lLast AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nNew" ></ param > 
        /// <param name="nOld" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSelect(nNew AS DWord, nOld REF DWord) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uCobFilter" ></ param > 
        /// <param name="cFilter" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetFilter(uCobFilter AS Codeblock, cFilter AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lFound" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetFound(lFound AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uCob" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetLocate(uCob AS Codeblock) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="cAlias" ></ param > 
        /// <param name="uCobKey" ></ param > 
        /// <param name="cKey" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetRelation(cAlias AS string, uCobKey AS Usual, cKey AS string) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="dbsci" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetScope(dbsci AS DBSCOPEINFO) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="siNewArea" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSetSelect(siNewArea AS Long) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nRecords" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSkip(nRecords AS Long) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nDest" ></ param > 
        /// <param name="fNames" ></ param > 
        /// <param name="uCobFor" ></ param > 
        /// <param name="uCobWhile" ></ param > 
        /// <param name="nNext" ></ param > 
        /// <param name="uRecID" ></ param > 
        /// <param name="lRest" ></ param > 
        /// <param name="fSortNames" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSort(nDest AS DWord, fNames AS _FIELDNAMES, uCobFor AS Usual, uCobWhile AS Usual, nNext AS Usual, uRecID AS Usual, lRest AS Logic, fSortNames AS _FIELDNAMES) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="symAlias" ></ param > 
        /// <returns></returns>
        FUNCTION VODBSymSelect(symAlias AS Symbol) AS DWord
        THROW NotImplementedException{}
        RETURN 0

        /// <summary>
        ///
        /// </summary>
        /// <param name="nDest" ></ param > 
        /// <param name="fldNames" ></ param > 
        /// <param name="uCobFor" ></ param > 
        /// <param name="uCobWhile" ></ param > 
        /// <param name="nNext" ></ param > 
        /// <param name="uRecID" ></ param > 
        /// <param name="lRest" ></ param > 
        /// <returns></returns>
        FUNCTION VODBTrans(nDest AS DWord, fldNames AS _FIELDNAMES, uCobFor AS Usual, uCobWhile AS Usual, nNext AS Usual, uRecID AS Usual, lRest AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="nDest" ></ param > 
        /// <param name="fldNames" ></ param > 
        /// <returns></returns>
        FUNCTION VODBTransRec(nDest AS DWord, fldNames AS _FIELDNAMES) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="uRecID" ></ param > 
        /// <returns></returns>
        FUNCTION VODBUnlock(uRecID AS Usual) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBUnlockAll() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNew" ></ param > 
        /// <param name="rddName" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="lShare" ></ param > 
        /// <param name="lReadOnly" ></ param > 
        /// <returns></returns>
        FUNCTION VODBUseArea(lNew AS Logic, rddName AS string, cName AS string, cAlias AS string, lShare AS Logic, lReadOnly AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNew" ></ param > 
        /// <param name="rddType" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="lShare" ></ param > 
        /// <param name="lReadOnly" ></ param > 
        /// <returns></returns>
        FUNCTION VODBUseArea(lNew AS Logic, rddType AS System.Type, cName AS string, cAlias AS string, lShare AS Logic, lReadOnly AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <param name="lNew" ></ param > 
        /// <param name="rddList" ></ param > 
        /// <param name="cName" ></ param > 
        /// <param name="cAlias" ></ param > 
        /// <param name="lShare" ></ param > 
        /// <param name="lReadOnly" ></ param > 
        /// <returns></returns>
        FUNCTION VODBUseArea(lNew AS Logic, rddList AS _RDDLIST, cName AS string, cAlias AS string, lShare AS Logic, lReadOnly AS Logic) AS Logic
        THROW NotImplementedException{}
        RETURN FALSE

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION VODBZap() AS Logic
        THROW NotImplementedException{}
        RETURN FALSE


        /// <summary>
        ///
        /// </summary>
        /// <param name="hwnd" ></ param > 
        /// <param name="nMsg" ></ param > 
        /// <param name="dwParam" ></ param > 
        /// <param name="lParam" ></ param > 
        /// <returns></returns>
        FUNCTION VOSendMessage(hwnd AS void PTR, nMsg AS DWord, dwParam AS DWord, lParam AS Long) AS Long
        /// <summary>
        ///
        /// </summary>
        /// <param name="libPath" ></ param > 
        /// <returns></returns>
        FUNCTION VulcanLoadLibrary(libPath AS string) AS System.Reflection.Assembly
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="n" ></ param > 
        /// <returns></returns>
        FUNCTION W2Bin(n AS Word) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="cStr" ></ param > 
        /// <returns></returns>
        FUNCTION Wide2Multi(cStr AS string) AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        FUNCTION WorkDir() AS string
        THROW NotImplementedException{}
        RETURN NULL

        /// <summary>
        ///
        /// </summary>
        /// <param name="d" ></ param > 
        /// <returns></returns>
        FUNCTION Year(d AS Date) AS DWord
        THROW NotImplementedException{}
        RETURN 0

END NAMESPACE 

