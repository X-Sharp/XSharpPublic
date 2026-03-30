//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Collections.Generic
USING System.IO
USING System.Reflection
USING System.Linq
USING System.Text


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbAlias/*" />
FUNCTION VoDbAlias(wWorkArea AS DWORD) AS STRING
    RETURN VoDb.Alias(wWorkArea)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbAliasSym/*" />
FUNCTION VoDbAliasSym(wWorkArea AS DWORD) AS SYMBOL
    RETURN AsSymbol(VoDb.Alias(wWorkArea))


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbAppend/*" />
FUNCTION VoDbAppend(lReleaseLocks AS LOGIC) AS LOGIC
    RETURN VoDb.Append(lReleaseLocks)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbBof/*" />
FUNCTION VoDbBof() AS LOGIC
    RETURN VoDb.Bof()

// <inheritdoc cref="CoreDb.BlobInfo"  />
/// <remarks> <inheritdoc cref="CoreDb.BlobInfo" />
/// <br/> <br/> <note type="tip">The difference between VoDbBlobInfo and CoreDb.BlobInfo is that VoDbBlobInfo takes a USUAL parameter.</note></remarks>
// <seealso cref="CoreDb.BlobInfo"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    RETURN VoDb.BlobInfo(nOrdinal, nPos, REF ptrRet)

/// <inheritdoc cref="CoreDb.BlobInfo"  />
/// <remarks> <inheritdoc cref="CoreDb.BlobInfo" />
/// <br/> <br/> <note type="tip">The difference between VoDbBlobInfo and CoreDb.BlobInfo is that VoDbBlobInfo takes a USUAL parameter.</note></remarks>
// <seealso cref="CoreDb.BlobInfo"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.BlobInfo(nOrdinal, nPos,  uValue)



/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbBuffRefresh/*" />
FUNCTION VoDbBuffRefresh() AS LOGIC
    RETURN VoDb.BuffRefresh()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbClearFilter/*" />
FUNCTION VoDbClearFilter() AS LOGIC
    RETURN VoDb.ClearFilter()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbClearLocate/*" />
FUNCTION VoDbClearLocate() AS LOGIC
    RETURN VoDb.ClearLocate()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbClearRelation/*" />
FUNCTION VoDbClearRelation() AS LOGIC
    RETURN VoDb.ClearRelation()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbClearScope/*" />
FUNCTION VoDbClearScope() AS LOGIC
    RETURN VoDb.ClearScope()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCloseAll/*" />
FUNCTION VoDbCloseAll() AS LOGIC
    RETURN VoDb.CloseAll()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCloseArea/*" />
FUNCTION VoDbCloseArea() AS LOGIC
    RETURN VoDb.CloseArea()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCommit/*" />
FUNCTION VoDbCommit() AS LOGIC
    RETURN VoDb.Commit()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCommitAll/*" />
FUNCTION VoDbCommitAll() AS LOGIC
   RETURN VoDb.CommitAll()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbContinue/*" />
FUNCTION VoDbContinue() AS LOGIC
    RETURN VoDb.Continue()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCreate/*" />
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), cRddName, lNew, cAlias, cDelim, lKeep, lJustOpen)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCreate/*" />
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, oRddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), oRddType, lNew, cAlias, cDelim, lKeep, lJustOpen)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbCreate/*" />
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, aList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), aList, lNew, cAlias, cDelim, lKeep, lJustOpen)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbDelete/*" />
FUNCTION VoDbDelete() AS LOGIC
    RETURN VoDb.Delete()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbDeleted/*" />
FUNCTION VoDbDeleted() AS LOGIC
     RETURN VoDb.Deleted()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbEof/*" />
FUNCTION VoDbEof() AS LOGIC
     RETURN VoDb.Eof()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbEval/*" />
FUNCTION VoDbEval(cbExecute AS USUAL,cbForCondition AS USUAL,cbWhileCondition AS USUAL,nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    cbExecute           := VoDb.ValidBlock(cbExecute, {||NIL})
    cbForCondition      := VoDb.ValidBlock(cbForCondition)
    cbWhileCondition    := VoDb.ValidBlock(cbWhileCondition)
    RETURN VoDb.Eval(VoDb.ValidBlock(cbExecute), cbForCondition, cbWhileCondition, nNext, nRecord, lRest)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldGet/*" />
FUNCTION VoDbFieldGet(wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.FieldGet(wFieldPos, REF ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldGetBytes/*" />
FUNCTION VoDbFieldGetBytes(wFieldPos AS DWORD,ptrRetVal REF BYTE[]) AS LOGIC
    RETURN VoDb.FieldGetBytes(wFieldPos, REF ptrRetVal)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldInfo/*" />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
   RETURN VoDb.FieldInfo(kInfoType, wFieldPos, REF ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldInfo/*" />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldInfo(kInfoType, wFieldPos, uValue)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldPut/*" />
FUNCTION VoDbFieldPut(wFieldPos AS DWORD,uNewValue IN USUAL) AS LOGIC
    RETURN VoDb.FieldPut(wFieldPos, uNewValue)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFieldPutBytes/*" />
FUNCTION VoDbFieldPutBytes(wFieldPos AS DWORD,uNewValue AS BYTE[]) AS LOGIC
    RETURN VoDb.FieldPutBytes(wFieldPos, uNewValue)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFileGet/*" />
FUNCTION VoDbFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FileGet(nPos, cFile)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFilePut/*" />
FUNCTION VoDbFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FilePut(nPos, cFile)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFilter/*" />
FUNCTION VoDbFilter() AS STRING
    RETURN VoDb.Filter()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFlock/*" />
FUNCTION VoDbFlock() AS LOGIC
    RETURN VoDb.Flock()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbFound/*" />
FUNCTION VoDbFound() AS LOGIC
    RETURN VoDb.Found()


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGetSelect/*" />
FUNCTION VoDbGetSelect() AS DWORD
    RETURN VoDb.GetSelect()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetselect1/*" />
FUNCTION VoDbGetSelect(sArea AS STRING) AS DWORD
    RETURN XSharp.RuntimeState.Workareas:FindAlias(sArea)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGoBottom/*" />
FUNCTION VoDbGoBottom() AS LOGIC
    RETURN VoDb.GoBottom()


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGoto/*" />
FUNCTION VoDbGoto(uRecId IN USUAL) AS LOGIC
    RETURN VoDb.GoToId(uRecId)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGotoId/*" />
FUNCTION VoDbGotoId(uRecId IN USUAL) AS LOGIC
    RETURN VoDb.GoToId(uRecId)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGoTop/*" />
FUNCTION VoDbGoTop() AS LOGIC
    RETURN VoDb.GoTop()


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbInfo/*" />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, REF ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbInfo/*" />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, ptrRetVal)



/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbJoinAppend/*" />
FUNCTION VoDbJoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
    RETURN VoDb.JoinAppend(nSelect, struList )


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbLastRec/*" />
FUNCTION VoDbLastRec() AS DWORD
    RETURN VoDb.LastRec()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbLocate/*" />
FUNCTION VoDbLocate(cbForCondition AS USUAL,cbWhileCondition AS USUAL,liNext AS DWORD,uRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Locate(VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), liNext, uRecord, lRest)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbMemoExt/*" />
FUNCTION VoDbMemoExt(cDriver AS STRING) AS STRING
    RETURN VoDb.MemoExt(cDriver)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdBagExt/*" />
FUNCTION VoDbOrdBagExt() AS STRING
    RETURN VoDb.OrdBagExt()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdCondSet/*" />
FUNCTION VoDbOrdCondSet(ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCondSet(ptrCondInfo)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdCreate/*" />
FUNCTION VoDbOrdCreate(cIndexFile AS STRING,cOrder AS USUAL,cKeyValue AS STRING,cbKeyValue AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCreate(cIndexFile, cOrder, cKeyValue, VoDb.ValidBlock(cbKeyValue), lUnique, ptrCondInfo)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdDestroy/*" />
FUNCTION VoDbOrdDestroy(cIndexFile AS STRING,uOrder IN USUAL) AS LOGIC
    RETURN VoDb.OrdDestroy(cIndexFile, uOrder)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrderInfo/*" />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder IN USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile, uOrder, REF ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrderInfo/*" />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder AS USUAL,uValue AS USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile,  uOrder, uValue)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdListAdd/*" />
FUNCTION VoDbOrdListAdd(cIndexFile AS STRING,uOrder IN USUAL) AS LOGIC
    RETURN VoDb.OrdListAdd(cIndexFile, uOrder)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdListClear/*" />
FUNCTION VoDbOrdListClear(cIndexFile AS STRING,uOrder IN USUAL) AS LOGIC
    RETURN VoDb.OrdListClear(cIndexFile, uOrder)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdListRebuild/*" />
FUNCTION VoDbOrdListRebuild() AS LOGIC
    RETURN VoDb.OrdListRebuild()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbOrdSetFocus/*" />
FUNCTION VoDbOrdSetFocus(cIndexFile AS STRING,uOrder IN USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDb.OrdSetFocus(cIndexFile,  uOrder, OUT cOrder)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbPack/*" />
FUNCTION VoDbPack() AS LOGIC
    RETURN VoDb.Pack()

[Obsolete( "'VoDbRddCount( nRddType )' is not supported, use VoDbRddCount() instead", TRUE )];
FUNCTION VoDbRddCount(nRddType AS DWORD) AS DWORD
    RETURN 0

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddCount/*" />
FUNCTION VoDbRddCount() AS DWORD
    RETURN VoDb.RddCount()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddInfo/*" />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, REF uRet)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddInfo/*" />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, uValue)


[Obsolete( "'VoDbRddList( rddList, nRddType )' is not supported, use VoDbRddList() instead", TRUE )];
FUNCTION VoDbRddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddList/*" />
FUNCTION VoDbRddList() AS STRING[]
  RETURN VoDb.RddList()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddName/*" />
FUNCTION VoDbRddName() AS STRING
    RETURN VoDb.RddName()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRddSetDefault/*" />
FUNCTION VoDbRddSetDefault(cNewSetting AS STRING) AS STRING
    RETURN VoDb.RddSetDefault(cNewSetting)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecall/*" />
FUNCTION VoDbRecall() AS LOGIC
    RETURN VoDb.Recall()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecno/*" />
FUNCTION VoDbRecno() AS DWORD
    RETURN VoDb.Recno()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecordGet/*" />
FUNCTION VoDbRecordGet() AS BYTE[]
    RETURN VoDb.RecordGet()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecordInfo/*" />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID IN USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, REF ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecordInfo/*" />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID IN USUAL,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, ptrRetVal)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecordPut/*" />
FUNCTION VoDbRecordPut(aRecord AS BYTE[]) AS LOGIC
    RETURN VoDb.RecordPut(aRecord)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRecordPut/*" />
FUNCTION VoDbRecordPut(aRecord AS BYTE PTR) AS LOGIC
    LOCAL uSize := NIL as USUAL
    VoDb.Info(DBI_GETRECSIZE, REF uSize)
    LOCAL nRecSize := uSize AS LONG
    LOCAL aRecordBuf AS BYTE[]
    aRecordBuf := BYTE[]{ nRecSize }
    FixedMemory.Copy(@aRecordBuf, aRecord, nRecSize)
    RETURN VoDb.RecordPut(aRecordBuf)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRelation/*" />
FUNCTION VoDbRelation(wRelation AS DWORD, pszRelation REF STRING) AS LOGIC
    RETURN VoDb.Relation(wRelation, REF pszRelation)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRlock/*" />
FUNCTION VoDbRlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.RLock(uRecId)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSeek/*" />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC, lLast AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( uKey, lSoftSeek, lLast)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSeek/*" />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC) AS LOGIC
    LOCAL oScope := VoDbGetScope() AS DbScopeInfo
    RETURN VoDb.Seek( uKey, lSoftSeek, oScope:Last)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbRSelect/*" />
FUNCTION VoDbRSelect(wRelation AS DWORD) AS DWORD
    RETURN VoDb.RSelect(wRelation)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSelect/*" />
FUNCTION VoDbSelect(wNew AS DWORD,wOld OUT USUAL) AS LOGIC
    RETURN VoDb.Select(wNew, OUT wOld)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSelect/*" />
FUNCTION VoDbSelect(wNew AS DWORD,wOld OUT DWORD ) AS LOGIC
  RETURN VoDb.Select(wNew, OUT wOld)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetFilter/*" />
FUNCTION VoDbSetFilter(cbCondition IN USUAL,cCondition AS STRING) AS LOGIC
    RETURN VoDb.SetFilter(VoDb.ValidBlock(cbCondition), cCondition)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetFound/*" />
FUNCTION VoDbSetFound(lFound AS LOGIC) AS LOGIC
    RETURN VoDb.SetFound(lFound)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetLocate/*" />
FUNCTION VoDbSetLocate(cbForCondition IN USUAL) AS LOGIC
    RETURN VoDb.SetLocate(VoDb.ValidBlock(cbForCondition))

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetRelation/*" />
FUNCTION VoDbSetRelation(cAlias AS STRING,cbRel IN USUAL,cRel AS STRING, cName := "" AS STRING) AS LOGIC
    RETURN VoDb.SetRelation(cAlias, VoDb.ValidBlock(cbRel), cRel, cName)


/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbGetScope/*" />
FUNCTION VoDbGetScope() AS DbScopeInfo
    RETURN VoDb.GetScope()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetScope/*" />
FUNCTION VoDbSetScope(scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.SetScope(scope)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSetSelect/*" />
FUNCTION VoDbSetSelect(siNewArea AS INT) AS DWORD
    RETURN VoDb.SetSelect(siNewArea)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSkip/*" />
FUNCTION VoDbSkip(liRecords AS LONG) AS LOGIC
    RETURN VoDb.Skip(liRecords)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSkipScope/*" />
FUNCTION VoDbSkipScope(nRecords AS LONG,scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.SkipScope(nRecords, scope)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSort/*" />
FUNCTION VoDbSort(nDest AS DWORD,fldNames AS _FieldNames,cbForCondition IN USUAL,cbWhileCondition IN USUAL, nNext IN USUAL,nRecord IN USUAL,lRest AS LOGIC,fnSortNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Sort(nDest, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest, fnSortNames)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbSymSelect/*" />
FUNCTION VoDbSymSelect(symAlias AS SYMBOL) AS INT
    RETURN VoDb.SymSelect(symAlias)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbTrans/*" />
FUNCTION VoDbTrans(wTarget AS DWORD,fldNames AS _FieldNames,cbForCondition IN USUAL,cbWhileCondition IN USUAL, nNext IN USUAL,nRecord IN USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Trans(wTarget, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbTransRec/*" />
FUNCTION VoDbTransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
    RETURN VoDb.TransRec(nDest, fldNames)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbUnlock/*" />
FUNCTION VoDbUnlock(uRecId IN USUAL) AS LOGIC
    RETURN VoDb.Unlock( uRecId)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbUnLockAll/*" />
FUNCTION VoDbUnLockAll() AS LOGIC
    RETURN VoDb.UnlockAll()

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbUseArea/*" />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddlist AS _RddList,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddlist, cDataFile, cAlias, lShared, lReadOnly)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbUseArea/*" />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddName AS STRING,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddName, cDataFile, cAlias, lShared, lReadOnly)

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbUseArea/*" />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddType AS System.Type,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea ,rddType ,cDataFile ,cAlias ,lShared ,lReadOnly )

/// <include file="XSharp.RT.Docs.xml" path="doc/VoDbZap/*" />
FUNCTION VoDbZap() AS LOGIC
    RETURN VoDb.Zap()

/// <include file="XSharp.RT.Docs.xml" path="doc/_VoDbErrInfoPtr/*" />
FUNCTION _VoDbErrInfoPtr AS Exception
    RETURN VoDb._ErrInfoPtr()

/// <exclude />
FUNCTION __AllocRddList(aRDDs AS ARRAY) AS _RddList
    LOCAL aNames AS List<System.String>
    aNames := List<System.String>{}
    FOREACH rdd AS USUAL IN aRDDs
        aNames:Add( (System.String) rdd)
    NEXT
    RETURN _RddList{aNames:ToArray()}
