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

/// These functions all map to the static methods inside VoDb (and CoreDb)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbalias/*" />
/// <seealso cref="CoreDb.Alias"  />
FUNCTION VoDbAlias(wWorkArea AS DWORD) AS STRING
    RETURN VoDb.Alias(wWorkArea)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbaliassym/*" />
/// <seealso cref="CoreDb.Alias"  />
FUNCTION VoDbAliasSym(wWorkArea AS DWORD) AS SYMBOL
    RETURN AsSymbol(VoDb.Alias(wWorkArea))


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbappend/*" />
/// <seealso cref="CoreDb.Append"  />
FUNCTION VoDbAppend(lReleaseLocks AS LOGIC) AS LOGIC
    RETURN VoDb.Append(lReleaseLocks)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbbof/*" />
/// <seealso cref="CoreDb.Bof"  />
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



/// <inheritdoc cref="CoreDb.BuffRefresh"  />
/// <seealso cref="CoreDb.BuffRefresh"  />
FUNCTION VoDbBuffRefresh() AS LOGIC
    RETURN VoDb.BuffRefresh()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearfilter/*" />
/// <seealso cref="CoreDb.ClearFilter"  />
FUNCTION VoDbClearFilter() AS LOGIC
    RETURN VoDb.ClearFilter()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearlocate/*" />
/// <seealso cref="CoreDb.ClearLocate"  />
FUNCTION VoDbClearLocate() AS LOGIC
    RETURN VoDb.ClearLocate()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearrelation/*" />
/// <seealso cref="CoreDb.ClearRelation"  />
FUNCTION VoDbClearRelation() AS LOGIC
    RETURN VoDb.ClearRelation()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearscope/*" />
/// <seealso cref="CoreDb.ClearScope"  />
FUNCTION VoDbClearScope() AS LOGIC
    RETURN VoDb.ClearScope()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcloseall/*" />
/// <seealso cref="CoreDb.CloseAll"  />
FUNCTION VoDbCloseAll() AS LOGIC
    RETURN VoDb.CloseAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclosearea/*" />
/// <seealso cref="CoreDb.CloseArea"  />
/// <remarks> <inheritdoc cref="CoreDb.CloseArea"  />
/// <br/> <br/> <note type="tip">VoDbCloseArea() is an alias for CoreDbCloseArea()</note></remarks>
FUNCTION VoDbCloseArea() AS LOGIC
    RETURN VoDb.CloseArea()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcommit/*" />
/// <seealso cref="CoreDb.Commit"  />
FUNCTION VoDbCommit() AS LOGIC
    RETURN VoDb.Commit()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcommitall/*" />
/// <seealso cref="CoreDb.CommitAll"  />
FUNCTION VoDbCommitAll() AS LOGIC
   RETURN VoDb.CommitAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcontinue/*" />
/// <seealso cref="CoreDb.Continue"  />
FUNCTION VoDbContinue() AS LOGIC
    RETURN VoDb.Continue()

/// <inheritdoc cref="CoreDb.Create" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <remarks> <inheritdoc cref="CoreDb.Create" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), cRddName, lNew, cAlias, cDelim, lKeep, lJustOpen)

/// <inheritdoc cref="CoreDb.Create" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="oRddType">Type of the RDDs to use when creating the file</param>
/// <remarks> <inheritdoc cref="CoreDb.Create" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, oRddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), oRddType, lNew, cAlias, cDelim, lKeep, lJustOpen)

/// <inheritdoc cref="CoreDb.Create" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="aList">structure that describes the list of RDDs to use</param>
/// <remarks> <inheritdoc cref="CoreDb.Create" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, aList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), aList, lNew, cAlias, cDelim, lKeep, lJustOpen)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbdelete/*" />
/// <seealso cref="CoreDb.Delete"  />
FUNCTION VoDbDelete() AS LOGIC
    RETURN VoDb.Delete()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbdeleted/*" />
/// <seealso cref="CoreDb.Deleted"  />
FUNCTION VoDbDeleted() AS LOGIC
     RETURN VoDb.Deleted()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbeof/*" />
/// <seealso cref="CoreDb.Eof"  />
FUNCTION VoDbEof() AS LOGIC
     RETURN VoDb.Eof()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbeval/*" />
/// <seealso cref="CoreDb.Eval"  />
FUNCTION VoDbEval(cbExecute AS USUAL,cbForCondition AS USUAL,cbWhileCondition AS USUAL,nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    cbExecute           := VoDb.ValidBlock(cbExecute, {||NIL})
    cbForCondition      := VoDb.ValidBlock(cbForCondition)
    cbWhileCondition    := VoDb.ValidBlock(cbWhileCondition)
    RETURN VoDb.Eval(VoDb.ValidBlock(cbExecute), cbForCondition, cbWhileCondition, nNext, nRecord, lRest)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldget/*" />
/// <seealso cref="CoreDb.FieldGet"  />
FUNCTION VoDbFieldGet(wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.FieldGet(wFieldPos, REF ptrRetVal)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldget/*" />
/// <seealso cref="CoreDb.FieldGet"  />
FUNCTION VoDbFieldGetBytes(wFieldPos AS DWORD,ptrRetVal REF BYTE[]) AS LOGIC
    RETURN VoDb.FieldGetBytes(wFieldPos, REF ptrRetVal)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldinfo/*" />
/// <seealso cref="CoreDb.FieldInfo"  />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
   RETURN VoDb.FieldInfo(kInfoType, wFieldPos, REF ptrRetVal)

/// <inheritdoc cref="VoDbFieldInfo" />
/// <param name="uValue">New value to assign to the Field.</param>
/// <seealso cref="CoreDb.FieldInfo"  />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldInfo(kInfoType, wFieldPos, uValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldput/*" />
/// <seealso cref="CoreDb.FieldPut"  />
FUNCTION VoDbFieldPut(wFieldPos AS DWORD,uNewValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldPut(wFieldPos, uNewValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldput/*" />
/// <seealso cref="CoreDb.FieldPut"  />
FUNCTION VoDbFieldPutBytes(wFieldPos AS DWORD,uNewValue AS BYTE[]) AS LOGIC
    RETURN VoDb.FieldPutBytes(wFieldPos, uNewValue)


/// <inheritdoc cref="CoreDb.FileGet"  />
/// <seealso cref="CoreDb.FileGet"  />
FUNCTION VoDbFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FileGet(nPos, cFile)

/// <inheritdoc cref="CoreDb.FilePut"  />
/// <seealso cref="CoreDb.FilePut"  />
FUNCTION VoDbFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FilePut(nPos, cFile)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfilter/*" />
/// <seealso cref="CoreDb.Filter"  />
FUNCTION VoDbFilter() AS STRING
    RETURN VoDb.Filter()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbflock/*" />
/// <seealso cref="CoreDb.Flock"  />
FUNCTION VoDbFlock() AS LOGIC
    RETURN VoDb.Flock()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfound/*" />
/// <seealso cref="CoreDb.Found"  />
FUNCTION VoDbFound() AS LOGIC
    RETURN VoDb.Found()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetselect/*" />
/// <seealso cref="CoreDb.GetSelect"  />
FUNCTION VoDbGetSelect() AS DWORD
    RETURN VoDb.GetSelect()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetselect1/*" />
FUNCTION VoDbGetSelect(sArea AS STRING) AS DWORD
    RETURN XSharp.RuntimeState.Workareas:FindAlias(sArea)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgobottom/*" />
/// <seealso cref="CoreDb.GoBottom"  />
FUNCTION VoDbGoBottom() AS LOGIC
    RETURN VoDb.GoBottom()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgoto/*" />
/// <seealso cref="CoreDb.Goto"  />
FUNCTION VoDbGoto(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.Goto(uRecId)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgotop/*" />
/// <seealso cref="CoreDb.GoTop"  />
FUNCTION VoDbGoTop() AS LOGIC
    RETURN VoDb.GoTop()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbinfo/*" />
/// <seealso cref="CoreDb.Info"  />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, REF ptrRetVal)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbinfo/*" />
/// <seealso cref="CoreDb.Info"  />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, ptrRetVal)



/// <inheritdoc cref="CoreDb.JoinAppend"  />
/// <seealso cref="CoreDb.JoinAppend"  />
FUNCTION VoDbJoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
    RETURN VoDb.JoinAppend(nSelect, struList )


/// <inheritdoc cref="CoreDb.LastRec"  />
/// <seealso cref="CoreDb.LastRec"  />
FUNCTION VoDbLastRec() AS LONG
    RETURN VoDb.LastRec()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodblocate/*" />
/// <seealso cref="CoreDb.Locate"  />
FUNCTION VoDbLocate(cbForCondition AS USUAL,cbWhileCondition AS USUAL,liNext AS LONG,uRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Locate(VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), liNext, uRecord, lRest)


/// <inheritdoc cref="CoreDb.MemoExt"  />
/// <seealso cref="CoreDb.MemoExt"  />
FUNCTION VoDbMemoExt(cDriver AS STRING) AS STRING
    RETURN VoDb.MemoExt(cDriver)

/// <inheritdoc cref="CoreDb.OrdBagExt"  />
/// <seealso cref="CoreDb.OrdBagExt"  />
FUNCTION VoDbOrdBagExt() AS STRING
    RETURN VoDb.OrdBagExt()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordcondset/*" />
/// <seealso cref="CoreDb.OrdCondSet"  />
FUNCTION VoDbOrdCondSet(ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCondSet(ptrCondInfo)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreateorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordcreate/*" />
/// <seealso cref="CoreDb.OrdCreate"  />
FUNCTION VoDbOrdCreate(cIndexFile AS STRING,cOrder AS USUAL,cKeyValue AS STRING,cbKeyValue AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCreate(cIndexFile, cOrder, cKeyValue, VoDb.ValidBlock(cbKeyValue), lUnique, ptrCondInfo)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdeleteorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodborddestroy/*" />
/// <seealso cref="CoreDb.OrdDestroy"  />
FUNCTION VoDbOrdDestroy(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdDestroy(cIndexFile, uOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodborderinfo/*" />
/// <seealso cref="CoreDb.OrderInfo"  />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder AS USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile, uOrder, REF ptrRetVal)

/// <inheritdoc cref="VoDbOrderInfo" />/>
/// <param name="uValue">New value to assign to the Order.</param>
/// <seealso cref="CoreDb.OrderInfo"  />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder AS USUAL,uValue AS USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile,  uOrder, uValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistadd/*" />
/// <seealso cref="CoreDb.OrdListAdd"  />
FUNCTION VoDbOrdListAdd(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListAdd(cIndexFile, uOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistclear/*" />
/// <seealso cref="CoreDb.OrdListClear"  />
FUNCTION VoDbOrdListClear(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListClear(cIndexFile, uOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbreindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistrebuild/*" />
/// <seealso cref="CoreDb.OrdListRebuild"  />
FUNCTION VoDbOrdListRebuild() AS LOGIC
    RETURN VoDb.OrdListRebuild()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordsetfocus/*" />
/// <param name="cOrder">This returns the previous order that was selected.</param>
/// <seealso cref="CoreDb.OrdSetFocus"  />
FUNCTION VoDbOrdSetFocus(cIndexFile AS STRING,uOrder AS USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDb.OrdSetFocus(cIndexFile,  uOrder, OUT cOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbpack/*" />
/// <seealso cref="CoreDb.Pack"  />
FUNCTION VoDbPack() AS LOGIC
    RETURN VoDb.Pack()

[Obsolete( "'VoDbRddCount( nRddType )' is not supported, use VoDbRddCount() instead", TRUE )];
FUNCTION VoDbRddCount(nRddType AS DWORD) AS DWORD
    RETURN 0

/// <inheritdoc cref="CoreDb.RddCount"  />
/// <seealso cref="CoreDb.RddCount"  />
FUNCTION VoDbRddCount() AS DWORD
    RETURN VoDb.RddCount()

/// <inheritdoc cref="CoreDb.RddInfo"  />
/// <remarks> <inheritdoc cref="CoreDb.RddInfo"  />
/// <br/><br/> <note type="tip">The difference between VoDbRddInfo and CoreDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="CoreDb.RddInfo"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, REF uRet)

/// <inheritdoc cref="CoreDb.RddInfo"  />
/// <remarks> <inheritdoc cref="CoreDb.RddInfo"  />
/// <br/><br/> <note type="tip">The difference between VoDbRddInfo and CoreDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="CoreDb.RddInfo"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, uValue)


[Obsolete( "'VoDbRddList( rddList, nRddType )' is not supported, use VoDbRddList() instead", TRUE )];
FUNCTION VoDbRddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}

/// <inheritdoc cref="CoreDb.RddList"  />
/// <seealso cref="CoreDb.RddList"  />
FUNCTION VoDbRddList() AS STRING[]
  RETURN VoDb.RddList()

/// <inheritdoc cref="CoreDb.RddName"  />
/// <seealso cref="CoreDb.RddName"  />
FUNCTION VoDbRddName() AS STRING
    RETURN VoDb.RddName()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrddsetdefault/*" />
/// <seealso cref="CoreDb.RddSetDefault"  />
FUNCTION VoDbRddSetDefault(cNewSetting AS STRING) AS STRING
    RETURN VoDb.RddSetDefault(cNewSetting)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecall/*" />
/// <seealso cref="CoreDb.Recall"  />
FUNCTION VoDbRecall() AS LOGIC
    RETURN VoDb.Recall()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecno/*" />
/// <seealso cref="CoreDb.Recno"  />
FUNCTION VoDbRecno() AS DWORD
    RETURN VoDb.Recno()

/// <inheritdoc cref="CoreDb.RecordGet"  />
/// <seealso cref="CoreDb.RecordGet"  />
FUNCTION VoDbRecordGet() AS BYTE[]
    RETURN VoDb.RecordGet()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecordinfo/*" />
/// <seealso cref="CoreDb.RecordInfo"  />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID AS USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, REF ptrRetVal)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecordinfo/*" />
/// <seealso cref="CoreDb.RecordInfo"  />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID AS USUAL,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, ptrRetVal)

/// <inheritdoc cref="CoreDb.RecordPut"  />
/// <seealso cref="CoreDb.RecordPut"  />
FUNCTION VoDbRecordPut(aRecord AS BYTE[]) AS LOGIC
    RETURN VoDb.RecordPut(aRecord)

/// <inheritdoc cref="CoreDb.RecordPut"  />
/// <seealso cref="CoreDb.RecordPut"  />
FUNCTION VoDbRecordPut(aRecord AS BYTE PTR) AS LOGIC
    LOCAL uSize := NIL as USUAL
    VoDb.Info(DBI_GETRECSIZE, REF uSize)
    LOCAL nRecSize := uSize AS LONG
    LOCAL aRecordBuf AS BYTE[]
    aRecordBuf := BYTE[]{ nRecSize }
    FixedMemory.Copy(@aRecordBuf, aRecord, nRecSize)
    RETURN VoDb.RecordPut(aRecordBuf)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrelation/*" />
/// <seealso cref="CoreDb.Relation"  />
FUNCTION VoDbRelation(wRelation AS DWORD, pszRelation REF STRING) AS LOGIC
    RETURN VoDb.Relation(wRelation, REF pszRelation)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrlock/*" />
/// <seealso cref="CoreDb.RLock"  />
FUNCTION VoDbRlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.RLock(uRecId)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbseek/*" />
/// <seealso cref="CoreDb.Seek"  />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC, lLast AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( uKey, lSoftSeek, lLast)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbseek/*" />
/// <seealso cref="CoreDb.Seek"  />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC) AS LOGIC
    LOCAL oScope := VoDbGetScope() AS DbScopeInfo
    RETURN VoDb.Seek( uKey, lSoftSeek, oScope:Last)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrselect/*" />
/// <seealso cref="CoreDb.RSelect"  />
FUNCTION VoDbRSelect(wRelation AS DWORD) AS DWORD
    RETURN VoDb.RSelect(wRelation)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbselect/*" />
/// <seealso cref="CoreDb.Select"  />
FUNCTION VoDbSelect(wNew AS DWORD,wOld OUT USUAL) AS LOGIC
    RETURN VoDb.Select(wNew, OUT wOld)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbselect/*" />
/// <seealso cref="CoreDb.Select"  />
FUNCTION VoDbSelect(wNew AS DWORD,wOld OUT DWORD ) AS LOGIC
  RETURN VoDb.Select(wNew, OUT wOld)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetfilter/*" />
/// <seealso cref="CoreDb.SetFilter"  />
FUNCTION VoDbSetFilter(cbCondition AS USUAL,cCondition AS STRING) AS LOGIC
    RETURN VoDb.SetFilter(VoDb.ValidBlock(cbCondition), cCondition)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetfound/*" />
/// <seealso cref="CoreDb.SetFound"  />
FUNCTION VoDbSetFound(lFound AS LOGIC) AS LOGIC
    RETURN VoDb.SetFound(lFound)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetlocate/*" />
/// <seealso cref="CoreDb.SetLocate"  />
FUNCTION VoDbSetLocate(cbForCondition AS USUAL) AS LOGIC
    RETURN VoDb.SetLocate(VoDb.ValidBlock(cbForCondition))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetrelation/*" />
/// <seealso cref="CoreDb.SetRelation"  />
FUNCTION VoDbSetRelation(cAlias AS STRING,cbRel AS USUAL,cRel AS STRING, cName := "" AS STRING) AS LOGIC
    RETURN VoDb.SetRelation(cAlias, VoDb.ValidBlock(cbRel), cRel, cName)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetscope/*" />
/// <seealso cref="CoreDb.SetScope"  />
/// <seealso cref="VoDbSetScope"  />
FUNCTION VoDbGetScope() AS DbScopeInfo
    RETURN VoDb.GetScope()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetscope/*" />
/// <seealso cref="CoreDb.SetScope"  />
/// <seealso cref="VoDbGetScope"  />
FUNCTION VoDbSetScope(scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.SetScope(scope)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetselect/*" />
/// <seealso cref="CoreDb.SetSelect"  />
FUNCTION VoDbSetSelect(siNewArea AS INT) AS DWORD
    RETURN VoDb.SetSelect(siNewArea)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbskip/*" />
/// <seealso cref="CoreDb.Skip"  />
FUNCTION VoDbSkip(liRecords AS LONG) AS LOGIC
    RETURN VoDb.Skip(liRecords)

/// <inheritdoc cref="CoreDb.SkipScope"  />
/// <seealso cref="CoreDb.SkipScope"  />
FUNCTION VoDbSkipScope(nRecords AS LONG,scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.SkipScope(nRecords, scope)

/// <param name="fldNames">List of field names to copy</param>
/// <param name="fnSortNames">List of field names to sort on</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsort/*" />
/// <seealso cref="CoreDb.Sort"  />
FUNCTION VoDbSort(nDest AS DWORD,fldNames AS _FieldNames,cbForCondition AS USUAL,cbWhileCondition AS USUAL, nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC,fnSortNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Sort(nDest, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest, fnSortNames)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsymselect/*" />
/// <seealso cref="CoreDb.SymSelect"/>
FUNCTION VoDbSymSelect(symAlias AS SYMBOL) AS INT
    RETURN VoDb.SymSelect(symAlias)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbtrans/*" />
/// <seealso cref="CoreDb.Trans"  />
/// <param name="fldNames">List of field names to copy </param>
FUNCTION VoDbTrans(wTarget AS DWORD,fldNames AS _FieldNames,cbForCondition AS USUAL,cbWhileCondition AS USUAL, nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Trans(wTarget, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest)

/// <inheritdoc cref="CoreDb.TransRec"  />
/// <seealso cref="CoreDb.TransRec"  />
FUNCTION VoDbTransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
    RETURN VoDb.TransRec(nDest, fldNames)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbunlock/*" />
/// <remarks> <inheritdoc cref="CoreDb.Unlock"  />
/// <br/><br/> <note type="tip">The difference between VoDbUnlock and CoreDb.UnLock is that VoDbUnlock takes USUAL parameters</note></remarks>
FUNCTION VoDbUnlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.Unlock( uRecId)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbunlockall/*" />
/// <seealso cref="CoreDb.UnlockAll"  />
FUNCTION VoDbUnLockAll() AS LOGIC
    RETURN VoDb.UnlockAll()

/// <overloads>
/// <summary>
/// Open a file
/// </summary>
/// <seealso cref="CoreDb.UseArea" />
/// </overloads>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" />
/// <seealso cref="CoreDb.UseArea"  />
/// <param name="rddlist">List of RDDs to use.</param>
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddlist AS _RddList,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddlist, cDataFile, cAlias, lShared, lReadOnly)

/// <param name="rddName">Name of the RDD to use.</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" />
/// <seealso cref="CoreDb.UseArea"  />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddName AS STRING,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddName, cDataFile, cAlias, lShared, lReadOnly)

/// <param name="rddType">Type of the RDD to use.</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" />
/// <seealso cref="CoreDb.UseArea"  />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddType AS System.Type,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea ,rddType ,cDataFile ,cAlias ,lShared ,lReadOnly )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbzap/*" />
/// <seealso cref="CoreDb.Zap"  />
FUNCTION VoDbZap() AS LOGIC
    RETURN VoDb.Zap()

/// <inheritdoc cref="CoreDb._ErrInfoPtr"  />
/// <seealso cref="CoreDb._ErrInfoPtr"  />
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
