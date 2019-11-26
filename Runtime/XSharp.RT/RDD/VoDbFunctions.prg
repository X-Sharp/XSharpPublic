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
/// <seealso cref="M:XSharp.CoreDb.Alias(System.UInt32)"  />
FUNCTION VoDbAlias(wWorkArea AS DWORD) AS STRING
    RETURN VoDb.Alias(wWorkArea)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbaliassym/*" />
/// <seealso cref="M:XSharp.CoreDb.Alias(System.UInt32)"  />
FUNCTION VoDbAliasSym(wWorkArea AS DWORD) AS SYMBOL
    RETURN AsSymbol(VoDb.Alias(wWorkArea))
  

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbappend/*" />
/// <seealso cref="M:XSharp.CoreDb.Append(System.Boolean)"  />
FUNCTION VoDbAppend(lReleaseLocks AS LOGIC) AS LOGIC
    RETURN VoDb.Append(lReleaseLocks)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbbof/*" />
/// <seealso cref="M:XSharp.CoreDb.Bof"  />
FUNCTION VoDbBof() AS LOGIC
    RETURN VoDb.Bof()

// <inheritdoc cref="M:XSharp.CoreDb.BlobInfo(System.UInt32,System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.BlobInfo(System.UInt32,System.UInt32,System.Object@)" /> 
/// <br/> <br/> <note type="tip">The difference between VoDbBlobInfo and CoreDb.BlobInfo is that VoDbBlobInfo takes a USUAL parameter.</note></remarks>
// <seealso cref="O:XSharp.CoreDb.BlobInfo"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    RETURN VoDb.BlobInfo(nOrdinal, nPos, REF ptrRet)

/// <inheritdoc cref="M:XSharp.CoreDb.BlobInfo(System.UInt32,System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.BlobInfo(System.UInt32,System.UInt32,System.Object@)" /> 
/// <br/> <br/> <note type="tip">The difference between VoDbBlobInfo and CoreDb.BlobInfo is that VoDbBlobInfo takes a USUAL parameter.</note></remarks>
/// <seealso cref="O:XSharp.CoreDb.BlobInfo"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.BlobInfo(nOrdinal, nPos,  uValue)



/// <inheritdoc cref="M:XSharp.CoreDb.BuffRefresh"  />
/// <seealso cref="M:XSharp.CoreDb.BuffRefresh"  />
FUNCTION VoDbBuffRefresh() AS LOGIC
    RETURN VoDb.BuffRefresh()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearfilter/*" />
/// <seealso cref="M:XSharp.CoreDb.ClearFilter"  />    
FUNCTION VoDbClearFilter() AS LOGIC
    RETURN VoDb.ClearFilter()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearlocate/*" />
/// <seealso cref="M:XSharp.CoreDb.ClearLocate"  />    
FUNCTION VoDbClearLocate() AS LOGIC
    RETURN VoDb.ClearLocate()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearrelation/*" />
/// <seealso cref="M:XSharp.CoreDb.ClearRelation"  /> 
FUNCTION VoDbClearRelation() AS LOGIC
    RETURN VoDb.ClearRelation()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclearscope/*" /> 
/// <seealso cref="M:XSharp.CoreDb.ClearScope"  /> 
FUNCTION VoDbClearScope() AS LOGIC
    RETURN VoDb.ClearScope()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcloseall/*" /> 
/// <seealso cref="M:XSharp.CoreDb.CloseAll"  /> 
FUNCTION VoDbCloseAll() AS LOGIC
    RETURN VoDb.CloseAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbclosearea/*" /> 
/// <seealso cref="M:XSharp.CoreDb.CloseArea"  /> 
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.CloseArea"  /> 
/// <br/> <br/> <note type="tip">VoDbCloseArea() is an alias for CoreDbCloseArea()</note></remarks>
FUNCTION VoDbCloseArea() AS LOGIC
    RETURN VoDb.CloseArea()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcommit/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Commit"  /> 
FUNCTION VoDbCommit() AS LOGIC
    RETURN VoDb.Commit()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcommitall/*" /> 
/// <seealso cref="M:XSharp.CoreDb.CommitAll"  /> 
FUNCTION VoDbCommitAll() AS LOGIC
   RETURN VoDb.CommitAll()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbcontinue/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Continue"  /> 
FUNCTION VoDbContinue() AS LOGIC
    RETURN VoDb.Continue()

/// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), cRddName, lNew, cAlias, cDelim, lKeep, lJustOpen)  
     
/// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="oRddType">Type of the RDDs to use when creating the file</param>
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, oRddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), oRddType, lNew, cAlias, cDelim, lKeep, lJustOpen)  

/// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="aList">structure that describes the list of RDDs to use</param>
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <br/> <br/> <note type="tip">The difference between VoDbCreate and CoreDb.Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, aList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), aList, lNew, cAlias, cDelim, lKeep, lJustOpen)  

  
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbdelete/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Delete"  /> 
FUNCTION VoDbDelete() AS LOGIC
    RETURN VoDb.Delete()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbdeleted/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Deleted"  /> 
FUNCTION VoDbDeleted() AS LOGIC
     RETURN VoDb.Deleted()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbeof/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Eof"  /> 
FUNCTION VoDbEof() AS LOGIC
     RETURN VoDb.Eof()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbeval/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Eval(XSharp.ICodeblock,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean)"  />
FUNCTION VoDbEval(cbExecute AS USUAL,cbForCondition AS USUAL,cbWhileCondition AS USUAL,nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Eval(VoDb.ValidBlock(cbExecute), VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldget/*" /> 
/// <seealso cref="M:XSharp.CoreDb.FieldGet(System.UInt32,System.Object@)"  />
FUNCTION VoDbFieldGet(wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.FieldGet(wFieldPos, REF ptrRetVal)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldinfo/*" /> 
/// <seealso cref="O:XSharp.CoreDb.FieldInfo"  />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,ptrRetVal REF USUAL) AS LOGIC
   RETURN VoDb.FieldInfo(kInfoType, wFieldPos, REF ptrRetVal)

/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbFieldInfo(System.UInt32,System.UInt32,XSharp.__Usual@)" />
/// <param name="uValue">New value to assign to the Field.</param>
/// <seealso cref="O:XSharp.CoreDb.FieldInfo"  />
FUNCTION VoDbFieldInfo(kInfoType AS DWORD,wFieldPos AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldInfo(kInfoType, wFieldPos, uValue)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfieldput/*" /> 
/// <seealso cref="M:XSharp.CoreDb.FieldPut(System.UInt32,System.Object)"  />
FUNCTION VoDbFieldPut(wFieldPos AS DWORD,uNewValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldPut(wFieldPos, uNewValue)

/// <inheritdoc cref="M:XSharp.CoreDb.FileGet(System.UInt32,System.String)"  />
/// <seealso cref="M:XSharp.CoreDb.FileGet(System.UInt32,System.String)"  />
FUNCTION VoDbFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FileGet(nPos, cFile)
    
/// <inheritdoc cref="M:XSharp.CoreDb.FilePut(System.UInt32,System.String)"  />
/// <seealso cref="M:XSharp.CoreDb.FilePut(System.UInt32,System.String)"  />
FUNCTION VoDbFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.FilePut(nPos, cFile)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfilter/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Filter"  /> 
FUNCTION VoDbFilter() AS STRING
    RETURN VoDb.Filter()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbflock/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Flock"  /> 
FUNCTION VoDbFlock() AS LOGIC
    RETURN VoDb.Flock()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbfound/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Found"  /> 
FUNCTION VoDbFound() AS LOGIC
    RETURN VoDb.Found()
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetselect/*" /> 
/// <seealso cref="M:XSharp.CoreDb.GetSelect"  /> 
FUNCTION VoDbGetSelect() AS DWORD
    RETURN VoDb.GetSelect()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgetselect1/*" /> 
FUNCTION VODbGetSelect(sArea AS STRING) AS DWORD
    RETURN XSharp.RuntimeState.Workareas:FindAlias(sArea)
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgobottom/*" />    
/// <seealso cref="M:XSharp.CoreDb.GoBottom"  />     
FUNCTION VoDbGoBottom() AS LOGIC
    RETURN VoDb.GoBottom()

    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgoto/*" />     
/// <seealso cref="M:XSharp.CoreDb.Goto(System.Object)"  />
FUNCTION VoDbGoto(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.Goto(uRecID)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbgotop/*" />          
/// <seealso cref="M:XSharp.CoreDb.GoTop"  />        
FUNCTION VoDbGoTop() AS LOGIC
    RETURN VoDb.GoTop()

    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbinfo/*" />  
/// <seealso cref="O:XSharp.CoreDb.Info"  />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, REF ptrRetVal)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbinfo/*" />  
/// <seealso cref="O:XSharp.CoreDb.Info"  />
FUNCTION VoDbInfo(kInfoType AS DWORD,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.Info(kInfoType, ptrRetVal)



/// <inheritdoc cref="M:XSharp.CoreDb.JoinAppend(System.UInt32,XSharp._JoinList)"  />        
/// <seealso cref="M:XSharp.CoreDb.JoinAppend(System.UInt32,XSharp._JoinList)"  />        
FUNCTION VoDbJoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
    RETURN VoDb.JoinAppend(nSelect, struList )
      
      
/// <inheritdoc cref="M:XSharp.CoreDb.LastRec"  />   
/// <seealso cref="M:XSharp.CoreDb.LastRec"  />   
FUNCTION VoDbLastRec() AS LONG
    RETURN VoDb.LastRec() 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodblocate/*" />  
/// <seealso cref="M:XSharp.CoreDb.Locate(XSharp.ICodeblock,XSharp.ICodeblock,System.Int32,System.Object,System.Boolean)"  />
FUNCTION VoDbLocate(cbForCondition AS USUAL,cbWhileCondition AS USUAL,liNext AS LONG,uRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Locate(VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), liNext, uRecord, lRest)
    

/// <inheritdoc cref="M:XSharp.CoreDb.MemoExt(System.String)"  /> 
/// <seealso cref="M:XSharp.CoreDb.MemoExt(System.String)"  /> 
FUNCTION VoDbMemoExt(cDriver AS STRING) AS STRING
    RETURN VoDb.MemoExt(cDriver)
    
/// <inheritdoc cref="M:XSharp.CoreDb.OrdBagExt"  /> 
/// <seealso cref="M:XSharp.CoreDb.OrdBagExt"  /> 
FUNCTION VoDbOrdBagExt() AS STRING
    RETURN VoDb.OrdBagExt()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordcondset/*" /> 
/// <seealso cref="M:XSharp.CoreDb.OrdCondSet(XSharp.RDD.Support.DbOrderCondInfo)"  />  
FUNCTION VoDbOrdCondSet(ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCondSet(ptrCondInfo)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreateorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordcreate/*" />
/// <seealso cref="M:XSharp.CoreDb.OrdCreate(System.String,System.Object,System.String,XSharp.ICodeblock,System.Boolean,XSharp.RDD.Support.DbOrderCondInfo)"  />
FUNCTION VoDbOrdCreate(cIndexFile AS STRING,cOrder AS USUAL,cKeyValue AS STRING,cbKeyValue AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCreate(cIndexFile, cOrder, cKeyValue, VoDb.ValidBlock(cbKeyValue), lUnique, ptrCondInfo)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdeleteorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodborddestroy/*" />
/// <seealso cref="M:XSharp.CoreDb.OrdDestroy(System.String,System.Object)"  />
FUNCTION VoDbOrdDestroy(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdDestroy(cIndexFile, uOrder)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodborderinfo/*" />
/// <seealso cref="O:XSharp.CoreDb.OrderInfo"  />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder AS USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile, uOrder, REF ptrRetVal)

/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbOrderInfo(System.UInt32,System.String,XSharp.__Usual,XSharp.__Usual@)" />/>
/// <param name="uValue">New value to assign to the Order.</param>
/// <seealso cref="O:XSharp.CoreDb.OrderInfo"  />
FUNCTION VoDbOrderInfo(kInfoType AS DWORD,cIndexFile AS STRING,uOrder AS USUAL,uValue AS USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(kInfoType, cIndexFile,  uOrder, uValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistadd/*" />
/// <seealso cref="M:XSharp.CoreDb.OrdListAdd(System.String,System.Object)"  />
FUNCTION VoDbOrdListAdd(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListAdd(cIndexFile, uOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistclear/*" />
/// <seealso cref="M:XSharp.CoreDb.OrdListClear(System.String,System.Object)"  />
FUNCTION VoDbOrdListClear(cIndexFile AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListClear(cIndexFile, uOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbreindex/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordlistrebuild/*" />
/// <seealso cref="M:XSharp.CoreDb.OrdListRebuild"  /> 
FUNCTION VoDbOrdListRebuild() AS LOGIC
    RETURN VoDb.OrdListRebuild()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetorder/*" />
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbordsetfocus/*" />
/// <param name="cOrder">This returns the previous order that was selected.</param>
/// <seealso cref="M:XSharp.CoreDb.OrdSetFocus(System.String,System.Object,System.String@)"  />
FUNCTION VoDbOrdSetFocus(cIndexFile AS STRING,uOrder AS USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDb.OrdSetFocus(cIndexFile,  uOrder, OUT cOrder)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbpack/*" />
/// <seealso cref="M:XSharp.CoreDb.Pack"  /> 
FUNCTION VoDbPack() AS LOGIC
    RETURN VoDb.Pack()
    
[Obsolete( "'VoDbRddCount( nRddType )' is not supported, use VoDbRddCount() instead", TRUE )];
FUNCTION VoDbRddCount(nRddType AS DWORD) AS DWORD
    RETURN 0
    
/// <inheritdoc cref="M:XSharp.CoreDb.RddCount"  /> 
/// <seealso cref="M:XSharp.CoreDb.RddCount"  /> 
FUNCTION VoDbRddCount() AS DWORD
    RETURN VoDb.RddCount()

/// <inheritdoc cref="M:XSharp.CoreDb.RddInfo(System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.RddInfo(System.UInt32,System.Object@)"  />
/// <br/><br/> <note type="tip">The difference between VoDbRddInfo and CoreDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="O:XSharp.CoreDb.RddInfo"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, REF uRet)
    
/// <inheritdoc cref="M:XSharp.CoreDb.RddInfo(System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.RddInfo(System.UInt32,System.Object)"  />
/// <br/><br/> <note type="tip">The difference between VoDbRddInfo and CoreDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="O:XSharp.CoreDb.RddInfo"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, uValue) 

   
[Obsolete( "'VoDbRddList( rddList, nRddType )' is not supported, use VoDbRddList() instead", TRUE )];
FUNCTION VoDbRddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}

/// <inheritdoc cref="M:XSharp.CoreDb.RddList"  /> 
/// <seealso cref="M:XSharp.CoreDb.RddList"  /> 
FUNCTION VoDbRddList() AS STRING[]
  RETURN VoDb.RddList()
    
/// <inheritdoc cref="M:XSharp.CoreDb.RddName"  /> 
/// <seealso cref="M:XSharp.CoreDb.RddName"  /> 
FUNCTION VoDbRddName() AS STRING
    RETURN VoDb.RddName()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrddsetdefault/*" />
/// <seealso cref="M:XSharp.CoreDb.RddSetDefault(System.String)"  /> 
FUNCTION VoDbRddSetDefault(cNewSetting AS STRING) AS STRING
    RETURN VoDb.RddSetDefault(cNewSetting)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecall/*" />
/// <seealso cref="M:XSharp.CoreDb.Recall"  /> 
FUNCTION VoDbRecall() AS LOGIC
    RETURN VoDb.Recall()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecno/*" />
/// <seealso cref="M:XSharp.CoreDb.Recno"  /> 
FUNCTION VoDbRecno() AS DWORD
    RETURN VoDb.Recno()
    
/// <inheritdoc cref="M:XSharp.CoreDb.RecordGet"  /> 
/// <seealso cref="M:XSharp.CoreDb.RecordGet"  /> 
FUNCTION VoDbRecordGet() AS BYTE[]
    RETURN VoDb.RecordGet()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecordinfo/*" />
/// <seealso cref="M:XSharp.CoreDb.RecordInfo(System.UInt32,System.Object,System.Object@)"  />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID AS USUAL,ptrRetVal REF USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, REF ptrRetVal)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrecordinfo/*" />
/// <seealso cref="M:XSharp.CoreDb.RecordInfo(System.UInt32,System.Object,System.Object)"  />
FUNCTION VoDbRecordInfo(kInfoType AS DWORD,nRecID AS USUAL,ptrRetVal AS USUAL) AS LOGIC
    RETURN VoDb.RecordInfo(kInfoType, nRecID, ptrRetVal)    
   
/// <inheritdoc cref="M:XSharp.CoreDb.RecordPut(System.Byte[])"  />  
/// <seealso cref="M:XSharp.CoreDb.RecordPut(System.Byte[])"  />  
FUNCTION VoDbRecordPut(aRecord AS BYTE[]) AS LOGIC
    RETURN VoDb.RecordPut(aRecord)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrelation/*" />
/// <seealso cref="M:XSharp.CoreDb.Relation(System.UInt32,System.String@)"  />
FUNCTION VoDbRelation(wRelation AS DWORD, pszRelation REF STRING) AS LOGIC
    RETURN VoDb.Relation(wRelation, REF pszRelation)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrlock/*" />
/// <seealso cref="M:XSharp.CoreDb.RLock(System.Object)"  />
FUNCTION VoDbRlock(uRecID AS USUAL) AS LOGIC
    RETURN VoDb.RLock(uRecId)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbseek/*" />
/// <seealso cref="M:XSharp.CoreDb.Seek(System.Object,System.Boolean,System.Boolean)"  />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC, lLast AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( uKey, lSoftSeek, lLast)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbseek/*" />
/// <seealso cref="M:XSharp.CoreDb.Seek(System.Object,System.Boolean,System.Boolean)"  />
FUNCTION VoDbSeek(uKey AS USUAL,lSoftSeek AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( uKey, lSoftSeek, FALSE)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbrselect/*" />
/// <seealso cref="M:XSharp.CoreDb.RSelect(System.UInt32)"  />
FUNCTION VoDbRSelect(wRelation AS DWORD) AS DWORD
    RETURN VoDb.RSelect(wRelation)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbselect/*" />
/// <seealso cref="M:XSharp.CoreDb.Select(System.UInt32,System.UInt32@)"  />
FUNCTION VoDbSelect(wNew AS DWORD,wOld REF USUAL) AS LOGIC
    RETURN VoDb.Select(wNew, REF wOld)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbselect/*" />
/// <seealso cref="M:XSharp.CoreDb.Select(System.UInt32,System.UInt32@)"  />  
FUNCTION VoDbSelect(wNew AS DWORD,wOld REF DWORD ) AS LOGIC
  RETURN VoDb.Select(wNew, REF wOld)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetfilter/*" />
/// <seealso cref="M:XSharp.CoreDb.SetFilter(XSharp.ICodeblock,System.String)"  />
FUNCTION VoDbSetFilter(cbCondition AS USUAL,cCondition AS STRING) AS LOGIC
    RETURN VoDb.SetFilter(VoDb.ValidBlock(cbCondition), cCondition)
   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetfound/*" />
/// <seealso cref="M:XSharp.CoreDb.SetFound(System.Boolean)"  />  
FUNCTION VoDbSetFound(lFound AS LOGIC) AS LOGIC
    RETURN VoDb.SetFound(lFound)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetlocate/*" />
/// <seealso cref="M:XSharp.CoreDb.SetLocate(XSharp.ICodeblock)"  />
FUNCTION VoDbSetLocate(cbForCondition AS USUAL) AS LOGIC
    RETURN VoDb.SetLocate(VoDb.ValidBlock(cbForCondition))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetrelation/*" />
/// <seealso cref="M:XSharp.CoreDb.SetRelation(System.String,XSharp.ICodeblock,System.String,System.String)"  />
FUNCTION VoDbSetRelation(cAlias AS STRING,cbRel AS USUAL,cRel AS STRING, cName := "" AS STRING) AS LOGIC
    RETURN VoDb.SetRelation(cAlias, VoDb.ValidBlock(cbRel), cRel, cName)
 
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetscope/*" /> 
/// <seealso cref="M:XSharp.CoreDb.SetScope(XSharp.RDD.Support.DbScopeInfo)"  />  
FUNCTION VoDbSetScope(scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.SetScope(scope)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsetselect/*" />   
/// <seealso cref="M:XSharp.CoreDb.SetSelect(System.Int32)"  />    
FUNCTION VoDbSetSelect(siNewArea AS INT) AS DWORD
    RETURN VoDb.SetSelect(siNewArea)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbskip/*" />   
/// <seealso cref="M:XSharp.CoreDb.Skip(System.Int32)"  /> 
FUNCTION VoDbSkip(liRecords AS LONG) AS LOGIC
    RETURN VoDb.Skip(liRecords)
    
/// <inheritdoc cref="M:XSharp.CoreDb.SkipScope(System.Int32,XSharp.RDD.Support.DbScopeInfo)"  />  
/// <seealso cref="M:XSharp.CoreDb.SkipScope(System.Int32,XSharp.RDD.Support.DbScopeInfo)"  />  
FUNCTION VoDbSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC 
    RETURN VoDb.SkipScope(nRecords, scope) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsort/*" />   
/// <seealso cref="M:XSharp.CoreDb.Sort(System.UInt32,XSharp._FieldNames,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean,XSharp._FieldNames)"  />
/// <param name="fldNames">List of field names to copy</param>
/// <param name="fnSortNames">List of field names to sort on</param>
FUNCTION VoDbSort(nDest AS DWORD,fldNames AS _FieldNames,cbForCondition AS USUAL,cbWhileCondition AS USUAL, nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC,fnSortNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Sort(nDest, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest, fnSortNames)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbsymselect/*" /> 
/// <seealso cref="M:XSharp.CoreDb.SymSelect(System.String)"  />
FUNCTION VoDbSymSelect(symAlias AS SYMBOL) AS INT
    RETURN VoDb.SymSelect(symAlias)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbtrans/*" /> 
/// <seealso cref="M:XSharp.CoreDb.Trans(System.UInt32,XSharp._FieldNames,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean)"  />
/// <param name="fldNames">List of field names to copy </param>
FUNCTION VoDbTrans(wTarget AS DWORD,fldNames AS _FieldNames,cbForCondition AS USUAL,cbWhileCondition AS USUAL, nNext AS USUAL,nRecord AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Trans(wTarget, fldNames, VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest)
    
/// <inheritdoc cref="M:XSharp.CoreDb.TransRec(System.UInt32,XSharp._FieldNames)"  />   
/// <seealso cref="M:XSharp.CoreDb.TransRec(System.UInt32,XSharp._FieldNames)"  />   
FUNCTION VoDbTransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
    RETURN VoDb.TransRec(nDest, fldNames)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbunlock/*" /> 
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Unlock(System.Object)"  />
/// <br/><br/> <note type="tip">The difference between VoDbUnlock and CoreDb.UnLock is that VoDbUnlock takes USUAL parameters</note></remarks>
FUNCTION VoDbUnlock(uRecID AS USUAL) AS LOGIC
    RETURN VoDb.UnLock( uRecID)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbunlockall/*" /> 
/// <seealso cref="M:XSharp.CoreDb.OrdBagExt"  /> 
FUNCTION VoDbUnLockAll() AS LOGIC
    RETURN VoDb.UnLockAll()

/// <overloads>
/// <summary>
/// Open a file
/// </summary>
/// <seealso cref="O:XSharp.CoreDb.UseArea" />
/// <seealso cref="M:XSharp.RT.Functions.DbUseArea(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
/// </overloads>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" /> 
/// <seealso cref="M:XSharp.CoreDb.UseArea(System.Boolean,XSharp._RddList,System.String,System.String,System.Boolean,System.Boolean)"  />
/// <param name="rddlist">List of RDDs to use.</param>
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddlist AS _RddList,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddlist, cDataFile, cAlias, lShared, lReadOnly)
    
/// <param name="rddName">Name of the RDD to use.</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" /> 
/// <seealso cref="M:XSharp.CoreDb.UseArea(System.Boolean,System.String,System.String,System.String,System.Boolean,System.Boolean)"  />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddName AS STRING,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea, rddName, cDataFile, cAlias, lShared, lReadOnly)
    
/// <param name="rddType">Type of the RDD to use.</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbusearea/*" /> 
/// <seealso cref="M:XSharp.CoreDb.UseArea(System.Boolean,System.Type,System.String,System.String,System.Boolean,System.Boolean)"  />
FUNCTION VoDbUseArea(lNewArea AS LOGIC,rddType AS System.Type,cDataFile AS STRING,cAlias AS STRING,lShared AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.UseArea(lNewArea ,rddType ,cDataFile ,cAlias ,lShared ,lReadOnly ) 
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/vodbzap/*" />  
/// <seealso cref="M:XSharp.CoreDb.Zap"  /> 
FUNCTION VoDbZap() AS LOGIC
    RETURN VoDb.Zap()

/// <inheritdoc cref="M:XSharp.CoreDb._ErrInfoPtr"  />     
/// <seealso cref="M:XSharp.CoreDb._ErrInfoPtr"  />     
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
