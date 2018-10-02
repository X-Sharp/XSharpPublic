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
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Alias(System.UInt32)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Alias(System.UInt32)"  />
FUNCTION VoDbAlias(nArea AS DWORD) AS STRING
    return VoDb.Alias(nArea)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Alias(System.UInt32)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Alias(System.UInt32)"  />
FUNCTION VoDbAliasSym(nArea AS DWORD) AS SYMBOL
    RETURN AsSymbol(VoDb.Alias(nArea))
  

/// <inheritdoc cref="M:XSharp.CoreVoDb.Append(System.Boolean)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Append(System.Boolean)"  />
FUNCTION VoDbAppend(lReleaseLocks AS LOGIC) AS LOGIC
    return VoDb.Append(lReleaseLocks)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Bof"  />
/// <seealso cref="M:XSharp.CoreVoDb.Bof"  />
FUNCTION VoDbBof() AS LOGIC
    return VoDb.Bof()

/// <inheritdoc cref="M:XSharp.CoreVoDb.BlobInfo(System.UInt32,System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbBlobInfo and CoreVoDb..BlobInfo is that VoDbBlobInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.BlobInfo(System.UInt32,System.UInt32,System.Object@)"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    return VoDb.BlobInfo(nOrdinal, nPos, REF ptrRet)

/// <inheritdoc cref="M:XSharp.CoreVoDb.BlobInfo(System.UInt32,System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbBlobInfo and CoreVoDb..BlobInfo is that VoDbBlobInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.BlobInfo(System.UInt32,System.UInt32,System.Object)"  />
FUNCTION VoDbBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    return VoDb.BlobInfo(nOrdinal, nPos,  uValue)



/// <inheritdoc cref="M:XSharp.CoreVoDb.BuffRefresh"  />
/// <seealso cref="M:XSharp.CoreVoDb.BuffRefresh"  />
FUNCTION VoDbBuffRefresh() AS LOGIC
    return VoDb.BuffRefresh()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.ClearFilter"  />    
/// <seealso cref="M:XSharp.CoreVoDb.ClearFilter"  />    
FUNCTION VoDbClearFilter() AS LOGIC
    return VoDb.ClearFilter()

/// <inheritdoc cref="M:XSharp.CoreVoDb.ClearLocate"  />    
/// <seealso cref="M:XSharp.CoreVoDb.ClearLocate"  />    
FUNCTION VoDbClearLocate() AS LOGIC
    return VoDb.ClearLocate()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.ClearRelation"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.ClearRelation"  /> 
FUNCTION VoDbClearRelation() AS LOGIC
    return VoDb.ClearRelation()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.ClearScope"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.ClearScope"  /> 
FUNCTION VoDbClearScope() AS LOGIC
    return VoDb.ClearScope()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.CloseAll"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.CloseAll"  /> 
FUNCTION VoDbCloseAll() AS LOGIC
    return VoDb.CloseAll()

/// <inheritdoc cref="M:XSharp.CoreVoDb.CloseArea"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.CloseArea"  /> 
/// <remarks> <inheritdoc/> < br/> <note type="tip">VoDbCloseArea() is an alias for CoreDbCloseArea()</note></remarks>
FUNCTION VoDbCloseArea() AS LOGIC
    return VoDb.CloseArea()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Commit"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Commit"  /> 
FUNCTION VoDbCommit() AS LOGIC
    return VoDb.Commit()

/// <inheritdoc cref="M:XSharp.CoreVoDb.CommitAll"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.CommitAll"  /> 
FUNCTION VoDbCommitAll() AS LOGIC
   return VoDb.CommitAll()

/// <inheritdoc cref="M:XSharp.CoreVoDb.Continue"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Continue"  /> 
FUNCTION VoDbContinue() AS LOGIC
    return VoDb.Continue()

/// <inheritdoc cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <seealso cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbCreate and CoreVoDb..Create is that VoDbCreate takes a ARRAY parameter</note></remarks>

FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), cRddName, lNew, cAlias, cDelim, lKeep, lJustOpen)  

/// <inheritdoc cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <seealso cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="oRddType">Type of the RDDs to use when creating the file</param>
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbCreate and CoreVoDb..Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, oRddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), oRddType, lNew, cAlias, cDelim, lKeep, lJustOpen)  

/// <inheritdoc cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <seealso cref="M:XSharp.CoreVoDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="aList">structure that describes the list of RDDs to use</param>
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbCreate and CoreVoDb..Create is that VoDbCreate takes a ARRAY parameter</note></remarks>
FUNCTION VoDbCreate( cName AS STRING, aStruct AS ARRAY, aList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Create(cName, VoDb.ArrayToFieldInfo(aStruct), aList, lNew, cAlias, cDelim, lKeep, lJustOpen)  

  
/// <inheritdoc cref="M:XSharp.CoreVoDb.Delete"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Delete"  /> 
FUNCTION VoDbDelete() AS LOGIC
    return VoDb.Delete()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Deleted"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Deleted"  /> 
FUNCTION VoDbDeleted() AS LOGIC
     return VoDb.Deleted()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Eof"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Eof"  /> 
FUNCTION VoDbEof() AS LOGIC
     return VoDb.Eof()

/// <inheritdoc cref="M:XSharp.CoreVoDb.Eval(XSharp.ICodeBlock,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbEval and CoreVoDb..Eval is that VoDbEval takes USUAL parameters</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Eval(XSharp.ICodeBlock,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
FUNCTION VoDbEval(uBlock AS USUAL,uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Eval(VoDb.ValidBlock(uBlock), VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest)

/// <inheritdoc cref="M:XSharp.CoreVoDb.FieldGet(System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbFieldGet and CoreVoDb..FieldGet is that VoDbFieldGet takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.FieldGet(System.UInt32,System.Object@)"  />
FUNCTION VoDbFieldGet(nPos AS DWORD,uRet REF USUAL) AS LOGIC
    RETURN VoDb.FieldGet(nPos, ref uRet)


/// <inheritdoc cref="M:XSharp.CoreVoDb.FieldInfo(System.UInt32,System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbFieldInfo and CoreVoDb.FieldInfo is that VoDbFieldInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.FieldInfo(System.UInt32,System.UInt32,System.Object@)"  />
FUNCTION VoDbFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
   RETURN VoDb.FieldInfo(nOrdinal, nPos, ref ptrRet)

/// <inheritdoc cref="M:XSharp.CoreVoDb.FieldInfo(System.UInt32,System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbFieldInfo and CoreVoDb.FieldInfo is that VoDbFieldInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.FieldInfo(System.UInt32,System.UInt32,System.Object)"  />
FUNCTION VoDbFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    return VoDb.FieldInfo(nOrdinal, nPos, uValue)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.FieldPut(System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbFieldPut and CoreVoDb.FieldPut is that VoDbFieldPut takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.FieldPut(System.UInt32,System.Object)"  />
FUNCTION VoDbFieldPut(nPos AS DWORD,xValue AS USUAL) AS LOGIC
    RETURN VoDb.FieldPut(nPos, xValue)

/// <inheritdoc cref="M:XSharp.CoreVoDb.FileGet(System.UInt32,System.String)"  />
/// <seealso cref="M:XSharp.CoreVoDb.FileGet(System.UInt32,System.String)"  />
FUNCTION VoDbFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    return VoDb.FileGet(nPos, cFile)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.FilePut(System.UInt32,System.String)"  />
/// <seealso cref="M:XSharp.CoreVoDb.FilePut(System.UInt32,System.String)"  />
FUNCTION VoDbFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    return VoDb.FilePut(nPos, cFile)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Filter"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Filter"  /> 
FUNCTION VoDbFilter() AS STRING
    return VoDb.Filter()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Flock"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Flock"  /> 
FUNCTION VoDbFlock() AS LOGIC
    return VoDb.Flock()

/// <inheritdoc cref="M:XSharp.CoreVoDb.Found"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Found"  /> 
FUNCTION VoDbFound() AS LOGIC
    return VoDb.Found()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.GetSelect"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.GetSelect"  /> 
FUNCTION VoDbGetSelect() AS DWORD
    return VoDb.GetSelect()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.GoBottom"  />     
/// <seealso cref="M:XSharp.CoreVoDb.GoBottom"  />     
FUNCTION VoDbGoBottom() AS LOGIC
    return VoDb.GoBottom()

    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Goto(System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbGoto and CoreVoDb.Goto is that VoDbGoto takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Goto(System.Object)"  />
FUNCTION VoDbGoto(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.Goto((OBJECT) uRecID)

/// <inheritdoc cref="M:XSharp.CoreVoDb.GoTop"  />        
/// <seealso cref="M:XSharp.CoreVoDb.GoTop"  />        
FUNCTION VoDbGoTop() AS LOGIC
    return VoDb.GoTop()

    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Info(System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbInfo and CoreVoDb.Info is that VoDbInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Info(System.UInt32,System.Object@)"  />
FUNCTION VoDbInfo(nOrdinal AS DWORD,ptrRet REF USUAL) AS LOGIC
    return VoDb.Info(nOrdinal, ref ptrRet)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Info(System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbInfo and CoreVoDb.Info is that VoDbInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Info(System.UInt32,System.Object)"  />
FUNCTION VoDbInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.Info(nOrdinal, uValue)



/// <inheritdoc cref="M:XSharp.CoreVoDb.JoinAppend(System.UInt32,XSharp._JoinList)"  />        
/// <seealso cref="M:XSharp.CoreVoDb.JoinAppend(System.UInt32,XSharp._JoinList)"  />        
FUNCTION VoDbJoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
    return VoDb.JoinAppend(nSelect, struList )
      
      
/// <inheritdoc cref="M:XSharp.CoreVoDb.LastRec"  />   
/// <seealso cref="M:XSharp.CoreVoDb.LastRec"  />   
FUNCTION VoDbLastRec() AS LONG
    return VoDb.LastRec() 

/// <inheritdoc cref="M:XSharp.CoreVoDb.Locate(XSharp.ICodeBlock,XSharp.ICodeBlock,System.Int32,System.Object,System.Boolean)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbLocate and CoreVoDb.Locate is that VoDbLocate takes USUAL parameters</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Locate(XSharp.ICodeBlock,XSharp.ICodeBlock,System.Int32,System.Object,System.Boolean)"  />

FUNCTION VoDbLocate(uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS LONG,uRecId AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Locate(VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, (OBJECT) uRecID, lRest)
    

/// <inheritdoc cref="M:XSharp.CoreVoDb.MemoExt(System.String)"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.MemoExt(System.String)"  /> 
FUNCTION VoDbMemoExt(cDriver AS STRING) AS STRING
    return VoDb.MemoExt(cDriver)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdBagExt"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.OrdBagExt"  /> 
FUNCTION VoDbOrdBagExt() AS STRING
    return VoDb.OrdBagExt()

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdCondSet(XSharp.RDD.Support.DbOrderCondInfo)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.OrdCondSet(XSharp.RDD.Support.DbOrderCondInfo)"  />  
FUNCTION VoDbOrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
    return VoDb.OrdCondSet(ordCondInfo)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdCreate(System.String,System.Object,System.String,XSharp.ICodeBlock,System.Boolean,XSharp.RDD.Support.DbOrderCondInfo)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrdCreate and CoreVoDb.OrdCreate is that VoDbOrdCreate takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrdCreate(System.String,System.Object,System.String,XSharp.ICodeBlock,System.Boolean,XSharp.RDD.Support.DbOrderCondInfo)"  />

FUNCTION VoDbOrdCreate(cBagName AS STRING,uOrder AS USUAL,cExpr AS STRING,uCobExpr AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.OrdCreate(cBagName, (OBJECT) uOrder, cExpr, VoDb.ValidBlock(uCobExpr), lUnique, ptrCondInfo)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdDestroy(System.String,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrdDestroy and CoreVoDb.OrdDestroy is that VoDbOrdDestroy takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrdDestroy(System.String,System.Object)"  />
FUNCTION VoDbOrdDestroy(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdDestroy(cOrdBag, (OBJECT) uOrder)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.OrderInfo(System.UInt32,System.String,System.Object,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrderInfo and CoreVoDb.OrderInfo is that VoDbOrderInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrderInfo(System.UInt32,System.String,System.Object,System.Object@)"  />

FUNCTION VoDbOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uRet REF USUAL) AS LOGIC
    return VoDb.OrderInfo(nOrdinal, cBagName, uOrder, REF uRet)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrderInfo(System.UInt32,System.String,System.Object,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrderInfo and CoreVoDb.OrderInfo is that VoDbOrderInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrderInfo(System.UInt32,System.String,System.Object,System.Object)"  />
FUNCTION VoDbOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uValue AS USUAL) AS LOGIC
    RETURN VoDb.OrderInfo(nOrdinal, cBagName,  uOrder, uValue)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdListAdd(System.String,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrdListAdd and CoreVoDb.OrdListAdd is that VoDbOrdListAdd takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrdListAdd(System.String,System.Object)"  />

FUNCTION VoDbOrdListAdd(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListAdd(cOrdBag, (OBJECT) uOrder)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdListClear(System.String,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrdListClear and CoreVoDb.OrdListClear is that VoDbOrdListClear takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrdListClear(System.String,System.Object)"  />
FUNCTION VoDbOrdListClear(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDb.OrdListClear(cOrdBag, (OBJECT) uOrder)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdListRebuild"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.OrdListRebuild"  /> 
FUNCTION VoDbOrdListRebuild() AS LOGIC
    return VoDb.OrdListRebuild()

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdSetFocus(System.String,System.Object,System.String@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbOrdSetFocus and CoreVoDb.OrdSetFocus is that VoDbOrdSetFocus takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.OrdSetFocus(System.String,System.Object,System.String@)"  />
FUNCTION VoDbOrdSetFocus(cOrdBag AS STRING,uOrder AS USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDb.OrdSetFocus(cOrdBag, (OBJECT) uOrder, OUT cOrder)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Pack"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Pack"  /> 
FUNCTION VoDbPack() AS LOGIC
    return VoDb.Pack()
    
[Obsolete( "'VoDbRddCount( nRddType )' is not supported, use VoDbRddCount() instead", TRUE )];
FUNCTION VoDbRddCount(nRddType AS DWORD) AS DWORD
    RETURN 0
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.RddCount"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.RddCount"  /> 
FUNCTION VoDbRddCount() AS DWORD
    return VoDb.RddCount()

/// <inheritdoc cref="M:XSharp.CoreVoDb.RddInfo(System.UInt32,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRddInfo and CoreVoDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.RddInfo(System.UInt32,System.Object@)"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    return VoDb.RddInfo(nOrdinal, REF uRet)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.RddInfo(System.UInt32,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRddInfo and CoreVoDb.RddInfo is that VoDbRddInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.RddInfo(System.UInt32,System.Object)"  />
FUNCTION VoDbRddInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    RETURN VoDb.RddInfo(nOrdinal, uValue) 

   
[Obsolete( "'VoDbRddList( rddList, nRddType )' is not supported, use VoDbRddList() instead", TRUE )];
FUNCTION VoDbRddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}

/// <inheritdoc cref="M:XSharp.CoreVoDb.RddList"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.RddList"  /> 
FUNCTION VoDbRddList() AS STRING[]
  return VoDb.RddList()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.RddName"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.RddName"  /> 
FUNCTION VoDbRddName() AS STRING
    return VoDb.RddName()

/// <inheritdoc cref="M:XSharp.CoreVoDb.RddSetDefault(System.String)"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.RddSetDefault(System.String)"  /> 
FUNCTION VoDbRddSetDefault(cNewRDD AS STRING) AS STRING
    return VoDb.RddSetDefault(cNewRDD)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Recall"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Recall"  /> 
FUNCTION VoDbRecall() AS LOGIC
    return VoDb.Recall()

/// <inheritdoc cref="M:XSharp.CoreVoDb.Recno"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Recno"  /> 
FUNCTION VoDbRecno() AS DWORD
    return VoDb.Recno()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.RecordGet"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.RecordGet"  /> 
FUNCTION VoDbRecordGet() AS BYTE[]
    return VoDb.RecordGet()

/// <inheritdoc cref="M:XSharp.CoreVoDb.RecordInfo(System.UInt32,System.Object,System.Object@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRecordInfo and CoreVoDb.RecordInfo is that VoDbRecordInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.RecordInfo(System.UInt32,System.Object,System.Object@)"  />
FUNCTION VoDbRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet REF USUAL) AS LOGIC
    return VoDb.RecordInfo(nOrdinal, uRecID, REF uRet)

/// <inheritdoc cref="M:XSharp.CoreVoDb.RecordInfo(System.UInt32,System.Object,System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRecordInfo and CoreVoDb.RecordInfo is that VoDbRecordInfo takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.RecordInfo(System.UInt32,System.Object,System.Object)"  />
FUNCTION VoDbRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uValue AS USUAL) AS LOGIC
    return VoDb.RecordInfo(nOrdinal, uRecID, uValue)    
   
/// <inheritdoc cref="M:XSharp.CoreVoDb.RecordPut(System.Byte[])"  />  
/// <seealso cref="M:XSharp.CoreVoDb.RecordPut(System.Byte[])"  />  
FUNCTION VoDbRecordPut(aRecord AS BYTE[]) AS LOGIC
    return VoDb.RecordPut(aRecord)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Relation(System.UInt32,System.String@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRelation and CoreVoDb.Relation is that VoDbRelation takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Relation(System.UInt32,System.String@)"  />

FUNCTION VoDbRelation(nPos AS DWORD, uRel REF USUAL) AS LOGIC
    RETURN VoDb.Relation(nPos, REF uRel)


/// <inheritdoc cref="M:XSharp.CoreVoDb.RLock(System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbRlock and CoreVoDb.RLock is that VoDbRlock takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.RLock(System.Object)"  />

FUNCTION VoDbRlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDb.RLock((OBJECT) uRecId)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Seek(System.Object,System.Boolean,System.Boolean)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSeek and CoreVoDb.Seek is that VoDbSeek takes a USUAL parameter</note></remarks>
/// <seealso cref="M:XSharp.CoreVoDb.Seek(System.Object,System.Boolean,System.Boolean)"  />

FUNCTION VoDbSeek(xValue AS USUAL,lSoft AS LOGIC, lLast AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( (OBJECT) xValue, lSoft, lLast)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Seek(System.Object,System.Boolean,System.Boolean)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Seek(System.Object,System.Boolean,System.Boolean)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSeek and CoreVoDb.Seek is that VoDbSeek takes a USUAL parameter</note></remarks>
FUNCTION VoDbSeek(xValue AS USUAL,lSoft AS LOGIC) AS LOGIC
    RETURN VoDb.Seek( (OBJECT) xValue, lSoft, FALSE)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.RSelect(System.UInt32)"  />
/// <seealso cref="M:XSharp.CoreVoDb.RSelect(System.UInt32)"  />
FUNCTION VoDbRSelect(nPos AS DWORD) AS DWORD
    return VoDb.RSelect(nPos)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Select(System.UInt32,System.UInt32@)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Select(System.UInt32,System.UInt32@)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSelect and CoreVoDb.Select is that VoDbSelect takes a USUAL parameter</note></remarks>
FUNCTION VoDbSelect(nNew AS DWORD,nOld REF USUAL) AS LOGIC
    return VoDb.Select(nNew, REF nOld)
    
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Select(System.UInt32,System.UInt32@)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.Select(System.UInt32,System.UInt32@)"  />  
FUNCTION VoDbSelect(nNew AS DWORD,nOld REF DWORD ) AS LOGIC
  return VoDb.Select(nNew, REF nOld)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.SetFilter(XSharp.ICodeBlock,System.String)"  />
/// <seealso cref="M:XSharp.CoreVoDb.SetFilter(XSharp.ICodeBlock,System.String)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSetFilter and CoreVoDb.SetFilter is that VoDbSetFilter takes a USUAL parameter</note></remarks>
FUNCTION VoDbSetFilter(uCobFilter AS USUAL,cFilter AS STRING) AS LOGIC
    RETURN VoDb.SetFilter(VoDb.ValidBlock(uCobFilter), cFilter)
   



/// <inheritdoc cref="M:XSharp.CoreVoDb.SetFound(System.Boolean)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.SetFound(System.Boolean)"  />  
FUNCTION VoDbSetFound(lFound AS LOGIC) AS LOGIC
    return VoDb.SetFound(lFound)

/// <inheritdoc cref="M:XSharp.CoreVoDb.SetLocate(XSharp.ICodeBlock)"  />
/// <seealso cref="M:XSharp.CoreVoDb.SetLocate(XSharp.ICodeBlock)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSetLocate and CoreVoDb.SetLocate is that VoDbSetLocate takes a USUAL parameter</note></remarks>
FUNCTION VoDbSetLocate(uCobFor AS USUAL) AS LOGIC
    RETURN VoDb.SetLocate(VoDb.ValidBlock(uCobFor))

/// <inheritdoc cref="M:XSharp.CoreVoDb.SetRelation(System.String,XSharp.ICodeBlock,System.String)"  />
/// <seealso cref="M:XSharp.CoreVoDb.SetRelation(System.String,XSharp.ICodeBlock,System.String)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSetRelation and CoreVoDb.SetRelation is that VoDbSetRelation takes a USUAL parameter</note></remarks>
FUNCTION VoDbSetRelation(cAlias AS STRING,uCobKey AS USUAL,cKey AS STRING) AS LOGIC
    RETURN VoDb.SetRelation(cAlias, VoDb.ValidBlock(uCobKey), cKey)
 
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.SetScope(XSharp.RDD.Support.DbScopeInfo)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.SetScope(XSharp.RDD.Support.DbScopeInfo)"  />  
FUNCTION VoDbSetScope(scope AS DbScopeInfo) AS LOGIC
    return VoDb.SetScope(scope)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.SetSelect(System.Int32)"  />    
/// <seealso cref="M:XSharp.CoreVoDb.SetSelect(System.Int32)"  />    
FUNCTION VoDbSetSelect(siNew AS INT) AS DWORD
    return VoDb.SetSelect(siNew)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Skip(System.Int32)"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Skip(System.Int32)"  /> 
FUNCTION VoDbSkip(nRecords AS LONG) AS LOGIC
    return VoDb.Skip(nRecords)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.SkipScope(System.Int32,XSharp.RDD.Support.DbScopeInfo)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.SkipScope(System.Int32,XSharp.RDD.Support.DbScopeInfo)"  />  
FUNCTION VoDbSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
    return VoDb.SkipScope(nRecords, scope)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Sort(System.UInt32,XSharp._FieldNames,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean,XSharp._FieldNames)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Sort(System.UInt32,XSharp._FieldNames,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean,XSharp._FieldNames)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSort and CoreVoDb.Sort is that VoDbSort takes USUAL parameters</note></remarks>
FUNCTION VoDbSort(nDest AS DWORD,fnNames AS _FieldNames,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC,fnSortNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Sort(nDest, fnNames, VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest, fnSortNames)

/// <inheritdoc cref="M:XSharp.CoreVoDb.SymSelect(System.String)"  />
/// <seealso cref="M:XSharp.CoreVoDb.SymSelect(System.String)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbSymSelect and CoreVoDb.SymSelect is that VoDbSymSelect takes SYMBOL parameters</note></remarks>
FUNCTION VoDbSymSelect(symAlias AS SYMBOL) AS INT
    return VoDb.SymSelect(symAlias)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Trans(System.UInt32,XSharp._FieldNames,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
/// <seealso cref="M:XSharp.CoreVoDb.Trans(System.UInt32,XSharp._FieldNames,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbTrans and CoreVoDb.Trans is that VoDbTrans takes USUAL parameters</note></remarks>
FUNCTION VoDbTrans(nDest AS DWORD,fldNames AS _FieldNames,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Trans(nDest, fldNames, VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.TransRec(System.UInt32,XSharp._FieldNames)"  />   
/// <seealso cref="M:XSharp.CoreVoDb.TransRec(System.UInt32,XSharp._FieldNames)"  />   
FUNCTION VoDbTransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
    return VoDb.TransRec(nDest, fldNames)

/// <inheritdoc cref="M:XSharp.CoreVoDb.Unlock(System.Object)"  />
/// <remarks> <inheritdoc/> < br/> <note type="tip">The difference between VoDbUnlock and CoreVoDb.UnLock is that VoDbUnlock takes USUAL parameters</note></remarks>
FUNCTION VoDbUnlock(uRecno AS USUAL) AS LOGIC
    RETURN VoDb.UnLock((OBJECT) uRecno)

/// <inheritdoc cref="M:XSharp.CoreVoDb.OrdBagExt"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.OrdBagExt"  /> 
FUNCTION VoDbUnlockAll() AS LOGIC
    return VoDb.UnlockAll()
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,XSharp._RddList,System.String,System.String,System.Boolean,System.Boolean)"  />  
/// <seealso cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,XSharp._RddList,System.String,System.String,System.Boolean,System.Boolean)"  />  
FUNCTION VoDbUseArea(lNew AS LOGIC,rddList AS _RddList,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    return VoDb.UseArea(lNew, rddList, cName, cAlias, lShare, lReadOnly)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,System.String,System.String,System.String,System.Boolean,System.Boolean)"  />    
/// <seealso cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,System.String,System.String,System.String,System.Boolean,System.Boolean)"  />    
FUNCTION VoDbUseArea(lNew AS LOGIC,rddName AS STRING,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    return VoDb.UseArea(lNew, rddName, cName, cAlias, lShare, lReadOnly)
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,System.Type,System.String,System.String,System.Boolean,System.Boolean)"  />    
/// <seealso cref="M:XSharp.CoreVoDb.UseArea(System.Boolean,System.Type,System.String,System.String,System.Boolean,System.Boolean)"  />    
FUNCTION VoDbUseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    return VoDb.UseArea(lNew ,rddType ,cName ,cAlias ,lShare ,lReadOnly ) 
    
/// <inheritdoc cref="M:XSharp.CoreVoDb.Zap"  /> 
/// <seealso cref="M:XSharp.CoreVoDb.Zap"  /> 
FUNCTION VoDbZap() AS LOGIC
    return VoDb.Zap()

/// <inheritdoc cref="M:XSharp.Core.Functions._CoreDbErrInfoPtr"  />     
/// <seealso cref="M:XSharp.Core.Functions._CoreDbErrInfoPtr"  />     
FUNCTION _VoDbErrInfoPtr AS Exception
    RETURN VoDb._ErrInfoPtr()


FUNCTION __AllocRddList(aRDDs AS ARRAY) AS _RddList
    LOCAL aNames AS List<STRING>
    aNames := List<STRING>{}
    FOREACH rdd AS USUAL IN aRDDs
        aNames:Add( (STRING) rdd)
    NEXT
    RETURN _RddList{aNames:ToArray()}
