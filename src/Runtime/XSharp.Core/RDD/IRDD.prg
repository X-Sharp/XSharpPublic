//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
/// <include file="XSharp.Core.Docs.xml" path="doc/IRdd/*" />
INTERFACE XSharp.RDD.IRdd
	// Navigation
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.DbEval/*" />
	METHOD DbEval(info AS DbEvalInfo)		AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoTop/*" />
	METHOD GoTop()							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoBottom/*" />
	METHOD GoBottom()						AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoTo/*" />
	METHOD GoTo(nRec AS DWORD)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoToId/*" />
	METHOD GoToId(oRec AS OBJECT)			AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Skip/*" />
	METHOD Skip(nToSkip AS INT)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SkipFilter/*" />
	METHOD SkipFilter(nToSkip AS INT)		AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SkipRaw/*" />
	METHOD SkipRaw(nToSkip AS INT)			AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SkipScope/*" />
	METHOD SkipScope(nToSkip AS INT)		AS LOGIC

	// Append and Delete
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Append/*" />
	METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Delete/*" />
	METHOD Delete()							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetRec/*" />
	METHOD GetRec()							AS BYTE[]

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Pack/*" />
	METHOD Pack()							AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.PutRec/*" />
    METHOD PutRec(aRec AS BYTE[])			AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Recall/*" />
    METHOD Recall()							AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Zap/*" />
    METHOD Zap()							AS LOGIC

	// Open and Close
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Close/*" />
	METHOD Close() 							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Create/*" />
	METHOD Create(info AS DbOpenInfo)		AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Open/*" />
	METHOD Open(info AS DbOpenInfo)			AS LOGIC

	// Filtering and Scoping
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ClearFilter/*" />
	METHOD ClearFilter() 					AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ClearScope/*" />
	METHOD ClearScope() 					AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Continue/*" />
	METHOD Continue()						AS LOGIC

	//METHOD CountScope(pOpt as PTR)		AS LONG
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetScope/*" />
	METHOD GetScope()						AS DbScopeInfo

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SetFilter/*" />
	METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SetScope/*" />
	METHOD SetScope(info AS DbScopeInfo)	AS LOGIC

	// Fields
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SetFieldExtent/*" />
	METHOD SetFieldExtent(fieldCount AS LONG ) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.AddField/*" />
	METHOD AddField(info AS RddFieldInfo)	AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.CreateFields/*" />
	METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.FieldIndex/*" />
	METHOD FieldIndex(fieldName AS STRING)	AS LONG

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.FieldInfo/*" />
	METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oValue AS OBJECT) AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.FieldName/*" />
	METHOD FieldName(nFldPos AS LONG)		AS STRING

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetField/*" />
    METHOD GetField(nFldPos AS LONG) AS RddFieldInfo
	// Read & Write

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetValue/*" />
	METHOD GetValue(nFldPos AS LONG)		AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetValueFile/*" />
	METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GetValueLength/*" />
	METHOD GetValueLength(nFldPos AS LONG)	AS LONG

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Flush/*" />
    METHOD Flush() 							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoCold/*" />
	METHOD GoCold()							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.GoHot/*" />
	METHOD GoHot()							AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.PutValue/*" />
	METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.PutValueFile/*" />
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Refresh/*" />
	METHOD Refresh() 							AS LOGIC
	// Locking

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.AppendLock/*" />
	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.HeaderLock/*" />
	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Lock/*" />
	METHOD Lock(uiMode REF DbLockInfo)		AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.UnLock/*" />
	METHOD UnLock(oRecId AS OBJECT)			AS LOGIC

	// Memo File Access
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.CloseMemFile/*" />
	METHOD CloseMemFile() 					AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.CreateMemFile/*" />
	METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OpenMemFile/*" />
	METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC

	// Indexes

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderCondition/*" />
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderCreate/*" />
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderDestroy/*" />
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderInfo/*" />
	METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderListAdd/*" />
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderListDelete/*" />
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderListFocus/*" />
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.OrderListRebuild/*" />
	METHOD OrderListRebuild() AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Seek/*" />
	METHOD Seek(info AS DbSeekInfo) AS LOGIC

	// Relations

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ChildEnd/*" />
	METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ChildStart/*" />
	METHOD ChildStart(info AS DbRelInfo)			AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ChildSync/*" />
	METHOD ChildSync(info AS DbRelInfo)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ClearRel/*" />
	METHOD ClearRel()								AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.ForceRel/*" />
	METHOD ForceRel()								AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RelArea/*" />
	METHOD RelArea(nRelNum AS DWORD)					AS DWORD

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RelEval/*" />
	METHOD RelEval(info AS DbRelInfo)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RelText/*" />
	METHOD RelText(nRelNum AS DWORD)					AS STRING

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SetRel/*" />
	METHOD SetRel(info AS DbRelInfo)				AS LOGIC

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.SyncChildren/*" />
	METHOD SyncChildren()							AS LOGIC

	// Bulk Operations
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Sort/*" />
	METHOD Sort(info AS DbSortInfo)					AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Trans/*" />
    METHOD Trans(info AS DbTransInfo) 				AS LOGIC

    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.TransRec/*" />
    METHOD TransRec(info AS DbTransInfo) 			AS LOGIC

	// Blob
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.BlobInfo/*" />
	METHOD BlobInfo(uiPos AS DWORD, nOrdinal AS DWORD) AS OBJECT

	// Codeblock Support

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Compile/*" />
	METHOD Compile(sBlock AS STRING)				AS ICodeblock
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.EvalBlock/*" />
	METHOD EvalBlock(oBlock AS ICodeblock)			AS OBJECT


 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Info/*" />
	METHOD Info(nOrdinal AS LONG, oValue AS OBJECT) AS OBJECT

 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RecInfo/*" />
	METHOD RecInfo( nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT

	// Properties
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Alias/*" />
	PROPERTY Alias 		AS STRING	GET SET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Area/*" />
	PROPERTY Area		AS DWORD	GET SET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.BoF/*" />
	PROPERTY BoF 		AS LOGIC	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Deleted/*" />
	PROPERTY Deleted 	AS LOGIC	GET
    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Driver/*" />
    PROPERTY Driver     AS STRING	GET
    /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.EoF/*" />
    PROPERTY EoF 		AS LOGIC	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Exclusive/*" />
	PROPERTY Exclusive	AS LOGIC	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.FieldCount/*" />
	PROPERTY FieldCount AS LONG		GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.FilterText/*" />
	PROPERTY FilterText	AS STRING	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Found/*" />
	PROPERTY Found		AS LOGIC	GET SET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RecCount/*" />
	PROPERTY RecCount	AS DWORD	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RecId/*" />
	PROPERTY RecId		AS OBJECT	GET		// Does not have to be numeric.
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.RecNo/*" />
	PROPERTY RecNo		AS DWORD	GET
 /// <include file="XSharp.Core.Docs.xml" path="doc/IRdd.Shared/*" />
	PROPERTY Shared		AS LOGIC	GET

END INTERFACE

