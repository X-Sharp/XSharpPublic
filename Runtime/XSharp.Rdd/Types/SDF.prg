//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#ifdef COMPLETED
USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD
/// <summary>SDF RDD. For reading and writing text files.</summary>
CLASS SDF INHERIT Workarea  
	CONSTRUCTOR
		SUPER()     
		SELF:_hFile    := IntPtr.Zero
		SELF:_TransRec := TRUE
		SELF:_RecordLength := 0
//	METHOD DbEval(info AS DbEvalInfo) AS LOGIC
/// <inheritdoc />
METHOD GoTop() AS LOGIC
	THROW NotImplementedException{}
//	METHOD GoBottom() AS LOGIC   
/// <inheritdoc />
METHOD GoTo(nRec AS LONG) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD GoToId(oRec AS OBJECT) AS LOGIC
	THROW NotImplementedException{}
//	METHOD Skip(nToSkip AS INT) AS LOGIC
//	METHOD SkipFilter(nToSkip AS INT) AS LOGIC
/// <inheritdoc />
METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
	THROW NotImplementedException{}
//	METHOD SkipScope(nToSkip AS INT) AS LOGIC

	// Append and Delete
/// <inheritdoc />
METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Delete() AS LOGIC   
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD GetRec() AS BYTE[]  
	THROW NotImplementedException{}
//	METHOD Pack() AS LOGIC
/// <inheritdoc />
METHOD PutRec(aRec AS BYTE[]) AS LOGIC 
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Recall() AS LOGIC
	THROW NotImplementedException{}
//	METHOD Zap() AS LOGIC   
		
	// Open and Close   
/// <inheritdoc />
METHOD Close() 			AS LOGIC      
	IF SELF:_hFile   != IntPtr.Zero
		SELF:GoCold()
		IF !SELF:_ReadOnly // && hb_setGetEOF
			FWrite(SELF:_hFile, " ", 1)
			SELF:_Flush := TRUE
		ENDIF                  
		SELF:Flush()
		FClose(SELF:_hFile)
		SELF:_hFile := IntPtr.Zero
	ENDIF
	RETURN SUPER:Close()
/// <inheritdoc />
METHOD Create(info AS DbOpenInfo) AS LOGIC  
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Open(info AS DbOpenInfo) AS LOGIC
	THROW NotImplementedException{}		
// Filtering and Scoping 
//	METHOD ClearFilter() 	AS LOGIC
//	METHOD ClearScope() 	AS LOGIC 
//	METHOD Continue()		AS LOGIC     
//	METHOD GetScope()		AS DbScopeInfo 
//	METHOD SetFilter(info AS DbFilterInfo) AS LOGIC 
//	METHOD SetScope(info AS DbScopeInfo) AS LOGIC
	// Fields
//	METHOD CreateFields(aFields AS DbField[]) AS LOGIC
//	METHOD FieldIndex(fieldName AS STRING) AS LONG 
//  METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
//	METHOD FieldName(nFldPos AS LONG) AS STRING 
/// <inheritdoc />
METHOD GetValue(nFldPos AS INT) AS OBJECT
	THROW NotImplementedException{}
//  METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC

//	METHOD GetValueLength(nFldPos AS INT) AS INT
/// <inheritdoc />
METHOD Flush() 			AS LOGIC
	THROW NotImplementedException{}

/// <inheritdoc />
METHOD GoCold()			AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD GoHot()			AS LOGIC   
	THROW NotImplementedException{}

/// <inheritdoc />
METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	THROW NotImplementedException{}
// METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	// Locking
//	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
//	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
//	METHOD Lock(uiMode AS DbLockMode) AS LOGIC 
//	METHOD UnLock(oRecId AS OBJECT) AS LOGIC
	// Memo File Access 
// METHOD CloseMemFile( ) AS LOGIC

// METHOD CreateMemFile(info AS XSharp.RDD.DbOpenInfo) AS LOGIC

//	METHOD OpenMemFile( ) AS LOGIC
	// Indexes
//  METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
//  METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
//	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
//	METHOD OrderInfo(nOrdinal AS DWORD) AS OBJECT
//	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
//	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
//	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
//	METHOD OrderListRebuild() AS LOGIC 
//	METHOD Seek(info AS DbSeekInfo) AS LOGIC
	// Relations
//	METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
//	METHOD ChildStart(info AS DbRelInfo) AS LOGIC
//	METHOD ChildSync(info AS DbRelInfo) AS LOGIC
//	METHOD ClearRel() AS LOGIC
//	METHOD ForceRel() AS LOGIC  
//	METHOD RelArea(nRelNum AS LONG) AS LONG 
//	METHOD RelEval(info AS DbRelInfo) AS LOGIC
//	METHOD RelText(nRelNum AS LONG) AS STRING
//	METHOD SetRel(info AS DbRelInfo) AS LOGIC  
//	METHOD SyncChildren() AS LOGIC

	// Trans	
METHOD Trans(info AS DbTransInfo) 		AS LOGIC
	THROW NotImplementedException{}
//    METHOD TransRec(info AS DbTransInfo) 	AS LOGIC
	// Blob
//	METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT

	// Codeblock Support
//	METHOD Compile(sBlock AS STRING) AS LOGIC
//	METHOD EvalBlock(oBlock AS OBJECT) AS OBJECT	
	// Other
/// <inheritdoc />
METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	THROW NotImplementedException{}
//	METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  
//	METHOD Sort(info AS DbSortInfo) AS LOGIC

	// Properties
//	PROPERTY Alias 		AS STRING GET
//	PROPERTY BoF 		AS LOGIC GET
	/// <inheritdoc />
	PROPERTY Deleted 	AS LOGIC GET 	FALSE
//	PROPERTY EoF 		AS LOGIC GET
//	PROPERTY Exclusive	AS LOGIC GET
//	PROPERTY FieldCount AS LONG GET 
//	PROPERTY FilterText	AS STRING GET 
//	PROPERTY Found		AS LOGIC GET 
	/// <inheritdoc />
	PROPERTY RecCount	AS LONG GET		0 
	/// <inheritdoc />
	PROPERTY RecId		AS OBJECT GET   NULL
	/// <inheritdoc />
	PROPERTY RecNo		AS LONG 	GET   0
//	PROPERTY Shared		AS LOGIC GET
/// <inheritdoc />
VIRTUAL PROPERTY SysName AS STRING GET typeof(SDF):ToString()
//	
	// Error Handling
//	PROPERTY LastGenCode	AS LONG GET
//	PROPERTY LastSubCode	AS LONG GET
//	PROPERTY LastError		AS Exception GET

END CLASS
END NAMESPACE
#endif
