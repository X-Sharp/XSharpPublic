//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

INTERFACE XSharp.RDD.IRDD
	// Navigation         
	METHOD DbEval(info AS DbEvalInfo) AS LOGIC
	METHOD GoTop() AS LOGIC
	METHOD GoBottom() AS LOGIC   
	METHOD GoTo(nRec AS LONG) AS LOGIC
	METHOD GoToId(oRec AS OBJECT) AS LOGIC
	METHOD Skip(nToSkip AS INT) AS LOGIC
	METHOD SkipFilter(nToSkip AS INT) AS LOGIC
	METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
	METHOD SkipScope(nToSkip AS INT) AS LOGIC

	// Append and Delete
	METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	METHOD Delete() AS LOGIC   
	METHOD GetRec() AS BYTE[]  
	METHOD Pack() AS LOGIC
	METHOD PutRec(aRec AS BYTE[]) AS LOGIC 
	METHOD Recall() AS LOGIC
	METHOD Zap() AS LOGIC   
		
	// Open and Close   
	METHOD Close() 			AS LOGIC  
	METHOD Create(info AS DbOpenInfo) AS LOGIC  
	METHOD Open(info AS DbOpenInfo) AS LOGIC
		
	// Filtering and Scoping 
	METHOD ClearFilter() 	AS LOGIC
	METHOD ClearScope() 	AS LOGIC 
	METHOD Continue()		AS LOGIC     
	//METHOD CountScope(pOpt as PTR) AS LONG 
	METHOD GetScope()		AS DbScopeInfo 
	METHOD ScopeInfo(nOrdinal AS LONG) AS OBJECT
	METHOD SetFilter(info AS DbFilterInfo) AS LOGIC 
	METHOD SetScope(info AS DbScopeInfo) AS LOGIC
		
	// Fields                          
	METHOD AddField(info AS DbFieldInfo) AS LOGIC
	METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
	METHOD FieldIndex(fieldName AS STRING) AS LONG 
	METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	METHOD FieldName(nFldPos AS LONG) AS STRING 
		
	// Read & Write		
	METHOD GetValue(nFldPos AS LONG) AS OBJECT
	METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	METHOD GetValueLength(nFldPos AS LONG) AS LONG
	METHOD Flush() 			AS LOGIC
	METHOD GoCold()			AS LOGIC
	METHOD GoHot()			AS LOGIC   
	METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	
		
	// Locking
	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
	METHOD Lock(uiMode AS DbLockMode) AS LOGIC 
	METHOD UnLock(oRecId AS OBJECT) AS LOGIC
	
	// Memo File Access 
	METHOD CloseMemFile() 	AS LOGIC    
	METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
	METHOD OpenMemFile() 	AS LOGIC   

	// Indexes
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
	METHOD OrderInfo(nOrdinal AS LONG) AS OBJECT
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
	METHOD OrderListRebuild() AS LOGIC 
	METHOD Seek(info AS DbSeekInfo) AS LOGIC
    	
	// Relations
	METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
	METHOD ChildStart(info AS DbRelInfo) AS LOGIC
	METHOD ChildSync(info AS DbRelInfo) AS LOGIC
	METHOD ClearRel() AS LOGIC
	METHOD ForceRel() AS LOGIC  
	METHOD RelArea(nRelNum AS LONG) AS LONG 
	METHOD RelEval(info AS DbRelInfo) AS LOGIC
	METHOD RelText(nRelNum AS LONG) AS STRING
	METHOD SetRel(info AS DbRelInfo) AS LOGIC  
	METHOD SyncChildren() AS LOGIC

	// Trans	
    METHOD Trans(info AS DbTransInfo) 		AS LOGIC
    METHOD TransRec(info AS DbTransInfo) 	AS LOGIC
    	
	// Blob
	METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT

	// CodeBlock Support
	METHOD Compile(sBlock AS STRING) AS LOGIC
	METHOD EvalBlock(oBlock AS OBJECT) AS OBJECT	

	// Other
	METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  
	METHOD Sort(info AS DbSortInfo) AS LOGIC

	// Properties
	PROPERTY Alias 		AS STRING GET
	PROPERTY Area		as LONG GET
	PROPERTY BoF 		AS LOGIC GET
	PROPERTY Deleted 	AS LOGIC GET
    PROPERTY Driver     AS STRING GET
    PROPERTY EoF 		AS LOGIC GET
	PROPERTY Exclusive	AS LOGIC GET
	PROPERTY FieldCount AS LONG GET 
	PROPERTY FilterText	AS STRING GET 
	PROPERTY Found		AS LOGIC GET SET
	PROPERTY RecCount	AS LONG GET
	PROPERTY RecId		AS OBJECT GET  
	PROPERTY RecNo		AS LONG GET   
	PROPERTY Shared		AS LOGIC GET
	PROPERTY SysName	AS STRING GET
	
			
END INTERFACE	

