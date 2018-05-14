//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp.RDD.Enums
INTERFACE XSharp.RDD.IRDD
	// Navigation         
	METHOD DbEval(info AS DbEvalInfo)		AS LOGIC
	METHOD GoTop()							AS LOGIC
	METHOD GoBottom()						AS LOGIC   
	METHOD GoTo(nRec AS LONG)				AS LOGIC
	METHOD GoToId(oRec AS OBJECT)			AS LOGIC
	METHOD Skip(nToSkip AS INT)				AS LOGIC
	METHOD SkipFilter(nToSkip AS INT)		AS LOGIC
	METHOD SkipRaw(nToSkip AS INT)			AS LOGIC 
	METHOD SkipScope(nToSkip AS INT)		AS LOGIC

	// Append and Delete
	METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC
	METHOD Delete()							AS LOGIC   
	METHOD GetRec()							AS BYTE[]  
	METHOD Pack()							AS LOGIC
	METHOD PutRec(aRec AS BYTE[])			AS LOGIC 
	METHOD Recall()							AS LOGIC
	METHOD Zap()							AS LOGIC   
		
	// Open and Close   
	METHOD Close() 							AS LOGIC  
	METHOD Create(info AS DbOpenInfo)		AS LOGIC  
	METHOD Open(info AS DbOpenInfo)			AS LOGIC
		
	// Filtering and Scoping 
	METHOD ClearFilter() 					AS LOGIC
	METHOD ClearScope() 					AS LOGIC 
	METHOD Continue()						AS LOGIC     
	//METHOD CountScope(pOpt as PTR)		AS LONG 
	METHOD GetScope()						AS DbScopeInfo 
	METHOD ScopeInfo(nOrdinal AS LONG)		AS OBJECT
	METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC 
	METHOD SetScope(info AS DbScopeInfo)	AS LOGIC
		
	// Fields                          
	METHOD AddField(info AS DbFieldInfo)	AS LOGIC
	METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
	METHOD FieldIndex(fieldName AS STRING)	AS LONG 
	METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	METHOD FieldName(nFldPos AS LONG)		AS STRING 
		
	// Read & Write		
	/// <summary>Get a value for the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be retrieved.</param>
	METHOD GetValue(nFldPos AS LONG)		AS OBJECT
	/// <summary>Get the value for a column and write (export) it to an external file.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be retrieved.</param>
	/// <param name="fileName">Name of the file that needs to be written to.</param>
	METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	/// <summary>Get the length of the for the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the length should be retrieved.</param>
	METHOD GetValueLength(nFldPos AS LONG)	AS LONG
	/// <summary>Flush the changes to the table, its indexes and memo file.</summary>	
	METHOD Flush() 							AS LOGIC
	METHOD GoCold()							AS LOGIC
	METHOD GoHot()							AS LOGIC   
	/// <summary>Write a value for a specified column</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be written.</param>
	/// <param name="oValue">New value that needs to written to the table this column.</param>
	METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
	/// <summary>Read (Import) a value from an external file and write it to the specified column.</summary>	
	/// <param name="nFldPos">1 based column number for which the value should be written.</param>
	/// <param name="fileName">Name of the file that needs to be read from.</param>
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	
		
	// Locking
	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
	METHOD Lock(uiMode AS DbLockMode)		AS LOGIC 
	METHOD UnLock(oRecId AS OBJECT)			AS LOGIC
	
	// Memo File Access
	/// <summary>Close the memo file</summary>	
	METHOD CloseMemFile() 					AS LOGIC    
	/// <summary>Create the memo file</summary>	
	/// <param name="info">object describing the file to create.</param>
	METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
	/// <summary>Open the memo file </summary>	
	/// <param name="info">object describing the file to open.</param>
	METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC   

	// Indexes
   /// <summary>Set the condition for the next Index Creation</summary>
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
	/// <summary>Create a new index or tag.</summary>
    METHOD OrderCreate(info AS DbOrderCreateInfo)	AS LOGIC	
	/// <summary>Delete an index or tag.</summary>
	METHOD OrderDestroy(info AS DbOrderInfo)		AS LOGIC    	
	/// <summary>Retrieve information about the current index.</summary>
	METHOD OrderInfo(nOrdinal AS LONG)				AS OBJECT
	/// <summary>Open an index file and add to the list of open indexes for the current workarea.</summary>
	METHOD OrderListAdd(info AS DbOrderInfo)		AS LOGIC
	/// <summary>Close an index file and remove it from the list of open indexes for the current workarea.</summary>
	METHOD OrderListDelete(info AS DbOrderInfo)		AS LOGIC
	/// <summary>Set focus to another index in the list open indexes for the current workarea.</summary>
	METHOD OrderListFocus(info AS DbOrderInfo)		AS LOGIC
	/// <summary>Rebuild all indexes for the current workarea.</summary>
	METHOD OrderListRebuild()						AS LOGIC 
	/// <summary>Perform a seek operation on the current selected index for the current workarea.</summary>
	METHOD Seek(info AS DbSeekInfo)					AS LOGIC
    	
	// Relations
	METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC
	METHOD ChildStart(info AS DbRelInfo)			AS LOGIC
	METHOD ChildSync(info AS DbRelInfo)				AS LOGIC
	METHOD ClearRel()								AS LOGIC
	METHOD ForceRel()								AS LOGIC  
	METHOD RelArea(nRelNum AS LONG)					AS LONG 
	METHOD RelEval(info AS DbRelInfo)				AS LOGIC
	METHOD RelText(nRelNum AS LONG)					AS STRING
	METHOD SetRel(info AS DbRelInfo)				AS LOGIC  
	METHOD SyncChildren()							AS LOGIC



	// Bulk Operations
	METHOD Sort(info AS DbSortInfo)					AS LOGIC
    METHOD Trans(info AS DbTransInfo) 				AS LOGIC
    METHOD TransRec(info AS DbTransInfo) 			AS LOGIC
    	
	// Blob
	METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT

	// CodeBlock Support
	METHOD Compile(sBlock AS STRING)				AS LOGIC
	METHOD EvalBlock(oBlock AS OBJECT)				AS OBJECT	

	// Info
	METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  

	// Properties
	PROPERTY Alias 		AS STRING	GET
	PROPERTY Area		as LONG		GET
	PROPERTY BoF 		AS LOGIC	GET
	PROPERTY Deleted 	AS LOGIC	GET
    PROPERTY Driver     AS STRING	GET
    PROPERTY EoF 		AS LOGIC	GET
	PROPERTY Exclusive	AS LOGIC	GET
	PROPERTY FieldCount AS LONG		GET 
	PROPERTY FilterText	AS STRING	GET 
	PROPERTY Found		AS LOGIC	GET SET
	PROPERTY RecCount	AS LONG		GET
	PROPERTY RecId		AS OBJECT	GET		// Does not have to be numeric. 
	PROPERTY RecNo		AS LONG		GET   
	PROPERTY Shared		AS LOGIC	GET
	PROPERTY SysName	AS STRING	GET
	
END INTERFACE	

