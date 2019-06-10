//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
/// <summary>Definition for the interface that each RDD must implement</summary>
/// <seealso cref="T:XSharp.RDD.Workarea"/>
INTERFACE XSharp.RDD.IRdd
	// Navigation         
	/// <summary>Evaluate a code block for each row.</summary>
	/// <param name="info">object containing the information about which rows to visit.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD DbEval(info AS DbEvalInfo)		AS LOGIC

	/// <summary>Position the cursor to the first logical row.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoTop()							AS LOGIC

	/// <summary>Position the cursor to the last logical row.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoBottom()						AS LOGIC   

	/// <summary>Position the cursor to a specific, physical row.</summary>
	/// <param name="nRec">The row number of the new cursor position.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoTo(nRec AS LONG)				AS LOGIC

	/// <summary>Position the cursor to a specific, physical identity.</summary>
	/// <param name="oRec">The row ID of the new cursor position.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoToId(oRec AS OBJECT)			AS LOGIC

	/// <summary>Position the cursor relative to its current position.</summary>
	/// <param name="nToSkip">The number of rows to skip.
    /// If this argument is positive, the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Skip(nToSkip AS INT)				AS LOGIC

	/// <summary>Position the cursor, respecting scope and filter conditions.</summary>
	/// <param name="nToSkip">The number of rows to skip.  If this argument is positive,
    /// the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SkipFilter(nToSkip AS INT)		AS LOGIC

	/// <summary>Position the cursor regardless of scope and filter conditions.</summary>
	/// <param name="nToSkip">The number of rows to skip.  If this argument is positive, the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SkipRaw(nToSkip AS INT)			AS LOGIC 

	/// <summary>Position the cursor relative to its current position within the current scope.</summary>
	/// <param name="nToSkip">The number of rows to skip.  If this argument is positive, the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SkipScope(nToSkip AS INT)		AS LOGIC

	// Append and Delete
	/// <summary>Append a blank row and position the cursor to the new row.</summary>
	/// <param name="lReleaseLock">A flag that is TRUE if you want to clear all pending row locks before appending the new row and FALSE if you want to add the new row to the end of the current lock list.</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Append(lReleaseLock AS LOGIC)	AS LOGIC

	/// <summary>Mark the row at the current cursor position for deletion.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Delete()							AS LOGIC   

	/// <summary>Retrieve the RDD's record buffer as array of bytes.</summary>
    /// <returns>Record buffer of the RDD. This is the original buffer, so changing this buffer may cause corruption !</returns>
	METHOD GetRec()							AS BYTE[]  

	/// <summary>Physically remove rows marked for deletion.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Pack()							AS LOGIC

	/// <summary>Replace the row at the current cursor position with the contents of a byte array.</summary>
	/// <param name="aRec">The buffer containing the information that you want to write.</param>
	/// <note>The buffer should have at least the # of bytes that matches the current record size.</note>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD PutRec(aRec AS BYTE[])			AS LOGIC 

	/// <summary>Remove the deletion marker from the row at the current cursor position.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD Recall()							AS LOGIC

	/// <summary>Physically remove all rows from a table.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD Zap()							AS LOGIC   
		
	// Open and Close   
	/// <summary>Close a table.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Close() 							AS LOGIC  

	/// <summary>Create a table.</summary>
	/// <param name="info">object describing the file to create.</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Create(info AS DbOpenInfo)		AS LOGIC  

	/// <summary>Open a table.</summary>
	/// <param name="info">object describing the file to open.</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Open(info AS DbOpenInfo)			AS LOGIC
		
	// Filtering and Scoping 
	/// <summary>Clear the active filter condition.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ClearFilter() 					AS LOGIC

	/// <summary>Clear the active locate condition.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ClearScope() 					AS LOGIC 

	/// <summary>Goto the next record based on the corrent locate condition.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Continue()						AS LOGIC     

	//METHOD CountScope(pOpt as PTR)		AS LONG 
	/// <summary>Retrieve the active locate condition.</summary>
    /// <returns>The current scope information for the RDD.</returns>
	METHOD GetScope()						AS DbScopeInfo 

	/// <summary>Set the filter condition.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SetFilter(info AS DbFilterInfo)	AS LOGIC 

	/// <summary>Set the locate condition.</summary>
	/// <param name="info">object containing the scope information.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SetScope(info AS DbScopeInfo)	AS LOGIC
		
	// Fields                          
	/// <summary>Add a column.</summary>
    /// <param name="info">RddFieldInfo object.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD AddField(info AS RddFieldInfo)	AS LOGIC

	/// <summary>Add columns defined in an array.</summary>
    /// <param name="aFields">Array of RddFieldInfo objects.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC

	/// <summary>Return the ONE based field index for a field name.</summary>
    /// <param name="fieldName">The name of the field in the database file structure. When the name is not found it will also check for a field alias with this name.</param>
    /// <returns>The ONE based position of the field in the database file structure.</returns>
	METHOD FieldIndex(fieldName AS STRING)	AS LONG 

	/// <summary>Retrieve and optionally change information about a column.</summary>
    /// <param name="nFldPos">The ONE based position of the field in the database file structure.</param>
    /// <param name="nOrdinal">Specifies the type of information.</param>
    /// <param name="oNewValue">If specified (not null), then this parameter is used to change the value of a setting.</param>
    /// <returns>The current value for the property indicated with &lt;nOrdinal&gt;</returns>
	METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT

	/// <summary>Retrieve a column name based on its ONE based column number.</summary>
	/// <param name="nFldPos">The ONE based position of the column whose name you want to obtain.</param>
    /// <returns>The name of the column.</returns>
	METHOD FieldName(nFldPos AS LONG)		AS STRING

    /// <param name="nFldPos">The ONE based position of the column whose info you want to obtain.</param>
    /// <returns>The Field information for the specified field.</returns>
    METHOD GetField(nFldPos AS LONG) AS RDDFieldInfo
	// Read & Write		

    /// <summary>Get a value for the specified column.</summary>	
	/// <param name="nFldPos">The ONE based position of the column whose value you want to obtain.</param>
    /// <returns>The value of the specified field.</returns>
	METHOD GetValue(nFldPos AS LONG)		AS OBJECT

    /// <summary>Get the value for a column and write (export) it to an external file.</summary>	
	/// <param name="nFldPos">ONE based position for which the value should be retrieved.</param>
	/// <param name="fileName">Name of the file that needs to be written to.</param>
    /// <returns>TRUE when the operation succeeded. Otherwise FALSE to indicate a failure.</returns>
	METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC

    /// <summary>Get the length of the for the specified column.</summary>	
	/// <param name="nFldPos">ONE based position for which the length should be retrieved.</param>
    /// <returns>The length of the value of the specified field.</returns>
	METHOD GetValueLength(nFldPos AS LONG)	AS LONG

    /// <summary>Flush the changes to the table, its indexes and memo file.</summary>	
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD Flush() 							AS LOGIC

    /// <summary>Write the contents of a work area's memory to the data store (usually a disk).</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoCold()							AS LOGIC

	/// <summary>Mark a data buffer as hot, indicating that it needs to be written to the data store.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD GoHot()							AS LOGIC   

	/// <summary>Write a value for a specified column</summary>	
	/// <param name="nFldPos">ONE based position for which the value should be written.</param>
	/// <param name="oValue">New value that needs to written to the table this column.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC

	/// <summary>Read (Import) a value from an external file and write it to the specified column.</summary>	
	/// <param name="nFldPos">ONE based position for which the value should be written.</param>
	/// <param name="fileName">Name of the file that needs to be read from.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC

	/// <summary>Discard all changes to the current record and reread the buffer from disk. When the current buffer is in Append mode then the server will move to the bottom of the file.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Refresh() 							AS LOGIC
	// Locking

	/// <summary>Add a newly appended row to the list of locked rows.</summary>
	/// <param name="uiMode">An enum value specifying the kind of lock to acquire.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  

	/// <summary>Lock or unlock the header of a database file.</summary>
	/// <param name="uiMode">An enum value specifying the kind of lock to acquire.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  

	/// <summary>Perform a lock.</summary>
	/// <param name="uiMode">An object specifying the kind of lock to acquire.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Lock(uiMode REF DBLOCKINFO)		AS LOGIC 

	/// <summary>Release locks.</summary>
	/// <param name="oRecId">The row number to unlock.  This argument is provided for drivers that support multiple row locks.  By convention, a value of zero indicates that all rows should be unlocked.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD UnLock(oRecId AS OBJECT)			AS LOGIC
	
	// Memo File Access
	/// <summary>Close the memo file</summary>	
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD CloseMemFile() 					AS LOGIC    

	/// <summary>Create the memo file</summary>	
	/// <param name="info">object describing the file to create.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC

	/// <summary>Open the memo file </summary>	
	/// <param name="info">object describing the file to open.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OpenMemFile(info AS DbOpenInfo) 		AS LOGIC   

	// Indexes

	/// <summary>Set the condition for the next Index Creation</summary>
    /// <param name="info">An object containing information for the order condition.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC

	/// <summary>Create a new index or tag.</summary>
	/// <param name="info">An object containing information for order creation.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
	
	/// <summary>Delete an index or tag.</summary>
	/// <param name="info">An object containing information about the order to remove.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	

	/// <summary>Retrieve information about the current index.</summary>
	/// <param name="info">An object containing information about the order to retrieve the info for.</param>
	/// <param name="nOrdinal">Specifies the type of information to retrieve.</param>
    /// <returns>The requested value for the specified index.</returns>
	METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT

	/// <summary>Open an index file and add to the list of open indexes for the current workarea.</summary>
	/// <param name="info">An object containing information about the orderlist (file)  to add.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC

	/// <summary>Close an index file and remove it from the list of open indexes for the current workarea.</summary>
	/// <param name="info"></param>
	/// <param name="info">An object containing information about the orderlist (file) to delete.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC

	/// <summary>Set focus to another index in the list open indexes for the current workarea.</summary>
	/// <param name="info">An object containing information about the order to select.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC

	/// <summary>Rebuild all indexes for the current workarea.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD OrderListRebuild() AS LOGIC 

	/// <summary>Perform a seek operation on the current selected index for the current workarea.</summary>
	/// <param name="info">An object containing containing the necessary seek information.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>The result of the actial seek operation is stored in the Found property of the RDD and the EOF property.</remarks>
	METHOD Seek(info AS DbSeekInfo) AS LOGIC
    	
	// Relations

	/// <summary>Report the initialization of a relation.</summary>
	/// <param name="info">An object containing information about the relation</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ChildEnd(info AS DbRelInfo)				AS LOGIC 

	/// <summary>Report the initialization of a relation.</summary>
	/// <param name="info">An object containing information about the relation</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ChildStart(info AS DbRelInfo)			AS LOGIC

	/// <summary>Post a pending relational movement, indicating that the specified child work area has been affected by a parental movement.</summary>
	/// <param name="info">An object containing information about the relation</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ChildSync(info AS DbRelInfo)				AS LOGIC

	/// <summary>Clear relations.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ClearRel()								AS LOGIC

	/// <summary>Force all pending relational seeks to be performed.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD ForceRel()								AS LOGIC  

	/// <summary>Retrieve the logical number of a related work area.</summary>
	/// <param name="nRelNum">The ONE based relation number for which to obtain the logical number.</param>
    /// <returns>The area number of the relation.</returns>
	METHOD RelArea(nRelNum AS DWORD)					AS DWORD 

	/// <summary>Evaluate a code block against the relation in a work area.</summary>
	/// <param name="info">An object containing information about the relation</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD RelEval(info AS DbRelInfo)				AS LOGIC

	/// <summary>Retrieve the key expression of a relation.</summary>
	/// <param name="nRelNum">The ONE based relation number for which to obtain the key expression. </param>
    /// <returns>The key expression of the relation.</returns>
	METHOD RelText(nRelNum AS DWORD)					AS STRING

	/// <summary>Set a relation.</summary>
	/// <param name="info">An object containing information about the relation</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SetRel(info AS DbRelInfo)				AS LOGIC  

	/// <summary>Force relational movement in child work areas to synchronize them with the parent work area.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD SyncChildren()							AS LOGIC

	// Bulk Operations
	/// <summary>Physically reorder a table.</summary>
	/// <param name="info">An object containing information about now to sort the table.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	METHOD Sort(info AS DbSortInfo)					AS LOGIC

	/// <summary>Copy one or more rows from one work area to another.</summary>
	/// <param name="info">An object containing information about the transfer of data.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD Trans(info AS DbTransInfo) 				AS LOGIC

	/// <summary>Copy a single row from one work area to another.</summary>
	/// <param name="info">An object containing information about the transfer of data.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    METHOD TransRec(info AS DbTransInfo) 			AS LOGIC
    	
	// Blob
	/// <summary>Retrieve information about a memo column.</summary>
    /// <param name="nOrdinal">Specifies the type of information.</param>
	METHOD BlobInfo(uiPos AS DWORD, nOrdinal AS DWORD) AS OBJECT

	// Codeblock Support
	/// <summary>Compile an expression.</summary>
	/// <param name="sBlock">The expression to compile.</param>

	METHOD Compile(sBlock AS STRING)				AS ICodeblock
	/// <summary>Evaluate a code block.</summary>
	/// <param name="sBlock">The code block to evaluate.</param>
	METHOD EvalBlock(oBlock AS ICodeblock)			AS OBJECT	

	// Info
	/// <summary>Retrieve and optionally change information about a work area.</summary>
	METHOD Info(nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	/// <summary>Retrieve and optionally change information about a row.</summary>
	METHOD RecInfo( nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT  

	// Properties
	/// <summary>Retrieve the alias name.</summary>
	PROPERTY Alias 		AS STRING	GET SET
	/// <summary>Retrieve the workarea number.</summary>
	PROPERTY Area		AS DWORD	GET SET
	/// <summary>Is the table at the logical beginning-of-file.</summary>
	PROPERTY BoF 		AS LOGIC	GET
	/// <summary>Is the current row deleted?</summary>
	PROPERTY Deleted 	AS LOGIC	GET
	/// <summary>Driver (RDD) name of the object.</summary>
    PROPERTY Driver     AS STRING	GET
	/// <summary>Is the table at the logical end-of-file.</summary>
    PROPERTY EoF 		AS LOGIC	GET
	/// <summary>Is the workarea opened Exclusively</summary>
	PROPERTY Exclusive	AS LOGIC	GET
	/// <summary>The # of fields in the current workarea.</summary>
	PROPERTY FieldCount AS LONG		GET 
	/// <summary>The filter condition as a string.</summary>
	PROPERTY FilterText	AS STRING	GET 
	/// <summary>The outcome of the last search operation.</summary>
	PROPERTY Found		AS LOGIC	GET SET
	/// <summary>The number of rows.</summary>
	PROPERTY RecCount	AS LONG		GET
	/// <summary>The row identifier at the current cursor position.</summary>
	PROPERTY RecId		AS OBJECT	GET		// Does not have to be numeric. 
	/// <summary>The physical row identifier at the current cursor position.</summary>
	PROPERTY RecNo		AS LONG		GET   
	/// <summary>Is the current workarea opened Shared?</summary>
	PROPERTY Shared		AS LOGIC	GET
	
END INTERFACE	

