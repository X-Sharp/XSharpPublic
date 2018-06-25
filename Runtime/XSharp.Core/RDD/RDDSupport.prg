//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
// The classes below are simple. No properties, but all public fields.

BEGIN NAMESPACE XSharp.RDD

/// <summary>Helper class to store the scope and codeblock for a DbEval() operation. </summary> 
CLASS DbEvalInfo  
	
	/// <summary>A code block to be evaluated with DbEval() on each row of the work area that is in the range defined by ScopeInfo.  </summary>
	PUBLIC Block	 AS ICodeBlock

	/// <summary>A DbScopeInfo structure limiting the evaluation of Block.</summary>
	PUBLIC ScopeInfo AS DbScopeInfo
END CLASS 


/// <summary>Helper class to store a filter condition for a table.</summary> 
CLASS DbFilterInfo
	/// <summary>A string representing the source code for itmCobExpr.</summary>
	PUBLIC FilterText	AS STRING
	
	/// <summary>A code block representing the condition that is evaluated at each cursor location.  If the result of the evaluation is FALSE, the cursor location requested is invalid according to the current filter condition.</summary>
	PUBLIC FilterBlock	AS ICodeBlock
	
	/// <summary>A flag that is TRUE if a filter is active.</summary>
	PUBLIC Active		AS LOGIC
	
	/// <summary>A flag that is TRUE if a filter is optimized.</summary>
	PUBLIC Optimized	AS LOGIC

	///<summary>Clear the filter fields.</summary>
	METHOD Clear() AS VOID
		SELF:FilterBlock := NULL
		SELF:FilterText	 := NULL
		SELF:Active		 := FALSE
		SELF:Optimized	 := FALSE
		RETURN   

	///<summary>Clone the filter object.</summary>
	METHOD Clone AS DbFilterInfo
		LOCAL oCLone AS DbFilterInfo
		oCLone := DbFilterInfo{}
		oCLone:FilterBlock := SELF:FilterBlock
		oClone:FilterText  := SELF:FilterText
		oClone:Optimized   := SELF:Optimized
		oClone:Active	   := FALSE
		RETURN oClone
END CLASS 

/// <summary>Helper structure to store information needed to lock a row or table for exclusive access.</summary>                 
STRUCTURE DbLockInfo
	/// <summary>An Item indicating the ID of the row to lock.  This member is meaningful only if Method is set to EXCLUSIVE or MULTIPLE. </summary>
	PUBLIC RecId		AS OBJECT
	
	/// <summary>A constant indicating the type of lock to obtain.  The possible values are of the Lockmethod enum. </summary>
	PUBLIC @@Method		AS LockMethod
	
	/// <summary>A flag that is TRUE if the lock operation was successful.</summary>
	PUBLIC Result		AS LOGIC
	/// <summary>List of possible Locking Methods </summary>
	ENUM LockMethod
		/// <summary>Lock a row, releasing currently locked rows.</summary>
		MEMBER Exclusive := 1
		/// <summary>Lock a row, maintaining currently locked rows.</summary>
		MEMBER Multiple  := 2
		/// <summary>Lock a table, releasing locks currently held.</summary>
		MEMBER File 	 := 3
	END ENUM	
END STRUCTURE

/// <summary>Helper class to store information needed to open a table.</summary> 
CLASS DbOpenInfo  
	/// <summary>Should the table be opened Readonly?</summary>
	PUBLIC ReadOnly 	AS LOGIC
	/// <summary>Should the table be opened Shared?</summary>
	PUBLIC Shared		AS LOGIC
	/// <summary>Unique Alias that the table has. This must be a valid XBase Identifier</summary>
	PUBLIC Alias		AS STRING
	/// <summary>The extension for the table that must be opened.</summary>
	PUBLIC Extension    AS STRING
	/// <summary>The filename (optionally includig a path) for the table that must be opened.</summary>
	PUBLIC FileName		AS STRING
	/// <summary>Workarea number in which the table will be opened.</summary>
	PUBLIC WorkArea		AS LONG    
	
	CONSTRUCTOR()
		
	CONSTRUCTOR(sFileName AS STRING, sAlias AS STRING, liWorkArea AS LONG, lShared AS LOGIC, lReadOnly AS LOGIC)
		FileName 	:= sFileName
        Extension   := Path.GetExtension(sFileName)
		Alias	 	:= sAlias
		WorkArea	:= liWorkArea
		Shared		:= lShared
		ReadOnly	:= lReadOnly
	/// <summary>Return the numeric FileMode based on the Shared and Readonly flags </summary>
	public property FileMode as DWORD 
	get
		local nMode as DWORD
		nMode := FO_COMPAT
		if (Shared)
			nMode |= FO_SHARED
		else
			nMode |= FO_EXCLUSIVE
		endif
		if (ReadOnly)
			nMode |= FO_READ
		else
			nMode |= FO_READWRITE
		endif
		return nMode
	end get
	end property
END CLASS  

/// <summary>Helper class to store information needed to create a conditional order.</summary> 
CLASS DbOrderCondInfo   
	/// <summary> A flag that is TRUE if one or more valid conditions have been specified in the structure. </summary>
	PUBLIC Active			AS LOGIC
	/// <summary> A flag that is TRUE if open orders should remain open while the new order is being created. </summary>
	PUBLIC Additive			AS LOGIC
	/// <summary> A flag that is TRUE if all rows are to be processed during order creation. </summary>
	PUBLIC All				AS LOGIC
	/// <summary>A flag that is TRUE if the new order will be a custom built order.</summary>
	PUBLIC Custom			AS LOGIC
	/// <summary> A flag that is TRUE if the order should be created in descending order. </summary>
	PUBLIC @@Descending 	AS LOGIC
	/// <summary>A code block defining the expression to evaluate every StepSize rows during the creation of the order.  The code block referenced should return a logical value: TRUE indicates that creation of the order should continue normally, and FALSE indicates that order creation should terminate. </summary>
	PUBLIC EvalBlock		AS ICodeBlock 
	/// <summary>A code block defining the for condition to use for the creation and maintenance of the order.</summary>
	PUBLIC ForBlock			AS ICodeBlock 
	/// <summary>A string defining the for condition to use for the creation and maintenance of the order.</summary>
	PUBLIC ForExpression	AS STRING    
	/// <summary>The number of rows to process for order creation.</summary>
	PUBLIC NextCount		AS LONG
	/// <summary>A flag that is TRUE if the for condition may NOT be optimized,</summary>
	PUBLIC NoOptimize		AS LOGIC
	/// <summary> A single row number to include in the order. </summary>
	PUBLIC RecNo			AS LONG
	/// <summary>A flag that is TRUE if only the rows specified by lStartRecno through end-of-file are to be included in the order.</summary>
	PUBLIC Rest				AS LOGIC
	/// <summary>A flag indicating whether the order is to be scoped. fScoped will be TRUE if WhileBlock, NextCount, Recno, Rest, or All is specified.</summary>
	PUBLIC Scoped			AS LOGIC
	/// <summary>The row at which to begin processing when either the NextCount or Rest scoping options are specified.</summary>
	PUBLIC StartRecNo		AS LONG
	/// <summary>The frequency of the evaluation of EvalBlock. </summary>
	PUBLIC StepSize			AS LONG
	/// <summary>A flag that is TRUE if only rows in the controlling order are to be included in the order.</summary>
	PUBLIC UseCurrent		AS LOGIC
	/// <summary>A code block defining the while condition to use for the creation of the order.  An empty value indicates that no while condition is being imposed.</summary>
	PUBLIC WhileBlock		AS ICodeBlock 	
END CLASS

/// <summary>Helper class to store information needed to create a new order.</summary> 
CLASS DbOrderCreateInfo 
	/// <summary>The index file name.</summary>
	PUBLIC BagName		AS STRING
	/// <summary>The order name or number to create in BagName.</summary>
	PUBLIC Order		AS OBJECT
	/// <summary>The key expression defining the order.</summary>
	PUBLIC Expression	AS STRING
	/// <summary>A flag that is TRUE if the order should contain only unique keys. </summary>
	PUBLIC Unique		AS LOGIC
	/// <summary>A code block containing the key expression defining the order imposed on the work area. </summary>
	PUBLIC Block		AS ICodeBlock
	/// <summary>A DbOrderCondInfo object containing information about the condition (if any) for the order. </summary>
	PUBLIC OrdCondInfo	AS DbOrderCondInfo
END CLASS

/// <summary>Helper class to store information needed to open/address an order.</summary> 
CLASS DbOrderInfo  
	/// <summary>A flag that is TRUE if all tags of the index file must be opened.</summary>
	PUBLIC AllTags		AS LOGIC
	/// <summary>A code block containing the key expression defining the order imposed on the work area.</summary>
	PUBLIC Expression	AS ICodeBlock
	/// <summary>An object containing the order name or number</summary>
	PUBLIC Order		AS OBJECT
	/// <summary>The index file name.</summary>
	PUBLIC BagName		AS STRING
	//PUBLIC Result		AS OBJECT 	// Is this needed ?
END CLASS

/// <summary>Helper class to store a list of relational information.</summary> 
CLASS DbRelInfo 
	/// <summary>The expression used to reposition the cursor of the child table when this relation is resolved.</summary>
	PUBLIC Key			AS STRING
	/// <summary>A code block used to reposition the cursor of the child table when this relation is resolved.</summary>
	PUBLIC Block		AS ICodeBlock
	/// <summary>A reference to the child RDD for the relation.</summary>
	PUBLIC Child		AS IRDD	
	/// <summary>A reference to the parent RDD for the relation.</summary>
	PUBLIC Parent		AS IRDD
END CLASS

/// <summary>Helper class to store references to all of the scope clause expressions. </summary> 
CLASS DbScopeInfo 
	/// <summary>A flag that is TRUE if a process should ignore duplicate key values.</summary>
	PUBLIC IgnoreDuplicates AS LOGIC
	/// <summary>A flag that is TRUE if a process should ignore any filter condition imposed on the current work area. </summary>
	PUBLIC IgnoreFilter		AS LOGIC
	/// <summary> A flag that is TRUE if a process should include deleted rows. </summary>
	PUBLIC IncludeDeleted	AS LOGIC
	/// <summary>A code block representing the conditional for clause.  A for condition is, essentially, a filter that hides rows for which the condition evaluates to FALSE.  The string value is provided for storage, while the code block is provided as a parameter for the EvalBlock() method.</summary>
	PUBLIC ForBlock			AS ICodeBlock
	/// <summary>A string representing the conditional for clause.  A for condition is, essentially, a filter that hides rows for which the condition evaluates to FALSE.  The string value is provided for storage, while the code block is provided as a parameter for the EvalBlock() method.</summary>
	PUBLIC ForExpression	AS STRING
	/// <summary>A flag that is TRUE if the last row of the current scope is required. </summary>
	PUBLIC Last				AS LOGIC 
	/// <summary>Permits continuation of a process for the next lNext rows, while obeying for and while clauses.</summary>
	PUBLIC NextCount		AS LONG
	/// <summary>Permits continuation of a process for a single row number, while obeying for and while clauses.</summary>
	PUBLIC RecId			AS OBJECT
	/// <summary>A flag that is TRUE if a process should continue stepping through data from the current work area cursor position until logical end-of-file. </summary>
	PUBLIC Rest				AS LOGIC    
	/// <summary>A code block representing the conditional while clause.  A while condition permits continuation of a process that steps through rows until the condition evaluates to FALSE.  The string value is provided for storage, while the code block is provided as a parameter for the EvalBlock() method.</summary>
	PUBLIC WhileBlock		AS ICodeBlock
	/// <summary>A string representing the conditional while clause.  A while condition permits continuation of a process that steps through rows until the condition evaluates to FALSE.  The string value is provided for storage, while the code block is provided as a parameter for the EvalBlock() method.</summary>
	PUBLIC WhileExpression	AS STRING    
	
	CONSTRUCTOR()
		SELF:Clear()

	///<summary>Clear the scope fields.</summary>
	PUBLIC METHOD Clear() AS VOID
		IgnoreDuplicates := FALSE
		IgnoreFilter	 := FALSE
		IncludeDeleted 	 := FALSE
		ForBlock		 := NULL
		ForExpression	 := NULL
		Last			 := FALSE
		NextCount		 := 0
		RecId			 := NULL
		Rest			 := FALSE
		WhileBlock		 := NULL
		WhileExpression  := NULL

	///<summary>Clone the scopeinfo object.</summary>
	METHOD Clone AS DbScopeInfo
		LOCAL oClone AS DbScopeInfo
		oClone := DbScopeInfo{}
		oClone:IgnoreDuplicates     := SELF:IgnoreDuplicates 
		oClone:IgnoreFilter	        := SELF:IgnoreFilter	 
		oClone:IncludeDeleted       := SELF:IncludeDeleted
		oClone:ForBlock		        := SELF:ForBlock		
		oClone:ForExpression        := SELF:ForExpression
		oClone:Last			        := SELF:Last			
		oClone:NextCount	        := SELF:NextCount	
		oClone:RecId		        := SELF:RecId		
		oClone:Rest			        := SELF:Rest			 
		oClone:WhileBlock		    := SELF:WhileBlock		
		oClone:WhileExpression      := SELF:WhileExpression  
		RETURN oClone
		
END CLASS

/// <summary>Helper structure to store information needed to perform a seek operation </summary> 
STRUCTURE DbSeekInfo
	/// <summary>A flag that is TRUE if the last occurrence of the specified key value is to be sought, rather than the first.</summary>
	PUBLIC Last 	AS LOGIC
	/// <summary>A flag that is TRUE if a soft seek is to be performed. </summary>
	PUBLIC SoftSeek AS LOGIC
	/// <summary>An object containing the key value to find.</summary>
	PUBLIC Value	AS OBJECT
END STRUCTURE

/// <summary>Helper class to store information needed to perform a physical sort. </summary> 
CLASS DbSortInfo 
	/// <summary>A DbTransInfo object holding the destination work area, column transfer information, and scoping information for the Sort() method. </summary>
	PUBLIC TransInfo AS DbTransInfo
	/// <summary>An array of DbSortItem structures defining the key values for the sort.  Note that the key values are processed in the order that they appear in this array. </summary>
	PUBLIC Items	 AS DbSortItem[]
END CLASS

/// <summary>Helper structure to store information about a single sort key value. </summary> 
STRUCTURE DbSortItem  
	/// <summary>A one-based index indicating the column on which the sort is based. </summary>
	PUBLIC FieldNo 	AS LONG
	/// <summary>The offset of the field in the workarea buffer.</summary>
	PUBLIC OffSet	AS LONG
	/// <summary>The length of the field in the workarea buffer.</summary>
	PUBLIC Length	AS LONG
	/// <summary>One or more constants that function as sort optimization and control flags.  They are passed to your RDD Sort() routine from the high-level wrapper function for the DBSort() function.</summary>
	PUBLIC Flags	AS LONG  
	/// <summary>An ascending sort (default)   </summary>
	PUBLIC CONST SF_Default := 0 AS LONG
	/// <summary> A case-insensitive sort        </summary>
	PUBLIC CONST SF_Case	:= 1 AS LONG
	/// <summary>A sort with printable numerics        </summary>
	PUBLIC CONST SF_Numeric := 2 AS LONG
	/// <summary>A sort for ASCII (not nation-dependent)</summary>
	PUBLIC CONST SF_Ascii	 := 4 AS LONG       
	/// <summary>A sort with long integer values        </summary>
	PUBLIC CONST SF_Long	   := 0x80 AS LONG
	/// <summary>A descending sort        </summary>
	PUBLIC CONST SF_Descending := 0x100 AS LONG
END STRUCTURE

/// <summary>Helper class to store information needed for the global transfer of data items from one work area to another. </summary> 
CLASS DbTransInfo  
	/// <summary>A DbScopeInfo object describing the limits of the scope of the transfer. </summary>
	PUBLIC Scope		AS DbScopeInfo
	/// <summary>The source work area. </summary>
	PUBLIC Source		AS IRDD
	/// <summary>The destination work area. </summary>
	PUBLIC Destination 	AS IRDD
	/// <summary>An array of DbTransItem structures defining the items to transfer to the destination work area. This is usually a list of column mappings from the source to the destination. </summary>
	PUBLIC Items		AS DbTransItem[]
	/// <summary>Transfer attributes specified using one or more of the constants Match or PutRec. </summary>
	PUBLIC Flags		AS LONG
	/// <summary>Both this work area and the destination work area have identical row structures (i.e., all columns match).</summary>
	PUBLIC CONST Match	:= 1 AS LONG
	/// <summary>The RDD has the ability to transfer an entire row.</summary>
	PUBLIC CONST PutRec	:= 2 AS LONG
END CLASS

/// <summary>Helper structure to store information about a single piece of data (usually a column) to transfer from one work area to another.</summary> 
STRUCTURE DbTransItem
	/// <summary>A one-based field index in the source work area. </summary>
	PUBLIC Source 		AS LONG
	/// <summary>A one-based field index in the destination work area. </summary>
	PUBLIC Destination 	AS LONG	
END STRUCTURE

/// <summary> </summary> 
CLASS RddFieldInfo
	PUBLIC Name 		AS STRING
	PUBLIC FieldType 	AS DBFieldType
	PUBLIC Length 		AS LONG
	PUBLIC Decimals 	AS LONG
	PUBLIC Alias 		AS STRING
	CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG)
		Name 		:= sName
		Length 		:= nLength
		Decimals 	:= nDecimals                            
		IF !String.IsNullOrEmpty(sType)
			FieldType := (DbFieldType) Char.ToUpper(sType[0])
		ELSE
			FieldType := DBFieldType.Unknown
		ENDIF  
		Alias       := sName
		RETURN
	CONSTRUCTOR(sName AS STRING, nType AS DbFieldType, nLength AS LONG, nDecimals AS LONG)
		Name 		:= sName                                
		FieldType 	:= nType
		Length 		:= nLength
		Decimals 	:= nDecimals
		Alias       := sName
		RETURN
	
END CLASS

CLASS DbJoinList
END CLASS

CLASS OrderStatus
END CLASS

CLASS RddList
END CLASS


CLASS DbFieldNames
END CLASS


CLASS DbWORKAREASTATUS
END CLASS




END NAMESPACE




