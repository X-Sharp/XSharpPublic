//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD
// The classes below are simple. No properties, but all public fields.

BEGIN NAMESPACE XSharp.RDD.Support

/// <summary>Helper class to store the scope and codeblock for a DbEval() operation. </summary> 
CLASS DbEvalInfo  
	
	/// <summary>A code block to be evaluated with DbEval() on each row of the work area that is in the range defined by ScopeInfo.  </summary>
	PUBLIC Block	 AS ICodeblock

	/// <summary>A DbScopeInfo structure limiting the evaluation of Block.</summary>
	PUBLIC ScopeInfo AS DbScopeInfo
    /// <summary>Construct a DbEvalInfo object.</summary>
    CONSTRUCTOR()
        SELF:ScopeInfo := DbScopeInfo{}
        RETURN
END CLASS 


/// <summary>Helper class to store a filter condition for a table.</summary> 
CLASS DbFilterInfo
	/// <summary>A string representing the source code for itmCobExpr.</summary>
	PUBLIC FilterText	AS STRING
	
	/// <summary>A code block representing the condition that is evaluated at each cursor location.  If the result of the evaluation is FALSE, the cursor location requested is invalid according to the current filter condition.</summary>
	PUBLIC FilterBlock	AS ICodeblock
	
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
        
    /// <summary>Construct a DbFilterInfo object.</summary>
    CONSTRUCTOR
        SELF:Clear()
        RETURN

	METHOD Compile(oRDD AS IRDD) AS VOID
        IF SELF:FilterBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:FilterText)
            SELF:FilterBlock := oRDD:Compile(SELF:FilterText)
        ENDIF
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
	PUBLIC WorkArea		AS DWORD    
    /// <summary>Construct a DbOpenInfo object.</summary>
	
	CONSTRUCTOR()
        SUPER()		
    /// <summary>Construct a DbOpenInfo object.</summary>
	CONSTRUCTOR(sFileName AS STRING, sAlias AS STRING, dwWorkArea AS DWORD, lShared AS LOGIC, lReadOnly AS LOGIC)
		FileName 	:= sFileName
        Extension   := Path.GetExtension(sFileName)
		IF String.IsNullOrEmpty( sAlias )
			Alias := Path.GetFileNameWithoutExtension( sFileName )
		ELSE
			Alias	 	:= sAlias
		ENDIF
		WorkArea	:= dwWorkArea
		Shared		:= lShared
		ReadOnly	:= lReadOnly
	/// <summary>Return the numeric FileMode based on the Shared and Readonly flags </summary>
	PUBLIC PROPERTY FileMode AS DWORD 
	GET
		LOCAL nMode AS DWORD
		nMode := FO_COMPAT
		IF (Shared)
			nMode |= FO_SHARED
		ELSE
			nMode |= FO_EXCLUSIVE
		ENDIF
		IF (ReadOnly)
			nMode |= FO_READ
		ELSE
			nMode |= FO_READWRITE
		ENDIF
		RETURN nMode
	END GET
	END PROPERTY
    PUBLIC METHOD Clone() AS DbOpenInfo
        RETURN (DbOpenInfo) SELF:MemberwiseClone()
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
	PUBLIC EvalBlock		AS ICodeblock 
	/// <summary>A code block defining the for condition to use for the creation and maintenance of the order.</summary>
	PUBLIC ForBlock			AS ICodeblock 
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
	PUBLIC WhileBlock		AS ICodeblock
	/// <summary>A string defining the for while condition to use for the creation and maintenance of the order.</summary>
	PUBLIC WhileExpression	AS STRING    

    METHOD Compile(oRDD AS IRDD) AS VOID
        IF SELF:WhileBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:WhileExpression)
            SELF:WhileBlock := oRDD:Compile(SELF:WhileExpression)
        ENDIF
        IF SELF:ForBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:ForExpression)
            SELF:ForBlock := oRDD:Compile(SELF:ForExpression)
        ENDIF
    METHOD Validate() AS VOID
        SELF:Active := FALSE
        SELF:Scoped := FALSE
        IF SELF:All .OR. SELF:ForBlock != NULL .OR. SELF:WhileBlock != NULL .OR.  ! String.IsNullOrEmpty(SELF:ForExpression ) ;
            .OR. SELF:NextCount != 0 .OR. SELF:RecNo != 0 .OR. SELF:Rest  .OR. SELF:Descending .OR. ! String.IsNullOrEmpty(SELF:WhileExpression)
            SELF:Active := TRUE
        ENDIF
        IF SELF:EvalBlock != NULL .AND. SELF:StepSize == 0
            SELF:StepSize := 1
        ENDIF
        IF SELF:Active
            IF SELF:All .OR. SELF:WhileBlock != NULL .OR. SELF:NextCount != 0 .OR. SELF:RecNo != 0 .OR. SELF:Rest
                SELF:Scoped := TRUE
            ENDIF
        ENDIF


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
	PUBLIC Block		AS ICodeblock
	/// <summary>A DbOrderCondInfo object containing information about the condition (if any) for the order. </summary>
	PUBLIC OrdCondInfo	AS DbOrderCondInfo

    METHOD Compile(oRDD AS IRDD) AS VOID
        IF SELF:Block == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:Expression)
            SELF:Block := oRDD:Compile(SELF:Expression)
        ENDIF
        IF SELF:OrdCondInfo != NULL
            SELF:OrdCondInfo:Compile(oRDD)
        ENDIF
END CLASS

/// <summary>Helper class to store information needed to open/address an order.</summary> 
CLASS DbOrderInfo  
	/// <summary>A flag that is TRUE if all tags of the index file must be opened.</summary>
	PUBLIC AllTags		AS LOGIC
	/// <summary>A code block containing the key expression defining the order imposed on the work area.</summary>
	PUBLIC Expression	AS ICodeblock
	/// <summary>An object containing the order name or number</summary>
	PUBLIC Order		AS OBJECT
	/// <summary>The index file name.</summary>
	PUBLIC BagName		AS STRING
	/// <summary>Return value for some order operations.</summary>
	PUBLIC Result		AS OBJECT 	
END CLASS

/// <summary>Helper class to store a list of relational information.</summary> 
CLASS DbRelInfo 
	/// <summary>The expression used to reposition the cursor of the child table when this relation is resolved.</summary>
	PUBLIC Key			AS STRING
	/// <summary>A code block used to reposition the cursor of the child table when this relation is resolved.</summary>
	PUBLIC Block		AS ICodeblock
	/// <summary>A reference to the child RDD for the relation.</summary>
	PUBLIC Child		AS IRDD	
	/// <summary>A reference to the parent RDD for the relation.</summary>
	PUBLIC Parent		AS IRDD

    METHOD Compile() AS VOID
        IF SELF:Block == NULL .AND. SELF:Parent != NULL .AND. ! String.IsNullOrWhiteSpace(SELF:Key)
            SELF:Block := SELF:Parent:Compile(SELF:Key)
        ENDIF
    
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
	PUBLIC ForBlock			AS ICodeblock
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
	PUBLIC WhileBlock		AS ICodeblock
	/// <summary>A string representing the conditional while clause.  A while condition permits continuation of a process that steps through rows until the condition evaluates to FALSE.  The string value is provided for storage, while the code block is provided as a parameter for the EvalBlock() method.</summary>
	PUBLIC WhileExpression	AS STRING    
	/// <summary>Construct a DbScopeInfo object.</summary>
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
	METHOD Compile(oRDD AS IRDD) AS VOID
        IF SELF:WhileBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:WhileExpression)
            SELF:WhileBlock := oRDD:Compile(SELF:WhileExpression)
        ENDIF
        IF SELF:ForBlock == NULL .AND. ! String.IsNullOrWhiteSpace(SELF:ForExpression)
            SELF:ForBlock := oRDD:Compile(SELF:ForExpression)
        ENDIF
END CLASS

/// <summary>Helper structure to store information needed to perform a seek operation </summary> 
STRUCTURE DbSeekInfo
	/// <summary>A flag that is TRUE if the last occurrence of the specified key value is to be sought, rather than the first.</summary>
	PUBLIC Last 	AS LOGIC
	/// <summary>A flag that is TRUE if a soft seek is to be performed. </summary>
	PUBLIC SoftSeek AS LOGIC
	/// <summary>An object containing the key value to find.</summary>
	PUBLIC @@Value AS OBJECT
END STRUCTURE

/// <summary>Helper class to store information needed to perform a physical sort. </summary> 
CLASS DbSortInfo 
	/// <summary>A DbTransInfo object holding the destination work area, column transfer information, and scoping information for the Sort() method. </summary>
	PUBLIC TransInfo  AS DbTransInfo
	/// <summary>An array of DbSortItem structures defining the key values for the sort.  Note that the key values are processed in the order that they appear in this array. </summary>
	PUBLIC Items	  AS DbSortItem[]
	/// <summary>Number of items in the Items array. </summary>
    PROPERTY ItemCount  AS LONG GET Items:Length
    /// <summary>Construct a DbSortInfo object.</summary>
    CONSTRUCTOR(transItemCount AS LONG, sortItemCount AS LONG)
        SELF:TransInfo := DbTransInfo{transItemCount}
        SELF:Items     := DbSortItem[]{sortItemCount}
        RETURN
END CLASS

/// <summary>Helper structure to store information about a single sort key value. </summary> 
STRUCTURE DbSortItem  
	/// <summary>A one-based index indicating the column on which the sort is based. </summary>
	PUBLIC FieldNo 	AS LONG
	/// <summary>The offset of the field in the workarea buffer.</summary>
	PUBLIC OffSet	AS LONG
	/// <summary>The length of the field in the workarea buffer.</summary>
	PUBLIC Length	AS LONG
	/// <summary>One or more constants that function as sort optimization and control flags.
    /// They are passed to your RDD Sort() routine from the high-level wrapper function for the DBSort() function.</summary>
	PUBLIC Flags	AS  DbSortFlags  

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
    /// <summary>Number of items in the Items array.</summary>
    PUBLIC PROPERTY ItemCount AS LONG AUTO
    /// <summary>Construct a DbTransInfo object.</summary>
    CONSTRUCTOR(itemCount AS LONG)
        SELF:Items := DbTransItem[]{itemCount}
        SELF:Scope := DbScopeInfo{}
        SELF:ItemCount := itemCount
END CLASS

/// <summary>Helper structure to store information about a single piece of data (usually a column) to transfer from one work area to another.</summary> 
STRUCTURE DbTransItem
	/// <summary>A one-based field index in the source work area. </summary>
	PUBLIC Source 		AS LONG
	/// <summary>A one-based field index in the destination work area. </summary>
	PUBLIC Destination 	AS LONG	
END STRUCTURE

/// <summary>Helper class for the RDD system to store field information</summary> 
CLASS RddFieldInfo
	PUBLIC Name 		AS STRING
	PUBLIC FieldType 	AS DBFieldType
	PUBLIC Length 		AS LONG
	PUBLIC Decimals 	AS LONG
	PUBLIC Alias 		AS STRING
	PROTECTED iOffset   AS LONG
    
     /// <summary>Construct a RddFieldInfo object.</summary>
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
		SELF:iOffset := -1
		RETURN
    /// <summary>Construct a RddFieldInfo object.</summary>        
	CONSTRUCTOR(sName AS STRING, nType AS DbFieldType, nLength AS LONG, nDecimals AS LONG)
		Name 		:= sName                                
		FieldType 	:= nType
		Length 		:= nLength
		Decimals 	:= nDecimals
		Alias       := sName
		SELF:iOffset := -1
		RETURN

    /// <summary>Clone a RddFieldInfo object.</summary>        
	METHOD Clone() AS RddFieldInfo
        VAR info := RddFieldInfo{SELF:Name, SELF:FieldType, SELF:Length, SELF:Decimals}
        info:Alias := SELF:Alias
        RETURN info
    /// <summary>Check if two fields match in type, length and decimals.</summary>        
    METHOD SameType(oFld AS RDDFieldInfo) AS LOGIC
        RETURN SELF:FieldType == oFld:FieldType .AND. SELF:Length == oFld:Length .AND. SELF:Decimals == oFld:Decimals
    PROPERTY Offset AS LONG
		GET
			RETURN SELF:iOffset
		END GET
		INTERNAL SET
			SELF:iOffset := VALUE
		END SET
	END PROPERTY
        
END CLASS




END NAMESPACE

/// <summary>Helper class for VoDbTrans and VoDbSort()</summary>
CLASS XSharp._FieldNames
    /// <summary>List of field names.</summary>
    PUBLIC fields AS STRING[]
    /// <summary>Number of fields in the list.</summary>
    PROPERTY fieldCount AS LONG GET fields:Length
    /// <summary>Construct a _FieldNames object.</summary>
    CONSTRUCTOR (aFields AS IList<STRING>)
        SELF:Fields := aFields:ToArray()
        RETURN
END CLASS


/// <summary>Helper class for DbJoin()</summary>
CLASS XSharp._JoinList
    /// <summary>Area number of destination workarea.</summary>
    PUBLIC uiDestSel AS DWORD
    /// <summary>List of field areas and positions.</summary>
    PUBLIC Fields AS _JoinField[]
    /// <summary>Number of fields in the list.</summary>
    PUBLIC PROPERTY Count AS LONG GET Fields:Length
    /// <summary>Construct a _JoinList object.</summary>
    PUBLIC CONSTRUCTOR(nFields AS LONG)
        SELF:Fields := _JoinField[]{nFields}
        RETURN
END CLASS

/// <summary>Helper structure for DbJoin()</summary>
STRUCTURE XSharp._JoinField
    /// <summary>Source workarea number.</summary>
    PUBLIC Area AS DWORD
    /// <summary>Source field position.</summary>
    PUBLIC Pos  AS DWORD
END STRUCTURE

/// <summary>Helper structure to store information for a list of RDD names for DbUseArea()</summary>
STRUCTURE XSharp._RddList
    /// <summary>List of RDD names.</summary>
    EXPORT atomRddName AS STRING[]
    /// <summary>Number of names in the list.</summary>
    PROPERTY uiRDDCount AS DWORD GET (DWORD) atomRDDName:Length
        
    /// <summary>Construct _RddList from class Tree.</summary>
    CONSTRUCTOR(oRDD AS WorkArea)
        VAR names := List<STRING>{}
        VAR type  := oRDD:GetType()
        DO WHILE type != typeof(WorkArea)
            VAR name := type:Name:ToUpper()
            // map names to VO compatible names
            IF name == "DBF"
                names:Add("CAVODBF")
            ELSE
                names:Add(name)
            ENDIF
            type := type:BaseType
        ENDDO
        names:Reverse()
        atomRDDName := names:ToArray()
    /// <summary>Construct _RddList from list of names.</summary>
    CONSTRUCTOR(aNames AS STRING[])
        atomRDDName := aNames
        RETURN
            
END STRUCTURE
