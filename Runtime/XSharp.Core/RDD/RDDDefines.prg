//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums

// RecInfo defines
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_DELETED 	:= DbRecordInfo.DBRI_DELETED 
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_LOCKED 		:= DbRecordInfo.DBRI_LOCKED 		
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_RECSIZE		:= DbRecordInfo.DBRI_RECSIZE		
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_RECNO		:= DbRecordInfo.DBRI_RECNO		
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_UPDATED		:= DbRecordInfo.DBRI_UPDATED		
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_BUFFPTR 	:= DbRecordInfo.DBRI_BUFFPTR 	
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_RAWRECORD   := DbRecordInfo.DBRI_RAWRECORD   
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_ENCRYPTED	:= DbRecordInfo.DBRI_ENCRYPTED	
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_RAWMEMOS	:= DbRecordInfo.DBRI_RAWMEMOS	
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_RAWDATA		:= DbRecordInfo.DBRI_RAWDATA		
/// <include file="CoreComments.xml" path="Comments/DbRecordInfo/*" />
DEFINE DBRI_USER		:= DbRecordInfo.DBRI_USER		


// FieldInfo defines
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_NAME					:= DbFieldInfo.DBS_NAME	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_TYPE					:= DbFieldInfo.DBS_TYPE	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_LEN					:= DbFieldInfo.DBS_LEN	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_DEC					:= DbFieldInfo.DBS_DEC	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_ALIAS				:= DbFieldInfo.DBS_ALIAS
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_ALEN                 := 4  // XBase++ has this define

/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_ISNULL               := DbFieldInfo.DBS_ISNULL  
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_COUNTER              := DbFieldInfo.DBS_COUNTER 
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_STEP                 := DbFieldInfo.DBS_STEP    
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_GET             := DbFieldInfo.DBS_BLOB_GET     
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_TYPE			:= DbFieldInfo.DBS_BLOB_TYPE	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_LEN				:= DbFieldInfo.DBS_BLOB_LEN		
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_OFFSET			:= DbFieldInfo.DBS_BLOB_OFFSET	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_POINTER			:= DbFieldInfo.DBS_BLOB_POINTER	
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_DIRECT_TYPE		:= DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_BLOB_DIRECT_LEN		:= DbFieldInfo.DBS_BLOB_DIRECT_LEN		
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_STRUCT				:= DbFieldInfo.DBS_STRUCT				
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_PROPERTIES			:= DbFieldInfo.DBS_PROPERTIES			
/// <include file="CoreComments.xml" path="Comments/DBFieldInfo/*" />
DEFINE DBS_USER					:= DbFieldInfo.DBS_USER					


// Scope defines
DEFINE TOPSCOPE                := 0 
DEFINE BOTTOMSCOPE             := 1 

//DbInfo Defines
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISDBF 			:=  DbInfo.DBI_ISDBF 		 	// Logical: RDD support DBF file format?
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_CANPUTREC 		:=  DbInfo.DBI_CANPUTREC 		// Logical: RDD support Putting Records? 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GETHEADERSIZE 	:= 	DbInfo.DBI_GETHEADERSIZE	// Numeric: Get header size of the file 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_LASTUPDATE 		:= 	DbInfo.DBI_LASTUPDATE 		// Date:    Last date RDD file updated 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GETDELIMITER 	:= 	DbInfo.DBI_GETDELIMITER		// String:  Get default delimiter
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_SETDELIMITER 	:=  DbInfo.DBI_SETDELIMITER		// String:  Set default delimiter
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GETRECSIZE 		:= 	DbInfo.DBI_GETRECSIZE 		// Numeric: Get record size of the file
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GETLOCKARRAY 	:=  DbInfo.DBI_GETLOCKARRAY		// Array:   Get array of locked records 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_TABLEEXT 		:=  DbInfo.DBI_TABLEEXT 		 // String:  Get table file extension
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_FULLPATH 		:= 	DbInfo.DBI_FULLPATH 		// String: Full path to data file
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISFLOCK 			:= 	DbInfo.DBI_ISFLOCK 			// Logic: Is there a file lock active? 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_READONLY 		:= 	DbInfo.DBI_READONLY 		// Logic: is the file opened readonly
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_CHILDCOUNT 		:= 	DbInfo.DBI_CHILDCOUNT 		// Number: Number of child relations set
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_FILEHANDLE 		:= 	DbInfo.DBI_FILEHANDLE 		// Stream: The data file's file stream
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISANSI 			:= 	DbInfo.DBI_ISANSI 			// Logic: Is the file ansi encoded
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BOF 				:= 	DbInfo.DBI_BOF 				// Logic: Same as Bof()
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_EOF 				:= 	DbInfo.DBI_EOF 				// Logic: Same as Eof()
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_DBFILTER 		:= 	DbInfo.DBI_DBFILTER 		// String: Current Filter setting 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_FOUND 			:= 	DbInfo.DBI_FOUND 			// Logic: Same as Found() 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_FCOUNT 			:= 	DbInfo.DBI_FCOUNT 			// Number: Number of fields per record
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_LOCKCOUNT 		:= 	DbInfo.DBI_LOCKCOUNT 		// Number: Number of record locks  
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_VALIDBUFFER  	:= 	DbInfo.DBI_VALIDBUFFER		// Logic: Is the buffer Valid
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ALIAS 			:= 	DbInfo.DBI_ALIAS 			// String: Alias
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GETSCOPE 		:= 	DbInfo.DBI_GETSCOPE 		// Object: The ScopeInfo
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_LOCKOFFSET 		:= 	DbInfo.DBI_LOCKOFFSET 		// Number: Lock offset
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_SHARED 			:= 	DbInfo.DBI_SHARED 			// Logic: is the file opened shared
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOEXT 			:= 	DbInfo.DBI_MEMOEXT 			// String: Memo file extension
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOHANDLE 		:= 	DbInfo.DBI_MEMOHANDLE 		// Stream: The memo file's file stream
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_HANDLE 		:= 	DbInfo.DBI_BLOB_HANDLE 		// Alias for MemoHandle
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOBLOCKSIZE 	:= 	DbInfo.DBI_MEMOBLOCKSIZE	// Number: The memo block size
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_CODEPAGE 		:= 	DbInfo.DBI_CODEPAGE 		// Number: The Windows Codepage
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_NEWINDEXLOCK 	:= 	DbInfo.DBI_NEWINDEXLOCK 	// Logic:  Use new index lock mechanism
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_DOSCODEPAGE 		:= 	DbInfo.DBI_DOSCODEPAGE 		// Number: The DOS Codepage
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_STRICTREAD  		:= DbInfo.DBI_STRICTREAD  	// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_OPTIMIZE    		:= DbInfo.DBI_OPTIMIZE    	// Flag for whether to use query optimization             
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_AUTOOPEN    		:= DbInfo.DBI_AUTOOPEN    	// Flag for automatically opening structural indexes      
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_AUTOORDER   		:= DbInfo.DBI_AUTOORDER   	// When a structural index is opened, the order to be set 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_AUTOSHARE   		:= DbInfo.DBI_AUTOSHARE   	// When a network is detected, open the index shared, otherwise open exclusively   
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_DB_VERSION 		:= DbInfo.DBI_DB_VERSION 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RDD_VERSION 		:= DbInfo.DBI_RDD_VERSION 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RDD_LIST 		:= DbInfo.DBI_RDD_LIST 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOFIELD 		:= DbInfo.DBI_MEMOFIELD 	 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_VO_MACRO_SYNTAX	:= DbInfo.DBI_VO_MACRO_SYNTAX	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RDD_OBJECT 		:= DbInfo.DBI_RDD_OBJECT 		

/* CA-Cl*pper documented for public use */

/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_DIRECT_LEN     := DbInfo.BLOB_DIRECT_LEN 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_DIRECT_TYPE    := DbInfo.BLOB_DIRECT_TYPE
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_INTEGRITY      := DbInfo.BLOB_FILE_INTEGRITY  
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_OFFSET         := DbInfo.BLOB_OFFSET     
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_BLOB_RECOVER        := DbInfo.BLOB_FILE_RECOVER


/* Harbour extension */

/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_LOCKSCHEME          := DbInfo.DBI_LOCKSCHEME      /* Locking scheme used by RDD */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISREADONLY          := DbInfo.DBI_ISREADONLY      /* Was the file opened readonly? */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ROLLBACK            := DbInfo.DBI_ROLLBACK        /* Rollback changes made to current record */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_PASSWORD            := DbInfo.DBI_PASSWORD        /* Workarea password */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISENCRYPTED         := DbInfo.DBI_ISENCRYPTED     /* The database is encrypted */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOTYPE            := DbInfo.DBI_MEMOTYPE        /* Type of MEMO file: DBT, SMT, FPT */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_SEPARATOR           := DbInfo.DBI_SEPARATOR       /* The record separator (as a string) */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOVERSION         := DbInfo.DBI_MEMOVERSION     /* sub version of memo file */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_TABLETYPE           := DbInfo.DBI_TABLETYPE       /* Type of table file */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_SCOPEDRELATION      := DbInfo.DBI_SCOPEDRELATION  /* Is given relation scoped */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_TRIGGER             := DbInfo.DBI_TRIGGER         /* Get/Set trigger function */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_OPENINFO            := DbInfo.DBI_OPENINFO        /* DBOPENINFO structure pointer */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ENCRYPT             := DbInfo.DBI_ENCRYPT         /* Encrypt table */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_DECRYPT             := DbInfo.DBI_DECRYPT         /* Decrypt table */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_MEMOPACK            := DbInfo.DBI_MEMOPACK        /* Pack memo file */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_DIRTYREAD           := DbInfo.DBI_DIRTYREAD       /* Get/Set index dirty read flag */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_POSITIONED          := DbInfo.DBI_POSITIONED      /* Is cursor positioned to valid record */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_ISTEMPORARY         := DbInfo.DBI_ISTEMPORARY     /* Is the table a temporary one? */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_LOCKTEST            := DbInfo.DBI_LOCKTEST        /* record / file lock test */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_CODEPAGE_HB         := DbInfo.DBI_CODEPAGE_HB     /* Codepage used also defined by VO and Vulcan */ 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_TRANSREC            := DbInfo.DBI_TRANSREC         /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_SETHEADER		   := DbInfo.DBI_SETHEADER		/* DBF header updating modes */ 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_QUERY			   := DbInfo.DBI_QUERY			/* if area represents result of a query, obtain expression of this query */ 

/* Harbour RECORD MAP (RM) support */

/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_SUPPORTED        := DbInfo.DBI_RM_SUPPORTED    /* has WA RDD record map support? */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_CREATE           := DbInfo.DBI_RM_CREATE     /* create new empty work area record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_REMOVE           := DbInfo.DBI_RM_REMOVE     /* remove active work area record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_CLEAR            := DbInfo.DBI_RM_CLEAR      /* remove all records from WA record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_FILL             := DbInfo.DBI_RM_FILL       /* add all records to WA record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_ADD              := DbInfo.DBI_RM_ADD        /* add record to work area record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_DROP             := DbInfo.DBI_RM_DROP       /* remove record from work area record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_TEST             := DbInfo.DBI_RM_TEST       /* test if record is set in WA record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_COUNT            := DbInfo.DBI_RM_COUNT      /* number of records set in record map */
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RM_HANDLE           := DbInfo.DBI_RM_HANDLE     /* get/set record map filter handle */ 



// CDX and Comix Record List Support
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_AND 		:= DbInfo.DBI_RL_AND 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_CLEAR 	:= DbInfo.DBI_RL_CLEAR 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_COUNT 	:= DbInfo.DBI_RL_COUNT 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_DESTROY 	:= DbInfo.DBI_RL_DESTROY 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_EXFILTER 	:= DbInfo.DBI_RL_EXFILTER 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_GETFILTER := DbInfo.DBI_RL_GETFILTER 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_HASMAYBE 	:= DbInfo.DBI_RL_HASMAYBE 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_LEN 		:= DbInfo.DBI_RL_LEN 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_MAYBEEVAL := DbInfo.DBI_RL_MAYBEEVAL 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_NEW 		:= DbInfo.DBI_RL_NEW 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_NEWDUP 	:= DbInfo.DBI_RL_NEWDUP 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_NEWQUERY 	:= DbInfo.DBI_RL_NEWQUERY 	
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_NEXTRECNO := DbInfo.DBI_RL_NEXTRECNO 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_NOT 		:= DbInfo.DBI_RL_NOT 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_OR 		:= DbInfo.DBI_RL_OR 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_PREVRECNO := DbInfo.DBI_RL_PREVRECNO 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_SET 		:= DbInfo.DBI_RL_SET 		
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_SETFILTER := DbInfo.DBI_RL_SETFILTER 
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_RL_TEST 		:= DbInfo.DBI_RL_TEST 		


/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_USER 				:= DbInfo.DBI_USER	// Start of user definable DBI_ values

// Advantage additions
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GET_ACE_TABLE_HANDLE  := DbInfo.DBI_GET_ACE_TABLE_HANDLE
/// <include file="CoreComments.xml" path="Comments/DBInfo/*" />
DEFINE DBI_GET_ACE_STMT_HANDLE   := DbInfo.DBI_GET_ACE_STMT_HANDLE


// OrderInfo Defines
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_CONDITION 	:= DbOrder_Info.DBOI_CONDITION      // String: The order's conditional expression     
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_EXPRESSION 	:= DbOrder_Info.DBOI_EXPRESSION 	// String: The order's key expression             
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_POSITION 	:= DbOrder_Info.DBOI_POSITION  	// Number: The current key position in scope and filter  
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_RECNO 		:= DbOrder_Info.DBOI_RECNO 		  	// Number: The current key position disregarding filters 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_NAME 		:= DbOrder_Info.DBOI_NAME 		   	// String: The name of the order                      
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_NUMBER 		:= DbOrder_Info.DBOI_NUMBER 		 	// Number: The numeric position in the list of orders
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_BAGNAME 	:= DbOrder_Info.DBOI_BAGNAME 	 	// String: The name of the file containing this order
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_INDEXNAME 	:= DbOrder_Info.DBOI_INDEXNAME   // Alias
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_BAGEXT 		:= DbOrder_Info.DBOI_BAGEXT 		   	// String: The extension of the file containing this order
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_INDEXEXT  	:= DbOrder_Info.DBOI_INDEXEXT  	   // Alias
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ORDERCOUNT  := DbOrder_Info.DBOI_ORDERCOUNT     // Number: The count of ORDERS contained in an index file or in total
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_FILEHANDLE 	:= DbOrder_Info.DBOI_FILEHANDLE 		// Stream: The stream of the index
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISCOND 		:= DbOrder_Info.DBOI_ISCOND 			// Logic : Does the order have a FOR condition?
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISDESC 		:= DbOrder_Info.DBOI_ISDESC 			// Logic : Is the order DESCENDing? 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_UNIQUE 		:= DbOrder_Info.DBOI_UNIQUE 			// Logic : Does the order have the UNIQUE attribute?


/* Clipper 5.3-level constants */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_FULLPATH 	:= DbOrder_Info.DBOI_FULLPATH 	   	// String: The full path to the index file (Bag)
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYTYPE 	:= DbOrder_Info.DBOI_KEYTYPE 	 	// The type of the order's key  
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYSIZE 	:= DbOrder_Info.DBOI_KEYSIZE 	 	// Number: The length of the order's key
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYCOUNT 	:= DbOrder_Info.DBOI_KEYCOUNT 	 	// Number: The count of keys in scope and filter
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SETCODEBLOCK:= DbOrder_Info.DBOI_SETCODEBLOCK 	// Block : The codeblock that produces the key 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYDEC 		:= DbOrder_Info.DBOI_KEYDEC 		 	// Number: The # of decimals in a numeric key 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_HPLOCKING 	:= DbOrder_Info.DBOI_HPLOCKING 	 	// Logic : Using High Performance locking for this order?
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_LOCKOFFSET 	:= DbOrder_Info.DBOI_LOCKOFFSET  	// Number: The offset used for logical locking 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYADD 		:= DbOrder_Info.DBOI_KEYADD 		 	// Logic: Custom Index: Was Key added successfully? 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYDELETE 	:= DbOrder_Info.DBOI_KEYDELETE 		// Logic: Custom Index: Was Key Deletion successful? 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYVAL 		:= DbOrder_Info.DBOI_KEYVAL 			// Object: The value of the current key 
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPETOP 	:= DbOrder_Info.DBOI_SCOPETOP 		// Object: Get or Set the scope top    
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPEBOTTOM := DbOrder_Info.DBOI_SCOPEBOTTOM	// Object: Get or Set the scope bottom
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPETOPCLEAR := DbOrder_Info.DBOI_SCOPETOPCLEAR  	// None	 :
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPEBOTTOMCLEAR:= DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR // None :
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_CUSTOM 		:= DbOrder_Info.DBOI_CUSTOM // Logic: Is this a Custom Index?  
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPUNIQUE 	:= DbOrder_Info.DBOI_SKIPUNIQUE // Logic: Was a skip to adjacent unique Key successful?  
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYSINCLUDED:= DbOrder_Info.DBOI_KEYSINCLUDED 	// Number: Number of keys in the index order
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYNORAW 	:= DbOrder_Info.DBOI_KEYNORAW 	 // Number: The key number disregarding filters
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYCOUNTRAW := DbOrder_Info.DBOI_KEYCOUNTRAW   // Number: The key count disregarding filter  
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_OPTLEVEL 	:= DbOrder_Info.DBOI_OPTLEVEL 	 // Number: Optimization level for current query


/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_STRICTREAD := DbOrder_Info.DBOI_STRICTREAD  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_OPTIMIZE   := DbOrder_Info.DBOI_OPTIMIZE    /* Flag for whether to use query optimization             */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_AUTOOPEN   := DbOrder_Info.DBOI_AUTOOPEN    /* Flag for automatically opening structural indexes      */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_AUTOORDER  := DbOrder_Info.DBOI_AUTOORDER   /* When a structural index is opened, the order to be set */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_AUTOSHARE  := DbOrder_Info.DBOI_AUTOSHARE   /* When a network is detected, open the index shared, otherwise open exclusively   */ 


/* Harbour extensions */

/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPEVAL           := DbOrder_Info.DBOI_SKIPEVAL           /* skip while code block doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPEVALBACK       := DbOrder_Info.DBOI_SKIPEVALBACK       /* skip backward while code block doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPREGEX          := DbOrder_Info.DBOI_SKIPREGEX          /* skip while regular expression on index key doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPREGEXBACK      := DbOrder_Info.DBOI_SKIPREGEXBACK      /* skip backward while regular expression on index key doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPWILD           := DbOrder_Info.DBOI_SKIPWILD           /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SKIPWILDBACK       := DbOrder_Info.DBOI_SKIPWILDBACK       /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPEEVAL          := DbOrder_Info.DBOI_SCOPEEVAL          /* skip through index evaluating given C function */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_FINDREC            := DbOrder_Info.DBOI_FINDREC            /* find given record in a Tag beginning from TOP */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_FINDRECCONT        := DbOrder_Info.DBOI_FINDRECCONT        /* find given record in a Tag beginning from current position */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPESET           := DbOrder_Info.DBOI_SCOPESET           /* set both scopes */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SCOPECLEAR         := DbOrder_Info.DBOI_SCOPECLEAR         /* clear both scopes */


/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_BAGCOUNT           := DbOrder_Info.DBOI_BAGCOUNT           /* number of open order bags */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_BAGNUMBER          := DbOrder_Info.DBOI_BAGNUMBER          /* bag position in bag list */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_BAGORDER           := DbOrder_Info.DBOI_BAGORDER           /* number of first order in a bag */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISMULTITAG         := DbOrder_Info.DBOI_ISMULTITAG         /* does RDD support multi tag in index file */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISSORTRECNO        := DbOrder_Info.DBOI_ISSORTRECNO        /* is record number part of key in sorting */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_LARGEFILE          := DbOrder_Info.DBOI_LARGEFILE          /* is large file size (>=4GB) supported */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_TEMPLATE           := DbOrder_Info.DBOI_TEMPLATE           /* order with free user keys */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_MULTIKEY           := DbOrder_Info.DBOI_MULTIKEY           /* custom order with multikeys */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_CHGONLY            := DbOrder_Info.DBOI_CHGONLY            /* update only existing keys */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_PARTIAL            := DbOrder_Info.DBOI_PARTIAL            /* is index partially updated */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_SHARED             := DbOrder_Info.DBOI_SHARED             /* is index open in shared mode */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISREADONLY         := DbOrder_Info.DBOI_ISREADONLY         /* is index open in readonly mode */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_READLOCK           := DbOrder_Info.DBOI_READLOCK           /* get/set index read lock */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_WRITELOCK          := DbOrder_Info.DBOI_WRITELOCK          /* get/set index write lock */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_UPDATECOUNTER      := DbOrder_Info.DBOI_UPDATECOUNTER      /* get/set update index counter */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_EVALSTEP           := DbOrder_Info.DBOI_EVALSTEP           /* eval step (EVERY) used in index command */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_ISREINDEX          := DbOrder_Info.DBOI_ISREINDEX          /* Is reindex in process */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_I_BAGNAME          := DbOrder_Info.DBOI_I_BAGNAME          /* created index name */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_I_TAGNAME          := DbOrder_Info.DBOI_I_TAGNAME          /* created tag name */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_RELKEYPOS          := DbOrder_Info.DBOI_RELKEYPOS          /* get/set relative key position (in range 0 - 1) */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_USECURRENT         := DbOrder_Info.DBOI_USECURRENT         /* get/set "use current index" flag */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_INDEXTYPE          := DbOrder_Info.DBOI_INDEXTYPE          /* current index type */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_RESETPOS           := DbOrder_Info.DBOI_RESETPOS           /* rest logical and raw positions */
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_INDEXPAGESIZE      := DbOrder_Info.DBOI_INDEXPAGESIZE      /* get index page size */

/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_USER 				:= DbOrder_Info.DBOI_USER

// Advantage extensions

/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_AXS_PERCENT_INDEXED  := DbOrder_Info.DBOI_AXS_PERCENT_INDEXED
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_GET_ACE_INDEX_HANDLE := DbOrder_Info.DBOI_GET_ACE_INDEX_HANDLE


// Duplicates
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYGOTO 	:= DbOrder_Info.DBOI_POSITION
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYGOTORAW 	:= DbOrder_Info.DBOI_KEYNORAW
/// <include file="CoreComments.xml" path="Comments/DBOrderInfo/*" />
DEFINE DBOI_KEYNO	 	:= DbOrder_Info.DBOI_POSITION

// Blob defines



/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_INFO_HANDLE 	:= DbInfo.BLOB_INFO_HANDLE 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_FILE_RECOVER 	:= DbInfo.BLOB_FILE_RECOVER 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_FILE_INTEGRITY 	:= DbInfo.BLOB_FILE_INTEGRITY 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_OFFSET 			:= DbInfo.BLOB_OFFSET 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_POINTER 		:= DbInfo.BLOB_POINTER 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_LEN 			:= DbInfo.BLOB_LEN 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_TYPE 			:= DbInfo.BLOB_TYPE 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_EXPORT 			:= DbInfo.BLOB_EXPORT 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_ROOT_UNLOCK 	:= DbInfo.BLOB_ROOT_UNLOCK 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_ROOT_PUT 		:= DbInfo.BLOB_ROOT_PUT 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_ROOT_GET 		:= DbInfo.BLOB_ROOT_GET 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_ROOT_LOCK 		:= DbInfo.BLOB_ROOT_LOCK 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_IMPORT 			:= DbInfo.BLOB_IMPORT 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_DIRECT_PUT 		:= DbInfo.BLOB_DIRECT_PUT 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_DIRECT_GET 		:= DbInfo.BLOB_DIRECT_GET 		
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_GET 			:= DbInfo.BLOB_GET 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_DIRECT_EXPORT 	:= DbInfo.BLOB_DIRECT_EXPORT 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_DIRECT_IMPORT 	:= DbInfo.BLOB_DIRECT_IMPORT 	
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_NMODE 			:= DbInfo.BLOB_NMODE 			
/// <include file="CoreComments.xml" path="Comments/BLOBInfo/*" />
DEFINE BLOB_USER			:= DbInfo.BLOB_USER

/// <include file="CoreComments.xml" path="Comments/BLOBImportExport/*" />
DEFINE BLOB_EXPORT_APPEND 	:= 0 
/// <include file="CoreComments.xml" path="Comments/BLOBImportExport/*" />
DEFINE BLOB_EXPORT_OVERWRITE:= 1 
/// <include file="CoreComments.xml" path="Comments/BLOBImportExport/*" />
DEFINE BLOB_IMPORT_COMPRESS := 1 
/// <include file="CoreComments.xml" path="Comments/BLOBImportExport/*" />
DEFINE BLOB_IMPORT_ENCRYPT	:= 2 


/* return values for DBOI_OPTLEVEL */

/// <include file="CoreComments.xml" path="Comments/OptLevel/*" />
DEFINE DBOI_OPTIMIZED_NONE       := 0 
/// <include file="CoreComments.xml" path="Comments/OptLevel/*" />
DEFINE DBOI_OPTIMIZED_PART       := 1 
/// <include file="CoreComments.xml" path="Comments/OptLevel/*" />
DEFINE DBOI_OPTIMIZED_FULL       := 2 

/* return values for DBOI_INDEXTYPE */

/// <include file="CoreComments.xml" path="Comments/IndexType/*" />
DEFINE DBOI_TYPE_UNDEF          := -1 
/// <include file="CoreComments.xml" path="Comments/IndexType/*" />
DEFINE DBOI_TYPE_NONE           :=  0 
/// <include file="CoreComments.xml" path="Comments/IndexType/*" />
DEFINE DBOI_TYPE_NONCOMPACT     :=  1 
/// <include file="CoreComments.xml" path="Comments/IndexType/*" />
DEFINE DBOI_TYPE_COMPACT        :=  2 
/// <include file="CoreComments.xml" path="Comments/IndexType/*" />
DEFINE DBOI_TYPE_COMPOUND       :=  3 

/* constants for DBOI_SCOPEEVAL array parameter */

DEFINE DBRMI_FUNCTION           := 1 
DEFINE DBRMI_PARAM              := 2 
DEFINE DBRMI_LOVAL              := 3 
DEFINE DBRMI_HIVAL              := 4 
DEFINE DBRMI_RESULT             := 5 
DEFINE DBRMI_SIZE               := 5 

/* Numeric DBF TYPES */
DEFINE DB_DBF_STD              := 1 
DEFINE DB_DBF_VFP              := 2 


/* Numeric MEMO TYPES */
DEFINE DB_MEMO_NONE            := 0 
DEFINE DB_MEMO_DBT             := 1 
DEFINE DB_MEMO_FPT             := 2 
DEFINE DB_MEMO_SMT             := 3 

/* MEMO EXTENDED TYPES */
DEFINE DB_MEMOVER_STD          := 1 
DEFINE DB_MEMOVER_SIX          := 2 
DEFINE DB_MEMOVER_FLEX         := 3 
DEFINE DB_MEMOVER_CLIP         := 4 


/* LOCK SCHEMES */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_DEFAULT      := 0 
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_CLIPPER      := 1   /* default Cl*pper locking scheme */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_COMIX        := 2   /* COMIX and CL53 DBFCDX hyper locking scheme */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_VFP          := 3   /* [V]FP, CL52 DBFCDX, SIx3 SIXCDX, CDXLOCK.OBJ */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_HB32         := 4   /* Harbour hyper locking scheme for 32bit file API */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_HB64         := 5   /* Harbour hyper locking scheme for 64bit file API */
/// <include file="CoreComments.xml" path="Comments/LockScheme/*" />
DEFINE DB_DBFLOCK_CLIPPER2     := 6   /* extended Cl*pper locking scheme NTXLOCK2.OBJ */ 


// File Extensions
DEFINE DBT_MEMOEXT             := ".DBT" 
DEFINE FPT_MEMOEXT             := ".FPT" 
DEFINE SMT_MEMOEXT             := ".SMT" 
DEFINE DBV_MEMOEXT             := ".DBV" 

// Blocks
/// <include file="CoreComments.xml" path="Comments/BlockSize/*" />
DEFINE DBT_DEFBLOCKSIZE        := 512
/// <include file="CoreComments.xml" path="Comments/BlockSize/*" />
DEFINE FPT_DEFBLOCKSIZE        := 64 
/// <include file="CoreComments.xml" path="Comments/BlockSize/*" />
DEFINE SMT_DEFBLOCKSIZE        := 32 

// Locks
DEFINE FPT_LOCKPOS             := 0		
DEFINE FPT_LOCKSIZE            := 1		
DEFINE FPT_ROOTBLOCK_OFFSET    := 536	 /* Clipper 5.3 ROOT data block offset */

DEFINE SIX_ITEM_BUFSIZE        :=    14 
DEFINE FLEX_ITEM_BUFSIZE       :=     8 
DEFINE MAX_SIXFREEBLOCKS       :=    82 
DEFINE MAX_FLEXFREEBLOCKS      :=   126 
DEFINE FLEXGCPAGE_SIZE         :=  1010 


// RDD Inheritance
DEFINE RDT_FULL            :=  1	
DEFINE RDT_TRANSFER        :=  2	
DEFINE RDT_HIDDEN          :=  8	


/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ISDBF             := RDDInfo.RDDI_ISDBF        /* Does this RDD support DBFs? */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_CANPUTREC         := RDDInfo.RDDI_CANPUTREC    /* Can this RDD Put Records? */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_DELIMITER         := RDDInfo.RDDI_DELIMITER    /* The field delimiter (as a string) */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_SEPARATOR         := RDDInfo.RDDI_SEPARATOR    /* The record separator (as a string) */
							  
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_TABLEEXT          := RDDInfo.RDDI_TABLEEXT     /* Default data file's file extension */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOEXT           := RDDInfo.RDDI_MEMOEXT      /* Default memo file's file extension */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ORDBAGEXT         := RDDInfo.RDDI_ORDBAGEXT    /* Default multi tag index's file extension */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ORDEREXT          := RDDInfo.RDDI_ORDEREXT     /* default single tag index's file extension */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ORDSTRUCTEXT      := RDDInfo.RDDI_ORDSTRUCTEXT /* default single tag index's file extension */
							  
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_LOCAL             := RDDInfo.RDDI_LOCAL        /* Local file access? */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_REMOTE            := RDDInfo.RDDI_REMOTE       /* Remote table access? */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_CONNECTION        := RDDInfo.RDDI_CONNECTION   /* Get/Set default connection */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_TABLETYPE         := RDDInfo.RDDI_TABLETYPE    /* Type of table file */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOTYPE          := RDDInfo.RDDI_MEMOTYPE     /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_LARGEFILE         := RDDInfo.RDDI_LARGEFILE    /* Is large file size (>=4GB) supported */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_LOCKSCHEME        := RDDInfo.RDDI_LOCKSCHEME   /* Locking scheme used by RDD */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_RECORDMAP         := RDDInfo.RDDI_RECORDMAP    /* Does RDD support record map functionality? */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ENCRYPTION        := RDDInfo.RDDI_ENCRYPTION   /* Does RDD support encryption */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_TRIGGER           := RDDInfo.RDDI_TRIGGER      /* Get/Set default trigger function */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_AUTOLOCK          := RDDInfo.RDDI_AUTOLOCK     /* automatic locking on update */

/* index parameters */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_STRUCTORD           := RDDInfo.RDDI_STRUCTORD    /* Are structural indexes supported */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_STRICTREAD          := RDDInfo.RDDI_STRICTREAD   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_STRICTSTRUCT        := RDDInfo.RDDI_STRICTSTRUCT /* Flag for strict structural order checking */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_OPTIMIZE            := RDDInfo.RDDI_OPTIMIZE     /* Flag for whether to use query optimization */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_FORCEOPT            := RDDInfo.RDDI_FORCEOPT     /* Flag for forcing linear optimization */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_AUTOOPEN            := RDDInfo.RDDI_AUTOOPEN     /* Flag for automatically opening structural indexes */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_AUTOORDER           := RDDInfo.RDDI_AUTOORDER    /* When a structural index is opened, the order to be set */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_AUTOSHARE           := RDDInfo.RDDI_AUTOSHARE    /* When a network is detected, open the index shared, otherwise open exclusively */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MULTITAG            := RDDInfo.RDDI_MULTITAG     /* Does RDD support multi tag in index file */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_SORTRECNO           := RDDInfo.RDDI_SORTRECNO    /* Is record number part of key in sorting */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MULTIKEY            := RDDInfo.RDDI_MULTIKEY     /* Does custom orders support repeated keys? */

/* memo parameters */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOBLOCKSIZE       := RDDInfo.RDDI_MEMOBLOCKSIZE   /* Memo File's block size */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOVERSION         := RDDInfo.RDDI_MEMOVERSION     /* sub version of memo file */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOGCTYPE          := RDDInfo.RDDI_MEMOGCTYPE      /* type of garbage collector used by GC */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOREADLOCK        := RDDInfo.RDDI_MEMOREADLOCK    /* use read lock in memo file access */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_MEMOREUSE           := RDDInfo.RDDI_MEMOREUSE       /* reuse free space on write */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_BLOB_SUPPORT        := RDDInfo.RDDI_BLOB_SUPPORT    /* can support BLOB files directly */

/* misc */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_PENDINGTRIGGER      := RDDInfo.RDDI_PENDINGTRIGGER    /* set pending trigger for next open operation */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_PENDINGPASSWORD     := RDDInfo.RDDI_PENDINGPASSWORD   /* set pending password for next open operation */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_PASSWORD            := RDDInfo.RDDI_PASSWORD          /* Get/Set default password */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_LOCKRETRY           := RDDInfo.RDDI_LOCKRETRY         /* Get/Set record and file lock timeout value */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_DIRTYREAD           := RDDInfo.RDDI_DIRTYREAD         /* Get/Set index dirty read flag */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_INDEXPAGESIZE       := RDDInfo.RDDI_INDEXPAGESIZE     /* Get/Set default index page size */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_DECIMALS            := RDDInfo.RDDI_DECIMALS          /* Get/Set default number of decimal places for numeric fields if it's undefined */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_SETHEADER           := RDDInfo.RDDI_SETHEADER         /* DBF header updating modes */

/* SQL */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_CONNECT             := RDDInfo.RDDI_CONNECT         /* connect to database */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_DISCONNECT          := RDDInfo.RDDI_DISCONNECT      /* disconnect from database */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_EXECUTE             := RDDInfo.RDDI_EXECUTE         /* execute SQL statement */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ERROR               := RDDInfo.RDDI_ERROR           /* error number */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_ERRORNO             := RDDInfo.RDDI_ERRORNO         /* error description */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_INSERTID            := RDDInfo.RDDI_INSERTID        /* last auto insert ID */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_AFFECTEDROWS        := RDDInfo.RDDI_AFFECTEDROWS    /* number of affected rows after UPDATE */
/// <include file="CoreComments.xml" path="Comments/RDDInfo/*" />
DEFINE RDDI_QUERY               := RDDInfo.RDDI_QUERY           /* last executed query */ 

/*
/* "V" field types * /
DEFINE HB_VF_CHAR            64000

DEFINE HB_VF_DATE            64001

DEFINE HB_VF_INT             64002

DEFINE HB_VF_LOG             64003

DEFINE HB_VF_DNUM            64004

DEFINE HB_VF_ARRAY           64005

DEFINE HB_VF_BLOB            64006

DEFINE HB_VF_BLOBCOMPRESS    64007

DEFINE HB_VF_BLOBENCRYPT     64008



/* SMT types * /

DEFINE SMT_IT_NIL            0

DEFINE SMT_IT_CHAR           1

DEFINE SMT_IT_INT            2

DEFINE SMT_IT_DOUBLE         3

DEFINE SMT_IT_DATE           4

DEFINE SMT_IT_LOGICAL        5

DEFINE SMT_IT_ARRAY          6


DEFINE FPTIT_DUMMY        0xDEADBEAF

DEFINE FPTIT_BINARY       0x0000

DEFINE FPTIT_PICT         0x0000      

/* Picture * /

DEFINE FPTIT_TEXT         0x0001      /* Text    * /

DEFINE FPTIT_OBJ          0x0002      /* Object  * /

DEFINE FPTIT_SIX_NIL      0x0000      /* NIL VALUE (USED ONLY IN ARRAYS) * /
DEFINE FPTIT_SIX_LNUM     0x0002      /* LONG LE * /
DEFINE FPTIT_SIX_DNUM     0x0008      /* DOUBLE LE * /

DEFINE FPTIT_SIX_LDATE    0x0020      /* DATE (LONG LE) * /
DEFINE FPTIT_SIX_LOG      0x0080      /* LOGIC * /

DEFINE FPTIT_SIX_CHAR     0x0400      /* CHAR * /

DEFINE FPTIT_SIX_ARRAY    0x8000      /* ARRAY * / 
*/



// RDD Errors
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB                 := Subcodes.EDB
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SEEK            := SubCodes.EDB_SEEK          
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SKIP            := SubCodes.EDB_SKIP          
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_GOTO            := SubCodes.EDB_GOTO          
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETRELATION     := SubCodes.EDB_SETRELATION   
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_USE             := SubCodes.EDB_USE           
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CREATEINDEX     := SubCodes.EDB_CREATEINDEX   
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETORDER        := SubCodes.EDB_SETORDER      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETINDEX        := SubCodes.EDB_SETINDEX      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_FIELDNAME       := SubCodes.EDB_FIELDNAME     
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_BADALIAS        := SubCodes.EDB_BADALIAS      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DUPALIAS        := SubCodes.EDB_DUPALIAS      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETFILTER       := SubCodes.EDB_SETFILTER     
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CYCLICREL       := SubCodes.EDB_CYCLICREL     
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CREATETABLE     := SubCodes.EDB_CREATETABLE   
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_RDDNOTFOUND     := SubCodes.EDB_RDDNOTFOUND   
// RESERVED EDB + 16
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_FIELDINDEX      := SubCodes.EDB_FIELDINDEX   
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SELECT          := SubCodes.EDB_SELECT       
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SYMSELECT       := SubCodes.EDB_SYMSELECT    
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_TOTAL           := SubCodes.EDB_TOTAL        
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_RECNO           := SubCodes.EDB_RECNO        
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_EXPRESSION      := SubCodes.EDB_EXPRESSION   
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_EXPR_WIDTH      := SubCodes.EDB_EXPR_WIDTH   
// RESERVED EDB + 24 - 29
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DRIVERLOAD      := SubCodes.EDB_DRIVERLOAD      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_PARAM           := SubCodes.EDB_PARAM           
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOAREAS         := SubCodes.EDB_NOAREAS         
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOMEM           := SubCodes.EDB_NOMEM           
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOFIELDS        := SubCodes.EDB_NOFIELDS        
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_BAD_ERROR_INFO  := SubCodes.EDB_BAD_ERROR_INFO  
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_WRONGFIELDNAME  := SubCodes.EDB_WRONGFIELDNAME  
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ORDDESTROY      := SubCodes.EDB_ORDDESTROY      
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOINITFUNCTION  := SubCodes.EDB_NOINITFUNCTION  
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ERRORINIT       := SubCodes.EDB_ERRORINIT       
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DBSTRUCT        := SubCodes.EDB_DBSTRUCT        

/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOTABLE         := SubCodes.EDB_NOTABLE       
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOORDER         := SubCodes.EDB_NOORDER       

/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ASSERTION       := SubCodes.EDB_ASSERTION     

// Xbase++ defines for DbScope()
/// <summary>Scope value for DbScope() and DbSetScope().</summary>
/// <seealso cref='M:XSharp.RT.Functions.DbScope(XSharp.__Usual)' >DbScope()</seealso>
/// <seealso cref='M:XSharp.RT.Functions.DbSetScope(System.Int32,XSharp.__Usual)' >DbSetScope()</seealso>
DEFINE SCOPE_TOP    := 0x01

/// <summary>Scope value for DbScope() and DbSetScope().</summary>
/// <seealso cref='M:XSharp.RT.Functions.DbScope(XSharp.__Usual)' >DbScope()</seealso>
/// <seealso cref='M:XSharp.RT.Functions.DbSetScope(System.Int32,XSharp.__Usual)' >DbSetScope()</seealso>
DEFINE SCOPE_BOTTOM := 0x02
/// <summary>Scope value for DbScope() and DbSetScope().</summary>
/// <seealso cref='M:XSharp.RT.Functions.DbScope(XSharp.__Usual)' >DbScope()</seealso>
/// <seealso cref='M:XSharp.RT.Functions.DbSetScope(System.Int32,XSharp.__Usual)' >DbSetScope()</seealso>
DEFINE SCOPE_BOTH   := 0xFF
