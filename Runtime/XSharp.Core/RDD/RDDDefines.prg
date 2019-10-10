//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums

DEFINE _NULLFLAGS := "_NULLFLAGS" 

// RecInfo defines
/// <exclude />
DEFINE DBRI_DELETED 	:= DbRecordInfo.DBRI_DELETED 
/// <exclude />
DEFINE DBRI_LOCKED 		:= DbRecordInfo.DBRI_LOCKED 		
/// <exclude />
DEFINE DBRI_RECSIZE		:= DbRecordInfo.DBRI_RECSIZE		
/// <exclude />
DEFINE DBRI_RECNO		:= DbRecordInfo.DBRI_RECNO		
/// <exclude />
DEFINE DBRI_UPDATED		:= DbRecordInfo.DBRI_UPDATED		
/// <exclude />
DEFINE DBRI_BUFFPTR 	:= DbRecordInfo.DBRI_BUFFPTR 	
/// <exclude />
DEFINE DBRI_RAWRECORD   := DbRecordInfo.DBRI_RAWRECORD   
/// <exclude />
DEFINE DBRI_ENCRYPTED	:= DbRecordInfo.DBRI_ENCRYPTED	
/// <exclude />
DEFINE DBRI_RAWMEMOS	:= DbRecordInfo.DBRI_RAWMEMOS	
/// <exclude />
DEFINE DBRI_RAWDATA		:= DbRecordInfo.DBRI_RAWDATA		
/// <exclude />
DEFINE DBRI_USER		:= DbRecordInfo.DBRI_USER		


// FieldInfo defines
/// <exclude />
DEFINE DBS_NAME					:= DbFieldInfo.DBS_NAME	
/// <exclude />
DEFINE DBS_TYPE					:= DbFieldInfo.DBS_TYPE	
/// <exclude />
DEFINE DBS_LEN					:= DbFieldInfo.DBS_LEN	
/// <exclude />
DEFINE DBS_DEC					:= DbFieldInfo.DBS_DEC	
/// <exclude />
DEFINE DBS_ALIAS				:= DbFieldInfo.DBS_ALIAS
DEFINE DBS_ALEN                 := 4  // XBase++ has this define

/// <exclude />
DEFINE DBS_ISNULL               := DbFieldInfo.DBS_ISNULL  
/// <exclude />
DEFINE DBS_COUNTER              := DbFieldInfo.DBS_COUNTER 
/// <exclude />
DEFINE DBS_STEP                 := DbFieldInfo.DBS_STEP    
/// <exclude />
DEFINE DBS_BLOB_GET             := DbFieldInfo.DBS_BLOB_GET     
/// <exclude />
DEFINE DBS_BLOB_TYPE			:= DbFieldInfo.DBS_BLOB_TYPE	
/// <exclude />
DEFINE DBS_BLOB_LEN				:= DbFieldInfo.DBS_BLOB_LEN		
/// <exclude />
DEFINE DBS_BLOB_OFFSET			:= DbFieldInfo.DBS_BLOB_OFFSET	
/// <exclude />
DEFINE DBS_BLOB_POINTER			:= DbFieldInfo.DBS_BLOB_POINTER	
/// <exclude />
DEFINE DBS_BLOB_DIRECT_TYPE		:= DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
/// <exclude />
DEFINE DBS_BLOB_DIRECT_LEN		:= DbFieldInfo.DBS_BLOB_DIRECT_LEN		
/// <exclude />
/// <inheritdoc cref="F:XSharp.RDD.Enums.DbFieldInfo.DBS_STRUCT"/>
DEFINE DBS_STRUCT				:= DbFieldInfo.DBS_STRUCT				
/// <exclude />
DEFINE DBS_PROPERTIES			:= DbFieldInfo.DBS_PROPERTIES			
/// <exclude />
DEFINE DBS_USER					:= DbFieldInfo.DBS_USER					


// Scope defines
DEFINE TOPSCOPE                := 0 
DEFINE BOTTOMSCOPE             := 1 

//DbInfo Defines
/// <exclude />
DEFINE DBI_ISDBF 			:=  DbInfo.DBI_ISDBF 		 	// Logical: RDD support DBF file format?
/// <exclude />
DEFINE DBI_CANPUTREC 		:=  DbInfo.DBI_CANPUTREC 		// Logical: RDD support Putting Records? 
/// <exclude />
DEFINE DBI_GETHEADERSIZE 	:= 	DbInfo.DBI_GETHEADERSIZE	// Numeric: Get header size of the file 
/// <exclude />
DEFINE DBI_LASTUPDATE 		:= 	DbInfo.DBI_LASTUPDATE 		// Date:    Last date RDD file updated 
/// <exclude />
DEFINE DBI_GETDELIMITER 	:= 	DbInfo.DBI_GETDELIMITER		// String:  Get default delimiter
/// <exclude />
DEFINE DBI_SETDELIMITER 	:=  DbInfo.DBI_SETDELIMITER		// String:  Set default delimiter
/// <exclude />
DEFINE DBI_GETRECSIZE 		:= 	DbInfo.DBI_GETRECSIZE 		// Numeric: Get record size of the file
/// <exclude />
DEFINE DBI_GETLOCKARRAY 	:=  DbInfo.DBI_GETLOCKARRAY		// Array:   Get array of locked records 
/// <exclude />
DEFINE DBI_TABLEEXT 		:=  DbInfo.DBI_TABLEEXT 		 // String:  Get table file extension
/// <exclude />
DEFINE DBI_FULLPATH 		:= 	DbInfo.DBI_FULLPATH 		// String: Full path to data file
/// <exclude />
DEFINE DBI_ISFLOCK 			:= 	DbInfo.DBI_ISFLOCK 			// Logic: Is there a file lock active? 
/// <exclude />
DEFINE DBI_READONLY 		:= 	DbInfo.DBI_READONLY 		// Logic: is the file opened readonly
/// <exclude />
DEFINE DBI_CHILDCOUNT 		:= 	DbInfo.DBI_CHILDCOUNT 		// Number: Number of child relations set
/// <exclude />
DEFINE DBI_FILEHANDLE 		:= 	DbInfo.DBI_FILEHANDLE 		// Stream: The data file's file stream
/// <exclude />
DEFINE DBI_ISANSI 			:= 	DbInfo.DBI_ISANSI 			// Logic: Is the file ansi encoded
/// <exclude />
DEFINE DBI_BOF 				:= 	DbInfo.DBI_BOF 				// Logic: Same as Bof()
/// <exclude />
DEFINE DBI_EOF 				:= 	DbInfo.DBI_EOF 				// Logic: Same as Eof()
/// <exclude />
DEFINE DBI_DBFILTER 		:= 	DbInfo.DBI_DBFILTER 		// String: Current Filter setting 
/// <exclude />
DEFINE DBI_FOUND 			:= 	DbInfo.DBI_FOUND 			// Logic: Same as Found() 
/// <exclude />
DEFINE DBI_FCOUNT 			:= 	DbInfo.DBI_FCOUNT 			// Number: Number of fields per record
/// <exclude />
DEFINE DBI_LOCKCOUNT 		:= 	DbInfo.DBI_LOCKCOUNT 		// Number: Number of record locks  
/// <exclude />
DEFINE DBI_VALIDBUFFER  	:= 	DbInfo.DBI_VALIDBUFFER		// Logic: Is the buffer Valid
/// <exclude />
DEFINE DBI_ALIAS 			:= 	DbInfo.DBI_ALIAS 			// String: Alias
/// <exclude />
DEFINE DBI_GETSCOPE 		:= 	DbInfo.DBI_GETSCOPE 		// Object: The ScopeInfo
/// <exclude />
DEFINE DBI_LOCKOFFSET 		:= 	DbInfo.DBI_LOCKOFFSET 		// Number: Lock offset
/// <exclude />
DEFINE DBI_SHARED 			:= 	DbInfo.DBI_SHARED 			// Logic: is the file opened shared
/// <exclude />
DEFINE DBI_MEMOEXT 			:= 	DbInfo.DBI_MEMOEXT 			// String: Memo file extension
/// <exclude />
DEFINE DBI_MEMOHANDLE 		:= 	DbInfo.DBI_MEMOHANDLE 		// Stream: The memo file's file stream
/// <exclude />
DEFINE DBI_BLOB_HANDLE 		:= 	DbInfo.DBI_BLOB_HANDLE 		// Alias for MemoHandle
/// <exclude />
DEFINE DBI_MEMOBLOCKSIZE 	:= 	DbInfo.DBI_MEMOBLOCKSIZE	// Number: The memo block size
/// <exclude />
DEFINE DBI_CODEPAGE 		:= 	DbInfo.DBI_CODEPAGE 		// Number: The Windows Codepage
/// <exclude />
DEFINE DBI_NEWINDEXLOCK 	:= 	DbInfo.DBI_NEWINDEXLOCK 	// Logic:  Use new index lock mechanism
/// <exclude />
DEFINE DBI_DOSCODEPAGE 		:= 	DbInfo.DBI_DOSCODEPAGE 		// Number: The DOS Codepage
/// <exclude />
DEFINE DBI_STRICTREAD  		:= DbInfo.DBI_STRICTREAD  	// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
/// <exclude />
DEFINE DBI_OPTIMIZE    		:= DbInfo.DBI_OPTIMIZE    	// Flag for whether to use query optimization             
/// <exclude />
DEFINE DBI_AUTOOPEN    		:= DbInfo.DBI_AUTOOPEN    	// Flag for automatically opening structural indexes      
/// <exclude />
DEFINE DBI_AUTOORDER   		:= DbInfo.DBI_AUTOORDER   	// When a structural index is opened, the order to be set 0 or 1
/// <exclude />
DEFINE DBI_AUTOSHARE   		:= DbInfo.DBI_AUTOSHARE   	// When a network is detected, open the index shared, otherwise open exclusively   0 = shared, 1 = exclusive
/// <exclude />
DEFINE DBI_DB_VERSION 		:= DbInfo.DBI_DB_VERSION 		
/// <exclude />
DEFINE DBI_RDD_VERSION 		:= DbInfo.DBI_RDD_VERSION 		
/// <exclude />
DEFINE DBI_RDD_LIST 		:= DbInfo.DBI_RDD_LIST 		
/// <exclude />
DEFINE DBI_MEMOFIELD 		:= DbInfo.DBI_MEMOFIELD 	 	
/// <exclude />
DEFINE DBI_VO_MACRO_SYNTAX	:= DbInfo.DBI_VO_MACRO_SYNTAX	
/// <exclude />
DEFINE DBI_RDD_OBJECT 		:= DbInfo.DBI_RDD_OBJECT 		

/* CA-Cl*pper documented for public use */

/// <exclude />
DEFINE DBI_BLOB_DIRECT_LEN     := DbInfo.BLOB_DIRECT_LEN 
/// <exclude />
DEFINE DBI_BLOB_DIRECT_TYPE    := DbInfo.BLOB_DIRECT_TYPE
/// <exclude />
DEFINE DBI_BLOB_INTEGRITY      := DbInfo.BLOB_FILE_INTEGRITY  
/// <exclude />
DEFINE DBI_BLOB_OFFSET         := DbInfo.BLOB_OFFSET     
/// <exclude />
DEFINE DBI_BLOB_RECOVER        := DbInfo.BLOB_FILE_RECOVER


/* Harbour extension */

/// <exclude />
DEFINE DBI_LOCKSCHEME          := DbInfo.DBI_LOCKSCHEME      /* Locking scheme used by RDD */
/// <exclude />
DEFINE DBI_ISREADONLY          := DbInfo.DBI_ISREADONLY      /* Was the file opened readonly? */
/// <exclude />
DEFINE DBI_ROLLBACK            := DbInfo.DBI_ROLLBACK        /* Rollback changes made to current record */
/// <exclude />
DEFINE DBI_PASSWORD            := DbInfo.DBI_PASSWORD        /* Workarea password */
/// <exclude />
DEFINE DBI_ISENCRYPTED         := DbInfo.DBI_ISENCRYPTED     /* The database is encrypted */
/// <exclude />
DEFINE DBI_MEMOTYPE            := DbInfo.DBI_MEMOTYPE        /* Type of MEMO file: DBT, SMT, FPT */
/// <exclude />
DEFINE DBI_SEPARATOR           := DbInfo.DBI_SEPARATOR       /* The record separator (as a string) */
/// <exclude />
DEFINE DBI_MEMOVERSION         := DbInfo.DBI_MEMOVERSION     /* sub version of memo file */
/// <exclude />
DEFINE DBI_TABLETYPE           := DbInfo.DBI_TABLETYPE       /* Type of table file */
/// <exclude />
DEFINE DBI_SCOPEDRELATION      := DbInfo.DBI_SCOPEDRELATION  /* Is given relation scoped */
/// <exclude />
DEFINE DBI_TRIGGER             := DbInfo.DBI_TRIGGER         /* Get/Set trigger function */
/// <exclude />
DEFINE DBI_OPENINFO            := DbInfo.DBI_OPENINFO        /* DBOPENINFO structure pointer */
/// <exclude />
DEFINE DBI_ENCRYPT             := DbInfo.DBI_ENCRYPT         /* Encrypt table */
/// <exclude />
DEFINE DBI_DECRYPT             := DbInfo.DBI_DECRYPT         /* Decrypt table */
/// <exclude />
DEFINE DBI_MEMOPACK            := DbInfo.DBI_MEMOPACK        /* Pack memo file */
/// <exclude />
DEFINE DBI_DIRTYREAD           := DbInfo.DBI_DIRTYREAD       /* Get/Set index dirty read flag */
/// <exclude />
DEFINE DBI_POSITIONED          := DbInfo.DBI_POSITIONED      /* Is cursor positioned to valid record */
/// <exclude />
DEFINE DBI_ISTEMPORARY         := DbInfo.DBI_ISTEMPORARY     /* Is the table a temporary one? */
/// <exclude />
DEFINE DBI_LOCKTEST            := DbInfo.DBI_LOCKTEST        /* record / file lock test */
/// <exclude />
DEFINE DBI_CODEPAGE_HB         := DbInfo.DBI_CODEPAGE_HB     /* Codepage used also defined by VO and Vulcan */ 
/// <exclude />
DEFINE DBI_TRANSREC            := DbInfo.DBI_TRANSREC         /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
/// <exclude />
DEFINE DBI_SETHEADER		   := DbInfo.DBI_SETHEADER		/* DBF header updating modes */ 
/// <exclude />
DEFINE DBI_QUERY			   := DbInfo.DBI_QUERY			/* if area represents result of a query, obtain expression of this query */ 

/* Harbour RECORD MAP (RM) support */

/// <exclude />
DEFINE DBI_RM_SUPPORTED        := DbInfo.DBI_RM_SUPPORTED    /* has WA RDD record map support? */
/// <exclude />
DEFINE DBI_RM_CREATE           := DbInfo.DBI_RM_CREATE     /* create new empty work area record map */
/// <exclude />
DEFINE DBI_RM_REMOVE           := DbInfo.DBI_RM_REMOVE     /* remove active work area record map */
/// <exclude />
DEFINE DBI_RM_CLEAR            := DbInfo.DBI_RM_CLEAR      /* remove all records from WA record map */
/// <exclude />
DEFINE DBI_RM_FILL             := DbInfo.DBI_RM_FILL       /* add all records to WA record map */
/// <exclude />
DEFINE DBI_RM_ADD              := DbInfo.DBI_RM_ADD        /* add record to work area record map */
/// <exclude />
DEFINE DBI_RM_DROP             := DbInfo.DBI_RM_DROP       /* remove record from work area record map */
/// <exclude />
DEFINE DBI_RM_TEST             := DbInfo.DBI_RM_TEST       /* test if record is set in WA record map */
/// <exclude />
DEFINE DBI_RM_COUNT            := DbInfo.DBI_RM_COUNT      /* number of records set in record map */
/// <exclude />
DEFINE DBI_RM_HANDLE           := DbInfo.DBI_RM_HANDLE     /* get/set record map filter handle */ 



// CDX and Comix Record List Support
/// <exclude />
DEFINE DBI_RL_AND 		:= DbInfo.DBI_RL_AND 		
/// <exclude />
DEFINE DBI_RL_CLEAR 	:= DbInfo.DBI_RL_CLEAR 	
/// <exclude />
DEFINE DBI_RL_COUNT 	:= DbInfo.DBI_RL_COUNT 	
/// <exclude />
DEFINE DBI_RL_DESTROY 	:= DbInfo.DBI_RL_DESTROY 	
/// <exclude />
DEFINE DBI_RL_EXFILTER 	:= DbInfo.DBI_RL_EXFILTER 	
/// <exclude />
DEFINE DBI_RL_GETFILTER := DbInfo.DBI_RL_GETFILTER 
/// <exclude />
DEFINE DBI_RL_HASMAYBE 	:= DbInfo.DBI_RL_HASMAYBE 	
/// <exclude />
DEFINE DBI_RL_LEN 		:= DbInfo.DBI_RL_LEN 		
/// <exclude />
DEFINE DBI_RL_MAYBEEVAL := DbInfo.DBI_RL_MAYBEEVAL 
/// <exclude />
DEFINE DBI_RL_NEW 		:= DbInfo.DBI_RL_NEW 		
/// <exclude />
DEFINE DBI_RL_NEWDUP 	:= DbInfo.DBI_RL_NEWDUP 	
/// <exclude />
DEFINE DBI_RL_NEWQUERY 	:= DbInfo.DBI_RL_NEWQUERY 	
/// <exclude />
DEFINE DBI_RL_NEXTRECNO := DbInfo.DBI_RL_NEXTRECNO 
/// <exclude />
DEFINE DBI_RL_NOT 		:= DbInfo.DBI_RL_NOT 		
/// <exclude />
DEFINE DBI_RL_OR 		:= DbInfo.DBI_RL_OR 		
/// <exclude />
DEFINE DBI_RL_PREVRECNO := DbInfo.DBI_RL_PREVRECNO 
/// <exclude />
DEFINE DBI_RL_SET 		:= DbInfo.DBI_RL_SET 		
/// <exclude />
DEFINE DBI_RL_SETFILTER := DbInfo.DBI_RL_SETFILTER 
/// <exclude />
DEFINE DBI_RL_TEST 		:= DbInfo.DBI_RL_TEST 		


/// <exclude />
DEFINE DBI_USER 				:= DbInfo.DBI_USER	// Start of user definable DBI_ values

// Advantage additions
/// <exclude />
DEFINE DBI_GET_ACE_TABLE_HANDLE  := DbInfo.DBI_GET_ACE_TABLE_HANDLE
/// <exclude />
DEFINE DBI_GET_ACE_STMT_HANDLE   := DbInfo.DBI_GET_ACE_STMT_HANDLE


// OrderInfo Defines
/// <exclude />
DEFINE DBOI_CONDITION 	:= DbOrder_Info.DBOI_CONDITION      // String: The order's conditional expression     
/// <exclude />
DEFINE DBOI_EXPRESSION 	:= DbOrder_Info.DBOI_EXPRESSION 	// String: The order's key expression             
/// <exclude />
DEFINE DBOI_POSITION 	:= DbOrder_Info.DBOI_POSITION  	// Number: The current key position in scope and filter  
/// <exclude />
DEFINE DBOI_RECNO 		:= DbOrder_Info.DBOI_RECNO 		  	// Number: The current key position disregarding filters 
/// <exclude />
DEFINE DBOI_NAME 		:= DbOrder_Info.DBOI_NAME 		   	// String: The name of the order                      
/// <exclude />
DEFINE DBOI_NUMBER 		:= DbOrder_Info.DBOI_NUMBER 		 	// Number: The numeric position in the list of orders
/// <exclude />
DEFINE DBOI_BAGNAME 	:= DbOrder_Info.DBOI_BAGNAME 	 	// String: The name of the file containing this order
/// <exclude />
DEFINE DBOI_INDEXNAME 	:= DbOrder_Info.DBOI_INDEXNAME   // Alias
/// <exclude />
DEFINE DBOI_BAGEXT 		:= DbOrder_Info.DBOI_BAGEXT 		   	// String: The extension of the file containing this order
/// <exclude />
DEFINE DBOI_INDEXEXT  	:= DbOrder_Info.DBOI_INDEXEXT  	   // Alias
/// <exclude />
DEFINE DBOI_ORDERCOUNT  := DbOrder_Info.DBOI_ORDERCOUNT     // Number: The count of ORDERS contained in an index file or in total
/// <exclude />
DEFINE DBOI_FILEHANDLE 	:= DbOrder_Info.DBOI_FILEHANDLE 		// Stream: The stream of the index
/// <exclude />
DEFINE DBOI_ISCOND 		:= DbOrder_Info.DBOI_ISCOND 			// Logic : Does the order have a FOR condition?
/// <exclude />
DEFINE DBOI_ISDESC 		:= DbOrder_Info.DBOI_ISDESC 			// Logic : Is the order DESCENDing? 
/// <exclude />
DEFINE DBOI_UNIQUE 		:= DbOrder_Info.DBOI_UNIQUE 			// Logic : Does the order have the UNIQUE attribute?


/* Clipper 5.3-level constants */
/// <exclude />
DEFINE DBOI_FULLPATH 	:= DbOrder_Info.DBOI_FULLPATH 	   	// String: The full path to the index file (Bag)
/// <exclude />
DEFINE DBOI_KEYTYPE 	:= DbOrder_Info.DBOI_KEYTYPE 	 	// The type of the order's key  
/// <exclude />
DEFINE DBOI_KEYSIZE 	:= DbOrder_Info.DBOI_KEYSIZE 	 	// Number: The length of the order's key
/// <exclude />
DEFINE DBOI_KEYCOUNT 	:= DbOrder_Info.DBOI_KEYCOUNT 	 	// Number: The count of keys in scope and filter
/// <exclude />
DEFINE DBOI_SETCODEBLOCK:= DbOrder_Info.DBOI_SETCODEBLOCK 	// Block : The codeblock that produces the key 
/// <exclude />
DEFINE DBOI_KEYDEC 		:= DbOrder_Info.DBOI_KEYDEC 		 	// Number: The # of decimals in a numeric key 
/// <exclude />
DEFINE DBOI_HPLOCKING 	:= DbOrder_Info.DBOI_HPLOCKING 	 	// Logic : Using High Performance locking for this order?
/// <exclude />
DEFINE DBOI_LOCKOFFSET 	:= DbOrder_Info.DBOI_LOCKOFFSET  	// Number: The offset used for logical locking 
/// <exclude />
DEFINE DBOI_KEYADD 		:= DbOrder_Info.DBOI_KEYADD 		 	// Logic: Custom Index: Was Key added successfully? 
/// <exclude />
DEFINE DBOI_KEYDELETE 	:= DbOrder_Info.DBOI_KEYDELETE 		// Logic: Custom Index: Was Key Deletion successful? 
/// <exclude />
DEFINE DBOI_KEYVAL 		:= DbOrder_Info.DBOI_KEYVAL 			// Object: The value of the current key 
/// <exclude />
DEFINE DBOI_SCOPETOP 	:= DbOrder_Info.DBOI_SCOPETOP 		// Object: Get or Set the scope top    
/// <exclude />
DEFINE DBOI_SCOPEBOTTOM := DbOrder_Info.DBOI_SCOPEBOTTOM	// Object: Get or Set the scope bottom
/// <exclude />
DEFINE DBOI_SCOPETOPCLEAR := DbOrder_Info.DBOI_SCOPETOPCLEAR  	// None	 :
/// <exclude />
DEFINE DBOI_SCOPEBOTTOMCLEAR:= DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR // None :
/// <exclude />
DEFINE DBOI_CUSTOM 		:= DbOrder_Info.DBOI_CUSTOM // Logic: Is this a Custom Index?  
/// <exclude />
DEFINE DBOI_SKIPUNIQUE 	:= DbOrder_Info.DBOI_SKIPUNIQUE // Logic: Was a skip to adjacent unique Key successful?  
/// <exclude />
DEFINE DBOI_KEYSINCLUDED:= DbOrder_Info.DBOI_KEYSINCLUDED 	// Number: Number of keys in the index order
/// <exclude />
DEFINE DBOI_KEYNORAW 	:= DbOrder_Info.DBOI_KEYNORAW 	 // Number: The key number disregarding filters
/// <exclude />
DEFINE DBOI_KEYCOUNTRAW := DbOrder_Info.DBOI_KEYCOUNTRAW   // Number: The key count disregarding filter  
/// <exclude />
DEFINE DBOI_OPTLEVEL 	:= DbOrder_Info.DBOI_OPTLEVEL 	 // Number: Optimization level for current query


/// <exclude />
DEFINE DBOI_STRICTREAD := DbOrder_Info.DBOI_STRICTREAD  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
/// <exclude />
DEFINE DBOI_OPTIMIZE   := DbOrder_Info.DBOI_OPTIMIZE    /* Flag for whether to use query optimization             */
/// <exclude />
DEFINE DBOI_AUTOOPEN   := DbOrder_Info.DBOI_AUTOOPEN    /* Flag for automatically opening structural indexes      */
/// <exclude />
DEFINE DBOI_AUTOORDER  := DbOrder_Info.DBOI_AUTOORDER   /* When a structural index is opened, the order to be set */
/// <exclude />
DEFINE DBOI_AUTOSHARE  := DbOrder_Info.DBOI_AUTOSHARE   /* When a network is detected, open the index shared, otherwise open exclusively   */ 


/* Harbour extensions */

/// <exclude />
DEFINE DBOI_SKIPEVAL           := DbOrder_Info.DBOI_SKIPEVAL           /* skip while code block doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SKIPEVALBACK       := DbOrder_Info.DBOI_SKIPEVALBACK       /* skip backward while code block doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SKIPREGEX          := DbOrder_Info.DBOI_SKIPREGEX          /* skip while regular expression on index key doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SKIPREGEXBACK      := DbOrder_Info.DBOI_SKIPREGEXBACK      /* skip backward while regular expression on index key doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SKIPWILD           := DbOrder_Info.DBOI_SKIPWILD           /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SKIPWILDBACK       := DbOrder_Info.DBOI_SKIPWILDBACK       /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
/// <exclude />
DEFINE DBOI_SCOPEEVAL          := DbOrder_Info.DBOI_SCOPEEVAL          /* skip through index evaluating given C function */
/// <exclude />
DEFINE DBOI_FINDREC            := DbOrder_Info.DBOI_FINDREC            /* find given record in a Tag beginning from TOP */
/// <exclude />
DEFINE DBOI_FINDRECCONT        := DbOrder_Info.DBOI_FINDRECCONT        /* find given record in a Tag beginning from current position */
/// <exclude />
DEFINE DBOI_SCOPESET           := DbOrder_Info.DBOI_SCOPESET           /* set both scopes */
/// <exclude />
DEFINE DBOI_SCOPECLEAR         := DbOrder_Info.DBOI_SCOPECLEAR         /* clear both scopes */


/// <exclude />
DEFINE DBOI_BAGCOUNT           := DbOrder_Info.DBOI_BAGCOUNT           /* number of open order bags */
/// <exclude />
DEFINE DBOI_BAGNUMBER          := DbOrder_Info.DBOI_BAGNUMBER          /* bag position in bag list */
/// <exclude />
DEFINE DBOI_BAGORDER           := DbOrder_Info.DBOI_BAGORDER           /* number of first order in a bag */
/// <exclude />
DEFINE DBOI_ISMULTITAG         := DbOrder_Info.DBOI_ISMULTITAG         /* does RDD support multi tag in index file */
/// <exclude />
DEFINE DBOI_ISSORTRECNO        := DbOrder_Info.DBOI_ISSORTRECNO        /* is record number part of key in sorting */
/// <exclude />
DEFINE DBOI_LARGEFILE          := DbOrder_Info.DBOI_LARGEFILE          /* is large file size (>=4GB) supported */
/// <exclude />
DEFINE DBOI_TEMPLATE           := DbOrder_Info.DBOI_TEMPLATE           /* order with free user keys */
/// <exclude />
DEFINE DBOI_MULTIKEY           := DbOrder_Info.DBOI_MULTIKEY           /* custom order with multikeys */
/// <exclude />
DEFINE DBOI_CHGONLY            := DbOrder_Info.DBOI_CHGONLY            /* update only existing keys */
/// <exclude />
DEFINE DBOI_PARTIAL            := DbOrder_Info.DBOI_PARTIAL            /* is index partially updated */
/// <exclude />
DEFINE DBOI_SHARED             := DbOrder_Info.DBOI_SHARED             /* is index open in shared mode */
/// <exclude />
DEFINE DBOI_ISREADONLY         := DbOrder_Info.DBOI_ISREADONLY         /* is index open in readonly mode */
/// <exclude />
DEFINE DBOI_READLOCK           := DbOrder_Info.DBOI_READLOCK           /* get/set index read lock */
/// <exclude />
DEFINE DBOI_WRITELOCK          := DbOrder_Info.DBOI_WRITELOCK          /* get/set index write lock */
/// <exclude />
DEFINE DBOI_UPDATECOUNTER      := DbOrder_Info.DBOI_UPDATECOUNTER      /* get/set update index counter */
/// <exclude />
DEFINE DBOI_EVALSTEP           := DbOrder_Info.DBOI_EVALSTEP           /* eval step (EVERY) used in index command */
/// <exclude />
DEFINE DBOI_ISREINDEX          := DbOrder_Info.DBOI_ISREINDEX          /* Is reindex in process */
/// <exclude />
DEFINE DBOI_I_BAGNAME          := DbOrder_Info.DBOI_I_BAGNAME          /* created index name */
/// <exclude />
DEFINE DBOI_I_TAGNAME          := DbOrder_Info.DBOI_I_TAGNAME          /* created tag name */
/// <exclude />
DEFINE DBOI_RELKEYPOS          := DbOrder_Info.DBOI_RELKEYPOS          /* get/set relative key position (in range 0 - 1) */
/// <exclude />
DEFINE DBOI_USECURRENT         := DbOrder_Info.DBOI_USECURRENT         /* get/set "use current index" flag */
/// <exclude />
DEFINE DBOI_INDEXTYPE          := DbOrder_Info.DBOI_INDEXTYPE          /* current index type */
/// <exclude />
DEFINE DBOI_RESETPOS           := DbOrder_Info.DBOI_RESETPOS           /* rest logical and raw positions */
/// <exclude />
DEFINE DBOI_INDEXPAGESIZE      := DbOrder_Info.DBOI_INDEXPAGESIZE      /* get index page size */

/// <exclude />
DEFINE DBOI_USER 				:= DbOrder_Info.DBOI_USER

// Advantage extensions

/// <exclude />
DEFINE DBOI_AXS_PERCENT_INDEXED  := DbOrder_Info.DBOI_AXS_PERCENT_INDEXED
/// <exclude />
DEFINE DBOI_GET_ACE_INDEX_HANDLE := DbOrder_Info.DBOI_GET_ACE_INDEX_HANDLE


// Duplicates
/// <exclude />
DEFINE DBOI_KEYGOTO 	:= DbOrder_Info.DBOI_POSITION
/// <exclude />
DEFINE DBOI_KEYGOTORAW 	:= DbOrder_Info.DBOI_KEYNORAW
/// <exclude />
DEFINE DBOI_KEYNO	 	:= DbOrder_Info.DBOI_POSITION

// Blob defines



/// <exclude />
DEFINE BLOB_INFO_HANDLE 	:= DbInfo.BLOB_INFO_HANDLE 	
/// <exclude />
DEFINE BLOB_FILE_RECOVER 	:= DbInfo.BLOB_FILE_RECOVER 	
/// <exclude />
DEFINE BLOB_FILE_INTEGRITY 	:= DbInfo.BLOB_FILE_INTEGRITY 	
/// <exclude />
DEFINE BLOB_OFFSET 			:= DbInfo.BLOB_OFFSET 			
/// <exclude />
DEFINE BLOB_POINTER 		:= DbInfo.BLOB_POINTER 		
/// <exclude />
DEFINE BLOB_LEN 			:= DbInfo.BLOB_LEN 			
/// <exclude />
DEFINE BLOB_TYPE 			:= DbInfo.BLOB_TYPE 			
/// <exclude />
DEFINE BLOB_EXPORT 			:= DbInfo.BLOB_EXPORT 			
/// <exclude />
DEFINE BLOB_ROOT_UNLOCK 	:= DbInfo.BLOB_ROOT_UNLOCK 	
/// <exclude />
DEFINE BLOB_ROOT_PUT 		:= DbInfo.BLOB_ROOT_PUT 		
/// <exclude />
DEFINE BLOB_ROOT_GET 		:= DbInfo.BLOB_ROOT_GET 		
/// <exclude />
DEFINE BLOB_ROOT_LOCK 		:= DbInfo.BLOB_ROOT_LOCK 		
/// <exclude />
DEFINE BLOB_IMPORT 			:= DbInfo.BLOB_IMPORT 			
/// <exclude />
DEFINE BLOB_DIRECT_PUT 		:= DbInfo.BLOB_DIRECT_PUT 		
/// <exclude />
DEFINE BLOB_DIRECT_GET 		:= DbInfo.BLOB_DIRECT_GET 		
/// <exclude />
DEFINE BLOB_GET 			:= DbInfo.BLOB_GET 			
/// <exclude />
DEFINE BLOB_DIRECT_EXPORT 	:= DbInfo.BLOB_DIRECT_EXPORT 	
/// <exclude />
DEFINE BLOB_DIRECT_IMPORT 	:= DbInfo.BLOB_DIRECT_IMPORT 	
/// <exclude />
DEFINE BLOB_NMODE 			:= DbInfo.BLOB_NMODE 			
/// <exclude />
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

/// <exclude />
DEFINE DBOI_TYPE_UNDEF          := -1 
/// <exclude />
DEFINE DBOI_TYPE_NONE           :=  0 
/// <exclude />
DEFINE DBOI_TYPE_NONCOMPACT     :=  1 
/// <exclude />
DEFINE DBOI_TYPE_COMPACT        :=  2 
/// <exclude />
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
/// <exclude />
DEFINE DB_DBFLOCK_DEFAULT      := 0 
/// <exclude />
DEFINE DB_DBFLOCK_CLIPPER      := 1   /* default Cl*pper locking scheme */
/// <exclude />
DEFINE DB_DBFLOCK_COMIX        := 2   /* COMIX and CL53 DBFCDX hyper locking scheme */
/// <exclude />
DEFINE DB_DBFLOCK_VFP          := 3   /* [V]FP, CL52 DBFCDX, SIx3 SIXCDX, CDXLOCK.OBJ */
/// <exclude />
DEFINE DB_DBFLOCK_HB32         := 4   /* Harbour hyper locking scheme for 32bit file API */
/// <exclude />
DEFINE DB_DBFLOCK_HB64         := 5   /* Harbour hyper locking scheme for 64bit file API */
/// <exclude />
DEFINE DB_DBFLOCK_CLIPPER2     := 6   /* extended Cl*pper locking scheme NTXLOCK2.OBJ */ 


// File Extensions
DEFINE DBT_MEMOEXT             := ".DBT" 
DEFINE FPT_MEMOEXT             := ".FPT" 
DEFINE SMT_MEMOEXT             := ".SMT" 
DEFINE DBV_MEMOEXT             := ".DBV" 

// Blocks
/// <exclude />
DEFINE DBT_DEFBLOCKSIZE        := 512
/// <exclude />
DEFINE FPT_DEFBLOCKSIZE        := 64 
/// <exclude />
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


/// <exclude />
DEFINE RDDI_ISDBF             := RDDInfo.RDDI_ISDBF        /* Does this RDD support DBFs? */
/// <exclude />
DEFINE RDDI_CANPUTREC         := RDDInfo.RDDI_CANPUTREC    /* Can this RDD Put Records? */
/// <exclude />
DEFINE RDDI_DELIMITER         := RDDInfo.RDDI_DELIMITER    /* The field delimiter (as a string) */
/// <exclude />
DEFINE RDDI_SEPARATOR         := RDDInfo.RDDI_SEPARATOR    /* The record separator (as a string) */
							  
/// <exclude />
DEFINE RDDI_TABLEEXT          := RDDInfo.RDDI_TABLEEXT     /* Default data file's file extension */
/// <exclude />
DEFINE RDDI_MEMOEXT           := RDDInfo.RDDI_MEMOEXT      /* Default memo file's file extension */
/// <exclude />
DEFINE RDDI_ORDBAGEXT         := RDDInfo.RDDI_ORDBAGEXT    /* Default multi tag index's file extension */
/// <exclude />
DEFINE RDDI_ORDEREXT          := RDDInfo.RDDI_ORDEREXT     /* default single tag index's file extension */
/// <exclude />
DEFINE RDDI_ORDSTRUCTEXT      := RDDInfo.RDDI_ORDSTRUCTEXT /* default single tag index's file extension */
							  
/// <exclude />
DEFINE RDDI_LOCAL             := RDDInfo.RDDI_LOCAL        /* Local file access? */
/// <exclude />
DEFINE RDDI_REMOTE            := RDDInfo.RDDI_REMOTE       /* Remote table access? */
/// <exclude />
DEFINE RDDI_CONNECTION        := RDDInfo.RDDI_CONNECTION   /* Get/Set default connection */
/// <exclude />
DEFINE RDDI_TABLETYPE         := RDDInfo.RDDI_TABLETYPE    /* Type of table file */
/// <exclude />
DEFINE RDDI_MEMOTYPE          := RDDInfo.RDDI_MEMOTYPE     /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
/// <exclude />
DEFINE RDDI_LARGEFILE         := RDDInfo.RDDI_LARGEFILE    /* Is large file size (>=4GB) supported */
/// <exclude />
DEFINE RDDI_LOCKSCHEME        := RDDInfo.RDDI_LOCKSCHEME   /* Locking scheme used by RDD */
/// <exclude />
DEFINE RDDI_RECORDMAP         := RDDInfo.RDDI_RECORDMAP    /* Does RDD support record map functionality? */
/// <exclude />
DEFINE RDDI_ENCRYPTION        := RDDInfo.RDDI_ENCRYPTION   /* Does RDD support encryption */
/// <exclude />
DEFINE RDDI_TRIGGER           := RDDInfo.RDDI_TRIGGER      /* Get/Set default trigger function */
/// <exclude />
DEFINE RDDI_AUTOLOCK          := RDDInfo.RDDI_AUTOLOCK     /* automatic locking on update */

/* index parameters */
/// <exclude />
DEFINE RDDI_STRUCTORD           := RDDInfo.RDDI_STRUCTORD    /* Are structural indexes supported */
/// <exclude />
DEFINE RDDI_STRICTREAD          := RDDInfo.RDDI_STRICTREAD   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
/// <exclude />
DEFINE RDDI_STRICTSTRUCT        := RDDInfo.RDDI_STRICTSTRUCT /* Flag for strict structural order checking */
/// <exclude />
DEFINE RDDI_OPTIMIZE            := RDDInfo.RDDI_OPTIMIZE     /* Flag for whether to use query optimization */
/// <exclude />
DEFINE RDDI_FORCEOPT            := RDDInfo.RDDI_FORCEOPT     /* Flag for forcing linear optimization */
/// <exclude />
DEFINE RDDI_AUTOOPEN            := RDDInfo.RDDI_AUTOOPEN     /* Flag for automatically opening structural indexes */
/// <exclude />
DEFINE RDDI_AUTOORDER           := RDDInfo.RDDI_AUTOORDER    /* When a structural index is opened, the order to be set */
/// <exclude />
DEFINE RDDI_AUTOSHARE           := RDDInfo.RDDI_AUTOSHARE    /* When a network is detected, open the index shared, otherwise open exclusively */
/// <exclude />
DEFINE RDDI_MULTITAG            := RDDInfo.RDDI_MULTITAG     /* Does RDD support multi tag in index file */
/// <exclude />
DEFINE RDDI_SORTRECNO           := RDDInfo.RDDI_SORTRECNO    /* Is record number part of key in sorting */
/// <exclude />
DEFINE RDDI_MULTIKEY            := RDDInfo.RDDI_MULTIKEY     /* Does custom orders support repeated keys? */

/* memo parameters */
/// <exclude />
DEFINE RDDI_MEMOBLOCKSIZE       := RDDInfo.RDDI_MEMOBLOCKSIZE   /* Memo File's block size */
/// <exclude />
DEFINE RDDI_MEMOVERSION         := RDDInfo.RDDI_MEMOVERSION     /* sub version of memo file */
/// <exclude />
DEFINE RDDI_MEMOGCTYPE          := RDDInfo.RDDI_MEMOGCTYPE      /* type of garbage collector used by GC */
/// <exclude />
DEFINE RDDI_MEMOREADLOCK        := RDDInfo.RDDI_MEMOREADLOCK    /* use read lock in memo file access */
/// <exclude />
DEFINE RDDI_MEMOREUSE           := RDDInfo.RDDI_MEMOREUSE       /* reuse free space on write */
/// <exclude />
DEFINE RDDI_BLOB_SUPPORT        := RDDInfo.RDDI_BLOB_SUPPORT    /* can support BLOB files directly */

/* misc */
/// <exclude />
DEFINE RDDI_PENDINGTRIGGER      := RDDInfo.RDDI_PENDINGTRIGGER    /* set pending trigger for next open operation */
/// <exclude />
DEFINE RDDI_PENDINGPASSWORD     := RDDInfo.RDDI_PENDINGPASSWORD   /* set pending password for next open operation */
/// <exclude />
DEFINE RDDI_PASSWORD            := RDDInfo.RDDI_PASSWORD          /* Get/Set default password */
/// <exclude />
DEFINE RDDI_LOCKRETRY           := RDDInfo.RDDI_LOCKRETRY         /* Get/Set record and file lock timeout value */
/// <exclude />
DEFINE RDDI_DIRTYREAD           := RDDInfo.RDDI_DIRTYREAD         /* Get/Set index dirty read flag */
/// <exclude />
DEFINE RDDI_INDEXPAGESIZE       := RDDInfo.RDDI_INDEXPAGESIZE     /* Get/Set default index page size */
/// <exclude />
DEFINE RDDI_DECIMALS            := RDDInfo.RDDI_DECIMALS          /* Get/Set default number of decimal places for numeric fields if it's undefined */
/// <exclude />
DEFINE RDDI_SETHEADER           := RDDInfo.RDDI_SETHEADER         /* DBF header updating modes */

/* SQL */
/// <exclude />
DEFINE RDDI_CONNECT             := RDDInfo.RDDI_CONNECT         /* connect to database */
/// <exclude />
DEFINE RDDI_DISCONNECT          := RDDInfo.RDDI_DISCONNECT      /* disconnect from database */
/// <exclude />
DEFINE RDDI_EXECUTE             := RDDInfo.RDDI_EXECUTE         /* execute SQL statement */
/// <exclude />
DEFINE RDDI_ERROR               := RDDInfo.RDDI_ERROR           /* error number */
/// <exclude />
DEFINE RDDI_ERRORNO             := RDDInfo.RDDI_ERRORNO         /* error description */
/// <exclude />
DEFINE RDDI_INSERTID            := RDDInfo.RDDI_INSERTID        /* last auto insert ID */
/// <exclude />
DEFINE RDDI_AFFECTEDROWS        := RDDInfo.RDDI_AFFECTEDROWS    /* number of affected rows after UPDATE */
/// <exclude />
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
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_TOP    := 0x01
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_BOTTOM := 0x02
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_BOTH   := 0xFF
