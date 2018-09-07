//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums

// RecInfo defines
DEFINE DBRI_DELETED 	:= DbRecordInfo.DBRI_DELETED 
DEFINE DBRI_LOCKED 		:= DbRecordInfo.DBRI_LOCKED 		
DEFINE DBRI_RECSIZE		:= DbRecordInfo.DBRI_RECSIZE		
DEFINE DBRI_RECNO		:= DbRecordInfo.DBRI_RECNO		
DEFINE DBRI_UPDATED		:= DbRecordInfo.DBRI_UPDATED		
DEFINE DBRI_BUFFPTR 	:= DbRecordInfo.DBRI_BUFFPTR 	
DEFINE DBRI_RAWRECORD   := DbRecordInfo.DBRI_RAWRECORD   
DEFINE DBRI_ENCRYPTED	:= DbRecordInfo.DBRI_ENCRYPTED	
DEFINE DBRI_RAWMEMOS	:= DbRecordInfo.DBRI_RAWMEMOS	
DEFINE DBRI_RAWDATA		:= DbRecordInfo.DBRI_RAWDATA		
DEFINE DBRI_USER		:= DbRecordInfo.DBRI_USER		


// FieldInfo defines
DEFINE DBS_NAME					:= DbFieldInfo.DBS_NAME	
DEFINE DBS_TYPE					:= DbFieldInfo.DBS_TYPE	
DEFINE DBS_LEN					:= DbFieldInfo.DBS_LEN	
DEFINE DBS_DEC					:= DbFieldInfo.DBS_DEC	
DEFINE DBS_ALIAS				:= DbFieldInfo.DBS_ALIAS

DEFINE DBS_ISNULL               := DbFieldInfo.DBS_ISNULL  
DEFINE DBS_COUNTER              := DbFieldInfo.DBS_COUNTER 
DEFINE DBS_STEP                 := DbFieldInfo.DBS_STEP    
DEFINE DBS_BLOB_GET             := DbFieldInfo.DBS_BLOB_GET     
DEFINE DBS_BLOB_TYPE			:= DbFieldInfo.DBS_BLOB_TYPE	
DEFINE DBS_BLOB_LEN				:= DbFieldInfo.DBS_BLOB_LEN		
DEFINE DBS_BLOB_OFFSET			:= DbFieldInfo.DBS_BLOB_OFFSET	
DEFINE DBS_BLOB_POINTER			:= DbFieldInfo.DBS_BLOB_POINTER	
DEFINE DBS_BLOB_DIRECT_TYPE		:= DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
DEFINE DBS_BLOB_DIRECT_LEN		:= DbFieldInfo.DBS_BLOB_DIRECT_LEN		
DEFINE DBS_STRUCT				:= DbFieldInfo.DBS_STRUCT				
DEFINE DBS_PROPERTIES			:= DbFieldInfo.DBS_PROPERTIES			
DEFINE DBS_USER					:= DbFieldInfo.DBS_USER					


// Scope defines
DEFINE TOPSCOPE                := 0 
DEFINE BOTTOMSCOPE             := 1 

//DbInfo Defines
DEFINE DBI_ISDBF 			:=  DbInfo.DBI_ISDBF 		 	// Logical: RDD support DBF file format?
DEFINE DBI_CANPUTREC 		:=  DbInfo.DBI_CANPUTREC 		// Logical: RDD support Putting Records? 
DEFINE DBI_GETHEADERSIZE 	:= 	DbInfo.DBI_GETHEADERSIZE	// Numeric: Get header size of the file 
DEFINE DBI_LASTUPDATE 		:= 	DbInfo.DBI_LASTUPDATE 		// Date:    Last date RDD file updated 
DEFINE DBI_GETDELIMITER 	:= 	DbInfo.DBI_GETDELIMITER		// String:  Get default delimiter
DEFINE DBI_SETDELIMITER 	:=  DbInfo.DBI_SETDELIMITER		// String:  Set default delimiter
DEFINE DBI_GETRECSIZE 		:= 	DbInfo.DBI_GETRECSIZE 		// Numeric: Get record size of the file
DEFINE DBI_GETLOCKARRAY 	:=  DbInfo.DBI_GETLOCKARRAY		// Array:   Get array of locked records 
DEFINE DBI_TABLEEXT 		:=  DbInfo.DBI_TABLEEXT 		 // String:  Get table file extension
DEFINE DBI_FULLPATH 		:= 	DbInfo.DBI_FULLPATH 		// String: Full path to data file
DEFINE DBI_ISFLOCK 			:= 	DbInfo.DBI_ISFLOCK 			// Logic: Is there a file lock active? 
DEFINE DBI_READONLY 		:= 	DbInfo.DBI_READONLY 		// Logic: is the file opened readonly
DEFINE DBI_CHILDCOUNT 		:= 	DbInfo.DBI_CHILDCOUNT 		// Number: Number of child relations set
DEFINE DBI_FILEHANDLE 		:= 	DbInfo.DBI_FILEHANDLE 		// Stream: The data file's file stream
DEFINE DBI_ISANSI 			:= 	DbInfo.DBI_ISANSI 			// Logic: Is the file ansi encoded
DEFINE DBI_BOF 				:= 	DbInfo.DBI_BOF 				// Logic: Same as Bof()
DEFINE DBI_EOF 				:= 	DbInfo.DBI_EOF 				// Logic: Same as Eof()
DEFINE DBI_DBFILTER 		:= 	DbInfo.DBI_DBFILTER 		// String: Current Filter setting 
DEFINE DBI_FOUND 			:= 	DbInfo.DBI_FOUND 			// Logic: Same as Found() 
DEFINE DBI_FCOUNT 			:= 	DbInfo.DBI_FCOUNT 			// Number: Number of fields per record
DEFINE DBI_LOCKCOUNT 		:= 	DbInfo.DBI_LOCKCOUNT 		// Number: Number of record locks  
DEFINE DBI_VALIDBUFFER  	:= 	DbInfo.DBI_VALIDBUFFER		// Logic: Is the buffer Valid
DEFINE DBI_ALIAS 			:= 	DbInfo.DBI_ALIAS 			// String: Alias
DEFINE DBI_GETSCOPE 		:= 	DbInfo.DBI_GETSCOPE 		// Object: The ScopeInfo
DEFINE DBI_LOCKOFFSET 		:= 	DbInfo.DBI_LOCKOFFSET 		// Number: Lock offset
DEFINE DBI_SHARED 			:= 	DbInfo.DBI_SHARED 			// Logic: is the file opened shared
DEFINE DBI_MEMOEXT 			:= 	DbInfo.DBI_MEMOEXT 			// String: Memo file extension
DEFINE DBI_MEMOHANDLE 		:= 	DbInfo.DBI_MEMOHANDLE 		// Stream: The memo file's file stream
DEFINE DBI_BLOB_HANDLE 		:= 	DbInfo.DBI_BLOB_HANDLE 		// Alias for MemoHandle
DEFINE DBI_MEMOBLOCKSIZE 	:= 	DbInfo.DBI_MEMOBLOCKSIZE	// Number: The memo block size
DEFINE DBI_CODEPAGE 		:= 	DbInfo.DBI_CODEPAGE 		// Number: The Windows Codepage
DEFINE DBI_NEWINDEXLOCK 	:= 	DbInfo.DBI_NEWINDEXLOCK 	// Logic:  Use new index lock mechanism
DEFINE DBI_DOSCODEPAGE 		:= 	DbInfo.DBI_DOSCODEPAGE 		// Number: The DOS Codepage
DEFINE DBI_STRICTREAD  		:= DbInfo.DBI_STRICTREAD  	// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
DEFINE DBI_OPTIMIZE    		:= DbInfo.DBI_OPTIMIZE    	// Flag for whether to use query optimization             
DEFINE DBI_AUTOOPEN    		:= DbInfo.DBI_AUTOOPEN    	// Flag for automatically opening structural indexes      
DEFINE DBI_AUTOORDER   		:= DbInfo.DBI_AUTOORDER   	// When a structural index is opened, the order to be set 
DEFINE DBI_AUTOSHARE   		:= DbInfo.DBI_AUTOSHARE   	// When a network is detected, open the index shared, otherwise open exclusively   
DEFINE DBI_DB_VERSION 		:= DbInfo.DBI_DB_VERSION 		
DEFINE DBI_RDD_VERSION 		:= DbInfo.DBI_RDD_VERSION 		
DEFINE DBI_RDD_LIST 		:= DbInfo.DBI_RDD_LIST 		
DEFINE DBI_MEMOFIELD 		:= DbInfo.DBI_MEMOFIELD 		
DEFINE DBI_VO_MACRO_SYNTAX	:= DbInfo.DBI_VO_MACRO_SYNTAX	
DEFINE DBI_RDD_OBJECT 		:= DbInfo.DBI_RDD_OBJECT 		

/* CA-Cl*pper documented for public use */

DEFINE DBI_BLOB_DIRECT_LEN     := DbInfo.BLOB_DIRECT_LEN 
DEFINE DBI_BLOB_DIRECT_TYPE    := DbInfo.BLOB_DIRECT_TYPE
DEFINE DBI_BLOB_INTEGRITY      := DbInfo.BLOB_FILE_INTEGRITY  
DEFINE DBI_BLOB_OFFSET         := DbInfo.BLOB_OFFSET     
DEFINE DBI_BLOB_RECOVER        := DbInfo.BLOB_FILE_RECOVER


/* Harbour extension */

DEFINE DBI_LOCKSCHEME          := DbInfo.DBI_LOCKSCHEME      /* Locking scheme used by RDD */
DEFINE DBI_ISREADONLY          := DbInfo.DBI_ISREADONLY      /* Was the file opened readonly? */
DEFINE DBI_ROLLBACK            := DbInfo.DBI_ROLLBACK        /* Rollback changes made to current record */
DEFINE DBI_PASSWORD            := DbInfo.DBI_PASSWORD        /* Workarea password */
DEFINE DBI_ISENCRYPTED         := DbInfo.DBI_ISENCRYPTED     /* The database is encrypted */
DEFINE DBI_MEMOTYPE            := DbInfo.DBI_MEMOTYPE        /* Type of MEMO file: DBT, SMT, FPT */
DEFINE DBI_SEPARATOR           := DbInfo.DBI_SEPARATOR       /* The record separator (as a string) */
DEFINE DBI_MEMOVERSION         := DbInfo.DBI_MEMOVERSION     /* sub version of memo file */
DEFINE DBI_TABLETYPE           := DbInfo.DBI_TABLETYPE       /* Type of table file */
DEFINE DBI_SCOPEDRELATION      := DbInfo.DBI_SCOPEDRELATION  /* Is given relation scoped */
DEFINE DBI_TRIGGER             := DbInfo.DBI_TRIGGER         /* Get/Set trigger function */
DEFINE DBI_OPENINFO            := DbInfo.DBI_OPENINFO        /* DBOPENINFO structure pointer */
DEFINE DBI_ENCRYPT             := DbInfo.DBI_ENCRYPT         /* Encrypt table */
DEFINE DBI_DECRYPT             := DbInfo.DBI_DECRYPT         /* Decrypt table */
DEFINE DBI_MEMOPACK            := DbInfo.DBI_MEMOPACK        /* Pack memo file */
DEFINE DBI_DIRTYREAD           := DbInfo.DBI_DIRTYREAD       /* Get/Set index dirty read flag */
DEFINE DBI_POSITIONED          := DbInfo.DBI_POSITIONED      /* Is cursor positioned to valid record */
DEFINE DBI_ISTEMPORARY         := DbInfo.DBI_ISTEMPORARY     /* Is the table a temporary one? */
DEFINE DBI_LOCKTEST            := DbInfo.DBI_LOCKTEST        /* record / file lock test */
DEFINE DBI_CODEPAGE_HB         := DbInfo.DBI_CODEPAGE_HB     /* Codepage used also defined by VO and Vulcan */ 
DEFINE DBI_TRANSREC            := DbInfo.DBI_TRANSREC         /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
DEFINE DBI_SETHEADER		   := DbInfo.DBI_SETHEADER		/* DBF header updating modes */ 
DEFINE DBI_QUERY			   := DbInfo.DBI_QUERY			/* if area represents result of a query, obtain expression of this query */ 

/* Harbour RECORD MAP (RM) support */

DEFINE DBI_RM_SUPPORTED        := DbInfo.DBI_RM_SUPPORTED    /* has WA RDD record map support? */
DEFINE DBI_RM_CREATE           := DbInfo.DBI_RM_CREATE     /* create new empty work area record map */
DEFINE DBI_RM_REMOVE           := DbInfo.DBI_RM_REMOVE     /* remove active work area record map */
DEFINE DBI_RM_CLEAR            := DbInfo.DBI_RM_CLEAR      /* remove all records from WA record map */
DEFINE DBI_RM_FILL             := DbInfo.DBI_RM_FILL       /* add all records to WA record map */
DEFINE DBI_RM_ADD              := DbInfo.DBI_RM_ADD        /* add record to work area record map */
DEFINE DBI_RM_DROP             := DbInfo.DBI_RM_DROP       /* remove record from work area record map */
DEFINE DBI_RM_TEST             := DbInfo.DBI_RM_TEST       /* test if record is set in WA record map */
DEFINE DBI_RM_COUNT            := DbInfo.DBI_RM_COUNT      /* number of records set in record map */
DEFINE DBI_RM_HANDLE           := DbInfo.DBI_RM_HANDLE     /* get/set record map filter handle */ 



// CDX and Comix Record List Support

DEFINE DBI_RL_AND 		:= DbInfo.DBI_RL_AND 		
DEFINE DBI_RL_CLEAR 	:= DbInfo.DBI_RL_CLEAR 	
DEFINE DBI_RL_COUNT 	:= DbInfo.DBI_RL_COUNT 	
DEFINE DBI_RL_DESTROY 	:= DbInfo.DBI_RL_DESTROY 	
DEFINE DBI_RL_EXFILTER 	:= DbInfo.DBI_RL_EXFILTER 	
DEFINE DBI_RL_GETFILTER := DbInfo.DBI_RL_GETFILTER 
DEFINE DBI_RL_HASMAYBE 	:= DbInfo.DBI_RL_HASMAYBE 	
DEFINE DBI_RL_LEN 		:= DbInfo.DBI_RL_LEN 		
DEFINE DBI_RL_MAYBEEVAL := DbInfo.DBI_RL_MAYBEEVAL 
DEFINE DBI_RL_NEW 		:= DbInfo.DBI_RL_NEW 		
DEFINE DBI_RL_NEWDUP 	:= DbInfo.DBI_RL_NEWDUP 	
DEFINE DBI_RL_NEWQUERY 	:= DbInfo.DBI_RL_NEWQUERY 	
DEFINE DBI_RL_NEXTRECNO := DbInfo.DBI_RL_NEXTRECNO 
DEFINE DBI_RL_NOT 		:= DbInfo.DBI_RL_NOT 		
DEFINE DBI_RL_OR 		:= DbInfo.DBI_RL_OR 		
DEFINE DBI_RL_PREVRECNO := DbInfo.DBI_RL_PREVRECNO 
DEFINE DBI_RL_SET 		:= DbInfo.DBI_RL_SET 		
DEFINE DBI_RL_SETFILTER := DbInfo.DBI_RL_SETFILTER 
DEFINE DBI_RL_TEST 		:= DbInfo.DBI_RL_TEST 		


DEFINE DBI_USER 				:= DbInfo.DBI_USER	// Start of user definable DBI_ values

// Advantage additions
DEFINE DBI_GET_ACE_TABLE_HANDLE  := DbInfo.DBI_GET_ACE_TABLE_HANDLE
DEFINE DBI_GET_ACE_STMT_HANDLE   := DbInfo.DBI_GET_ACE_STMT_HANDLE


// OrderInfo Defines
DEFINE DBOI_CONDITION 	:= DBOrderInfo.DBOI_CONDITION      // String: The order's conditional expression     
DEFINE DBOI_EXPRESSION 	:= DBOrderInfo.DBOI_EXPRESSION 	// String: The order's key expression             
DEFINE DBOI_POSITION 	:= DBOrderInfo.DBOI_POSITION  	// Number: The current key position in scope and filter  
DEFINE DBOI_RECNO 		:= DBOrderInfo.DBOI_RECNO 		  	// Number: The current key position disregarding filters 
DEFINE DBOI_NAME 		:= DBOrderInfo.DBOI_NAME 		   	// String: The name of the order                      
DEFINE DBOI_NUMBER 		:= DBOrderInfo.DBOI_NUMBER 		 	// Number: The numeric position in the list of orders
DEFINE DBOI_BAGNAME 	:= DBOrderInfo.DBOI_BAGNAME 	 	// String: The name of the file containing this order
DEFINE DBOI_INDEXNAME 	:= DBOrderInfo.DBOI_INDEXNAME   // Alias
DEFINE DBOI_BAGEXT 		:= DBOrderInfo.DBOI_BAGEXT 		   	// String: The extension of the file containing this order
DEFINE DBOI_INDEXEXT  	:= DBOrderInfo.DBOI_INDEXEXT  	   // Alias
DEFINE DBOI_ORDERCOUNT  := DBOrderInfo.DBOI_ORDERCOUNT     // Number: The count of ORDERS contained in an index file or in total
DEFINE DBOI_FILEHANDLE 	:= DBOrderInfo.DBOI_FILEHANDLE 		// Stream: The stream of the index
DEFINE DBOI_ISCOND 		:= DBOrderInfo.DBOI_ISCOND 			// Logic : Does the order have a FOR condition?
DEFINE DBOI_ISDESC 		:= DBOrderInfo.DBOI_ISDESC 			// Logic : Is the order DESCENDing? 
DEFINE DBOI_UNIQUE 		:= DBOrderInfo.DBOI_UNIQUE 			// Logic : Does the order have the UNIQUE attribute?


/* Clipper 5.3-level constants */
DEFINE DBOI_FULLPATH 	:= DBOrderInfo.DBOI_FULLPATH 	   	// String: The full path to the index file (Bag)
DEFINE DBOI_KEYTYPE 	:= DBOrderInfo.DBOI_KEYTYPE 	 	// The type of the order's key  
DEFINE DBOI_KEYSIZE 	:= DBOrderInfo.DBOI_KEYSIZE 	 	// Number: The length of the order's key
DEFINE DBOI_KEYCOUNT 	:= DBOrderInfo.DBOI_KEYCOUNT 	 	// Number: The count of keys in scope and filter
DEFINE DBOI_SETCODEBLOCK:= DBOrderInfo.DBOI_SETCODEBLOCK 	// Block : The codeblock that produces the key 
DEFINE DBOI_KEYDEC 		:= DBOrderInfo.DBOI_KEYDEC 		 	// Number: The # of decimals in a numeric key 
DEFINE DBOI_HPLOCKING 	:= DBOrderInfo.DBOI_HPLOCKING 	 	// Logic : Using High Performance locking for this order?
DEFINE DBOI_LOCKOFFSET 	:= DBOrderInfo.DBOI_LOCKOFFSET  	// Number: The offset used for logical locking 
DEFINE DBOI_KEYADD 		:= DbOrderInfo.DBOI_KEYADD 		 	// Logic: Custom Index: Was Key added successfully? 
DEFINE DBOI_KEYDELETE 	:= DbOrderInfo.DBOI_KEYDELETE 		// Logic: Custom Index: Was Key Deletion successful? 
DEFINE DBOI_KEYVAL 		:= DbOrderInfo.DBOI_KEYVAL 			// Object: The value of the current key 
DEFINE DBOI_SCOPETOP 	:= DbOrderInfo.DBOI_SCOPETOP 		// Object: Get or Set the scope top    
DEFINE DBOI_SCOPEBOTTOM := DbOrderInfo.DBOI_SCOPEBOTTOM	// Object: Get or Set the scope bottom
DEFINE DBOI_SCOPETOPCLEAR := DbOrderInfo.DBOI_SCOPETOPCLEAR  	// None	 :
DEFINE DBOI_SCOPEBOTTOMCLEAR:= DbOrderInfo.DBOI_SCOPEBOTTOMCLEAR // None :
DEFINE DBOI_CUSTOM 		:= DbOrderInfo.DBOI_CUSTOM // Logic: Is this a Custom Index?  
DEFINE DBOI_SKIPUNIQUE 	:= DbOrderInfo.DBOI_SKIPUNIQUE // Logic: Was a skip to adjacent unique Key successful?  
DEFINE DBOI_KEYSINCLUDED:= DbOrderInfo.DBOI_KEYSINCLUDED 	// Number: Number of keys in the index order
DEFINE DBOI_KEYNORAW 	:= DbOrderInfo.DBOI_KEYNORAW 	 // Number: The key number disregarding filters
DEFINE DBOI_KEYCOUNTRAW := DbOrderInfo.DBOI_KEYCOUNTRAW   // Number: The key count disregarding filter  
DEFINE DBOI_OPTLEVEL 	:= DbOrderInfo.DBOI_OPTLEVEL 	 // Number: Optimization level for current query


DEFINE DBOI_STRICTREAD := DbOrderInfo.DBOI_STRICTREAD  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
DEFINE DBOI_OPTIMIZE   := DbOrderInfo.DBOI_OPTIMIZE    /* Flag for whether to use query optimization             */
DEFINE DBOI_AUTOOPEN   := DbOrderInfo.DBOI_AUTOOPEN    /* Flag for automatically opening structural indexes      */
DEFINE DBOI_AUTOORDER  := DbOrderInfo.DBOI_AUTOORDER   /* When a structural index is opened, the order to be set */
DEFINE DBOI_AUTOSHARE  := DbOrderInfo.DBOI_AUTOSHARE   /* When a network is detected, open the index shared, otherwise open exclusively   */ 


/* Harbour extensions */

DEFINE DBOI_SKIPEVAL           := DbOrderInfo.DBOI_SKIPEVAL           /* skip while code block doesn't return TRUE */
DEFINE DBOI_SKIPEVALBACK       := DbOrderInfo.DBOI_SKIPEVALBACK       /* skip backward while code block doesn't return TRUE */
DEFINE DBOI_SKIPREGEX          := DbOrderInfo.DBOI_SKIPREGEX          /* skip while regular expression on index key doesn't return TRUE */
DEFINE DBOI_SKIPREGEXBACK      := DbOrderInfo.DBOI_SKIPREGEXBACK      /* skip backward while regular expression on index key doesn't return TRUE */
DEFINE DBOI_SKIPWILD           := DbOrderInfo.DBOI_SKIPWILD           /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
DEFINE DBOI_SKIPWILDBACK       := DbOrderInfo.DBOI_SKIPWILDBACK       /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
DEFINE DBOI_SCOPEEVAL          := DbOrderInfo.DBOI_SCOPEEVAL          /* skip through index evaluating given C function */
DEFINE DBOI_FINDREC            := DbOrderInfo.DBOI_FINDREC            /* find given record in a Tag beginning from TOP */
DEFINE DBOI_FINDRECCONT        := DbOrderInfo.DBOI_FINDRECCONT        /* find given record in a Tag beginning from current position */
DEFINE DBOI_SCOPESET           := DbOrderInfo.DBOI_SCOPESET           /* set both scopes */
DEFINE DBOI_SCOPECLEAR         := DbOrderInfo.DBOI_SCOPECLEAR         /* clear both scopes */


DEFINE DBOI_BAGCOUNT           := DbOrderInfo.DBOI_BAGCOUNT           /* number of open order bags */
DEFINE DBOI_BAGNUMBER          := DbOrderInfo.DBOI_BAGNUMBER          /* bag position in bag list */
DEFINE DBOI_BAGORDER           := DbOrderInfo.DBOI_BAGORDER           /* number of first order in a bag */
DEFINE DBOI_ISMULTITAG         := DbOrderInfo.DBOI_ISMULTITAG         /* does RDD support multi tag in index file */
DEFINE DBOI_ISSORTRECNO        := DbOrderInfo.DBOI_ISSORTRECNO        /* is record number part of key in sorting */
DEFINE DBOI_LARGEFILE          := DbOrderInfo.DBOI_LARGEFILE          /* is large file size (>=4GB) supported */
DEFINE DBOI_TEMPLATE           := DbOrderInfo.DBOI_TEMPLATE           /* order with free user keys */
DEFINE DBOI_MULTIKEY           := DbOrderInfo.DBOI_MULTIKEY           /* custom order with multikeys */
DEFINE DBOI_CHGONLY            := DbOrderInfo.DBOI_CHGONLY            /* update only existing keys */
DEFINE DBOI_PARTIAL            := DbOrderInfo.DBOI_PARTIAL            /* is index partially updated */
DEFINE DBOI_SHARED             := DbOrderInfo.DBOI_SHARED             /* is index open in shared mode */
DEFINE DBOI_ISREADONLY         := DbOrderInfo.DBOI_ISREADONLY         /* is index open in readonly mode */
DEFINE DBOI_READLOCK           := DbOrderInfo.DBOI_READLOCK           /* get/set index read lock */
DEFINE DBOI_WRITELOCK          := DbOrderInfo.DBOI_WRITELOCK          /* get/set index write lock */
DEFINE DBOI_UPDATECOUNTER      := DbOrderInfo.DBOI_UPDATECOUNTER      /* get/set update index counter */
DEFINE DBOI_EVALSTEP           := DbOrderInfo.DBOI_EVALSTEP           /* eval step (EVERY) used in index command */
DEFINE DBOI_ISREINDEX          := DbOrderInfo.DBOI_ISREINDEX          /* Is reindex in process */
DEFINE DBOI_I_BAGNAME          := DbOrderInfo.DBOI_I_BAGNAME          /* created index name */
DEFINE DBOI_I_TAGNAME          := DbOrderInfo.DBOI_I_TAGNAME          /* created tag name */
DEFINE DBOI_RELKEYPOS          := DbOrderInfo.DBOI_RELKEYPOS          /* get/set relative key position (in range 0 - 1) */
DEFINE DBOI_USECURRENT         := DbOrderInfo.DBOI_USECURRENT         /* get/set "use current index" flag */
DEFINE DBOI_INDEXTYPE          := DbOrderInfo.DBOI_INDEXTYPE          /* current index type */
DEFINE DBOI_RESETPOS           := DbOrderInfo.DBOI_RESETPOS           /* rest logical and raw positions */
DEFINE DBOI_INDEXPAGESIZE      := DbOrderInfo.DBOI_INDEXPAGESIZE      /* get index page size */

DEFINE DBOI_USER 				:= DbOrderInfo.DBOI_USER

// Advantage extensions

DEFINE DBOI_AXS_PERCENT_INDEXED  := DbOrderInfo.DBOI_AXS_PERCENT_INDEXED
DEFINE DBOI_GET_ACE_INDEX_HANDLE := DbOrderInfo.DBOI_GET_ACE_INDEX_HANDLE


// Duplicates
DEFINE DBOI_KEYGOTO 	:= DbOrderInfo.DBOI_POSITION
DEFINE DBOI_KEYGOTORAW 	:= DbOrderInfo.DBOI_KEYNORAW
DEFINE DBOI_KEYNO	 	:= DbOrderInfo.DBOI_POSITION

// Blob defines



DEFINE BLOB_INFO_HANDLE 	:= DbInfo.BLOB_INFO_HANDLE 	
DEFINE BLOB_FILE_RECOVER 	:= DbInfo.BLOB_FILE_RECOVER 	
DEFINE BLOB_FILE_INTEGRITY 	:= DbInfo.BLOB_FILE_INTEGRITY 	
DEFINE BLOB_OFFSET 			:= DbInfo.BLOB_OFFSET 			
DEFINE BLOB_POINTER 		:= DbInfo.BLOB_POINTER 		
DEFINE BLOB_LEN 			:= DbInfo.BLOB_LEN 			
DEFINE BLOB_TYPE 			:= DbInfo.BLOB_TYPE 			
DEFINE BLOB_EXPORT 			:= DbInfo.BLOB_EXPORT 			
DEFINE BLOB_ROOT_UNLOCK 	:= DbInfo.BLOB_ROOT_UNLOCK 	
DEFINE BLOB_ROOT_PUT 		:= DbInfo.BLOB_ROOT_PUT 		
DEFINE BLOB_ROOT_GET 		:= DbInfo.BLOB_ROOT_GET 		
DEFINE BLOB_ROOT_LOCK 		:= DbInfo.BLOB_ROOT_LOCK 		
DEFINE BLOB_IMPORT 			:= DbInfo.BLOB_IMPORT 			
DEFINE BLOB_DIRECT_PUT 		:= DbInfo.BLOB_DIRECT_PUT 		
DEFINE BLOB_DIRECT_GET 		:= DbInfo.BLOB_DIRECT_GET 		
DEFINE BLOB_GET 			:= DbInfo.BLOB_GET 			
DEFINE BLOB_DIRECT_EXPORT 	:= DbInfo.BLOB_DIRECT_EXPORT 	
DEFINE BLOB_DIRECT_IMPORT 	:= DbInfo.BLOB_DIRECT_IMPORT 	
DEFINE BLOB_NMODE 			:= DbInfo.BLOB_NMODE 			
DEFINE BLOB_USER			:= DbInfo.BLOB_USER

DEFINE BLOB_EXPORT_APPEND 	:= 0 
DEFINE BLOB_EXPORT_OVERWRITE:= 1 

DEFINE BLOB_IMPORT_COMPRESS := 1 
DEFINE BLOB_IMPORT_ENCRYPT	:= 2 


/* return values for DBOI_OPTLEVEL */

DEFINE DBOI_OPTIMIZED_NONE       := 0 
DEFINE DBOI_OPTIMIZED_PART       := 1 
DEFINE DBOI_OPTIMIZED_FULL       := 2 

/* return values for DBOI_INDEXTYPE */

DEFINE DBOI_TYPE_UNDEF          := -1 
DEFINE DBOI_TYPE_NONE           :=  0 
DEFINE DBOI_TYPE_NONCOMPACT     :=  1 
DEFINE DBOI_TYPE_COMPACT        :=  2 
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
DEFINE DB_DBFLOCK_DEFAULT      := 0 
DEFINE DB_DBFLOCK_CLIPPER      := 1   /* default Cl*pper locking scheme */
DEFINE DB_DBFLOCK_COMIX        := 2   /* COMIX and CL53 DBFCDX hyper locking scheme */
DEFINE DB_DBFLOCK_VFP          := 3   /* [V]FP, CL52 DBFCDX, SIx3 SIXCDX, CDXLOCK.OBJ */
DEFINE DB_DBFLOCK_HB32         := 4   /* Harbour hyper locking scheme for 32bit file API */
DEFINE DB_DBFLOCK_HB64         := 5   /* Harbour hyper locking scheme for 64bit file API */
DEFINE DB_DBFLOCK_CLIPPER2     := 6   /* extended Cl*pper locking scheme NTXLOCK2.OBJ */ 


// File Extensions
DEFINE DBT_MEMOEXT             := ".dbt" 
DEFINE FPT_MEMOEXT             := ".fpt" 
DEFINE SMT_MEMOEXT             := ".smt" 
DEFINE DBV_MEMOEXT             := ".dbv" 

// Blocks
DEFINE DBT_DEFBLOCKSIZE        := 512
DEFINE FPT_DEFBLOCKSIZE        := 64 
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


DEFINE RDDI_ISDBF             := RDDInfo.RDDI_ISDBF        /* Does this RDD support DBFs? */
DEFINE RDDI_CANPUTREC         := RDDInfo.RDDI_CANPUTREC    /* Can this RDD Put Records? */
DEFINE RDDI_DELIMITER         := RDDInfo.RDDI_DELIMITER    /* The field delimiter (as a string) */
DEFINE RDDI_SEPARATOR         := RDDInfo.RDDI_SEPARATOR    /* The record separator (as a string) */
							  
DEFINE RDDI_TABLEEXT          := RDDInfo.RDDI_TABLEEXT     /* Default data file's file extension */
DEFINE RDDI_MEMOEXT           := RDDInfo.RDDI_MEMOEXT      /* Default memo file's file extension */
DEFINE RDDI_ORDBAGEXT         := RDDInfo.RDDI_ORDBAGEXT    /* Default multi tag index's file extension */
DEFINE RDDI_ORDEREXT          := RDDInfo.RDDI_ORDEREXT     /* default single tag index's file extension */
DEFINE RDDI_ORDSTRUCTEXT      := RDDInfo.RDDI_ORDSTRUCTEXT /* default single tag index's file extension */
							  
DEFINE RDDI_LOCAL             := RDDInfo.RDDI_LOCAL        /* Local file access? */
DEFINE RDDI_REMOTE            := RDDInfo.RDDI_REMOTE       /* Remote table access? */
DEFINE RDDI_CONNECTION        := RDDInfo.RDDI_CONNECTION   /* Get/Set default connection */
DEFINE RDDI_TABLETYPE         := RDDInfo.RDDI_TABLETYPE    /* Type of table file */
DEFINE RDDI_MEMOTYPE          := RDDInfo.RDDI_MEMOTYPE     /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
DEFINE RDDI_LARGEFILE         := RDDInfo.RDDI_LARGEFILE    /* Is large file size (>=4GB) supported */
DEFINE RDDI_LOCKSCHEME        := RDDInfo.RDDI_LOCKSCHEME   /* Locking scheme used by RDD */
DEFINE RDDI_RECORDMAP         := RDDInfo.RDDI_RECORDMAP    /* Does RDD support record map functionality? */
DEFINE RDDI_ENCRYPTION        := RDDInfo.RDDI_ENCRYPTION   /* Does RDD support encryption */
DEFINE RDDI_TRIGGER           := RDDInfo.RDDI_TRIGGER      /* Get/Set default trigger function */
DEFINE RDDI_AUTOLOCK          := RDDInfo.RDDI_AUTOLOCK     /* automatic locking on update */

/* index parameters */
DEFINE RDDI_STRUCTORD           := RDDInfo.RDDI_STRUCTORD    /* Are structural indexes supported */
DEFINE RDDI_STRICTREAD          := RDDInfo.RDDI_STRICTREAD   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
DEFINE RDDI_STRICTSTRUCT        := RDDInfo.RDDI_STRICTSTRUCT /* Flag for strict structural order checking */
DEFINE RDDI_OPTIMIZE            := RDDInfo.RDDI_OPTIMIZE     /* Flag for whether to use query optimization */
DEFINE RDDI_FORCEOPT            := RDDInfo.RDDI_FORCEOPT     /* Flag for forcing linear optimization */
DEFINE RDDI_AUTOOPEN            := RDDInfo.RDDI_AUTOOPEN     /* Flag for automatically opening structural indexes */
DEFINE RDDI_AUTOORDER           := RDDInfo.RDDI_AUTOORDER    /* When a structural index is opened, the order to be set */
DEFINE RDDI_AUTOSHARE           := RDDInfo.RDDI_AUTOSHARE    /* When a network is detected, open the index shared, otherwise open exclusively */
DEFINE RDDI_MULTITAG            := RDDInfo.RDDI_MULTITAG     /* Does RDD support multi tag in index file */
DEFINE RDDI_SORTRECNO           := RDDInfo.RDDI_SORTRECNO    /* Is record number part of key in sorting */
DEFINE RDDI_MULTIKEY            := RDDInfo.RDDI_MULTIKEY     /* Does custom orders support repeated keys? */

/* memo parameters */
DEFINE RDDI_MEMOBLOCKSIZE       := RDDInfo.RDDI_MEMOBLOCKSIZE   /* Memo File's block size */
DEFINE RDDI_MEMOVERSION         := RDDInfo.RDDI_MEMOVERSION     /* sub version of memo file */
DEFINE RDDI_MEMOGCTYPE          := RDDInfo.RDDI_MEMOGCTYPE      /* type of garbage collector used by GC */
DEFINE RDDI_MEMOREADLOCK        := RDDInfo.RDDI_MEMOREADLOCK    /* use read lock in memo file access */
DEFINE RDDI_MEMOREUSE           := RDDInfo.RDDI_MEMOREUSE       /* reuse free space on write */
DEFINE RDDI_BLOB_SUPPORT        := RDDInfo.RDDI_BLOB_SUPPORT    /* can support BLOB files directly */

/* misc */
DEFINE RDDI_PENDINGTRIGGER      := RDDInfo.RDDI_PENDINGTRIGGER    /* set pending trigger for next open operation */
DEFINE RDDI_PENDINGPASSWORD     := RDDInfo.RDDI_PENDINGPASSWORD   /* set pending password for next open operation */
DEFINE RDDI_PASSWORD            := RDDInfo.RDDI_PASSWORD          /* Get/Set default password */
DEFINE RDDI_LOCKRETRY           := RDDInfo.RDDI_LOCKRETRY         /* Get/Set record and file lock timeout value */
DEFINE RDDI_DIRTYREAD           := RDDInfo.RDDI_DIRTYREAD         /* Get/Set index dirty read flag */
DEFINE RDDI_INDEXPAGESIZE       := RDDInfo.RDDI_INDEXPAGESIZE     /* Get/Set default index page size */
DEFINE RDDI_DECIMALS            := RDDInfo.RDDI_DECIMALS          /* Get/Set default number of decimal places for numeric fields if it's undefined */
DEFINE RDDI_SETHEADER           := RDDInfo.RDDI_SETHEADER         /* DBF header updating modes */

/* SQL */
DEFINE RDDI_CONNECT             := RDDInfo.RDDI_CONNECT         /* connect to database */
DEFINE RDDI_DISCONNECT          := RDDInfo.RDDI_DISCONNECT      /* disconnect from database */
DEFINE RDDI_EXECUTE             := RDDInfo.RDDI_EXECUTE         /* execute SQL statement */
DEFINE RDDI_ERROR               := RDDInfo.RDDI_ERROR           /* error number */
DEFINE RDDI_ERRORNO             := RDDInfo.RDDI_ERRORNO         /* error description */
DEFINE RDDI_INSERTID            := RDDInfo.RDDI_INSERTID        /* last auto insert ID */
DEFINE RDDI_AFFECTEDROWS        := RDDInfo.RDDI_AFFECTEDROWS    /* number of affected rows after UPDATE */
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
DEFINE EDB                 := Subcodes.EDB
DEFINE EDB_SEEK            := SubCodes.EDB_SEEK          
DEFINE EDB_SKIP            := SubCodes.EDB_SKIP          
DEFINE EDB_GOTO            := SubCodes.EDB_GOTO          
DEFINE EDB_SETRELATION     := SubCodes.EDB_SETRELATION   
DEFINE EDB_USE             := SubCodes.EDB_USE           
DEFINE EDB_CREATEINDEX     := SubCodes.EDB_CREATEINDEX   
DEFINE EDB_SETORDER        := SubCodes.EDB_SETORDER      
DEFINE EDB_SETINDEX        := SubCodes.EDB_SETINDEX      
DEFINE EDB_FIELDNAME       := SubCodes.EDB_FIELDNAME     
DEFINE EDB_BADALIAS        := SubCodes.EDB_BADALIAS      
DEFINE EDB_DUPALIAS        := SubCodes.EDB_DUPALIAS      
DEFINE EDB_SETFILTER       := SubCodes.EDB_SETFILTER     
DEFINE EDB_CYCLICREL       := SubCodes.EDB_CYCLICREL     
DEFINE EDB_CREATETABLE     := SubCodes.EDB_CREATETABLE   
DEFINE EDB_RDDNOTFOUND     := SubCodes.EDB_RDDNOTFOUND   
// RESERVED EDB + 16
DEFINE EDB_FIELDINDEX      := SubCodes.EDB_FIELDINDEX   
DEFINE EDB_SELECT          := SubCodes.EDB_SELECT       
DEFINE EDB_SYMSELECT       := SubCodes.EDB_SYMSELECT    
DEFINE EDB_TOTAL           := SubCodes.EDB_TOTAL        
DEFINE EDB_RECNO           := SubCodes.EDB_RECNO        
DEFINE EDB_EXPRESSION      := SubCodes.EDB_EXPRESSION   
DEFINE EDB_EXPR_WIDTH      := SubCodes.EDB_EXPR_WIDTH   
// RESERVED EDB + 24 - 29
DEFINE EDB_DRIVERLOAD      := SubCodes.EDB_DRIVERLOAD      
DEFINE EDB_PARAM           := SubCodes.EDB_PARAM           
DEFINE EDB_NOAREAS         := SubCodes.EDB_NOAREAS         
DEFINE EDB_NOMEM           := SubCodes.EDB_NOMEM           
DEFINE EDB_NOFIELDS        := SubCodes.EDB_NOFIELDS        
DEFINE EDB_BAD_ERROR_INFO  := SubCodes.EDB_BAD_ERROR_INFO  
DEFINE EDB_WRONGFIELDNAME  := SubCodes.EDB_WRONGFIELDNAME  
DEFINE EDB_ORDDESTROY      := SubCodes.EDB_ORDDESTROY      
DEFINE EDB_NOINITFUNCTION  := SubCodes.EDB_NOINITFUNCTION  
DEFINE EDB_ERRORINIT       := SubCodes.EDB_ERRORINIT       
DEFINE EDB_DBSTRUCT        := SubCodes.EDB_DBSTRUCT        

DEFINE EDB_NOTABLE         := SubCodes.EDB_NOTABLE       
DEFINE EDB_NOORDER         := SubCodes.EDB_NOORDER       

DEFINE EDB_ASSERTION       := SubCodes.EDB_ASSERTION     


