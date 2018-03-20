//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
// RecInfo defines
define DBRI_DELETED 	:= DbRecordInfo.DBRI_DELETED 
define DBRI_LOCKED 		:= DbRecordInfo.DBRI_LOCKED 		
define DBRI_RECSIZE		:= DbRecordInfo.DBRI_RECSIZE		
define DBRI_RECNO		:= DbRecordInfo.DBRI_RECNO		
define DBRI_UPDATED		:= DbRecordInfo.DBRI_UPDATED		
define DBRI_BUFFPTR 	:= DbRecordInfo.DBRI_BUFFPTR 	
define DBRI_RAWRECORD   := DbRecordInfo.DBRI_RAWRECORD   
define DBRI_ENCRYPTED	:= DbRecordInfo.DBRI_ENCRYPTED	
define DBRI_RAWMEMOS	:= DbRecordInfo.DBRI_RAWMEMOS	
define DBRI_RAWDATA		:= DbRecordInfo.DBRI_RAWDATA		
define DBRI_USER		:= DbRecordInfo.DBRI_USER		


// FieldInfo defines
define DBS_NAME					:= DbFieldInfo.DBS_NAME	
define DBS_TYPE					:= DbFieldInfo.DBS_TYPE	
define DBS_LEN					:= DbFieldInfo.DBS_LEN	
define DBS_DEC					:= DbFieldInfo.DBS_DEC	
define DBS_ALIAS				:= DbFieldInfo.DBS_ALIAS

define DBS_ISNULL               := DbFieldInfo.DBS_ISNULL  
define DBS_COUNTER              := DbFieldInfo.DBS_COUNTER 
define DBS_STEP                 := DbFieldInfo.DBS_STEP    

define DBS_BLOB_GET             := DbFieldInfo.DBS_BLOB_GET     
define DBS_BLOB_TYPE			:= DbFieldInfo.DBS_BLOB_TYPE	
define DBS_BLOB_LEN				:= DbFieldInfo.DBS_BLOB_LEN		
define DBS_BLOB_OFFSET			:= DbFieldInfo.DBS_BLOB_OFFSET	
define DBS_BLOB_POINTER			:= DbFieldInfo.DBS_BLOB_POINTER	

define DBS_BLOB_DIRECT_TYPE		:= DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
define DBS_BLOB_DIRECT_LEN		:= DbFieldInfo.DBS_BLOB_DIRECT_LEN		

define DBS_STRUCT				:= DbFieldInfo.DBS_STRUCT				
define DBS_PROPERTIES			:= DbFieldInfo.DBS_PROPERTIES			
define DBS_USER					:= DbFieldInfo.DBS_USER					


// Scope defines
define TOPSCOPE                := 0 
define BOTTOMSCOPE             := 1 



//DbInfo Defines
define DBI_ISDBF 			:=  DbInfo.DBI_ISDBF 		 	// Logical: RDD support DBF file format?
define DBI_CANPUTREC 		:=  DbInfo.DBI_CANPUTREC 		// Logical: RDD support Putting Records? 
define DBI_GETHEADERSIZE 	:= 	DbInfo.DBI_GETHEADERSIZE	// Numeric: Get header size of the file 
define DBI_LASTUPDATE 		:= 	DbInfo.DBI_LASTUPDATE 		// Date:    Last date RDD file updated 
define DBI_GETDELIMITER 	:= 	DbInfo.DBI_GETDELIMITER		// String:  Get default delimiter
define DBI_SETDELIMITER 	:=  DbInfo.DBI_SETDELIMITER		// String:  Set default delimiter
define DBI_GETRECSIZE 		:= 	DbInfo.DBI_GETRECSIZE 		// Numeric: Get record size of the file
define DBI_GETLOCKARRAY 	:=  DbInfo.DBI_GETLOCKARRAY		// Array:   Get array of locked records 
define DBI_TABLEEXT 		:=  DbInfo.DBI_TABLEEXT 		 // String:  Get table file extension
define DBI_FULLPATH 		:= 	DbInfo.DBI_FULLPATH 		// String: Full path to data file
define DBI_ISFLOCK 			:= 	DbInfo.DBI_ISFLOCK 			// Logic: Is there a file lock active? 
define DBI_READONLY 		:= 	DbInfo.DBI_READONLY 		// Logic: is the file opened readonly
define DBI_CHILDCOUNT 		:= 	DbInfo.DBI_CHILDCOUNT 		// Number: Number of child relations set
define DBI_FILEHANDLE 		:= 	DbInfo.DBI_FILEHANDLE 		// Stream: The data file's file stream
define DBI_ISANSI 			:= 	DbInfo.DBI_ISANSI 			// Logic: Is the file ansi encoded
define DBI_BOF 				:= 	DbInfo.DBI_BOF 				// Logic: Same as Bof()
define DBI_EOF 				:= 	DbInfo.DBI_EOF 				// Logic: Same as Eof()
define DBI_DBFILTER 		:= 	DbInfo.DBI_DBFILTER 		// String: Current Filter setting 
define DBI_FOUND 			:= 	DbInfo.DBI_FOUND 			// Logic: Same as Found() 
define DBI_FCOUNT 			:= 	DbInfo.DBI_FCOUNT 			// Number: Number of fields per record
define DBI_LOCKCOUNT 		:= 	DbInfo.DBI_LOCKCOUNT 		// Number: Number of record locks  
define DBI_VALIDBUFFER  	:= 	DbInfo.DBI_VALIDBUFFER		// Logic: Is the buffer Valid
define DBI_ALIAS 			:= 	DbInfo.DBI_ALIAS 			// String: Alias
define DBI_GETSCOPE 		:= 	DbInfo.DBI_GETSCOPE 		// Object: The ScopeInfo
define DBI_LOCKOFFSET 		:= 	DbInfo.DBI_LOCKOFFSET 		// Number: Lock offset
define DBI_SHARED 			:= 	DbInfo.DBI_SHARED 			// Logic: is the file opened shared
define DBI_MEMOEXT 			:= 	DbInfo.DBI_MEMOEXT 			// String: Memo file extension
define DBI_MEMOHANDLE 		:= 	DbInfo.DBI_MEMOHANDLE 		// Stream: The memo file's file stream
define DBI_BLOB_HANDLE 		:= 	DbInfo.DBI_BLOB_HANDLE 		// Alias for MemoHandle
define DBI_MEMOBLOCKSIZE 	:= 	DbInfo.DBI_MEMOBLOCKSIZE	// Number: The memo block size
define DBI_CODEPAGE 		:= 	DbInfo.DBI_CODEPAGE 		// Number: The Windows Codepage
define DBI_NEWINDEXLOCK 	:= 	DbInfo.DBI_NEWINDEXLOCK 	// Logic:  Use new index lock mechanism
define DBI_DOSCODEPAGE 		:= 	DbInfo.DBI_DOSCODEPAGE 		// Number: The DOS Codepage

define DBI_STRICTREAD  		:= DbInfo.DBI_STRICTREAD  	// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
define DBI_OPTIMIZE    		:= DbInfo.DBI_OPTIMIZE    	// Flag for whether to use query optimization             
define DBI_AUTOOPEN    		:= DbInfo.DBI_AUTOOPEN    	// Flag for automatically opening structural indexes      
define DBI_AUTOORDER   		:= DbInfo.DBI_AUTOORDER   	// When a structural index is opened, the order to be set 
define DBI_AUTOSHARE   		:= DbInfo.DBI_AUTOSHARE   	// When a network is detected, open the index shared, otherwise open exclusively   

define DBI_DB_VERSION 		:= DbInfo.DBI_DB_VERSION 		
define DBI_RDD_VERSION 		:= DbInfo.DBI_RDD_VERSION 		
define DBI_RDD_LIST 		:= DbInfo.DBI_RDD_LIST 		
define DBI_MEMOFIELD 		:= DbInfo.DBI_MEMOFIELD 		
define DBI_VO_MACRO_SYNTAX	:= DbInfo.DBI_VO_MACRO_SYNTAX	
define DBI_RDD_OBJECT 		:= DbInfo.DBI_RDD_OBJECT 		


/* CA-Cl*pper documented for public use */

define DBI_BLOB_DIRECT_LEN     := DbInfo.BLOB_DIRECT_LEN 
define DBI_BLOB_DIRECT_TYPE    := DbInfo.BLOB_DIRECT_TYPE
define DBI_BLOB_INTEGRITY      := DbInfo.BLOB_FILE_INTEGRITY  
define DBI_BLOB_OFFSET         := DbInfo.BLOB_OFFSET     
define DBI_BLOB_RECOVER        := DbInfo.BLOB_FILE_RECOVER


/* Harbour extension */

define DBI_LOCKSCHEME          := DbInfo.DBI_LOCKSCHEME      /* Locking scheme used by RDD */
define DBI_ISREADONLY          := DbInfo.DBI_ISREADONLY      /* Was the file opened readonly? */
define DBI_ROLLBACK            := DbInfo.DBI_ROLLBACK        /* Rollback changes made to current record */
define DBI_PASSWORD            := DbInfo.DBI_PASSWORD        /* Workarea password */
define DBI_ISENCRYPTED         := DbInfo.DBI_ISENCRYPTED     /* The database is encrypted */
define DBI_MEMOTYPE            := DbInfo.DBI_MEMOTYPE        /* Type of MEMO file: DBT, SMT, FPT */
define DBI_SEPARATOR           := DbInfo.DBI_SEPARATOR       /* The record separator (as a string) */
define DBI_MEMOVERSION         := DbInfo.DBI_MEMOVERSION     /* sub version of memo file */
define DBI_TABLETYPE           := DbInfo.DBI_TABLETYPE       /* Type of table file */
define DBI_SCOPEDRELATION      := DbInfo.DBI_SCOPEDRELATION  /* Is given relation scoped */
define DBI_TRIGGER             := DbInfo.DBI_TRIGGER         /* Get/Set trigger function */
define DBI_OPENINFO            := DbInfo.DBI_OPENINFO        /* DBOPENINFO structure pointer */
define DBI_ENCRYPT             := DbInfo.DBI_ENCRYPT         /* Encrypt table */
define DBI_DECRYPT             := DbInfo.DBI_DECRYPT         /* Decrypt table */
define DBI_MEMOPACK            := DbInfo.DBI_MEMOPACK        /* Pack memo file */
define DBI_DIRTYREAD           := DbInfo.DBI_DIRTYREAD       /* Get/Set index dirty read flag */
define DBI_POSITIONED          := DbInfo.DBI_POSITIONED      /* Is cursor positioned to valid record */
define DBI_ISTEMPORARY         := DbInfo.DBI_ISTEMPORARY     /* Is the table a temporary one? */
define DBI_LOCKTEST            := DbInfo.DBI_LOCKTEST        /* record / file lock test */
define DBI_CODEPAGE_HB         := DbInfo.DBI_CODEPAGE_HB     /* Codepage used also defined by VO and Vulcan */ 
define DBI_TRANSREC            := DbInfo.DBI_TRANSREC         /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
define DBI_SETHEADER		   := DbInfo.DBI_SETHEADER		/* DBF header updating modes */ 
define DBI_QUERY			   := DbInfo.DBI_QUERY			/* if area represents result of a query, obtain expression of this query */ 

/* Harbour RECORD MAP (RM) support */

define DBI_RM_SUPPORTED        := DbInfo.DBI_RM_SUPPORTED    /* has WA RDD record map support? */
define DBI_RM_CREATE           := DbInfo.DBI_RM_CREATE     /* create new empty work area record map */
define DBI_RM_REMOVE           := DbInfo.DBI_RM_REMOVE     /* remove active work area record map */
define DBI_RM_CLEAR            := DbInfo.DBI_RM_CLEAR      /* remove all records from WA record map */
define DBI_RM_FILL             := DbInfo.DBI_RM_FILL       /* add all records to WA record map */
define DBI_RM_ADD              := DbInfo.DBI_RM_ADD        /* add record to work area record map */
define DBI_RM_DROP             := DbInfo.DBI_RM_DROP       /* remove record from work area record map */
define DBI_RM_TEST             := DbInfo.DBI_RM_TEST       /* test if record is set in WA record map */
define DBI_RM_COUNT            := DbInfo.DBI_RM_COUNT      /* number of records set in record map */
define DBI_RM_HANDLE           := DbInfo.DBI_RM_HANDLE     /* get/set record map filter handle */ 



// CDX and Comix Record List Support

define DBI_RL_AND 		:= DbInfo.DBI_RL_AND 		
define DBI_RL_CLEAR 	:= DbInfo.DBI_RL_CLEAR 	
define DBI_RL_COUNT 	:= DbInfo.DBI_RL_COUNT 	
define DBI_RL_DESTROY 	:= DbInfo.DBI_RL_DESTROY 	
define DBI_RL_EXFILTER 	:= DbInfo.DBI_RL_EXFILTER 	
define DBI_RL_GETFILTER := DbInfo.DBI_RL_GETFILTER 
define DBI_RL_HASMAYBE 	:= DbInfo.DBI_RL_HASMAYBE 	
define DBI_RL_LEN 		:= DbInfo.DBI_RL_LEN 		
define DBI_RL_MAYBEEVAL := DbInfo.DBI_RL_MAYBEEVAL 
define DBI_RL_NEW 		:= DbInfo.DBI_RL_NEW 		
define DBI_RL_NEWDUP 	:= DbInfo.DBI_RL_NEWDUP 	
define DBI_RL_NEWQUERY 	:= DbInfo.DBI_RL_NEWQUERY 	
define DBI_RL_NEXTRECNO := DbInfo.DBI_RL_NEXTRECNO 
define DBI_RL_NOT 		:= DbInfo.DBI_RL_NOT 		
define DBI_RL_OR 		:= DbInfo.DBI_RL_OR 		
define DBI_RL_PREVRECNO := DbInfo.DBI_RL_PREVRECNO 
define DBI_RL_SET 		:= DbInfo.DBI_RL_SET 		
define DBI_RL_SETFILTER := DbInfo.DBI_RL_SETFILTER 
define DBI_RL_TEST 		:= DbInfo.DBI_RL_TEST 		


define DBI_USER 				:= DbInfo.DBI_USER	// Start of user definable DBI_ values

// Advantage additions
define DBI_GET_ACE_TABLE_HANDLE  := DbInfo.DBI_GET_ACE_TABLE_HANDLE
define DBI_GET_ACE_STMT_HANDLE   := DbInfo.DBI_GET_ACE_STMT_HANDLE


// OrderInfo Defines
define DBOI_CONDITION 	:= DBOrderInfo.DBOI_CONDITION      // String: The order's conditional expression     
define DBOI_EXPRESSION 	:= DBOrderInfo.DBOI_EXPRESSION 	// String: The order's key expression             
define DBOI_POSITION 	:= DBOrderInfo.DBOI_POSITION  	// Number: The current key position in scope and filter  
define DBOI_RECNO 		:= DBOrderInfo.DBOI_RECNO 		  	// Number: The current key position disregarding filters 
define DBOI_NAME 		:= DBOrderInfo.DBOI_NAME 		   	// String: The name of the order                      
define DBOI_NUMBER 		:= DBOrderInfo.DBOI_NUMBER 		 	// Number: The numeric position in the list of orders
define DBOI_BAGNAME 	:= DBOrderInfo.DBOI_BAGNAME 	 	// String: The name of the file containing this order
define DBOI_INDEXNAME 	:= DBOrderInfo.DBOI_INDEXNAME   // Alias
define DBOI_BAGEXT 		:= DBOrderInfo.DBOI_BAGEXT 		   	// String: The extension of the file containing this order
define DBOI_INDEXEXT  	:= DBOrderInfo.DBOI_INDEXEXT  	   // Alias
define DBOI_ORDERCOUNT  := DBOrderInfo.DBOI_ORDERCOUNT     // Number: The count of ORDERS contained in an index file or in total
define DBOI_FILEHANDLE 	:= DBOrderInfo.DBOI_FILEHANDLE 		// Stream: The stream of the index
define DBOI_ISCOND 		:= DBOrderInfo.DBOI_ISCOND 			// Logic : Does the order have a FOR condition?
define DBOI_ISDESC 		:= DBOrderInfo.DBOI_ISDESC 			// Logic : Is the order DESCENDing? 
define DBOI_UNIQUE 		:= DBOrderInfo.DBOI_UNIQUE 			// Logic : Does the order have the UNIQUE attribute?


/* Clipper 5.3-level constants */
define DBOI_FULLPATH 	:= DBOrderInfo.DBOI_FULLPATH 	   	// String: The full path to the index file (Bag)
define DBOI_KEYTYPE 	:= DBOrderInfo.DBOI_KEYTYPE 	 	// The type of the order's key  
define DBOI_KEYSIZE 	:= DBOrderInfo.DBOI_KEYSIZE 	 	// Number: The length of the order's key
define DBOI_KEYCOUNT 	:= DBOrderInfo.DBOI_KEYCOUNT 	 	// Number: The count of keys in scope and filter
define DBOI_SETCODEBLOCK:= DBOrderInfo.DBOI_SETCODEBLOCK 	// Block : The codeblock that produces the key 
define DBOI_KEYDEC 		:= DBOrderInfo.DBOI_KEYDEC 		 	// Number: The # of decimals in a numeric key 
define DBOI_HPLOCKING 	:= DBOrderInfo.DBOI_HPLOCKING 	 	// Logic : Using High Performance locking for this order?
define DBOI_LOCKOFFSET 	:= DBOrderInfo.DBOI_LOCKOFFSET  	// Number: The offset used for logical locking 

define DBOI_KEYADD 		:= DbOrderInfo.DBOI_KEYADD 		 	// Logic: Custom Index: Was Key added successfully? 
define DBOI_KEYDELETE 	:= DbOrderInfo.DBOI_KEYDELETE 		// Logic: Custom Index: Was Key Deletion successful? 
define DBOI_KEYVAL 		:= DbOrderInfo.DBOI_KEYVAL 			// Object: The value of the current key 
define DBOI_SCOPETOP 	:= DbOrderInfo.DBOI_SCOPETOP 		// Object: Get or Set the scope top    
define DBOI_SCOPEBOTTOM := DbOrderInfo.DBOI_SCOPEBOTTOM	// Object: Get or Set the scope bottom
define DBOI_SCOPETOPCLEAR := DbOrderInfo.DBOI_SCOPETOPCLEAR  	// None	 :
define DBOI_SCOPEBOTTOMCLEAR:= DbOrderInfo.DBOI_SCOPEBOTTOMCLEAR // None :
define DBOI_CUSTOM 		:= DbOrderInfo.DBOI_CUSTOM // Logic: Is this a Custom Index?  
define DBOI_SKIPUNIQUE 	:= DbOrderInfo.DBOI_SKIPUNIQUE // Logic: Was a skip to adjacent unique Key successful?  

define DBOI_KEYSINCLUDED:= DbOrderInfo.DBOI_KEYSINCLUDED 	// Number: Number of keys in the index order
define DBOI_KEYNORAW 	:= DbOrderInfo.DBOI_KEYNORAW 	 // Number: The key number disregarding filters
define DBOI_KEYCOUNTRAW := DbOrderInfo.DBOI_KEYCOUNTRAW   // Number: The key count disregarding filter  
define DBOI_OPTLEVEL 	:= DbOrderInfo.DBOI_OPTLEVEL 	 // Number: Optimization level for current query


define DBOI_STRICTREAD := DbOrderInfo.DBOI_STRICTREAD  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
define DBOI_OPTIMIZE   := DbOrderInfo.DBOI_OPTIMIZE    /* Flag for whether to use query optimization             */
define DBOI_AUTOOPEN   := DbOrderInfo.DBOI_AUTOOPEN    /* Flag for automatically opening structural indexes      */
define DBOI_AUTOORDER  := DbOrderInfo.DBOI_AUTOORDER   /* When a structural index is opened, the order to be set */
define DBOI_AUTOSHARE  := DbOrderInfo.DBOI_AUTOSHARE   /* When a network is detected, open the index shared, otherwise open exclusively   */ 


/* Harbour extensions */

define DBOI_SKIPEVAL           := DbOrderInfo.DBOI_SKIPEVAL           /* skip while code block doesn't return TRUE */
define DBOI_SKIPEVALBACK       := DbOrderInfo.DBOI_SKIPEVALBACK       /* skip backward while code block doesn't return TRUE */
define DBOI_SKIPREGEX          := DbOrderInfo.DBOI_SKIPREGEX          /* skip while regular expression on index key doesn't return TRUE */
define DBOI_SKIPREGEXBACK      := DbOrderInfo.DBOI_SKIPREGEXBACK      /* skip backward while regular expression on index key doesn't return TRUE */
define DBOI_SKIPWILD           := DbOrderInfo.DBOI_SKIPWILD           /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
define DBOI_SKIPWILDBACK       := DbOrderInfo.DBOI_SKIPWILDBACK       /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
define DBOI_SCOPEEVAL          := DbOrderInfo.DBOI_SCOPEEVAL          /* skip through index evaluating given C function */
define DBOI_FINDREC            := DbOrderInfo.DBOI_FINDREC            /* find given record in a Tag beginning from TOP */
define DBOI_FINDRECCONT        := DbOrderInfo.DBOI_FINDRECCONT        /* find given record in a Tag beginning from current position */
define DBOI_SCOPESET           := DbOrderInfo.DBOI_SCOPESET           /* set both scopes */
define DBOI_SCOPECLEAR         := DbOrderInfo.DBOI_SCOPECLEAR         /* clear both scopes */


define DBOI_BAGCOUNT           := DbOrderInfo.DBOI_BAGCOUNT           /* number of open order bags */
define DBOI_BAGNUMBER          := DbOrderInfo.DBOI_BAGNUMBER          /* bag position in bag list */
define DBOI_BAGORDER           := DbOrderInfo.DBOI_BAGORDER           /* number of first order in a bag */
define DBOI_ISMULTITAG         := DbOrderInfo.DBOI_ISMULTITAG         /* does RDD support multi tag in index file */
define DBOI_ISSORTRECNO        := DbOrderInfo.DBOI_ISSORTRECNO        /* is record number part of key in sorting */
define DBOI_LARGEFILE          := DbOrderInfo.DBOI_LARGEFILE          /* is large file size (>=4GB) supported */
define DBOI_TEMPLATE           := DbOrderInfo.DBOI_TEMPLATE           /* order with free user keys */
define DBOI_MULTIKEY           := DbOrderInfo.DBOI_MULTIKEY           /* custom order with multikeys */
define DBOI_CHGONLY            := DbOrderInfo.DBOI_CHGONLY            /* update only existing keys */
define DBOI_PARTIAL            := DbOrderInfo.DBOI_PARTIAL            /* is index partially updated */
define DBOI_SHARED             := DbOrderInfo.DBOI_SHARED             /* is index open in shared mode */
define DBOI_ISREADONLY         := DbOrderInfo.DBOI_ISREADONLY         /* is index open in readonly mode */
define DBOI_READLOCK           := DbOrderInfo.DBOI_READLOCK           /* get/set index read lock */
define DBOI_WRITELOCK          := DbOrderInfo.DBOI_WRITELOCK          /* get/set index write lock */
define DBOI_UPDATECOUNTER      := DbOrderInfo.DBOI_UPDATECOUNTER      /* get/set update index counter */
define DBOI_EVALSTEP           := DbOrderInfo.DBOI_EVALSTEP           /* eval step (EVERY) used in index command */
define DBOI_ISREINDEX          := DbOrderInfo.DBOI_ISREINDEX          /* Is reindex in process */
define DBOI_I_BAGNAME          := DbOrderInfo.DBOI_I_BAGNAME          /* created index name */
define DBOI_I_TAGNAME          := DbOrderInfo.DBOI_I_TAGNAME          /* created tag name */
define DBOI_RELKEYPOS          := DbOrderInfo.DBOI_RELKEYPOS          /* get/set relative key position (in range 0 - 1) */
define DBOI_USECURRENT         := DbOrderInfo.DBOI_USECURRENT         /* get/set "use current index" flag */
define DBOI_INDEXTYPE          := DbOrderInfo.DBOI_INDEXTYPE          /* current index type */
define DBOI_RESETPOS           := DbOrderInfo.DBOI_RESETPOS           /* rest logical and raw positions */
define DBOI_INDEXPAGESIZE      := DbOrderInfo.DBOI_INDEXPAGESIZE      /* get index page size */

define DBOI_USER 				:= DbOrderInfo.DBOI_USER

// Advantage extensions

define DBOI_AXS_PERCENT_INDEXED  := DbOrderInfo.DBOI_AXS_PERCENT_INDEXED
define DBOI_GET_ACE_INDEX_HANDLE := DbOrderInfo.DBOI_GET_ACE_INDEX_HANDLE


// Duplicates
define DBOI_KEYGOTO 	:= DbOrderInfo.DBOI_POSITION
define DBOI_KEYGOTORAW 	:= DbOrderInfo.DBOI_KEYNORAW
define DBOI_KEYNO	 	:= DbOrderInfo.DBOI_POSITION

// Blob defines



define BLOB_INFO_HANDLE 	:= DbInfo.BLOB_INFO_HANDLE 	
define BLOB_FILE_RECOVER 	:= DbInfo.BLOB_FILE_RECOVER 	
define BLOB_FILE_INTEGRITY 	:= DbInfo.BLOB_FILE_INTEGRITY 	
define BLOB_OFFSET 			:= DbInfo.BLOB_OFFSET 			
define BLOB_POINTER 		:= DbInfo.BLOB_POINTER 		
define BLOB_LEN 			:= DbInfo.BLOB_LEN 			
define BLOB_TYPE 			:= DbInfo.BLOB_TYPE 			
define BLOB_EXPORT 			:= DbInfo.BLOB_EXPORT 			
define BLOB_ROOT_UNLOCK 	:= DbInfo.BLOB_ROOT_UNLOCK 	
define BLOB_ROOT_PUT 		:= DbInfo.BLOB_ROOT_PUT 		
define BLOB_ROOT_GET 		:= DbInfo.BLOB_ROOT_GET 		
define BLOB_ROOT_LOCK 		:= DbInfo.BLOB_ROOT_LOCK 		
define BLOB_IMPORT 			:= DbInfo.BLOB_IMPORT 			
define BLOB_DIRECT_PUT 		:= DbInfo.BLOB_DIRECT_PUT 		
define BLOB_DIRECT_GET 		:= DbInfo.BLOB_DIRECT_GET 		
define BLOB_GET 			:= DbInfo.BLOB_GET 			
define BLOB_DIRECT_EXPORT 	:= DbInfo.BLOB_DIRECT_EXPORT 	
define BLOB_DIRECT_IMPORT 	:= DbInfo.BLOB_DIRECT_IMPORT 	
define BLOB_NMODE 			:= DbInfo.BLOB_NMODE 			
define BLOB_USER			:= DbInfo.BLOB_USER

define BLOB_EXPORT_APPEND 	:= 0 
define BLOB_EXPORT_OVERWRITE:= 1 

define BLOB_IMPORT_COMPRESS := 1 
define BLOB_IMPORT_ENCRYPT	:= 2 


/* return values for DBOI_OPTLEVEL */

define DBOI_OPTIMIZED_NONE       := 0 
define DBOI_OPTIMIZED_PART       := 1 
define DBOI_OPTIMIZED_FULL       := 2 

/* return values for DBOI_INDEXTYPE */

define DBOI_TYPE_UNDEF          := -1 
define DBOI_TYPE_NONE           :=  0 
define DBOI_TYPE_NONCOMPACT     :=  1 
define DBOI_TYPE_COMPACT        :=  2 
define DBOI_TYPE_COMPOUND       :=  3 

/* constants for DBOI_SCOPEEVAL array parameter */

define DBRMI_FUNCTION           := 1 
define DBRMI_PARAM              := 2 
define DBRMI_LOVAL              := 3 
define DBRMI_HIVAL              := 4 
define DBRMI_RESULT             := 5 
define DBRMI_SIZE               := 5 

/* Numeric DBF TYPES */
define DB_DBF_STD              := 1 
define DB_DBF_VFP              := 2 


/* Numeric MEMO TYPES */
define DB_MEMO_NONE            := 0 
define DB_MEMO_DBT             := 1 
define DB_MEMO_FPT             := 2 
define DB_MEMO_SMT             := 3 

/* MEMO EXTENDED TYPES */
define DB_MEMOVER_STD          := 1 
define DB_MEMOVER_SIX          := 2 
define DB_MEMOVER_FLEX         := 3 
define DB_MEMOVER_CLIP         := 4 


/* LOCK SCHEMES */
define DB_DBFLOCK_DEFAULT      := 0 
define DB_DBFLOCK_CLIPPER      := 1   /* default Cl*pper locking scheme */
define DB_DBFLOCK_COMIX        := 2   /* COMIX and CL53 DBFCDX hyper locking scheme */
define DB_DBFLOCK_VFP          := 3   /* [V]FP, CL52 DBFCDX, SIx3 SIXCDX, CDXLOCK.OBJ */
define DB_DBFLOCK_HB32         := 4   /* Harbour hyper locking scheme for 32bit file API */
define DB_DBFLOCK_HB64         := 5   /* Harbour hyper locking scheme for 64bit file API */
define DB_DBFLOCK_CLIPPER2     := 6   /* extended Cl*pper locking scheme NTXLOCK2.OBJ */ 


// File Extensions
define DBT_MEMOEXT             := ".dbt" 
define FPT_MEMOEXT             := ".fpt" 
define SMT_MEMOEXT             := ".smt" 
define DBV_MEMOEXT             := ".dbv" 

// Blocks
define DBT_DEFBLOCKSIZE        := 512
define FPT_DEFBLOCKSIZE        := 64 
define SMT_DEFBLOCKSIZE        := 32 

// Locks
define FPT_LOCKPOS             := 0		
define FPT_LOCKSIZE            := 1		
define FPT_ROOTBLOCK_OFFSET    := 536	 /* Clipper 5.3 ROOT data block offset */

define SIX_ITEM_BUFSIZE        :=    14 
define FLEX_ITEM_BUFSIZE       :=     8 
define MAX_SIXFREEBLOCKS       :=    82 
define MAX_FLEXFREEBLOCKS      :=   126 
define FLEXGCPAGE_SIZE         :=  1010 


// RDD Inheritance
define RDT_FULL            :=  1	
define RDT_TRANSFER        :=  2	
define RDT_HIDDEN          :=  8	


define RDDI_ISDBF             := RDDInfo.RDDI_ISDBF        /* Does this RDD support DBFs? */
define RDDI_CANPUTREC         := RDDInfo.RDDI_CANPUTREC    /* Can this RDD Put Records? */
define RDDI_DELIMITER         := RDDInfo.RDDI_DELIMITER    /* The field delimiter (as a string) */
define RDDI_SEPARATOR         := RDDInfo.RDDI_SEPARATOR    /* The record separator (as a string) */
							  
define RDDI_TABLEEXT          := RDDInfo.RDDI_TABLEEXT     /* Default data file's file extension */
define RDDI_MEMOEXT           := RDDInfo.RDDI_MEMOEXT      /* Default memo file's file extension */
define RDDI_ORDBAGEXT         := RDDInfo.RDDI_ORDBAGEXT    /* Default multi tag index's file extension */
define RDDI_ORDEREXT          := RDDInfo.RDDI_ORDEREXT     /* default single tag index's file extension */
define RDDI_ORDSTRUCTEXT      := RDDInfo.RDDI_ORDSTRUCTEXT /* default single tag index's file extension */
							  
define RDDI_LOCAL             := RDDInfo.RDDI_LOCAL        /* Local file access? */
define RDDI_REMOTE            := RDDInfo.RDDI_REMOTE       /* Remote table access? */
define RDDI_CONNECTION        := RDDInfo.RDDI_CONNECTION   /* Get/Set default connection */
define RDDI_TABLETYPE         := RDDInfo.RDDI_TABLETYPE    /* Type of table file */
define RDDI_MEMOTYPE          := RDDInfo.RDDI_MEMOTYPE     /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
define RDDI_LARGEFILE         := RDDInfo.RDDI_LARGEFILE    /* Is large file size (>=4GB) supported */
define RDDI_LOCKSCHEME        := RDDInfo.RDDI_LOCKSCHEME   /* Locking scheme used by RDD */
define RDDI_RECORDMAP         := RDDInfo.RDDI_RECORDMAP    /* Does RDD support record map functionality? */
define RDDI_ENCRYPTION        := RDDInfo.RDDI_ENCRYPTION   /* Does RDD support encryption */
define RDDI_TRIGGER           := RDDInfo.RDDI_TRIGGER      /* Get/Set default trigger function */
define RDDI_AUTOLOCK          := RDDInfo.RDDI_AUTOLOCK     /* automatic locking on update */

/* index parameters */
define RDDI_STRUCTORD           := RDDInfo.RDDI_STRUCTORD    /* Are structural indexes supported */
define RDDI_STRICTREAD          := RDDInfo.RDDI_STRICTREAD   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
define RDDI_STRICTSTRUCT        := RDDInfo.RDDI_STRICTSTRUCT /* Flag for strict structural order checking */
define RDDI_OPTIMIZE            := RDDInfo.RDDI_OPTIMIZE     /* Flag for whether to use query optimization */
define RDDI_FORCEOPT            := RDDInfo.RDDI_FORCEOPT     /* Flag for forcing linear optimization */
define RDDI_AUTOOPEN            := RDDInfo.RDDI_AUTOOPEN     /* Flag for automatically opening structural indexes */
define RDDI_AUTOORDER           := RDDInfo.RDDI_AUTOORDER    /* When a structural index is opened, the order to be set */
define RDDI_AUTOSHARE           := RDDInfo.RDDI_AUTOSHARE    /* When a network is detected, open the index shared, otherwise open exclusively */
define RDDI_MULTITAG            := RDDInfo.RDDI_MULTITAG     /* Does RDD support multi tag in index file */
define RDDI_SORTRECNO           := RDDInfo.RDDI_SORTRECNO    /* Is record number part of key in sorting */
define RDDI_MULTIKEY            := RDDInfo.RDDI_MULTIKEY     /* Does custom orders support repeated keys? */

/* memo parameters */
define RDDI_MEMOBLOCKSIZE       := RDDInfo.RDDI_MEMOBLOCKSIZE   /* Memo File's block size */
define RDDI_MEMOVERSION         := RDDInfo.RDDI_MEMOVERSION     /* sub version of memo file */
define RDDI_MEMOGCTYPE          := RDDInfo.RDDI_MEMOGCTYPE      /* type of garbage collector used by GC */
define RDDI_MEMOREADLOCK        := RDDInfo.RDDI_MEMOREADLOCK    /* use read lock in memo file access */
define RDDI_MEMOREUSE           := RDDInfo.RDDI_MEMOREUSE       /* reuse free space on write */
define RDDI_BLOB_SUPPORT        := RDDInfo.RDDI_BLOB_SUPPORT    /* can support BLOB files directly */

/* misc */
define RDDI_PENDINGTRIGGER      := RDDInfo.RDDI_PENDINGTRIGGER    /* set pending trigger for next open operation */
define RDDI_PENDINGPASSWORD     := RDDInfo.RDDI_PENDINGPASSWORD   /* set pending password for next open operation */
define RDDI_PASSWORD            := RDDInfo.RDDI_PASSWORD          /* Get/Set default password */
define RDDI_LOCKRETRY           := RDDInfo.RDDI_LOCKRETRY         /* Get/Set record and file lock timeout value */
define RDDI_DIRTYREAD           := RDDInfo.RDDI_DIRTYREAD         /* Get/Set index dirty read flag */
define RDDI_INDEXPAGESIZE       := RDDInfo.RDDI_INDEXPAGESIZE     /* Get/Set default index page size */
define RDDI_DECIMALS            := RDDInfo.RDDI_DECIMALS          /* Get/Set default number of decimal places for numeric fields if it's undefined */
define RDDI_SETHEADER           := RDDInfo.RDDI_SETHEADER         /* DBF header updating modes */

/* SQL */
define RDDI_CONNECT             := RDDInfo.RDDI_CONNECT         /* connect to database */
define RDDI_DISCONNECT          := RDDInfo.RDDI_DISCONNECT      /* disconnect from database */
define RDDI_EXECUTE             := RDDInfo.RDDI_EXECUTE         /* execute SQL statement */
define RDDI_ERROR               := RDDInfo.RDDI_ERROR           /* error number */
define RDDI_ERRORNO             := RDDInfo.RDDI_ERRORNO         /* error description */
define RDDI_INSERTID            := RDDInfo.RDDI_INSERTID        /* last auto insert ID */
define RDDI_AFFECTEDROWS        := RDDInfo.RDDI_AFFECTEDROWS    /* number of affected rows after UPDATE */
define RDDI_QUERY               := RDDInfo.RDDI_QUERY           /* last executed query */ 

/*
/* "V" field types * /
define HB_VF_CHAR            64000

define HB_VF_DATE            64001

define HB_VF_INT             64002

define HB_VF_LOG             64003

define HB_VF_DNUM            64004

define HB_VF_ARRAY           64005

define HB_VF_BLOB            64006

define HB_VF_BLOBCOMPRESS    64007

define HB_VF_BLOBENCRYPT     64008



/* SMT types * /

define SMT_IT_NIL            0

define SMT_IT_CHAR           1

define SMT_IT_INT            2

define SMT_IT_DOUBLE         3

define SMT_IT_DATE           4

define SMT_IT_LOGICAL        5

define SMT_IT_ARRAY          6


define FPTIT_DUMMY        0xDEADBEAF

define FPTIT_BINARY       0x0000

define FPTIT_PICT         0x0000      

/* Picture * /

define FPTIT_TEXT         0x0001      /* Text    * /

define FPTIT_OBJ          0x0002      /* Object  * /

define FPTIT_SIX_NIL      0x0000      /* NIL VALUE (USED ONLY IN ARRAYS) * /
define FPTIT_SIX_LNUM     0x0002      /* LONG LE * /
define FPTIT_SIX_DNUM     0x0008      /* DOUBLE LE * /

define FPTIT_SIX_LDATE    0x0020      /* DATE (LONG LE) * /
define FPTIT_SIX_LOG      0x0080      /* LOGIC * /

define FPTIT_SIX_CHAR     0x0400      /* CHAR * /

define FPTIT_SIX_ARRAY    0x8000      /* ARRAY * / 
*/



// RDD Errors
define EDB                 := Subcodes.EDB
define EDB_SEEK            := SubCodes.EDB_SEEK          
define EDB_SKIP            := SubCodes.EDB_SKIP          
define EDB_GOTO            := SubCodes.EDB_GOTO          
define EDB_SETRELATION     := SubCodes.EDB_SETRELATION   
define EDB_USE             := SubCodes.EDB_USE           
define EDB_CREATEINDEX     := SubCodes.EDB_CREATEINDEX   
define EDB_SETORDER        := SubCodes.EDB_SETORDER      
define EDB_SETINDEX        := SubCodes.EDB_SETINDEX      
define EDB_FIELDNAME       := SubCodes.EDB_FIELDNAME     
define EDB_BADALIAS        := SubCodes.EDB_BADALIAS      
define EDB_DUPALIAS        := SubCodes.EDB_DUPALIAS      
define EDB_SETFILTER       := SubCodes.EDB_SETFILTER     
define EDB_CYCLICREL       := SubCodes.EDB_CYCLICREL     
define EDB_CREATETABLE     := SubCodes.EDB_CREATETABLE   
define EDB_RDDNOTFOUND     := SubCodes.EDB_RDDNOTFOUND   
// RESERVED EDB + 16
define EDB_FIELDINDEX      := SubCodes.EDB_FIELDINDEX   
define EDB_SELECT          := SubCodes.EDB_SELECT       
define EDB_SYMSELECT       := SubCodes.EDB_SYMSELECT    
define EDB_TOTAL           := SubCodes.EDB_TOTAL        
define EDB_RECNO           := SubCodes.EDB_RECNO        
define EDB_EXPRESSION      := SubCodes.EDB_EXPRESSION   
define EDB_EXPR_WIDTH      := SubCodes.EDB_EXPR_WIDTH   
// RESERVED EDB + 24 - 29
define EDB_DRIVERLOAD      := SubCodes.EDB_DRIVERLOAD      
define EDB_PARAM           := SubCodes.EDB_PARAM           
define EDB_NOAREAS         := SubCodes.EDB_NOAREAS         
define EDB_NOMEM           := SubCodes.EDB_NOMEM           
define EDB_NOFIELDS        := SubCodes.EDB_NOFIELDS        
define EDB_BAD_ERROR_INFO  := SubCodes.EDB_BAD_ERROR_INFO  
define EDB_WRONGFIELDNAME  := SubCodes.EDB_WRONGFIELDNAME  
define EDB_ORDDESTROY      := SubCodes.EDB_ORDDESTROY      
define EDB_NOINITFUNCTION  := SubCodes.EDB_NOINITFUNCTION  
define EDB_ERRORINIT       := SubCodes.EDB_ERRORINIT       
define EDB_DBSTRUCT        := SubCodes.EDB_DBSTRUCT        

define EDB_NOTABLE         := SubCodes.EDB_NOTABLE       
define EDB_NOORDER         := SubCodes.EDB_NOORDER       

define EDB_ASSERTION       := SubCodes.EDB_ASSERTION     


