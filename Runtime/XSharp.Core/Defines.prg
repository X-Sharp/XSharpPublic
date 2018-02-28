//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// RecInfo defines
DEFINE DBRI_DELETED 	:= 1 AS SHORT
DEFINE DBRI_LOCKED 		:= 2 AS SHORT
DEFINE DBRI_RECSIZE		:= 3 AS SHORT
DEFINE DBRI_RECNO		:= 4 AS SHORT
DEFINE DBRI_UPDATED		:= 5 AS SHORT
DEFINE DBRI_BUFFPTR 	:= 6 AS SHORT
DEFINE DBRI_RAWRECORD   := DBRI_BUFFPTR AS SHORT
DEFINE DBRI_ENCRYPTED	:= 7 AS SHORT
DEFINE DBRI_RAWMEMOS	:= 8 AS SHORT
DEFINE DBRI_RAWDATA		:= 9 AS SHORT
DEFINE DBRI_USER		:= 1000 AS SHORT



// FieldInfo defines
DEFINE DBS_NAME					:= 1 AS SHORT
DEFINE DBS_TYPE					:= 2 AS SHORT
DEFINE DBS_LEN					:= 3 AS SHORT
DEFINE DBS_DEC					:= 4 AS SHORT
DEFINE DBS_ALIAS				:= 5 AS SHORT  

DEFINE DBS_ISNULL               := 11 AS SHORT
DEFINE DBS_COUNTER              := 12 AS SHORT
DEFINE DBS_STEP                 := 13 AS SHORT

DEFINE DBS_BLOB_GET             := 101 AS SHORT /* This is internal definition */ 
DEFINE DBS_BLOB_TYPE			:= 102 AS SHORT
DEFINE DBS_BLOB_LEN				:= 103 AS SHORT
DEFINE DBS_BLOB_OFFSET			:= 104 AS SHORT
DEFINE DBS_BLOB_POINTER			:= 198 AS SHORT

DEFINE DBS_BLOB_DIRECT_TYPE		:= 222 AS SHORT
DEFINE DBS_BLOB_DIRECT_LEN		:= 223 AS SHORT

DEFINE DBS_STRUCT				:= 998 AS SHORT
DEFINE DBS_PROPERTIES			:= 999 AS SHORT
DEFINE DBS_USER					:= 1000 AS SHORT


// Scope defines
DEFINE TOPSCOPE                := 0 AS SHORT
DEFINE BOTTOMSCOPE             := 1 AS SHORT


//DbInfo Defines
DEFINE DBI_ISDBF 			:= 1 AS SHORT     	// Logical: RDD support DBF file format?
DEFINE DBI_CANPUTREC 		:= 2 AS SHORT       // Logical: RDD support Putting Records? 
DEFINE DBI_GETHEADERSIZE 	:= 3 AS SHORT		// Numeric: Get header size of the file 
DEFINE DBI_LASTUPDATE 		:= 4 AS SHORT		// Date:    Last date RDD file updated 
DEFINE DBI_GETDELIMITER 	:= 5 AS SHORT		// String:  Get default delimiter
DEFINE DBI_SETDELIMITER 	:= 6 AS SHORT       // String:  Set default delimiter
DEFINE DBI_GETRECSIZE 		:= 7 AS SHORT		// Numeric: Get record size of the file
DEFINE DBI_GETLOCKARRAY 	:= 8 AS SHORT      	// Array:   Get array of locked records 
DEFINE DBI_TABLEEXT 		:= 9 AS SHORT        // String:  Get table file extension
DEFINE DBI_FULLPATH 		:= 10 AS SHORT		// String: Full path to data file
DEFINE DBI_ISFLOCK 			:= 20 AS SHORT		// Logic: Is there a file lock active? 
DEFINE DBI_READONLY 		:= 21 AS SHORT		// Logic: is the file opened readonly
DEFINE DBI_CHILDCOUNT 		:= 22 AS SHORT		// Number: Number of child relations set
DEFINE DBI_FILEHANDLE 		:= 23 AS SHORT		// Stream: The data file's file stream
DEFINE DBI_ISANSI 			:= 25 AS SHORT		// Logic: Is the file ansi encoded
DEFINE DBI_BOF 				:= 26 AS SHORT  	// Logic: Same as Bof()
DEFINE DBI_EOF 				:= 27 AS SHORT		// Logic: Same as Eof()
DEFINE DBI_DBFILTER 		:= 28 AS SHORT 		// String: Current Filter setting 
DEFINE DBI_FOUND 			:= 29 AS SHORT		// Logic: Same as Found() 
DEFINE DBI_FCOUNT 			:= 30 AS SHORT		// Number: Number of fields per record
DEFINE DBI_LOCKCOUNT 		:= 31 AS SHORT		// Number: Number of record locks  
DEFINE DBI_VALIDBUFFER  	:= 32 AS SHORT		// Logic: Is the buffer Valid
DEFINE DBI_ALIAS 			:= 33 AS SHORT		// String: Alias
DEFINE DBI_GETSCOPE 		:= 34 AS SHORT		// Object: The ScopeInfo
DEFINE DBI_LOCKOFFSET 		:= 35 AS SHORT		// Number: Lock offset
DEFINE DBI_SHARED 			:= 36 AS SHORT		// Logic: is the file opened shared
DEFINE DBI_MEMOEXT 			:= 37 AS SHORT		// String: Memo file extension
DEFINE DBI_MEMOHANDLE 		:= 38 AS SHORT		// Stream: The memo file's file stream
DEFINE DBI_BLOB_HANDLE 		:= 38 AS SHORT		// Alias for MemoHandle
DEFINE DBI_MEMOBLOCKSIZE 	:= 39 AS SHORT		// Number: The memo block size
DEFINE DBI_CODEPAGE 		:= 41 AS SHORT		// Number: The Windows Codepage
DEFINE DBI_NEWINDEXLOCK 	:= 42 AS SHORT		// Logic:  Use new index lock mechanism
DEFINE DBI_DOSCODEPAGE 		:= 43 AS SHORT		// Number: The DOS Codepage

DEFINE DBI_STRICTREAD  		:= 60 AS SHORT 	// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
DEFINE DBI_OPTIMIZE    		:= 61 AS SHORT 	// Flag for whether to use query optimization             
DEFINE DBI_AUTOOPEN    		:= 62 AS SHORT 	// Flag for automatically opening structural indexes      
DEFINE DBI_AUTOORDER   		:= 63 AS SHORT 	// When a structural index is opened, the order to be set 
DEFINE DBI_AUTOSHARE   		:= 64 AS SHORT 	// When a network is detected, open the index shared, otherwise open exclusively   

DEFINE DBI_DB_VERSION 	:= 101 AS SHORT
DEFINE DBI_RDD_VERSION 	:= 102 AS SHORT
DEFINE DBI_RDD_LIST 	:= 103 AS SHORT
DEFINE DBI_MEMOFIELD 	:= 104 AS SHORT
DEFINE DBI_VO_MACRO_SYNTAX := 105 AS SHORT
DEFINE DBI_RDD_OBJECT 	:= 106 AS SHORT


/* CA-Cl*pper documented for public use */

DEFINE DBI_BLOB_DIRECT_LEN     := 209 AS SHORT
DEFINE DBI_BLOB_DIRECT_TYPE    := 210 AS SHORT
DEFINE DBI_BLOB_INTEGRITY      := 211 AS SHORT
DEFINE DBI_BLOB_OFFSET         := 212 AS SHORT
DEFINE DBI_BLOB_RECOVER        := 213 AS SHORT


/* Harbour extension */

DEFINE DBI_LOCKSCHEME          := 128 AS SHORT /* Locking scheme used by RDD */
DEFINE DBI_ISREADONLY          := 129 AS SHORT /* Was the file opened readonly? */
DEFINE DBI_ROLLBACK            := 130 AS SHORT /* Rollback changes made to current record */
DEFINE DBI_PASSWORD            := 131 AS SHORT /* Workarea password */
DEFINE DBI_ISENCRYPTED         := 132 AS SHORT /* The database is encrypted */
DEFINE DBI_MEMOTYPE            := 133 AS SHORT /* Type of MEMO file: DBT, SMT, FPT */
DEFINE DBI_SEPARATOR           := 134 AS SHORT /* The record separator (as a string) */
DEFINE DBI_MEMOVERSION         := 135 AS SHORT /* sub version of memo file */
DEFINE DBI_TABLETYPE           := 136 AS SHORT /* Type of table file */
DEFINE DBI_SCOPEDRELATION      := 137 AS SHORT /* Is given relation scoped */
DEFINE DBI_TRIGGER             := 138 AS SHORT /* Get/Set trigger function */
DEFINE DBI_OPENINFO            := 139 AS SHORT /* DBOPENINFO structure pointer */
DEFINE DBI_ENCRYPT             := 140 AS SHORT /* Encrypt table */
DEFINE DBI_DECRYPT             := 141 AS SHORT /* Decrypt table */
DEFINE DBI_MEMOPACK            := 142 AS SHORT /* Pack memo file */
DEFINE DBI_DIRTYREAD           := 143 AS SHORT /* Get/Set index dirty read flag */
DEFINE DBI_POSITIONED          := 144 AS SHORT /* Is cursor positioned to valid record */
DEFINE DBI_ISTEMPORARY         := 145 AS SHORT /* Is the table a temporary one? */
DEFINE DBI_LOCKTEST            := 146 AS SHORT /* record / file lock test */
//DEFINE DBI_CODEPAGE            := 147 AS SHORT /* Codepage used */
DEFINE DBI_TRANSREC            := 148 AS SHORT /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 


/* Harbour RECORD MAP (RM) support */

DEFINE DBI_RM_SUPPORTED        := 150 AS SHORT   /* has WA RDD record map support? */
DEFINE DBI_RM_CREATE           := 151 AS SHORT   /* create new empty work area record map */
DEFINE DBI_RM_REMOVE           := 152 AS SHORT   /* remove active work area record map */
DEFINE DBI_RM_CLEAR            := 153 AS SHORT   /* remove all records from WA record map */
DEFINE DBI_RM_FILL             := 154 AS SHORT   /* add all records to WA record map */
DEFINE DBI_RM_ADD              := 155 AS SHORT   /* add record to work area record map */
DEFINE DBI_RM_DROP             := 156 AS SHORT   /* remove record from work area record map */
DEFINE DBI_RM_TEST             := 157 AS SHORT   /* test if record is set in WA record map */
DEFINE DBI_RM_COUNT            := 158 AS SHORT   /* number of records set in record map */
DEFINE DBI_RM_HANDLE           := 159 AS SHORT   /* get/set record map filter handle */ 


// CDX and Comix Record List Support

DEFINE DBI_RL_AND 		:= 1001 AS SHORT
DEFINE DBI_RL_CLEAR 	:= 1002 AS SHORT
DEFINE DBI_RL_COUNT 	:= 1003 AS SHORT
DEFINE DBI_RL_DESTROY 	:= 1004 AS SHORT
DEFINE DBI_RL_EXFILTER 	:= 1005 AS SHORT
DEFINE DBI_RL_GETFILTER := 1006 AS SHORT
DEFINE DBI_RL_HASMAYBE 	:= 1007 AS SHORT
DEFINE DBI_RL_LEN 		:= 1008 AS SHORT
DEFINE DBI_RL_MAYBEEVAL := 1009 AS SHORT
DEFINE DBI_RL_NEW 		:= 1010 AS SHORT
DEFINE DBI_RL_NEWDUP 	:= 1011 AS SHORT
DEFINE DBI_RL_NEWQUERY 	:= 1012 AS SHORT
DEFINE DBI_RL_NEXTRECNO := 1013 AS SHORT
DEFINE DBI_RL_NOT 		:= 1014 AS SHORT
DEFINE DBI_RL_OR 		:= 1015 AS SHORT
DEFINE DBI_RL_PREVRECNO := 1016 AS SHORT
DEFINE DBI_RL_SET 		:= 1017 AS SHORT
DEFINE DBI_RL_SETFILTER := 1018 AS SHORT
DEFINE DBI_RL_TEST 		:= 1019 AS SHORT


DEFINE DBI_USER 				:= 2000 AS SHORT  	// Start of user definable DBI_ values

// Advantage additions
DEFINE DBI_GET_ACE_TABLE_HANDLE  := DBI_USER  + 110
DEFINE DBI_GET_ACE_STMT_HANDLE   := DBI_USER  + 111


// OrderInfo Defines
DEFINE DBOI_CONDITION 	:= 1 AS SHORT    // String: The order's conditional expression     
DEFINE DBOI_EXPRESSION 	:= 2 AS SHORT	// String: The order's key expression             
DEFINE DBOI_POSITION 	:= 3 AS SHORT 	// Number: The current key position in scope and filter  
DEFINE DBOI_RECNO 		:= 4 AS SHORT 	// Number: The current key position disregarding filters 
DEFINE DBOI_NAME 		:= 5 AS SHORT  	// String: The name of the order                      
DEFINE DBOI_NUMBER 		:= 6 AS SHORT	// Number: The numeric position in the list of orders
DEFINE DBOI_BAGNAME 	:= 7 AS SHORT	// String: The name of the file containing this order
DEFINE DBOI_INDEXNAME 	:= DBOI_BAGNAME AS SHORT // Alias
DEFINE DBOI_BAGEXT 		:= 8 AS SHORT   	// String: The extension of the file containing this order
DEFINE DBOI_INDEXEXT  	:= DBOI_BAGEXT AS SHORT // Alias
DEFINE DBOI_ORDERCOUNT  :=  9 AS SHORT   // Number: The count of ORDERS contained in an index file or in total
DEFINE DBOI_FILEHANDLE 	:= 10 AS SHORT	// Stream: The stream of the index
DEFINE DBOI_ISCOND 		:= 11 AS SHORT	// Logic : Does the order have a FOR condition?
DEFINE DBOI_ISDESC 		:= 12 AS SHORT	// Logic : Is the order DESCENDing? 
DEFINE DBOI_UNIQUE 		:= 13 AS SHORT	// Logic : Does the order have the UNIQUE attribute?
  
/* Clipper 5.3-level constants */
DEFINE DBOI_FULLPATH 	:= 20 AS SHORT  	// String: The full path to the index file (Bag)
DEFINE DBOI_KEYTYPE 	:= 24 AS SHORT	// The type of the order's key  
DEFINE DBOI_KEYSIZE 	:= 25 AS SHORT	// Number: The length of the order's key
DEFINE DBOI_KEYCOUNT 	:= 26 AS SHORT	// Number: The count of keys in scope and filter
DEFINE DBOI_SETCODEBLOCK:= 27 AS SHORT	// Block : The codeblock that produces the key 
DEFINE DBOI_KEYDEC 		:= 28 AS SHORT	// Number: The # of decimals in a numeric key 
DEFINE DBOI_HPLOCKING 	:= 29 AS SHORT	// Logic : Using High Performance locking for this order?
DEFINE DBOI_LOCKOFFSET 	:= 35 AS SHORT	// Number: The offset used for logical locking 

DEFINE DBOI_KEYADD 		:= 36 AS SHORT 	// Logic: Custom Index: Was Key added successfully? 
DEFINE DBOI_KEYDELETE 	:= 37 AS SHORT	// Logic: Custom Index: Was Key Deletion successful? 
DEFINE DBOI_KEYVAL 		:= 38 AS SHORT	// Object: The value of the current key 
DEFINE DBOI_SCOPETOP 	:= 39 AS SHORT	// Object: Get or Set the scope top    
DEFINE DBOI_SCOPEBOTTOM := 40 AS SHORT	// Object: Get or Set the scope bottom
DEFINE DBOI_SCOPETOPCLEAR := 41 AS SHORT	// None	 :
DEFINE DBOI_SCOPEBOTTOMCLEAR:= 42 AS SHORT// None :
DEFINE DBOI_CUSTOM 		:= 45 AS SHORT	// Logic: Is this a Custom Index?  
DEFINE DBOI_SKIPUNIQUE 	:= 46 AS SHORT	// Logic: Was a skip to adjacent unique Key successful?  

DEFINE DBOI_KEYSINCLUDED:= 50 AS SHORT  	// Number: Number of keys in the index order
DEFINE DBOI_KEYNORAW 	:= 51 AS SHORT 	// Number: The key number disregarding filters
DEFINE DBOI_KEYCOUNTRAW := 52 AS SHORT   // Number: The key count disregarding filter  
DEFINE DBOI_OPTLEVEL 	:= 53 AS SHORT	// Number: Optimization level for current query


/* Harbour extensions */

DEFINE DBOI_SKIPEVAL           := 100 AS SHORT /* skip while code block doesn't return TRUE */
DEFINE DBOI_SKIPEVALBACK       := 101 AS SHORT /* skip backward while code block doesn't return TRUE */
DEFINE DBOI_SKIPREGEX          := 102 AS SHORT /* skip while regular expression on index key doesn't return TRUE */
DEFINE DBOI_SKIPREGEXBACK      := 103 AS SHORT /* skip backward while regular expression on index key doesn't return TRUE */
DEFINE DBOI_SKIPWILD           := 104 AS SHORT /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
DEFINE DBOI_SKIPWILDBACK       := 105 AS SHORT /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
DEFINE DBOI_SCOPEEVAL          := 106 AS SHORT /* skip through index evaluating given C function */
DEFINE DBOI_FINDREC            := 107 AS SHORT /* find given record in a Tag beginning from TOP */
DEFINE DBOI_FINDRECCONT        := 108 AS SHORT /* find given record in a Tag beginning from current position */
DEFINE DBOI_SCOPESET           := 109 AS SHORT /* set both scopes */
DEFINE DBOI_SCOPECLEAR         := 110 AS SHORT /* clear both scopes */


DEFINE DBOI_BAGCOUNT           := 111 AS SHORT /* number of open order bags */
DEFINE DBOI_BAGNUMBER          := 112 AS SHORT /* bag position in bag list */
DEFINE DBOI_BAGORDER           := 113 AS SHORT /* number of first order in a bag */
DEFINE DBOI_ISMULTITAG         := 114 AS SHORT /* does RDD support multi tag in index file */
DEFINE DBOI_ISSORTRECNO        := 115 AS SHORT /* is record number part of key in sorting */
DEFINE DBOI_LARGEFILE          := 116 AS SHORT /* is large file size (>=4GB) supported */
DEFINE DBOI_TEMPLATE           := 117 AS SHORT /* order with free user keys */
DEFINE DBOI_MULTIKEY           := 118 AS SHORT /* custom order with multikeys */
DEFINE DBOI_CHGONLY            := 119 AS SHORT /* update only existing keys */
DEFINE DBOI_PARTIAL            := 120 AS SHORT /* is index partially updated */
DEFINE DBOI_SHARED             := 121 AS SHORT /* is index open in shared mode */
DEFINE DBOI_ISREADONLY         := 122 AS SHORT /* is index open in readonly mode */
DEFINE DBOI_READLOCK           := 123 AS SHORT /* get/set index read lock */
DEFINE DBOI_WRITELOCK          := 124 AS SHORT /* get/set index write lock */
DEFINE DBOI_UPDATECOUNTER      := 125 AS SHORT /* get/set update index counter */
DEFINE DBOI_EVALSTEP           := 126 AS SHORT /* eval step (EVERY) used in index command */
DEFINE DBOI_ISREINDEX          := 127 AS SHORT /* Is reindex in process */
DEFINE DBOI_I_BAGNAME          := 128 AS SHORT /* created index name */
DEFINE DBOI_I_TAGNAME          := 129 AS SHORT /* created tag name */
DEFINE DBOI_RELKEYPOS          := 130 AS SHORT /* get/set relative key position (in range 0 - 1) */
DEFINE DBOI_USECURRENT         := 131 AS SHORT /* get/set "use current index" flag */
DEFINE DBOI_INDEXTYPE          := 132 AS SHORT /* current index type */
DEFINE DBOI_RESETPOS           := 133 AS SHORT /* rest logical and raw positions */
DEFINE DBOI_INDEXPAGESIZE      := 134 AS SHORT /* get index page size */

DEFINE DBOI_USER 				:= 1000 AS SHORT

// Advantage extensions

DEFINE DBOI_AXS_PERCENT_INDEXED  := 1805 as SHORT
DEFINE DBOI_GET_ACE_INDEX_HANDLE := 1806 as SHORT


// Duplicates
DEFINE DBOI_KEYGOTO 	:= DBOI_POSITION  AS SHORT
DEFINE DBOI_KEYGOTORAW 	:= DBOI_KEYNORAW  AS SHORT
DEFINE DBOI_KEYNO	 	:= DBOI_POSITION  AS SHORT

// Blob defines

DEFINE BLOB_INFO_HANDLE 	:= 201 AS SHORT
DEFINE BLOB_FILE_RECOVER 	:= 202 AS SHORT
DEFINE BLOB_FILE_INTEGRITY 	:= 203 AS SHORT
DEFINE BLOB_OFFSET 			:= 204 AS SHORT
DEFINE BLOB_POINTER 		:= 205 AS SHORT
DEFINE BLOB_LEN 			:= 206 AS SHORT
DEFINE BLOB_TYPE 			:= 207 AS SHORT
DEFINE BLOB_EXPORT 			:= 208 AS SHORT
DEFINE BLOB_ROOT_UNLOCK 	:= 209 AS SHORT
DEFINE BLOB_ROOT_PUT 		:= 210 AS SHORT
DEFINE BLOB_ROOT_GET 		:= 211 AS SHORT
DEFINE BLOB_ROOT_LOCK 		:= 212 AS SHORT
DEFINE BLOB_IMPORT 			:= 213 AS SHORT
DEFINE BLOB_DIRECT_PUT 		:= 214 AS SHORT
DEFINE BLOB_DIRECT_GET 		:= 215 AS SHORT
DEFINE BLOB_GET 			:= 216 AS SHORT
DEFINE BLOB_DIRECT_EXPORT 	:= 217 AS SHORT
DEFINE BLOB_DIRECT_IMPORT 	:= 218 AS SHORT
DEFINE BLOB_NMODE 			:= 219 AS SHORT
DEFINE BLOB_USER			:= 1000 AS SHORT

DEFINE BLOB_EXPORT_APPEND 	:= 0 AS SHORT
DEFINE BLOB_EXPORT_OVERWRITE:= 1 AS SHORT

DEFINE BLOB_IMPORT_COMPRESS := 1 AS SHORT
DEFINE BLOB_IMPORT_ENCRYPT	:= 2 AS SHORT


/* return values for DBOI_OPTLEVEL */

DEFINE DBOI_OPTIMIZED_NONE       := 0 AS BYTE
DEFINE DBOI_OPTIMIZED_PART       := 1 AS BYTE
DEFINE DBOI_OPTIMIZED_FULL       := 2 AS BYTE

/* return values for DBOI_INDEXTYPE */

DEFINE DBOI_TYPE_UNDEF          := -1 AS SHORT
DEFINE DBOI_TYPE_NONE           :=  0 AS SHORT
DEFINE DBOI_TYPE_NONCOMPACT     :=  1 AS SHORT
DEFINE DBOI_TYPE_COMPACT        :=  2 AS SHORT
DEFINE DBOI_TYPE_COMPOUND       :=  3 AS SHORT

/* constants for DBOI_SCOPEEVAL array parameter */

DEFINE DBRMI_FUNCTION           := 1 AS BYTE
DEFINE DBRMI_PARAM              := 2 AS BYTE
DEFINE DBRMI_LOVAL              := 3 AS BYTE
DEFINE DBRMI_HIVAL              := 4 AS BYTE
DEFINE DBRMI_RESULT             := 5 AS BYTE
DEFINE DBRMI_SIZE               := 5 AS BYTE

/* Numeric DBF TYPES */
DEFINE DB_DBF_STD              := 1 AS BYTE
DEFINE DB_DBF_VFP              := 2 AS BYTE 


/* Numeric MEMO TYPES */
DEFINE DB_MEMO_NONE            := 0 AS BYTE
DEFINE DB_MEMO_DBT             := 1 AS BYTE
DEFINE DB_MEMO_FPT             := 2 AS BYTE
DEFINE DB_MEMO_SMT             := 3 AS BYTE
 
/* MEMO EXTENDED TYPES */
DEFINE DB_MEMOVER_STD          := 1 AS BYTE
DEFINE DB_MEMOVER_SIX          := 2 AS BYTE
DEFINE DB_MEMOVER_FLEX         := 3 AS BYTE
DEFINE DB_MEMOVER_CLIP         := 4 AS BYTE


/* LOCK SCHEMES */
DEFINE DB_DBFLOCK_DEFAULT      := 0 AS BYTE 
DEFINE DB_DBFLOCK_CLIPPER      := 1 AS BYTE    /* default Cl*pper locking scheme */
DEFINE DB_DBFLOCK_COMIX        := 2 AS BYTE    /* COMIX and CL53 DBFCDX hyper locking scheme */
DEFINE DB_DBFLOCK_VFP          := 3 AS BYTE    /* [V]FP, CL52 DBFCDX, SIx3 SIXCDX, CDXLOCK.OBJ */
DEFINE DB_DBFLOCK_HB32         := 4 AS BYTE    /* Harbour hyper locking scheme for 32bit file API */
DEFINE DB_DBFLOCK_HB64         := 5 AS BYTE    /* Harbour hyper locking scheme for 64bit file API */
DEFINE DB_DBFLOCK_CLIPPER2     := 6 AS BYTE    /* extended Cl*pper locking scheme NTXLOCK2.OBJ */ 


// File Extensions
DEFINE DBT_MEMOEXT             := ".dbt" AS STRING
DEFINE FPT_MEMOEXT             := ".fpt" AS STRING
DEFINE SMT_MEMOEXT             := ".smt" AS STRING
DEFINE DBV_MEMOEXT             := ".dbv" AS STRING

// Blocks
DEFINE DBT_DEFBLOCKSIZE        := 512 AS SHORT
DEFINE FPT_DEFBLOCKSIZE        := 64 AS SHORT
DEFINE SMT_DEFBLOCKSIZE        := 32 AS SHORT

// Locks
DEFINE FPT_LOCKPOS             := 0	AS SHORT
DEFINE FPT_LOCKSIZE            := 1 AS SHORT
DEFINE FPT_ROOTBLOCK_OFFSET    := 536 AS SHORT /* Clipper 5.3 ROOT data block offset */

DEFINE SIX_ITEM_BUFSIZE        :=    14 AS SHORT
DEFINE FLEX_ITEM_BUFSIZE       :=     8 AS SHORT
DEFINE MAX_SIXFREEBLOCKS       :=    82 AS SHORT
DEFINE MAX_FLEXFREEBLOCKS      :=   126 AS SHORT
DEFINE FLEXGCPAGE_SIZE         :=  1010 AS SHORT 


// RDD Inheritance
DEFINE RDT_FULL            :=  1	AS SHORT
DEFINE RDT_TRANSFER        :=  2	AS SHORT	
DEFINE RDT_HIDDEN          :=  8	AS SHORT

/*
/* "V" field types * /
#define HB_VF_CHAR            64000

#define HB_VF_DATE            64001

#define HB_VF_INT             64002

#define HB_VF_LOG             64003

#define HB_VF_DNUM            64004

#define HB_VF_ARRAY           64005

#define HB_VF_BLOB            64006

#define HB_VF_BLOBCOMPRESS    64007

#define HB_VF_BLOBENCRYPT     64008



/* SMT types * /

#define SMT_IT_NIL            0

#define SMT_IT_CHAR           1

#define SMT_IT_INT            2

#define SMT_IT_DOUBLE         3

#define SMT_IT_DATE           4

#define SMT_IT_LOGICAL        5

#define SMT_IT_ARRAY          6


#define FPTIT_DUMMY        0xDEADBEAF

#define FPTIT_BINARY       0x0000

#define FPTIT_PICT         0x0000      

/* Picture * /

#define FPTIT_TEXT         0x0001      /* Text    * /

#define FPTIT_OBJ          0x0002      /* Object  * /

#define FPTIT_SIX_NIL      0x0000      /* NIL VALUE (USED ONLY IN ARRAYS) * /
#define FPTIT_SIX_LNUM     0x0002      /* LONG LE * /
#define FPTIT_SIX_DNUM     0x0008      /* DOUBLE LE * /

#define FPTIT_SIX_LDATE    0x0020      /* DATE (LONG LE) * /
#define FPTIT_SIX_LOG      0x0080      /* LOGIC * /

#define FPTIT_SIX_CHAR     0x0400      /* CHAR * /

#define FPTIT_SIX_ARRAY    0x8000      /* ARRAY * / 
*/


// RDD Errors
DEFINE EDB                 := 1000		AS SHORT
DEFINE EDB_SEEK            := EDB + 1
DEFINE EDB_SKIP            := EDB + 2
DEFINE EDB_GOTO            := EDB + 3
DEFINE EDB_SETRELATION     := EDB + 4
DEFINE EDB_USE             := EDB + 5
DEFINE EDB_CREATEINDEX     := EDB + 6
DEFINE EDB_SETORDER        := EDB + 7
DEFINE EDB_SETINDEX        := EDB + 8
DEFINE EDB_FIELDNAME       := EDB + 9
DEFINE EDB_BADALIAS        := EDB + 10
DEFINE EDB_DUPALIAS        := EDB + 11
DEFINE EDB_SETFILTER       := EDB + 12
DEFINE EDB_CYCLICREL       := EDB + 13
DEFINE EDB_CREATETABLE     := EDB + 14
DEFINE EDB_RDDNOTFOUND     := EDB + 15
	// RESERVED EDB + 16
DEFINE EDB_FIELDINDEX      := EDB + 17
DEFINE EDB_SELECT          := EDB + 18
DEFINE EDB_SYMSELECT       := EDB + 19
DEFINE EDB_TOTAL           := EDB + 20
DEFINE EDB_RECNO           := EDB + 21
DEFINE EDB_EXPRESSION      := EDB + 22
DEFINE EDB_EXPR_WIDTH      := EDB + 23
	// RESERVED EDB + 24 - 29
DEFINE EDB_DRIVERLOAD      := EDB + 30
DEFINE EDB_PARAM           := EDB + 31
DEFINE EDB_NOAREAS         := EDB + 32
DEFINE EDB_NOMEM           := EDB + 33
DEFINE EDB_NOFIELDS        := EDB + 35
DEFINE EDB_BAD_ERROR_INFO  := EDB + 36
DEFINE EDB_WRONGFIELDNAME  := EDB + 37
DEFINE EDB_ORDDESTROY      := EDB + 38
DEFINE EDB_NOINITFUNCTION  := EDB + 39
DEFINE EDB_ERRORINIT       := EDB + 40
DEFINE EDB_DBSTRUCT        := EDB + 41
	// RESERVED EDB + 42 - 49
DEFINE EDB_NOTABLE         := EDB + 50
DEFINE EDB_NOORDER         := EDB + 51
	// RESERVED EDB + 52
DEFINE EDB_ASSERTION       := EDB + 53



DEFINE   F_ERROR := PTR(_CAST,0xFFFFFFFF) // Error value (all functions)

// FERROR() returns, which are not reflected as DOSERROR()
DEFINE   FERROR_FULL    := 256   // disk full
DEFINE   FERROR_EOF     := 257   // eof was already reached, when a read was tried
DEFINE   FERROR_PARAM   := 258   // invalid parameter already detected before giving to DOS

// FSEEK(), _llseek() modes
DEFINE   FS_SET         := 0  // Seek from beginning of file
DEFINE   FS_RELATIVE    := 1  // Seek from current file position
DEFINE   FS_END         := 2  // Seek from end of file

// FOPEN() access modes
DEFINE   FO_READ        := 0  // Open for reading (default)
DEFINE   FO_WRITE       := 1  // Open for writing
DEFINE   FO_READWRITE   := 2  // Open for reading or writing

// FOPEN() sharing modes (combine with open mode using +)
DEFINE FO_COMPAT     := 0x00000000  // Compatibility mode (default)
DEFINE FO_EXCLUSIVE  := 0x00000010  // Exclusive
DEFINE FO_DENYWRITE  := 0x00000020  // Prevent other processes from writing
DEFINE FO_DENYREAD   := 0x00000030  // Prevent other processes from reading
DEFINE FO_DENYNONE   := 0x00000040  // (same as FO_SHARED)
DEFINE FO_SHARED     := 0x00000040  // Allow other processes to read or write

// FXOPEN() mode
DEFINE FXO_WILD      := 0x00010000  // Allow wildcards in file name
 // FCREATE() file attribute modes (always opens with OF_READWRITE)
DEFINE FC_NORMAL     := 0x00000000  // normal read/write file (default for create)
DEFINE FC_READONLY   := 0x00000001  // read-only file
DEFINE FC_HIDDEN     := 0x00000002  // hidden file
DEFINE FC_SYSTEM     := 0x00000004  // system file
DEFINE FC_ARCHIVED   := 0x00000020

//
// additional file attribute for DIRECTORY(), FFIRST() and FFCOUNT()
//
DEFINE FA_VOLUME     := 0x00000008
DEFINE FA_DIRECTORY  := 0x00000010
DEFINE FA_NORMAL	 := 0x00000080
DEFINE FA_TEMPORARY  := 0x00000100
DEFINE FA_COMPRESSED := 0x00000800
DEFINE FA_OFFLINE    := 0x00001000


//
// subscripts for Directory() array
//
DEFINE F_NAME  := 1
DEFINE F_SIZE  := 2
DEFINE F_DATE  := 3
DEFINE F_TIME  := 4
DEFINE F_ATTR  := 5
DEFINE F_LEN   := 5   // length of array


//
// Standard math definitions
//

DEFINE PI      :=    3.14159265358979323846      // Pi
DEFINE PI_2    :=    1.57079632679489661923      // Pi/2  ** NEW !
DEFINE PI_4    :=    0.785398163397448309616     // Pi/4  ** NEW !
DEFINE L2E     :=    1.44269504088896340736      // Log2 ( e)
DEFINE L2T     :=    3.32192809488736234781      // Log2 (10)
DEFINE L10E    :=    0.434294481903251827651     // Log10 (e) ** NEW !
DEFINE LG2     :=    0.301029995663981195226     // Log10( 2)
DEFINE LN2     :=    0.693147180559945309417     // LogE ( 2)
DEFINE LN10    :=    2.30258509299404568402      // LogE ( 10) ** NEW !

DEFINE REAL4_EPSILON := 1.192092896e-07          // smallest such that 1.0+REAL4_EPSILON != 1.0
DEFINE REAL4_MAX     := 3.402823466e+38          // Maximum representable number
DEFINE REAL8_EPSILON := 2.2204460492503131e-016  // smallest such that 1.0+REAL8_EPSILON != 1.0
DEFINE REAL8_MAX     := 1.7976931348623158e+308  // Maximum representable number


DEFINE TICK_FREQUENCY   :=    18.20647  // ticks per second 

//
// ASCII character equates
//

DEFINE ASC_BELL   :=     7    // Bell
DEFINE ASC_BS     :=     8    // BackSpace
DEFINE ASC_TAB    :=     9    // Tab
DEFINE ASC_LF     :=    10    // LineFeed
DEFINE ASC_FF     :=    12    // FormFeed
DEFINE ASC_CR     :=    13    // CarriageReturn
DEFINE ASC_SOFT_CR :=   141      // softCarriageReturn
DEFINE ASC_EOF    :=    26    // EndOfFile
DEFINE ASC_ESC    :=    27    // Escape
DEFINE ASC_BLANK  :=    32
DEFINE ASC_0      :=    48
DEFINE ASC_1      :=    49
DEFINE ASC_9      :=    57
DEFINE ASC_A      :=    65
DEFINE ASC_Z      :=    90

DEFINE CRLF       := Chr(ASC_CR) +Chr(ASC_LF)

DEFINE INI_GROUP_RUNTIME :=   "CA-Visual Objects"
