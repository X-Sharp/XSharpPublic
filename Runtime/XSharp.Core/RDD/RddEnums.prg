//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

begin namespace XSharp
	enum AutoShareMode
		member NoChange		 := 0
		member Auto	  		 := 1
		member ForeExclusive := 2
	end enum
	
	enum CollationMode
		member WINDOWS
		member CLIPPER
	end enum
	
	enum DbLockMode
		member Lock
		member UnLock
	end	 enum
	
	enum DbRecordInfo
		member DBRI_DELETED 	:= 1 
		member DBRI_LOCKED 		:= 2 	
		member DBRI_RECSIZE 	:= 3	
		member DBRI_RECNO 		:= 4	
		member DBRI_UPDATED 	:= 5	
		member DBRI_BUFFPTR 	:= 6 
		member DBRI_RAWRECORD   := 6
		// harbour
		member DBRI_ENCRYPTED	:= 7 
		member DBRI_RAWMEMOS	:= 8 
		member DBRI_RAWDATA		:= 9 
		member DBRI_USER 		:= 1000
	end	 enum
	
	enum DbFieldInfo
		member DBS_NAME				:= 1
		member DBS_TYPE				:= 2
		member DBS_LEN				:= 3
		member DBS_DEC				:= 4
		member DBS_ALIAS			:= 5
		// harbour extensions
		member DBS_ISNULL			:= 11
		member DBS_COUNTER			:= 12
		member DBS_STEP				:= 13
		
		member DBS_BLOB_GET			:= 101
		member DBS_BLOB_TYPE		:= 102
		member DBS_BLOB_LEN			:= 103
		member DBS_BLOB_OFFSET		:= 104
		member DBS_BLOB_POINTER		:= 198
		member DBS_BLOB_DIRECT_TYPE	:= 222
		member DBS_BLOB_DIRECT_LEN	:= 223
		member DBS_STRUCT			:= 998
		member DBS_PROPERTIES		:= 999
		member DBS_USER				:= 1000
	end	 enum             
	
	enum DbInfo
		// matches numeric values with Vulcan
		member DBI_ISDBF 			:= 1				// Logical: RDD support DBF file format?
		member DBI_CANPUTREC		:= 2				// Logical: RDD support Putting Records? 
		member DBI_GETHEADERSIZE	:= 3				// Numeric: Get header size of the file 
		member DBI_LASTUPDATE		:= 4				// Date:    Last date RDD file updated 
		member DBI_GETDELIMITER		:= 5				// String:  Get default delimiter
		member DBI_SETDELIMITER		:= 6				// String:  Set default delimiter
		member DBI_GETRECSIZE		:= 7				// Numeric: Get record size of the file
		member DBI_GETLOCKARRAY		:= 8				// Array:   Get array of locked records 
		member DBI_TABLEEXT 		:= 9				// String:  Get table file extension
		member DBI_READONLY 		:= 10				// Logic: is the file opened readonly
		// 11-19 missing								
		member DBI_ISFLOCK 			:= 20				// Logic: Is there a file lock active? 
		// 21 missing									
		member DBI_CHILDCOUNT 		:= 22				// Number: Number of child relations set
		member DBI_FILEHANDLE 		:= 23				// Stream: The data file's file stream
		member DBI_FULLPATH			:= 24				// String: Full path to data file
		member DBI_ISANSI 			:= 25				// Logic: Is the file ansi encoded
		member DBI_BOF 				:= 26				// Logic: Same as Bof()
		member DBI_EOF 				:= 27				// Logic: Same as Eof()
		member DBI_DBFILTER 		:= 28				// String: Current Filter setting 
		member DBI_FOUND 			:= 29				// Logic: Same as Found() 
		member DBI_FCOUNT 			:= 30				// Number: Number of fields per record
		member DBI_LOCKCOUNT		:= 31				// Number: Number of record locks  
		member DBI_VALIDBUFFER		:= 32				// Logic: Is the buffer Valid
		member DBI_ALIAS 			:= 33				// String: Alias
		member DBI_GETSCOPE 		:= 34				// Object: The ScopeInfo
		member DBI_LOCKOFFSET		:= 35				// Number: Lock offset
		member DBI_SHARED 			:= 36				// Logic: is the file opened shared
		member DBI_MEMOEXT 			:= 37				// String: Memo file extension
		member DBI_MEMOHANDLE		:= 38				// Stream: The memo file's file stream
		member DBI_BLOB_HANDLE 		:= 38				// Alias for MemoHandle
		member DBI_MEMOBLOCKSIZE 	:= 39				// Number: The memo block size
		member DBI_BLOB_INTEGRITY	:= 40				// Number: The Windows Codepage
		member DBI_CODEPAGE 		:= 41				// Logic:  Use new index lock mechanism
		member DBI_DOSCODEPAGE		:= 42				// Number: The DOS Codepage
		member DBI_BLOB_RECOVER 	:= 43
		member DBI_NEWINDEXLOCK 	:= 44
		member DBI_STRICTREAD  		:= 60				// Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  
		member DBI_OPTIMIZE    		:= 61 				// Flag for whether to use query optimization             
		member DBI_AUTOOPEN    		:= 62 				// Flag for automatically opening structural indexes      
		member DBI_AUTOORDER   		:= 63 				// When a structural index is opened, the order to be set 
		member DBI_AUTOSHARE   		:= 64 				// When a network is detected, open the index shared, otherwise open exclusively   
		member DBI_DB_VERSION 		:= 101
		member DBI_RDD_VERSION 		:= 102					
		member DBI_RDD_LIST 		:= 103
		member DBI_MEMOFIELD 		:= 104
		member DBI_VO_MACRO_SYNTAX 	:= 105
		member DBI_RDD_OBJECT		:= 106
		// 107 - 127 missing
		// Harbour extensions
		member DBI_LOCKSCHEME          := 128 /* Locking scheme used by RDD */
		member DBI_ISREADONLY          := 129 /* Was the file opened readonly? */
		member DBI_ROLLBACK            := 130 /* Rollback changes made to current record */
		member DBI_PASSWORD            := 131 /* Workarea password */
		member DBI_ISENCRYPTED         := 132 /* The database is encrypted */
		member DBI_MEMOTYPE            := 133 /* Type of MEMO file: DBT, SMT, FPT */
		member DBI_SEPARATOR           := 134 /* The record separator (as a string) */
		member DBI_MEMOVERSION         := 135 /* sub version of memo file */
		member DBI_TABLETYPE           := 136 /* Type of table file */
		member DBI_SCOPEDRELATION      := 137 /* Is given relation scoped */
		member DBI_TRIGGER             := 138 /* Get/Set trigger function */
		member DBI_OPENINFO            := 139 /* DBOPENINFO structure pointer */
		member DBI_ENCRYPT             := 140 /* Encrypt table */
		member DBI_DECRYPT             := 141 /* Decrypt table */
		member DBI_MEMOPACK            := 142 /* Pack memo file */
		member DBI_DIRTYREAD           := 143 /* Get/Set index dirty read flag */
		member DBI_POSITIONED          := 144 /* Is cursor positioned to valid record */
		member DBI_ISTEMPORARY         := 145 /* Is the table a temporary one? */
		member DBI_LOCKTEST            := 146 /* record / file lock test */
		member DBI_CODEPAGE_HB         := 147 /* Codepage used also memberd by VO and Vulcan */ 
		member DBI_TRANSREC            := 148  /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
		member DBI_SETHEADER		   := 149	/* DBF header updating modes */ 
		/* Harbour RECORD MAP (RM) support */
		member DBI_RM_SUPPORTED        := 150 /* has WA RDD record map support? */
		member DBI_RM_CREATE           := 151 /* create new empty work area record map */
		member DBI_RM_REMOVE           := 152 /* remove active work area record map */
		member DBI_RM_CLEAR            := 153 /* remove all records from WA record map */
		member DBI_RM_FILL             := 154 /* add all records to WA record map */
		member DBI_RM_ADD              := 155 /* add record to work area record map */
		member DBI_RM_DROP             := 156 /* remove record from work area record map */
		member DBI_RM_TEST             := 157 /* test if record is set in WA record map */
		member DBI_RM_COUNT            := 158 /* number of records set in record map */
		member DBI_RM_HANDLE           := 159 /* get/set record map filter handle */ 
		// 160 - 160 missing
		member DBI_QUERY				:= 170 /* if area represents result of a query, obtain expression of this query */ 
		// 171 - 200 missing		
		member BLOB_INFO_HANDLE		:= 201
		member BLOB_FILE_RECOVER	:= 202
		member BLOB_FILE_INTEGRITY	:= 203
		member BLOB_OFFSET			:= 204
		member BLOB_POINTER			:= 205
		member BLOB_LEN				:= 206
		member BLOB_TYPE			:= 207
		member BLOB_EXPORT			:= 208
		member BLOB_ROOT_UNLOCK		:= 209
		member BLOB_ROOT_PUT		:= 210
		member BLOB_ROOT_GET		:= 211
		member BLOB_ROOT_LOCK		:= 212
		member BLOB_IMPORT			:= 213
		member BLOB_DIRECT_PUT		:= 214
		member BLOB_DIRECT_GET		:= 215
		member BLOB_GET				:= 216
		member BLOB_DIRECT_EXPORT	:= 217
		member BLOB_DIRECT_IMPORT	:= 218
		member BLOB_NMODE			:= 219
		member BLOB_EXPORT_APPEND	:= 220
		member BLOB_EXPORT_OVERWRITE:= 221
		member BLOB_DIRECT_TYPE		:= 222
		member BLOB_DIRECT_LEN		:= 223
		member BLOB_USER			:= 2000
		
		// Clipmore functions
		member DBI_USER 			:= 1000
		member DBI_RL_AND 			:= 1001
		member DBI_RL_CLEAR 		:= 1002
		member DBI_RL_COUNT 		:= 1003
		member DBI_RL_DESTROY 		:= 1004
		member DBI_RL_EXFILTER 		:= 1005
		member DBI_RL_GETFILTER 	:= 1006
		member DBI_RL_HASMAYBE 		:= 1007
		member DBI_RL_LEN 			:= 1008
		member DBI_RL_MAYBEEVAL 	:= 1009
		member DBI_RL_NEW 			:= 1010
		member DBI_RL_NEWDUP 		:= 1011
		member DBI_RL_NEWQUERY 		:= 1012
		member DBI_RL_NEXTRECNO 	:= 1013
		member DBI_RL_NOT 			:= 1014
		member DBI_RL_OR 			:= 1015
		member DBI_RL_PREVRECNO 	:= 1016
		member DBI_RL_SET 			:= 1017
		member DBI_RL_SETFILTER 	:= 1018
		member DBI_RL_TEST 			:= 1019
		
		
		// advnatage
		member DBI_GET_ACE_TABLE_HANDLE  := 1110
		member DBI_GET_ACE_STMT_HANDLE   := 1111
		
		
	end	 enum
	
	
	enum DbOrderInfo
		member DBOI_CONDITION 	:= 1     // String: The order's conditional expression     
		member DBOI_EXPRESSION 	:= 2 	// String: The order's key expression             
		member DBOI_POSITION 	:= 3  	// Number: The current key position in scope and filter  
		member DBOI_KEYNO	 	:= 3	// Alias
		member DBOI_KEYGOTO 	:= 3	// Alias
		member DBOI_RECNO 		:= 4  	// Number: The current key position disregarding filters 
		member DBOI_NAME 		:= 5   	// String: The name of the order                      
		member DBOI_NUMBER 		:= 6 	// Number: The numeric position in the list of orders
		member DBOI_BAGNAME 	:= 7 	// String: The name of the file containing this order
		member DBOI_INDEXNAME 	:= 7	// Alias
		member DBOI_BAGEXT 		:= 8    // String: The extension of the file containing this order
		member DBOI_INDEXEXT  	:= 8	// Alias
		member DBOI_ORDERCOUNT  :=  9   // Number: The count of ORDERS contained in an index file or in total
		member DBOI_FILEHANDLE 	:= 10 	// Stream: The stream of the index
		member DBOI_ISCOND 		:= 11 	// Logic : Does the order have a FOR condition?
		member DBOI_ISDESC 		:= 12 	// Logic : Is the order DESCENDing? 
		member DBOI_UNIQUE 		:= 13 	// Logic : Does the order have the UNIQUE attribute?
		
		// 14-19 missign
		/* Clipper 5.3-level constants */
		member DBOI_FULLPATH 	:= 20   	// String: The full path to the index file (Bag)
		// 21-23
		member DBOI_KEYTYPE 	:= 24 	// The type of the order's key  
		member DBOI_KEYSIZE 	:= 25 	// Number: The length of the order's key
		member DBOI_KEYCOUNT 	:= 26 	// Number: The count of keys in scope and filter
		member DBOI_SETCODEBLOCK:= 27 	// Block : The codeblock that produces the key 
		member DBOI_KEYDEC 		:= 28 	// Number: The # of decimals in a numeric key 
		member DBOI_HPLOCKING 	:= 29 	// Logic : Using High Performance locking for this order?
		member DBOI_LOCKOFFSET 	:= 35 	// Number: The offset used for logical locking 
		
		member DBOI_KEYADD 		:= 36  	// Logic: Custom Index: Was Key added successfully? 
		member DBOI_KEYDELETE 	:= 37 	// Logic: Custom Index: Was Key Deletion successful? 
		member DBOI_KEYVAL 		:= 38 	// Object: The value of the current key 
		member DBOI_SCOPETOP 	:= 39 	// Object: Get or Set the scope top    
		member DBOI_SCOPEBOTTOM := 40 	// Object: Get or Set the scope bottom
		member DBOI_SCOPETOPCLEAR := 41  	// None	 :
		member DBOI_SCOPEBOTTOMCLEAR:= 42 // None :
		member DBOI_CUSTOM 		:= 45 // Logic: Is this a Custom Index?  
		member DBOI_SKIPUNIQUE 	:= 46 // Logic: Was a skip to adjacent unique Key successful?  
		// 47-49
		member DBOI_KEYSINCLUDED:= 50 	// Number: Number of keys in the index order
		member DBOI_KEYNORAW 	:= 51 // Number: The key number disregarding filters
		member DBOI_KEYGOTORAW 	:= 51	// Alias
		member DBOI_KEYCOUNTRAW := 52  // Number: The key count disregarding filter  
		/* Query Optimization */ 
		member DBOI_OPTLEVEL 	:= 53 // Number: Optimization level for current query
		
		// 54-59
		/* These shouldn't need an open table */
		member DBOI_STRICTREAD          := 60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
		member DBOI_OPTIMIZE            := 61  /* Flag for whether to use query optimization             */
		member DBOI_AUTOOPEN            := 62  /* Flag for automatically opening structural indexes      */
		member DBOI_AUTOORDER           := 63  /* When a structural index is opened, the order to be set */
		member DBOI_AUTOSHARE           := 64  /* When a network is detected, open the index shared, otherwise open exclusively   */ 

		// 65-99
		/* Harbour extensions */
		member DBOI_SKIPEVAL           := 100  /* skip while code block doesn't return TRUE */
		member DBOI_SKIPEVALBACK       := 101  /* skip backward while code block doesn't return TRUE */
		member DBOI_SKIPREGEX          := 102  /* skip while regular expression on index key doesn't return TRUE */
		member DBOI_SKIPREGEXBACK      := 103  /* skip backward while regular expression on index key doesn't return TRUE */
		member DBOI_SKIPWILD           := 104  /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
		member DBOI_SKIPWILDBACK       := 105  /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
		member DBOI_SCOPEEVAL          := 106  /* skip through index evaluating given C function */
		member DBOI_FINDREC            := 107  /* find given record in a Tag beginning from TOP */
		member DBOI_FINDRECCONT        := 108  /* find given record in a Tag beginning from current position */
		member DBOI_SCOPESET           := 109  /* set both scopes */
		member DBOI_SCOPECLEAR         := 110  /* clear both scopes */
		member DBOI_BAGCOUNT           := 111  /* number of open order bags */
		member DBOI_BAGNUMBER          := 112  /* bag position in bag list */
		member DBOI_BAGORDER           := 113  /* number of first order in a bag */
		member DBOI_ISMULTITAG         := 114  /* does RDD support multi tag in index file */
		member DBOI_ISSORTRECNO        := 115  /* is record number part of key in sorting */
		member DBOI_LARGEFILE          := 116  /* is large file size (>=4GB) supported */
		member DBOI_TEMPLATE           := 117  /* order with free user keys */
		member DBOI_MULTIKEY           := 118  /* custom order with multikeys */
		member DBOI_CHGONLY            := 119  /* update only existing keys */
		member DBOI_PARTIAL            := 120  /* is index partially updated */
		member DBOI_SHARED             := 121  /* is index open in shared mode */
		member DBOI_ISREADONLY         := 122  /* is index open in readonly mode */
		member DBOI_READLOCK           := 123  /* get/set index read lock */
		member DBOI_WRITELOCK          := 124  /* get/set index write lock */
		member DBOI_UPDATECOUNTER      := 125  /* get/set update index counter */
		member DBOI_EVALSTEP           := 126  /* eval step (EVERY) used in index command */
		member DBOI_ISREINDEX          := 127  /* Is reindex in process */
		member DBOI_I_BAGNAME          := 128  /* created index name */
		member DBOI_I_TAGNAME          := 129  /* created tag name */
		member DBOI_RELKEYPOS          := 130  /* get/set relative key position (in range 0 - 1) */
		member DBOI_USECURRENT         := 131  /* get/set "use current index" flag */
		member DBOI_INDEXTYPE          := 132  /* current index type */
		member DBOI_RESETPOS           := 133  /* rest logical and raw positions */
		member DBOI_INDEXPAGESIZE      := 134  /* get index page size */
		
		member DBOI_USER 				:= 1000 
		// Advantage
		member DBOI_AXS_PERCENT_INDEXED  := 1805
		member DBOI_GET_ACE_INDEX_HANDLE := 1806
		
		
		
	end enum
	
	enum DbFieldType as byte
		member @@Unknown		:= 0	//                                  
		member @@Character 		:= 67 	// 'C', uses len and dec
		member @@Date	 		:= 68 	// 'D', 8 bytes
		member @@Logic   		:= 76  	// 'L', 1 byte
		member @@Memo    		:= 77  	// 'M', 4 or 10 bytes see Length
		member @@Number    		:= 78  	// 'N', uses len and dec
		member @@VOObject		:= 79  	// 'O'
		// Extended Non Clipper Types in numerical order
		member @@AutoIncrement	:= 43	// '+' = AutoInc, 4 bytes
		member @@Integer2		:= 50  	// '2'	2 byte int, autoInc
		member @@Integer4		:= 52  	// '4'	4 byte int, autoInc
		member @@Double8		:= 56	// '8'  Same as 'B'
		member @@ModTime		:= 61	// '=' = ModTime, 8 bytes 
		member @@TimeSpamp		:= 64   // '@' = Timestamp 8 bytes
		member @@Double			:= 66  	// 'B'	FOX Type, also '8'
		member @@Float			:= 70  	// 'F'	FOX Type, uses len and dec
		member @@Ole			:= 71	// 'G' = Ole 4 or 10 bytes
		member @@Integer		:= 73  	// 'I'	FOX Type , autoInc
		member @@Picture		:= 80  	// 'P'	FOX Type, 4 or 10 bytes
		member @@VarLength2		:= 81	// 'Q' = VarLenghth , between 1 and 255 
		member @@DateTime		:= 84  	// 'T'	FOX Type can be 4 or 8 bytes
		member @@VarLength1		:= 86	// 'V' = VarLength
		member @@Blob			:= 87	// 'W' = Blob 4 or 10 bytes
		member @@Currency		:= 89  	// 'Y'	8 byte FOX Type
		member @@Currency2		:= 90  	// 'Z'	8 byte Currency
		member @@RowVer			:= 94	// '^' = RowVer, 8 bytes  
	end	 enum
	
	enum DbfHeaderCodepage as byte
		member CP_DBF_DOS_OLD:=0
		member CP_DBF_DOS_US:=1
		member CP_DBF_DOS_INTL:=2
		member CP_DBF_WIN_ANSI:=3
		member CP_DBF_MAC_STANDARD:=4
		member CP_DBF_DOS_EEUROPEAN:=100
		member CP_DBF_DOS_RUSSIAN:=101
		member CP_DBF_DOS_NORDIC:=102
		member CP_DBF_DOS_ICELANDIC:=103
		member CP_DBF_DOS_KAMENICKY:=104
		member CP_DBF_DOS_MAZOVIA:=105
		member CP_DBF_DOS_GREEK:=106
		member CP_DBF_DOS_TURKISH:=107
		member CP_DBF_DOS_CANADIAN:=108
		member CP_DBF_WIN_CHINESE_1:=120
		member CP_DBF_WIN_KOREAN:=121
		member CP_DBF_WIN_CHINESE_2:=122
		member CP_DBF_WIN_JAPANESE:=123
		member CP_DBF_WIN_THAI:=124
		member CP_DBF_WIN_HEBREW:=125
		member CP_DBF_WIN_ARABIC:=126
		member CP_DBF_MAC_RUSSIAN:=150
		member CP_DBF_MAC_EEUROPEAN:=151
		member CP_DBF_MAC_GREEK:=152
		member CP_DBF_WIN_EEUROPEAN:=200
		member CP_DBF_WIN_RUSSIAN:=201
		member CP_DBF_WIN_TURKISH:=202
		member CP_DBF_WIN_GREEK:=203
	end	 enum
	
	enum RDDInfo
		member RDDI_ISDBF              :=   1   /* Does this RDD support DBFs? */
		member RDDI_CANPUTREC          :=   2   /* Can this RDD Put Records? */
		member RDDI_DELIMITER          :=   3   /* The field delimiter (as a string) */
		member RDDI_SEPARATOR          :=   4   /* The record separator (as a string) */
		
		member RDDI_TABLEEXT           :=   5   /* Default data file's file extension */
		member RDDI_MEMOEXT            :=   6   /* Default memo file's file extension */
		member RDDI_ORDBAGEXT          :=   7   /* Default multi tag index's file extension */
		member RDDI_ORDEREXT           :=   8   /* default single tag index's file extension */
		member RDDI_ORDSTRUCTEXT       :=   9   /* default single tag index's file extension */
		
		member RDDI_LOCAL              :=  10   /* Local file access? */
		member RDDI_REMOTE             :=  11   /* Remote table access? */
		member RDDI_CONNECTION         :=  12   /* Get/Set default connection */
		member RDDI_TABLETYPE          :=  13   /* Type of table file */
		member RDDI_MEMOTYPE           :=  14   /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
		member RDDI_LARGEFILE          :=  15   /* Is large file size (>=4GB) supported */
		member RDDI_LOCKSCHEME         :=  16   /* Locking scheme used by RDD */
		member RDDI_RECORDMAP          :=  17   /* Does RDD support record map functionality? */
		member RDDI_ENCRYPTION         :=  18   /* Does RDD support encryption */
		member RDDI_TRIGGER            :=  19   /* Get/Set default trigger function */
		member RDDI_AUTOLOCK           :=  20   /* automatic locking on update */
		
		/* index parameters */		   
		member RDDI_STRUCTORD          :=  21   /* Are structural indexes supported */
		member RDDI_STRICTREAD         :=  22   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
		member RDDI_STRICTSTRUCT       :=  23   /* Flag for strict structural order checking */
		member RDDI_OPTIMIZE           :=  24   /* Flag for whether to use query optimization */
		member RDDI_FORCEOPT           :=  25   /* Flag for forcing linear optimization */
		member RDDI_AUTOOPEN           :=  26   /* Flag for automatically opening structural indexes */
		member RDDI_AUTOORDER          :=  27   /* When a structural index is opened, the order to be set */
		member RDDI_AUTOSHARE          :=  28   /* When a network is detected, open the index shared, otherwise open exclusively */
		member RDDI_MULTITAG           :=  29   /* Does RDD support multi tag in index file */
		member RDDI_SORTRECNO          :=  30   /* Is record number part of key in sorting */
		member RDDI_MULTIKEY           :=  31   /* Does custom orders support repeated keys? */
		
		/* memo parameters */		   
		member RDDI_MEMOBLOCKSIZE      :=  32   /* Memo File's block size */
		member RDDI_MEMOVERSION        :=  33   /* sub version of memo file */
		member RDDI_MEMOGCTYPE         :=  34   /* type of garbage collector used by GC */
		member RDDI_MEMOREADLOCK       :=  35   /* use read lock in memo file access */
		member RDDI_MEMOREUSE          :=  36   /* reuse free space on write */
		member RDDI_BLOB_SUPPORT       :=  37   /* can support BLOB files directly */
		
		/* misc */					   
		member RDDI_PENDINGTRIGGER     :=  40   /* set pending trigger for next open operation */
		member RDDI_PENDINGPASSWORD    :=  41   /* set pending password for next open operation */
		member RDDI_PASSWORD           :=  42   /* Get/Set default password */
		member RDDI_LOCKRETRY          :=  43   /* Get/Set record and file lock timeout value */
		member RDDI_DIRTYREAD          :=  44   /* Get/Set index dirty read flag */
		member RDDI_INDEXPAGESIZE      :=  45   /* Get/Set default index page size */
		member RDDI_DECIMALS           :=  46   /* Get/Set default number of decimal places for numeric fields if it's undefined */
		member RDDI_SETHEADER          :=  47   /* DBF header updating modes */
		
		/* SQL */					   
		member RDDI_CONNECT            :=  61   /* connect to database */
		member RDDI_DISCONNECT         :=  62   /* disconnect from database */
		member RDDI_EXECUTE            :=  63   /* execute SQL statement */
		member RDDI_ERROR              :=  64   /* error number */
		member RDDI_ERRORNO            :=  65   /* error description */
		member RDDI_INSERTID           :=  66   /* last auto insert ID */
		member RDDI_AFFECTEDROWS       :=  67   /* number of affected rows after UPDATE */
		member RDDI_QUERY              :=  68   /* last executed query */
		
	end enum
	
end namespace
