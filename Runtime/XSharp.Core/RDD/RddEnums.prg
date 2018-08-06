//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

	/// <summary>This enum specifies the collation mode used to create and update index strings.</summary>
BEGIN NAMESPACE XSharp
	ENUM CollationMode
		MEMBER @@Windows
		MEMBER @@Clipper
		MEMBER @@Unicode
		MEMBER @@Ordinal
	END ENUM
END NAMESPACE


BEGIN NAMESPACE XSharp.RDD.Enums
	/// <summary>This enum specifies how files should be opened.</summary>
	ENUM AutoShareMode
		MEMBER NoChange		 := 0
		MEMBER @@Auto		 := 1
		MEMBER ForceExclusive := 2
	END ENUM
	
	/// <summary>This enum specifies the lock modes.</summary>
	ENUM DbLockMode
		MEMBER Lock
		MEMBER UnLock
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the RecordInfo method for the IRDD interface accepts.
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbRecordInfo." prefix.
	/// </note>
	///</summary>
	ENUM DbRecordInfo
		MEMBER DBRI_DELETED 	:= 1 
		MEMBER DBRI_LOCKED 		:= 2 	
		MEMBER DBRI_RECSIZE 	:= 3	
		MEMBER DBRI_RECNO 		:= 4	
		MEMBER DBRI_UPDATED 	:= 5	
		MEMBER DBRI_BUFFPTR 	:= 6 
		MEMBER DBRI_RAWRECORD   := 6
		// harbour
		MEMBER DBRI_ENCRYPTED	:= 7 
		MEMBER DBRI_RAWMEMOS	:= 8 
		MEMBER DBRI_RAWDATA		:= 9 
		MEMBER DBRI_USER 		:= 1000
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the FieldInfo method for the IRDD interface accepts.
	///</summary>
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbFieldInfo." prefix.
	/// </note>

	ENUM DbFieldInfo
		MEMBER DBS_NAME				:= 1
		MEMBER DBS_TYPE				:= 2
		MEMBER DBS_LEN				:= 3
		MEMBER DBS_DEC				:= 4
		MEMBER DBS_ALIAS			:= 5
		// harbour extensions
		MEMBER DBS_ISNULL			:= 11
		MEMBER DBS_COUNTER			:= 12
		MEMBER DBS_STEP				:= 13
		
		MEMBER DBS_BLOB_GET			:= 101
		MEMBER DBS_BLOB_TYPE		:= 102
		MEMBER DBS_BLOB_LEN			:= 103
		MEMBER DBS_BLOB_OFFSET		:= 104
		MEMBER DBS_BLOB_POINTER		:= 198
		MEMBER DBS_BLOB_DIRECT_TYPE	:= 222
		MEMBER DBS_BLOB_DIRECT_LEN	:= 223
		MEMBER DBS_STRUCT			:= 998
		MEMBER DBS_PROPERTIES		:= 999
		MEMBER DBS_USER				:= 1000
	END	 ENUM             
	
	/// <summary>This enum specifies the various values that the Info method for the IRDD interface accepts.
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbInfo." prefix.
	/// </note>
	///</summary>

	ENUM DbInfo
		/// <summary>Logical: Is the RDD DBF based ?</summary>
		MEMBER DBI_ISDBF 			:= 1				
		/// <summary>Logical: Does the RDD support the PutRec mechanism ?</summary>
		MEMBER DBI_CANPUTREC		:= 2				
		/// <summary>Numeric: Get header size of the file  ?</summary>
		MEMBER DBI_GETHEADERSIZE	:= 3				
		/// <summary>Date:    Last date RDD file updated  </summary> 
		MEMBER DBI_LASTUPDATE		:= 4				
		/// <summary>String:  Get default delimiter</summary> 
		MEMBER DBI_GETDELIMITER		:= 5				
		/// <summary>String:  Set default delimiter</summary> 
		MEMBER DBI_SETDELIMITER		:= 6				
		/// <summary>Numeric: Get record size </summary> 
		MEMBER DBI_GETRECSIZE		:= 7				
		/// <summary>Array:   Get array of locked records</summary> 
		MEMBER DBI_GETLOCKARRAY		:= 8				
		/// <summary>String:  Get table file extension</summary> 
		MEMBER DBI_TABLEEXT 		:= 9				
		/// <summary>Logic: is the file opened readonly</summary> 
		MEMBER DBI_READONLY 		:= 10				
		// 11-19 missing								
		/// <summary>Logic: Is there a file lock active? </summary> 
		MEMBER DBI_ISFLOCK 			:= 20				
		// 21 missing									
		/// <summary>Number: Number of child relations set</summary> 
		MEMBER DBI_CHILDCOUNT 		:= 22				
		/// <summary>IntPtr: The data file's handle</summary> 
		MEMBER DBI_FILEHANDLE 		:= 23
		/// <summary>String: Full path to data file</summary> 
		MEMBER DBI_FULLPATH			:= 24
		/// <summary>Logic: Is the file ansi encoded</summary> 
		MEMBER DBI_ISANSI 			:= 25
		/// <summary>Logic: Same as Bof()</summary> 
		MEMBER DBI_BOF 				:= 26
		/// <summary>Logic: Same as Eof()</summary> 
		MEMBER DBI_EOF 				:= 27
		/// <summary>String: Current Filter setting </summary> 
		MEMBER DBI_DBFILTER 		:= 28			
		/// <summary>Logic: Same as Found() </summary> 
		MEMBER DBI_FOUND 			:= 29				
		/// <summary>Number: Number of fields per record</summary> 
		MEMBER DBI_FCOUNT 			:= 30				
		/// <summary>Number: Number of record locks  </summary> 
		MEMBER DBI_LOCKCOUNT		:= 31			
		/// <summary>Logic: Is the buffer Valid</summary> 
		MEMBER DBI_VALIDBUFFER		:= 32		
		/// <summary>String: Alias</summary> 
		MEMBER DBI_ALIAS 			:= 33				
		/// <summary>Object: The ScopeInfo</summary> 
		MEMBER DBI_GETSCOPE 		:= 34				
		/// <summary>Number: Lock offset</summary> 
		MEMBER DBI_LOCKOFFSET		:= 35				
		/// <summary>Logic: is the file opened shared</summary> 
		MEMBER DBI_SHARED 			:= 36			
		/// <summary>String: Memo file extension</summary> 
		MEMBER DBI_MEMOEXT 			:= 37		
		/// <summary>Intptr: The memo file's handle</summary> 
		MEMBER DBI_MEMOHANDLE		:= 38			
		/// <summary>Intptr: The memo file's handle</summary> 
		MEMBER DBI_BLOB_HANDLE 		:= 38				
		/// <summary>Number: The memo block size</summary> 
		MEMBER DBI_MEMOBLOCKSIZE 	:= 39		
		MEMBER DBI_BLOB_INTEGRITY	:= 40				
		/// <summary>Number: The Windows Codepage</summary> 
		MEMBER DBI_CODEPAGE 		:= 41				
		/// <summary>Number: The DOS Codepage</summary> 
		MEMBER DBI_DOSCODEPAGE		:= 42				
		/// <summary></summary> 
		MEMBER DBI_BLOB_RECOVER 	:= 43
		/// <summary>Logic:  Use new index lock mechanism</summary> 
		MEMBER DBI_NEWINDEXLOCK 	:= 44
		/// <summary>Flag for avoiding RDD hierarchy and using a bigger buffer when indexing</summary> 
		MEMBER DBI_STRICTREAD  		:= 60				
		/// <summary>Flag for whether to use query optimization</summary> 
		MEMBER DBI_OPTIMIZE    		:= 61 				
		/// <summary>Flag for automatically opening structural indexes</summary> 
		MEMBER DBI_AUTOOPEN    		:= 62 				
		/// <summary>Should the order be set to the first index when a structural index is opened</summary> 
		MEMBER DBI_AUTOORDER   		:= 63 				
		/// <summary>When a network is detected, open the index shared, otherwise open exclusively</summary> 
		MEMBER DBI_AUTOSHARE   		:= 64 				
		/// <summary></summary> 
		MEMBER DBI_DB_VERSION 		:= 101
		/// <summary></summary> 
		MEMBER DBI_RDD_VERSION 		:= 102					
		/// <summary></summary> 
		MEMBER DBI_RDD_LIST 		:= 103
		/// <summary></summary> 
		MEMBER DBI_MEMOFIELD 		:= 104
		/// <summary></summary> 
		MEMBER DBI_VO_MACRO_SYNTAX 	:= 105
		/// <summary></summary> 
		MEMBER DBI_RDD_OBJECT		:= 106
		/// <summary></summary> 
		// 107 - 127 missing
		// Harbour extensions
		/// <summary>Harbour extension: Locking scheme used by RDD</summary> 
		MEMBER DBI_LOCKSCHEME          := 128 /* Locking scheme used by RDD */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_ISREADONLY          := 129 /* Was the file opened readonly? */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_ROLLBACK            := 130 /* Rollback changes made to current record */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_PASSWORD            := 131 /* Workarea password */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_ISENCRYPTED         := 132 /* The database is encrypted */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_MEMOTYPE            := 133 /* Type of MEMO file: DBT, SMT, FPT */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_SEPARATOR           := 134 /* The record separator (as a string) */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_MEMOVERSION         := 135 /* sub version of memo file */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_TABLETYPE           := 136 /* Type of table file */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_SCOPEDRELATION      := 137 /* Is given relation scoped */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_TRIGGER             := 138 /* Get/Set trigger function */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_OPENINFO            := 139 /* DBOPENINFO structure pointer */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_ENCRYPT             := 140 /* Encrypt table */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_DECRYPT             := 141 /* Decrypt table */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_MEMOPACK            := 142 /* Pack memo file */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_DIRTYREAD           := 143 /* Get/Set index dirty read flag */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_POSITIONED          := 144 /* Is cursor positioned to valid record */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_ISTEMPORARY         := 145 /* Is the table a temporary one? */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_LOCKTEST            := 146 /* record / file lock test */
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_CODEPAGE_HB         := 147 /* Codepage used also memberd by VO and Vulcan */ 
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_TRANSREC            := 148  /* Is it destination table of currently processed COPY TO or APPEND FROM operation? */ 
		/// <summary>Harbour extension: </summary> 
		MEMBER DBI_SETHEADER		   := 149	/* DBF header updating modes */ 
		/// <summary></summary> 
		/* Harbour RECORD MAP (RM) support */
		/// <summary>Harbour record map extension: has WA RDD record map support?</summary> 
		MEMBER DBI_RM_SUPPORTED        := 150 
		/// <summary>Harbour record map extension: create new empty work area record map</summary> 
		MEMBER DBI_RM_CREATE           := 151 
		/// <summary>Harbour record map extension: remove active work area record map</summary> 
		MEMBER DBI_RM_REMOVE           := 152 
		/// <summary>Harbour record map extension: remove all records from WA record map</summary> 
		MEMBER DBI_RM_CLEAR            := 153 
		/// <summary>Harbour record map extension: add all records to WA record map </summary> 
		MEMBER DBI_RM_FILL             := 154 
		/// <summary>Harbour record map extension: add record to work area record map </summary> 
		MEMBER DBI_RM_ADD              := 155 
		/// <summary>Harbour record map extension: remove record from work area record map</summary> 
		MEMBER DBI_RM_DROP             := 156 
		/// <summary>Harbour record map extension: test if record is set in WA record map</summary> 
		MEMBER DBI_RM_TEST             := 157 
		/// <summary>Harbour record map extension: number of records set in record map </summary> 
		MEMBER DBI_RM_COUNT            := 158 
		/// <summary>Harbour record map extension: get/set record map filter handle</summary> 
		MEMBER DBI_RM_HANDLE           := 159  
		// 160 - 169 missing
		/// <summary>if area represents result of a query, obtain expression of this query </summary> 
		MEMBER DBI_QUERY				:= 170 
		// 171 - 200 missing		
		/// <summary></summary> 
		MEMBER BLOB_INFO_HANDLE		:= 201
		/// <summary></summary> 
		MEMBER BLOB_FILE_RECOVER	:= 202
		/// <summary></summary> 
		MEMBER BLOB_FILE_INTEGRITY	:= 203
		/// <summary></summary> 
		MEMBER BLOB_OFFSET			:= 204
		/// <summary></summary> 
		MEMBER BLOB_POINTER			:= 205
		/// <summary></summary> 
		MEMBER BLOB_LEN				:= 206
		/// <summary></summary> 
		MEMBER BLOB_TYPE			:= 207
		/// <summary></summary> 
		MEMBER BLOB_EXPORT			:= 208
		/// <summary></summary> 
		MEMBER BLOB_ROOT_UNLOCK		:= 209
		/// <summary></summary> 
		MEMBER BLOB_ROOT_PUT		:= 210
		/// <summary></summary> 
		MEMBER BLOB_ROOT_GET		:= 211
		/// <summary></summary> 
		MEMBER BLOB_ROOT_LOCK		:= 212
		/// <summary></summary> 
		MEMBER BLOB_IMPORT			:= 213
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_PUT		:= 214
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_GET		:= 215
		/// <summary></summary> 
		MEMBER BLOB_GET				:= 216
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_EXPORT	:= 217
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_IMPORT	:= 218
		/// <summary></summary> 
		MEMBER BLOB_NMODE			:= 219
		/// <summary></summary> 
		MEMBER BLOB_EXPORT_APPEND	:= 220
		/// <summary></summary> 
		MEMBER BLOB_EXPORT_OVERWRITE:= 221
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_TYPE		:= 222
		/// <summary></summary> 
		MEMBER BLOB_DIRECT_LEN		:= 223
		/// <summary></summary> 
		MEMBER BLOB_USER			:= 2000
		
		// Clipmore functions
		/// <summary>Start of user defined DBI values</summary> 
		MEMBER DBI_USER 			:= 1000
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_AND 			:= 1001
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_CLEAR 		:= 1002
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_COUNT 		:= 1003
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_DESTROY 		:= 1004
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_EXFILTER 		:= 1005
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_GETFILTER 	:= 1006
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_HASMAYBE 		:= 1007
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_LEN 			:= 1008
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_MAYBEEVAL 	:= 1009
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_NEW 			:= 1010
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_NEWDUP 		:= 1011
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_NEWQUERY 		:= 1012
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_NEXTRECNO 	:= 1013
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_NOT 			:= 1014
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_OR 			:= 1015
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_PREVRECNO 	:= 1016
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_SET 			:= 1017
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_SETFILTER 	:= 1018
		/// <summary>Clipmore extension: </summary> 
		MEMBER DBI_RL_TEST 			:= 1019
		
		
		// advnatage
		MEMBER DBI_GET_ACE_TABLE_HANDLE  := 1110
		MEMBER DBI_GET_ACE_STMT_HANDLE   := 1111
		
		
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the OrderInfo method for the IRDD interface accepts.
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbOrderInfo." prefix.
	/// </note>
	///</summary>

	ENUM DbOrderInfo
		MEMBER DBOI_CONDITION 	:= 1     // String: The order's conditional expression     
		MEMBER DBOI_EXPRESSION 	:= 2 	// String: The order's key expression             
		MEMBER DBOI_POSITION 	:= 3  	// Number: The current key position in scope and filter  
		MEMBER DBOI_KEYNO	 	:= 3	// Alias
		MEMBER DBOI_KEYGOTO 	:= 3	// Alias
		MEMBER DBOI_RECNO 		:= 4  	// Number: The current key position disregarding filters 
		MEMBER DBOI_NAME 		:= 5   	// String: The name of the order                      
		MEMBER DBOI_NUMBER 		:= 6 	// Number: The numeric position in the list of orders
		MEMBER DBOI_BAGNAME 	:= 7 	// String: The name of the file containing this order
		MEMBER DBOI_INDEXNAME 	:= 7	// Alias
		MEMBER DBOI_BAGEXT 		:= 8    // String: The extension of the file containing this order
		MEMBER DBOI_INDEXEXT  	:= 8	// Alias
		MEMBER DBOI_ORDERCOUNT  := 9    // Number: The count of ORDERS contained in an index file or in total
		MEMBER DBOI_FILEHANDLE 	:= 10 	// IntPtr: The handle of the index
		MEMBER DBOI_ISCOND 		:= 11 	// Logic : Does the order have a FOR condition?
		MEMBER DBOI_ISDESC 		:= 12 	// Logic : Is the order DESCENDing? 
		MEMBER DBOI_UNIQUE 		:= 13 	// Logic : Does the order have the UNIQUE attribute?
		
		// 14-19 missign
		/* Clipper 5.3-level constants */
		MEMBER DBOI_FULLPATH 	:= 20   	// String: The full path to the index file (Bag)
		// 21-23
		MEMBER DBOI_KEYTYPE 	:= 24 	// The type of the order's key  
		MEMBER DBOI_KEYSIZE 	:= 25 	// Number: The length of the order's key
		MEMBER DBOI_KEYCOUNT 	:= 26 	// Number: The count of keys in scope and filter
		MEMBER DBOI_SETCODEBLOCK:= 27 	// Block : The codeblock that produces the key 
		MEMBER DBOI_KEYDEC 		:= 28 	// Number: The # of decimals in a numeric key 
		MEMBER DBOI_HPLOCKING 	:= 29 	// Logic : Using High Performance locking for this order?
		// 30-34
		MEMBER DBOI_LOCKOFFSET 	:= 35 	// Number: The offset used for logical locking 
		MEMBER DBOI_KEYADD 		:= 36  	// Logic: Custom Index: Was Key added successfully? 
		MEMBER DBOI_KEYDELETE 	:= 37 	// Logic: Custom Index: Was Key Deletion successful? 
		MEMBER DBOI_KEYVAL 		:= 38 	// Object: The value of the current key 
		MEMBER DBOI_SCOPETOP 	:= 39 	// Object: Get or Set the scope top    
		MEMBER DBOI_SCOPEBOTTOM := 40 	// Object: Get or Set the scope bottom
		MEMBER DBOI_SCOPETOPCLEAR := 41  	// None	 :
		MEMBER DBOI_SCOPEBOTTOMCLEAR:= 42 // None :
		// 43-44
		MEMBER DBOI_CUSTOM 		:= 45 // Logic: Is this a Custom Index?  
		MEMBER DBOI_SKIPUNIQUE 	:= 46 // Logic: Was a skip to adjacent unique Key successful?  
		// 47-49
		MEMBER DBOI_KEYSINCLUDED:= 50 	// Number: Number of keys in the index order
		MEMBER DBOI_KEYNORAW 	:= 51 // Number: The key number disregarding filters
		MEMBER DBOI_KEYGOTORAW 	:= 51	// Alias
		MEMBER DBOI_KEYCOUNTRAW := 52  // Number: The key count disregarding filter  
		/* Query Optimization */ 
		MEMBER DBOI_OPTLEVEL 	:= 53 // Number: Optimization level for current query
		
		// 54-59
		/* These shouldn't need an open table */
		MEMBER DBOI_STRICTREAD          := 60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
		MEMBER DBOI_OPTIMIZE            := 61  /* Flag for whether to use query optimization             */
		MEMBER DBOI_AUTOOPEN            := 62  /* Flag for automatically opening structural indexes      */
		MEMBER DBOI_AUTOORDER           := 63  /* When a structural index is opened, the order to be set */
		MEMBER DBOI_AUTOSHARE           := 64  /* When a network is detected, open the index shared, otherwise open exclusively   */ 

		// 65-99
		/* Harbour extensions */
		MEMBER DBOI_SKIPEVAL           := 100  /* skip while code block doesn't return TRUE */
		MEMBER DBOI_SKIPEVALBACK       := 101  /* skip backward while code block doesn't return TRUE */
		MEMBER DBOI_SKIPREGEX          := 102  /* skip while regular expression on index key doesn't return TRUE */
		MEMBER DBOI_SKIPREGEXBACK      := 103  /* skip backward while regular expression on index key doesn't return TRUE */
		MEMBER DBOI_SKIPWILD           := 104  /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
		MEMBER DBOI_SKIPWILDBACK       := 105  /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
		MEMBER DBOI_SCOPEEVAL          := 106  /* skip through index evaluating given C function */
		MEMBER DBOI_FINDREC            := 107  /* find given record in a Tag beginning from TOP */
		MEMBER DBOI_FINDRECCONT        := 108  /* find given record in a Tag beginning from current position */
		MEMBER DBOI_SCOPESET           := 109  /* set both scopes */
		MEMBER DBOI_SCOPECLEAR         := 110  /* clear both scopes */
		MEMBER DBOI_BAGCOUNT           := 111  /* number of open order bags */
		MEMBER DBOI_BAGNUMBER          := 112  /* bag position in bag list */
		MEMBER DBOI_BAGORDER           := 113  /* number of first order in a bag */
		MEMBER DBOI_ISMULTITAG         := 114  /* does RDD support multi tag in index file */
		MEMBER DBOI_ISSORTRECNO        := 115  /* is record number part of key in sorting */
		MEMBER DBOI_LARGEFILE          := 116  /* is large file size (>=4GB) supported */
		MEMBER DBOI_TEMPLATE           := 117  /* order with free user keys */
		MEMBER DBOI_MULTIKEY           := 118  /* custom order with multikeys */
		MEMBER DBOI_CHGONLY            := 119  /* update only existing keys */
		MEMBER DBOI_PARTIAL            := 120  /* is index partially updated */
		MEMBER DBOI_SHARED             := 121  /* is index open in shared mode */
		MEMBER DBOI_ISREADONLY         := 122  /* is index open in readonly mode */
		MEMBER DBOI_READLOCK           := 123  /* get/set index read lock */
		MEMBER DBOI_WRITELOCK          := 124  /* get/set index write lock */
		MEMBER DBOI_UPDATECOUNTER      := 125  /* get/set update index counter */
		MEMBER DBOI_EVALSTEP           := 126  /* eval step (EVERY) used in index command */
		MEMBER DBOI_ISREINDEX          := 127  /* Is reindex in process */
		MEMBER DBOI_I_BAGNAME          := 128  /* created index name */
		MEMBER DBOI_I_TAGNAME          := 129  /* created tag name */
		MEMBER DBOI_RELKEYPOS          := 130  /* get/set relative key position (in range 0 - 1) */
		MEMBER DBOI_USECURRENT         := 131  /* get/set "use current index" flag */
		MEMBER DBOI_INDEXTYPE          := 132  /* current index type */
		MEMBER DBOI_RESETPOS           := 133  /* rest logical and raw positions */
		MEMBER DBOI_INDEXPAGESIZE      := 134  /* get index page size */
		
		MEMBER DBOI_USER 				:= 1000 
		// Advantage
		MEMBER DBOI_AXS_PERCENT_INDEXED  := 1805
		MEMBER DBOI_GET_ACE_INDEX_HANDLE := 1806
		
		
		
	END ENUM
	
	/// <summary>This enum specifies the various field types that can appear in DBF files.</summary>
	ENUM DbFieldType AS BYTE
		MEMBER @@Unknown		:= 0	//                                  
		MEMBER @@Character 		:= 67 	// 'C', uses len and dec
		MEMBER @@Date	 		:= 68 	// 'D', 8 bytes
		MEMBER @@Logic   		:= 76  	// 'L', 1 byte
		MEMBER @@Memo    		:= 77  	// 'M', 4 or 10 bytes see Length
		MEMBER @@Number    		:= 78  	// 'N', uses len and dec
		MEMBER @@VOObject		:= 79  	// 'O'
		// Extended Non Clipper Types in numerical order
		MEMBER @@AutoIncrement	:= 43	// '+' = AutoInc, 4 bytes
		MEMBER @@Integer2		:= 50  	// '2'	2 byte int, autoInc
		MEMBER @@Integer4		:= 52  	// '4'	4 byte int, autoInc
		MEMBER @@Double8		:= 56	// '8'  Same as 'B'
		MEMBER @@ModTime		:= 61	// '=' = ModTime, 8 bytes 
		MEMBER @@TimeSpamp		:= 64   // '@' = Timestamp 8 bytes
		MEMBER @@Double			:= 66  	// 'B'	FOX Type, also '8'
		MEMBER @@Float			:= 70  	// 'F'	FOX Type, uses len and dec
		MEMBER @@Ole			:= 71	// 'G' = Ole 4 or 10 bytes
		MEMBER @@Integer		:= 73  	// 'I'	FOX Type , autoInc
		MEMBER @@Picture		:= 80  	// 'P'	FOX Type, 4 or 10 bytes
		MEMBER @@VarLength2		:= 81	// 'Q' = VarLenghth , between 1 and 255 
		MEMBER @@DateTime		:= 84  	// 'T'	FOX Type can be 4 or 8 bytes
		MEMBER @@VarLength1		:= 86	// 'V' = VarLength
		MEMBER @@Blob			:= 87	// 'W' = Blob 4 or 10 bytes
		MEMBER @@Currency		:= 89  	// 'Y'	8 byte FOX Type
		MEMBER @@Currency2		:= 90  	// 'Z'	8 byte Currency
		MEMBER @@RowVer			:= 94	// '^' = RowVer, 8 bytes  
	END	 ENUM
	
	/// <summary>This enum specifies the various code pages that can appear in DBF files.</summary>
	ENUM DbfHeaderCodepage AS BYTE
		MEMBER CP_DBF_DOS_OLD:=0
		MEMBER CP_DBF_DOS_US:=1
		MEMBER CP_DBF_DOS_INTL:=2
		MEMBER CP_DBF_WIN_ANSI:=3
		MEMBER CP_DBF_MAC_STANDARD:=4
		MEMBER CP_DBF_DOS_EEUROPEAN:=100
		MEMBER CP_DBF_DOS_RUSSIAN:=101
		MEMBER CP_DBF_DOS_NORDIC:=102
		MEMBER CP_DBF_DOS_ICELANDIC:=103
		MEMBER CP_DBF_DOS_KAMENICKY:=104
		MEMBER CP_DBF_DOS_MAZOVIA:=105
		MEMBER CP_DBF_DOS_GREEK:=106
		MEMBER CP_DBF_DOS_TURKISH:=107
		MEMBER CP_DBF_DOS_CANADIAN:=108
		MEMBER CP_DBF_WIN_CHINESE_1:=120
		MEMBER CP_DBF_WIN_KOREAN:=121
		MEMBER CP_DBF_WIN_CHINESE_2:=122
		MEMBER CP_DBF_WIN_JAPANESE:=123
		MEMBER CP_DBF_WIN_THAI:=124
		MEMBER CP_DBF_WIN_HEBREW:=125
		MEMBER CP_DBF_WIN_ARABIC:=126
		MEMBER CP_DBF_MAC_RUSSIAN:=150
		MEMBER CP_DBF_MAC_EEUROPEAN:=151
		MEMBER CP_DBF_MAC_GREEK:=152
		MEMBER CP_DBF_WIN_EEUROPEAN:=200
		MEMBER CP_DBF_WIN_RUSSIAN:=201
		MEMBER CP_DBF_WIN_TURKISH:=202
		MEMBER CP_DBF_WIN_GREEK:=203
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the RDDInfo method for the IRDD interface accepts.
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "RDDInfo." prefix.
	/// </note>
	///</summary>

	ENUM RDDInfo
		MEMBER RDDI_ISDBF              :=   1   /* Does this RDD support DBFs? */
		MEMBER RDDI_CANPUTREC          :=   2   /* Can this RDD Put Records? */
		MEMBER RDDI_DELIMITER          :=   3   /* The field delimiter (as a string) */
		MEMBER RDDI_SEPARATOR          :=   4   /* The record separator (as a string) */
		
		MEMBER RDDI_TABLEEXT           :=   5   /* Default data file's file extension */
		MEMBER RDDI_MEMOEXT            :=   6   /* Default memo file's file extension */
		MEMBER RDDI_ORDBAGEXT          :=   7   /* Default multi tag index's file extension */
		MEMBER RDDI_ORDEREXT           :=   8   /* default single tag index's file extension */
		MEMBER RDDI_ORDSTRUCTEXT       :=   9   /* default single tag index's file extension */
		
		MEMBER RDDI_LOCAL              :=  10   /* Local file access? */
		MEMBER RDDI_REMOTE             :=  11   /* Remote table access? */
		MEMBER RDDI_CONNECTION         :=  12   /* Get/Set default connection */
		MEMBER RDDI_TABLETYPE          :=  13   /* Type of table file */
		MEMBER RDDI_MEMOTYPE           :=  14   /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
		MEMBER RDDI_LARGEFILE          :=  15   /* Is large file size (>=4GB) supported */
		MEMBER RDDI_LOCKSCHEME         :=  16   /* Locking scheme used by RDD */
		MEMBER RDDI_RECORDMAP          :=  17   /* Does RDD support record map functionality? */
		MEMBER RDDI_ENCRYPTION         :=  18   /* Does RDD support encryption */
		MEMBER RDDI_TRIGGER            :=  19   /* Get/Set default trigger function */
		MEMBER RDDI_AUTOLOCK           :=  20   /* automatic locking on update */
		
		/* index parameters */		   
		MEMBER RDDI_STRUCTORD          :=  21   /* Are structural indexes supported */
		MEMBER RDDI_STRICTREAD         :=  22   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
		MEMBER RDDI_STRICTSTRUCT       :=  23   /* Flag for strict structural order checking */
		MEMBER RDDI_OPTIMIZE           :=  24   /* Flag for whether to use query optimization */
		MEMBER RDDI_FORCEOPT           :=  25   /* Flag for forcing linear optimization */
		MEMBER RDDI_AUTOOPEN           :=  26   /* Flag for automatically opening structural indexes */
		MEMBER RDDI_AUTOORDER          :=  27   /* When a structural index is opened, the order to be set */
		MEMBER RDDI_AUTOSHARE          :=  28   /* When a network is detected, open the index shared, otherwise open exclusively */
		MEMBER RDDI_MULTITAG           :=  29   /* Does RDD support multi tag in index file */
		MEMBER RDDI_SORTRECNO          :=  30   /* Is record number part of key in sorting */
		MEMBER RDDI_MULTIKEY           :=  31   /* Does custom orders support repeated keys? */
		
		/* memo parameters */		   
		MEMBER RDDI_MEMOBLOCKSIZE      :=  32   /* Memo File's block size */
		MEMBER RDDI_MEMOVERSION        :=  33   /* sub version of memo file */
		MEMBER RDDI_MEMOGCTYPE         :=  34   /* type of garbage collector used by GC */
		MEMBER RDDI_MEMOREADLOCK       :=  35   /* use read lock in memo file access */
		MEMBER RDDI_MEMOREUSE          :=  36   /* reuse free space on write */
		MEMBER RDDI_BLOB_SUPPORT       :=  37   /* can support BLOB files directly */
		
		/* misc */					   
		MEMBER RDDI_PENDINGTRIGGER     :=  40   /* set pending trigger for next open operation */
		MEMBER RDDI_PENDINGPASSWORD    :=  41   /* set pending password for next open operation */
		MEMBER RDDI_PASSWORD           :=  42   /* Get/Set default password */
		MEMBER RDDI_LOCKRETRY          :=  43   /* Get/Set record and file lock timeout value */
		MEMBER RDDI_DIRTYREAD          :=  44   /* Get/Set index dirty read flag */
		MEMBER RDDI_INDEXPAGESIZE      :=  45   /* Get/Set default index page size */
		MEMBER RDDI_DECIMALS           :=  46   /* Get/Set default number of decimal places for numeric fields if it's undefined */
		MEMBER RDDI_SETHEADER          :=  47   /* DBF header updating modes */
		
		/* SQL */					   
		MEMBER RDDI_CONNECT            :=  61   /* connect to database */
		MEMBER RDDI_DISCONNECT         :=  62   /* disconnect from database */
		MEMBER RDDI_EXECUTE            :=  63   /* execute SQL statement */
		MEMBER RDDI_ERROR              :=  64   /* error number */
		MEMBER RDDI_ERRORNO            :=  65   /* error description */
		MEMBER RDDI_INSERTID           :=  66   /* last auto insert ID */
		MEMBER RDDI_AFFECTEDROWS       :=  67   /* number of affected rows after UPDATE */
		MEMBER RDDI_QUERY              :=  68   /* last executed query */
		
	END ENUM
	
END NAMESPACE
