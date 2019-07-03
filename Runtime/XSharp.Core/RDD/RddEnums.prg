//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

	
BEGIN NAMESPACE XSharp
    /// <summary>This enum specifies the collation mode used to create and update index strings.</summary>
	ENUM CollationMode
		MEMBER Windows
		MEMBER Clipper
		MEMBER Unicode
		MEMBER Ordinal
	END ENUM
END NAMESPACE


BEGIN NAMESPACE XSharp.RDD.Enums
	/// <summary>This enum specifies how files should be opened.</summary>
	ENUM AutoShareMode
		MEMBER NoChange		 := 0
		MEMBER Auto		 := 1
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
        /// <summary>Gets the deleted flag for the record.</summary>
		MEMBER DBRI_DELETED 	:= 1
        /// <summary>Gets the locked state for the record. You may specify a specific record.</summary>
		MEMBER DBRI_LOCKED 		:= 2
        /// <summary>Gets the record size. You may specify a specific record.</summary>
		MEMBER DBRI_RECSIZE 	:= 3
        /// <summary>Gets the record number.</summary>
		MEMBER DBRI_RECNO 		:= 4
        /// <summary>Gets/Sets a boolean indicating if the current record is updated.</summary>
		MEMBER DBRI_UPDATED 	:= 5
        /// <summary>Gets the buffer used by the RDD system for the record data. This is an array of bytes in X#.</summary>
		MEMBER DBRI_BUFFPTR 	:= 6 
		// harbour extensions
        /// <summary>Harbour: Is the current record encrypted?</summary>
		MEMBER DBRI_ENCRYPTED	:= 7
        /// <summary>Harbour extension: Return all the memos in the current record as one string.</summary>
		MEMBER DBRI_RAWMEMOS	:= 8
        /// <summary>Harbour extension: Return the current record and all the memos in the current record as one string.</summary>
		MEMBER DBRI_RAWDATA		:= 9
        /// <summary>Harbour extension: Return the current record as one string.</summary>
		MEMBER DBRI_RAWRECORD   := 10
        /// <summary>Offset of user defined values.</summary>
		MEMBER DBRI_USER 		:= 1000
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the FieldInfo method for the IRDD interface accepts.
	///</summary>
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbFieldInfo." prefix.
	/// </note>

	ENUM DbFieldInfo
        /// <summary>Returns the name of the field.</summary>
		MEMBER DBS_NAME				:= 1
        /// <summary>Returns the data type of the field. This is a single character string such as 'C' or 'N'.</summary>
		MEMBER DBS_TYPE				:= 2
        /// <summary>Returns the length of the field.</summary>
		MEMBER DBS_LEN				:= 3
        /// <summary> Returns the number of decimal places for the field. </summary>
		MEMBER DBS_DEC				:= 4
        /// <summary>Returns and optionally changes an alternate name (or alias) by which a field can be referenced (by default, same as DBS_NAME).  </summary>
		MEMBER DBS_ALIAS			:= 5
        /// <summary>Harbour extension: Returns the flag that indicates if a field is Nullable.</summary>
		MEMBER DBS_ISNULL			:= 11
        /// <summary>Harbour extension: Returns the next available value for autoincrement fields.</summary>
		MEMBER DBS_COUNTER			:= 12
        /// <summary>Harbour extension: Returns the step value for autoincrement fields.</summary>
		MEMBER DBS_STEP				:= 13

        /// <summary>Gets a BLOB value.</summary>
		MEMBER DBS_BLOB_GET			:= 101
        /// <summary>Unlike memo fields maintained in .DBT files, BLOB files allow you to store many different types of data in memo fields. This returns type type of the BLOB as a single character string.</summary>
		MEMBER DBS_BLOB_TYPE		:= 102
        /// <summary>Returns the length of the BLOB data in a memo field as an unsigned long integer.</summary>
		MEMBER DBS_BLOB_LEN			:= 103
        /// <summary></summary>
		MEMBER DBS_BLOB_OFFSET		:= 104
        /// <summary>Returns a numeric pointer to the BLOB data associated with a memo field.</summary>
		MEMBER DBS_BLOB_POINTER		:= 198
        /// <summary>Returns the type of data in a BLOB as an unsigned long integer, without referencing a particular memo field.
        /// With this constant, you must specify the BLOB using a numeric pointer obtained from BLOBDirectPut(), BLOBDirectImport(),
        /// or DBFieldInfo(DBS_BLOB_POINTER, &lt;nFieldPos&gt;).
        ///</summary>
		MEMBER DBS_BLOB_DIRECT_TYPE	:= 222
        /// <summary>Returns the length of data in a BLOB as an unsigned long integer, without referencing a particular memo field.
        /// With this constant, you must specify the BLOB using a numeric pointer obtained from BLOBDirectPut(), BLOBDirectImport(),
        /// or DBFieldInfo(DBS_BLOB_POINTER, &lt;nFieldPos&gt;).
        ///</summary>
		MEMBER DBS_BLOB_DIRECT_LEN	:= 223
        /// <summary></summary>
		MEMBER DBS_STRUCT			:= 998
        /// <summary>Returns the number of properties defined for a field.</summary>
		MEMBER DBS_PROPERTIES		:= 999
        /// <summary>Start of user defined FieldInfo values.</summary>
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
		/// <summary>Returns RDD Object.</summary> 
		MEMBER DBI_RDD_OBJECT		:= 106
		/// <summary></summary> 
		// 107 - 127 missing
		// Harbour extensions
		/// <summary>Harbour extension: Locking scheme used by RDD</summary> 
		MEMBER DBI_LOCKSCHEME          := 128 
		/// <summary>Harbour extension: Was the file opened readonly?</summary> 
		MEMBER DBI_ISREADONLY          := 129 
		/// <summary>Harbour extension: Rollback changes made to current record</summary> 
		MEMBER DBI_ROLLBACK            := 130 
		/// <summary>Harbour extension: orkarea password </summary> 
		MEMBER DBI_PASSWORD            := 131 /* W*/
		/// <summary>Harbour extension: The database is encrypted</summary> 
		MEMBER DBI_ISENCRYPTED         := 132 
		/// <summary>Harbour extension: Type of MEMO file: DBT, SMT, FPT</summary> 
		MEMBER DBI_MEMOTYPE            := 133 
		/// <summary>Harbour extension: The record separator (as a string)</summary> 
		MEMBER DBI_SEPARATOR           := 134 
		/// <summary>Harbour extension: sub version of memo file</summary> 
		MEMBER DBI_MEMOVERSION         := 135 
		/// <summary>Harbour extension: Type of table file</summary> 
		MEMBER DBI_TABLETYPE           := 136 
		/// <summary>Harbour extension: Is given relation scoped</summary> 
		MEMBER DBI_SCOPEDRELATION      := 137 
		/// <summary>Harbour extension: Get/Set trigger function</summary> 
		MEMBER DBI_TRIGGER             := 138 
		/// <summary>Harbour extension: DBOPENINFO structure pointer</summary> 
		MEMBER DBI_OPENINFO            := 139 
		/// <summary>Harbour extension: Encrypt table</summary> 
		MEMBER DBI_ENCRYPT             := 140 
		/// <summary>Harbour extension: Decrypt table</summary> 
		MEMBER DBI_DECRYPT             := 141 
		/// <summary>Harbour extension: Pack memo file </summary> 
		MEMBER DBI_MEMOPACK            := 142 
		/// <summary>Harbour extension: Get/Set index dirty read flag</summary> 
		MEMBER DBI_DIRTYREAD           := 143 
		/// <summary>Harbour extension: Is cursor positioned to valid record</summary> 
		MEMBER DBI_POSITIONED          := 144 
		/// <summary>Harbour extension: Is the table a temporary one?</summary> 
		MEMBER DBI_ISTEMPORARY         := 145 
		/// <summary>Harbour extension: record / file lock test</summary> 
		MEMBER DBI_LOCKTEST            := 146 
		/// <summary>Harbour extension: Codepage used also memberd by VO and Vulcan</summary> 
		MEMBER DBI_CODEPAGE_HB         := 147 
		/// <summary>Harbour extension: Is it destination table of currently processed COPY TO or APPEND FROM operation?</summary> 
		MEMBER DBI_TRANSREC            := 148  
		/// <summary>Harbour extension: DBF header updating modes</summary> 
		MEMBER DBI_SETHEADER		   := 149	
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
		
		
		// advantage
		MEMBER DBI_GET_ACE_TABLE_HANDLE  := 1110
		MEMBER DBI_GET_ACE_STMT_HANDLE   := 1111
		
		
	END	 ENUM
	
	/// <summary>This enum specifies the various values that the OrderInfo method for the IRDD interface accepts.
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "DbOrderInfo." prefix.
	/// </note>
	///</summary>
    /// <seealso cref='M:XSharp.RT.Functions.DbOrderInfo(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)' >DbOrderInfo()</seealso>
    /// <remarks>The types in the list of possible value are the types of the return value when you call DbOrderInfo() with this enum value.</ermarks>
	ENUM DbOrder_Info
        // These number match the defines for Vulcan
        // there are some differences between the various dialects unfortunately
        /// <summary>String: The order's conditional expression</summary>
		MEMBER DBOI_CONDITION 	:= 1     
        /// <summary>String: The order's key expression</summary>
		MEMBER DBOI_EXPRESSION 	:= 2 	
        /// <summary>Number: The current key position in scope and filter</summary>
		MEMBER DBOI_POSITION 	:= 3  	
        /// <summary>Alias for DBOI_POSITION</summary>
		MEMBER DBOI_KEYNO	 	:= 3	
        /// <summary>Number: The current key position disregarding filters</summary>
		MEMBER DBOI_RECNO 		:= 4  	
        /// <summary>String: The name of the order</summary>
		MEMBER DBOI_NAME 		:= 5   	
        /// <summary>Number: The numeric position in the list of orders</summary>
		MEMBER DBOI_NUMBER 		:= 6 	
        /// <summary>String: The name of the file containing this order</summary>
		MEMBER DBOI_BAGNAME 	:= 7 	
        /// <summary>Alias for DBOI_BAGNAME</summary>
		MEMBER DBOI_INDEXNAME 	:= 7	
        /// <summary>String: The extension of the file containing this order</summary>
		MEMBER DBOI_BAGEXT 		:= 8    
        /// <summary>Alias for DBOI_BAGEXT</summary>
		MEMBER DBOI_INDEXEXT  	:= 8	
		
		// 14-19 missing
        /// <summary>String: The full path to the index file (Bag)</summary>
		MEMBER DBOI_FULLPATH 	:= 20   
        /// <summary>IntPtr: The handle of the index</summary>
		MEMBER DBOI_FILEHANDLE 	:= 21 	
        /// <summary>Logic : Is the order DESCENDing? </summary>
		MEMBER DBOI_ISDESC 		:= 22 	
        /// <summary>Logic : Does the order have a FOR condition?</summary>
		MEMBER DBOI_ISCOND 		:= 23 	
        /// <summary>The type of the order's key  (usualType value)</summary>
		MEMBER DBOI_KEYTYPE 	:= 24 	
        /// <summary>Number: The length of the order's key</summary>
		MEMBER DBOI_KEYSIZE 	:= 25 	
        /// <summary>Number: The count of keys in scope and filter</summary>
		MEMBER DBOI_KEYCOUNT 	:= 26 	
        /// <summary>Block : The codeblock that produces the key </summary>
		MEMBER DBOI_SETCODEBLOCK:= 27 	
        /// <summary>Number: The # of decimals in a numeric key </summary>
		MEMBER DBOI_KEYDEC 		:= 28 	
        /// <summary>Logic : Using High Performance locking for this order?</summary>
		MEMBER DBOI_HPLOCKING 	:= 29 	
		// 30-34 missing
        /// <summary>Number: The offset used for logical locking </summary>
		MEMBER DBOI_LOCKOFFSET 	:= 35 	
        /// <summary>Logic: Custom Index: Add key  </summary>
		MEMBER DBOI_KEYADD 		:= 36  	
        /// <summary>Logic: Custom Index: Delete key </summary>
		MEMBER DBOI_KEYDELETE 	:= 37 	
        /// <summary>Object: The value of the current key </summary>
		MEMBER DBOI_KEYVAL 		:= 38 	
        /// <summary>Object: Get or Set the scope top    </summary>
		MEMBER DBOI_SCOPETOP 	:= 39 	
        /// <summary>Object: Get or Set the scope bottom</summary>
		MEMBER DBOI_SCOPEBOTTOM := 40 	
        /// <summary>Void: Clear top scope</summary>
		MEMBER DBOI_SCOPETOPCLEAR := 41  	
        /// <summary>Void: Clear Bottom scope</summary>
		MEMBER DBOI_SCOPEBOTTOMCLEAR:= 42 
        /// <summary>Logic : Does the order have the UNIQUE attribute?</summary>
		MEMBER DBOI_UNIQUE 		:= 43 	
        /// <summary>Number: The count of ORDERS contained in an index file or in total</summary>
		MEMBER DBOI_ORDERCOUNT  := 44    
        /// <summary>Logic: Is this a Custom Index?  </summary>
		MEMBER DBOI_CUSTOM 		:= 45 
        /// <summary>Logic: Was a skip to adjacent unique Key successful?  </summary>
		MEMBER DBOI_SKIPUNIQUE 	:= 46 
        /// <summary></summary>
		MEMBER DBOI_KEYGOTO 	:= 47	
        /// <summary>Number: Number of keys in the index order</summary>
		MEMBER DBOI_KEYSINCLUDED:= 48  
        /// <summary>Number: The key number disregarding filters</summary>
		MEMBER DBOI_KEYNORAW 	:= 49  
        /// <summary>Number: Optimization level for current query</summary>
		MEMBER DBOI_OPTLEVEL 	:= 50 
        /// <summary>Number: The key count disregarding filter  </summary>
		MEMBER DBOI_KEYCOUNTRAW := 51  

		// 54-59
		/* These shouldn't need an open table */
		MEMBER DBOI_STRICTREAD   := 60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
		MEMBER DBOI_OPTIMIZE     := 61  /* Flag for whether to use query optimization             */
		MEMBER DBOI_AUTOOPEN     := 62  /* Flag for automatically opening structural indexes      */
		MEMBER DBOI_AUTOORDER    := 63  /* When a structural index is opened, the order to be set */
		MEMBER DBOI_AUTOSHARE    := 64  /* When a network is detected, open the index shared, otherwise open exclusively   */ 

        MEMBER DBOI_LOCK_ALL    := 100  //
        MEMBER DBOI_LOCK_FAIL   := 101 // 
        MEMBER DBOI_HPLOCK_GATE := 102 // 




		// 65-99
		/* Harbour extensions */
		MEMBER DBOI_SKIPEVAL           := 200  /* skip while code block doesn't return TRUE */
		MEMBER DBOI_SKIPEVALBACK       := 201  /* skip backward while code block doesn't return TRUE */
		MEMBER DBOI_SKIPREGEX          := 202  /* skip while regular expression on index key doesn't return TRUE */
		MEMBER DBOI_SKIPREGEXBACK      := 203  /* skip backward while regular expression on index key doesn't return TRUE */
		MEMBER DBOI_SKIPWILD           := 204  /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
		MEMBER DBOI_SKIPWILDBACK       := 205  /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
		MEMBER DBOI_SCOPEEVAL          := 206  /* skip through index evaluating given C function */
		MEMBER DBOI_FINDREC            := 207  /* find given record in a Tag beginning from TOP */
		MEMBER DBOI_FINDRECCONT        := 208  /* find given record in a Tag beginning from current position */
		MEMBER DBOI_SCOPESET           := 209  /* set both scopes */
		MEMBER DBOI_SCOPECLEAR         := 210  /* clear both scopes */
		MEMBER DBOI_BAGCOUNT           := 211  /* number of open order bags */
		MEMBER DBOI_BAGNUMBER          := 212  /* bag position in bag list */
		MEMBER DBOI_BAGORDER           := 213  /* number of first order in a bag */
		MEMBER DBOI_ISMULTITAG         := 214  /* does RDD support multi tag in index file */
		MEMBER DBOI_ISSORTRECNO        := 215  /* is record number part of key in sorting */
		MEMBER DBOI_LARGEFILE          := 216  /* is large file size (>=4GB) supported */
		MEMBER DBOI_TEMPLATE           := 217  /* order with free user keys */
		MEMBER DBOI_MULTIKEY           := 218  /* custom order with multikeys */
		MEMBER DBOI_CHGONLY            := 219  /* update only existing keys */
		MEMBER DBOI_PARTIAL            := 220  /* is index partially updated */
		MEMBER DBOI_SHARED             := 221  /* is index open in shared mode */
		MEMBER DBOI_ISREADONLY         := 222  /* is index open in readonly mode */
		MEMBER DBOI_READLOCK           := 223  /* get/set index read lock */
		MEMBER DBOI_WRITELOCK          := 224  /* get/set index write lock */
		MEMBER DBOI_UPDATECOUNTER      := 225  /* get/set update index counter */
		MEMBER DBOI_EVALSTEP           := 226  /* eval step (EVERY) used in index command */
		MEMBER DBOI_ISREINDEX          := 227  /* Is reindex in process */
		MEMBER DBOI_I_BAGNAME          := 228  /* created index name */
		MEMBER DBOI_I_TAGNAME          := 229  /* created tag name */
		MEMBER DBOI_RELKEYPOS          := 230  /* get/set relative key position (in range 0 - 1) */
		MEMBER DBOI_USECURRENT         := 231  /* get/set "use current index" flag */
		MEMBER DBOI_INDEXTYPE          := 232  /* current index type */
		MEMBER DBOI_RESETPOS           := 233  /* rest logical and raw positions */
		MEMBER DBOI_INDEXPAGESIZE      := 234  /* get index page size */
		
		MEMBER DBOI_USER 				:= 1000 
		// Advantage
		MEMBER DBOI_AXS_PERCENT_INDEXED  := 1805
		MEMBER DBOI_GET_ACE_INDEX_HANDLE := 1806
		
		
		
	END ENUM

    /// <summary>DBF Field flags.</summary>                            
    [Flags];
    ENUM DBFFieldFlags AS BYTE
        MEMBER None             := 0x00
        MEMBER System           := 0x01
        MEMBER Nullable         := 0x02
        MEMBER Binary           := 0x04
        MEMBER AutoIncrement    := 0x08
        // Harbour additions
        MEMBER Compressed       := 0x10
        MEMBER Encrypted        := 0x20
        MEMBER Unicode          := 0x40
        
    END ENUM

	/// <summary>This enum specifies the various field types that can appear in DBF files.</summary>
	ENUM DbFieldType AS BYTE
		MEMBER Unknown		:= 0	
        /// <summary>'C', uses len and dec</summary>
		MEMBER Character 		:= 67 	 
        /// <summary>'D', 8 bytes</summary>
		MEMBER Date	 		:= 68 	 
        /// <summary>'L', 1 byte</summary>
		MEMBER Logic   		:= 76  	 
        /// <summary>'M', 4 or 10 bytes see Length</summary>
		MEMBER Memo    		:= 77  	 
        /// <summary>'N', uses len and dec</summary>
		MEMBER Number    		:= 78  	 
        /// <summary>'O', is anybody using this ?</summary>
		MEMBER VOObject		:= 79  	
		// FoxPro types in 'Name' order

        /// <summary>'W' = Blob 4 or 10 bytes</summary>
		MEMBER Blob			:= 87
        //MEMBER Character 		:= 67 	 
        /// <summary> 'Y'	8 byte FOX Type</summary>
		MEMBER Currency		:= 89
        // MEMBER Date	 		:= 68 	 
        /// <summary>'B'	FOX Type, also '8'</summary>
		MEMBER Double		:= 66  	 
        /// <summary>'T'	FOX Type can be 4 or 8 bytes</summary>
		MEMBER DateTime		:= 84  	 
        /// <summary>'F'	FOX Type, uses len and dec</summary>
		MEMBER Float		:= 70  	 
        /// <summary>'G' = Ole 4 or 10 bytes</summary>
		MEMBER General		:= 71	 
        /// <summary>'I'	FOX Type , autoInc</summary>
		MEMBER Integer		:= 73  	 
        /// <summary>'P'	FOX Type, 4 or 10 bytes</summary>
        // MEMBER Logic   		:= 76
        // MEMBER Number    		:= 78  	 
		MEMBER Picture		:= 80  	 
        /// <summary>'Q' = VarBinary , between 1 and 255 </summary>
		MEMBER VarBinary		:= 81	 
        /// <summary>'V' = Any/summary>
		MEMBER VarChar      := 86	 

        /// <summary>'0' = Null Flags
        MEMBER NullFlags        := 48


        // other types for Harbour will be supported later
        /*
        /// <summary>'+' = AutoInc, 4 bytes</summary>
		MEMBER AutoIncrement	:= 43	 
        /// <summary>'2'	2 byte int, autoInc</summary>
		MEMBER Integer2		:= 50  	 
        /// <summary>'4'	4 byte int, autoInc</summary>
		MEMBER Integer4		:= 52  	 
        /// <summary>'8'  Same as 'B'</summary>
		MEMBER Double8		:= 56	 
        /// <summary>'=' = ModTime, 8 bytes </summary>
		MEMBER ModTime		:= 61	 
        /// <summary>'@' = Timestamp 8 bytes</summary>
		MEMBER TimeStamp		:= 64    
        /// <summary>'Z'	8 byte Currency</summary>
		MEMBER CurrencyDouble	:= 90  	 
        /// <summary>'^' = RowVer, 8 bytes  </summary>
		MEMBER RowVer			:= 94
        */
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
        /// <summary>(Hong Kong SAR, Taiwan) </summary>
		MEMBER CP_DBF_WIN_CHINESE_1:=120     
		MEMBER CP_DBF_WIN_KOREAN:=121
        /// <summary>Chinese (PRC, Singapore) </summary>
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
    /// <seealso cref='M:XSharp.RT.Functions.RddInfo(XSharp.__Usual,XSharp.__Usual)' >RddInfo()</seealso>
    /// <remarks>Most of these ENUMs come from Harbour and have not been implemented yet. </remarks>
	ENUM RddInfo
        /// <summary>Does this RDD support DBFs?</summary>
		MEMBER RDDI_ISDBF              :=   1   
        /// <summary>Can this RDD Put Records?</summary>
		MEMBER RDDI_CANPUTREC          :=   2   
        /// <summary>The field delimiter (as a string)</summary>
		MEMBER RDDI_DELIMITER          :=   3   
        /// <summary>The record separator (as a string) </summary>
		MEMBER RDDI_SEPARATOR          :=   4   
		/// <summary>Default data file's file extension</summary>
		MEMBER RDDI_TABLEEXT           :=   5   
        /// <summary>Default memo file's file extension </summary>
		MEMBER RDDI_MEMOEXT            :=   6   
        /// <summary>Default multi tag index's file extension</summary>
		MEMBER RDDI_ORDBAGEXT          :=   7   
        /// <summary>default single tag index's file extension</summary>
		MEMBER RDDI_ORDEREXT           :=   8   
        /// <summary>default single tag index's file extension</summary>
		MEMBER RDDI_ORDSTRUCTEXT       :=   9   
		
        /// <summary>Local file access?</summary>
		MEMBER RDDI_LOCAL              :=  10   
        /// <summary>Remote table access? </summary>
		MEMBER RDDI_REMOTE             :=  11   
        /// <summary>Get/Set default connection</summary>
		MEMBER RDDI_CONNECTION         :=  12   
        /// <summary>Type of table file</summary>
		MEMBER RDDI_TABLETYPE          :=  13   
        /// <summary>Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) </summary>
		MEMBER RDDI_MEMOTYPE           :=  14   
        /// <summary>Is large file size (>=4GB) supported</summary>
		MEMBER RDDI_LARGEFILE          :=  15   
        /// <summary>Locking scheme used by RDD</summary>
		MEMBER RDDI_LOCKSCHEME         :=  16   
        /// <summary>Does RDD support record map functionality?</summary>
		MEMBER RDDI_RECORDMAP          :=  17   
        /// <summary>Does RDD support encryption</summary>
		MEMBER RDDI_ENCRYPTION         :=  18   
        /// <summary>Get/Set default trigger function</summary>
		MEMBER RDDI_TRIGGER            :=  19   
        /// <summary>automatic locking on update</summary>
		MEMBER RDDI_AUTOLOCK           :=  20   
		
		/* index parameters */		   
        /// <summary>Are structural indexes supported</summary>
		MEMBER RDDI_STRUCTORD          :=  21   
        /// <summary>Flag for avoiding RDD hierarchy and using a bigger buffer when indexing</summary>
		MEMBER RDDI_STRICTREAD         :=  22   
        /// <summary>Flag for strict structural order checking</summary>
		MEMBER RDDI_STRICTSTRUCT       :=  23   
        /// <summary>Flag for whether to use query optimization</summary>
		MEMBER RDDI_OPTIMIZE           :=  24   
        /// <summary>Flag for forcing linear optimization</summary>
		MEMBER RDDI_FORCEOPT           :=  25   
        /// <summary>Flag for automatically opening structural indexes</summary>
		MEMBER RDDI_AUTOOPEN           :=  26   
        /// <summary>When a structural index is opened, the order to be set</summary>
		MEMBER RDDI_AUTOORDER          :=  27   
        /// <summary>When a network is detected, open the index shared, otherwise open exclusively</summary>
		MEMBER RDDI_AUTOSHARE          :=  28   
        /// <summary>Does RDD support multi tag in index file</summary>
		MEMBER RDDI_MULTITAG           :=  29   
        /// <summary>Is record number part of key in sorting</summary>
		MEMBER RDDI_SORTRECNO          :=  30   
        /// <summary>Does custom orders support repeated keys?</summary>
		MEMBER RDDI_MULTIKEY           :=  31   
		
		/* memo parameters */		   
        /// <summary>Memo File's block size</summary>
		MEMBER RDDI_MEMOBLOCKSIZE      :=  32   
        /// <summary>sub version of memo file</summary>
		MEMBER RDDI_MEMOVERSION        :=  33   
        /// <summary>type of garbage collector used by GC</summary>
		MEMBER RDDI_MEMOGCTYPE         :=  34   
        /// <summary>use read lock in memo file access</summary>
		MEMBER RDDI_MEMOREADLOCK       :=  35   
        /// <summary>reuse free space on write</summary>
		MEMBER RDDI_MEMOREUSE          :=  36   
        /// <summary>can support BLOB files directly</summary>
		MEMBER RDDI_BLOB_SUPPORT       :=  37   
		
		/* misc */					   
        /// <summary>set pending trigger for next open operation</summary>
		MEMBER RDDI_PENDINGTRIGGER     :=  40   
        /// <summary>set pending password for next open operation</summary>
		MEMBER RDDI_PENDINGPASSWORD    :=  41   
        /// <summary>Get/Set default password</summary>
		MEMBER RDDI_PASSWORD           :=  42   
        /// <summary>Get/Set record and file lock timeout value</summary>
		MEMBER RDDI_LOCKRETRY          :=  43   
        /// <summary>Get/Set index dirty read flag</summary>
		MEMBER RDDI_DIRTYREAD          :=  44   
        /// <summary>Get/Set default index page size</summary>
		MEMBER RDDI_INDEXPAGESIZE      :=  45   
        /// <summary>Get/Set default number of decimal places for numeric fields if it's undefined</summary>
		MEMBER RDDI_DECIMALS           :=  46   
        /// <summary>DBF header updating modes</summary>
		MEMBER RDDI_SETHEADER          :=  47   
		
		/* SQL */					   
        /// <summary>connect to database</summary>
		MEMBER RDDI_CONNECT            :=  61   
        /// <summary>disconnect from database</summary>
		MEMBER RDDI_DISCONNECT         :=  62   
        /// <summary>execute SQL statement </summary>
		MEMBER RDDI_EXECUTE            :=  63   
        /// <summary>error number</summary>
		MEMBER RDDI_ERROR              :=  64   
        /// <summary>error description</summary>
		MEMBER RDDI_ERRORNO            :=  65   
        /// <summary>last auto insert ID</summary>
		MEMBER RDDI_INSERTID           :=  66   
        /// <summary>number of affected rows after UPDATE</summary>
		MEMBER RDDI_AFFECTEDROWS       :=  67   
        /// <summary>last executed query</summary>
		MEMBER RDDI_QUERY              :=  68   
		
	END ENUM

    [Flags];
    ENUM DbSortFlags
	    /// <summary>An ascending sort (default)   </summary>
	    MEMBER Default := 0 
	    /// <summary> A case-insensitive sort        </summary>
	    MEMBER Case	:= 1 
	    /// <summary>A sort with printable numerics        </summary>
	    MEMBER Numeric := 2 
	    /// <summary>A sort for ASCII (not nation-dependent)</summary>
	    MEMBER Ascii	 := 4 
	    /// <summary>A sort with long integer values        </summary>
	    MEMBER Long	   := 0x80 
	    /// <summary>A descending sort        </summary>
	    MEMBER Descending := 0x100 
    END ENUM

    /// <summary>Flags that describe how a DbTrans operation can be done.</summary>
    [Flags];
    ENUM DbTransInfoFlags
        /// <summary>Default</summary>
        MEMBER None := 0
        /// <summary>Both this work area and the destination work area have identical row structures (i.e., all columns match).</summary>
        MEMBER SameStructure := 1
	    /// <summary>The RDD has the ability to transfer an entire row.</summary>
        MEMBER CanPutRec     := 2
    END ENUM


END NAMESPACE


