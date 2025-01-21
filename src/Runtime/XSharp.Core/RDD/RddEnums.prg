//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



BEGIN NAMESPACE XSharp.RDD.Enums
/// <include file="XSharp.CoreDefines.xml" path="members/AutoShareMode/*" />
ENUM AutoShareMode
    MEMBER NoChange		 := 0
    MEMBER Auto		 := 1
    MEMBER ForceExclusive := 2
END ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbLockMode/*" />
ENUM DbLockMode
    MEMBER Lock
    MEMBER UnLock
END	 ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo/*" />
ENUM DbRecordInfo
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_DELETED/*" />
MEMBER DBRI_DELETED 	:= 1
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_LOCKED/*" />
MEMBER DBRI_LOCKED 		:= 2
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RECSIZE/*" />
MEMBER DBRI_RECSIZE 	:= 3
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RECNO/*" />
MEMBER DBRI_RECNO 		:= 4
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_UPDATED/*" />
MEMBER DBRI_UPDATED 	:= 5
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_BUFFPTR/*" />
MEMBER DBRI_BUFFPTR 	:= 6
// harbour extensions
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_ENCRYPTED/*" />
MEMBER DBRI_ENCRYPTED	:= 7
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWMEMOS/*" />
MEMBER DBRI_RAWMEMOS	:= 8
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWDATA/*" />
MEMBER DBRI_RAWDATA		:= 9
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWRECORD/*" />
MEMBER DBRI_RAWRECORD   := 10
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_USER/*" />
MEMBER DBRI_USER 		:= 1000
END	 ENUM


/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo/*" />
ENUM DbFieldInfo
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_NAME/*" />
MEMBER DBS_NAME				:= 1
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_TYPE/*" />
MEMBER DBS_TYPE				:= 2
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_LEN/*" />
MEMBER DBS_LEN				:= 3
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_DEC/*" />
MEMBER DBS_DEC				:= 4
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_ALIAS/*" />
MEMBER DBS_ALIAS			:= 5
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_FLAGS/*" />
MEMBER DBS_FLAGS			:= 6
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_ISNULL/*" />
MEMBER DBS_ISNULL			:= 11
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_COUNTER/*" />
MEMBER DBS_COUNTER			:= 12
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_STEP/*" />
MEMBER DBS_STEP				:= 13

/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_CAPTION/*" />
MEMBER DBS_CAPTION 		    := 14
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_COLUMNINFO/*" />
MEMBER DBS_COLUMNINFO       := 15
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_DESCRIPTION/*" />
MEMBER DBS_DESCRIPTION      := 16
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLANK/*" />
MEMBER DBS_BLANK            := 17

/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_GET/*" />
MEMBER DBS_BLOB_GET			:= 101
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_TYPE/*" />
MEMBER DBS_BLOB_TYPE		:= 102
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_LEN/*" />
MEMBER DBS_BLOB_LEN			:= 103
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_OFFSET/*" />
MEMBER DBS_BLOB_OFFSET		:= 104
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_POINTER/*" />
MEMBER DBS_BLOB_POINTER		:= 198
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_DIRECT_TYPE/*" />
MEMBER DBS_BLOB_DIRECT_TYPE	:= 222
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_DIRECT_LEN/*" />
MEMBER DBS_BLOB_DIRECT_LEN	:= 223
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_STRUCT/*" />
MEMBER DBS_STRUCT			:= 998
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_PROPERTIES/*" />
MEMBER DBS_PROPERTIES		:= 999
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_USER/*" />
MEMBER DBS_USER				:= 1000
END	 ENUM


/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo/*" />
ENUM DbInfo
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISDBF/*" />
MEMBER DBI_ISDBF 			:= 1
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CANPUTREC/*" />
MEMBER DBI_CANPUTREC		:= 2
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETHEADERSIZE/*" />
MEMBER DBI_GETHEADERSIZE	:= 3
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LASTUPDATE/*" />
MEMBER DBI_LASTUPDATE		:= 4
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETDELIMITER/*" />
MEMBER DBI_GETDELIMITER		:= 5
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SETDELIMITER/*" />
MEMBER DBI_SETDELIMITER		:= 6
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETRECSIZE/*" />
MEMBER DBI_GETRECSIZE		:= 7
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETLOCKARRAY/*" />
MEMBER DBI_GETLOCKARRAY		:= 8
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TABLEEXT/*" />
MEMBER DBI_TABLEEXT 		:= 9
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TABLEEXT/*" />
MEMBER DBI_READONLY 		:= 10
// 11-19 missing
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISFLOCK/*" />
MEMBER DBI_ISFLOCK 			:= 20
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FILESTREAM/*" />
MEMBER DBI_FILESTREAM 		:= 21
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CHILDCOUNT/*" />
MEMBER DBI_CHILDCOUNT 		:= 22
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FILEHANDLE/*" />
MEMBER DBI_FILEHANDLE 		:= 23
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FULLPATH/*" />
MEMBER DBI_FULLPATH			:= 24
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISANSI/*" />
MEMBER DBI_ISANSI 			:= 25
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BOF/*" />
MEMBER DBI_BOF 				:= 26
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_EOF/*" />
MEMBER DBI_EOF 				:= 27
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DBFILTER/*" />
MEMBER DBI_DBFILTER 		:= 28
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FOUND/*" />
MEMBER DBI_FOUND 			:= 29
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FCOUNT/*" />
MEMBER DBI_FCOUNT 			:= 30
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKCOUNT/*" />
MEMBER DBI_LOCKCOUNT		:= 31
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_VALIDBUFFER/*" />
MEMBER DBI_VALIDBUFFER		:= 32
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ALIAS/*" />
MEMBER DBI_ALIAS 			:= 33
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETSCOPE/*" />
MEMBER DBI_GETSCOPE 		:= 34
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKOFFSET/*" />
MEMBER DBI_LOCKOFFSET		:= 35
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SHARED/*" />
MEMBER DBI_SHARED 			:= 36
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOEXT/*" />
MEMBER DBI_MEMOEXT 			:= 37
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOHANDLE/*" />
MEMBER DBI_MEMOHANDLE		:= 38
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BLOB_HANDLE/*" />
MEMBER DBI_BLOB_HANDLE 		:= 38
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOBLOCKSIZE/*" />
MEMBER DBI_MEMOBLOCKSIZE 	:= 39
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BLOB_INTEGRITY/*" />
MEMBER DBI_BLOB_INTEGRITY	:= 40
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CODEPAGE/*" />
MEMBER DBI_CODEPAGE 		:= 41
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DOSCODEPAGE/*" />
MEMBER DBI_DOSCODEPAGE		:= 42
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BLOB_RECOVER/*" />
MEMBER DBI_BLOB_RECOVER 	:= 43
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_NEWINDEXLOCK/*" />
MEMBER DBI_NEWINDEXLOCK 	:= 44
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_STRICTREAD/*" />
MEMBER DBI_STRICTREAD  		:= 60
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_OPTIMIZE/*" />
MEMBER DBI_OPTIMIZE    		:= 61
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOOPEN/*" />
MEMBER DBI_AUTOOPEN    		:= 62
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOORDER/*" />
MEMBER DBI_AUTOORDER   		:= 63
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOSHARE/*" />
MEMBER DBI_AUTOSHARE   		:= 64
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOSTREAM/*" />
MEMBER DBI_MEMOSTREAM 		:= 65
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOPATH/*" />
MEMBER DBI_MEMOPATH         := 66
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DB_VERSION/*" />
MEMBER DBI_DB_VERSION 		:= 101
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_VERSION/*" />
MEMBER DBI_RDD_VERSION 		:= 102
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_LIST/*" />
MEMBER DBI_RDD_LIST 		:= 103
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOFIELD/*" />
MEMBER DBI_MEMOFIELD 		:= 104
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_VO_MACRO_SYNTAX/*" />
MEMBER DBI_VO_MACRO_SYNTAX 	:= 105
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_OBJECT/*" />
MEMBER DBI_RDD_OBJECT		:= 106
// 107 - 127 missing
// Harbour extensions
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKSCHEME/*" />
MEMBER DBI_LOCKSCHEME          := 128
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISREADONLY/*" />
MEMBER DBI_ISREADONLY          := 129
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ROLLBACK/*" />
MEMBER DBI_ROLLBACK            := 130
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_PASSWORD/*" />
MEMBER DBI_PASSWORD            := 131 /* W*/
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISENCRYPTED/*" />
MEMBER DBI_ISENCRYPTED         := 132
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOTYPE/*" />
MEMBER DBI_MEMOTYPE            := 133
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SEPARATOR/*" />
MEMBER DBI_SEPARATOR           := 134
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOVERSION/*" />
MEMBER DBI_MEMOVERSION         := 135
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TABLETYPE/*" />
MEMBER DBI_TABLETYPE           := 136
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SCOPEDRELATION/*" />
MEMBER DBI_SCOPEDRELATION      := 137
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TRIGGER/*" />
MEMBER DBI_TRIGGER             := 138
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_OPENINFO/*" />
MEMBER DBI_OPENINFO            := 139
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ENCRYPT/*" />
MEMBER DBI_ENCRYPT             := 140
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DECRYPT/*" />
MEMBER DBI_DECRYPT             := 141
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOPACK/*" />
MEMBER DBI_MEMOPACK            := 142
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DIRTYREAD/*" />
MEMBER DBI_DIRTYREAD           := 143
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_POSITIONED/*" />
MEMBER DBI_POSITIONED          := 144
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISTEMPORARY/*" />
MEMBER DBI_ISTEMPORARY         := 145
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKTEST/*" />
MEMBER DBI_LOCKTEST            := 146
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CODEPAGE_HB/*" />
MEMBER DBI_CODEPAGE_HB         := 147
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TRANSREC/*" />
MEMBER DBI_TRANSREC            := 148
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SETHEADER/*" />
MEMBER DBI_SETHEADER		   := 149
/* Harbour RECORD MAP (RM) support */
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_SUPPORTED/*" />
MEMBER DBI_RM_SUPPORTED        := 150
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_CREATE/*" />
MEMBER DBI_RM_CREATE           := 151
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_REMOVE/*" />
MEMBER DBI_RM_REMOVE           := 152
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_CLEAR/*" />
MEMBER DBI_RM_CLEAR            := 153
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_FILL/*" />
MEMBER DBI_RM_FILL             := 154
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_ADD/*" />
MEMBER DBI_RM_ADD              := 155
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_DROP/*" />
MEMBER DBI_RM_DROP             := 156
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_TEST/*" />
MEMBER DBI_RM_TEST             := 157
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_COUNT/*" />
MEMBER DBI_RM_COUNT            := 158
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_HANDLE/*" />
MEMBER DBI_RM_HANDLE           := 159
// 160 - 169 missing
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_QUERY/*" />
member DBI_QUERY				:= 170

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_REFRESH/*" />
member DBI_REFRESH		        := 171

// 171 - 200 missing
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_INFO_HANDLE/*" />
MEMBER BLOB_INFO_HANDLE		:= 201
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_FILE_RECOVER/*" />
MEMBER BLOB_FILE_RECOVER	:= 202
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_FILE_INTEGRITY/*" />
MEMBER BLOB_FILE_INTEGRITY	:= 203
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_OFFSET/*" />
MEMBER BLOB_OFFSET			:= 204
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_POINTER/*" />
MEMBER BLOB_POINTER			:= 205
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_LEN/*" />
MEMBER BLOB_LEN				:= 206
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_TYPE/*" />
MEMBER BLOB_TYPE			:= 207
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_EXPORT/*" />
MEMBER BLOB_EXPORT			:= 208
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_ROOT_UNLOCK/*" />
MEMBER BLOB_ROOT_UNLOCK		:= 209
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_ROOT_PUT/*" />
MEMBER BLOB_ROOT_PUT		:= 210
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_ROOT_GET/*" />
MEMBER BLOB_ROOT_GET		:= 211
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_ROOT_LOCK/*" />
MEMBER BLOB_ROOT_LOCK		:= 212
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_IMPORT/*" />
MEMBER BLOB_IMPORT			:= 213
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_PUT/*" />
MEMBER BLOB_DIRECT_PUT		:= 214
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_GET/*" />
MEMBER BLOB_DIRECT_GET		:= 215
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_GET/*" />
MEMBER BLOB_GET				:= 216
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_EXPORT/*" />
MEMBER BLOB_DIRECT_EXPORT	:= 217
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_IMPORT/*" />
MEMBER BLOB_DIRECT_IMPORT	:= 218
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_NMODE/*" />
MEMBER BLOB_NMODE			:= 219
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_EXPORT_APPEND/*" />
MEMBER BLOB_EXPORT_APPEND	:= 220
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_EXPORT_OVERWRITE/*" />
MEMBER BLOB_EXPORT_OVERWRITE:= 221
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_TYPE/*" />
MEMBER BLOB_DIRECT_TYPE		:= 222
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_LEN/*" />
MEMBER BLOB_DIRECT_LEN		:= 223
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_USER/*" />
MEMBER BLOB_USER			:= 2000

// Clipmore functions
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_USER/*" />
MEMBER DBI_USER 			:= 1000
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_AND/*" />
MEMBER DBI_RL_AND 			:= 1001
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_CLEAR/*" />
MEMBER DBI_RL_CLEAR 		:= 1002
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_COUNT/*" />
MEMBER DBI_RL_COUNT 		:= 1003
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_DESTROY/*" />
MEMBER DBI_RL_DESTROY 		:= 1004
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_EXFILTER/*" />
MEMBER DBI_RL_EXFILTER 		:= 1005
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_GETFILTER/*" />
MEMBER DBI_RL_GETFILTER 	:= 1006
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_HASMAYBE/*" />
MEMBER DBI_RL_HASMAYBE 		:= 1007
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_LEN/*" />
MEMBER DBI_RL_LEN 			:= 1008
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_MAYBEEVAL/*" />
MEMBER DBI_RL_MAYBEEVAL 	:= 1009
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEW/*" />
MEMBER DBI_RL_NEW 			:= 1010
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEWDUP/*" />
MEMBER DBI_RL_NEWDUP 		:= 1011
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEWQUERY/*" />
MEMBER DBI_RL_NEWQUERY 		:= 1012
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEXTRECNO/*" />
MEMBER DBI_RL_NEXTRECNO 	:= 1013
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NOT/*" />
MEMBER DBI_RL_NOT 			:= 1014
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_OR/*" />
MEMBER DBI_RL_OR 			:= 1015
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_PREVRECNO/*" />
MEMBER DBI_RL_PREVRECNO 	:= 1016
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_SET/*" />
MEMBER DBI_RL_SET 			:= 1017
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_SETFILTER/*" />
MEMBER DBI_RL_SETFILTER 	:= 1018
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_TEST/*" />
MEMBER DBI_RL_TEST 			:= 1019

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_HITS/*" />
MEMBER DBI_RL_HITS 			:= 1020
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_MISSES/*" />
MEMBER DBI_RL_MISSES    	:= 1021
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_ENABLE/*" />
MEMBER DBI_RL_ENABLE		:= 1022

// advantage
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GET_ACE_TABLE_HANDLE/*" />
MEMBER DBI_GET_ACE_TABLE_HANDLE  := DBI_USER + 110
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GET_ACE_STMT_HANDLE/*" />
MEMBER DBI_GET_ACE_STMT_HANDLE   := DBI_USER + 111
// SQL RDD

END	 ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info/*" />
ENUM DbOrder_Info
    // These number match the defines for Vulcan
    // there are some differences between the various dialects unfortunately
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_CONDITION/*" />
    MEMBER DBOI_CONDITION 	:= 1
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_EXPRESSION/*" />
    MEMBER DBOI_EXPRESSION 	:= 2
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_POSITION/*" />
    MEMBER DBOI_POSITION 	:= 3
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYGOTO/*" />
    MEMBER DBOI_KEYGOTO     := 3
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYNO/*" />
    MEMBER DBOI_KEYNO	 	:= 3
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_RECNO/*" />
    MEMBER DBOI_RECNO 		:= 4
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_NAME/*" />
    MEMBER DBOI_NAME 		:= 5
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_NUMBER/*" />
    MEMBER DBOI_NUMBER 		:= 6
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGNAME/*" />
    MEMBER DBOI_BAGNAME 	:= 7
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXNAME/*" />
    MEMBER DBOI_INDEXNAME 	:= 7
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGEXT/*" />
    MEMBER DBOI_BAGEXT 		:= 8
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXEXT/*" />
    MEMBER DBOI_INDEXEXT  	:= 8
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_DEFBAGEXT/*" />
    MEMBER DBOI_DEFBAGEXT 	:= 9

    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_COLLATION/*" />
    MEMBER DBOI_COLLATION  	:= 10


    // 14-19 missing
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FULLPATH/*" />
    MEMBER DBOI_FULLPATH 	:= 20
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FILEHANDLE/*" />
    MEMBER DBOI_FILEHANDLE 	:= 21
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISDESC/*" />
    MEMBER DBOI_ISDESC 		:= 22
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISCOND/*" />
    MEMBER DBOI_ISCOND 		:= 23
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYTYPE/*" />
    MEMBER DBOI_KEYTYPE 	:= 24
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYSIZE/*" />
    MEMBER DBOI_KEYSIZE 	:= 25
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYCOUNT/*" />
    MEMBER DBOI_KEYCOUNT 	:= 26
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SETCODEBLOCK/*" />
    MEMBER DBOI_SETCODEBLOCK:= 27
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYDEC/*" />
    MEMBER DBOI_KEYDEC 		:= 28
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_HPLOCKING/*" />
    MEMBER DBOI_HPLOCKING 	:= 29
    // 30-34 missing
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_LOCKOFFSET/*" />
    MEMBER DBOI_LOCKOFFSET 	:= 35
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYADD/*" />
    MEMBER DBOI_KEYADD 		:= 36
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYDELETE/*" />
    MEMBER DBOI_KEYDELETE 	:= 37
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYVAL/*" />
    MEMBER DBOI_KEYVAL 		:= 38
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPETOP/*" />
    MEMBER DBOI_SCOPETOP 	:= 39
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPEBOTTOM/*" />
    MEMBER DBOI_SCOPEBOTTOM := 40
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPETOPCLEAR/*" />
    MEMBER DBOI_SCOPETOPCLEAR := 41
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR/*" />
    MEMBER DBOI_SCOPEBOTTOMCLEAR:= 42
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_UNIQUE/*" />
    MEMBER DBOI_UNIQUE 		:= 43
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ORDERCOUNT/*" />
    MEMBER DBOI_ORDERCOUNT  := 44
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_CUSTOM/*" />
    MEMBER DBOI_CUSTOM 		:= 45
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPUNIQUE/*" />
    MEMBER DBOI_SKIPUNIQUE 	:= 46
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYSINCLUDED/*" />
    MEMBER DBOI_KEYSINCLUDED:= 48
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYNORAW/*" />
    MEMBER DBOI_KEYNORAW 	:= 49
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_OPTLEVEL/*" />
    MEMBER DBOI_OPTLEVEL 	:= 50
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYCOUNTRAW/*" />
    MEMBER DBOI_KEYCOUNTRAW := 51
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FILESTREAM/*" />
    MEMBER DBOI_FILESTREAM 	:=52

    // 53-59
    /* These shouldn't need an open table */
    /// <summary>The following numbers are reserved but not implemented yet.</summary>
    MEMBER DBOI_STRICTREAD   := 60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
    MEMBER DBOI_OPTIMIZE     := 61  /* Flag for whether to use query optimization             */
    MEMBER DBOI_AUTOOPEN     := 62  /* Flag for automatically opening structural indexes      */

    MEMBER DBOI_AUTOORDER    := 63  /* When a structural index is opened, the order to be set */
    MEMBER DBOI_AUTOSHARE    := 64  /* When a network is detected, open the index shared, otherwise open exclusively   */
    // 65-99

    MEMBER DBOI_LOCK_ALL    := 100  //
    MEMBER DBOI_LOCK_FAIL   := 101 //
    MEMBER DBOI_HPLOCK_GATE := 102 //




    /* Harbour extensions , not all implemented yet*/
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

    // XSharp extensions
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_DUMP/*" />
    MEMBER DBOI_DUMP                := 300
    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_VALIDATE/*" />
    MEMBER DBOI_VALIDATE            := 301

    /// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_USER/*" />
    MEMBER DBOI_USER 		:= 1000
    // Advantage
    MEMBER DBOI_AXS_PERCENT_INDEXED  := 1805
    MEMBER DBOI_GET_ACE_INDEX_HANDLE := 1806

END ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags/*" />
[Flags];
ENUM DBFFieldFlags AS BYTE
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.None/*" />
    MEMBER None             := 0x00
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.System/*" />
    MEMBER System           := 0x01
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.Nullable/*" />
    MEMBER Nullable         := 0x02
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.Binary/*" />
    MEMBER Binary           := 0x04
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.AutoIncrement/*" />
    MEMBER AutoIncrement    := 0x08
    // Harbour additions
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.Compressed/*" />
    MEMBER Compressed       := 0x10
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.Encrypted/*" />
    MEMBER Encrypted        := 0x20
    /// <include file="XSharp.CoreDefines.xml" path="members/DBFFieldFlags.Unicode/*" />
    MEMBER Unicode          := 0x40

END ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType/*" />
ENUM DbFieldType AS BYTE
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Unknown/*" />
    MEMBER Unknown		:= 0
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Character/*" />
    MEMBER Character 		:= 67
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Date/*" />
    MEMBER Date	 		:= 68
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Logic/*" />
    MEMBER Logic   		:= 76
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Memo/*" />
    MEMBER Memo    		:= 77
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Number/*" />
    MEMBER Number    		:= 78
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.VOObject/*" />
    MEMBER VOObject		:= 79
    // FoxPro types in 'Name' order

    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Blob/*" />
    MEMBER Blob			:= 87
    //MEMBER Character 		:= 67
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Currency/*" />
    MEMBER Currency		:= 89
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Double/*" />
    MEMBER Double		:= 66
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.DateTime/*" />
    MEMBER DateTime		:= 84
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Float/*" />
    MEMBER Float		:= 70
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.General/*" />
    MEMBER General		:= 71
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Integer/*" />
    MEMBER Integer		:= 73
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.Picture/*" />
    MEMBER Picture		:= 80
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.VarBinary/*" />
    MEMBER VarBinary		:= 81
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.VarChar/*" />
    MEMBER VarChar      := 86
    /// <include file="XSharp.CoreDefines.xml" path="members/DbFieldType.NullFlags/*" />
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

/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo/*" />
ENUM RddInfo
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ISDBF/*" />
    MEMBER RDDI_ISDBF              :=   1
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CANPUTREC/*" />
    MEMBER RDDI_CANPUTREC          :=   2
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DELIMITER/*" />
    MEMBER RDDI_DELIMITER          :=   3
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SEPARATOR/*" />
    MEMBER RDDI_SEPARATOR          :=   4
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TABLEEXT/*" />
    MEMBER RDDI_TABLEEXT           :=   5
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOEXT/*" />
    MEMBER RDDI_MEMOEXT            :=   6
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDBAGEXT/*" />
    MEMBER RDDI_ORDBAGEXT          :=   7
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDEREXT/*" />
    MEMBER RDDI_ORDEREXT           :=   8
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDSTRUCTEXT/*" />
    MEMBER RDDI_ORDSTRUCTEXT       :=   9

    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCAL/*" />
    MEMBER RDDI_LOCAL              :=  10
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_REMOTE/*" />
    MEMBER RDDI_REMOTE             :=  11
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CONNECTION/*" />
    MEMBER RDDI_CONNECTION         :=  12
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TABLETYPE/*" />
    MEMBER RDDI_TABLETYPE          :=  13
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOTYPE/*" />
    MEMBER RDDI_MEMOTYPE           :=  14
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LARGEFILE/*" />
    MEMBER RDDI_LARGEFILE          :=  15
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCKSCHEME/*" />
    MEMBER RDDI_LOCKSCHEME         :=  16
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_RECORDMAP/*" />
    MEMBER RDDI_RECORDMAP          :=  17
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ENCRYPTION/*" />
    MEMBER RDDI_ENCRYPTION         :=  18
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TRIGGER/*" />
    MEMBER RDDI_TRIGGER            :=  19
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOLOCK/*" />
    MEMBER RDDI_AUTOLOCK           :=  20

    /* index parameters */
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRUCTORD/*" />
    MEMBER RDDI_STRUCTORD          :=  21
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRICTREAD/*" />
    MEMBER RDDI_STRICTREAD         :=  22
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRICTSTRUCT/*" />
    MEMBER RDDI_STRICTSTRUCT       :=  23
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_OPTIMIZE/*" />
    MEMBER RDDI_OPTIMIZE           :=  24
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_FORCEOPT/*" />
    MEMBER RDDI_FORCEOPT           :=  25
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOOPEN/*" />
    MEMBER RDDI_AUTOOPEN           :=  26
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOORDER/*" />
    MEMBER RDDI_AUTOORDER          :=  27
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOSHARE/*" />
    MEMBER RDDI_AUTOSHARE          :=  28
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MULTITAG/*" />
    MEMBER RDDI_MULTITAG           :=  29
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SORTRECNO/*" />
    MEMBER RDDI_SORTRECNO          :=  30
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MULTIKEY/*" />
    MEMBER RDDI_MULTIKEY           :=  31

    /* memo parameters */
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOBLOCKSIZE/*" />
    MEMBER RDDI_MEMOBLOCKSIZE      :=  32
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOVERSION/*" />
    MEMBER RDDI_MEMOVERSION        :=  33
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOGCTYPE/*" />
    MEMBER RDDI_MEMOGCTYPE         :=  34
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOREADLOCK/*" />
    MEMBER RDDI_MEMOREADLOCK       :=  35
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOREUSE/*" />
    MEMBER RDDI_MEMOREUSE          :=  36
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_BLOB_SUPPORT/*" />
    MEMBER RDDI_BLOB_SUPPORT       :=  37

    /* misc */
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PENDINGTRIGGER/*" />
    MEMBER RDDI_PENDINGTRIGGER     :=  40
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PENDINGPASSWORD/*" />
    MEMBER RDDI_PENDINGPASSWORD    :=  41
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PASSWORD/*" />
    MEMBER RDDI_PASSWORD           :=  42
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCKRETRY/*" />
    MEMBER RDDI_LOCKRETRY          :=  43
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DIRTYREAD/*" />
    MEMBER RDDI_DIRTYREAD          :=  44
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_INDEXPAGESIZE/*" />
    MEMBER RDDI_INDEXPAGESIZE      :=  45
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DECIMALS/*" />
    MEMBER RDDI_DECIMALS           :=  46
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SETHEADER/*" />
    MEMBER RDDI_SETHEADER          :=  47

    /* SQL */
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CONNECT/*" />
    MEMBER RDDI_CONNECT            :=  61
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DISCONNECT/*" />
    MEMBER RDDI_DISCONNECT         :=  62
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_EXECUTE/*" />
    MEMBER RDDI_EXECUTE            :=  63
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ERROR/*" />
    MEMBER RDDI_ERROR              :=  64
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ERRORNO/*" />
    MEMBER RDDI_ERRORNO            :=  65
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_INSERTID/*" />
    MEMBER RDDI_INSERTID           :=  66
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AFFECTEDROWS/*" />
    MEMBER RDDI_AFFECTEDROWS       :=  67
    /// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_QUERY/*" />
    MEMBER RDDI_QUERY              :=  68

END ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags/*" />
[Flags];
ENUM DbSortFlags
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Default/*" />
    MEMBER Default := 0
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Case/*" />
    MEMBER Case	:= 1
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Numeric/*" />
    MEMBER Numeric := 2
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Ascii/*" />
    MEMBER Ascii	 := 4
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Long/*" />
    MEMBER Long	   := 0x80
    /// <include file="XSharp.CoreDefines.xml" path="members/DbSortFlags.Descending/*" />
    MEMBER Descending := 0x100
END ENUM

/// <include file="XSharp.CoreDefines.xml" path="members/DbTransInfoFlags/*" />
[Flags];
ENUM DbTransInfoFlags
    /// <include file="XSharp.CoreDefines.xml" path="members/DbTransInfoFlags.None/*" />
    MEMBER None := 0
    /// <include file="XSharp.CoreDefines.xml" path="members/DbTransInfoFlags.SameStructure/*" />
    MEMBER SameStructure := 1
    /// <include file="XSharp.CoreDefines.xml" path="members/DbTransInfoFlags.CanPutRec/*" />
    MEMBER CanPutRec     := 2
END ENUM


/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType/*" />
/// <seealso cref="IDbNotify"/>
/// <seealso cref="DbRegisterClient"/>
/// <seealso cref="DbUnRegisterClient"/>
Enum DbNotificationType
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.FileCreate/*" />
    MEMBER FileCreate
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.FileOpen/*" />
    MEMBER FileOpen
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.FileClose/*" />
    MEMBER FileClose
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.IndexCreate/*" />
    MEMBER IndexCreate
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.IndexDelete/*" />
    MEMBER IndexDelete
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.IndexOpen/*" />
    MEMBER IndexOpen
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.IndexClose/*" />
    MEMBER IndexClose
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.BeforeBulkOperation/*" />
    MEMBER BeforeBulkOperation
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.AfterBulkOperation/*" />
    MEMBER AfterBulkOperation
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.OrderChanged/*" />
    MEMBER OrderChanged
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.BeforeFieldUpdate/*" />
    MEMBER BeforeFieldUpdate
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.AfterFieldUpdate/*" />
    MEMBER AfterFieldUpdate
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.BeforeMove/*" />
    MEMBER BeforeMove
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.AfterMove/*" />
    MEMBER AfterMove
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.RecordAppended/*" />
    MEMBER RecordAppended
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.AfterRecordDeleted/*" />
    MEMBER AfterRecordDeleted
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.RecordDeleted/*" />
    MEMBER RecordDeleted := AfterRecordDeleted
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.AfterRecordRecalled/*" />
    MEMBER AfterRecordRecalled
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.RecordRecalled/*" />
    MEMBER RecordRecalled := AfterRecordRecalled
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.RecordLocked/*" />
    MEMBER RecordLocked
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.RecordUnLocked/*" />
    MEMBER RecordUnLocked
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.FileCommit/*" />
    MEMBER FileCommit
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.BeforeRecordDeleted/*" />
    MEMBER BeforeRecordDeleted
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.BeforeRecordRecalled/*" />
    MEMBER BeforeRecordRecalled
/// <include file="XSharp.CoreDefines.xml" path="members/DbNotificationType.OperationFailed/*" />
    MEMBER OperationFailed    := 99
END ENUM

END NAMESPACE


