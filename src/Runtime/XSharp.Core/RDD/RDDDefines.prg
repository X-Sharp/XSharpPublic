//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums

DEFINE _NULLFLAGS := "_NullFlags"

// RecInfo defines
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_DELETED/*" />
DEFINE DBRI_DELETED 	:= DbRecordInfo.DBRI_DELETED
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_LOCKED/*" />
DEFINE DBRI_LOCKED 		:= DbRecordInfo.DBRI_LOCKED
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RECSIZE/*" />
DEFINE DBRI_RECSIZE		:= DbRecordInfo.DBRI_RECSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RECNO/*" />
DEFINE DBRI_RECNO		:= DbRecordInfo.DBRI_RECNO
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_UPDATED/*" />
DEFINE DBRI_UPDATED		:= DbRecordInfo.DBRI_UPDATED
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_BUFFPTR/*" />
DEFINE DBRI_BUFFPTR 	:= DbRecordInfo.DBRI_BUFFPTR
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWRECORD/*" />
DEFINE DBRI_RAWRECORD   := DbRecordInfo.DBRI_RAWRECORD
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_ENCRYPTED/*" />
DEFINE DBRI_ENCRYPTED	:= DbRecordInfo.DBRI_ENCRYPTED
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWMEMOS/*" />
DEFINE DBRI_RAWMEMOS	:= DbRecordInfo.DBRI_RAWMEMOS
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_RAWDATA/*" />
DEFINE DBRI_RAWDATA		:= DbRecordInfo.DBRI_RAWDATA
/// <include file="XSharp.CoreDefines.xml" path="members/DbRecordInfo.DBRI_USER/*" />
DEFINE DBRI_USER		:= DbRecordInfo.DBRI_USER


// FieldInfo defines
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_NAME/*" />
DEFINE DBS_NAME					:= DbFieldInfo.DBS_NAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_TYPE/*" />
DEFINE DBS_TYPE					:= DbFieldInfo.DBS_TYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_LEN/*" />
DEFINE DBS_LEN					:= DbFieldInfo.DBS_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_DEC/*" />
DEFINE DBS_DEC					:= DbFieldInfo.DBS_DEC
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_ALIAS/*" />
DEFINE DBS_ALIAS				:= DbFieldInfo.DBS_ALIAS
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_FLAGS/*" />
DEFINE DBS_FLAGS				:= DbFieldInfo.DBS_FLAGS
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_ALEN/*" />
DEFINE DBS_ALEN                 := 4
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_CAPTION/*" />
DEFINE DBS_CAPTION              := DbFieldInfo.DBS_CAPTION
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_DESCRIPTION/*" />
DEFINE DBS_DESCRIPTION          := DbFieldInfo.DBS_DESCRIPTION
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_COLUMNINFO/*" />
DEFINE DBS_COLUMNINFO           := DbFieldInfo.DBS_COLUMNINFO
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLANK/*" />
DEFINE DBS_BLANK                := DbFieldInfo.DBS_BLANK
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_ISNULL/*" />
DEFINE DBS_ISNULL               := DbFieldInfo.DBS_ISNULL
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_COUNTER/*" />
DEFINE DBS_COUNTER              := DbFieldInfo.DBS_COUNTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_STEP/*" />
DEFINE DBS_STEP                 := DbFieldInfo.DBS_STEP
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_GET/*" />
DEFINE DBS_BLOB_GET             := DbFieldInfo.DBS_BLOB_GET
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_TYPE/*" />
DEFINE DBS_BLOB_TYPE			:= DbFieldInfo.DBS_BLOB_TYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_LEN/*" />
DEFINE DBS_BLOB_LEN				:= DbFieldInfo.DBS_BLOB_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_OFFSET/*" />
DEFINE DBS_BLOB_OFFSET			:= DbFieldInfo.DBS_BLOB_OFFSET
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_POINTER/*" />
DEFINE DBS_BLOB_POINTER			:= DbFieldInfo.DBS_BLOB_POINTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_DIRECT_TYPE/*" />
DEFINE DBS_BLOB_DIRECT_TYPE		:= DbFieldInfo.DBS_BLOB_DIRECT_TYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_BLOB_DIRECT_LEN/*" />
DEFINE DBS_BLOB_DIRECT_LEN		:= DbFieldInfo.DBS_BLOB_DIRECT_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_STRUCT/*" />
DEFINE DBS_STRUCT				:= DbFieldInfo.DBS_STRUCT
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_PROPERTIES/*" />
DEFINE DBS_PROPERTIES			:= DbFieldInfo.DBS_PROPERTIES
/// <include file="XSharp.CoreDefines.xml" path="members/DbFieldInfo.DBS_USER/*" />
DEFINE DBS_USER					:= DbFieldInfo.DBS_USER

// Scope defines
DEFINE TOPSCOPE                := 0
DEFINE BOTTOMSCOPE             := 1

//DbInfo Defines
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISDBF/*" />
DEFINE DBI_ISDBF 			:=  DbInfo.DBI_ISDBF
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CANPUTREC/*" />
DEFINE DBI_CANPUTREC 		:=  DbInfo.DBI_CANPUTREC
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETHEADERSIZE/*" />
DEFINE DBI_GETHEADERSIZE 	:= 	DbInfo.DBI_GETHEADERSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LASTUPDATE/*" />
DEFINE DBI_LASTUPDATE 		:= 	DbInfo.DBI_LASTUPDATE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETDELIMITER/*" />
DEFINE DBI_GETDELIMITER 	:= 	DbInfo.DBI_GETDELIMITER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SETDELIMITER/*" />
DEFINE DBI_SETDELIMITER 	:=  DbInfo.DBI_SETDELIMITER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETRECSIZE/*" />
DEFINE DBI_GETRECSIZE 		:= 	DbInfo.DBI_GETRECSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETLOCKARRAY/*" />
DEFINE DBI_GETLOCKARRAY 	:=  DbInfo.DBI_GETLOCKARRAY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TABLEEXT/*" />
DEFINE DBI_TABLEEXT 		:=  DbInfo.DBI_TABLEEXT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FULLPATH/*" />
DEFINE DBI_FULLPATH 		:= 	DbInfo.DBI_FULLPATH
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISFLOCK/*" />
DEFINE DBI_ISFLOCK 			:= 	DbInfo.DBI_ISFLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_READONLY/*" />
DEFINE DBI_READONLY 		:= 	DbInfo.DBI_READONLY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CHILDCOUNT/*" />
DEFINE DBI_CHILDCOUNT 		:= 	DbInfo.DBI_CHILDCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FILEHANDLE/*" />
DEFINE DBI_FILEHANDLE 		:= 	DbInfo.DBI_FILEHANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FILESTREAM/*" />
DEFINE DBI_FILESTREAM 		:= 	DbInfo.DBI_FILESTREAM
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISANSI/*" />
DEFINE DBI_ISANSI 			:= 	DbInfo.DBI_ISANSI
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BOF/*" />
DEFINE DBI_BOF 				:= 	DbInfo.DBI_BOF
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_EOF/*" />
DEFINE DBI_EOF 				:= 	DbInfo.DBI_EOF
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DBFILTER/*" />
DEFINE DBI_DBFILTER 		:= 	DbInfo.DBI_DBFILTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FOUND/*" />
DEFINE DBI_FOUND 			:= 	DbInfo.DBI_FOUND
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_FCOUNT/*" />
DEFINE DBI_FCOUNT 			:= 	DbInfo.DBI_FCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKCOUNT/*" />
DEFINE DBI_LOCKCOUNT 		:= 	DbInfo.DBI_LOCKCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_VALIDBUFFER/*" />
DEFINE DBI_VALIDBUFFER  	:= 	DbInfo.DBI_VALIDBUFFER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ALIAS/*" />
DEFINE DBI_ALIAS 			:= 	DbInfo.DBI_ALIAS
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GETSCOPE/*" />
DEFINE DBI_GETSCOPE 		:= 	DbInfo.DBI_GETSCOPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKOFFSET/*" />
DEFINE DBI_LOCKOFFSET 		:= 	DbInfo.DBI_LOCKOFFSET
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SHARED/*" />
DEFINE DBI_SHARED 			:= 	DbInfo.DBI_SHARED
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOEXT/*" />
DEFINE DBI_MEMOEXT 			:= 	DbInfo.DBI_MEMOEXT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOHANDLE/*" />
DEFINE DBI_MEMOHANDLE 		:= 	DbInfo.DBI_MEMOHANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOSTREAM/*" />
DEFINE DBI_MEMOSTREAM 		:= 	DbInfo.DBI_MEMOSTREAM
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_BLOB_HANDLE/*" />
DEFINE DBI_BLOB_HANDLE 		:= 	DbInfo.DBI_BLOB_HANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOBLOCKSIZE/*" />
DEFINE DBI_MEMOBLOCKSIZE 	:= 	DbInfo.DBI_MEMOBLOCKSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CODEPAGE/*" />
DEFINE DBI_CODEPAGE 		:= 	DbInfo.DBI_CODEPAGE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_NEWINDEXLOCK/*" />
DEFINE DBI_NEWINDEXLOCK 	:= 	DbInfo.DBI_NEWINDEXLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DOSCODEPAGE/*" />
DEFINE DBI_DOSCODEPAGE 		:= 	DbInfo.DBI_DOSCODEPAGE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_STRICTREAD/*" />
DEFINE DBI_STRICTREAD  		:= DbInfo.DBI_STRICTREAD
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_OPTIMIZE/*" />
DEFINE DBI_OPTIMIZE    		:= DbInfo.DBI_OPTIMIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOOPEN/*" />
DEFINE DBI_AUTOOPEN    		:= DbInfo.DBI_AUTOOPEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOORDER/*" />
DEFINE DBI_AUTOORDER   		:= DbInfo.DBI_AUTOORDER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_AUTOSHARE/*" />
DEFINE DBI_AUTOSHARE   		:= DbInfo.DBI_AUTOSHARE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DB_VERSION/*" />
DEFINE DBI_DB_VERSION 		:= DbInfo.DBI_DB_VERSION
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_VERSION/*" />
DEFINE DBI_RDD_VERSION 		:= DbInfo.DBI_RDD_VERSION
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_LIST/*" />
DEFINE DBI_RDD_LIST 		:= DbInfo.DBI_RDD_LIST
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOFIELD/*" />
DEFINE DBI_MEMOFIELD 		:= DbInfo.DBI_MEMOFIELD
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_VO_MACRO_SYNTAX/*" />
DEFINE DBI_VO_MACRO_SYNTAX	:= DbInfo.DBI_VO_MACRO_SYNTAX
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RDD_OBJECT/*" />
DEFINE DBI_RDD_OBJECT 		:= DbInfo.DBI_RDD_OBJECT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.*//*" />
/* CA-Cl*pper documented for public use */

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_LEN/*" />
DEFINE DBI_BLOB_DIRECT_LEN     := DbInfo.BLOB_DIRECT_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_DIRECT_TYPE/*" />
DEFINE DBI_BLOB_DIRECT_TYPE    := DbInfo.BLOB_DIRECT_TYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_FILE_INTEGRITY/*" />
DEFINE DBI_BLOB_INTEGRITY      := DbInfo.BLOB_FILE_INTEGRITY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_OFFSET/*" />
DEFINE DBI_BLOB_OFFSET         := DbInfo.BLOB_OFFSET
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.BLOB_FILE_RECOVER/*" />
DEFINE DBI_BLOB_RECOVER        := DbInfo.BLOB_FILE_RECOVER

/* Harbour extension */

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKSCHEME/*" />
DEFINE DBI_LOCKSCHEME          := DbInfo.DBI_LOCKSCHEME
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISREADONLY/*" />
DEFINE DBI_ISREADONLY          := DbInfo.DBI_ISREADONLY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ROLLBACK/*" />
DEFINE DBI_ROLLBACK            := DbInfo.DBI_ROLLBACK
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_PASSWORD/*" />
DEFINE DBI_PASSWORD            := DbInfo.DBI_PASSWORD
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISENCRYPTED/*" />
DEFINE DBI_ISENCRYPTED         := DbInfo.DBI_ISENCRYPTED
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOTYPE/*" />
DEFINE DBI_MEMOTYPE            := DbInfo.DBI_MEMOTYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SEPARATOR/*" />
DEFINE DBI_SEPARATOR           := DbInfo.DBI_SEPARATOR
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOVERSION/*" />
DEFINE DBI_MEMOVERSION         := DbInfo.DBI_MEMOVERSION
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOPATH/*" />
DEFINE DBI_MEMOPATH            := DbInfo.DBI_MEMOPATH
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TABLETYPE/*" />
DEFINE DBI_TABLETYPE           := DbInfo.DBI_TABLETYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SCOPEDRELATION/*" />
DEFINE DBI_SCOPEDRELATION      := DbInfo.DBI_SCOPEDRELATION
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TRIGGER/*" />
DEFINE DBI_TRIGGER             := DbInfo.DBI_TRIGGER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_OPENINFO/*" />
DEFINE DBI_OPENINFO            := DbInfo.DBI_OPENINFO
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ENCRYPT/*" />
DEFINE DBI_ENCRYPT             := DbInfo.DBI_ENCRYPT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DECRYPT/*" />
DEFINE DBI_DECRYPT             := DbInfo.DBI_DECRYPT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_MEMOPACK/*" />
DEFINE DBI_MEMOPACK            := DbInfo.DBI_MEMOPACK
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_DIRTYREAD/*" />
DEFINE DBI_DIRTYREAD           := DbInfo.DBI_DIRTYREAD
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_POSITIONED/*" />
DEFINE DBI_POSITIONED          := DbInfo.DBI_POSITIONED
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_ISTEMPORARY/*" />
DEFINE DBI_ISTEMPORARY         := DbInfo.DBI_ISTEMPORARY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_LOCKTEST/*" />
DEFINE DBI_LOCKTEST            := DbInfo.DBI_LOCKTEST
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_CODEPAGE_HB/*" />
DEFINE DBI_CODEPAGE_HB         := DbInfo.DBI_CODEPAGE_HB
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_TRANSREC/*" />
DEFINE DBI_TRANSREC            := DbInfo.DBI_TRANSREC
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_SETHEADER/*" />
DEFINE DBI_SETHEADER		   := DbInfo.DBI_SETHEADER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_QUERY/*" />
DEFINE DBI_QUERY			   := DbInfo.DBI_QUERY

/* Harbour RECORD MAP (RM) support */

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_SUPPORTED/*" />
DEFINE DBI_RM_SUPPORTED        := DbInfo.DBI_RM_SUPPORTED
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_CREATE/*" />
DEFINE DBI_RM_CREATE           := DbInfo.DBI_RM_CREATE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_REMOVE/*" />
DEFINE DBI_RM_REMOVE           := DbInfo.DBI_RM_REMOVE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_CLEAR/*" />
DEFINE DBI_RM_CLEAR            := DbInfo.DBI_RM_CLEAR
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_FILL/*" />
DEFINE DBI_RM_FILL             := DbInfo.DBI_RM_FILL
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_ADD/*" />
DEFINE DBI_RM_ADD              := DbInfo.DBI_RM_ADD
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_DROP/*" />
DEFINE DBI_RM_DROP             := DbInfo.DBI_RM_DROP
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_TEST/*" />
DEFINE DBI_RM_TEST             := DbInfo.DBI_RM_TEST
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_COUNT/*" />
DEFINE DBI_RM_COUNT            := DbInfo.DBI_RM_COUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RM_HANDLE/*" />
DEFINE DBI_RM_HANDLE           := DbInfo.DBI_RM_HANDLE



// CDX and Comix Record List Support
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_AND/*" />
DEFINE DBI_RL_AND 		:= DbInfo.DBI_RL_AND
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_CLEAR/*" />
DEFINE DBI_RL_CLEAR 	:= DbInfo.DBI_RL_CLEAR
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_COUNT/*" />
DEFINE DBI_RL_COUNT 	:= DbInfo.DBI_RL_COUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_DESTROY/*" />
DEFINE DBI_RL_DESTROY 	:= DbInfo.DBI_RL_DESTROY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_EXFILTER/*" />
DEFINE DBI_RL_EXFILTER 	:= DbInfo.DBI_RL_EXFILTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_GETFILTER/*" />
DEFINE DBI_RL_GETFILTER := DbInfo.DBI_RL_GETFILTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_HASMAYBE/*" />
DEFINE DBI_RL_HASMAYBE 	:= DbInfo.DBI_RL_HASMAYBE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_LEN/*" />
DEFINE DBI_RL_LEN 		:= DbInfo.DBI_RL_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_MAYBEEVAL/*" />
DEFINE DBI_RL_MAYBEEVAL := DbInfo.DBI_RL_MAYBEEVAL
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEW/*" />
DEFINE DBI_RL_NEW 		:= DbInfo.DBI_RL_NEW
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEWDUP/*" />
DEFINE DBI_RL_NEWDUP 	:= DbInfo.DBI_RL_NEWDUP
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEWQUERY/*" />
DEFINE DBI_RL_NEWQUERY 	:= DbInfo.DBI_RL_NEWQUERY
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NEXTRECNO/*" />
DEFINE DBI_RL_NEXTRECNO := DbInfo.DBI_RL_NEXTRECNO
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_NOT/*" />
DEFINE DBI_RL_NOT 		:= DbInfo.DBI_RL_NOT
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_OR/*" />
DEFINE DBI_RL_OR 		:= DbInfo.DBI_RL_OR
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_PREVRECNO/*" />
DEFINE DBI_RL_PREVRECNO := DbInfo.DBI_RL_PREVRECNO
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_SET/*" />
DEFINE DBI_RL_SET 		:= DbInfo.DBI_RL_SET
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_SETFILTER/*" />
DEFINE DBI_RL_SETFILTER := DbInfo.DBI_RL_SETFILTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_TEST/*" />
DEFINE DBI_RL_TEST 		:= DbInfo.DBI_RL_TEST
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_HITS/*" />
DEFINE DBI_RL_HITS 			:= DbInfo.DBI_RL_HITS
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_MISSES/*" />
DEFINE DBI_RL_MISSES    	:= DbInfo.DBI_RL_MISSES
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_RL_ENABLE/*" />
DEFINE DBI_RL_ENABLE		:= DbInfo.DBI_RL_ENABLE

/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_USER/*" />
DEFINE DBI_USER 				:= DbInfo.DBI_USER

// Advantage additions
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GET_ACE_TABLE_HANDLE/*" />
DEFINE DBI_GET_ACE_TABLE_HANDLE  := DbInfo.DBI_GET_ACE_TABLE_HANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbInfo.DBI_GET_ACE_STMT_HANDLE/*" />
DEFINE DBI_GET_ACE_STMT_HANDLE   := DbInfo.DBI_GET_ACE_STMT_HANDLE

// OrderInfo Defines
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_CONDITION/*" />
DEFINE DBOI_CONDITION 	:= DbOrder_Info.DBOI_CONDITION
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_EXPRESSION/*" />
DEFINE DBOI_EXPRESSION 	:= DbOrder_Info.DBOI_EXPRESSION
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_POSITION/*" />
DEFINE DBOI_POSITION 	:= DbOrder_Info.DBOI_POSITION
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_RECNO/*" />
DEFINE DBOI_RECNO 		:= DbOrder_Info.DBOI_RECNO
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_NAME/*" />
DEFINE DBOI_NAME 		:= DbOrder_Info.DBOI_NAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_NUMBER/*" />
DEFINE DBOI_NUMBER 		:= DbOrder_Info.DBOI_NUMBER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGNAME/*" />
DEFINE DBOI_BAGNAME 	:= DbOrder_Info.DBOI_BAGNAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXNAME/*" />
DEFINE DBOI_INDEXNAME 	:= DbOrder_Info.DBOI_INDEXNAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGEXT/*" />
DEFINE DBOI_BAGEXT 		:= DbOrder_Info.DBOI_BAGEXT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXEXT/*" />
DEFINE DBOI_INDEXEXT  	:= DbOrder_Info.DBOI_INDEXEXT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_DEFBAGEXT/*" />
DEFINE DBOI_DEFBAGEXT   := DbOrder_Info.DBOI_DEFBAGEXT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ORDERCOUNT/*" />
DEFINE DBOI_ORDERCOUNT  := DbOrder_Info.DBOI_ORDERCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FILEHANDLE/*" />
DEFINE DBOI_FILEHANDLE 	:= DbOrder_Info.DBOI_FILEHANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FILESTREAM/*" />
DEFINE DBOI_FILESTREAM 	:= DbOrder_Info.DBOI_FILESTREAM
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISCOND/*" />
DEFINE DBOI_ISCOND 		:= DbOrder_Info.DBOI_ISCOND
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISDESC/*" />
DEFINE DBOI_ISDESC 		:= DbOrder_Info.DBOI_ISDESC
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_UNIQUE/*" />
DEFINE DBOI_UNIQUE 		:= DbOrder_Info.DBOI_UNIQUE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_COLLATION/*" />
DEFINE DBOI_COLLATION 	:= DbOrder_Info.DBOI_COLLATION

/* Clipper 5.3-level constants */
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FULLPATH/*" />
DEFINE DBOI_FULLPATH 	:= DbOrder_Info.DBOI_FULLPATH
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYTYPE/*" />
DEFINE DBOI_KEYTYPE 	:= DbOrder_Info.DBOI_KEYTYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYSIZE/*" />
DEFINE DBOI_KEYSIZE 	:= DbOrder_Info.DBOI_KEYSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYCOUNT/*" />
DEFINE DBOI_KEYCOUNT 	:= DbOrder_Info.DBOI_KEYCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SETCODEBLOCK/*" />
DEFINE DBOI_SETCODEBLOCK:= DbOrder_Info.DBOI_SETCODEBLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYDEC/*" />
DEFINE DBOI_KEYDEC 		:= DbOrder_Info.DBOI_KEYDEC
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_HPLOCKING/*" />
DEFINE DBOI_HPLOCKING 	:= DbOrder_Info.DBOI_HPLOCKING
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_LOCKOFFSET/*" />
DEFINE DBOI_LOCKOFFSET 	:= DbOrder_Info.DBOI_LOCKOFFSET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYADD/*" />
DEFINE DBOI_KEYADD 		:= DbOrder_Info.DBOI_KEYADD
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYDELETE/*" />
DEFINE DBOI_KEYDELETE 	:= DbOrder_Info.DBOI_KEYDELETE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYVAL/*" />
DEFINE DBOI_KEYVAL 		:= DbOrder_Info.DBOI_KEYVAL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPETOP/*" />
DEFINE DBOI_SCOPETOP 	:= DbOrder_Info.DBOI_SCOPETOP
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPEBOTTOM/*" />
DEFINE DBOI_SCOPEBOTTOM := DbOrder_Info.DBOI_SCOPEBOTTOM
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPETOPCLEAR/*" />
DEFINE DBOI_SCOPETOPCLEAR := DbOrder_Info.DBOI_SCOPETOPCLEAR
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR/*" />
DEFINE DBOI_SCOPEBOTTOMCLEAR:= DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_CUSTOM/*" />
DEFINE DBOI_CUSTOM 		:= DbOrder_Info.DBOI_CUSTOM
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPUNIQUE/*" />
DEFINE DBOI_SKIPUNIQUE 	:= DbOrder_Info.DBOI_SKIPUNIQUE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYSINCLUDED/*" />
DEFINE DBOI_KEYSINCLUDED:= DbOrder_Info.DBOI_KEYSINCLUDED
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYNORAW/*" />
DEFINE DBOI_KEYNORAW 	:= DbOrder_Info.DBOI_KEYNORAW
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYCOUNTRAW/*" />
DEFINE DBOI_KEYCOUNTRAW := DbOrder_Info.DBOI_KEYCOUNTRAW
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_OPTLEVEL/*" />
DEFINE DBOI_OPTLEVEL 	:= DbOrder_Info.DBOI_OPTLEVEL


/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_STRICTREAD/*" />
DEFINE DBOI_STRICTREAD := DbOrder_Info.DBOI_STRICTREAD
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_OPTIMIZE/*" />
DEFINE DBOI_OPTIMIZE   := DbOrder_Info.DBOI_OPTIMIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_AUTOOPEN/*" />
DEFINE DBOI_AUTOOPEN   := DbOrder_Info.DBOI_AUTOOPEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_AUTOORDER/*" />
DEFINE DBOI_AUTOORDER  := DbOrder_Info.DBOI_AUTOORDER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_AUTOSHARE/*" />
DEFINE DBOI_AUTOSHARE  := DbOrder_Info.DBOI_AUTOSHARE


/* Harbour extensions */

/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPEVAL/*" />
DEFINE DBOI_SKIPEVAL           := DbOrder_Info.DBOI_SKIPEVAL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPEVALBACK/*" />
DEFINE DBOI_SKIPEVALBACK       := DbOrder_Info.DBOI_SKIPEVALBACK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPREGEX/*" />
DEFINE DBOI_SKIPREGEX          := DbOrder_Info.DBOI_SKIPREGEX
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPREGEXBACK/*" />
DEFINE DBOI_SKIPREGEXBACK      := DbOrder_Info.DBOI_SKIPREGEXBACK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPWILD/*" />
DEFINE DBOI_SKIPWILD           := DbOrder_Info.DBOI_SKIPWILD
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SKIPWILDBACK/*" />
DEFINE DBOI_SKIPWILDBACK       := DbOrder_Info.DBOI_SKIPWILDBACK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPEEVAL/*" />
DEFINE DBOI_SCOPEEVAL          := DbOrder_Info.DBOI_SCOPEEVAL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FINDREC/*" />
DEFINE DBOI_FINDREC            := DbOrder_Info.DBOI_FINDREC
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_FINDRECCONT/*" />
DEFINE DBOI_FINDRECCONT        := DbOrder_Info.DBOI_FINDRECCONT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPESET/*" />
DEFINE DBOI_SCOPESET           := DbOrder_Info.DBOI_SCOPESET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SCOPECLEAR/*" />
DEFINE DBOI_SCOPECLEAR         := DbOrder_Info.DBOI_SCOPECLEAR


/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGCOUNT/*" />
DEFINE DBOI_BAGCOUNT           := DbOrder_Info.DBOI_BAGCOUNT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGNUMBER/*" />
DEFINE DBOI_BAGNUMBER          := DbOrder_Info.DBOI_BAGNUMBER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_BAGORDER/*" />
DEFINE DBOI_BAGORDER           := DbOrder_Info.DBOI_BAGORDER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISMULTITAG/*" />
DEFINE DBOI_ISMULTITAG         := DbOrder_Info.DBOI_ISMULTITAG
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISSORTRECNO/*" />
DEFINE DBOI_ISSORTRECNO        := DbOrder_Info.DBOI_ISSORTRECNO
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_LARGEFILE/*" />
DEFINE DBOI_LARGEFILE          := DbOrder_Info.DBOI_LARGEFILE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_TEMPLATE/*" />
DEFINE DBOI_TEMPLATE           := DbOrder_Info.DBOI_TEMPLATE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_MULTIKEY/*" />
DEFINE DBOI_MULTIKEY           := DbOrder_Info.DBOI_MULTIKEY
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_CHGONLY/*" />
DEFINE DBOI_CHGONLY            := DbOrder_Info.DBOI_CHGONLY
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_PARTIAL/*" />
DEFINE DBOI_PARTIAL            := DbOrder_Info.DBOI_PARTIAL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_SHARED/*" />
DEFINE DBOI_SHARED             := DbOrder_Info.DBOI_SHARED
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISREADONLY/*" />
DEFINE DBOI_ISREADONLY         := DbOrder_Info.DBOI_ISREADONLY
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_READLOCK/*" />
DEFINE DBOI_READLOCK           := DbOrder_Info.DBOI_READLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_WRITELOCK/*" />
DEFINE DBOI_WRITELOCK          := DbOrder_Info.DBOI_WRITELOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_UPDATECOUNTER/*" />
DEFINE DBOI_UPDATECOUNTER      := DbOrder_Info.DBOI_UPDATECOUNTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_EVALSTEP/*" />
DEFINE DBOI_EVALSTEP           := DbOrder_Info.DBOI_EVALSTEP
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_ISREINDEX/*" />
DEFINE DBOI_ISREINDEX          := DbOrder_Info.DBOI_ISREINDEX
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_I_BAGNAME/*" />
DEFINE DBOI_I_BAGNAME          := DbOrder_Info.DBOI_I_BAGNAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_I_TAGNAME/*" />
DEFINE DBOI_I_TAGNAME          := DbOrder_Info.DBOI_I_TAGNAME
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_RELKEYPOS/*" />
DEFINE DBOI_RELKEYPOS          := DbOrder_Info.DBOI_RELKEYPOS
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_USECURRENT/*" />
DEFINE DBOI_USECURRENT         := DbOrder_Info.DBOI_USECURRENT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXTYPE/*" />
DEFINE DBOI_INDEXTYPE          := DbOrder_Info.DBOI_INDEXTYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_RESETPOS/*" />
DEFINE DBOI_RESETPOS           := DbOrder_Info.DBOI_RESETPOS
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_INDEXPAGESIZE/*" />
DEFINE DBOI_INDEXPAGESIZE      := DbOrder_Info.DBOI_INDEXPAGESIZE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_USER/*" />
DEFINE DBOI_USER 				:= DbOrder_Info.DBOI_USER

/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_DUMP/*" />
DEFINE DBOI_DUMP 			:= DbOrder_Info.DBOI_DUMP
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_VALIDATE/*" />
DEFINE DBOI_VALIDATE 			:= DbOrder_Info.DBOI_VALIDATE

// Advantage extensions

/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_AXS_PERCENT_INDEXED/*" />
DEFINE DBOI_AXS_PERCENT_INDEXED  := DbOrder_Info.DBOI_AXS_PERCENT_INDEXED
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_GET_ACE_INDEX_HANDLE/*" />
DEFINE DBOI_GET_ACE_INDEX_HANDLE := DbOrder_Info.DBOI_GET_ACE_INDEX_HANDLE


// Duplicates
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_POSITION/*" />
DEFINE DBOI_KEYGOTO 	:= DbOrder_Info.DBOI_POSITION
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_KEYNORAW/*" />
DEFINE DBOI_KEYGOTORAW 	:= DbOrder_Info.DBOI_KEYNORAW
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_POSITION/*" />
DEFINE DBOI_KEYNO	 	:= DbOrder_Info.DBOI_POSITION


/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_LOCK_ALL/*" />
DEFINE DBOI_LOCK_ALL    := DbOrder_Info.DBOI_LOCK_ALL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_LOCK_FAIL/*" />
DEFINE DBOI_LOCK_FAIL   := DbOrder_Info.DBOI_LOCK_FAIL
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.DBOI_HPLOCK_GATE/*" />
DEFINE DBOI_HPLOCK_GATE := DbOrder_Info.DBOI_HPLOCK_GATE

// Blob defines



/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_INFO_HANDLE/*" />
DEFINE BLOB_INFO_HANDLE 	:= DbInfo.BLOB_INFO_HANDLE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_FILE_RECOVER/*" />
DEFINE BLOB_FILE_RECOVER 	:= DbInfo.BLOB_FILE_RECOVER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_FILE_INTEGRITY/*" />
DEFINE BLOB_FILE_INTEGRITY 	:= DbInfo.BLOB_FILE_INTEGRITY
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_OFFSET/*" />
DEFINE BLOB_OFFSET 			:= DbInfo.BLOB_OFFSET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_POINTER/*" />
DEFINE BLOB_POINTER 		:= DbInfo.BLOB_POINTER
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_LEN/*" />
DEFINE BLOB_LEN 			:= DbInfo.BLOB_LEN
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_TYPE/*" />
DEFINE BLOB_TYPE 			:= DbInfo.BLOB_TYPE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_EXPORT/*" />
DEFINE BLOB_EXPORT 			:= DbInfo.BLOB_EXPORT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_ROOT_UNLOCK/*" />
DEFINE BLOB_ROOT_UNLOCK 	:= DbInfo.BLOB_ROOT_UNLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_ROOT_PUT/*" />
DEFINE BLOB_ROOT_PUT 		:= DbInfo.BLOB_ROOT_PUT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_ROOT_GET/*" />
DEFINE BLOB_ROOT_GET 		:= DbInfo.BLOB_ROOT_GET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_ROOT_LOCK/*" />
DEFINE BLOB_ROOT_LOCK 		:= DbInfo.BLOB_ROOT_LOCK
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_IMPORT/*" />
DEFINE BLOB_IMPORT 			:= DbInfo.BLOB_IMPORT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_DIRECT_PUT/*" />
DEFINE BLOB_DIRECT_PUT 		:= DbInfo.BLOB_DIRECT_PUT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_DIRECT_GET/*" />
DEFINE BLOB_DIRECT_GET 		:= DbInfo.BLOB_DIRECT_GET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_GET/*" />
DEFINE BLOB_GET 			:= DbInfo.BLOB_GET
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_DIRECT_EXPORT/*" />
DEFINE BLOB_DIRECT_EXPORT 	:= DbInfo.BLOB_DIRECT_EXPORT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_DIRECT_IMPORT/*" />
DEFINE BLOB_DIRECT_IMPORT 	:= DbInfo.BLOB_DIRECT_IMPORT
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_NMODE/*" />
DEFINE BLOB_NMODE 			:= DbInfo.BLOB_NMODE
/// <include file="XSharp.CoreDefines.xml" path="members/DbOrder_Info.BLOB_USER/*" />
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
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBF_STD/*" />
DEFINE DB_DBF_STD              := 1
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBF_VFP/*" />
DEFINE DB_DBF_VFP              := 2


/* Numeric MEMO TYPES */
/// <include file="XSharp.CoreDefines.xml" path="members/DB_MEMO_NONE/*" />
DEFINE DB_MEMO_NONE            := 0
/// <include file="XSharp.CoreDefines.xml" path="members/DB_MEMO_DBT/*" />
DEFINE DB_MEMO_DBT             := 1
/// <include file="XSharp.CoreDefines.xml" path="members/DB_MEMO_FPT/*" />
DEFINE DB_MEMO_FPT             := 2
/// <include file="XSharp.CoreDefines.xml" path="members/DB_MEMO_SMT/*" />
DEFINE DB_MEMO_SMT             := 3

/* MEMO EXTENDED TYPES */
DEFINE DB_MEMOVER_STD          := 1
DEFINE DB_MEMOVER_SIX          := 2
DEFINE DB_MEMOVER_FLEX         := 3
DEFINE DB_MEMOVER_CLIP         := 4


/* LOCK SCHEMES */
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_DEFAULT/*" />
DEFINE DB_DBFLOCK_DEFAULT      := 0
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_CLIPPER/*" />
DEFINE DB_DBFLOCK_CLIPPER      := 1
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_COMIX/*" />
DEFINE DB_DBFLOCK_COMIX        := 2
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_VFP/*" />
DEFINE DB_DBFLOCK_VFP          := 3
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_HB32/*" />
DEFINE DB_DBFLOCK_HB32         := 4
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_HB64/*" />
DEFINE DB_DBFLOCK_HB64         := 5
/// <include file="XSharp.CoreDefines.xml" path="members/DB_DBFLOCK_CLIPPER2/*" />
DEFINE DB_DBFLOCK_CLIPPER2     := 6


// File Extensions
DEFINE DBT_MEMOEXT             := ".DBT"
DEFINE FPT_MEMOEXT             := ".FPT"
DEFINE SMT_MEMOEXT             := ".SMT"
DEFINE DBV_MEMOEXT             := ".DBV"

// Blocks
/// <include file="XSharp.CoreDefines.xml" path="members/DBT_DEFBLOCKSIZE/*" />
DEFINE DBT_DEFBLOCKSIZE        := 512
/// <include file="XSharp.CoreDefines.xml" path="members/FPT_DEFBLOCKSIZE/*" />
DEFINE FPT_DEFBLOCKSIZE        := 64
/// <include file="XSharp.CoreDefines.xml" path="members/SMT_DEFBLOCKSIZE/*" />
DEFINE SMT_DEFBLOCKSIZE        := 32

// Locks
DEFINE FPT_LOCKPOS             := 0
DEFINE FPT_LOCKSIZE            := 1
DEFINE FPT_ROOTBLOCK_OFFSET    := 536	/* Clipper 5.3 ROOT data block offset */

DEFINE SIX_ITEM_BUFSIZE        :=    14
DEFINE FLEX_ITEM_BUFSIZE       :=     8
DEFINE MAX_SIXFREEBLOCKS       :=    82
DEFINE MAX_FLEXFREEBLOCKS      :=   126
DEFINE FLEXGCPAGE_SIZE         :=  1010


// RDD Inheritance
DEFINE RDT_FULL            :=  1
DEFINE RDT_TRANSFER        :=  2
DEFINE RDT_HIDDEN          :=  8


/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ISDBF/*" />
DEFINE RDDI_ISDBF             := RddInfo.RDDI_ISDBF
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CANPUTREC/*" />
DEFINE RDDI_CANPUTREC         := RddInfo.RDDI_CANPUTREC
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DELIMITER/*" />
DEFINE RDDI_DELIMITER         := RddInfo.RDDI_DELIMITER
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SEPARATOR/*" />
DEFINE RDDI_SEPARATOR         := RddInfo.RDDI_SEPARATOR
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TABLEEXT/*" />
DEFINE RDDI_TABLEEXT          := RddInfo.RDDI_TABLEEXT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOEXT/*" />
DEFINE RDDI_MEMOEXT           := RddInfo.RDDI_MEMOEXT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDBAGEXT/*" />
DEFINE RDDI_ORDBAGEXT         := RddInfo.RDDI_ORDBAGEXT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDEREXT/*" />
DEFINE RDDI_ORDEREXT          := RddInfo.RDDI_ORDEREXT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ORDSTRUCTEXT/*" />
DEFINE RDDI_ORDSTRUCTEXT      := RddInfo.RDDI_ORDSTRUCTEXT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCAL/*" />
DEFINE RDDI_LOCAL             := RddInfo.RDDI_LOCAL
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_REMOTE/*" />
DEFINE RDDI_REMOTE            := RddInfo.RDDI_REMOTE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CONNECTION/*" />
DEFINE RDDI_CONNECTION        := RddInfo.RDDI_CONNECTION
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TABLETYPE/*" />
DEFINE RDDI_TABLETYPE         := RddInfo.RDDI_TABLETYPE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOTYPE/*" />
DEFINE RDDI_MEMOTYPE          := RddInfo.RDDI_MEMOTYPE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LARGEFILE/*" />
DEFINE RDDI_LARGEFILE         := RddInfo.RDDI_LARGEFILE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCKSCHEME/*" />
DEFINE RDDI_LOCKSCHEME        := RddInfo.RDDI_LOCKSCHEME
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_RECORDMAP/*" />
DEFINE RDDI_RECORDMAP         := RddInfo.RDDI_RECORDMAP
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ENCRYPTION/*" />
DEFINE RDDI_ENCRYPTION        := RddInfo.RDDI_ENCRYPTION
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_TRIGGER/*" />
DEFINE RDDI_TRIGGER           := RddInfo.RDDI_TRIGGER
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOLOCK /*" />
DEFINE RDDI_AUTOLOCK          := RddInfo.RDDI_AUTOLOCK

/* index parameters */
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRUCTORD/*" />
DEFINE RDDI_STRUCTORD           := RddInfo.RDDI_STRUCTORD
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRICTREAD/*" />
DEFINE RDDI_STRICTREAD          := RddInfo.RDDI_STRICTREAD
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_STRICTSTRUCT/*" />
DEFINE RDDI_STRICTSTRUCT        := RddInfo.RDDI_STRICTSTRUCT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_OPTIMIZE/*" />
DEFINE RDDI_OPTIMIZE            := RddInfo.RDDI_OPTIMIZE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_FORCEOPT/*" />
DEFINE RDDI_FORCEOPT            := RddInfo.RDDI_FORCEOPT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOOPEN/*" />
DEFINE RDDI_AUTOOPEN            := RddInfo.RDDI_AUTOOPEN
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOORDER/*" />
DEFINE RDDI_AUTOORDER           := RddInfo.RDDI_AUTOORDER
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AUTOSHARE/*" />
DEFINE RDDI_AUTOSHARE           := RddInfo.RDDI_AUTOSHARE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MULTITAG/*" />
DEFINE RDDI_MULTITAG            := RddInfo.RDDI_MULTITAG
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SORTRECNO/*" />
DEFINE RDDI_SORTRECNO           := RddInfo.RDDI_SORTRECNO
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MULTIKEY/*" />
DEFINE RDDI_MULTIKEY            := RddInfo.RDDI_MULTIKEY

/* memo parameters */
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ISDBF/*" />
DEFINE RDDI_MEMOBLOCKSIZE       := RddInfo.RDDI_MEMOBLOCKSIZE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOVERSION/*" />
DEFINE RDDI_MEMOVERSION         := RddInfo.RDDI_MEMOVERSION
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOGCTYPE/*" />
DEFINE RDDI_MEMOGCTYPE          := RddInfo.RDDI_MEMOGCTYPE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOREADLOCK/*" />
DEFINE RDDI_MEMOREADLOCK        := RddInfo.RDDI_MEMOREADLOCK
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_MEMOREUSE/*" />
DEFINE RDDI_MEMOREUSE           := RddInfo.RDDI_MEMOREUSE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_BLOB_SUPPORT/*" />
DEFINE RDDI_BLOB_SUPPORT        := RddInfo.RDDI_BLOB_SUPPORT

/* misc */
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PENDINGTRIGGER/*" />
DEFINE RDDI_PENDINGTRIGGER      := RddInfo.RDDI_PENDINGTRIGGER
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PENDINGPASSWORD/*" />
DEFINE RDDI_PENDINGPASSWORD     := RddInfo.RDDI_PENDINGPASSWORD
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_PASSWORD/*" />
DEFINE RDDI_PASSWORD            := RddInfo.RDDI_PASSWORD
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_LOCKRETRY/*" />
DEFINE RDDI_LOCKRETRY           := RddInfo.RDDI_LOCKRETRY
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DIRTYREAD/*" />
DEFINE RDDI_DIRTYREAD           := RddInfo.RDDI_DIRTYREAD
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_INDEXPAGESIZE/*" />
DEFINE RDDI_INDEXPAGESIZE       := RddInfo.RDDI_INDEXPAGESIZE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DECIMALS/*" />
DEFINE RDDI_DECIMALS            := RddInfo.RDDI_DECIMALS
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_SETHEADER/*" />
DEFINE RDDI_SETHEADER           := RddInfo.RDDI_SETHEADER

/* SQL */
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_CONNECT/*" />
DEFINE RDDI_CONNECT             := RddInfo.RDDI_CONNECT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_DISCONNECT/*" />
DEFINE RDDI_DISCONNECT          := RddInfo.RDDI_DISCONNECT
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_EXECUTE/*" />
DEFINE RDDI_EXECUTE             := RddInfo.RDDI_EXECUTE
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ERROR/*" />
DEFINE RDDI_ERROR               := RddInfo.RDDI_ERROR
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_ERRORNO/*" />
DEFINE RDDI_ERRORNO             := RddInfo.RDDI_ERRORNO
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_INSERTID/*" />
DEFINE RDDI_INSERTID            := RddInfo.RDDI_INSERTID
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_AFFECTEDROWS/*" />
DEFINE RDDI_AFFECTEDROWS        := RddInfo.RDDI_AFFECTEDROWS
/// <include file="XSharp.CoreDefines.xml" path="members/RddInfo.RDDI_QUERY/*" />
DEFINE RDDI_QUERY               := RddInfo.RDDI_QUERY

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

DEFINE FPTIT_TEXT         0x0001/* Text    * /

DEFINE FPTIT_OBJ          0x0002/* Object  * /

DEFINE FPTIT_SIX_NIL      0x0000/* NIL VALUE (USED ONLY IN ARRAYS) * /
DEFINE FPTIT_SIX_LNUM     0x0002/* LONG LE * /
DEFINE FPTIT_SIX_DNUM     0x0008/* DOUBLE LE * /

DEFINE FPTIT_SIX_LDATE    0x0020/* DATE (LONG LE) * /
DEFINE FPTIT_SIX_LOG      0x0080/* LOGIC * /

DEFINE FPTIT_SIX_CHAR     0x0400/* CHAR * /

DEFINE FPTIT_SIX_ARRAY    0x8000/* ARRAY * /
*/



// RDD Errors
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB                 := Subcodes.EDB
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SEEK            := Subcodes.EDB_SEEK
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SKIP            := Subcodes.EDB_SKIP
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_GOTO            := Subcodes.EDB_GOTO
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETRELATION     := Subcodes.EDB_SETRELATION
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_USE             := Subcodes.EDB_USE
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CREATEINDEX     := Subcodes.EDB_CREATEINDEX
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETORDER        := Subcodes.EDB_SETORDER
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETINDEX        := Subcodes.EDB_SETINDEX
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_FIELDNAME       := Subcodes.EDB_FIELDNAME
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_BADALIAS        := Subcodes.EDB_BADALIAS
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DUPALIAS        := Subcodes.EDB_DUPALIAS
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SETFILTER       := Subcodes.EDB_SETFILTER
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CYCLICREL       := Subcodes.EDB_CYCLICREL
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_CREATETABLE     := Subcodes.EDB_CREATETABLE
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_RDDNOTFOUND     := Subcodes.EDB_RDDNOTFOUND
// RESERVED EDB + 16
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_FIELDINDEX      := Subcodes.EDB_FIELDINDEX
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SELECT          := Subcodes.EDB_SELECT
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_SYMSELECT       := Subcodes.EDB_SYMSELECT
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_TOTAL           := Subcodes.EDB_TOTAL
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_RECNO           := Subcodes.EDB_RECNO
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_EXPRESSION      := Subcodes.EDB_EXPRESSION
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_EXPR_WIDTH      := Subcodes.EDB_EXPR_WIDTH
// RESERVED EDB + 24 - 29
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DRIVERLOAD      := Subcodes.EDB_DRIVERLOAD
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_PARAM           := Subcodes.EDB_PARAM
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOAREAS         := Subcodes.EDB_NOAREAS
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOMEM           := Subcodes.EDB_NOMEM
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOFIELDS        := Subcodes.EDB_NOFIELDS
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_BAD_ERROR_INFO  := Subcodes.EDB_BAD_ERROR_INFO
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_WRONGFIELDNAME  := Subcodes.EDB_WRONGFIELDNAME
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ORDDESTROY      := Subcodes.EDB_ORDDESTROY
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOINITFUNCTION  := Subcodes.EDB_NOINITFUNCTION
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ERRORINIT       := Subcodes.EDB_ERRORINIT
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_DBSTRUCT        := Subcodes.EDB_DBSTRUCT

/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOTABLE         := Subcodes.EDB_NOTABLE
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NOORDER         := Subcodes.EDB_NOORDER
/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_NODB            := Subcodes.EDB_NODB

/// <include file="CoreComments.xml" path="Comments/RDDError/*" />
DEFINE EDB_ASSERTION       := Subcodes.EDB_ASSERTION

// Xbase++ defines for DbScope()
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_TOP    := 0x01
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_BOTTOM := 0x02
/// <include file="CoreComments.xml" path="Comments/ScopeInfo/*" />
DEFINE SCOPE_BOTH   := 0xFF
