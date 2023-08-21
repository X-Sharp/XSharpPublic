///////////////////////////////////////////////////////////////////////////
// DB.vh
//
// Copyright c Grafx Database Systems, Inc.  All rights reserved.
//
// Vulcan.NET Database-related preprocessor directives
//

// Field array
#define DBS_NAME  1
#define DBS_TYPE  2
#define DBS_LEN   3
#define DBS_DEC   4
#define DBS_ALIAS 5

#define DBI_ISDBF 1
#define DBI_CANPUTREC 2
#define DBI_GETHEADERSIZE 3
#define DBI_LASTUPDATE 4
#define DBI_GETDELIMITER 5
#define DBI_SETDELIMITER 6
#define DBI_GETRECSIZE 7
#define DBI_GETLOCKARRAY 8
#define DBI_TABLEEXT 9
#define DBI_READONLY 10

#define DBI_ISFLOCK 20
#define DBI_CHILDCOUNT 22
#define DBI_FILEHANDLE 23
#define DBI_FULLPATH 24
#define DBI_ISANSI 25
#define DBI_BOF 26
#define DBI_EOF 27
#define DBI_DBFILTER 28
#define DBI_FOUND 29
#define DBI_FCOUNT 30
#define DBI_LOCKCOUNT 31
#define DBI_VALIDBUFFER 32
#define DBI_ALIAS 33
#define DBI_GETSCOPE 34
#define DBI_LOCKOFFSET 35
#define DBI_SHARED 36
#define DBI_MEMOEXT 37
#define DBI_MEMOHANDLE 38
#define DBI_BLOB_HANDLE 38
#define DBI_MEMOBLOCKSIZE 39
#define DBI_BLOB_INTEGRITY 40
#define DBI_CODEPAGE 41
#define DBI_BLOB_RECOVER 43
#define DBI_NEWINDEXLOCK 44
#define DBI_DB_VERSION 101
#define DBI_RDD_VERSION 102
#define DBI_RDD_LIST 103
#define DBI_MEMOFIELD 104
#define DBI_VO_MACRO_SYNTAX 105
#define DBI_RDD_OBJECT 106
#define DBI_USER 1000

#define DBOI_CONDITION 1
#define DBOI_EXPRESSION 2
#define DBOI_POSITION 3
#define DBOI_RECNO 4
#define DBOI_NAME 5
#define DBOI_NUMBER 6
#define DBOI_INDEXNAME 7
#define DBOI_INDEXEXT 8
#define DBOI_BAGNAME 7
#define DBOI_BAGEXT 8
#define DBOI_FULLPATH 20
#define DBOI_FILEHANDLE 21
#define DBOI_ISDESC 22
#define DBOI_ISCOND 23
#define DBOI_KEYTYPE 24
#define DBOI_KEYSIZE 25
#define DBOI_KEYCOUNT 26
#define DBOI_SETCODEBLOCK 27
#define DBOI_KEYDEC 28
#define DBOI_HPLOCKING 29
#define DBOI_LOCKOFFSET 35
#define DBOI_KEYADD 36
#define DBOI_KEYDELETE 37
#define DBOI_KEYVAL 38
#define DBOI_SCOPETOP 39
#define DBOI_SCOPEBOTTOM 40
#define DBOI_SCOPETOPCLEAR 41
#define DBOI_SCOPEBOTTOMCLEAR 42
#define DBOI_UNIQUE 43
#define DBOI_ORDERCOUNT 44
#define DBOI_CUSTOM 45
#define DBOI_SKIPUNIQUE 46
#define DBOI_KEYGOTO 47
#define DBOI_KEYSINCLUDED 48
#define DBOI_KEYNORAW 49
#define DBOI_OPTLEVEL 50
#define DBOI_KEYCOUNTRAW 51
#define DBOI_LOCK_ALL 100
#define DBOI_LOCK_FAIL 101
#define DBOI_HPLOCK_GATE 102
#define DBOI_USER 1000

#define TOPSCOPE 0
#define BOTTOMSCOPE 1

#define DBRI_DELETED 1
#define DBRI_LOCKED 2
#define DBRI_RECSIZE 3
#define DBRI_RECNO 4
#define DBRI_UPDATED 5
#define DBRI_BUFFPTR 6
#define DBRI_USER 1000

#define DBS_BLOB_TYPE 102
#define DBS_BLOB_LEN 103
#define DBS_BLOB_POINTER 198
#define DBS_BLOB_DIRECT_TYPE 222
#define DBS_BLOB_DIRECT_LEN 223
#define DBS_STRUCT 998
#define DBS_PROPERTIES 999
#define DBS_USER 1000

#define BLOB_INFO_HANDLE 201  // note: the BLOB_ defines have different values than in VO
#define BLOB_FILE_RECOVER 202
#define BLOB_FILE_INTEGRITY 203
#define BLOB_OFFSET 204
#define BLOB_POINTER 205
#define BLOB_LEN 206
#define BLOB_TYPE 207
#define BLOB_EXPORT 208
#define BLOB_ROOT_UNLOCK 209
#define BLOB_ROOT_PUT 210
#define BLOB_ROOT_GET 211
#define BLOB_ROOT_LOCK 212
#define BLOB_IMPORT 213
#define BLOB_DIRECT_PUT 214
#define BLOB_DIRECT_GET 215
#define BLOB_GET 216
#define BLOB_DIRECT_EXPORT 217
#define BLOB_DIRECT_IMPORT 218
#define BLOB_NMODE 219
#define BLOB_EXPORT_APPEND 220
#define BLOB_EXPORT_OVERWRITE 221

#define RDD_INFO                     100
#define _SET_MEMOBLOCKSIZE           101
#define _SET_DEFAULTRDD              102
#define _SET_MEMOEXT                 103
#define _SET_AUTOOPEN                104
#define _SET_AUTOORDER               105
#define _SET_HPLOCKING               106
#define _SET_NEWINDEXLOCK            107
#define _SET_AUTOSHARE               108
#define _SET_STRICTREAD              109
#define _SET_BLOB_CIRCULAR_ARRAY_REF 110
#define _SET_OPTIMIZE                111
#define _SET_FOXLOCK                 112
#define _SET_WINCODEPAGE             113
#define _SET_DOSCODEPAGE             114
#define RDD_INFO_MAX                 114

// defines used for the RL support in the DBFCDX driver
#define DBI_RL_AND          DBI_USER + 1
#define DBI_RL_CLEAR        DBI_USER + 2
#define DBI_RL_COUNT        DBI_USER + 3
#define DBI_RL_DESTROY      DBI_USER + 4
#define DBI_RL_EXFILTER     DBI_USER + 5
#define DBI_RL_GETFILTER    DBI_USER + 6
#define DBI_RL_HASMAYBE     DBI_USER + 7
#define DBI_RL_LEN          DBI_USER + 8
#define DBI_RL_MAYBEEVAL    DBI_USER + 9
#define DBI_RL_NEW          DBI_USER + 10
#define DBI_RL_NEWDUP       DBI_USER + 11
#define DBI_RL_NEWQUERY     DBI_USER + 12
#define DBI_RL_NEXTRECNO    DBI_USER + 13
#define DBI_RL_NOT          DBI_USER + 14
#define DBI_RL_OR           DBI_USER + 15
#define DBI_RL_PREVRECNO    DBI_USER + 16
#define DBI_RL_SET          DBI_USER + 17
#define DBI_RL_SETFILTER    DBI_USER + 18
#define DBI_RL_TEST         DBI_USER + 19

#define _SET_USER (RDD_INFO + 100)

// eof
