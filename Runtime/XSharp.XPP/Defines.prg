//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// These defines were all created from the XBase++ header files
// CLASS.CH
// default
DEFINE CLASS_HIDDEN         := 0x0001   
DEFINE CLASS_PROTECTED      := 0x0002
DEFINE CLASS_EXPORTED       := 0x0003

DEFINE VAR_ASSIGN_HIDDEN    := 0x0010  
DEFINE VAR_ASSIGN_PROTECTED := 0x0020
// default is CLASS_... value
DEFINE VAR_ASSIGN_EXPORTED  := 0x0030   

// default
DEFINE VAR_INSTANCE         := 0x0100   
DEFINE VAR_CLASS            := 0x0300
DEFINE VAR_CLASS_SHARED     := 0x0B00

// default
DEFINE METHOD_INSTANCE      := 0x0010   
DEFINE METHOD_CLASS         := 0x0020

DEFINE METHOD_ACCESS        := 0x0400
DEFINE METHOD_ASSIGN        := 0x0800

// defines for :classDescribe(<nMode>)
DEFINE CLASS_DESCR_ALL          := 0
DEFINE CLASS_DESCR_CLASSNAME    := 1
DEFINE CLASS_DESCR_SUPERCLASSES := 2
DEFINE CLASS_DESCR_MEMBERS      := 3
DEFINE CLASS_DESCR_METHODS      := 4
DEFINE CLASS_DESCR_SUPERDETAILS := 5

// index into member array returned by :classDescribe()
DEFINE CLASS_MEMBER_NAME         := 1
DEFINE CLASS_MEMBER_ATTR         := 2
DEFINE CLASS_MEMBER_TYPE         := 3

// index into superclass array returned by :classDescribe()
DEFINE CLASS_SUPERCLASS_NAME     := 1
DEFINE CLASS_SUPERCLASS_ATTR     := 2
DEFINE CLASS_SUPERCLASS_TYPE     := 3

// index into method array returned by :classDescribe()
DEFINE CLASS_METHOD_NAME        := 1
DEFINE CLASS_METHOD_ATTR        := 2
DEFINE CLASS_METHOD_BLOCK       := 3
DEFINE CLASS_METHOD_VARNAME     := 4   // ACCESS/ASSIGN
DEFINE CLASS_METHOD_TYPES       := 5   // => Array

//  COLLAT.CH
DEFINE COLLAT_SYSTEM         :=   0
DEFINE COLLAT_GERMAN         :=   1
DEFINE COLLAT_BRITISH        :=   2
DEFINE COLLAT_AMERICAN       :=   2
DEFINE COLLAT_FINNISH        :=   3
DEFINE COLLAT_FRENCH         :=   4
DEFINE COLLAT_DANISH         :=   5
DEFINE COLLAT_GREEK437       :=   6
DEFINE COLLAT_GREEK851       :=   7
DEFINE COLLAT_ICELANDIC850   :=   8
DEFINE COLLAT_ICELANDIC861   :=   9
DEFINE COLLAT_ITALIAN        :=  10
DEFINE COLLAT_NORWEGIAN      :=  11
DEFINE COLLAT_PORTUGUESE     :=  12
DEFINE COLLAT_SPANISH        :=  13
DEFINE COLLAT_SWEDISH        :=  14
DEFINE COLLAT_DUTCH          :=  15
DEFINE COLLAT_USER           :=  16
DEFINE COLLAT_ASCII          :=  -1   
DEFINE COLLAT_COUNT          :=  17

DEFINE CHARSET_OEM       := 1
DEFINE CHARSET_ANSI      := 0

//  DLL.CH
// calling convention constants
DEFINE  DLL_SYSTEM           :=     4
DEFINE  DLL_CDECL            :=     8
DEFINE  DLL_STDCALL          :=    32
DEFINE  DLL_XPPCALL          :=   128

DEFINE DLL_OSAPI             := DLL_STDCALL

// calling mode constants
DEFINE  DLL_CALLMODE_NORMAL    := 0x0000
DEFINE  DLL_CALLMODE_COPY      := 0x2000


// DllInfo() DEFINE s
DEFINE DLL_INFO_HANDLE       :=  1
DEFINE DLL_INFO_NAME         :=  2
DEFINE DLL_INFO_PATHNAME     :=  3
DEFINE DLL_INFO_LOADED       :=  4
DEFINE DLL_INFO_TYPE         :=  5
DEFINE DLL_INFO_LIST         :=  6
DEFINE DLL_INFO_PREFIX       :=  7
DEFINE DLL_INFO_UNLOADABLE   :=  8
DEFINE DLL_INFO_USAGELIST    :=  9
DEFINE DLL_INFO_FUNCLIST     := 10
DEFINE DLL_INFO_CLASSFUNCLIST:= 11
DEFINE DLL_INFO_IMPORTS      := 12
DEFINE DLL_TYPE_UNKNOWN      :=  0
DEFINE DLL_TYPE_GENERAL      :=  1
DEFINE DLL_TYPE_XPP_STATIC   :=  2
DEFINE DLL_TYPE_XPP_DYNAMIC  :=  3
DEFINE DLL_TYPE_XPP_DYNAMIC_NOUNLOAD := 4

/*
// DLLFUNCTION command
#command  DLLFUNCTION <Func>([<x,...>]) ;
                USING <sys:CDECL,OSAPI,STDCALL,SYSTEM> ;
                 FROM <(Dll)> ;
       => ;
             FUNCTION <Func>([<x>]);;
                LOCAL nDll:=DllLoad(<(Dll)>);;
                LOCAL xRet:=DllCall(nDll,__Sys(<sys>),<(Func)>[,<x>]);;
                      DllUnLoad(nDll);;
               RETURN xRet

#command  STATIC DLLFUNCTION <Func>([<x,...>]) ;
                USING <sys:CDECL,OSAPI,STDCALL,SYSTEM> ;
                 FROM <(Dll)> ;
       => ;
             STATIC FUNCTION <Func>([<x>]);;
                LOCAL nDll:=DllLoad(<(Dll)>);;
                LOCAL xRet:=DllCall(nDll,__Sys(<sys>),<(Func)>[,<x>]);;
                      DllUnLoad(nDll);;
               RETURN xRet

#xtrans __Sys( CDECL )     =>   DLL_CDECL  
#xtrans __Sys( OSAPI )     =>   DLL_OSAPI  
#xtrans __Sys( STDCALL )   =>   DLL_STDCALL
#xtrans __Sys( SYSTEM )    =>   DLL_SYSTEM 


*/

DEFINE RES_STRING         :=       6
DEFINE RES_VERSION        :=      16
DEFINE RES_RAWSTRING      :=    1006
DEFINE RES_VERSIONFIXED   :=    1016

// constants for LoadResource() - version return 
DEFINE RES_VERSION_KEY     :=     1
DEFINE RES_VERSION_VALUE   :=     2
                           
DEFINE RES_PRODVER_LS      :=     1
DEFINE RES_PRODVER_MS      :=     2
DEFINE RES_FILEVER_LS      :=     3
DEFINE RES_FILEVER_MS      :=     4
DEFINE RES_FILETIME_LS     :=     5
DEFINE RES_FILETIME_MS     :=     6

// some predefined module handles
DEFINE XPP_MOD_EXE     := 0
DEFINE XPP_MOD_NLS     := 0xFFFFFFFF
DEFINE XPP_MOD_RT1     := 0xFFFFFFFE
DEFINE XPP_MOD_UI1     := 0xFFFFFFFD
DEFINE XPP_MOD_UI2     := 0xFFFFFFFC



//  DMLB.CH
DEFINE DMLB_FILENAME           := 0x01
DEFINE DMLB_QUALIFIED_FILENAME := 0x02

//
// Defines for DMLB components
//
DEFINE COMPONENT_DATA       := 1
DEFINE COMPONENT_ORDER      := 2
DEFINE COMPONENT_RELATION   := 3
DEFINE COMPONENT_DICTIONARY := 4

//
// DBE defines to be used for DbeInfo()
//
// Name of the database engine
DEFINE DBE_NAME             := 1 
// Version of DBE
DEFINE DBE_VERSION          := 2 
// Producer of DBE
DEFINE DBE_MANUFACTURER     := 3 
// Supported data types
DEFINE DBE_DATATYPES        := 4 
// Default file extension
DEFINE DBE_EXTENSION        := 5 
// Offset for DBE specific defines
DEFINE DBE_USER          := 1000 

// Added to handle distributed locking
DEFINE DBE_LOCKMODE         := 6
DEFINE LOCKING_STANDARD     := 1
DEFINE LOCKING_EXTENDED     := 2

//
// DBO defines to be used for DbInfo()
//
// File name of database
DEFINE DBO_FILENAME         := 1 
// Alias of work area
DEFINE DBO_ALIAS            := 2 
// Number of relations
DEFINE DBO_RELATIONS        := 3 
// Number of orders
DEFINE DBO_ORDERS           := 4 
// .T. if DBO is in shared mode
DEFINE DBO_SHARED           := 5 
// .T. if storage is located on a remote drive
DEFINE DBO_REMOTE           := 6 
// Return Name of DBE responsive of this DBO
DEFINE DBO_DBENAME          := 7 
// .T. data is managed on the server side (Client/Server)
DEFINE DBO_SERVER           := 8
// .T. read only access 
DEFINE DBO_READONLY         := 9

//
// FLD_.. defines to be used for  FieldInfo()
//
// Data type of field at language level as character token
DEFINE FLD_TYPE                  := 1 
// Data type of field at language level as numeric
DEFINE FLD_TYPE_AS_NUMERIC       := 2
// Native data type at DBE level as character token
DEFINE FLD_NATIVETYPE            := 3
// Native data type at DBE level as numeric
DEFINE FLD_NATIVETYPE_AS_NUMERIC := 4 
// Field length
DEFINE FLD_LEN                   := 5
// Number of decimal places
DEFINE FLD_DEC                   := 6

//
// Defines for notifications from the work area (database object)
//
// Database will be closed
DEFINE DBO_CLOSE_REQUEST    := 1 
// A bulk operation will commence
DEFINE DBO_BULK_REQUEST     := 2 
// A bulk operation is finished
DEFINE DBO_BULK_COMPLETE    := 3 

// The order (controlling index) of a table has changed
DEFINE DBO_CHANGE_ORDER     := 4 

// A row (record) of a table was changed
DEFINE DBO_TABLE_UPDATE     := 5 
// A row (record) of a table was deleted
DEFINE DBO_TABLE_DELETED    := 6 
// A new row (record) was added to a table
DEFINE DBO_TABLE_APPEND     := 7 

// The record pointer (cursor of a table) will change
DEFINE DBO_MOVE_PROLOG      := 8 
// The record pointer was changed
DEFINE DBO_MOVE_DONE        := 9 
// Only after DbGoBottom()
DEFINE DBO_GOBOTTOM        := 10 
// Only after DbGoTop()
DEFINE DBO_GOTOP           := 11 

// Reflects changes in DbSetDescend()
DEFINE DBO_ORDER_REVERSED  := 12

// Reflects changes of rowset due to Filter, Deleted or Scope changes
DEFINE DBO_ROWSET_CHANGED  := 13

// OrdSetFocus() , SET ORDER TO changes
DEFINE DBO_ORDER_CHANGED   := 14

DEFINE DBO_ROWSET_CHANGING := 15

// Offset for DBE specific notification messages
DEFINE DBO_NOTIFY_USER   := 1000 


//
// Defines for Workspace Mamangement/Access
//
// Primary Workspace of Thread
DEFINE DB_WORKSPACE        :=  1 
// Virtual Workspace used to exchange Workareas between Workspaces.
DEFINE DB_ZEROSPACE        :=  2 
// The alert space collects completions of asyncronous database operations.
DEFINE DB_ALERTSPACE       :=  3 


/// Specifies the locking behaviour inside/outside of transactions
DEFINE LOCKING_EXCLUSIVE              :=    1
DEFINE LOCKING_SHARED                 :=    2  
DEFINE LOCKING_UPDATE                 :=    4
DEFINE LOCKING_FORCESINGLE            :=  128
                                      
DEFINE ISOLATIONLEVEL_READ_UNCOMMITED :=    1
DEFINE ISOLATIONLEVEL_READ_COMMITED   :=    2
DEFINE ISOLATIONLEVEL_READ_REPEATABLE :=    4
DEFINE ISOLATIONLEVEL_SERIALIZABLE    :=    8
DEFINE ISOLATIONLEVEL_SNAPSHOT        :=   16
DEFINE ISOLATIONLEVEL_UNUSED1         :=   32
DEFINE ISOLATIONLEVEL_UNUSED2         :=   64
DEFINE ISOLATIONLEVEL_UNUSED3         :=  128

//  DELDBE.CH


DEFINE  DELDBE_MODE             := DBE_USER+1
DEFINE  DELDBE_RECORD_TOKEN     := DBE_USER+2
DEFINE  DELDBE_FIELD_TOKEN      := DBE_USER+3
DEFINE  DELDBE_DECIMAL_TOKEN    := DBE_USER+4
DEFINE  DELDBE_DELIMITER_TOKEN  := DBE_USER+5
DEFINE  DELDBE_LOGICAL_TOKEN    := DBE_USER+6
DEFINE  DELDBE_MAX_BUFFERSIZE   := DBE_USER+7
DEFINE  DELDBE_FIELD_TYPES      := DBE_USER+8
DEFINE  DELDBE_IGNORE_CTRL_CHAR := DBE_USER+9
DEFINE  DELDBE_SHARED_SUPPORT   := DBE_USER+10
                                 
DEFINE  DELDBO_MODE             := DBE_USER+1

DEFINE  DELDBE_SINGLEFIELD      := 1
DEFINE  DELDBE_MULTIFIELD       := 2
DEFINE  DELDBE_AUTOFIELD        := 3

//  DBFDBE.CH
DEFINE  DBFDBE_MEMOFILE_EXT    := DBE_USER+1
DEFINE  DBFDBE_MEMOBLOCKSIZE   := DBE_USER+2
DEFINE  DBFDBE_LOCKMODE        := DBE_USER+3
DEFINE  DBFDBE_LOCKOFFSET      := DBE_USER+4
DEFINE  DBFDBE_LOCKDELAY       := DBE_USER+5
DEFINE  DBFDBE_LOCKRETRY       := DBE_USER+6
DEFINE  DBFDBE_CACHESIZE       := DBE_USER+7
DEFINE  DBFDBE_LIFETIME        := DBE_USER+8
DEFINE  DBFDBE_LOCKDELAY_MIN   := DBE_USER+9
DEFINE  DBFDBE_LOCKRETRY_MIN   := DBE_USER+10
DEFINE  DBFDBE_ANSI            := DBE_USER+11

// DBO instances of DBFDBE can use:
//
DEFINE  DBFDBO_LOCKMODE        := DBE_USER+1
DEFINE  DBFDBO_LOCKOFFSET      := DBE_USER+2
DEFINE  DBFDBO_LOCKDELAY       := DBE_USER+3
DEFINE  DBFDBO_LOCKRETRY       := DBE_USER+4
DEFINE  DBFDBO_DBFHANDLE       := DBE_USER+5
DEFINE  DBFDBO_DBTHANDLE       := DBE_USER+6
DEFINE  DBFDBO_ANSI            := DBE_USER+7


DEFINE DBF_NOLOCK             := 1      

// enhanced OS/2 mode performs autolocking for
// append, read/write, delete, recall
DEFINE DBF_AUTOLOCK           := 2      



//  DBTYPES.CH
DEFINE XPP_BLOB           := 16384   

