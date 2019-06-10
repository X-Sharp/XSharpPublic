//////////////////////////////////////////////////////////////////////
//
//  DMLB.CH
//
//  Contents:
//      Generic define constants for DbeInfo(), DbInfo() and FieldInfo()
//   
//////////////////////////////////////////////////////////////////////


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
