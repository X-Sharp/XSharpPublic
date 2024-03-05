USING XSharp.Internal
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.ADS

#pragma warning (9023, off) // taking the address of an unfixed expression

INTERNAL FUNCTION  _AdsGetString(pBytes as Byte PTR, nLen as LONG) AS STRING
    LOCAL cResult as STRING
    LOCAL nPos as LONG
    cResult := RuntimeState.WinEncoding:GetString(pBytes, nLen)
    nPos := cResult:IndexOf('\0')
    if nPos >= 0
        cResult := cResult:Substring(0, nPos)
    endif
    RETURN cResult


//typedef struct _ADD_FIELD_DESC_
//   {
//   UNSIGNED16  usFieldType;
//   UNSIGNED16  usFieldLength;
//   UNSIGNED16  usFieldDecimal;
//   } ADD_FIELD_DESC;

    /// <exclude />
[VoStructAttribute(6, 2)];
[StructLayout(LayoutKind.Explicit, Size := 6)];
STRUCTURE ADD_FIELD_DESC
   [FieldOffset(0)] PUBLIC usFieldType    AS WORD
   [FieldOffset(2)] PUBLIC usFieldLength  AS WORD
   [FieldOffset(4)] PUBLIC usFieldDecimal AS WORD
   PROPERTY FieldType       AS WORD GET usFieldType
   PROPERTY FieldLength     AS WORD GET usFieldLength
   PROPERTY FieldDecimal    AS WORD GET usFieldDecimal
END STRUCTURE


/*//typedef struct
//   {
//   UNSIGNED32 ulRecordNumber;       /* Record number that is locked */
//   } ADS_MGMT_RECORD_INFO;
//

/// <summary>
/// Structure used by the ADS MGMT API to return information about a locked record.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>
[VoStructAttribute(4, 4)];
STRUCTURE ADS_MGMT_RECORD_INFO
    PUBLIC ulRecordNumber AS DWORD      /* Record number that is locked */
    PROPERTY RecordNumber       AS DWORD GET ulRecordNumber
END STRUCTURE



//typedef struct
//   {
//   UNSIGNED16  usDays;        /* Number of days server has been up    */
//   UNSIGNED16  usHours;       /* Number of hours server has been up   */
//   UNSIGNED16  usMinutes;     /* Number of minutes server has been up */
//   UNSIGNED16  usSeconds;     /* Number of seconds server has been up */
//   } ADS_MGMT_UPTIME_INFO;
//
/// <summary>
/// Structure used by the ADS MGMT API to return information about the server's uptime.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(8, 2)];
[StructLayout(LayoutKind.Explicit, Size := 8)];
STRUCTURE ADS_MGMT_UPTIME_INFO
    [FieldOffset(0)] PUBLIC usDays        AS WORD  /* Number of days server has been up    */
    [FieldOffset(2)] PUBLIC usHours       AS WORD  /* Number of hours server has been up   */
    [FieldOffset(4)] PUBLIC usMinutes     AS WORD  /* Number of minutes server has been up */
    [FieldOffset(6)] PUBLIC usSeconds     AS WORD  /* Number of seconds server has been up */
    PROPERTY Days       AS WORD GET usDays
    PROPERTY Hours      AS WORD GET usHours
    PROPERTY Minutes    AS WORD GET usMinutes
    PROPERTY Seconds    AS WORD GET usSeconds

END STRUCTURE

//typedef struct
//   {
//   UNSIGNED32  ulInUse;       /* Number of items in use        */
//   UNSIGNED32  ulMaxUsed;     /* Max number of items ever used */
//   UNSIGNED32  ulRejected;    /* Number of items rejected      */
//   } ADS_MGMT_USAGE_INFO;


/// <summary>
/// Structure used by the ADS MGMT API to return information about the server's usage.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(12, 4)];
[StructLayout(LayoutKind.Explicit, Size := 12)];
STRUCTURE ADS_MGMT_USAGE_INFO
    [FieldOffset(0)] PUBLIC ulInUse       AS DWORD  /* Number of items in use        */
    [FieldOffset(4)] PUBLIC ulMaxUsed     AS DWORD  /* Max number of items ever used */
    [FieldOffset(8)] PUBLIC ulRejected    AS DWORD  /* Number of items rejected      */
    PROPERTY InUse       AS DWORD GET ulInUse
    PROPERTY MaxUsed     AS DWORD GET ulMaxUsed
    PROPERTY Rejected    AS DWORD GET ulRejected

END STRUCTURE



//typedef struct
//   {
//   UNSIGNED32           ulOperations;     /* Number operations since started */
//   UNSIGNED32           ulLoggedErrors;   /* Number logged errors            */
//   ADS_MGMT_UPTIME_INFO stUpTime;         /* Length of time ADS has been up  */
//   ADS_MGMT_USAGE_INFO  stUsers;          /* Users in use, max, rejected     */
//   ADS_MGMT_USAGE_INFO  stConnections;    /* Conns in use, max, rejected     */
//   ADS_MGMT_USAGE_INFO  stWorkAreas;      /* WAs in use, max, rejected       */
//   ADS_MGMT_USAGE_INFO  stTables;         /* Tables in use, max, rejected    */
//   ADS_MGMT_USAGE_INFO  stIndexes;        /* Indexes in use, max, rejected   */
//   ADS_MGMT_USAGE_INFO  stLocks;          /* Locks in use, max, rejected     */
//   ADS_MGMT_USAGE_INFO  stTpsHeaderElems; /* TPS header elems in use, max    */
//   ADS_MGMT_USAGE_INFO  stTpsVisElems;    /* TPS vis elems in use, max       */
//   ADS_MGMT_USAGE_INFO  stTpsMemoElems;   /* TPS memo elems in use, max      */
//   ADS_MGMT_USAGE_INFO  stWorkerThreads;  /* Worker threads in use, max      */
//   ADS_MGMT_USAGE_INFO  stQueries;        /* Queries in use, max, rejected   */
//   ADS_MGMT_USAGE_INFO  stStatefulUsers;  /* Stateful users in use          */
//   ADS_MGMT_USAGE_INFO  stStatelessUsers; /* Stateless users in use          */
//   } ADS_MGMT_ACTIVITY_INFO;

/// <summary>
/// Structure used by the ADS MGMT API to return information about the server's activity.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(172, 4)];
[StructLayout(LayoutKind.Explicit, Size := 172)];
STRUCTURE ADS_MGMT_ACTIVITY_INFO
    // use Explicit layout to declare buffers for nested structures
    [FieldOffset(  0)] PUBLIC ulOperations     AS DWORD                /* Number operations since started */
    [FieldOffset(  4)] PUBLIC ulLoggedErrors   AS DWORD                /* Number logged errors            */
    [FieldOffset(  8)] PUBLIC stUpTime         AS ADS_MGMT_UPTIME_INFO /* Length of time ADS has been up  */
    [FieldOffset( 16)] PUBLIC stUsers          AS ADS_MGMT_USAGE_INFO  /* Users in use, max, rejected     */
    [FieldOffset( 28)] PUBLIC stConnections    AS ADS_MGMT_USAGE_INFO  /* Conns in use, max, rejected     */
    [FieldOffset( 40)] PUBLIC stWorkAreas      AS ADS_MGMT_USAGE_INFO  /* WAs in use, max, rejected       */
    [FieldOffset( 52)] PUBLIC stTables         AS ADS_MGMT_USAGE_INFO  /* Tables in use, max, rejected    */
    [FieldOffset( 64)] PUBLIC stIndexes        AS ADS_MGMT_USAGE_INFO  /* Indexes in use, max, rejected   */
    [FieldOffset( 76)] PUBLIC stLocks          AS ADS_MGMT_USAGE_INFO  /* Locks in use, max, rejected     */
    [FieldOffset( 88)] PUBLIC stTpsHeaderElems AS ADS_MGMT_USAGE_INFO  /* TPS header elems in use, max    */
    [FieldOffset(100)] PUBLIC stTpsVisElems    AS ADS_MGMT_USAGE_INFO  /* TPS vis elems in use, max       */
    [FieldOffset(112)] PUBLIC stTpsMemoElems   AS ADS_MGMT_USAGE_INFO  /* TPS memo elems in use, max      */
    [FieldOffset(124)] PUBLIC stWorkerThreads  AS ADS_MGMT_USAGE_INFO  /* Worker threads in use, max      */
    [FieldOffset(136)] PUBLIC stQueries        AS ADS_MGMT_USAGE_INFO  /* Queries in use, max, rejected   */
    [FieldOffset(148)] PUBLIC stStatefulUsers  AS ADS_MGMT_USAGE_INFO  /* Stateful users in use           */
    [FieldOffset(160)] PUBLIC stStatelessUsers AS ADS_MGMT_USAGE_INFO  /* Stateless users in use          */
END STRUCTURE



//typedef struct
//   {
//   double      dPercentCheckSums;   /* % of pkts with checksum failures */
//   UNSIGNED32  ulTotalPackets;      /* Total packets received           */
//   UNSIGNED32  ulRcvPktOutOfSeq;    /* Receive packets out of sequence  */
//   UNSIGNED32  ulNotLoggedIn;       /* Packet owner not logged in       */
//   UNSIGNED32  ulRcvReqOutOfSeq;    /* Receive requests out of sequence */
//   UNSIGNED32  ulCheckSumFailures;  /* Checksum failures                */
//   UNSIGNED32  ulDisconnectedUsers; /* Server initiated disconnects     */
//   UNSIGNED32  ulPartialConnects;   /* Removed partial connections      */
//   UNSIGNED32  ulInvalidPackets;    /* Rcvd invalid packets (NT only)   */
//   UNSIGNED32  ulRecvFromErrors;    /* RecvFrom failed (NT only)        */
//   UNSIGNED32  ulSendToErrors;      /* SendTo failed (NT only)          */
//   } ADS_MGMT_COMM_STATS;
//


/// <summary>
/// Structure used by the ADS MGMT API to return information about the server's  communication statistics.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(48, 8)];
STRUCTURE ADS_MGMT_COMM_STATS
    PUBLIC  dPercentCheckSums   AS REAL8 /* % of pkts with checksum failures */
    PUBLIC  ulTotalPackets      AS DWORD /* Total packets received           */
    PUBLIC  ulRcvPktOutOfSeq    AS DWORD /* Receive packets out of sequence  */
    PUBLIC  ulNotLoggedIn       AS DWORD /* Packet owner not logged in       */
    PUBLIC  ulRcvReqOutOfSeq    AS DWORD /* Receive requests out of sequence */
    PUBLIC  ulCheckSumFailures  AS DWORD /* Checksum failures                */
    PUBLIC  ulDisconnectedUsers AS DWORD /* Server initiated disconnects     */
    PUBLIC  ulPartialConnects   AS DWORD /* Removed partial connections      */
    PUBLIC  ulInvalidPackets    AS DWORD /* Rcvd invalid packets (NT only)   */
    PUBLIC  ulRecvFromErrors    AS DWORD /* RecvFrom failed (NT only)        */
    PUBLIC  ulSendToErrors      AS DWORD /* SendTo failed (NT only)          */

    PROPERTY PercentCheckSums  AS REAL8 GET dPercentCheckSums
    PROPERTY TotalPackets      AS DWORD GET ulTotalPackets
    PROPERTY RcvPktOutOfSeq    AS DWORD GET ulRcvPktOutOfSeq
    PROPERTY NotLoggedIn       AS DWORD GET ulNotLoggedIn
    PROPERTY RcvReqOutOfSeq    AS DWORD GET ulRcvReqOutOfSeq
    PROPERTY CheckSumFailures  AS DWORD GET ulCheckSumFailures
    PROPERTY DisconnectedUsers AS DWORD GET ulDisconnectedUsers
    PROPERTY PartialConnects   AS DWORD GET ulPartialConnects
    PROPERTY InvalidPackets    AS DWORD GET ulInvalidPackets
    PROPERTY RecvFromErrors    AS DWORD GET ulRecvFromErrors
    PROPERTY SendToErrors      AS DWORD GET ulSendToErrors

END STRUCTURE




//typedef struct
//   {
//   UNSIGNED32  ulTotalConfigMem;          /* Total mem taken by cfg params */
//   UNSIGNED32  ulConnectionMem;           /* memory taken by connections   */
//   UNSIGNED32  ulWorkAreaMem;             /* memory taken by work areas    */
//   UNSIGNED32  ulTableMem;                /* memory taken by tables        */
//   UNSIGNED32  ulIndexMem;                /* memory taken by indexes       */
//   UNSIGNED32  ulLockMem;                 /* memory taken by locks         */
//   UNSIGNED32  ulUserBufferMem;           /* memory taken by user buffer   */
//   UNSIGNED32  ulTPSHeaderElemMem;        /* memory taken by TPS hdr elems */
//   UNSIGNED32  ulTPSVisibilityElemMem;    /* memory taken by TPS vis elems */
//   UNSIGNED32  ulTPSMemoTransElemMem;     /* mem taken by TPS memo elems   */
//   UNSIGNED32  ulReceiveEcbMem;           /* mem taken by rcv ECBs (NLM)   */
//   UNSIGNED32  ulSendEcbMem;              /* mem taken by send ECBs (NLM)  */
//   UNSIGNED32  ulWorkerThreadMem;         /* mem taken by worker threads   */
//   UNSIGNED32  ulQueryMem;                /* mem taken by queries          */
//   } ADS_MGMT_CONFIG_MEMORY;


/// <summary>
/// Structure used by the ADS MGMT API to return information about the server's memory configuration.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(56, 8)];
STRUCTURE ADS_MGMT_CONFIG_MEMORY
    PUBLIC ulTotalConfigMem          AS DWORD  /* Total mem taken by cfg params */
    PUBLIC ulConnectionMem           AS DWORD  /* memory taken by connections   */
    PUBLIC ulWorkAreaMem             AS DWORD  /* memory taken by work areas    */
    PUBLIC ulTableMem                AS DWORD  /* memory taken by tables        */
    PUBLIC ulIndexMem                AS DWORD  /* memory taken by indexes       */
    PUBLIC ulLockMem                 AS DWORD  /* memory taken by locks         */
    PUBLIC ulUserBufferMem           AS DWORD  /* memory taken by user buffer   */
    PUBLIC ulTPSHeaderElemMem        AS DWORD  /* memory taken by TPS hdr elems */
    PUBLIC ulTPSVisibilityElemMem    AS DWORD  /* memory taken by TPS vis elems */
    PUBLIC ulTPSMemoTransElemMem     AS DWORD  /* mem taken by TPS memo elems   */
    PUBLIC ulReceiveEcbMem           AS DWORD  /* mem taken by rcv ECBs (NLM)   */
    PUBLIC ulSendEcbMem              AS DWORD  /* mem taken by send ECBs (NLM)  */
    PUBLIC ulWorkerThreadMem         AS DWORD  /* mem taken by worker threads   */
    PUBLIC ulQueryMem                AS DWORD  /* mem taken by queries          */

    PROPERTY TotalConfigMem          AS DWORD  GET ulTotalConfigMem
    PROPERTY ConnectionMem           AS DWORD  GET ulConnectionMem
    PROPERTY WorkAreaMem             AS DWORD  GET ulWorkAreaMem
    PROPERTY TableMem                AS DWORD  GET ulTableMem
    PROPERTY IndexMem                AS DWORD  GET ulIndexMem
    PROPERTY LockMem                 AS DWORD  GET ulLockMem
    PROPERTY UserBufferMem           AS DWORD  GET ulUserBufferMem
    PROPERTY TPSHeaderElemMem        AS DWORD  GET ulTPSHeaderElemMem
    PROPERTY TPSVisibilityElemMem    AS DWORD  GET ulTPSVisibilityElemMem
    PROPERTY TPSMemoTransElemMem     AS DWORD  GET ulTPSMemoTransElemMem
    PROPERTY ReceiveEcbMem           AS DWORD  GET ulReceiveEcbMem
    PROPERTY SendEcbMem              AS DWORD  GET ulSendEcbMem
    PROPERTY WorkerThreadMem         AS DWORD  GET ulWorkerThreadMem
    PROPERTY QueryMem                AS DWORD  GET ulQueryMem


END STRUCTURE



//typedef struct
//   {
//   UNSIGNED8  aucTableName[ADS_MGMT_MAX_PATH]; /* Fully qualified table name */
//   UNSIGNED16 usLockType;                      /* Advantage locking mode     */
//   } ADS_MGMT_TABLE_INFO;

/// <summary>
/// Structure used by the ADS MGMT API to return information about a table.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>


[VoStructAttribute(262, 2)];
[StructLayout(LayoutKind.Explicit, Size := 262)];
STRUCTURE ADS_MGMT_TABLE_INFO
    // use Explicit layout to declare buffer for aucTableName
    PRIVATE CONST a1 := 0   AS LONG
    PRIVATE CONST a2 := a1 + ACE.ADS_MGMT_MAX_PATH AS LONG
    [FieldOffset(a1)] PUBLIC aucTableName    AS BYTE     /* Fully qualified table name */
    [FieldOffset(a2)] PUBLIC usLockType      AS WORD     /* Advantage locking mode     */

    PROPERTY TableName       AS STRING  GET _AdsGetString(@aucTableName,ACE.ADS_MGMT_MAX_PATH):Trim()
    PROPERTY LockType        AS WORD    GET usLockType

END STRUCTURE


//typedef struct
//   {
//   UNSIGNED8  aucIndexName[ADS_MGMT_MAX_PATH]; /* Fully qualified table name */
//   } ADS_MGMT_INDEX_INFO;

/// <summary>
/// Structure used by the ADS MGMT API to return information about an index.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(260, 1)];
[StructLayout(LayoutKind.Explicit, Size := 260)];
STRUCTURE ADS_MGMT_INDEX_INFO
    // use Explicit layout to declare buffer for aucIndexName
    [FieldOffset(0)] PUBLIC aucIndexName   AS BYTE   /* Fully qualified index name */
    PROPERTY IndexName       AS STRING  GET _AdsGetString(@aucIndexName,ACE.ADS_MGMT_MAX_PATH):Trim()

END STRUCTURE

//typedef struct
//   {
//   UNSIGNED32 ulThreadNumber;                 /* Thread Number               */
//   UNSIGNED16 usOpCode;                       /* Operation in progress       */
//   UNSIGNED8  aucUserName[ADS_MAX_USER_NAME]; /* Name of user                */
//   UNSIGNED16 usConnNumber;                   /* NetWare conn num (NLM only) */
//   UNSIGNED16 usReserved1;                    /* Reserved                    */
//   UNSIGNED8  aucOSUserLoginName[ADS_MAX_USER_NAME]; /* OS user login name   */
//   } ADS_MGMT_THREAD_ACTIVITY;


/// <summary>
/// Structure used by the ADS MGMT API to return information about worker threads.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(110, 4)];
[StructLayout(LayoutKind.Explicit, Size := 110)];
STRUCTURE ADS_MGMT_THREAD_ACTIVITY
    // use Explicit layout to declare buffer for aucUserName and aucOSUserLoginName
    PRIVATE CONST a1 := 0  AS LONG
    PRIVATE CONST a2 := a1 + SIZEOF(DWORD)  AS LONG
    PRIVATE CONST a3 := a2 + SIZEOF(WORD) AS LONG
    PRIVATE CONST a4 := a3 + ACE.ADS_MAX_USER_NAME AS LONG
    PRIVATE CONST a5 := a4 + sizeof(WORD) AS LONG
    PRIVATE CONST a6 := a5 + sizeof(WORD) AS LONG
    [FieldOffset(a1)] PUBLIC ulThreadNumber            AS DWORD   /* Thread Number               */
    [FieldOffset(a2)] PUBLIC usOpCode                  AS WORD    /* Operation in progress       */
    [FieldOffset(a3)] PUBLIC aucUserName               AS BYTE    /* Name of user                */
    [FieldOffset(a4)] PUBLIC usConnNumber              AS WORD    /* NetWare conn num (NLM only) */
    [FieldOffset(a5)] PUBLIC usReserved1               AS WORD    /* Reserved                    */
    [FieldOffset(a6)] PUBLIC aucOSUserLoginName        AS BYTE    /* OS User Login Name          */

    PROPERTY ThreadNumber            AS DWORD   GET ulThreadNumber
    PROPERTY OpCode                  AS WORD    GET usOpCode
    PROPERTY UserName                AS STRING  GET _AdsGetString(@aucUserName,ACE.ADS_MAX_USER_NAME):Trim()
    PROPERTY ConnNumber              AS WORD    GET usConnNumber
    PROPERTY Reserved1               AS WORD    GET usReserved1
    PROPERTY OSUserLoginName         AS STRING  GET _AdsGetString(@aucOSUserLoginName,ACE.ADS_MAX_USER_NAME):Trim()

END STRUCTURE



//typedef struct
//   {
//   UNSIGNED8  aucUserName[ADS_MAX_USER_NAME]; /* Name of connected user    */
//   UNSIGNED16 usConnNumber;                   /* NetWare conn # (NLM only) */
//   UNSIGNED8  aucAuthUserName[ADS_MAX_USER_NAME]; /* Dictionary user name  */
//   UNSIGNED8  aucAddress[ADS_MAX_ADDRESS_SIZE]; /* Network address of user */
//   UNSIGNED8  aucOSUserLoginName[ADS_MAX_USER_NAME]; /* OS user login name */
//   UNSIGNED8  aucTSAddress[ADS_MAX_ADDRESS_SIZE]; /* Terminal Services client IP Address */
//   UNSIGNED8  aucApplicationID[ADS_MAX_MGMT_APPID_SIZE]; /* application id */
//   UNSIGNED32 ulAveRequestCost;               /* estimated average cost of each server request */
//   UNSIGNED16 usReserved1;                    /* reserved to maintain byte alignment (ace.pas structs are not packed) */
//   } ADS_MGMT_USER_INFO;

/// <summary>
/// Structure used by the ADS MGMT API to return information about users.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(290, 4)];
[StructLayout(LayoutKind.Explicit, Size := 290)];
STRUCTURE ADS_MGMT_USER_INFO
    // use Explicit layout to declare buffer for several strings
    PRIVATE CONST a1 := 0   AS LONG
    PRIVATE CONST a2 := a1 + ACE.ADS_MAX_USER_NAME AS LONG
    PRIVATE CONST a3 := a2 + sizeof(WORD) AS LONG
    PRIVATE CONST a4 := a3 + ACE.ADS_MAX_USER_NAME AS LONG
    PRIVATE CONST a5 := a4 + ACE.ADS_MAX_ADDRESS_SIZE AS LONG
    PRIVATE CONST a6 := a5 + ACE.ADS_MAX_USER_NAME AS LONG
    PRIVATE CONST a7 := a6 + ACE.ADS_MAX_ADDRESS_SIZE AS LONG
    PRIVATE CONST a8 := a7 + ACE.ADS_MAX_MGMT_APPID_SIZE AS LONG
    PRIVATE CONST a9 := a8 + sizeof(DWORD) AS LONG

    [FieldOffset(a1)] PUBLIC aucUserName                      AS BYTE    /* Name of connected user    */
    [FieldOffset(a2)] PUBLIC usConnNumber                     AS WORD    /* NetWare conn # (NLM only) */
    [FieldOffset(a3)] PUBLIC aucAuthUserName                  AS BYTE    /* Data Dictionary user name */
    [FieldOffset(a4)] PUBLIC aucAddress                       AS BYTE    /* Network address of user   */
    [FieldOffset(a5)] PUBLIC aucOSUserLoginName               AS BYTE    /* OS User Login Name        */
    [FieldOffset(a6)] PUBLIC aucTSAddress                     AS BYTE    /* Terminal Services client IP Address */
    [FieldOffset(a7)] PUBLIC aucApplicationID                 AS BYTE    /* application id */
    [FieldOffset(a8)] PUBLIC ulAveRequestCost                 AS DWORD   /* estimated average cost of each server request */
    [FieldOffset(a9)] PUBLIC usReserved1                      AS WORD    /* reserved to maintain byte alignment (ace.pas structs are not packed) */
    PROPERTY UserName        as STRING GET _AdsGetString(@aucUserName,ACE.ADS_MAX_USER_NAME):Trim()
    PROPERTY ConnNumber      as WORD   GET usConnNumber
    PROPERTY AuthUserName    as STRING GET _AdsGetString(@aucAuthUserName,ACE.ADS_MAX_USER_NAME):Trim()
    PROPERTY Address         as STRING GET _AdsGetString(@aucAddress,ACE.ADS_MAX_ADDRESS_SIZE):Trim()
    PROPERTY OSUserLoginName as STRING GET _AdsGetString(@aucOSUserLoginName,ACE.ADS_MAX_USER_NAME):Trim()
    PROPERTY TSAddress       as STRING GET _AdsGetString(@aucTSAddress,ACE.ADS_MAX_ADDRESS_SIZE):Trim()
    PROPERTY ApplicationID   as STRING GET _AdsGetString(@aucApplicationID,ACE.ADS_MAX_MGMT_APPID_SIZE):Trim()
    PROPERTY AveRequestCost  as DWORD  GET ulAveRequestCost
    PROPERTY Reserved1       as WORD   GET  usReserved1

END STRUCTURE



//typedef struct
//   {
//   UNSIGNED32  ulUserOption;                            /* User option purchased                  */
//   UNSIGNED8   aucRegisteredOwner[ADS_REG_OWNER_LEN];   /* Registered owner                       */
//   UNSIGNED8   aucVersionStr[ADS_REVISION_LEN];         /* Advantage version                      */
//   UNSIGNED8   aucInstallDate[ADS_INST_DATE_LEN];       /* Install date string                    */
//   UNSIGNED8   aucOemCharName[ADS_OEM_CHAR_NAME_LEN];   /* OEM char language                      */
//   UNSIGNED8   aucAnsiCharName[ADS_ANSI_CHAR_NAME_LEN]; /* ANSI char language                     */
//   UNSIGNED8   aucEvalExpireDate[ADS_INST_DATE_LEN];    /* Eval expiration date                   */
//   UNSIGNED8   aucSerialNumber[ADS_SERIAL_NUM_LEN];     /* Serial number string                   */
//   UNSIGNED32  ulMaxStatefulUsers;                      /* How many stateful connections allowed */
//   UNSIGNED32  ulMaxStatelessUsers;                     /* How many stateless connections allowed */
//   } ADS_MGMT_INSTALL_INFO;


/// <summary>
/// Structure used by the ADS MGMT API to return information about the installation.<br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(144, 4)];
[StructLayout(LayoutKind.Explicit, Size := 144)];
STRUCTURE ADS_MGMT_INSTALL_INFO
    // use Explicit layout to declare buffer for several strings
    PRIVATE CONST a1 := 0   AS LONG
    PRIVATE CONST a2 := a1 + sizeof(DWORD) AS LONG
    PRIVATE CONST a3 := a2 + ACE.ADS_REG_OWNER_LEN AS LONG
    PRIVATE CONST a4 := a3 + ACE.ADS_REVISION_LEN  AS LONG
    PRIVATE CONST a5 := a4 + ACE.ADS_INST_DATE_LEN AS LONG
    PRIVATE CONST a6 := a5 + ACE.ADS_OEM_CHAR_NAME_LEN AS LONG
    PRIVATE CONST a7 := a6 + ACE.ADS_ANSI_CHAR_NAME_LEN AS LONG
    PRIVATE CONST a8 := a7 + ACE.ADS_INST_DATE_LEN AS LONG
    PRIVATE CONST a9 := a8 + ACE.ADS_SERIAL_NUM_LEN AS LONG
    PRIVATE CONST a10 := a9 + sizeof(DWORD) AS LONG

    [FieldOffset(a1)]  PUBLIC ulUserOption                     AS DWORD      /* User option purchased*/
    [FieldOffset(a2)]  PUBLIC aucRegisteredOwner               AS BYTE       /* Registered owner     */
    [FieldOffset(a3)]  PUBLIC aucVersionStr                    AS BYTE       /* Advantage version    */
    [FieldOffset(a4)]  PUBLIC aucInstallDate                   AS BYTE       /* Install date string  */
    [FieldOffset(a5)]  PUBLIC aucOemCharName                   AS BYTE       /* OEM char language    */
    [FieldOffset(a6)]  PUBLIC aucAnsiCharName                  AS BYTE       /* ANSI char language   */
    [FieldOffset(a7)]  PUBLIC aucEvalExpireDate                AS BYTE       /* Eval expiration date */
    [FieldOffset(a8)]  PUBLIC aucSerialNumber                  AS BYTE       /* Serial number string */
    [FieldOffset(a9)]  PUBLIC ulMaxStatefulUsers               AS DWORD      /* How many stateful connections allowed */
    [FieldOffset(a10)] PUBLIC ulMaxStatelessUsers              AS DWORD      /* How many stateless connections allowed */

    PROPERTY UserOption         as DWORD  GET ulUserOption
    PROPERTY RegisteredOwner    as STRING GET _AdsGetString(@aucRegisteredOwner,ACE.ADS_REG_OWNER_LEN):Trim()
    PROPERTY VersionStr         as STRING GET _AdsGetString(@aucVersionStr,ACE.ADS_REVISION_LEN):Trim()
    PROPERTY InstallDate        as STRING GET _AdsGetString(@aucInstallDate,ACE.ADS_INST_DATE_LEN):Trim()
    PROPERTY OemCharName        as STRING GET _AdsGetString(@aucOemCharName,ACE.ADS_OEM_CHAR_NAME_LEN):Trim()
    PROPERTY AnsiCharName       as STRING GET _AdsGetString(@aucAnsiCharName,ACE.ADS_ANSI_CHAR_NAME_LEN):Trim()
    PROPERTY EvalExpireDate     as STRING GET _AdsGetString(@aucEvalExpireDate,ACE.ADS_INST_DATE_LEN):Trim()
    PROPERTY SerialNumber       as STRING GET _AdsGetString(@aucSerialNumber,ACE.ADS_SERIAL_NUM_LEN):Trim()
    PROPERTY MaxStatefulUsers   as DWORD  GET ulMaxStatefulUsers
    PROPERTY MaxStatelessUsers  as DWORD  GET ulMaxStatelessUsers

END STRUCTURE


//typedef struct
//   {
//   UNSIGNED32  ulNumConnections;          /* number connections            */
//   UNSIGNED32  ulNumWorkAreas;            /* number work areas             */
//   UNSIGNED32  ulNumTables;               /* number tables                 */
//   UNSIGNED32  ulNumIndexes;              /* number indexes                */
//   UNSIGNED32  ulNumLocks;                /* number locks                  */
//   UNSIGNED32  ulUserBufferSize;          /* user buffer                   */
//   UNSIGNED32  ulStatDumpInterval;        /* statistics dump interval      */
//   UNSIGNED32  ulErrorLogMax;             /* max size of error log         */
//   UNSIGNED32  ulNumTPSHeaderElems;       /* number TPS header elems       */
//   UNSIGNED32  ulNumTPSVisibilityElems;   /* number TPS vis elems          */
//   UNSIGNED32  ulNumTPSMemoTransElems;    /* number TPS memo elems         */
//   UNSIGNED16  usNumReceiveECBs;          /* number rcv ECBs (NLM only)    */
//   UNSIGNED16  usNumSendECBs;             /* number send ECBs (NLM only)   */
//   UNSIGNED16  usNumBurstPackets;         /* number packets per burst      */
//   UNSIGNED16  usNumWorkerThreads;        /* number worker threads         */
//   UNSIGNED32  ulSortBuffSize;            /* index sort buffer size        */
//   UNSIGNED8   aucErrorLog[ADS_MAX_CFG_PATH];    /* error log path         */
//   UNSIGNED8   aucSemaphore[ADS_MAX_CFG_PATH];   /* semaphore file path    */
//   UNSIGNED8   aucTransaction[ADS_MAX_CFG_PATH]; /* TPS log file path      */
//
//   UNSIGNED8   ucReserved3;               /* reserved                      */
//   UNSIGNED8   ucReserved4;               /* reserved                      */
//   UNSIGNED16  usSendIPPort;              /* NT Service IP send port #     */
//   UNSIGNED16  usReceiveIPPort;           /* NT Service IP rcv port #      */
//   UNSIGNED8   ucUseIPProtocol;           /* Win9x only. Which protocol to use */
//   UNSIGNED8   ucFlushEveryUpdate;        /* Win9x specific                */
//
//   UNSIGNED32  ulGhostTimeout;            /* Diconnection time for partial connections */
//   UNSIGNED32  ulFlushFrequency;          /* For local server only         */
//
//   UNSIGNED32  ulKeepAliveTimeOut;     /* When not using semaophore files. In milliseconds */
//   UNSIGNED8   ucDisplayNWLoginNames;  /* Display connections using user names. */
//   UNSIGNED8   ucUseSemaphoreFiles;    /* Whether or not to use semaphore files */
//   UNSIGNED8   ucUseDynamicAOFs;
//   UNSIGNED8   ucUseInternet;          /* 0 if an Internet port is not specified. */
//
//   UNSIGNED16  usInternetPort;         /* Internet Port */
//   UNSIGNED16  usMaxConnFailures;      /* Maximum Internet connection failures allowed. */
//   UNSIGNED32  ulInternetKeepAlive;    /* In Milliseconds */
//
//   UNSIGNED16  usCompressionLevel;     /* Compression option at server.  ADS_COMPRESS_NEVER,
//                                        * ADS_COMPRESS_INTERNET, or ADS_COMPRESS_ALWAYS */
//   UNSIGNED32  ulNumQueries;           /* number of queries */
//   UNSIGNED16  usReceiveSSLPort;       /* Port number used for SSL */
//
//   } ADS_MGMT_CONFIG_PARAMS;


/// <summary>
/// Structure used by the ADS MGMT API to return information about the configuration parameters. <br/>
/// See the ADS documentation for a description of this structure.
/// </summary>

[VoStructAttribute(866, 4)];
[StructLayout(LayoutKind.Explicit, Size := 866)];
STRUCTURE ADS_MGMT_CONFIG_PARAMS
    PRIVATE CONST a01 := 0   AS LONG
    PRIVATE CONST a02 := a01 + sizeof(DWORD) AS LONG
    PRIVATE CONST a03 := a02 + sizeof(DWORD) AS LONG
    PRIVATE CONST a04 := a03 + sizeof(DWORD) AS LONG
    PRIVATE CONST a05 := a04 + sizeof(DWORD) AS LONG
    PRIVATE CONST a06 := a05 + sizeof(DWORD) AS LONG
    PRIVATE CONST a07 := a06 + sizeof(DWORD) AS LONG
    PRIVATE CONST a08 := a07 + sizeof(DWORD) AS LONG
    PRIVATE CONST a09 := a08 + sizeof(DWORD) AS LONG
    PRIVATE CONST a10 := a09 + sizeof(DWORD) AS LONG
    PRIVATE CONST a11 := a10 + sizeof(DWORD) AS LONG
    PRIVATE CONST a12 := a11 + sizeof(DWORD) AS LONG
    PRIVATE CONST a13 := a12 + sizeof(WORD) AS LONG
    PRIVATE CONST a14 := a13 + sizeof(WORD) AS LONG
    PRIVATE CONST a15 := a14 + sizeof(WORD) AS LONG
    PRIVATE CONST a16 := a15 + sizeof(WORD) AS LONG
    PRIVATE CONST a17 := a16 + sizeof(DWORD) AS LONG
    PRIVATE CONST a18 := a17 + ACE.ADS_MAX_CFG_PATH AS LONG
    PRIVATE CONST a19 := a18 + ACE.ADS_MAX_CFG_PATH AS LONG
    PRIVATE CONST a20 := a19 + ACE.ADS_MAX_CFG_PATH AS LONG
    PRIVATE CONST a21 := a20 + sizeof(BYTE)  AS LONG
    PRIVATE CONST a22 := a21 + sizeof(BYTE) AS LONG
    PRIVATE CONST a23 := a22 + sizeof(WORD) AS LONG
    PRIVATE CONST a24 := a23 + sizeof(WORD) AS LONG
    PRIVATE CONST a25 := a24 + sizeof(BYTE) AS LONG
    PRIVATE CONST a26 := a25 + sizeof(BYTE) AS LONG
    PRIVATE CONST a27 := a26 + sizeof(DWORD) AS LONG
    PRIVATE CONST a28 := a27 + sizeof(DWORD) AS LONG
    PRIVATE CONST a29 := a28 + sizeof(DWORD) AS LONG
    PRIVATE CONST a30 := a29 + sizeof(BYTE) AS LONG
    PRIVATE CONST a31 := a30 + sizeof(BYTE) AS LONG
    PRIVATE CONST a32 := a31 + sizeof(BYTE) AS LONG
    PRIVATE CONST a33 := a32 + sizeof(BYTE) AS LONG
    PRIVATE CONST a34 := a33 + sizeof(WORD) AS LONG
    PRIVATE CONST a35 := a34 + sizeof(WORD) AS LONG
    PRIVATE CONST a36 := a35 + sizeof(DWORD) AS LONG
    PRIVATE CONST a37 := a36 + sizeof(WORD) AS LONG
    PRIVATE CONST a38 := a37 + sizeof(DWORD) AS LONG

    [FieldOffset(a01)] PUBLIC ulNumConnections        AS DWORD    /* number connections            */
    [FieldOffset(a02)] PUBLIC ulNumWorkAreas          AS DWORD    /* number work areas             */
    [FieldOffset(a03)] PUBLIC ulNumTables             AS DWORD    /* number tables                 */
    [FieldOffset(a04)] PUBLIC ulNumIndexes            AS DWORD    /* number indexes                */
    [FieldOffset(a05)] PUBLIC ulNumLocks              AS DWORD    /* number locks                  */
    [FieldOffset(a06)] PUBLIC ulUserBufferSize        AS DWORD    /* user buffer                   */
    [FieldOffset(a07)] PUBLIC ulStatDumpInterval      AS DWORD    /* statistics dump interval      */
    [FieldOffset(a08)] PUBLIC ulErrorLogMax           AS DWORD    /* max size of error log         */
    [FieldOffset(a09)] PUBLIC ulNumTPSHeaderElems     AS DWORD    /* number TPS header elems       */
    [FieldOffset(a10)] PUBLIC ulNumTPSVisibilityElems AS DWORD    /* number TPS vis elems          */
    [FieldOffset(a11)] PUBLIC ulNumTPSMemoTransElems  AS DWORD    /* number TPS memo elems         */
    [FieldOffset(a12)] PUBLIC usNumReceiveECBs        AS WORD     /* number rcv ECBs (NLM only)    */
    [FieldOffset(a13)] PUBLIC usNumSendECBs           AS WORD     /* number send ECBs (NLM only)   */
    [FieldOffset(a14)] PUBLIC usNumBurstPackets       AS WORD     /* number packets per burst      */
    [FieldOffset(a15)] PUBLIC usNumWorkerThreads      AS WORD     /* number worker threads         */
    [FieldOffset(a16)] PUBLIC ulSortBuffSize          AS DWORD    /* index sort buffer size        */
    [FieldOffset(a17)] PUBLIC aucErrorLog             AS BYTE     /* error log path         */
    [FieldOffset(a18)] PUBLIC aucSemaphore            AS BYTE     /* semaphore file path    */
    [FieldOffset(a19)] PUBLIC aucTransaction          AS BYTE     /* TPS log file path      */

    [FieldOffset(a20)] PUBLIC ucReserved3            AS BYTE       /* reserved                      */
    [FieldOffset(a21)] PUBLIC ucReserved4            AS BYTE       /* reserved                      */
    [FieldOffset(a22)] PUBLIC usSendIPPort           AS WORD       /* NT Service IP send port #     */
    [FieldOffset(a23)] PUBLIC usReceiveIPPort        AS WORD       /* NT Service IP rcv port #      */
    [FieldOffset(a24)] PUBLIC ucUseIPProtocol        AS BYTE       /* Win9x only. Which protocol to use */
    [FieldOffset(a25)] PUBLIC ucFlushEveryUpdate     AS BYTE       /* Win9x specific                */

    [FieldOffset(a26)] PUBLIC ulGhostTimeout         AS DWORD      /* Diconnection time for partial connections */
    [FieldOffset(a27)] PUBLIC ulFlushFrequency       AS DWORD      /* For local server only         */

    [FieldOffset(a28)] PUBLIC ulKeepAliveTimeOut     AS DWORD   /* When not using semaophore files. In milliseconds */
    [FieldOffset(a29)] PUBLIC ucDisplayNWLoginNames  AS BYTE    /* Display connections using user names. */
    [FieldOffset(a30)] PUBLIC ucUseSemaphoreFiles    AS BYTE    /* Whether or not to use semaphore files */
    [FieldOffset(a31)] PUBLIC ucUseDynamicAOFs       AS BYTE
    [FieldOffset(a32)] PUBLIC ucUseInternet          AS BYTE    /* 0 if an Internet port is not specified. */

    [FieldOffset(a33)] PUBLIC usInternetPort         AS WORD    /* Internet Port */
    [FieldOffset(a34)] PUBLIC usMaxConnFailures      AS WORD    /* Maximum Internet connection failures allowed. */
    [FieldOffset(a35)] PUBLIC ulInternetKeepAlive    AS DWORD   /* In Milliseconds */

    [FieldOffset(a36)] PUBLIC usCompressionLevel	 AS WORD    /* Compression option at server.  ADS_COMPRESS_NEVER, */
                                             /* ADS_COMPRESS_INTERNET, or ADS_COMPRESS_ALWAYS */
    [FieldOffset(a37)] PUBLIC ulNumQueries           AS DWORD   /* number of queries */
    [FieldOffset(a38)] PUBLIC usReceiveSSLPort       AS WORD    /* Port number used for SSL */



    PROPERTY NumConnections        AS DWORD    GET ulNumConnections
    PROPERTY NumWorkAreas          AS DWORD    GET ulNumWorkAreas
    PROPERTY NumTables             AS DWORD    GET ulNumTables
    PROPERTY NumIndexes            AS DWORD    GET ulNumIndexes
    PROPERTY NumLocks              AS DWORD    GET ulNumLocks
    PROPERTY UserBufferSize        AS DWORD    GET ulUserBufferSize
    PROPERTY StatDumpInterval      AS DWORD    GET ulStatDumpInterval
    PROPERTY ErrorLogMax           AS DWORD    GET ulErrorLogMax
    PROPERTY NumTPSHeaderElems     AS DWORD    GET ulNumTPSHeaderElems
    PROPERTY NumTPSVisibilityElems AS DWORD    GET ulNumTPSVisibilityElems
    PROPERTY NumTPSMemoTransElems  AS DWORD    GET ulNumTPSMemoTransElems
    PROPERTY NumReceiveECBs        AS WORD     GET usNumReceiveECBs
    PROPERTY NumSendECBs           AS WORD     GET usNumSendECBs
    PROPERTY NumBurstPackets       AS WORD     GET usNumBurstPackets
    PROPERTY NumWorkerThreads      AS WORD     GET usNumWorkerThreads
    PROPERTY SortBuffSize          AS DWORD    GET ulSortBuffSize
    PROPERTY ErrorLog              AS STRING   GET _AdsGetString(@aucErrorLog,ACE.ADS_MAX_CFG_PATH):Trim()
    PROPERTY Semaphore             AS STRING   GET _AdsGetString(@aucSemaphore,ACE.ADS_MAX_CFG_PATH):Trim()
    PROPERTY Transaction           AS STRING   GET _AdsGetString(@aucTransaction,ACE.ADS_MAX_CFG_PATH):Trim()

    PROPERTY Reserved3             AS BYTE   GET ucReserved3
    PROPERTY Reserved4             AS BYTE   GET ucReserved4
    PROPERTY SendIPPort            AS WORD   GET usSendIPPort
    PROPERTY ReceiveIPPort         AS WORD   GET usReceiveIPPort
    PROPERTY UseIPProtocol         AS BYTE   GET ucUseIPProtocol
    PROPERTY FlushEveryUpdate      AS BYTE   GET ucFlushEveryUpdate

   PROPERTY GhostTimeout          AS DWORD  GET ulGhostTimeout
   PROPERTY FlushFrequency        AS DWORD  GET ulFlushFrequency

   PROPERTY KeepAliveTimeOut      AS DWORD GET ulKeepAliveTimeOut
   PROPERTY DisplayNWLoginNames   AS BYTE  GET ucDisplayNWLoginNames
   PROPERTY UseSemaphoreFiles     AS BYTE  GET ucUseSemaphoreFiles
   PROPERTY UseDynamicAOFs        AS BYTE  GET ucUseDynamicAOFs
   PROPERTY UseInternet           AS BYTE  GET ucUseInternet

   PROPERTY InternetPort          AS WORD  GET usInternetPort
   PROPERTY MaxConnFailures       AS WORD  GET usMaxConnFailures
   PROPERTY InternetKeepAlive     AS DWORD GET ulInternetKeepAlive

   PROPERTY CompressionLevel	  AS WORD    GET usCompressionLevel
   PROPERTY NumQueries            AS DWORD   GET ulNumQueries
   PROPERTY ReceiveSSLPort        AS WORD    GET usReceiveSSLPort



END STRUCTURE


END NAMESPACE


DEFINE ADS_MGMT_ADT_LOCKING :=           ACE.ADS_MGMT_ADT_LOCKING
DEFINE ADS_MGMT_CDX_LOCKING :=           ACE.ADS_MGMT_CDX_LOCKING
DEFINE ADS_MGMT_COMIX_LOCKING :=         ACE.ADS_MGMT_COMIX_LOCKING

DEFINE ADS_MGMT_FILE_LOCK :=       ACE.ADS_MGMT_FILE_LOCK

/*
 * Constants for MgGetInstallInfo
 */
DEFINE ADS_MGMT_LINUX_SERVER :=             ACE.ADS_MGMT_LINUX_SERVER
DEFINE ADS_MGMT_LINUX_SERVER_64_BIT :=      ACE.ADS_MGMT_LINUX_SERVER_64_BIT


/*
 * Constants for AdsMgGetLockOwner
 */
DEFINE ADS_MGMT_LOCAL_SERVER :=             ACE.ADS_MGMT_LOCAL_SERVER
DEFINE ADS_MGMT_MAX_PATH :=              ACE.ADS_MGMT_MAX_PATH
DEFINE ADS_MGMT_NETWARE_SERVER :=           ACE.ADS_MGMT_NETWARE_SERVER
DEFINE ADS_MGMT_NETWARE4_OR_OLDER_SERVER := ACE.ADS_MGMT_NETWARE4_OR_OLDER_SERVER
DEFINE ADS_MGMT_NETWARE5_OR_NEWER_SERVER := ACE.ADS_MGMT_NETWARE5_OR_NEWER_SERVER
DEFINE ADS_MGMT_NO_LOCK :=         ACE.ADS_MGMT_NO_LOCK
DEFINE ADS_MGMT_NT_SERVER :=                ACE.ADS_MGMT_NT_SERVER
DEFINE ADS_MGMT_NT_SERVER_64_BIT :=         ACE.ADS_MGMT_NT_SERVER_64_BIT
DEFINE ADS_MGMT_NTX_LOCKING :=           ACE.ADS_MGMT_NTX_LOCKING
DEFINE ADS_MGMT_PROPRIETARY_LOCKING :=   ACE.ADS_MGMT_PROPRIETARY_LOCKING
DEFINE ADS_MGMT_RECORD_LOCK :=     ACE.ADS_MGMT_RECORD_LOCK
DEFINE ADS_MGMT_WIN9X_SERVER :=             ACE.ADS_MGMT_WIN9X_SERVER

DEFINE ADS_MAX_ADDRESS_SIZE :=    ACE.ADS_MAX_ADDRESS_SIZE

/*
 * Management API structures
 */
DEFINE ADS_MAX_ADI_PAGESIZE :=     ACE.ADS_MAX_ADI_PAGESIZE


/* data types */
DEFINE ADS_MAX_CFG_PATH :=         ACE.ADS_MAX_CFG_PATH

/*
 * Constants for AdsMgGetServerType
 * Note ADS_MGMT_NETWARE_SERVER remains for backwards compatibility only
 */
DEFINE ADS_MAX_CHAR_SETS :=              ACE.ADS_MAX_CHAR_SETS

/* Options for notification events */
DEFINE ADS_MAX_DATEMASK :=         ACE.ADS_MAX_DATEMASK
DEFINE ADS_MAX_DBF_FIELD_NAME :=   ACE.ADS_MAX_DBF_FIELD_NAME    /* maximum length of field name in a DBF */
DEFINE ADS_MAX_ERROR_LEN :=        ACE.ADS_MAX_ERROR_LEN
DEFINE ADS_MAX_FIELD_NAME :=       ACE.ADS_MAX_FIELD_NAME

/* Table properties between 200 and 299 */

/* Common properties numbers < 100 */
DEFINE ADS_MAX_INDEX_EXPR_LEN :=   ACE.ADS_MAX_INDEX_EXPR_LEN   /* this is only accurate for index expressions */
DEFINE ADS_MAX_INDEXES :=          ACE.ADS_MAX_INDEXES        /* physical index files, NOT index orders */
DEFINE ADS_MAX_KEY_LENGTH :=       ACE.ADS_MAX_KEY_LENGTH     /* maximum key value length.  This is the max key length
                                          * of ADI indexes.  Max CDX key length is 240.  Max
                                          * NTX key length is 256 */
DEFINE ADS_MAX_MGMT_APPID_SIZE :=  ACE.ADS_MAX_MGMT_APPID_SIZE



DEFINE ADS_MAX_OBJECT_NAME :=      ACE.ADS_MAX_OBJECT_NAME   /* maximum length of DD object name */


/*
 * Valid range of page sizes for ADI indexes.  The default page size is 512
 * bytes.  Before using another page size, please read the section titled
 * "Index Page Size" in the Advantage Client Engine help file (ace.hlp)
 */
DEFINE ADS_MAX_PATH :=             ACE.ADS_MAX_PATH
DEFINE ADS_MAX_TABLE_NAME :=       ACE.ADS_MAX_TABLE_NAME
DEFINE ADS_MAX_TAG_NAME :=         ACE.ADS_MAX_TAG_NAME
DEFINE ADS_MAX_TAGS :=             ACE.ADS_MAX_TAGS
DEFINE ADS_MAX_USER_NAME :=        ACE.ADS_MAX_USER_NAME

DEFINE ADS_REG_OWNER_LEN :=        ACE.ADS_REG_OWNER_LEN
DEFINE ADS_REVISION_LEN :=         ACE.ADS_REVISION_LEN
DEFINE ADS_INST_DATE_LEN :=        ACE.ADS_INST_DATE_LEN
DEFINE ADS_OEM_CHAR_NAME_LEN :=    ACE.ADS_OEM_CHAR_NAME_LEN
DEFINE ADS_ANSI_CHAR_NAME_LEN :=   ACE.ADS_ANSI_CHAR_NAME_LEN
DEFINE ADS_SERIAL_NUM_LEN :=       ACE.ADS_SERIAL_NUM_LEN


