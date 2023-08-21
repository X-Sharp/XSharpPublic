VOSTRUCT _WINSERVICE_STATUS
	MEMBER  dwServiceType               AS DWORD
	MEMBER  dwCurrentState              AS DWORD
	MEMBER  dwControlsAccepted          AS DWORD
	MEMBER  dwWin32ExitCode             AS DWORD
	MEMBER  dwServiceSpecificExitCode   AS DWORD
	MEMBER  dwCheckPoint                AS DWORD
	MEMBER  dwWaitHint                  AS DWORD



	//
	// Structures for the Lock API functions
	//

VOSTRUCT _WINQUERY_SERVICE_LOCK_STATUS
	MEMBER  fIsLocked       AS DWORD
	MEMBER  lpLockOwner     AS PSZ
	MEMBER  dwLockDuration  AS DWORD


	//
	// Query Service Configuration Structure
	//

VOSTRUCT _WINQUERY_SERVICE_CONFIG
	MEMBER  dwServiceType       AS DWORD
	MEMBER  dwStartType         AS DWORD
	MEMBER  dwErrorControl      AS DWORD
	MEMBER  lpBinaryPathName    AS PSZ
	MEMBER  lpLoadOrderGroup    AS PSZ
	MEMBER  dwTagId             AS DWORD
	MEMBER  lpDependencies      AS PSZ
	MEMBER  lpServiceStartName  AS PSZ
	MEMBER  lpDisplayName       AS PSZ

VOSTRUCT _SERVICE_TABLE_ENTRY
	MEMBER  lpServiceName		AS PSZ
	MEMBER	lpServiceProc		AS /*LPSERVICE_MAIN_FUNCTION*/ PTR

VOSTRUCT _SERVICE_DISPATCH_TABLE
	MEMBER 	TableEntry1			IS _SERVICE_TABLE_ENTRY
	MEMBER 	TableEntry2			IS _SERVICE_TABLE_ENTRY


_DLL FUNC ChangeServiceConfig(hService AS PTR,;
	dwServiceType  AS DWORD      ,;
	dwStartType    AS DWORD      ,;
	dwErrorControl AS DWORD      ,;
	lpBinaryPathName AS PSZ     ,;
	lpLoadOrderGroup AS PSZ     ,;
	lpdwTagId        AS DWORD PTR   ,;
	lpDependencies   AS PSZ        ,;
	lpServiceStartName AS PSZ      ,;
	lpPassword      AS PSZ     ,;
	lpDisplayNam    AS PSZ    );
	AS LOGIC PASCAL:ADVAPI32.ChangeServiceConfigA

_DLL FUNC CloseServiceHandle(hSCObject AS PTR);
	AS LOGIC PASCAL:ADVAPI32.CloseServiceHandle

_DLL FUNC ControlService    (hService   AS PTR      ,;
	dwControl  AS DWORD    ,;
	pServiceStatus AS _WINSERVICE_STATUS);
	AS LOGIC PASCAL:ADVAPI32.ControlService

_DLL FUNC CreateService     (hSCManager AS PTR      ,;
	lpServiceName      AS PSZ     ,;
	lpDisplayName      AS PSZ     ,;
	dwDesiredAccess    AS DWORD   ,;
	dwServiceType      AS DWORD   ,;
	dwStartType        AS DWORD   ,;
	dwErrorControl     AS DWORD   ,;
	lpBinaryPathName   AS PSZ     ,;
	lpLoadOrderGroup   AS PSZ     ,;
	lpdwTagId          AS DWORD PTR,;
	lpDependencies     AS PSZ     ,;
	lpServiceStartName AS PSZ     ,;
	lpPassword         AS PSZ) ;
	AS PTR PASCAL:ADVAPI32.CreateServiceA

_DLL FUNC StartServiceCtrlDispatcher(lpServiceStartTable AS _SERVICE_DISPATCH_TABLE) ;
	AS VOID PASCAL:ADVAPI32.StartServiceCtrlDispatcherA


_DLL FUNC DeleteService   ( hService AS PTR )   ;
	AS LOGIC PASCAL:ADVAPI32.DeleteService

_DLL FUNC OpenSCManager   ( lpMachineName AS PSZ,;
	lpDatabaseName AS PSZ,;
	dwDesiredAcces AS DWORD  );
	AS PTR PASCAL:ADVAPI32.OpenSCManagerA

_DLL FUNC OpenService    ( hSCManager     AS PTR,;
	lpServiceName  AS PSZ,;
	dwDesiredAcces AS DWORD);
	AS PTR PASCAL:ADVAPI32.OpenServiceA

_DLL FUNC QueryServiceStatus(hService AS PTR,;
	lpServiceStatus AS _WINSERVICE_STATUS);
	AS LOGIC PASCAL:ADVAPI32.QueryServiceStatus

_DLL FUNC RegisterServiceCtrlHandler    (lpServiceName AS PSZ,;
	lpHandlerProc AS PTR);
	AS DWORD PASCAL:ADVAPI32.RegisterServiceCtrlHandlerA

_DLL FUNC SetServiceStatus( hServiceStatus  AS DWORD,;
	lpServiceStatus AS _WINSERVICE_STATUS);
	AS LOGIC PASCAL:ADVAPI32.SetServiceStatus
FUNCTION LPSERVICE_MAIN_FUNCTION	(dwNumServicesArgs AS DWORD, lpServiceArgVectors AS PTR) AS VOID STRICT
	RETURN


	//
	// Function Prototype for the Service Main Function
	//



#region defines
DEFINE SERVICE_CONTROL_RUN  := 0x00000000
//
// Value to indicate no change to an optional parameter
//
DEFINE SERVICE_NO_CHANGE := 0xFFFFFFFF
//
// Service State -- for Enum Requests (Bit Mask)
//
DEFINE SERVICE_ACTIVE       := 0x00000001
DEFINE SERVICE_INACTIVE     := 0x00000002
DEFINE SERVICE_STATE_ALL    := 0x00000003
//
// Controls
//
DEFINE SERVICE_CONTROL_STOP         :=  0x00000001
DEFINE SERVICE_CONTROL_PAUSE        :=  0x00000002
DEFINE SERVICE_CONTROL_CONTINUE     :=  0x00000003
DEFINE SERVICE_CONTROL_INTERROGATE  :=  0x00000004
DEFINE SERVICE_CONTROL_SHUTDOWN     :=  0x00000005
DEFINE SERVICE_CONTROL_PARAMCHANGE            := 0x00000006
DEFINE SERVICE_CONTROL_NETBINDADD             := 0x00000007
DEFINE SERVICE_CONTROL_NETBINDREMOVE          := 0x00000008
DEFINE SERVICE_CONTROL_NETBINDENABLE          := 0x00000009
DEFINE SERVICE_CONTROL_NETBINDDISABLE         := 0x0000000A
DEFINE SERVICE_CONTROL_DEVICEEVENT            := 0x0000000B
DEFINE SERVICE_CONTROL_HARDWAREPROFILECHANGE  := 0x0000000C
DEFINE SERVICE_CONTROL_POWEREVENT             := 0x0000000D
DEFINE SERVICE_CONTROL_SESSIONCHANGE          := 0x0000000E
//
// Service State -- for CurrentState
//
DEFINE SERVICE_STOPPED              := 0x00000001
DEFINE SERVICE_START_PENDING        := 0x00000002
DEFINE SERVICE_STOP_PENDING         := 0x00000003
DEFINE SERVICE_RUNNING              := 0x00000004
DEFINE SERVICE_CONTINUE_PENDING     := 0x00000005
DEFINE SERVICE_PAUSE_PENDING        := 0x00000006
DEFINE SERVICE_PAUSED               := 0x00000007
//
// Controls Accepted  (Bit Mask)
//
DEFINE SERVICE_ACCEPT_STOP          := 0x00000001
DEFINE SERVICE_ACCEPT_PAUSE_CONTINUE := 0x00000002
DEFINE SERVICE_ACCEPT_SHUTDOWN      := 0x00000004
DEFINE SERVICE_ACCEPT_PARAMCHANGE             := 0x00000008
DEFINE SERVICE_ACCEPT_NETBINDCHANGE           := 0x00000010
DEFINE SERVICE_ACCEPT_HARDWAREPROFILECHANGE   := 0x00000020
DEFINE SERVICE_ACCEPT_POWEREVENT              := 0x00000040
DEFINE SERVICE_ACCEPT_SESSIONCHANGE           := 0x00000080
//
// Service Control Manager object specific access types
//
DEFINE SC_MANAGER_CONNECT           := 0x0001
DEFINE SC_MANAGER_CREATE_SERVICE    := 0x0002
DEFINE SC_MANAGER_ENUMERATE_SERVICE := 0x0004
DEFINE SC_MANAGER_LOCK              := 0x0008
DEFINE SC_MANAGER_QUERY_LOCK_STATUS := 0x0010
DEFINE SC_MANAGER_MODIFY_BOOT_CONFIG:= 0x0020
DEFINE SC_MANAGER_ALL_ACCESS        := 0x000F003FL
//
// Service object specific access type
//
DEFINE SERVICE_QUERY_CONFIG         := 0x0001
DEFINE SERVICE_CHANGE_CONFIG        := 0x0002
DEFINE SERVICE_QUERY_STATUS         := 0x0004
DEFINE SERVICE_ENUMERATE_DEPENDENTS := 0x0008
DEFINE SERVICE_START                := 0x0010
DEFINE SERVICE_STOP                 := 0x0020
DEFINE SERVICE_PAUSE_CONTINUE       := 0x0040
DEFINE SERVICE_INTERROGATE          := 0x0080
DEFINE SERVICE_USER_DEFINED_CONTROL := 0x0100
DEFINE SERVICE_ALL_ACCESS       := 0x000F01FFL
//
// Service Status Structure
//
#endregion
