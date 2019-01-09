CLASS CINetDial
   PROTECT _oOwner            AS OBJECT //Owner object, this object receives the notify messages
   PROTECT _symNotify         AS SYMBOL //method name of the Notify method of Owner object
   PROTECT _dwConnection      AS DWORD  //Connection handle of InternetDial()
   PROTECT _lAutoDial         AS LOGIC
   PROTECT _dwDialFlag        AS DWORD  //see into ACCESS DialFlag
   PROTECT _dwConnectFlags    AS DWORD  //see into ACCESS ConnectionFlags
   PROTECT _dwError           AS DWORD

   #ifndef __VULCAN__
    ~"ONLYEARLY+"
    DECLARE METHOD __Notify
    DECLARE METHOD __IsModem
    ~"ONLYEARLY-"
   #endif

METHOD __IsModem(hConnection AS PTR) AS LOGIC STRICT
   LOCAL sConnStatus IS _winRASCONNSTATUS
   LOCAL cDeviceType AS STRING

   IF hConnection != NULL_PTR
      sConnStatus:dwSize := _SIZEOF(_winRASCONNSTATUS)
      IF RasGetConnectStatus(hConnection, @sConnStatus) = ERROR_SUCCESS
         cDeviceType := Psz2String(@sConnStatus:szDeviceType[1])
         RETURN (cDeviceType = RASDT_Modem .OR. cDeviceType = RASDT_Isdn)
      ENDIF
   ENDIF
   RETURN FALSE

METHOD __Notify(kNotifyKode AS DWORD) AS LOGIC STRICT
   LOCAL uRet  AS USUAL
   LOCAL liRet AS LONGINT

   IF _symNotify != NULL_SYMBOL
      uRet := Send(_oOwner, _symNotify, kNotifyKode, SELF)
      IF IsLogic(uRet)
         RETURN uRet
      ENDIF
      IF IsLong(uRet)
         liRet := uRet
         RETURN  liRet != 0
      ENDIF
   ENDIF

   RETURN FALSE

METHOD AutoDial(dwDialFlag)
   LOCAL dwFlag       AS DWORD
   LOCAL hWnd         AS PTR

   IF _lAutoDial
      RETURN TRUE
   ENDIF

   IF IsLong(dwDialFlag)
      dwFlag := dwDialFlag
   ELSE
      dwFlag := _dwDialFlag
   ENDIF

   IF dwFlag = 0
      dwFlag := INTERNET_AutoDial_FORCE_ONLINE
   ENDIF

   IF IsInstanceOf(_oOwner, #Window)
      hWnd := Send(_oOwner, #Handle)
   ENDIF

   _dwError := 0
   IF dwFlag <= INTERNET_AUTODIAL_OVERRIDE_NET_PRESENT
      _lAutoDial := InternetAutodial(dwFlag, hWnd)
      RETURN _lAutoDial
   ENDIF
   _lAutoDial := FALSE

   RETURN FALSE


METHOD Dial(cConnectionName, dwDialFlag)
   LOCAL dwConnection AS DWORD
   LOCAL dwFlag       AS DWORD
   LOCAL cConnectID   AS STRING
   LOCAL hWnd         AS PTR

   IF IsString(cConnectionName) .AND. SLen(cConnectionName) > 0
      cConnectID := AllTrim(cConnectionName)
   ENDIF

   IF IsLong(dwDialFlag)
      dwFlag := dwDialFlag
   ELSE
      dwFlag := _dwDialFlag
   ENDIF

   IF dwFlag = 0
      dwFlag := INTERNET_AutoDial_FORCE_ONLINE
   ENDIF

   IF IsInstanceOf(_oOwner, #Window)
      hWnd := Send(_oOwner, #Handle)
   ENDIF

   _dwError := 0

   IF !(dwFlag = INTERNET_AUTODIAL_OVERRIDE_NET_PRESENT .OR. dwFlag = INTERNET_AUTODIAL_FAILIFSECURITYCHECK)
      _dwError := InternetDial(hWnd, String2Psz(cConnectID), dwFlag, @dwConnection, 0)
      IF _dwError == ERROR_SUCCESS
         _dwConnection := dwConnection
         RETURN TRUE
      ENDIF
   ENDIF

   _dwConnection := 0

   RETURN FALSE




ACCESS DialFlag
   RETURN _dwDialFlag

/*
This parameter can be one of the following values.

INTERNET_AUTODIAL_FORCE_ONLINE
  Forces an online connection.

INTERNET_AUTODIAL_FORCE_UNATTENDED
  Forces an unattended Internet dial-up.
  If user intervention is required, the function will fail.

INTERNET_DIAL_FORCE_PROMPT
  Ignores the "dial automatically" setting and
  forces the dialing user interface to be displayed.

INTERNET_DIAL_UNATTENDED Connects
  to the Internet through a modem, without displaying a user interface,
  if possible. Otherwise, the function will wait for user input.

INTERNET_DIAL_SHOW_OFFLINE
  Shows the Work Offline button instead of the Cancel button in the dialing user interface.

*/

ASSIGN DialFlag(dwValue)
   IF IsLong(dwValue) .AND. dwValue > 0
      _dwDialFlag := dwValue
   ELSE
      _dwDialFlag := INTERNET_AUTODIAL_FORCE_ONLINE
   ENDIF
   RETURN

ACCESS Error
   RETURN _dwError

ACCESS ErrorDescription
   LOCAL cError AS STRING
   LOCAL pError AS PSZ

   IF ! _dwError == ERROR_SUCCESS
      IF (pError := MemAlloc(256)) != NULL_PSZ

         IF RasGetErrorString(_dwError, pError, 256) = 0
            cError := Psz2String(pError)
         ENDIF

         MemFree(pError)
      ENDIF
   ENDIF

   RETURN cError

METHOD GetConnectedState()
   LOCAL dwFlags AS DWORD

   IF InternetGetConnectedState(@dwFlags, 0)
      _dwConnectFlags := dwFlags
      RETURN TRUE
   ENDIF

   _dwConnectFlags := 0
   RETURN FALSE


METHOD GetConnections()
   LOCAL pEntries  AS _winRASCONN PTR
   LOCAL pEntry    AS _winRASCONN
   LOCAL dwRetries AS DWORD
   LOCAL dwEntries AS DWORD
   LOCAL dwCB      AS DWORD
   LOCAL dwErr     AS DWORD
   LOCAL aEntries  AS ARRAY

   dwRetries := 5
   dwEntries := 1
   dwCB      := _SIZEOF(_WINRASCONN)
   pEntries  := NULL_PTR

   DO WHILE dwRetries-- > 0

      IF pEntries != NULL_PTR
         MemFree(pEntries)
      ENDIF

      pEntries := MemAlloc(dwCB)
      IF pEntries != NULL_PTR
         pEntry := pEntries
         pEntry:dwSize := _SIZEOF(_WINRASCONN)

         dwErr := RasEnumConnections(pEntries, @dwCB, @dwEntries)
         IF dwErr != ERROR_BUFFER_TOO_SMALL
            EXIT
         ENDIF
      ENDIF

   ENDDO

   IF dwErr = ERROR_SUCCESS
      IF dwEntries > 0
         aEntries := {}
         FOR dwCB := 1 UPTO dwEntries
             pEntry := pEntries
             IF SELF:__IsModem(pEntry:hrasconn)
                AAdd(aEntries, {Psz2String(@pEntry:szEntryName[1]), DWORD(_CAST, pEntry:hrasconn)})
             ENDIF
             pEntries       += 1
         NEXT  // dwCB
      ENDIF
   ENDIF

   IF pEntries != NULL_PTR
      MemFree(pEntries)
   ENDIF

   RETURN aEntries

METHOD GetDialUpEntries()
   LOCAL pEntries  AS _winRASENTRYNAME PTR
   LOCAL pEntry    AS _winRASENTRYNAME
   LOCAL dwRetries AS DWORD
   LOCAL dwEntries AS DWORD
   LOCAL dwCB      AS DWORD
   LOCAL dwErr     AS DWORD
   LOCAL aEntries  AS ARRAY

   dwRetries := 5
   dwEntries := 1
   dwCB      := _SIZEOF(_winRASENTRYNAME)
   pEntries  := NULL_PTR

   DO WHILE dwRetries-- > 0

      IF pEntries != NULL_PTR
         MemFree(pEntries)
      ENDIF

      pEntries := MemAlloc(dwCB)
      IF pEntries != NULL_PTR
         pEntry := pEntries
         pEntry:dwSize := _SIZEOF(_winRASENTRYNAME)

         dwErr := RasEnumEntries(NULL_PSZ, NULL_PSZ, pEntries, @dwCB, @dwEntries)
         IF dwErr != ERROR_BUFFER_TOO_SMALL
            EXIT
         ENDIF
      ENDIF

   ENDDO

   IF dwErr = ERROR_SUCCESS
      IF dwEntries > 0
         aEntries := ArrayNew(dwEntries)
         FOR dwCB := 1 UPTO dwEntries
             pEntry         := pEntries
             aEntries[dwCB] := Psz2String( @pEntry:szEntryName[1])
             pEntries       += 1
         NEXT  // dwCB
      ENDIF
   ENDIF

   IF pEntries != NULL_PTR
      MemFree(pEntries)
   ENDIF

   RETURN aEntries

METHOD HangUp(lAllModems)
   LOCAL lHangUpAll    AS LOGIC
   LOCAL aConnections  AS ARRAY
   LOCAL dwI, dwCount  AS DWORD
   LOCAL lDisconnected AS LOGIC

   IF IsLogic(lAllModems)
      lHangUpAll := lAllModems
   ELSE
      lHangUpAll := FALSE
   ENDIF

   _dwError := 0
   lDisconnected := FALSE

   IF _dwConnection != 0
      IF SELF:__Notify(NOTIFY_CINetDial_QueryHangUp)
         IF InternetHangUp(_dwConnection, 0) != 0
            _dwConnection := 0
            lDisconnected := TRUE
         ENDIF
      ENDIF
   ENDIF

   IF _lAutoDial
      IF SELF:__Notify(NOTIFY_CINetDial_QueryHangUp)
         IF InternetAutodialHangup(0)
            _dwConnection := 0
            _lAutoDial    := FALSE
            lDisconnected := TRUE
         ENDIF
      ENDIF
   ENDIF

   IF lHangUpAll
      IF SELF:IsConnected
         aConnections := SELF:GetConnections()
         dwCount := ALen(aConnections)
         IF dwCount > 0 .AND. SELF:__Notify(NOTIFY_CINetDial_QueryHangUpAll)
            FOR dwI := 1 UPTO dwCount
                InternetHangUp(aConnections[dwI,2], 0)
            NEXT
            lDisconnected := TRUE
         ENDIF
      ENDIF
   ENDIF

   IF lDisconnected
      SELF:__Notify(NOTIFY_CINetDial_DISCONNECTED)
   ENDIF

   RETURN lDisconnected



CONSTRUCTOR(dwDialFlag, oOwner, symNotifyMethod)

   SELF:DialFlag := dwDialFlag

   SELF:SetOwner(oOwner, symNotifyMethod)

   SELF:GetConnectedState()

   RETURN

ACCESS IsConfigured
    RETURN (_AND(_dwConnectFlags, INTERNET_CONNECTION_CONFIGURED)!=0)

ACCESS IsConnected
    /*
    IF (gethostbyname(String2Psz("www.microsoft.com")) != Null_Ptr)
       SELF:GetConnectedState()
       RETURN TRUE
    ENDIF
    SELF:GetConnectedState()
    RETURN FALSE
    */
    RETURN SELF:GetConnectedState()

ACCESS IsLan
    RETURN (_AND(_dwConnectFlags, INTERNET_CONNECTION_LAN)!=0)

ACCESS IsModem
    RETURN (_AND(_dwConnectFlags, INTERNET_CONNECTION_MODEM)!=0)

ACCESS IsOffline
    RETURN (_AND(_dwConnectFlags, INTERNET_CONNECTION_OFFLINE)!=0)

ACCESS IsProxy
    RETURN (_AND(_dwConnectFlags, INTERNET_CONNECTION_PROXY)!=0)

METHOD SetOwner(oOwner, symNotifyMethod)

   IF IsObject(oOwner)
      _oOwner := oOwner
   ENDIF

   IF _oOwner != NULL_OBJECT
      IF IsSymbol(symNotifyMethod)
         _symNotify := symNotifyMethod
      ELSE
         _symNotify := #Notify
      ENDIF
      IF ! IsMethod(_oOwner, _symNotify)
         _symNotify := NULL_SYMBOL
      ENDIF
   ENDIF

   RETURN NIL
// DEFINE INTERNET_AUTODIAL_FAILIFSECURITYCHECK := 4
// DEFINE INTERNET_AutoDial_FORCE_ONLINE        := 1
// DEFINE INTERNET_AUTODIAL_FORCE_UNATTENDED    := 2
// DEFINE INTERNET_CONNECTION_CONFIGURED :=   0x40
// DEFINE INTERNET_CONNECTION_LAN        :=   0x02
// DEFINE INTERNET_CONNECTION_MODEM      :=   0x01
// DEFINE INTERNET_CONNECTION_OFFLINE    :=   0x20
// DEFINE INTERNET_CONNECTION_PROXY      :=   0x04
// DEFINE INTERNET_RAS_INSTALLED         :=   0x10
// DEFINE INTERNET_OPTION_CONNECTED_STATE := 0x32
// DEFINE INTERNET_STATE_DISCONNECTED_BY_USER := 0x10
// DEFINE INTERNET_STATE_CONNECTED := 0x01
// DEFINE ISO_FORCE_DISCONNECTED := 0x01
// STRUCT _winINTERNET_CONNECTED_INFO
//   MEMBER dwConnectedState  AS DWORD
//   MEMBER dwFlags AS DWORD

// DEFINE INTERNET_DIAL_FORCE_PROMPT := 0x2000
// DEFINE INTERNET_DIAL_SHOW_OFFLINE := 0x4000
// DEFINE INTERNET_DIAL_UNATTENDED   := 0x8000
// _DLL FUNC InternetAttemptConnect(dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetAttemptConnect#207
// _DLL FUNC InternetAutodial(dwFlags AS DWORD, hwndParent AS PTR) AS LOGIC PASCAL:WININET.InternetAutodial#208
// _DLL FUNC InternetAutodialHangup(dwReserved AS DWORD) AS LOGIC PASCAL:WININET.InternetAutodialHangup#210
// _DLL FUNC InternetCheckConnection(lpszUrl AS PSZ, dwFlags AS DWORD, dwReserved AS DWORD);
//      AS LOGIC PASCAL:WININET.InternetCheckConnectionA
// _DLL FUNC InternetDial(hwndParent AS PTR, lpszConnectoid AS PSZ, dwFlags AS DWORD, lpdwConnection AS PTR, dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetDial#227
// _DLL FUNC InternetGetConnectedState(lpdwFlags AS PTR, dwReserved AS DWORD) AS LOGIC PASCAL:WININET.InternetGetConnectedState

// _DLL FUNC InternetHangUp(dwConnection AS DWORD, dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetHangUp#247
// DEFINE FLAG_ICC_FORCE_CONNECTION := 0x00000001
// _DLL FUNC RasGetErrorString( P1 AS DWORD, P2 AS PSZ, P3 AS DWORD ) AS DWORD PASCAL:RASAPI32.RasGetErrorStringA
// _DLL FUNC InternetSetOption(HINTERNET AS PTR, dwOption AS DWORD, lpBuffer AS PTR, dwBufferLength AS DWORD) AS LOGIC PASCAL:WININET.InternetSetOptionA

METHOD SetSystemOffline(lEnable)
   LOCAL sInfo IS _winINTERNET_CONNECTED_INFO

   IF ! IsLogic(lEnable) .OR. lEnable
      sInfo:dwConnectedState := INTERNET_STATE_DISCONNECTED_BY_USER
      sInfo:dwFlags := ISO_FORCE_DISCONNECTED
   ELSE
      sInfo:dwConnectedState := INTERNET_STATE_CONNECTED
      sInfo:dwFlags := 0
   ENDIF

   RETURN InternetSetOption(0, INTERNET_OPTION_CONNECTED_STATE, @sInfo, _SIZEOF(_winINTERNET_CONNECTED_INFO))

METHOD VerifyConnection(dwDialFlag, cConnectionName)
   LOCAL dwFlag       AS DWORD
   LOCAL cConnectID   AS STRING

   IF SELF:IsConnected
      RETURN TRUE
   ENDIF

   IF IsString(cConnectionName) .AND. SLen(cConnectionName) > 0
      cConnectID := AllTrim(cConnectionName)
   ENDIF

   SELF:HangUp()

   IF IsLong(dwDialFlag)
      dwFlag := dwDialFlag
   ELSE
      dwFlag := _dwDialFlag
   ENDIF

   IF dwFlag = 0
      dwFlag := INTERNET_AutoDial_FORCE_ONLINE
   ENDIF

   _dwError := 0
   IF cConnectID == NULL_STRING .AND. dwFlag <= INTERNET_AUTODIAL_OVERRIDE_NET_PRESENT
      IF SELF:AutoDial(dwFlag)
         IF SELF:IsConnected
            SELF:__Notify(NOTIFY_CINetDial_CONNECTED)
            RETURN TRUE
         ENDIF
         _lAutoDial := FALSE
      ELSE
         SELF:__Notify(NOTIFY_CINetDial_ERROR)
         RETURN FALSE
      ENDIF
   ENDIF
   _lAutoDial := FALSE

   IF SELF:Dial(cConnectID, dwFlag)
      IF SELF:IsConnected
         SELF:__Notify(NOTIFY_CINetDial_CONNECTED)
         RETURN TRUE
      ENDIF
      SELF:HangUp()
   ENDIF

   SELF:__Notify(NOTIFY_CINetDial_ERROR)

   RETURN FALSE


END CLASS




#region defines
DEFINE NOTIFY_CINetDial_ERROR := 1    
DEFINE NOTIFY_CINetDial_QueryHangUp := 2
DEFINE NOTIFY_CINetDial_CONNECTED := 3
DEFINE NOTIFY_CINetDial_DISCONNECTED := 4
DEFINE NOTIFY_CINetDial_QueryHangUpAll := 5
#endregion
