STATIC FUNCTION __FindStatus(h AS PTR) AS DWORD STRICT
    RETURN AScan(agStatus, {|a| a[STATUS_HANDLE] == h} )


STATIC GLOBAL agStatus := {} AS ARRAY

CLASS CSession
    PROTECT cHostAddress        AS STRING
    PROTECT cUserName           AS STRING
    PROTECT cPassWord           AS STRING
    PROTECT cError              AS STRING
    PROTECT nError              AS DWORD
    PROTECT cAgent              AS STRING
    PROTECT cProxy              AS STRING
    PROTECT cProxyBypass        AS STRING
    PROTECT hSession            AS PTR
    PROTECT hConnect            AS PTR
    PROTECT nPort               AS WORD
    PROTECT nAccessType         AS DWORD
    PROTECT lFixPortNumber      AS LOGIC

    //  UH 10/29/2001
    PROTECT nOpenFlags          AS DWORD

    //  UH 02/08/2000
    PROTECT lStatus             AS LOGIC

    #ifdef __VULCAN__
       STATIC HIDDEN callbackDelegate AS INTStatusCallback_Delegate
    #endif
    //RvdH 100216 Moved GLOBAL and Lock structure to Session Class
    STATIC EXPORT oLock := OBJECT{}        AS OBJECT
    STATIC EXPORT ogStatus AS OBJECT

METHOD __DelStatus        (h)
    LOCAL n     AS DWORD
    LOCAL lRet  AS LOGIC

    IF SELF:lStatus
        //  UH 02/11/2000
        //EnterCriticalSection(@csWSA)
        BEGIN LOCK oLock

        n := __FindStatus(h)

        IF n > 0
            ADel(agStatus, n)
            ASize(agStatus, ALen(agStatus) - 1 )
            lRet := .T.
        ENDIF
        //  UH 02/11/2000
        //LeaveCriticalSection(@csWSA)
        END LOCK
        SELF:lStatus := FALSE
    ENDIF

    RETURN lRet


METHOD __DelStatusObject  ()
    //  UH 02/11/2000
    //EnterCriticalSection(@csWSA)
    BEGIN LOCK oLock
    ogStatus := NULL_OBJECT
    //  UH 02/11/2000
    //LeaveCriticalSection(@csWSA)
    END LOCK
    RETURN SELF


METHOD __GetStatusContext   (h)
    LOCAL nRet      AS DWORD

    //  UH 02/11/2000
    //EnterCriticalSection(@csWSA)
    BEGIN LOCK oLock

    DEFAULT(@h, NULL_PTR)

    IF h == NULL_PTR
        nRet := ALen(agStatus)  + 1
    ELSE
        nRet := __FindStatus(h)
        IF nRet == 0
            nRet := ALen(agStatus)  + 1
        ENDIF
    ENDIF
    //  UH 02/11/2000
    //LeaveCriticalSection(@csWSA)
    END LOCK
    RETURN nRet



METHOD __SetStatus        (h)
    //  UH 02/08/2000
    LOCAL nStatus       AS DWORD
    LOCAL lRet          AS LOGIC

    IF SELF:lStatus
        //  UH 02/11/2000
        //EnterCriticalSection(@csWSA)
        BEGIN LOCK oLock
        nStatus := __FindStatus(h)
        IF nStatus = 0
            AAdd(agStatus, {h, SELF} )
#ifdef __VULCAN__
            IF callbackDelegate == NULL
                callbackDelegate := INTStatusCallback_Delegate{ NULL, @INTStatusCallback() }
            ENDIF
            InternetSetStatusCallback( h, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) callbackDelegate ) )
#else
            InternetSetStatusCallback(h, @INTStatusCallback() )
#endif
            lRet := .T.
        ENDIF
        //  UH 02/11/2000
        //LeaveCriticalSection(@csWSA)
        END LOCK
    ENDIF

    RETURN lRet

METHOD __SetStatusObject  ()
    //  UH 02/11/2000
    //EnterCriticalSection(@csWSA)
    BEGIN LOCK oLock
    ogStatus := SELF
    //  UH 02/11/2000
    //LeaveCriticalSection(@csWSA)
    END LOCK

    RETURN SELF

ACCESS AccessType
    RETURN SELF:nAccessType


ASSIGN AccessType(nNew)
    IF IsNumeric(nNew)
       SELF:nAccessType := nNew
    ENDIF
    RETURN


DESTRUCTOR()
    SELF:CloseRemote()

    IF SELF:hSession != NULL_PTR
       //  UH 02/08/2000
       IF SELF:lStatus
          SELF:__DelStatus (SELF:hSession)
       ENDIF

       IF InternetCloseHandle(SELF:hSession)
          SELF:hSession := NULL_PTR
       ENDIF
    ENDIF

    // RvdH 050302 Added next line. Was missing
    UnregisterAxit(SELF)

    RETURN


METHOD CloseFile(hFile)
    LOCAL lRet AS LOGIC

    IF IsPtr(hFile) .AND. (hFile != NULL_PTR)
        SELF:__DelStatus(hFile)
        lRet := InternetCloseHandle(hFile)
        IF !lRet
           SELF:Error := GetLastError()
        ENDIF
    ENDIF

    //  UH 02/27/2001
    SELF:SetResponseStatus()

    RETURN lRet




METHOD CloseRemote()
    LOCAL lRet AS LOGIC

    IF SELF:hConnect != NULL_PTR
       //  UH 02/08/2000
       IF SELF:lStatus
          lRet := SELF:__DelStatus (SELF:hConnect)
       ENDIF

       IF InternetCloseHandle(SELF:hConnect)
          SELF:hConnect := NULL_PTR
       ENDIF
       lRet := TRUE
    ENDIF

    //  UH 02/27/2001
    //  SELF:SetResponseStatus()

    RETURN lRet



ACCESS @@Connected
    RETURN (SELF:hConnect != NULL_PTR)


ACCESS ConnectHandle
    RETURN SELF:hConnect



METHOD ConnectRemote(cIP, nService, nFlags, nContext)
    LOCAL lRet        AS LOGIC
    LOCAL nPortNumber AS WORD

    IF IsString(cIP)
       SELF:RemoteHost := cIP
    ENDIF

    IF SELF:hSession == NULL_PTR
       lRet := SELF:Open(NIL,SELF:cProxy, SELF:cProxyBypass)
    ELSE
       lRet := TRUE
    ENDIF


    IF lRet .AND. (SELF:hConnect = NULL_PTR)
        SELF:__SetStatusObject()
        IF SELF:lFixPortNumber
           nPortNumber := SELF:nPort
        ELSE
           nPortNumber := INTERNET_INVALID_PORT_NUMBER
        ENDIF


        SELF:hConnect := InternetConnect(;
                                    SELF:hSession, ;
                                    String2Psz(SELF:cHostAddress),;
                                    nPortNumber, ;
                                    String2Psz(SELF:cUserName), ;
                                    String2Psz(SELF:cPassword), ;
                                    nService,;
                                    nFlags,;
                                    nContext)

        IF SELF:hConnect = NULL_PTR
           //  UH 02/27/2001
           SELF:SetResponseStatus()
           lRet := .F.
           IF SELF:nError = 0
               SELF:nError := ERROR_INTERNET_NAME_NOT_RESOLVED
           ENDIF
        ELSE
           SELF:__SetStatus(SELF:hConnect)
           //  UH 02/27/2001
           SELF:SetResponseStatus()
        ENDIF
        SELF:__DelStatusObject()
    ENDIF

    #IFDEF __DEBUG__
        DebOut32("  --> CSession:ConnectRemote() - END")
    #ENDIF

    RETURN lRet

ACCESS Error
    RETURN SELF:nError

ASSIGN Error(n)
    IF IsNumeric(n)
       SELF:nError := n
    ENDIF

    RETURN


ACCESS ErrorMsg
    RETURN SystemErrorString(SELF:nError, "FTP Error " + NTrim(SELF:nError))

ACCESS Handle
    RETURN SELF:hSession

CONSTRUCTOR(cCaption, n, lStat)
    IF IsNumeric(n)
       SELF:nPort := n
       SELF:lFixPortNumber := TRUE
    ELSE
       SELF:nPort := 0
       SELF:lFixPortNumber := FALSE
    ENDIF

    IF IsString(cCaption)
       SELF:cAgent := cCaption
    ELSE
       SELF:cAgent := "VO Internet Client"
    ENDIF

    SELF:nAccessType := INTERNET_OPEN_TYPE_DIRECT

    //  UH 02/08/2000
    DEFAULT(@lStat, TRUE)
    SELF:lStatus  := lStat



    RETURN


METHOD InternetStatus(nContext, nStatus, pStatusInfo, nStatusLength)

    RETURN NIL


METHOD Open(nFlags, xProxy, aProxyByPass)
    LOCAL lRet      AS LOGIC
    LOCAL lProxy    AS LOGIC
    LOCAL cTemp     AS STRING
    LOCAL n,i       AS DWORD

    IF SELF:hSession != NULL_PTR
       RETURN TRUE
    ENDIF

    DEFAULT(@nFlags, SELF:nOpenFlags)
    DEFAULT(@xProxy, "")
    DEFAULT(@aProxyByPass, {})

    IF SLen(xProxy) > 0
        lProxy := TRUE
    ELSE
        lProxy := FALSE
    ENDIF

    IF lProxy
    	// UH 07/30/2001
        SELF:cProxy := xProxy

        IF IsArray(aProxyByPass)
            n := ALen(aProxyByPass)
            cTemp := ""
            FOR i := 1 TO n
                IF IsString(aProxyByPass[i])
                    IF SLen(aProxyByPass[i]) > 0
                        cTemp += aProxyByPass[i]
                        cTemp += " "
                    ENDIF
                ENDIF
            NEXT
            SELF:cProxyBypass := Trim(cTemp)
        ELSE
        ENDIF

        IF SELF:nAccessType = INTERNET_OPEN_TYPE_DIRECT
            SELF:nAccessType := INTERNET_OPEN_TYPE_PROXY
        ENDIF

        SELF:hSession := InternetOpen( String2Psz(SELF:cAgent), ;
                                   SELF:nAccessType,;
                                   String2Psz(SELF:cProxy), ;
                                   String2Psz(SELF:cProxyBypass), ;
                                   nFlags )
    ELSE
        SELF:hSession := InternetOpen( String2Psz(SELF:cAgent), ;
                                   SELF:nAccessType, ;
                                   NULL, ;
                                   NULL, ;
                                   nFlags )
    ENDIF

    IF SELF:hSession != NULL_PTR
        lRet := TRUE
        //  UH 02/08/2000
        IF lRet
            SELF:__SetStatus(SELF:hSession)
        ENDIF
    ENDIF

    //  UH 02/27/2001
    SELF:SetResponseStatus()

    RETURN lRet


ACCESS OpenFlags
    //  UH 10/29/2001
    RETURN SELF:nOpenFlags

ASSIGN OpenFlags(n)
    //  UH 10/29/2001
    IF IsNumeric(n)
       SELF:nOpenFlags := n
    ENDIF

    RETURN

ACCESS PassWord
    RETURN SELF:cPassWord


ASSIGN PassWord(cNew)
    IF IsString(cNew)
       SELF:cPassWord := cNew
    ENDIF

    RETURN

ACCESS Port
    RETURN SELF:nPort

ASSIGN Port(n)
    IF IsNumeric(n)
       SELF:nPort := n
    ENDIF

    RETURN


ACCESS Proxy
    RETURN SELF:cProxy

ASSIGN Proxy(cNew)
    IF IsString(cNew)
        SELF:cProxy := cNew
    ENDIF

    RETURN


ACCESS ProxyBypass
    RETURN SELF:cProxyBypass

ASSIGN ProxyBypass(cNew)
    IF IsString(cNew)
       SELF:cProxyBypass := cNew
    ENDIF

    RETURN

ACCESS RemoteHost
    // Returns the remote computer (mail server)
    RETURN SELF:cHostAddress

ASSIGN RemoteHost(cServer)
    // Sets the remote computer (mail server)
    IF IsString(cServer)
       //SELF:cHostAddress := CheckHostIP(cServer)
       SELF:cHostAddress := cServer
    ENDIF

    RETURN


METHOD SetResponseStatus()
    //SE-040707
    LOCAL dwError                   AS DWORD
    LOCAL dwSize                    AS DWORD
    LOCAL DIM abStatus[STATUS_SIZE] AS BYTE	//RvdH 070407 changed from DWORD to BYTE
    LOCAL cStatus                   AS STRING
    LOCAL dwContext                 AS DWORD

    dwSize := STATUS_SIZE

    IF InternetGetLastResponseInfo(@dwError, @abStatus[1], @dwSize)
       SELF:nError := dwError

       // UH 07/30/2001
       IF dwSize == 0
          dwError := GetLastError()
        	 cStatus := "Error " + NTrim(dwError)
        	 dwSize  := SLen(cStatus)
       ELSE
	       cStatus := Mem2String(@abStatus[1], dwSize)
	    ENDIF

       dwContext := SELF:__GetStatusContext()

       SELF:InternetStatus(dwContext, INTERNET_STATUS_RESPONSE_RECEIVED, cStatus, dwSize)

       #IFDEF __DEBUG__
         IF dwSize > 0
            DebOut32(NTrim(dwError) + ": " + SubStr(cStatus, 1, 80))
         ENDIF
       #ENDIF

       RETURN TRUE
    ENDIF

    RETURN FALSE


ACCESS UserName
    RETURN SELF:cUserName


ASSIGN UserName(cNew)
    IF IsString(cNew)
       SELF:cUserName := cNew
    ENDIF

    RETURN


END CLASS


#ifdef __VULCAN__
   DELEGATE INTStatusCallback_Delegate( hInternet AS PTR, dwContext AS DWORD, dwInternetStatus AS DWORD, pStatusInformation AS PTR, dwStatusInfoLength AS DWORD ) AS VOID
#endif

FUNCTION INTStatusCallback( hInternet           AS PTR, ;
                            dwContext           AS DWORD,;
                            dwInternetStatus    AS DWORD,;
                            pStatusInformation  AS PTR,;
                            dwStatusInfoLength  AS DWORD)  AS VOID STRICT
    LOCAL oSession  AS CSession
    LOCAL n         AS DWORD

    //  UH 02/11/2000
    //EnterCriticalSection(@csWSA)
    BEGIN LOCK cSession.oLock

    IF (n := __FindStatus(hInternet)) > 0
       oSession := agStatus[n, STATUS_OP]
    ELSE
       oSession := cSession.ogStatus
    ENDIF

    IF ! oSession == NULL_OBJECT
       oSession:InternetStatus(dwContext, dwInternetStatus, pStatusInformation, dwStatusInfoLength)
    ENDIF

    //  UH 02/11/2000
    //LeaveCriticalSection(@csWSA)
    END LOCK

    RETURN


//STATIC GLOBAL ogStatus AS OBJECT



#region defines
DEFINE  STATUS_HANDLE   := 1
DEFINE  STATUS_OP       := 2
DEFINE  STATUS_SIZE := 1024
#endregion
