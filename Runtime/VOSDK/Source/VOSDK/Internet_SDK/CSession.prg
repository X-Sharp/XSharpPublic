INTERNAL FUNCTION __FindStatus(h AS PTR) AS DWORD STRICT
    RETURN AScan(agStatus, {|a| a[STATUS_HANDLE] == h} )




INTERNAL GLOBAL agStatus := {} AS ARRAY




/// <include file="Internet.xml" path="doc/CSession/*" />
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


    PROTECT nOpenFlags          AS DWORD


    PROTECT lStatus             AS LOGIC


    STATIC HIDDEN callbackDelegate AS __INTStatusCallback_Delegate
    STATIC EXPORT oLock := OBJECT{}        AS OBJECT
    STATIC EXPORT ogStatus AS OBJECT


 /// <exclude />
METHOD __DelStatus        (h)
    LOCAL n     AS DWORD
    LOCAL lRet  AS LOGIC


    IF SELF:lStatus
        BEGIN LOCK oLock


        n := __FindStatus(h)


        IF n > 0
            ADel(agStatus, n)
            ASize(agStatus, ALen(agStatus) - 1 )
            lRet := .T.
        ENDIF
        END LOCK
        SELF:lStatus := FALSE
    ENDIF


    RETURN lRet




 /// <exclude />
METHOD __DelStatusObject  ()
    BEGIN LOCK oLock
        ogStatus := NULL_OBJECT
    END LOCK
    RETURN SELF




 /// <exclude />
METHOD __GetStatusContext   (h)
    LOCAL nRet      AS DWORD


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
    END LOCK
    RETURN nRet






 /// <exclude />
METHOD __SetStatus        (h)
    LOCAL nStatus       AS DWORD
    LOCAL lRet          AS LOGIC


    IF SELF:lStatus
        BEGIN LOCK oLock
        nStatus := __FindStatus(h)
        IF nStatus = 0
            AAdd(agStatus, {h, SELF} )
            IF callbackDelegate == NULL
                callbackDelegate := __INTStatusCallback_Delegate{ NULL, @INTStatusCallback() }
            ENDIF
            InternetSetStatusCallback( h, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) callbackDelegate ) )
            lRet := .T.
        ENDIF
        END LOCK
    ENDIF


    RETURN lRet


 /// <exclude />
METHOD __SetStatusObject  ()
    BEGIN LOCK oLock
        ogStatus := SELF
    END LOCK


    RETURN SELF


/// <include file="Internet.xml" path="doc/CSession.AccessType/*" />
ACCESS AccessType
    RETURN SELF:nAccessType




/// <include file="Internet.xml" path="doc/CSession.AccessType/*" />
ASSIGN AccessType(nNew)
    IF IsNumeric(nNew)
       SELF:nAccessType := nNew
    ENDIF
    RETURN


/// <include file="Internet.xml" path="doc/CSession.dtor/*" />
DESTRUCTOR()
    SELF:Destroy()


/// <include file="Internet.xml" path="doc/CSession.Destroy/*" />
METHOD Destroy() AS VOID
    SELF:CloseRemote()


    IF SELF:hSession != NULL_PTR
       IF SELF:lStatus
          SELF:__DelStatus (SELF:hSession)
       ENDIF


       IF InternetCloseHandle(SELF:hSession)
          SELF:hSession := NULL_PTR
       ENDIF
    ENDIF
    // No need to run the destructor anymore.
    UnregisterAxit(SELF)
    RETURN








/// <include file="Internet.xml" path="doc/CSession.CloseFile/*" />
METHOD CloseFile(hFile)
    LOCAL lRet AS LOGIC


    IF IsPtr(hFile) .AND. (hFile != NULL_PTR)
        SELF:__DelStatus(hFile)
        lRet := InternetCloseHandle(hFile)
        IF !lRet
           SELF:Error := GetLastError()
        ENDIF
    ENDIF


    SELF:SetResponseStatus()


    RETURN lRet








/// <include file="Internet.xml" path="doc/CSession.CloseRemote/*" />
METHOD CloseRemote()
    LOCAL lRet AS LOGIC


    IF SELF:hConnect != NULL_PTR
       IF SELF:lStatus
          lRet := SELF:__DelStatus (SELF:hConnect)
       ENDIF


       IF InternetCloseHandle(SELF:hConnect)
          SELF:hConnect := NULL_PTR
       ENDIF
       lRet := TRUE
    ENDIF




    RETURN lRet






/// <include file="Internet.xml" path="doc/CSession.Connected/*" />
ACCESS Connected
    RETURN (SELF:hConnect != NULL_PTR)




/// <include file="Internet.xml" path="doc/CSession.ConnectHandle/*" />
ACCESS ConnectHandle
    RETURN SELF:hConnect






/// <include file="Internet.xml" path="doc/CSession.ConnectRemote/*" />
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
           SELF:SetResponseStatus()
           lRet := .F.
           IF SELF:nError = 0
               SELF:nError := ERROR_INTERNET_NAME_NOT_RESOLVED
           ENDIF
        ELSE
           SELF:__SetStatus(SELF:hConnect)
           SELF:SetResponseStatus()
        ENDIF
        SELF:__DelStatusObject()
    ENDIF


    #IFDEF __DEBUG__
        DebOut32("  --> CSession:ConnectRemote() - END")
    #ENDIF


    RETURN lRet


/// <include file="Internet.xml" path="doc/CSession.Error/*" />
ACCESS Error
    RETURN SELF:nError


/// <include file="Internet.xml" path="doc/CSession.Error/*" />
ASSIGN Error(n)
    IF IsNumeric(n)
       SELF:nError := n
    ENDIF


    RETURN




/// <include file="Internet.xml" path="doc/CSession.ErrorMsg/*" />
ACCESS ErrorMsg
    RETURN SystemErrorString(SELF:nError, "FTP Error " + NTrim(SELF:nError))


/// <include file="Internet.xml" path="doc/CSession.Handle/*" />
ACCESS Handle
    RETURN SELF:hSession


/// <include file="Internet.xml" path="doc/CSession.ctor/*" />
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


    DEFAULT(@lStat, TRUE)
    SELF:lStatus  := lStat






    RETURN




/// <include file="Internet.xml" path="doc/CSession.InternetStatus/*" />
METHOD InternetStatus(nContext, nStatus, pStatusInfo, nStatusLength)


    RETURN NIL




/// <include file="Internet.xml" path="doc/CSession.Open/*" />
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
            NOP
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
        IF lRet
            SELF:__SetStatus(SELF:hSession)
        ENDIF
    ENDIF


    SELF:SetResponseStatus()


    RETURN lRet




/// <include file="Internet.xml" path="doc/CSession.OpenFlags/*" />
ACCESS OpenFlags
    RETURN SELF:nOpenFlags


/// <include file="Internet.xml" path="doc/CSession.OpenFlags/*" />
ASSIGN OpenFlags(n)
    IF IsNumeric(n)
       SELF:nOpenFlags := n
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CSession.PassWord/*" />
ACCESS PassWord
    RETURN SELF:cPassWord




/// <include file="Internet.xml" path="doc/CSession.PassWord/*" />
ASSIGN PassWord(cNew)
    IF IsString(cNew)
       SELF:cPassWord := cNew
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CSession.Port/*" />
ACCESS Port
    RETURN SELF:nPort


/// <include file="Internet.xml" path="doc/CSession.Port/*" />
ASSIGN Port(n)
    IF IsNumeric(n)
       SELF:nPort := n
    ENDIF


    RETURN




/// <include file="Internet.xml" path="doc/CSession.Proxy/*" />
ACCESS Proxy
    RETURN SELF:cProxy


/// <include file="Internet.xml" path="doc/CSession.Proxy/*" />
ASSIGN Proxy(cNew)
    IF IsString(cNew)
        SELF:cProxy := cNew
    ENDIF


    RETURN




/// <include file="Internet.xml" path="doc/CSession.ProxyBypass/*" />
ACCESS ProxyBypass
    RETURN SELF:cProxyBypass


/// <include file="Internet.xml" path="doc/CSession.ProxyBypass/*" />
ASSIGN ProxyBypass(cNew)
    IF IsString(cNew)
       SELF:cProxyBypass := cNew
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CSession.RemoteHost/*" />
ACCESS RemoteHost
    // Returns the remote computer (mail server)
    RETURN SELF:cHostAddress


/// <include file="Internet.xml" path="doc/CSession.RemoteHost/*" />
ASSIGN RemoteHost(cServer)
    // Sets the remote computer (mail server)
    IF IsString(cServer)
       //SELF:cHostAddress := CheckHostIP(cServer)
       SELF:cHostAddress := cServer
    ENDIF


    RETURN




/// <include file="Internet.xml" path="doc/CSession.SetResponseStatus/*" />
METHOD SetResponseStatus()
    LOCAL dwError                   AS DWORD
    LOCAL dwSize                    AS DWORD
    LOCAL DIM abStatus[STATUS_SIZE] AS BYTE
    LOCAL cStatus                   AS STRING
    LOCAL dwContext                 AS DWORD


    dwSize := STATUS_SIZE


    IF InternetGetLastResponseInfo(@dwError, @abStatus[1], @dwSize)
       SELF:nError := dwError


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




/// <include file="Internet.xml" path="doc/CSession.UserName/*" />
ACCESS UserName
    RETURN SELF:cUserName




/// <include file="Internet.xml" path="doc/CSession.UserName/*" />
ASSIGN UserName(cNew)
    IF IsString(cNew)
       SELF:cUserName := cNew
    ENDIF


    RETURN






END CLASS




/// <exclude/>
DELEGATE __INTStatusCallback_Delegate( hInternet AS PTR, dwContext AS DWORD, dwInternetStatus AS DWORD, pStatusInformation AS PTR, dwStatusInfoLength AS DWORD ) AS VOID


/// <include file="Internet.xml" path="doc/INTStatusCallback/*" />
FUNCTION INTStatusCallback( hInternet           AS PTR, ;
                            dwContext           AS DWORD,;
                            dwInternetStatus    AS DWORD,;
                            pStatusInformation  AS PTR,;
                            dwStatusInfoLength  AS DWORD)  AS VOID STRICT
    LOCAL oSession  AS CSession
    LOCAL n         AS DWORD


    BEGIN LOCK cSession.oLock


        IF (n := __FindStatus(hInternet)) > 0
           oSession := agStatus[n, STATUS_OP]
        ELSE
           oSession := cSession.ogStatus
        ENDIF


        IF ! oSession == NULL_OBJECT
           oSession:InternetStatus(dwContext, dwInternetStatus, pStatusInformation, dwStatusInfoLength)
        ENDIF


    END LOCK


    RETURN




//STATIC GLOBAL ogStatus AS OBJECT






#region defines
DEFINE  STATUS_HANDLE   := 1
DEFINE  STATUS_OP       := 2
DEFINE  STATUS_SIZE := 1024
#endregion
