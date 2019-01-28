CLASS CFtp INHERIT CSession

METHOD Append (cLocalFile, nFlags) 

    LOCAL lRet          AS LOGIC
    LOCAL cRemoteFile   AS STRING
    LOCAL cCommand      AS STRING
    LOCAL nPos          AS DWORD
    LOCAL nDefFlags     AS DWORD
    LOCAL hFTP          AS PTR

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    nDefFlags := FTP_TRANSFER_TYPE_BINARY
    DEFAULT(@nFlags, nDefFlags)

    DEFAULT(@cLocalFile, "")

    IF !File(cLocalFile)
        RETURN .F. 
    ENDIF

    nPos := RAt("\", cLocalFile)
    IF nPos > 0
        cRemoteFile := SubStr2(cLocalFile, nPos + 1)
    ELSE
        cRemoteFile := cLocalFile
    ENDIF

    cCommand := "APPEND " + cRemoteFile

    lRet := FtpCommand(SELF:hConnect, .F. , nFlags, String2Psz( cCommand),  SELF:__GetStatusContext(), @hFTP)

    RETURN lRet

METHOD ConnectRemote(cIP, cID, cPw, lBypass) 

	LOCAL lRet          AS LOGIC

	DEFAULT(@cID, "")
	DEFAULT(@cPw, "")
	DEFAULT(@lBypass, FALSE)

	SELF:Password := cPw
	SELF:UserName := cID

    //  UH 02/28/2001 New argument nFlags, can be one of the following:
    //  INTERNET_FLAG_ASYNC ...... ....... Only asynchronous requests on handles descended from that connection
    //  INTERNET_FLAG_CACHE_ASYNC ........ Allows a lazy cache write.
    //  INTERNET_FLAG_DONT_CACHE  ........ Used by FtpFindFirstFile, FtpGetFile, FtpOpenFile, FtpPutFile and InternetOpenUrl.
    //  INTERNET_FLAG_EXISTING_CONNECT ... Use an existing InternetConnect OBJECT
    //  INTERNET_FLAG_HYPERLINK .......... Forces a reload iF there was no Expires time
    //                                     and no Last-Modified time returned from the server when determining
    //                                     whether to reload the item from the network.
    //                                     Can be used by FtpFindFirstFile, FtpGetFile, FtpOpenFile, FtpPutFile and InternetOpenUrl.
    //  INTERNET_FLAG_NEED_FILE .......... Causes a temporary file to be created if the file
    //                                     cannot be cached. Can be used by FtpFindFirstFile, FtpGetFile, FtpOpenFile, FtpPutFile,
    //                                     and InternetOpenUrl.
    //  INTERNET_FLAG_NO_CACHE_WRITE ..... FtpFindFirstFile, FtpGetFile, FtpOpenFile, FtpPutFile and InternetOpenUrl.
    //  INTERNET_FLAG_PASSIVE ............ Use passive FTP semantics. Only InternetConnect and
    //                                     InternetOpenUrl use this flag. InternetConnect uses this flag for FTP requests,
	//										and InternetOpenUrl uses this flag for FTP files and directories.
    //  INTERNET_FLAG_RELOAD ............. Forces a download of the requested file (directory listing) from server,
    //                                     not from the cache.
    //  INTERNET_FLAG_RESYNCHRONIZE ...... Reloads resources if has been modified since the last time it was downloaded.
    //  INTERNET_FLAG_SECURE ............. Uses secure transaction semantics. This translates to using Secure Sockets Layer/PRIVATE Communications Technology (SSL/PCT) and IS only meaningful IN HTTP requests. THIS flag IS used by HttpOpenRequest and InternetOpenUrl, but THIS IS made redundant IF "https://" appears IN the URL.
    //  INTERNET_FLAG_TRANSFER_ASCII ..... Transfers file AS ASCII. Can be used by FtpOpenFile,
    //                                     FtpGetFile, and FtpPutFile.
    //  INTERNET_FLAG_TRANSFER_BINARY .... Transfers file as binary. Can be used by FtpOpenFile,
    //                                     FtpGetFile, and FtpPutFile.
    //  WININET_API_FLAG_ASYNC ........... Forces asynchronous operations.
    //  WININET_API_FLAG_SYNC ............ Forces synchronous operations.
    //  WININET_API_FLAG_USE_CONTEXT ..... Forces the API to use the context value

	IF lBypass
		// this bypasses cFTP class where the INTERNET_FLAG_PASSIVE is used inadvisedly - Uwe Holz 01/03/01
		lRet := SUPER:ConnectRemote(cIP, INTERNET_SERVICE_FTP, 0, SELF:__GetStatusContext())
	ELSE
		lRet := SUPER:ConnectRemote(cIP, INTERNET_SERVICE_FTP, INTERNET_FLAG_PASSIVE, SELF:__GetStatusContext())
	ENDIF

	IF SELF:hConnect = NULL_PTR
		SELF:InternetStatus(SELF:__GetStatusContext(), INTERNET_STATUS_CONNECTION_CLOSED, NULL_PTR, 0)
		SELF:nError := DWORD(_CAST,WSAGetLastError())
		IF SELF:nError = 0
			SELF:nError := ERROR_INTERNET_NAME_NOT_RESOLVED
		ENDIF
	ELSE
	ENDIF

	RETURN lRet

METHOD CreateDir(cRemoteDir) 

    LOCAL lRet AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    IF !IsString(cRemoteDir) .OR. SLen(cRemoteDir) = 0
        RETURN .F. 
    ENDIF

    lRet := FtpCreateDirectory(SELF:hConnect, cRemoteDir)

    IF !lRet
        SELF:nError := GetLastError()
    ENDIF

    RETURN lRet

METHOD DeleteFile(cRemoteFile) 

    LOCAL lRet AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    IF SLen(cRemoteFile) = 0
        RETURN .F. 
    ENDIF

    lRet := FtpDeleteFile(SELF:hConnect, cRemoteFile)

    IF !lRet
        SELF:nError := GetLastError()
    ENDIF

    RETURN lRet

METHOD Directory (cFile, nFlags) 

    LOCAL pData      AS _WINWIN32_FIND_DATA
    LOCAL hFind      AS PTR
    LOCAL aRet       AS ARRAY
    LOCAL aTemp      AS ARRAY
    LOCAL nContext   AS DWORD

    aRet := {}

    DEFAULT(@cFile, "*.*")

    //  UH 05/25/2000
    DEFAULT(@nFlags, INTERNET_FLAG_RELOAD + INTERNET_FLAG_RESYNCHRONIZE)

    IF SELF:hConnect = NULL_PTR
        RETURN aRet
    ENDIF

    pData := MemAlloc(_SIZEOF(_WINWIN32_FIND_DATA) )

    nContext := SELF:__GetStatusContext()

    //  Start enumeration and get file handle
    hFind := IFXFtpFindFirstFile(SELF:hConnect, cFile, pData, nFlags, nContext)

    IF hFind = NULL_PTR
        SELF:nError := GetLastError()
    ELSE
        SELF:__SetStatus(hFind)
        DO WHILE hFind != NULL_PTR
            aTemp := __GetFileData(pData)
            AAdd(aRet, aTemp)
            IF !IFXInternetFindNextFile(hFind, pData)
                SELF:nError := GetLastError()
                IF SELF:nError = DWORD(ERROR_NO_MORE_FILES)
                    SELF:nError := 0
                ENDIF
                InternetCloseHandle(hFind)
                SELF:__DelStatus(hFind)
                hFind := NULL_PTR
            ENDIF
        ENDDO
    ENDIF

    MemFree(pData)

    RETURN aRet

METHOD GetCurDir() 

    LOCAL     lRet              AS LOGIC
    LOCAL     cRet              AS STRING
    LOCAL DIM abTemp[_MAX_PATH] AS BYTE
    LOCAL     nSize             AS DWORD
    LOCAL     pSave             AS PTR

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    nSize := _MAX_PATH

    IF SELF:lStatus
        pSave := InternetSetStatusCallback( SELF:hConnect, NULL_PTR)
    ENDIF

    lRet  := FtpGetCurrentDirectory(SELF:hConnect, @abTemp[1], @nSize)
      
    //RvdH 070407 Restore not recreate statuscallback
    //SELF:__SetStatus(SELF:hConnect)                
    InternetSetStatusCallback( SELF:hConnect, pSave )


    IF lRet
        cRet := "/" + Psz2String(@abTemp)
    ELSE
        SELF:nError := GetLastError()
    ENDIF

    RETURN cRet

METHOD GetFile(cRemoteFile, cNewFile, lFailIfExists, nFlags) 

    LOCAL lRet AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    DEFAULT(@lFailIfExists, .F. )
    DEFAULT(@nFlags, INTERNET_FLAG_TRANSFER_BINARY)
    DEFAULT(@cRemoteFile, "")
    DEFAULT(@cNewFile, "")

    IF SLen(cRemoteFile) = 0
        RETURN .F. 
    ENDIF

    IF SLen(cNewFile) = 0
        cNewFile := cRemoteFile
    ENDIF

    lRet := FtpGetFile(SELF:hConnect, cRemoteFile, cNewFile, lFailIfExists, FC_ARCHIVED, nFlags, SELF:__GetStatusContext())
    // RvdH 081105 suggestion from D Herijgers
    IF !lRet
        SELF:nError := GetLastError()
        IF SELF:nError == ERROR_INTERNET_EXTENDED_ERROR
            SELF:SetResponseStatus()
        ENDIF
    ELSE

        //  UH 02/27/2001
        SELF:SetResponseStatus()
    ENDIF
    RETURN lRet

CONSTRUCTOR (cCaption, n, lStat) 

    IF !IsString(cCaption) .OR. IsNil(cCaption)
        cCaption := "VO Ftp Client"
    ENDIF

    IF IsNumeric(n)
    ELSE
        n := INTERNET_DEFAULT_FTP_PORT
    ENDIF

    SUPER(cCaption, n, lStat)

    SELF:AccessType := INTERNET_OPEN_TYPE_DIRECT

    // RvdH 050302 Not needed. Is handled in cSession
    // RegisterAxit(SELF)

    RETURN 

METHOD InternetStatus(nContext, nStatus, pStatusInfo, nStatusLength) 
	//
	//	This method receives all Low Level FTP notification. Please keep all
	//	parameters if you want to overwrite it for your own purpose
	//
    LOCAL cMsg  AS USUAL

    cMsg := NIL	// used to detect no update required

    DO CASE
    CASE nStatus == INTERNET_STATUS_RESOLVING_NAME
        cMsg := "Resolving Name ... "

    CASE nStatus == INTERNET_STATUS_NAME_RESOLVED
        cMsg := "Name resolved"

    CASE nStatus == INTERNET_STATUS_CONNECTING_TO_SERVER
        cMsg := "Connecting to Server ... "

    CASE nStatus == INTERNET_STATUS_CONNECTED_TO_SERVER
        cMsg := "Connected to Server"

    CASE nStatus == INTERNET_STATUS_SENDING_REQUEST
//        cMsg := "Sending Request ... "

    CASE nStatus == INTERNET_STATUS_REQUEST_SENT
        cMsg := "Request sent"

    CASE nStatus == INTERNET_STATUS_RECEIVING_RESPONSE
//        cMsg := "Receiving response ..."

    CASE nStatus == INTERNET_STATUS_RESPONSE_RECEIVED
        cMsg := "Response received"

    CASE nStatus == INTERNET_STATUS_CTL_RESPONSE_RECEIVED
//        cMsg := "CTL Response received"

    CASE nStatus == INTERNET_STATUS_PREFETCH
        cMsg := "Prefetch"

    CASE nStatus == INTERNET_STATUS_CLOSING_CONNECTION
        cMsg := "Closing Connection ..."

    CASE nStatus == INTERNET_STATUS_CONNECTION_CLOSED
        cMsg := "Connection closed"

    CASE nStatus == INTERNET_STATUS_HANDLE_CREATED
//        cMsg := "Handle created"

    CASE nStatus == INTERNET_STATUS_HANDLE_CLOSING
//        cMsg := "Closing handle ..."

    CASE nStatus == INTERNET_STATUS_REQUEST_COMPLETE
        cMsg := "Request complete"

    CASE nStatus == INTERNET_STATUS_REDIRECT
        cMsg := "Redirect"

    OTHERWISE
        cMsg := "Unkown FTP Status"

    ENDCASE

    RETURN cMsg

METHOD Open(nFlags, xProxy, aProxyByPass) 

    LOCAL lRet AS LOGIC

    lRet := SUPER:Open(nFlags, xProxy, aProxyByPass)

    RETURN lRet

METHOD OpenFile(cRemoteFile, nAccess, nFlags) 

    LOCAL hRet      AS PTR
    LOCAL n         AS DWORD
    LOCAL nContxt   AS DWORD

    DEFAULT(@nFlags, FTP_TRANSFER_TYPE_BINARY + INTERNET_FLAG_RELOAD)
    DEFAULT(@nAccess, 0)
    DEFAULT(@cRemoteFile, "")

    nContxt := SELF:__GetStatusContext()

    IF nAccess = 0
        n := DWORD(_CAST, GENERIC_READ)
    ELSE
        n := nAccess
    ENDIF

    IF SLen(cRemoteFile) > 0
        hRet := FtpOpenFile(SELF:hConnect, cRemoteFile, n, nFlags, nContxt)

        IF hRet = NULL_PTR
            SELF:nError := GetLastError()
        ELSE
            SELF:__SetStatus(hRet)
        ENDIF
    ENDIF

    RETURN hRet

ASSIGN Proxy(cNew) 

    IF IsString(cNew)
    	// UH 30/07/2001
    	IF SLen(cNew) > 0
			IF AtC("=", cNew) == 0
				cNew := "ftp=ftp://" + cNew
			ENDIF
		ENDIF

        SELF:cProxy := cNew
    ENDIF

    RETURN SELF:cProxy

METHOD PutFile(cLocalFile, cRemoteFile, lFailIfExists, nFlags) 

    LOCAL lRet      AS LOGIC
    LOCAL lFound    AS LOGIC
    LOCAL nPos      AS DWORD
    LOCAL nDefFlags AS DWORD
    LOCAL pData     AS _WINWIN32_FIND_DATA
    LOCAL hFind     AS PTR

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    //  UH 10/28/2001
    //  Default(@nFlags, 0)
    nDefFlags := FTP_TRANSFER_TYPE_BINARY
    DEFAULT(@nFlags, nDefFlags)

    DEFAULT(@cLocalFile, "")
    DEFAULT(@cRemoteFile, "")
    DEFAULT(@lFailIfExists, .F. )

    IF !File(cLocalFile)
        RETURN .F. 
    ENDIF

    IF SLen(cRemoteFile) = 0
        nPos := RAt("\", cLocalFile)
        IF nPos > 0
            cLocalFile := SubStr2(cLocalFile, nPos + 1)
        ENDIF
        cRemoteFile := cLocalFile
    ENDIF

    IF lFailIfExists
        pData := MemAlloc(_SIZEOF(_WINWIN32_FIND_DATA) )

        //  UH 04/09/2001
        hFind := IFXFtpFindFirstFile(SELF:hConnect, cRemoteFile, pData, 0, 0)

        IF hFind = NULL_PTR
            SELF:nError := 0
        ELSE
            lFound := .T. 
            InternetCloseHandle(hFind)
        ENDIF
        MemFree(pData)
    ENDIF

    IF lFound
        lRet := .F. 
        SELF:nError := ERR_FILE_EXISTS
    ELSE
        lRet := FtpPutFile(SELF:hConnect, cLocalFile, cRemoteFile, nFlags, SELF:__GetStatusContext())
        // RvdH 081105 suggestion from D Herijgers
        IF !lRet
            SELF:nError := GetLastError()
            IF SELF:nError == ERROR_INTERNET_EXTENDED_ERROR
                SELF:SetResponseStatus()
            ENDIF
        ELSE
            //  UH 02/27/2001
            SELF:SetResponseStatus()
        ENDIF
    ENDIF

    RETURN lRet

METHOD RemoveDir(cRemoteDir) 

    LOCAL lRet AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    IF !IsString(cRemoteDir) .OR. SLen(cRemoteDir) = 0
        RETURN .F. 
    ENDIF

    lRet := FtpRemoveDirectory(SELF:hConnect, cRemoteDir)

    IF !lRet
        SELF:nError := GetLastError()
    ENDIF

    RETURN lRet

METHOD RenameFile(cRemoteFile, cNewName) 

    LOCAL lRet AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    IF SLen(cRemoteFile) = 0 .OR. SLen(cNewName) = 0
        RETURN .F. 
    ENDIF

    lRet := FtpRenameFile(SELF:hConnect, cRemoteFile, cNewName)

    IF !lRet
        SELF:nError := GetLastError()
    ENDIF

    RETURN lRet


METHOD SetCurDir(cRemoteDir) 

    LOCAL lRet     AS LOGIC

    IF SELF:hConnect = NULL_PTR
        RETURN .F. 
    ENDIF

    IF !IsString(cRemoteDir) .OR. SLen(cRemoteDir) = 0
        RETURN .F. 
    ENDIF

    lRet := FtpSetCurrentDirectory(SELF:hConnect, cRemoteDir)

    IF !lRet
        SELF:nError := GetLastError()
    ENDIF

    RETURN lRet


END CLASS

