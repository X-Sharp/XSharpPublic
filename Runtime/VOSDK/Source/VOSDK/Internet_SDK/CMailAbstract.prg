PARTIAL CLASS CMailAbstract

	PROTECT cHostAddress    AS STRING
	PROTECT nFlags          AS DWORD
	PROTECT nTries          AS DWORD
	PROTECT nCurState       AS DWORD
	PROTECT cReply          AS STRING
	PROTECT nReply          AS INT
	PROTECT lSocketOpen     AS LOGIC

	PROTECT nError			AS DWORD
	PROTECT cDomainName		AS STRING
	EXPORT  oSocket			AS CSocket

   PROTECT cUserName       AS STRING
   PROTECT cPassWord       AS STRING
   PROTECT wHostPort       AS WORD

METHOD __SendLine(cData) 

	SELF:nError := 0

	IF SELF:oSocket:SendLine(cData) < 0
		SELF:nError := SELF:oSocket:Error
		RETURN FALSE
	ENDIF

	RETURN TRUE

DESTRUCTOR() 

	IF SELF:lSocketOpen
		SELF:Close()
		SELF:lSocketOpen := .F. 
	ENDIF

	UnregisterAxit(SELF)
	RETURN 

METHOD Close() 

	IF SELF:lSocketOpen
		SELF:oSocket:Destroy()
		SELF:lSocketOpen := .F. 
		RETURN .T. 
	ENDIF
	IF ! InCollect()
		UnregisterAxit(SELF)
	ENDIF

	RETURN .F. 

ACCESS DomainName 
	RETURN SELF:cDomainName

ASSIGN DomainName(uValue) 
	SELF:cDomainName := uValue
	RETURN 

ACCESS Error() 
    RETURN SELF:nError

ASSIGN Error(n) 
    IF IsNumeric(n)
        SELF:nError := n
    ENDIF

    RETURN SELF:nError


ACCESS ErrorMsg 

	LOCAL cRet  AS STRING

	IF SLen(SELF:cReply) > 0
		cRet := SELF:cReply
	ELSE
		cRet := SystemErrorString(SELF:nError, "Session Error " + NTrim(SELF:nError))
	ENDIF

	RETURN cRet

CONSTRUCTOR(nPort, cServer) 

	SELF:nFlags := 0
	SELF:nTries := 50

	IF SELF:Open()
		SELF:DomainName := HostName()
	ENDIF

	SELF:RemotePort := nPort	//	the calling class (SMTP or POP) will set this
	SELF:RemoteHost := cServer	// converts string server name to IP string - must exist
   //RvdH 070407 RegisterAxit is moved to Open()
	//RegisterAxit(SELF)

	RETURN 

METHOD InternetStatus( nContext, nStatus, xStatus, nStatusLength ) 

	// Stub method to be replaced in inherited classes

	RETURN .T. 

METHOD Open() 

	SELF:nError := 0

	IF !SELF:lSocketOpen
		SELF:oSocket := StdSocket{SELF, SOCK_STREAM}

		IF SELF:oSocket:Status = SSTAT_DISCONNECTED
			SELF:lSocketOpen := .T. 
			RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
		ELSE
			SELF:nError := DWORD(_CAST,WSAGetLastError())
		ENDIF
	ENDIF
                                     
	RETURN SELF:lSocketOpen

ACCESS PassWord 
	RETURN SELF:cPassWord

ASSIGN PassWord(uValue) 
	SELF:cPassWord := uValue
	RETURN 

METHOD RecvRemote() 

	SELF:cReply := SELF:oSocket:GetLine()

	RETURN .T. 

ACCESS RemoteHost() 
	//
	// Returns the remote computer(mail server)
	//
	RETURN SELF:cHostAddress

ASSIGN RemoteHost(cServer) 
	//
	// Sets the remote computer(mail server)
	//
	IF IsString(cServer)
		SELF:cHostAddress := CheckHostIP(cServer)
	ENDIF

	RETURN SELF:cHostAddress

ACCESS RemotePort() 
    //
    // Returns the internet port to be used on the remote computer
    //
    RETURN SELF:wHostPort

ASSIGN RemotePort(wNew) 
    //
    // Sets the internet port to be used on the remote computer
    //
    IF IsNumeric(wNew)
        SELF:wHostPort := wNew
    ENDIF

	RETURN SELF:wHostPort


ACCESS  ReplyCode() 
	//
	// Returns a response code received from the remote computer
	//
	RETURN SELF:nReply

ACCESS  ReplyString() 
	//
	// Returns a string received from the remote computer
	//
	RETURN SELF:cReply

METHOD SendData(cData) 
	LOCAL lRet AS LOGIC

	IF IsString(cData)
		lRet := SELF:__SendLine(cData + CRLF)
	ENDIF

	RETURN lRet

METHOD SendRaw(cData) 
   //SE-040625

	SELF:nError := 0
	
	IF SELF:oSocket:SendRawText(cData)
		RETURN TRUE
	ENDIF

	SELF:nError := SELF:oSocket:Error
	
	RETURN FALSE

METHOD SendRemote(cData) 
	LOCAL lRet AS LOGIC

	IF IsString(cData)
		lRet := SELF:__SendLine(cData)
	ENDIF

	RETURN lRet

ACCESS Timeout() 
	//
	// Returns the length of time that this control will wait for response
	//
	RETURN SELF:oSocket:TimeOut

ASSIGN Timeout(nNew) 
	//
	// Sets the length of time that this control will wait for response
	//
	IF IsNumeric(nNew)
		SELF:oSocket:Timeout := nNew
	ENDIF

	RETURN SELF:oSocket:Timeout

ACCESS TimeoutRetries() 
	//
	// Returns the length of time that this control will wait for response
	//
	RETURN SELF:oSocket:TimeOutRetries

ASSIGN TimeoutRetries(nNew) 
 //
 // Sets the length of time that this control will wait for response
 //
 IF IsNumeric(nNew)
  SELF:oSocket:TimeOutRetries := nNew
 ENDIF

 RETURN SELF:oSocket:TimeoutRetries

ACCESS UserName 
	RETURN SELF:cUserName

ASSIGN UserName(uValue) 
	SELF:cUserName := uValue
	RETURN 
END CLASS

