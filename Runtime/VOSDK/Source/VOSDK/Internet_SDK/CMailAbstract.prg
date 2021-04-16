/// <include file="Internet.xml" path="doc/CMailAbstract/*" />
CLASS CMailAbstract


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


 /// <exclude />
METHOD __SendLine(cData) 


	SELF:nError := 0


	IF SELF:oSocket:SendLine(cData) < 0
		SELF:nError := SELF:oSocket:Error
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Internet.xml" path="doc/CMailAbstract.dtor/*" />
DESTRUCTOR() 
    SELF:Destroy()


/// <include file="Internet.xml" path="doc/CMailAbstract.Destroy/*" />
METHOD Destroy() AS VOID
	IF SELF:lSocketOpen
		SELF:Close()
		SELF:lSocketOpen := .F. 
	ENDIF
	UnregisterAxit(SELF)
    RETURN


/// <include file="Internet.xml" path="doc/CMailAbstract.Close/*" />
METHOD Close() 


	IF SELF:lSocketOpen
		SELF:oSocket:Destroy()
		SELF:lSocketOpen := .F. 
		RETURN .T. 
	ENDIF
	UnregisterAxit(SELF)


	RETURN .F. 


/// <include file="Internet.xml" path="doc/CMailAbstract.DomainName/*" />
ACCESS DomainName 
	RETURN SELF:cDomainName


/// <include file="Internet.xml" path="doc/CMailAbstract.DomainName/*" />
ASSIGN DomainName(uValue) 
	SELF:cDomainName := uValue
	RETURN 


/// <include file="Internet.xml" path="doc/CMailAbstract.Error/*" />
ACCESS Error() 
    RETURN SELF:nError


/// <include file="Internet.xml" path="doc/CMailAbstract.Error/*" />
ASSIGN Error(n) 
    IF IsNumeric(n)
        SELF:nError := n
    ENDIF


    RETURN 




/// <include file="Internet.xml" path="doc/CMailAbstract.ErrorMsg/*" />
ACCESS ErrorMsg 


	LOCAL cRet  AS STRING


	IF SLen(SELF:cReply) > 0
		cRet := SELF:cReply
	ELSE
		cRet := SystemErrorString(SELF:nError, "Session Error " + NTrim(SELF:nError))
	ENDIF


	RETURN cRet


/// <include file="Internet.xml" path="doc/CMailAbstract.ctor/*" />
CONSTRUCTOR(nPort, cServer) 


	SELF:nFlags := 0
	SELF:nTries := 50


	IF SELF:Open()
		SELF:DomainName := HostName()
	ENDIF


	SELF:RemotePort := nPort	//	the calling class (SMTP or POP) will set this
	SELF:RemoteHost := cServer	// converts string server name to IP string - must exist


	RETURN 


/// <include file="Internet.xml" path="doc/CMailAbstract.InternetStatus/*" />
METHOD InternetStatus( nContext, nStatus, xStatus, nStatusLength ) 


	// Stub method to be replaced in inherited classes


	RETURN .T. 


/// <include file="Internet.xml" path="doc/CMailAbstract.Open/*" />
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


/// <include file="Internet.xml" path="doc/CMailAbstract.PassWord/*" />
ACCESS PassWord 
	RETURN SELF:cPassWord


/// <include file="Internet.xml" path="doc/CMailAbstract.PassWord/*" />
ASSIGN PassWord(uValue) 
	SELF:cPassWord := uValue
	RETURN 


/// <include file="Internet.xml" path="doc/CMailAbstract.RecvRemote/*" />
METHOD RecvRemote() 


	SELF:cReply := SELF:oSocket:GetLine()


	RETURN .T. 


/// <include file="Internet.xml" path="doc/CMailAbstract.RemoteHost/*" />
ACCESS RemoteHost() 
	//
	// Returns the remote computer(mail server)
	//
	RETURN SELF:cHostAddress


/// <include file="Internet.xml" path="doc/CMailAbstract.RemoteHost/*" />
ASSIGN RemoteHost(cServer) 
	//
	// Sets the remote computer(mail server)
	//
	IF IsString(cServer)
		SELF:cHostAddress := CheckHostIP(cServer)
	ENDIF


	RETURN 
/// <include file="Internet.xml" path="doc/CMailAbstract.RemotePort/*" />
ACCESS RemotePort() 
    //
    // Returns the internet port to be used on the remote computer
    //
    RETURN SELF:wHostPort


/// <include file="Internet.xml" path="doc/CMailAbstract.RemotePort/*" />
ASSIGN RemotePort(wNew) 
    //
    // Sets the internet port to be used on the remote computer
    //
    IF IsNumeric(wNew)
        SELF:wHostPort := wNew
    ENDIF


	RETURN 




/// <include file="Internet.xml" path="doc/CMailAbstract.ReplyCode/*" />
ACCESS  ReplyCode() 
	//
	// Returns a response code received from the remote computer
	//
	RETURN SELF:nReply


/// <include file="Internet.xml" path="doc/CMailAbstract.ReplyString/*" />
ACCESS  ReplyString() 
	//
	// Returns a string received from the remote computer
	//
	RETURN SELF:cReply


/// <include file="Internet.xml" path="doc/CMailAbstract.SendData/*" />
METHOD SendData(cData) 
	LOCAL lRet AS LOGIC


	IF IsString(cData)
		lRet := SELF:__SendLine(cData + CRLF)
	ENDIF


	RETURN lRet


/// <include file="Internet.xml" path="doc/CMailAbstract.SendRaw/*" />
METHOD SendRaw(cData) 
	SELF:nError := 0
	
	
	IF SELF:oSocket:SendRawText(cData)
		RETURN TRUE
	ENDIF


	SELF:nError := SELF:oSocket:Error
	
	
	RETURN FALSE


/// <include file="Internet.xml" path="doc/CMailAbstract.SendRemote/*" />
METHOD SendRemote(cData) 
	LOCAL lRet AS LOGIC


	IF IsString(cData)
		lRet := SELF:__SendLine(cData)
	ENDIF


	RETURN lRet


/// <include file="Internet.xml" path="doc/CMailAbstract.Timeout/*" />
ACCESS Timeout() 
	//
	// Returns the length of time that this control will wait for response
	//
	RETURN SELF:oSocket:TimeOut


/// <include file="Internet.xml" path="doc/CMailAbstract.Timeout/*" />
ASSIGN Timeout(nNew) 
	//
	// Sets the length of time that this control will wait for response
	//
	IF IsNumeric(nNew)
		SELF:oSocket:Timeout := nNew
	ENDIF


	RETURN 


/// <include file="Internet.xml" path="doc/CMailAbstract.TimeoutRetries/*" />
ACCESS TimeoutRetries() 
	//
	// Returns the length of time that this control will wait for response
	//
	RETURN SELF:oSocket:TimeOutRetries


/// <include file="Internet.xml" path="doc/CMailAbstract.TimeoutRetries/*" />
ASSIGN TimeoutRetries(nNew) 
 //
 // Sets the length of time that this control will wait for response
 //
 IF IsNumeric(nNew)
  SELF:oSocket:TimeOutRetries := nNew
 ENDIF


 RETURN 


/// <include file="Internet.xml" path="doc/CMailAbstract.UserName/*" />
ACCESS UserName 
	RETURN SELF:cUserName


/// <include file="Internet.xml" path="doc/CMailAbstract.UserName/*" />
ASSIGN UserName(uValue) 
	SELF:cUserName := uValue
	RETURN 
END CLASS


