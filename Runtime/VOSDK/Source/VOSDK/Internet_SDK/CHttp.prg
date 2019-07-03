CLASS CHttp  INHERIT CSession
    PROTECT hRequest          	AS PTR
    PROTECT cResponse         	AS STRING
    PROTECT cResponseHeader   	AS STRING
    PROTECT cCurDir				AS STRING
    PROTECT cCurrentUrl         AS STRING
	PROTECT	lFtpRequest			AS LOGIC


METHOD __GetUrl					(c)										
	LOCAL cRet     	AS STRING
	LOCAL n			AS DWORD

	cRet := SELF:cCurrentUrl
	n := SLen(cRet)
	IF SubStr2(cRet, n) == "/"
		cRet := SubStr3(cRet, 1, n-1)
	ENDIF

	cRet += SELF:cCurDir

	IF IsString(c)
		cRet += c
	ENDIF

	RETURN cRet

METHOD AddRequestHeaders(cHeaders, nModifiers)              
	LOCAL lRet	AS LOGIC

	IF SELF:hRequest != NULL_PTR
		lRet := HttpAddRequestHeaders(SELF:hRequest, cHeaders, SLen(cHeaders), nModifiers)
	ENDIF

	RETURN lRet

METHOD CloseRemote  ()                                  
    RETURN SUPER:CloseRemote()


METHOD CloseRequest ()                                      
    IF SELF:hRequest = NULL_PTR
        RETURN .F. 
    ELSE
        SELF:__DelStatus(SELF:hRequest)
        InternetCloseHandle(SELF:hRequest)
        SELF:hRequest := NULL_PTR
    ENDIF

    RETURN .T. 



METHOD ConnectRemote(cIP, cID, cPw)                     
	LOCAL lRet		AS LOGIC

    DEFAULT(@cID, "")
    DEFAULT(@cPw, "")

    SELF:Password := cPw
    SELF:UserName := cID

    lRet := SUPER:ConnectRemote(cIP, INTERNET_SERVICE_HTTP, 0, 0)

    IF lRet
		IF At2("//", cIP) == 0
			IF SELF:lFtpRequest
				cIP := "ftp://" + cIP
			ELSE
				cIP := "http://" + cIP
			ENDIF
		ENDIF

		SELF:cCurrentUrl := cIP
	ENDIF

	RETURN lRet



METHOD Directory    ()                                 
	LOCAL cTemp			AS STRING
	LOCAL cUrl 			AS STRING
	LOCAL cLine			AS STRING
	LOCAL aTemp			AS ARRAY
	LOCAL nPos 			AS DWORD
	LOCAL n				AS DWORD
	LOCAL aRet			AS ARRAY

	cUrl  := SELF:__GetUrl()
	cTemp := SELF:GetDocumentByURL(cURL)
	#IFDEF __DEBUG__
        MemoWrit("dir.htm", cTemp)
    #ENDIF

	aRet := {}

	IF SLen (cTemp) > 0
		n := 1
		cLine := MemoLine(cTemp, 250, n)
		DO WHILE SLen(cLine) > 0
			nPos := AtC(DIR_HREF_START, cLine)

			IF nPos > 0
				aTemp := __GetFile(cLine, nPos)

				IF aTemp != NULL_ARRAY
                 	AAdd(aRet, aTemp)
              	ENDIF
          	ENDIF

			n++
			cLine := MemoLine(cTemp, 250, n)
		ENDDO
	ENDIF

	RETURN aRet


ACCESS FtpRequest											
	RETURN SELF:lFtpRequest


ASSIGN FtpRequest			(lNew)							
	IF IsLogic(lNew)
		SELF:lFtpRequest  := lNew
	ENDIF
	RETURN SELF:lFtpRequest

METHOD GetCurDir    ()                  					
	RETURN SELF:cCurDir

METHOD GetDocumentByURL		(cURL, nFlags)                          
    LOCAL cHead             AS STRING
    LOCAL cRet              AS STRING
    LOCAL lRet				AS LOGIC

    DEFAULT(@nFlags, INTERNET_FLAG_DONT_CACHE)
    DEFAULT(@cURL, SELF:cCurrentUrl)
	cRet := ""

    IF cUrl == SELF:cCurrentUrl
        RETURN SELF:GetDocumentFromserver(cUrl)
    ENDIF

    cHead := HEADER_ACCEPT

    IF SELF:hSession == NULL_PTR
        lRet := SELF:Open(NIL, SELF:cProxy, SELF:cProxyBypass)
    ELSE
		lRet := .T. 
    ENDIF

    IF lRet
        //  UH 02/11/2000
        SELF:__SetStatusObject()
        SELF:hRequest := InternetOpenUrl(SELF:hSession, String2Psz(cUrl), String2Psz(cHead), SLen(cHead), nFlags, SELF:__GetStatusContext())

        IF SELF:hRequest == NULL_PTR
        ELSE
            SELF:__SetStatus(SELF:hRequest)

            // UH 03/13/2002
            SELF:GetResponseHeader()
        	cRet := SELF:GetResponse()
            SELF:CloseRequest()
        ENDIF
        SELF:__DelStatusObject()
    ENDIF

    RETURN cRet


METHOD GetDocumentFromServer	(cServer, cDocument, cID, cPw)          
    LOCAL cRet              AS STRING

    DEFAULT(@cServer, URL_LOCAL_HOST)
    DEFAULT(@cDocument, "")

    SELF:ConnectRemote(cServer, cID, cPw)

    IF SELF:Connected
        IF SELF:OpenRequest("GET", cDocument, HTTP_VERSION)
            IF SELF:SendRequest("", NULL_PTR, 0)
                SELF:GetResponseHeader()
                cRet := SELF:GetResponse()
            ENDIF

            SELF:CloseRequest()
        ENDIF

        SELF:CloseRemote()
    ENDIF

    RETURN cRet



METHOD GetFile (cRemoteFile, cNewFile, lFailIfExists) 		
    LOCAL lRet     	AS LOGIC
    LOCAL cUrl		AS STRING
    LOCAL cRet		AS STRING

	DEFAULT(@lFailIfExists, .F. )
    DEFAULT(@cRemoteFile, "")
    DEFAULT(@cNewFile, "")

 	IF SLen(cRemoteFile) == 0
		RETURN .F. 
	ENDIF

	IF SLen(cNewFile) == 0
		cNewFile := cRemoteFile
	ENDIF

	IF lFailIfExists .AND. File(cNewFile)
		RETURN .F. 
	ENDIF

	cUrl := SELF:__GetUrl(cRemoteFile)

	cRet := SELF:GetDocumentByURL(cURL)

    IF SLen(cRet)== 0
        lRet := .F. 
	ELSE
		MemoWrit(cNewFile, cRet)
		lRet := .T. 
    ENDIF

    RETURN lRet


METHOD GetResponse          ()                              
    LOCAL cRet  				AS STRING
    LOCAL DIM abTemp[MAX_SOCKBUFF]    AS BYTE
    LOCAL nSize					AS DWORD

    cRet := ""

    DO WHILE InternetReadFile(SELF:hRequest, @abTemp[1], MAX_SOCKBUFF, @nSize)
        IF nSize == 0
            EXIT
        ENDIF

        cRet += Mem2String(@abTemp[1], nSize)
    ENDDO

    SELF:cResponse := cRet

    RETURN cRet




METHOD GetResponseHeader    ()                              
    LOCAL cRet  	AS STRING
    LOCAL pBuffer	AS PTR
    LOCAL nSize		AS DWORD

  	HttpQueryInfo(SELF:hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, NULL_PTR, @nSize, NULL_PTR)

  	IF nSize > 0
  		pBuffer := MemAlloc(nSize)
  		IF pBuffer != NULL_PTR
  			IF HttpQueryInfo(SELF:hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, pBuffer, @nSize, NULL_PTR)
  				cRet := Mem2String(pBuffer, nSize)
  			ENDIF
			MemFree(pBuffer)
			SELF:cResponseHeader := cRet
		ENDIF
	ENDIF

    RETURN cRet

CONSTRUCTOR         (cCaption, n, lStat)            

    IF IsString(cCaption)
    ELSE
        cCaption := "VO Http Client"
    ENDIF

    IF IsNumeric(n)
    ELSE
        n := INTERNET_DEFAULT_HTTP_PORT
    ENDIF

    SUPER(cCaption, n, lStat)

    SELF:AccessType := INTERNET_OPEN_TYPE_DIRECT

    SELF:cCurDir := "/"

    RETURN 

METHOD Open         (nFlags, xProxy, aProxyByPass)      
    IF IsString(xProxy)
        // UH 07/30/2001
        IF SLen(xProxy) > 0
            SELF:Proxy := xProxy
	    //RvdH 050419 Moved down.
            //xProxy := SELF:Proxy
        ENDIF
        xProxy := SELF:Proxy
    ENDIF

    RETURN SUPER:Open(nFlags, xProxy, aProxyByPass)

METHOD OpenFile (cRemoteFile, nAccess, nFlags)          
    LOCAL cHead     AS STRING
    LOCAL n			AS DWORD
    LOCAL lRet		AS LOGIC
    LOCAL hRet		AS PTR
    LOCAL cUrl		AS STRING

    cHead := HEADER_ACCEPT

    IF SELF:hSession == NULL_PTR
        lRet := SELF:Open(NIL,SELF:cProxy, SELF:cProxyBypass)
    ELSE
		lRet := .T. 
    ENDIF

    IF lRet
        SELF:__SetStatusObject()
		n := INTERNET_FLAG_DONT_CACHE + INTERNET_FLAG_EXISTING_CONNECT
		cUrl := SELF:__GetUrl(cRemoteFile)
		hRet := InternetOpenUrl(SELF:hSession, String2Psz(cUrl), String2Psz(cHead), SLen(cHead), n, SELF:__GetStatusContext())

        IF hRet != NULL_PTR
            SELF:__SetStatus(hRet)
        ENDIF
        SELF:__DelStatusObject()
	ENDIF

	RETURN hRet


METHOD OpenRequest(cMethod, cDocument, nFlags)   
    LOCAL dwFlags           	AS DWORD
    LOCAL lRet                  AS LOGIC
    LOCAL DIM pszAccept[64] 	AS BYTE
    LOCAL DIM abAcceptTypes[2] 	AS PSZ
    LOCAL DIM abObj[_MAX_PATH] 	AS BYTE

    MemCopy(@pszAccept[1], String2Psz(HEADER_ACCEPT_REQ), SLen(HEADER_ACCEPT_REQ) + 1)
    abAcceptTypes[1] := @pszAccept[1]
    abAcceptTypes[2] := NULL_PSZ

    IF IsNumeric(nFlags)
    	dwFlags := nFlags
    ELSE
		dwFlags := DWORD(_CAST, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE + INTERNET_FLAG_KEEP_CONNECTION)
    ENDIF

	DEFAULT(@cDocument, "")

	IF SLen(cDocument) > 0
		MemCopy(@abObj[1], String2Psz( cDocument), SLen(cDocument) + 1)
	ELSE
		abObj[1] := 0
	ENDIF

    SELF:__SetStatusObject()

    SELF:hRequest := HttpOpenRequest(SELF:hConnect,;
                                     String2Psz(cMethod),;
                                     String2Psz(cDocument),;
                                     String2Psz(HTTP_VERSION),;
                                     NULL_PSZ,;
                                     @abAcceptTypes[1],;
                                     dwFlags,;
                                     SELF:__GetStatusContext())

    IF SELF:hRequest == NULL_PTR
        lRet := .F. 
    ELSE
        SELF:__SetStatus(SELF:hRequest)
        lRet := .T. 
    ENDIF

    SELF:__DelStatusObject()

    RETURN lRet



ASSIGN Proxy        (cNew)                              	
    IF IsString(cNew)
    	// UH 30/07/2001
    	IF SLen(cNew) > 0
			IF AtC("=", cNew) == 0
				cNew := "http=http://" + cNew
			ENDIF
		ENDIF

        SELF:cProxy := cNew
    ENDIF

    RETURN SELF:cProxy


ACCESS	Response											
	RETURN SELF:cResponse


ACCESS	ResponseHeader										
	RETURN SELF:cResponseHeader

METHOD SendRequest  (cHeaders, pData, nDataSize)            
    RETURN HttpSendRequest(SELF:hRequest, cHeaders, SLen(cHeaders), pData, nDataSize)


METHOD SetCurDir    (cRemoteDir)                 			
	LOCAL cTemp  	AS STRING
	LOCAL n         AS DWORD

	IF IsString(cRemoteDir)
		IF SLen(cRemoteDir) == 0
			SELF:cCurDir := "/"
		ELSE
			IF cRemoteDir == SELF:cCurDir
				//	RETURN SELF:cCurDir
			ELSE
				IF "/" == cRemoteDir
					SELF:cCurDir := "/"
				ELSEIF ".." == cRemoteDir
					cTemp := SELF:cCurDir
	            	n := SLen(cTemp)
	            	cTemp := SubStr(cTemp, 1, n-1)
	            	n := RAt("/", cTemp)
	            	IF n > 0
	            		cTemp := SubStr(cTemp, 1, n)
	            	ELSE
	            		cTemp := "/"
	            	ENDIF
					SELF:cCurDir := cTemp
				ELSE
					IF SubStr(cRemoteDir, 1, 1) == "/"
						cRemoteDir := SubStr2(cRemoteDir, 2)
					ENDIF
					n := SLen(cRemoteDir)
					IF SubStr(cRemoteDir, n, 1) == "/"
						cRemoteDir := SubStr3(cRemoteDir, 1, n-1)
					ENDIF
					SELF:cCurDir := SELF:cCurDir + cRemoteDir + "/"
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	RETURN SELF:cCurDir



END CLASS

STATIC FUNCTION __GetAttr	(c AS STRING, a AS ARRAY)			AS VOID STRICT

	a[F_ATTR] := "A"
	a[F_SIZE] := Val(StrTran(c, ",", ""))

	RETURN

STATIC FUNCTION __GetFile	(cLine AS STRING, nPos AS DWORD)		AS ARRAY STRICT
    LOCAL aRet 		AS ARRAY
    LOCAL cFile		AS STRING
    LOCAL cTemp1	AS STRING
    LOCAL cTemp2	AS STRING
    LOCAL cF		AS STRING
    LOCAL n 		AS DWORD

    cF := GetDateFormat()
    SetDateFormat("MM/DD/YY")

    cTemp1 := SubStr3(cLine, 1, nPos - 1)
    cTemp2 := SubStr2(cLine, nPos)

    cFile := __GetToken(cTemp2, e"\"", e"\"", .F. )

	IF (SLen(cFile) > 0) .AND. (cFile != "..")
		aRet := ArrayCreate(5)

		n := At2(" ", cTemp1)
		IF n > 0
			cTemp2 := SubStr3(cTemp1, 1, n-1)
			aRet[F_DATE] := CToD(cTemp2)
			cTemp1 := SubStr2(cTemp1, n+1)

			n := At2(" ", cTemp1)
			IF n > 0
				cTemp2 := SubStr3(cTemp1, 1, n-1)
				aRet[F_TIME] := __GetVOTime(cTemp2)
				cTemp1 := SubStr2(cTemp1, n+1)

				n := AtC("DIR", cTemp1)

				IF n > 0
					aRet[F_ATTR] := "D"
					aRet[F_NAME] := __GetFName(cFile, .T. )
				ELSE
					aRet[F_NAME] := __GetFName(cFile, .F. )
					__GetAttr(cTemp1, aRet)
				ENDIF
			ENDIF
		ENDIF
	ENDIF

    SetDateFormat(cF)

	RETURN aRet


STATIC FUNCTION __GetFName	(c AS STRING, lDir AS LOGIC)	AS STRING STRICT
	LOCAL n		AS DWORD

	n := SLen(c)

	IF lDir .AND. n > 0
		c := SubStr(c, 1, n-1)
	ENDIF

	n :=	RAt("/", c)

	IF n > 0
		c := SubStr2(c, n+1)
	ENDIF

	RETURN c


STATIC FUNCTION __GetVOTime	(c AS STRING)		AS STRING STRICT
	LOCAL n		AS INT

	UpperA(@c)
	IF At2("PM", c) > 0
		n := Val(SubStr(c, 1, 2))
		n += 12
		c := StrZero(n, 2, 0) + SubStr2(c, 3)
	ELSE
		c := StrTran(c, "AM", "")
	ENDIF

	IF SLen(c) == 5
		c += ":00"
    ENDIF

	RETURN c



#region defines
DEFINE  DIR_HREF_START     := "<A HREF="
DEFINE  DIR_HREF_STOP      := ">"
DEFINE  HEADER_ACCEPT      := "Accept: *" + "/" + "*"  + CRLF + CRLF
DEFINE  HEADER_ACCEPT_REQ   := "*" + "/" + "*"
DEFINE  URL_LOCAL_HOST     := "http://localhost"
#endregion
