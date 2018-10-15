PARTIAL CLASS CImap INHERIT CPop
    PROTECT nPrefix     AS DWORD
    PROTECT cMailBox    AS STRING


METHOD  __FetchExtract  (c)     
    LOCAL nPos      AS DWORD
    LOCAL cEnd      AS STRING

    nPos := At2(CRLF, c)
    IF nPos > 0
        c := SubStr2(c, nPos + 2)
        cEnd := ")" + CRLF + NTrim(SELF:nPrefix)
        nPos := At2(cEnd, c)
        IF nPos > 0
            c := SubStr3(c, 1, nPos -1)
        ENDIF
    ENDIF

    RETURN c


METHOD  __FetchToken    (cBuffer, cStart, cStop) 
    LOCAL nPos      AS DWORD
    LOCAL cRet      AS STRING

    nPos := AtC(cStart, cBuffer)
    IF nPos > 0
        cRet := SubStr2(cBuffer, nPos + SLen(cStart))
        nPos := AtC(cStop, cRet)
        IF nPos > 0
            cRet := SubStr(cRet, 1, nPos - 1)
        ENDIF
    ENDIF

    RETURN cRet

METHOD  __GetBody     (nMail) 
    LOCAL cBody     AS STRING
    LOCAL cTemp     AS STRING

    cTemp := "fetch " + NTrim(nMail) + " RFC822.TEXT"

    IF SELF:ClientCommand(cTemp)
        cBody := SELF:__FetchExtract(SELF:cReply)
    ENDIF

    RETURN cBody



METHOD  __GetHeader   (nMail) 
    LOCAL cHeader   AS STRING
    LOCAL cTemp     AS STRING

    cTemp := "fetch " + NTrim(nMail) + " RFC822.HEADER"
    IF SELF:ClientCommand(cTemp)
        cHeader := SELF:__FetchExtract(SELF:cReply)
    ENDIF

    RETURN cHeader


METHOD  __GetSize   (nMail) 
    LOCAL nRet      AS INT
    LOCAL cTemp     AS STRING

    cTemp := "fetch " + NTrim(nMail) + " RFC822.SIZE"
    IF SELF:ClientCommand(cTemp)
        cTemp := SELF:__FetchToken(SELF:cReply, TEMP_RFC_SIZE, ")")
        nRet := Val(cTemp)
    ENDIF

    RETURN nRet


METHOD  CheckReply      ()  
    LOCAL c     AS STRING
    LOCAL lRet  AS LOGIC
    LOCAL cLast AS STRING
    LOCAL nPos  AS DWORD

    c := NTrim(SELF:nPrefix) + " " + "OK"

    cLast := SELF:cReply

    nPos := RAt(CRLF, cLast)
    IF nPos > 0
        cLast := SubStr3(cLast, 1, nPos -1)
        nPos := RAt(CRLF, cLast)
        IF nPos > 0
            cLast := SubStr2(cLast, nPos + 2)
        ENDIF
    ENDIF

    IF SubStr(cLast, 1, 7) == c
        lRet := .T. 
    ELSE
        lRet := .F. 
    ENDIF

    RETURN lRet


METHOD  ClientCommand   (cBuffer)    
    LOCAL lRet      AS LOGIC

    Default(@cBuffer, "")

    SELF:nError := 0

    IF SELF:nCurState > 0
        IF SLen(cBuffer) > 0
            lRet := SELF:SendRemote(cBuffer + CRLF)
            IF lRet
                lRet := SELF:RecvRemote()
                IF lRet
                    lRet := SELF:CheckReply()
                ENDIF
            ENDIF
        ENDIF
    ENDIF
    RETURN lRet


METHOD  DeleteMail      (nMail)     
    LOCAL cTemp     AS STRING

    cTemp := "store " + NTrim(nMail) + " +flags \deleted"

    RETURN SELF:ClientCommand(cTemp)





METHOD  Disconnect      ()                              
    LOCAL cBuffer   AS STRING

    IF SELF:lSocketOpen
        SELF:nError := 0

        cBuffer := "LOGOUT" + CRLF

        SELF:nCurState := SENDING_REQUEST

        IF !SELF:SendRemote(cBuffer)
            RETURN .F. 
        ENDIF

        SELF:nCurState := RETREIVING_DATA

        IF !SELF:RecvRemote()
            RETURN .F. 
        ENDIF

        IF SELF:CheckReply()
            SELF:Close()
        ENDIF
        SELF:nCurState := 0
    ENDIF

    RETURN .T. 


METHOD  GetMail         (nMail) 
    LOCAL cHeader   AS STRING
    LOCAL cBody     AS STRING
    LOCAL cTemp     AS STRING
    LOCAL lRet      AS LOGIC

    cHeader := SELF:__GetHeader(nMail)

    #IFDEF __DEBUG__
		MemoWrit("header.txt", cHeader)
    #ENDIF

    IF SLen(cHeader) > 0
        cBody := SELF:__GetBody(nMail)

        #IFDEF __DEBUG__
    		MemoWrit("body.txt", cBody)
        #ENDIF

        cTemp := cHeader + cBody

        #IFDEF __DEBUG__
    		MemoWrit("raw.txt", cTemp)
        #ENDIF

        SELF:oEmail := CEmail{}

        lRet := SELF:oEmail:Decode(cTemp)
    ENDIF

    RETURN lRet


METHOD  GetPrefix       ()                              
    SELF:nPrefix := SELF:nPrefix + 1
    RETURN NTrim(SELF:nPrefix) + " "


METHOD  GetStatus       ()      
    LOCAL cBuffer   AS STRING
    LOCAL cTemp     AS STRING
    LOCAL i         AS DWORD

    SELF:nError := 0

    cBuffer := "STATUS"
    IF SLen(SELF:cMailBox) > 0
        cBuffer += " " + SELF:cMailBox
    ENDIF

    cBuffer += " (" + TEMP_MESSAGES + ")" + CRLF

    SELF:nCurState := SENDING_REQUEST

    IF !SELF:SendRemote(cBuffer)
        RETURN ""
    ENDIF

    SELF:nCurState := RETREIVING_DATA

    IF SELF:RecvRemote()
        IF SELF:CheckReply()
        ELSE
        ENDIF
        cBuffer := SELF:ReplyString
    ELSE
        cBuffer := ""
    ENDIF

    IF SLen(cBuffer) > 0
        cTemp := SELF:__FetchToken(cBuffer, "(" + TEMP_MESSAGES, ")")
        SELF:nMailCount := Val(cTemp)
    ENDIF

    SELF:nTotalBytes := 0
    IF SELF:nMailCount > 0
        FOR i := 1 TO SELF:nMailCount
            SELF:nTotalBytes += SELF:__GetSize(i)
        NEXT
    ENDIF

    RETURN cBuffer


CONSTRUCTOR            (cServer, cUid, cPwd)               

    SUPER(cServer, cUid, cPwd, IPPORT_IMAP)

    SELF:nPrefix := 1000

    IF SELF:nError = 0
        IF IsString(cUID)
            SELF:cUserName := cUID
        ENDIF

        IF IsString(cPwd)
            SELF:cPassWord := cPwd
        ENDIF

        SELF:TimeOut := 1000

        SELF:oEmail := CEmail{}

    ENDIF

    SELF:cMailBox := "inbox"

    RETURN 


METHOD  LogOn           (cUID, cPwd)    
    LOCAL cBuffer AS STRING

    Default(@cUID, "")
    Default(@cPwd, "")

    SELF:nError := 0

    IF SLen(cUID) > 0
        SELF:cUserName := cUID
    ENDIF

    IF SLen(cPwd) > 0
        SELF:cPassWord := cPwd
    ENDIF

    SELF:nCurState := CONNECTING

    IF !SELF:connect( SELF:cHostAddress )
        RETURN .F. 
    ENDIF

    SELF:nCurState := LOGGING_ON

    cBuffer := "LOGIN " + SELF:cUserName + " " + SELF:cPassWord + CRLF

    IF !SELF:SendRemote(cBuffer)
        RETURN .F. 
    ENDIF

    IF !SELF:RecvRemote()
        RETURN .F. 
    ENDIF

    IF !SELF:CheckReply()
        SELF:nError :=  ERROR_INTERNET_INCORRECT_PASSWORD
        RETURN .F. 
    ENDIF

    IF SLen(SELF:cMailBox) > 0
        SELF:ClientCommand("SELECT " + SELF:cMailBox)
    ENDIF

    RETURN .T. 


ACCESS  MailBox         ()                                  
    RETURN SELF:cMailBox

ASSIGN  MailBox         (xNew)                              
    Default(@xNew, "")
    IF SELF:nCurState > 0
        IF SLen(xNew) > 0
            IF SELF:ClientCommand("SELECT " + xNew)
                SELF:cMailBox := xNew
            ENDIF
        ENDIF
    ELSE
        SELF:cMailBox := xNew
    ENDIF
    RETURN SELF:cMailBox


METHOD  SendRemote      (cData)                         
    LOCAL lRet      AS LOGIC

    IF IsString(cData)
        lRet := SUPER:Sendremote( SELF:GetPrefix() + cData )
    ENDIF

    RETURN lRet


END CLASS




#region defines
DEFINE TEMP_MESSAGES := "MESSAGES"
DEFINE TEMP_RFC_SIZE := "(RFC822.SIZE"
#endregion
