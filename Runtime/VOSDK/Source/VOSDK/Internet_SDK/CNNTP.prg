STATIC FUNCTION __GetDataTime(dStart AS DATE, cTime AS STRING, lGmt AS LOGIC)   AS STRING STRICT
    LOCAL cRet      AS STRING
    LOCAL cTemp     AS STRING

    cTemp := DToS(dStart)
    cRet  := SubStr2(cTemp, 3)

    cTemp := StrTran(cTime, ":", "")
    cRet += " " + cTemp

    IF lGmt
        cRet += " " + "[GMT]"
    ENDIF

    RETURN cRet


PARTIAL CLASS CNNTP INHERIT CMailAbstract
    PROTECT oNews           AS CNews
    PROTECT nReceiveBytes   AS DWORD
    PROTECT nMsgCount       AS DWORD
    PROTECT nMsgFirst       AS DWORD
    PROTECT nMsgLast        AS DWORD
    PROTECT nMsg            AS DWORD
    PROTECT cMsgID          AS STRING
    PROTECT cGroupName      AS STRING


METHOD  __DecodeStatus  (n)
    LOCAL cTemp     AS STRING
    LOCAL cBuffer   AS STRING
    LOCAL i         AS DWORD
    LOCAL nPos      AS DWORD

    cBuffer := SELF:cReply
    Default(@n, SELF:nReply)

    DO CASE
    CASE n == 211
        FOR i := 1 TO 5
            nPos := At2(" ", cBuffer)
            IF nPos > 0
                cTemp    := SubStr3(cBuffer, 1, nPos-1)
                cBuffer  := AllTrim(SubStr2(cBuffer, nPos+1))
                DO CASE
                CASE i == 2
                    SELF:nMsgCount := Val(cTemp)
                CASE i == 3
                    SELF:nMsgFirst := Val(cTemp)

                CASE i == 4
                    SELF:nMsgLast  := Val(cTemp)

                CASE i == 5
                    SELF:cGroupName:= cTemp
                ENDCASE
            ENDIF
        NEXT

    CASE n == 223
        FOR i := 1 TO 3
            nPos := At2(" ", cBuffer)
            IF nPos > 0
                cTemp    := SubStr3(cBuffer, 1, nPos-1)
                cBuffer  := AllTrim(SubStr2(cBuffer, nPos+1))
                DO CASE
                CASE i == 2
                    SELF:nMsg := Val(cTemp)
                CASE i == 3
                    SELF:cMsgID := cTemp
                ENDCASE
            ENDIF
        NEXT

    ENDCASE


    RETURN NIL


METHOD  __GetGroups     (cCommand, cSearch)
    LOCAL aRet      AS ARRAY
    LOCAL cTemp     AS STRING
    LOCAL cBuffer   AS STRING
    LOCAL i,j, n    AS DWORD
    LOCAL aTemp     AS ARRAY
    LOCAL nPos      AS DWORD

    aRet := {}

    SELF:nError := 0

    Default(@cCommand, "LIST" + CRLF)

    SELF:nCurState := SENDING_REQUEST

    IF SELF:SendRemote(cCommand)
        SELF:nCurState := RETREIVING_DATA

        Default(@cSearch, "")
        //  UH 05/12/2000
        //  aRet := SELF:oSocket:GetLines(cSearch)
        IF IsArray(cSearch)
            aRet := SELF:oSocket:GetLines(cSearch)
        ELSE
            IF SLen(cSearch) == 0
                aRet := SELF:oSocket:GetLines({})
            ELSE
                aRet := SELF:oSocket:GetLines({cSearch})
            ENDIF
        ENDIF

        n := ALen(aRet)

        FOR i := 1 TO n
            aTemp := ArrayCreate(NEWSLIST_MAX)
            cBuffer := aRet[i]
            FOR j := 1 TO 3
                nPos := At2(" ", cBuffer)
                IF nPos > 0
                    cTemp    := SubStr3(cBuffer, 1, nPos-1)
                    cBuffer  := AllTrim(SubStr2(cBuffer, nPos+1))
                    IF j == 1
                        aTemp[j] := cTemp
                    ELSE
                        aTemp[j] := Val(cTemp)
                    ENDIF
                ENDIF
            NEXT

            IF AtC("y", cBuffer) > 0
                aTemp[4] := .T.
            ELSE
                aTemp[4] := .F.
            ENDIF

            aRet[i] := aTemp
        NEXT

        IF ALen(aRet) > 0
            IF aRet[1, NEWSLIST_NAME] == "215" .OR. ;
               aRet[1, NEWSLIST_NAME] == "231"
                ADel(aRet, 1)
                ASize(aRet, n-1)
            ENDIF
        ENDIF
    ENDIF

    RETURN aRet


METHOD  __GetNews       (cCommand)
    LOCAL aRet      AS ARRAY
    LOCAL n         AS DWORD

    aRet := {}
    SELF:nError := 0
    SELF:nCurState := SENDING_REQUEST

    IF SELF:SendRemote(cCommand)
        SELF:nCurState := RETREIVING_DATA

        aRet := SELF:oSocket:GetLines({})

        n := ALen(aRet)

        IF ALen(aRet) > 0
            ADel(aRet, 1)
            ASize(aRet, n-1)
        ENDIF
    ENDIF

    RETURN aRet


METHOD  Authenticate        ()
    LOCAL cBuffer   AS STRING

    IF SELF:lSocketOpen
        SELF:nError := 0

        //
        //  UH: - Code moved to Method SetReadMode()
        //      - Problems with Exchange News Server
        //        (regarding to Dieter's mail from 06/07/2000)
        //
        /*cBuffer := "MODE READER" + CRLF
        SELF:nCurState := SENDING_REQUEST
        IF !SELF:SendRemote(cBuffer)
            RETURN .F.
        ENDIF
        SELF:nCurState := RETREIVING_DATA
        IF !SELF:RecvRemote()
            RETURN .F.
        ENDIF
        IF !SELF:CheckReply()
            RETURN .F.
        ENDIF
        IF SELF:nReply != 480
            RETURN .T.
        ENDIF
        */

        // USER
        cBuffer := "AUTHINFO USER " + SELF:cUserName + CRLF
        SELF:nCurState := SENDING_REQUEST
        IF !SELF:SendRemote(cBuffer)
            RETURN .F.
        ENDIF
        SELF:nCurState := RETREIVING_DATA
        IF !SELF:RecvRemote()
            RETURN .F.
        ENDIF
        IF !SELF:CheckReply()
            RETURN .F.
        ENDIF
        IF SELF:nReply != 381
            RETURN .T.
        ENDIF

        // PASSWORD
        cBuffer := "AUTHINFO PASS " + SELF:cPassWord + CRLF
        SELF:nCurState := SENDING_REQUEST
        IF !SELF:SendRemote(cBuffer)
            RETURN .F.
        ENDIF
        SELF:nCurState := RETREIVING_DATA
        IF !SELF:RecvRemote()
            RETURN .F.
        ENDIF
        IF !SELF:CheckReply()
            RETURN .F.
        ENDIF
        IF SELF:nReply == 502
            RETURN .F.
        ELSE
            RETURN .T.
        ENDIF
    ENDIF

    RETURN .F.


METHOD  CheckReply      ()

    LOCAL c     AS STRING
    LOCAL lRet  AS LOGIC

    c := SubStr(SELF:cReply, 1, 3)

    SELF:nReply := Val(c)

    IF SELF:nReply > 0
        lRet := .T.
    ELSE
        lRet := .F.
    ENDIF

    RETURN lRet



METHOD  connect         (cIP, n)
    LOCAL nPort     AS WORD
    LOCAL lRet      AS LOGIC

    IF IsNumeric(n)
        nPort := n
    ELSE
        nPort := SELF:wHostPort
    ENDIF

    IF !IsString(cIP)
        cIP := SELF:cHostAddress
    ENDIF

    IF SELF:oSocket:connect(cIP, nPort)
        IF SELF:RecvRemote()
            lRet := SELF:Authenticate()
        ENDIF
    ELSE
        SELF:nError := SELF:oSocket:Error
    ENDIF

    RETURN lRet



ACCESS  CurrentNews
    RETURN SELF:oNews

ASSIGN  CurrentNews     (x)
    IF IsInstanceOfUsual(x, #CNews)
        SELF:oNews := x
    ENDIF

    RETURN SELF:oNews

METHOD  Disconnect      ()
    LOCAL cBuffer   AS STRING
    LOCAL cTemp     AS STRING
    LOCAL nTemp     AS DWORD

    nTemp := SELF:nError
    cTemp := SELF:cReply

    IF SELF:lSocketOpen
        SELF:nError := 0

        cBuffer := "QUIT" + CRLF

        SELF:nCurState := SENDING_REQUEST

        IF !SELF:SendRemote(cBuffer)
            RETURN .F.
        ENDIF

        SELF:nCurState := RETREIVING_DATA

        IF !SELF:RecvRemote()
            RETURN .F.
        ENDIF

        IF SELF:CheckReply()
            //  SUPER:Close()
            SELF:oSocket:Disconnect()
        ENDIF
        SELF:nError := nTemp
        SELF:cReply := cTemp
    ENDIF


    RETURN .T.



METHOD  GetArticle  (xMsg)
    LOCAL cBuffer   AS STRING
    LOCAL lRet      AS LOGIC

    SELF:nError := 0
    Default(@xMsg, SELF:nMsg)

    cBuffer := SELF:PrepareCommand("ARTICLE ", xMsg)

    IF SLen(cBuffer) == 0
        SELF:nError := ERR_NO_ARTICLE_NUMBER
        RETURN .F.
    ENDIF

    lRet := SELF:RecvData(cBuffer, 220)

    IF lRet
        //  UH 10/15/1999
        SELF:oNews := CNews{SELF:cReply}

        IF IsNumeric(xMsg)
            SELF:__DecodeStatus(223)
        ELSEIF IsString(xMsg)
            cBuffer := SubStr2(cBuffer, 9)
            SELF:cMsgID := StrTran(cBuffer, CRLF, "")
        ENDIF
    ENDIF

    RETURN lRet


METHOD  GetBody     (xMsg)
    LOCAL cBuffer   AS STRING
    LOCAL lRet      AS LOGIC

    Default(@xMsg, SELF:nMsg)

    SELF:nError := 0

    cBuffer := SELF:PrepareCommand("BODY ", xMsg)

    IF SLen(cBuffer) == 0
        SELF:nError := ERR_NO_ARTICLE_NUMBER
        RETURN .F.
    ENDIF

    lRet := SELF:RecvData(cBuffer, 222)

    IF lRet .AND. IsNumeric(xMsg)
        SELF:__DecodeStatus(223)
    ENDIF

    RETURN lRet


METHOD  GetHeader   (xMsg)
    LOCAL cBuffer   AS STRING
    LOCAL lRet      AS LOGIC

    SELF:nError := 0
    Default(@xMsg, SELF:nMsg)

    cBuffer := SELF:PrepareCommand("HEAD ", xMsg)

    IF SLen(cBuffer) == 0
        SELF:nError := ERR_NO_ARTICLE_NUMBER
        RETURN .F.
    ENDIF

    lRet := SELF:RecvData(cBuffer, 221)

    IF lRet .AND. IsNumeric(xMsg)
        //  UH 10/15/1999
        SELF:oNews := CNews{SELF:cReply}

        SELF:__DecodeStatus(223)
    ENDIF

    RETURN lRet


METHOD  GetList         (cSearch)
    LOCAL cCommand  AS STRING

    cCommand := "LIST" + CRLF

    RETURN SELF:__GetGroups(cCommand, cSearch)


METHOD  GetNewGroups    (cSearch, dDate, cTime, lGmt)
    LOCAL cCommand  AS STRING

    Default(@dDate, Today())
    Default(@cTime, "00:00:00")
    Default(@cSearch, "")
    Default(@lGmt, .F. )

    cCommand := "NEWGROUPS "
    cCommand += __GetDataTime(dDate, cTime, lGmt)

    cCommand += CRLF

    RETURN SELF:__GetGroups (cCommand, cSearch)


METHOD  GetNewNews      (cGroups, dDate, cTime, lGmt)
    LOCAL cCommand  AS STRING

    Default(@dDate, Today())
    Default(@cTime, "00:00:00")
    Default(@lGmt, .F. )

    cCommand := "NEWNEWS "
    cCommand += cGroups + " "
    cCommand += __GetDataTime(dDate, cTime, lGmt)

    cCommand += CRLF

    RETURN SELF:__GetNews(cCommand)


METHOD  GetStatus   (xMsg)
    LOCAL cBuffer   AS STRING
    LOCAL lRet      AS LOGIC

    SELF:nError := 0
    Default(@xMsg, SELF:nMsg)

    cBuffer := SELF:PrepareCommand("STAT ", xMsg)

    IF SLen(cBuffer) == 0
        SELF:nError := ERR_NO_ARTICLE_NUMBER
        RETURN .F.
    ENDIF

    IF SELF:SendRemote(cBuffer)
        SELF:nCurState := RETREIVING_DATA
        SELF:RecvRemote()

        IF SELF:CheckReply()
            IF SELF:nReply == 223
                lRet := .T.
            ELSE
                SELF:nError := DWORD(INTERNET_ERROR_BASE + SELF:nReply)
            ENDIF
        ENDIF
    ENDIF

    IF lRet .AND. IsNumeric(xMsg)
        SELF:__DecodeStatus(223)
    ENDIF

    RETURN lRet


ACCESS GroupName        ()
    RETURN SELF:cGroupName


CONSTRUCTOR            (cServer, cUid, cPwd)

    SUPER(IPPORT_NNTP, cServer)

    IF SELF:nError = 0
        SELF:oNews := CNews{}

        IF IsString(cUID)
            SELF:cUserName := cUID
        ENDIF

        IF IsString(cPwd)
            SELF:cPassWord := cPwd
        ENDIF

        SELF:TimeOut := 1000
    ENDIF

    SELF:TimeOut := 1000

    RETURN


ACCESS Message          ()
    RETURN SELF:nMsg

ACCESS MessageCount     ()
    RETURN SELF:nMsgCount

ACCESS MessageFirst     ()
    RETURN SELF:nMsgFirst

ACCESS MessageID        ()
    RETURN SELF:cMsgID

ACCESS MessageLast      ()
    RETURN SELF:nMsgLast

METHOD  Post        (oMsg)
    LOCAL cBuffer   AS STRING
    LOCAL lRet      AS LOGIC

    SELF:nError := 0
    Default(@oMsg, SELF:oNews)

    lRet := IsObject(oMsg)

    IF lRet
        cBuffer := "POST" + CRLF
        lRet := SELF:SendRemote(cBuffer)
        IF lRet
            SELF:RecvRemote()
            IF SELF:Checkreply()
                IF SELF:nReply == 340
                    lRet := .T.
                ELSE
                    lRet := .F.
                    SELF:nError := DWORD(INTERNET_ERROR_BASE + SELF:nReply)
                ENDIF
            ENDIF
        ENDIF

        IF lRet
            lRet := .F.
            SELF:oNews := oMsg

            SELF:nCurState := SENDING_DATA
            lRet := SELF:SendHeader()
            IF lRet
                lRet := SELF:SendMailBody()
            ENDIF

            IF lRet
                SELF:nCurState := RETREIVING_DATA
                SELF:RecvRemote()
                IF SELF:CheckReply()
                    IF SELF:nReply == 240
                        lRet := .T.
                    ELSE
                        SELF:nError :=DWORD(INTERNET_ERROR_BASE + SELF:nReply)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    RETURN lRet


METHOD  PrepareCommand(cCommand, xMsg)
    LOCAL cRet  AS STRING

    cRet := cCommand

    IF IsString(xMsg)
        IF SubStr(xMsg, 1, 1) == "<"
            cRet += xMsg + CRLF
        ELSE
            cRet += "<" + xMsg + ">" + CRLF
        ENDIF

    ELSEIF IsNumeric(xMsg)
        cRet += NTrim(xMsg) + CRLF

    ELSE
        cRet := ""
    ENDIF

    RETURN cRet


METHOD  RecvData(cCommand, nRetCode)
    LOCAL lRet      AS LOGIC
    LOCAL cTemp     AS STRING
    LOCAL nPos      AS DWORD

    SELF:nCurState := SENDING_REQUEST

    IF SELF:SendRemote(cCommand)
        SELF:nCurState := RETREIVING_DATA
        cTemp := SELF:oSocket:GetRaw()
        SELF:cReply        := cTemp
        SELF:nReceiveBytes := SLen(cTemp)

        lRet := SELF:CheckReply()

        IF lRet
            IF SELF:nReply != nRetCode
               SELF:nError := DWORD(INTERNET_ERROR_BASE + SELF:nReply)
               lRet := .F.
            ENDIF
        ENDIF

        nPos := At2(CRLF, cTemp)
        IF nPos > 0
            cTemp := SubStr(cTemp, 1, nPos - 1)
        ENDIF
        cTemp := "NNTP Reply: " + cTemp
        SELF:InternetStatus(0, INTERNET_STATUS_RESPONSE_RECEIVED, cTemp, SLen(cTemp))
    ENDIF

    RETURN lRet


METHOD  RecvRemote()
    LOCAL lRet      AS LOGIC
    LOCAL nSize     AS DWORD
    LOCAL cTemp     AS STRING

    cTemp       := SELF:oSocket:GetLine()
    SELF:cReply := cTemp
    nSize       := SLen(cTemp)

    SELF:nReceiveBytes := nSize

    IF nSize = 0
       lRet := FALSE
    ELSE
       lRet := TRUE
    ENDIF

    RETURN lRet


ACCESS  ReplyString     ()
    RETURN SELF:cReply


METHOD  SendHeader  ()
    LOCAL cRet      AS STRING

    IF SLen(SELF:oNews:MessageID) = 0
        SELF:oNews:MessageID := GetUseNetMsgID("", GetHostByIP(SELF:RemoteHost))
    ENDIF

    SELF:oNews:SetMailTime()

    IF SLen(SELF:oNews:Path) > 0
        cRet := "Path: " + SELF:oNews:Path + CRLF
    ENDIF

    cRet += "From: "
    IF SLen(SELF:oNews:FromName) > 0
        cRet += e"\"" + SELF:oNews:FromName + e"\" "
    ENDIF
    cRet += "<" + SELF:oNews:FromAddress + ">"
    cRet += CRLF

    cRet += "Subject: " + SELF:oNews:Subject + CRLF

    cRet += "Message-ID: " + SELF:oNews:MessageID + CRLF

    cRet += "Date: " + SELF:oNews:Timestamp + CRLF
    cRet += "Newsgroups: " + SELF:oNews:NewsGroups + CRLF
    IF SLen(SELF:oNews:ReplyTo) > 0
        cRet += "Reply-To: "
        cRet += "<" + SELF:oNews:ReplyTo + ">" + CRLF
    ENDIF

    IF SLen(SELF:oNews:Sender) > 0
        cRet += "Sender: " + SELF:oNews:Sender + CRLF
    ENDIF

    IF SLen(SELF:oNews:FollowUpTo) > 0
        cRet += "Followup-To: " + SELF:oNews:FollowUpTo + CRLF
    ENDIF

    IF SLen(SELF:oNews:References) > 0
        cRet += "References: " + SELF:oNews:References + CRLF
    ENDIF

    IF SLen(SELF:oNews:Organization) > 0
        cRet += "Organization: " + SELF:oNews:Organization + CRLF
    ENDIF

    cRet += CRLF + CRLF
#IFDEF __DEBUG__
    MemoWrit("nntp1.txt", cRet)
#ENDIF
    RETURN SELF:SendRaw(cRet)



METHOD SendMailBody()
    LOCAL cRet      AS STRING
    LOCAL aFiles    AS ARRAY
    LOCAL cTemp     AS STRING
    LOCAL i,n       AS DWORD

    aFiles := SELF:oNews:AttachmentFileList
    n      := ALen(aFiles)
    cRet   := SELF:oNews:MailBody

    cRet   += CRLF

    IF n > 0
        FOR i := 1 TO n
            cTemp := UUEncFile(aFiles[i])
            IF SLen(cTemp) > 0
                cRet += CRLF + CRLF + cTemp
            ENDIF
        NEXT
    ENDIF

    cRet += DEFAULT_STOPDATA

#IFDEF __DEBUG__
    MemoWrit("nntp2.txt", cRet)
#ENDIF
    RETURN SELF:SendRaw(cRet)


METHOD  SetNewsGroup    (cGroup)
    LOCAL lRet      AS LOGIC
    LOCAL cBuffer   AS STRING

    SELF:nError := 0

    Default(@cGroup, "")

    IF SLen(cGroup) > 0
        cBuffer := "GROUP " + cGroup + CRLF

        SELF:nCurState := SENDING_REQUEST

        IF SELF:SendRemote(cBuffer)
            SELF:nCurState := RETREIVING_DATA

            IF SELF:RecvRemote()
                IF SELF:Checkreply()
                    IF SELF:nReply == 211
                        SELF:__DecodeStatus(211)
                        SELF:nMsg := SELF:nMsgFirst
                        lRet := .T.
                    ELSE
                        SELF:nError :=  ERR_NEWSGROUP_MISSING
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    RETURN lRet


METHOD  SetReadMode         ()
    //
    //  UH: Necessary for some News servers to
    //
    LOCAL cBuffer   AS STRING

    cBuffer := "MODE READER" + CRLF
    SELF:nCurState := SENDING_REQUEST
    IF !SELF:SendRemote(cBuffer)
        RETURN .F.
    ENDIF
    SELF:nCurState := RETREIVING_DATA
    IF !SELF:RecvRemote()
        RETURN .F.
    ENDIF
    IF !SELF:CheckReply()
        RETURN .F.
    ENDIF
    IF SELF:nReply != 480
        RETURN .T.
    ENDIF

    RETURN .F.



METHOD  SkipNext    ()
    LOCAL lRet      AS LOGIC
    LOCAL cBuffer   AS STRING

    SELF:nError := 0

    cBuffer := "NEXT" + CRLF

    SELF:nCurState := SENDING_REQUEST

    IF SELF:SendRemote(cBuffer)
        SELF:nCurState := RETREIVING_DATA

        IF SELF:RecvRemote()
            IF SELF:Checkreply()
                IF SELF:nReply == 223
                    SELF:__DecodeStatus(223)
                    lRet := .T.
                ELSE
                    SELF:nError := DWORD(INTERNET_ERROR_BASE + SELF:nReply)
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    RETURN lRet


METHOD  SkipPrev    ()
    LOCAL lRet      AS LOGIC
    LOCAL cBuffer   AS STRING

    SELF:nError := 0

    cBuffer := "LAST" + CRLF

    SELF:nCurState := SENDING_REQUEST

    IF SELF:SendRemote(cBuffer)
        SELF:nCurState := RETREIVING_DATA

        IF SELF:RecvRemote()
            IF SELF:Checkreply()
                IF SELF:nReply == 223
                    SELF:__DecodeStatus()
                    lRet := .T.
                ELSE
                    SELF:nError := DWORD(INTERNET_ERROR_BASE + SELF:nReply)
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    RETURN lRet



END CLASS


FUNCTION NNTPSendMail       (cServerIP      AS STRING,;
                         cNewsGroups    AS STRING,;
                         cMailSubject   AS STRING,;
                         cBody          AS STRING,;
                         cFromAddress   AS STRING,;
                         xAttachFile    AS USUAL,;
                         cFromName      AS STRING)   AS LOGIC STRICT

    LOCAL oNNTP 	AS CNNTP
    LOCAL lRet  	AS LOGIC
    LOCAL oNews	    AS CNews

    oNNTP := CNNTP{}

    oNNTP:RemoteHost := cServerIP
    oNNTP:TimeOut    := 5000


	oNews := CNews{}
    oNews:FromAddress   := cFromAddress
    oNews:NewsGroups    := cNewsGroups
    oNews:FromName      := cFromName
    oNews:Subject       := cMailSubject
	oNews:MailBody	    := cBody
	oNews:Path          := GetHostByIP(oNNTP:RemoteHost)

    IF !IsNil(xAttachFile)
        oNews:AttachmentFileList := xAttachFile
    ENDIF

    lRet := oNNTP:connect(cServerIP)

    IF lRet
        lRet := oNNTP:Post(oNews)

    	#IFDEF __DEBUG__
    		IF !lRet
    			DebOut32(NTrim(oNNTP:Error) + " " + oNNTP:ErrorMsg)
    		ENDIF
    	#ENDIF
        oNNTP:Disconnect()
    ENDIF

    RETURN lRet



#region defines
DEFINE NEWSLIST_FIRST := 3
DEFINE NEWSLIST_LAST  := 2
DEFINE NEWSLIST_MAX   := 4
DEFINE NEWSLIST_NAME  := 1
DEFINE NEWSLIST_POST  := 4
#endregion
