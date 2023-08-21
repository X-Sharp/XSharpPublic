/// <include file="Internet.xml" path="doc/CPop/*" />
CLASS CPop INHERIT CMailAbstract


	// Class to retrieve mail or mail info
	PROTECT nDecodeBytes       AS DWORD
	PROTECT nHeaderLineCount  AS DWORD


	PROTECT oEmail         	  AS CEmail
	PROTECT oStorage          AS CStorage


	PROTECT nMailCount        AS DWORD
	PROTECT nReceiveBytes     AS DWORD
	PROTECT nTotalBytes       AS DWORD


 /// <exclude />
METHOD __GetScanList(cBuffer AS STRING) AS ARRAY
   LOCAL pChar AS BYTE PTR
	LOCAL dwPos AS DWORD
	LOCAL dwEnd AS DWORD
	LOCAL liNum AS LONGINT


	dwPos := 1
	dwEnd := SLen(cBuffer)
	pChar := String2Psz( cBuffer)


	DO WHILE dwPos <= dwEnd .AND. pChar[dwPos] = 0x20
	   dwPos++
	ENDDO


	IF dwPos < dwEnd
	   dwPos++
   	DO WHILE dwPos <= dwEnd .AND. pChar[dwPos] != 0x20
   	   dwPos++
   	ENDDO
	ENDIF


	liNum := Val(SubStr3(cBuffer, 1, dwPos-1))


	cBuffer := SubStr2(cBuffer, dwPos)
	IF (dwPos := At2(CRLF, cBuffer)) > 0
		cBuffer := SubStr3(cBuffer, 1, dwPos-1)
	ENDIF


	RETURN {liNum, AllTrim(cBuffer)}


 /// <exclude />
METHOD __ListMail(cCommand AS STRING, nMessageNum AS USUAL) AS ARRAY
	LOCAL cTemp     AS STRING
	LOCAL aList     AS ARRAY
	LOCAL lSingle   AS LOGIC


	SELF:nError        := 0
	SELF:nReceiveBytes := 0


	cTemp := cCommand
	IF IsNumeric(nMessageNum)
		cTemp   += " " + NTrim(nMessageNum) + CRLF
		lSingle := TRUE
	ELSE
		cTemp   += CRLF
		lSingle := FALSE
	ENDIF


	SELF:nCurState := SENDING_REQUEST


	aList := {}


	IF SELF:SendRemote(cTemp)


		SELF:nCurState := RETREIVING_DATA


		IF ! SELF:RecvRemote()
			RETURN aList
		ENDIF


		IF ! SELF:CheckReply()
			RETURN aList
		ENDIF


		IF lSingle
			RETURN {SELF:__GetScanList(SubStr2(SELF:cReply, 4))}
		ENDIF


		aList := {}


		DO WHILE TRUE
			cTemp := SELF:oSocket:GetRawText(TRUE, TRUE)
			IF ! Empty(cTemp) .AND. cTemp != "."
				AAdd(aList, SELF:__GetScanList(cTemp))
			ELSE
				EXIT
			ENDIF
		ENDDO


	ENDIF


	RETURN aList


/// <include file="Internet.xml" path="doc/CPop.CheckReply/*" />
METHOD CheckReply()
	RETURN SubStr3(SELF:cReply, 1, 3) = "+OK"


/// <include file="Internet.xml" path="doc/CPop.connect/*" />
METHOD connect(cIP, n)
	LOCAL nPort AS WORD


	IF IsNumeric(n)
		nPort := n
	ELSE
		nPort := SELF:RemotePort
	ENDIF


	IF ! SELF:oSocket:connect(cIP, nPort)
		SELF:nError := SELF:oSocket:Error
		RETURN FALSE
	ENDIF


	IF !SELF:RecvRemote()
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Internet.xml" path="doc/CPop.DeleteMail/*" />
METHOD DeleteMail(nMail)
	SELF:nCurState := SENDING_REQUEST


	IF ! SELF:SendRemote("DELE " + NTrim(nMail) + CRLF)
		RETURN FALSE
	ENDIF


	SELF:nCurState := RETREIVING_DATA


	IF ! SELF:RecvRemote()
		RETURN FALSE
	ENDIF


	RETURN SELF:CheckReply()


/// <include file="Internet.xml" path="doc/CPop.Disconnect/*" />
METHOD Disconnect()


	IF SELF:lSocketOpen
		#IFDEF __DEBUG__
			DebOut32("Disconnecting...")
		#ENDIF
		SELF:nError := 0


		SELF:nCurState := SENDING_REQUEST


		IF !SELF:SendRemote("QUIT" + CRLF)
			RETURN FALSE
		ENDIF


		SELF:nCurState := RETREIVING_DATA


		IF ! SELF:RecvRemote()
			RETURN FALSE
		ENDIF
		/*
		IF SELF:CheckReply()
		//  SUPER:Close()
		ENDIF
		*/
	ENDIF


	RETURN TRUE


/// <include file="Internet.xml" path="doc/CPop.Email/*" />
ACCESS Email
	RETURN SELF:oEmail




/// <include file="Internet.xml" path="doc/CPop.GetList/*" />
METHOD GetList(nMail)
   //retrieves a list of emails and their size  {...{nNum, cSize}...}
	//to retrieve the size of one mail, use LIST n instead
   RETURN SELF:__ListMail("LIST", nMail)


/// <include file="Internet.xml" path="doc/CPop.GetListIDs/*" />
METHOD GetListIDs(nMail)
	//retrieves a list of emails and their Unique ID  {...{nNum, cID}...}
	//to retrieve the ID of one mail, use UIDL n instead
   RETURN SELF:__ListMail("UIDL", nMail)


/// <include file="Internet.xml" path="doc/CPop.GetMail/*" />
METHOD GetMail(nMail)
	LOCAL cLine AS STRING


	SELF:nError        := 0
	SELF:nReceiveBytes := 0


	cLine := "RETR " + NTrim(nMail) + CRLF


	SELF:nCurState := SENDING_REQUEST


	IF ! SELF:SendRemote(cLine)
		RETURN FALSE
	ENDIF


	SELF:nCurState := RETREIVING_DATA


	IF ! SELF:RecvRemote()
		RETURN FALSE
	ENDIF


	IF ! SELF:CheckReply()
		RETURN FALSE
	ENDIF


	#IFDEF __DEBUG__
		DebOut32("-----Found message to be OK Socket:GetRawText")
	#ENDIF


	SELF:oEmail := oStorage:CreateNewEMail()


	SELF:oEmail:StreamStart()
	DO WHILE TRUE
		cLine := oSocket:GetRawText(TRUE, TRUE)
		IF oSocket:Error != 0 .OR. cLine == NULL_STRING
			EXIT
		ENDIF
    	SELF:oEmail:StreamIn(cLine)
	ENDDO


	SELF:oEmail:StreamIn(NULL_STRING) //closes the stream


	RETURN TRUE




/// <include file="Internet.xml" path="doc/CPop.GetMailTop/*" />
METHOD GetMailTop(nMail, nBodyLines)
	LOCAL cTemp    AS STRING
	LOCAL aTop 	   AS ARRAY
	LOCAL cHeader  AS STRING
	LOCAL lHeader  AS LOGIC
	LOCAL cBody    AS STRING


	// Retrieve information from the email header for anti-spam and anti-virus purposes
	// return value is an array: {cHeader, cBody}


	IF ! (IsNumeric(nBodyLines) .AND. nBodyLines > 0)
		nBodyLines := 1
	ENDIF


	cTemp := "TOP " + NTrim(nMail) + " " + NTrim(nBodyLines) + CRLF	// get one line


	SELF:nCurState := SENDING_REQUEST


	aTop := {NULL_STRING, NULL_STRING}


	IF SELF:SendRemote(cTemp)


		SELF:nCurState := RETREIVING_DATA


		IF ! SELF:RecvRemote()
			RETURN aTop
		ENDIF


		IF ! SELF:CheckReply()
			RETURN aTop
		ENDIF


		cHeader := NULL_STRING
		lHeader := TRUE
		cBody   := NULL_STRING


		DO WHILE TRUE
			cTemp := oSocket:GetRawText(TRUE, TRUE)
			IF oSocket:Error != 0 .OR. cTemp == NULL_STRING
				EXIT
			ENDIF
			IF lHeader
				IF cTemp == CRLF
					lHeader := FALSE
				ELSE
					cHeader += cTemp
				ENDIF
			ELSE
				cBody += cTemp
			ENDIF
		ENDDO


		aTop := {cHeader, cBody}
	ELSE
		aTop := {NULL_STRING, NULL_STRING}
	ENDIF


	RETURN aTop


/// <include file="Internet.xml" path="doc/CPop.GetStatus/*" />
METHOD GetStatus()
	LOCAL cBuffer   AS STRING
	LOCAL aScanList AS ARRAY


	SELF:nError := 0


	cBuffer := "STAT" + CRLF


	SELF:nCurState := SENDING_REQUEST


	IF !SELF:SendRemote(cBuffer)
		RETURN ""
	ENDIF


	SELF:nCurState := RETREIVING_DATA


	IF !SELF:RecvRemote()
		RETURN ""
	ENDIF


	IF !SELF:CheckReply()
		SELF:nError := ERR_LOGON_FAILED
		RETURN ""
	ENDIF


	cBuffer   := SubStr2(SELF:cReply, 4)
	aScanList := SELF:__GetScanList(cBuffer)


	SELF:nMailCount  := aScanList[1]
	SELF:nTotalBytes := Val(aScanList[2])


	RETURN cBuffer


/// <include file="Internet.xml" path="doc/CPop.ctor/*" />
CONSTRUCTOR(cServer, cUserName, cPassword, nPort, oEMailStorage)


	DEFAULT(@nPort, IPPORT_POP)


	SUPER(nPort, cServer)


	IF SELF:nError = 0


		IF IsString(cUserName)
			SELF:UserName := cUserName
		ENDIF
		IF IsString(cPassword)
			SELF:PassWord := cPassword
		ENDIF
		SELF:TimeOut := 1000


		SELF:Storage := oEMailStorage
		IF oStorage = NULL_OBJECT
			oStorage := CStorage{}
		ENDIF


	ENDIF


	RETURN


/// <include file="Internet.xml" path="doc/CPop.ListMail/*" />
METHOD ListMail()
    LOCAL aRet    AS ARRAY
    //LOCAL cTemp   AS STRING   not used
    LOCAL dwI     AS DWORD
    LOCAL dwCount AS DWORD


    //cTemp   := SELF:GetStatus()


    dwCount := SELF:nMailCount


    aRet    := ArrayCreate(dwCount)


    FOR dwI := 1 UPTO dwCount
        IF SELF:GetMail(dwI)
            aRet[dwI] := SELF:oEmail
        ENDIF
    NEXT  // dwI


    RETURN aRet


/// <include file="Internet.xml" path="doc/CPop.LogOn/*" />
METHOD LogOn(cUID, cPwd)
	DEFAULT(@cUID, "")
	DEFAULT(@cPwd, "")


	SELF:nError := 0


	IF SLen(cUID) > 0
		SELF:UserName := cUID
	ENDIF


	IF SLen(cPwd) > 0
		SELF:PassWord := cPwd
	ENDIF


	SELF:nCurState := CONNECTING


	IF !SELF:connect(SELF:cHostAddress)
		RETURN FALSE
	ENDIF


	SELF:nCurState := LOGGING_ON


	IF !SELF:SendRemote("USER " + SELF:UserName + CRLF)
		RETURN FALSE
	ENDIF


	IF !SELF:RecvRemote()
		RETURN FALSE
	ENDIF


	IF ! SELF:CheckReply()
		SELF:nError := ERR_LOGON_FAILED
		RETURN FALSE
	ENDIF


	IF ! SELF:SendRemote("PASS " + SELF:PassWord + CRLF)
		RETURN FALSE
	ENDIF


	IF ! SELF:RecvRemote()
		RETURN FALSE
	ENDIF


	IF !SELF:CheckReply()
		SELF:nError :=  ERROR_INTERNET_INCORRECT_PASSWORD
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Internet.xml" path="doc/CPop.MailCount/*" />
ACCESS MailCount
	RETURN SELF:nMailCount


/// <include file="Internet.xml" path="doc/CPop.ReceiveBytes/*" />
ACCESS  ReceiveBytes
    RETURN SELF:nReceiveBytes


/// <include file="Internet.xml" path="doc/CPop.RecvRemote/*" />
METHOD RecvRemote()
	LOCAL dwSize AS DWORD
	LOCAL cLine	 AS STRING


	cLine  := SELF:oSocket:GetRawText(TRUE, FALSE) //SELF:oSocket:GetLine()
	dwSize := SLen(cLine)


	SELF:nReceiveBytes := dwSize
	SELF:cReply        := cLine


	RETURN dwSize > 0


/// <include file="Internet.xml" path="doc/CPop.Storage/*" />
ASSIGN Storage(oEMailStorage)
	IF IsInstanceOfUsual(oEMailStorage, #CStorage)
		oStorage := oEMailStorage
	ENDIF
	RETURN


/// <include file="Internet.xml" path="doc/CPop.TotalBytes/*" />
ACCESS TotalBytes
	RETURN SELF:nTotalBytes
END CLASS


