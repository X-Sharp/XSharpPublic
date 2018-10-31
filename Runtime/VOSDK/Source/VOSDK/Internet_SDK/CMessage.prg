PARTIAL CLASS CMessage

   PROTECT cHeader             AS STRING
	PROTECT cBody               AS STRING
	PROTECT cBodyHtml           AS STRING //New 2.7
	PROTECT cAttach             AS STRING

	PROTECT cFromName           AS STRING
	PROTECT cFromAddress        AS STRING

	PROTECT aAttachList         AS ARRAY
	PROTECT aFileList     	    AS ARRAY
	PROTECT aTransferEncoding   AS ARRAY
	PROTECT nTransferEncoding   AS DWORD
	PROTECT aContentType        AS ARRAY
	PROTECT cContentType		    AS STRING

	PROTECT dDate               AS DATE
	PROTECT cTimeStamp          AS STRING
	PROTECT cTime               AS STRING

	PROTECT cSubject            AS STRING
	PROTECT cBoundary			    AS STRING
	//PROTECT cSubBoundary		    AS STRING //New 2.7
	PROTECT cCargo              AS STRING
	PROTECT cReplyTo            AS STRING
	PROTECT nPriority			    AS INT
	PROTECT cMessageID          AS STRING
	PROTECT cReferences         AS STRING

	PROTECT nError			 	    AS DWORD

	PROTECT cCharSet				  AS STRING 	//RvdH 070125
	#ifndef __VULCAN__
   ~"ONLYEARLY+"
   DECLARE METHOD __DecodeContent
   DECLARE ACCESS AttachmentCount
   ~"ONLYEARLY-"
   #endif
METHOD __CheckAttachment(cMail, nPart)
    LOCAL dwPos      AS DWORD
    LOCAL dwNewBound	AS DWORD
    LOCAL dwStop     AS DWORD
    LOCAL cTemp      AS STRING
    LOCAL dwSize     AS DWORD

    Default(@nPart, 1)

	 dwNewBound := At2(TEMP_BOUNDARY, cMail)

    IF (dwPos := At2(TEMP_FNAME, cMail)) = 0
	    dwPos  := At2(TEMP_NAME, cMail)
       dwSize := SLen(TEMP_NAME)
    ELSE
       dwSize := SLen(TEMP_FNAME)
	 ENDIF

    IF dwPos > 0 // TEMP_FNAME or TEMP_NAME found
		IF dwNewBound = 0 .OR. dwPos < dwNewBound  // position is less than next boundery or no boundery exists
		   dwPos += dwSize
	      IF(dwStop := At3(CRLF, cMail, dwPos)) > 0
	         cTemp := SubStr3(cMail, dwPos, dwStop - dwPos)
	         IF cTemp = e"\""
               cTemp := __GetToken(cTemp, e"\"", e"\"")
	         ENDIF
	      ENDIF
	   ENDIF
	ENDIF

	IF SLen(cTemp) == 0
    	// UH 09/06/1999
      //	RETURN .F.
	   cTemp := "part" + NTrim(nPart) + IIF(dwNewBound > 0,".mim","")
   ENDIF

   cTemp := __GoodFileName(cTemp)

   AAdd(SELF:aFileList, cTemp)

   cTemp := Lower(__GetMailInfo(cMail, TEMP_ENCODE, TRUE))

   DO CASE
      CASE cTemp = "base64"
           AAdd(SELF:aTransferEncoding, CODING_TYPE_BASE64)
      CASE cTemp = "uuencode"
           AAdd(SELF:aTransferEncoding, CODING_TYPE_UUENCODE)
      CASE cTemp = "quoted-printable"
           AAdd(SELF:aTransferEncoding, CODING_TYPE_PRINTABLE)
      CASE cTemp = "7bit"
           AAdd(SELF:aTransferEncoding, CODING_TYPE_7BIT)
      OTHERWISE
           AAdd(SELF:aTransferEncoding, CODING_TYPE_UNKNOWN)
   ENDCASE
	cTemp :=__GetMailInfo(cMail, TEMP_CONTENT, TRUE)
   AAdd(SELF:aContentType, cTemp )
   SELF:cCharSet := CharSetFromContentType(cTemp)

   RETURN TRUE



METHOD __DecodeContent(cValue AS STRING) AS STRING STRICT
   LOCAL dwType AS DWORD

   dwType := SELF:nTransferEncoding
   DO CASE
      CASE dwType = CODING_TYPE_BASE64
           cValue := B64DecodeString(cValue)
      CASE dwType = CODING_TYPE_UUENCODE
      CASE dwType = CODING_TYPE_PRINTABLE
           cValue := QPDecode(cValue)
      CASE dwType = CODING_TYPE_7BIT
      CASE dwType = CODING_TYPE_UNKNOWN
   ENDCASE
   IF SELF:cCharSet == CHARSET_UTF8
   	 cValue := ConvertFromCodePageToCodePage(cValue, CP_UTF8, CP_ACP)
   ENDIF
   RETURN cValue

METHOD  __GetMailTime(cDate)
    //SE-040628
    LOCAL dDate AS DATE
    LOCAL cTime AS STRING

    IF DecodeMailTimeStamp(cDate, @dDate, @cTime)
       SELF:dDate      := dDate
       SELF:cTime      := cTime
       SELF:cTimeStamp := cDate
       RETURN TRUE
    ENDIF

    RETURN FALSE



ACCESS AttachmentCount AS DWORD STRICT
   RETURN ALen(SELF:aFileList)

ACCESS AttachmentFileList()

	// For NNTP class
    RETURN SELF:aFileList

ASSIGN AttachmentFileList(xNew)

	// For NNTP class
    IF IsString(xNew)
        SELF:aFileList := __StrList2Array(xNew)
    ELSEIF IsArray(xNew)
        SELF:aFileList := xNew
    ENDIF

    RETURN SELF:aFileList

ACCESS  AttachmentList()
    RETURN SELF:aAttachList

ASSIGN  AttachmentList(xNew)
    IF IsString(xNew)
        SELF:aAttachList := __StrList2Array(xNew)
    ELSEIF IsArray(xNew)
        SELF:aAttachList := xNew
    ENDIF

    RETURN SELF:aAttachList

ACCESS Body
    RETURN SELF:cBody

ASSIGN Body(cNew)
    IF IsString(cNew)
       //SE-071017 no StrTran() needed for CEmail class
       IF IsInstanceOf(SELF, #CEMail) //SE-071017
          SELF:cBody := cNew
       ELSE
          SELF:cBody := StrTran(cNew, DEFAULT_STOPDATA, MY_STOPDATA)
       ENDIF
    ENDIF
    RETURN SELF:cBody

METHOD BodyExtract(c)

    LOCAL cTemp     AS STRING
    LOCAL cRet      AS STRING
    LOCAL nPos      AS DWORD

    Default(@c, SELF:cBody)

    cRet  := c
    cTemp := __GetMailInfo(c, TEMP_ENCODE, .T. )

    IF SLen(cTemp) > 0
        cTemp := Lower(cTemp)
        DO CASE
        CASE cTemp = "base64"
            SELF:nTransferEncoding := CODING_TYPE_BASE64
        CASE cTemp = "uuencode"
            SELF:nTransferEncoding := CODING_TYPE_UUENCODE
        CASE cTemp = "quoted-printable"
            SELF:nTransferEncoding := CODING_TYPE_PRINTABLE
        CASE cTemp = "7bit"
            SELF:nTransferEncoding := CODING_TYPE_7BIT
        OTHERWISE
            SELF:nTransferEncoding := CODING_TYPE_UNKNOWN
        ENDCASE

        cTemp := __GetMailInfo(c, TEMP_CONTENT, .T. )
        SELF:cContentType := cTemp
        IF (nPos := At2(CRLF + CRLF, c)) > 0
            cRet := SubStr2(c, nPos + 4)
        ENDIF
        IF cTemp = "multipart/alternative"
           cTemp := __GetMailInfo(c, TEMP_BOUND, .T. )
           cTemp := SubStr(cTemp, 2, SLen(cTemp) - 2)
           cTemp := __GetToken(cRet, cTemp, cTemp)
           IF ! cTemp == NULL_STRING
              cRet := cTemp
           ENDIF
           cRet  := SELF:BodyExtract(cRet)
        ENDIF
    ENDIF

    RETURN cRet

ACCESS BodyHtml
    RETURN SELF:cBodyHtml

ASSIGN BodyHtml(cNew)

    IF IsString(cNew)
       //SE-071017 no StrTran() needed for CEmail class
       IF IsInstanceOf(SELF, #CEMail)
          SELF:cBodyHtml := cNew
       ELSE
          SELF:cBodyHtml := StrTran(cNew, DEFAULT_STOPDATA, MY_STOPDATA)
       ENDIF
    ENDIF

    RETURN SELF:cBodyHtml

ACCESS Cargo
	RETURN SELF:cCargo

ASSIGN Cargo(uValue)
	RETURN SELF:cCargo := uValue

ACCESS ContentType
	RETURN SELF:cContentType

ASSIGN ContentType(uValue)
	RETURN SELF:cContentType := uValue

METHOD Decode(cMail)
    //SE-040628
	 // this is required for the NEWS class but is overridden for email
    LOCAL lRet      AS LOGIC
    LOCAL lBoundary AS LOGIC
    LOCAL cBound    AS STRING
    LOCAL cTemp     AS STRING
    LOCAL dwPos     AS DWORD
    LOCAL dwStop    AS DWORD

    SELF:aAttachList       := {}
    SELF:aFileList         := {}
    SELF:aTransferEncoding := {}

    //
    //  To do: Decode mail to 3 parts:
    //  - Header
    //  - Body
    //  - Attachement
    //

    IF (dwPos := At2(TEMP_BOUND, cMail)) > 0
        dwPos += SLen(TEMP_BOUND)
        IF (dwStop := At3(TEMP_STOP, cMail, dwPos)) > 0
           cBound := SubStr(cMail, dwPos, dwStop - dwPos)
        ENDIF
    ENDIF

    IF SLen(cBound) = 0
       cBound := DEFAULT_BOUNDARY
    ELSE
       cBound := CRLF + BOUNDARY_START + cBound
       lBoundary := .T.
    ENDIF

    SELF:cBoundary := cBound

    IF (dwPos := At3(cBound, cMail, dwStop + 1)) > 0
        //  UH 06/11/2001
        //  UH 09/12/2000
        //  lBoundary := .T.
        lRet := .T.
        SELF:cHeader := SubStr3(cMail, 1, dwPos - 1) + CRLF
        cTemp := SubStr2(cMail, dwPos + SLen(cBound))
    ELSE
    	//	UH 10/01/1999
      //	cTemp := cMail
		cMail := StrTran(cMail, MY_STOPDATA, DEFAULT_STOPDATA)
		cMail := StrTran(cMail, DEFAULT_STOPDATA, "")
      SELF:cHeader := cMail
      cTemp := ""
    ENDIF

    IF lBoundary
       dwPos := At2(cBound, cTemp)
    ELSE
       dwPos := 0
    ENDIF

    IF dwPos > 0
       SELF:cBody   := SubStr3(cTemp, 1, dwPos - 1)
       SELF:cAttach := cMail := SubStr2(cTemp, dwPos + SLen(cBound) )
    ELSE
       SELF:cBody := cTemp
    ENDIF

    SELF:cBody := StrTran(SELF:cBody, MY_STOPDATA, DEFAULT_STOPDATA)
    SELF:cBody := StrTran(SELF:cBody, DEFAULT_STOPDATA, "")
    SELF:Body := SELF:BodyExtract(SELF:cBody)	// extra translations done

    SELF:GetHeaderInfo()
    SELF:GetAttachInfo(SELF:cAttach)

    cMail := SELF:cBody

    IF (dwPos := At2(TEMP_BOUND, cMail)) > 0
       dwPos  := SLen(TEMP_BOUND)
       IF (dwStop := At3( TEMP_STOP, cMail, dwPos)) > 0
          cBound := SubStr(cMail, dwPos, dwStop - dwPos)
          cTemp  := SELF:cBoundary

			 IF (dwPos := At2(cBound, cMail)) > 0
		    	 dwPos += SLen(cBound)
		    	 SELF:cBody := SubStr3(cMail, 1, dwPos - 1)
		    	 cMail := SubStr2(cMail, dwPos)
	          SELF:cBoundary := cBound
			    SELF:GetAttachInfo(cMail)
	          SELF:cBoundary := cTemp
	       ENDIF
       ENDIF
    ENDIF

	RETURN lRet


METHOD DecodeAndSaveAs(cPath, cFile, cMail)
    RETURN __SaveAs(cFile, cPath, cMail)

ACCESS Error()
    RETURN SELF:nError

ASSIGN Error(n)
    IF IsNumeric(n)
        SELF:nError := n
    ENDIF

    RETURN SELF:nError


ACCESS ErrorMsg
    RETURN SystemErrorString(SELF:nError, "Message Error " + NTrim(SELF:nError))


METHOD FakeAttachmentList()

	LOCAL i,n		AS DWORD
	LOCAL cRet		AS STRING

	n := SELF:AttachmentCount

	cRet := CRLF

	FOR i := 1 TO n
		cRet += "<<" + SELF:aFileList[i] + ">>" + CRLF
	NEXT

	RETURN cRet

ACCESS @@From
   RETURN __FormatAddress(SELF:cFromAddress, SELF:cFromName)

ASSIGN @@From(cValue)
   //SE-040717
   LOCAL cAddress AS STRING
   LOCAL cName    AS STRING

	IF (cAddress := __ParseAddress(cValue, @cName)) == NULL_STRING
	   cName := NULL_STRING
   ENDIF

   SELF:cFromAddress := cAddress
   SELF:cFromName    := cName

   RETURN

ACCESS FromAddress
	RETURN SELF:cFromAddress

ASSIGN FromAddress(uValue)
	RETURN SELF:cFromAddress := uValue

ACCESS FromName
	RETURN SELF:cFromName

ASSIGN FromName(uValue)
	RETURN SELF:cFromName := uValue

METHOD  GetAttachInfo (c, lNewsGroupMessage)

	// For newsgroup use only
    LOCAL nPos      AS DWORD
    LOCAL cTemp     AS STRING
    LOCAL cRest     AS STRING
    LOCAL cBound    AS STRING
    LOCAL n			AS DWORD

    cBound := SELF:cBoundary

    Default(@c, SELF:cAttach)
	 Default(@lNewsGroupMessage, .F. )

    cRest  := c

    nPos := At2(cBound, cRest)
    n    := 1

    DO WHILE nPos > 0
		  //RvdH 070417 CollectForced should not be needed. Let VO handle it.
        //CollectForced()

        cTemp := SubStr3(cRest, 1, nPos-1)
        cRest := SubStr2(cRest, nPos + SLen(cBound))

        IF SELF:__CheckAttachment(cTemp, n)
            AAdd(SELF:aAttachList, cTemp)
        ENDIF

        nPos := At2(ATTACHMENT_END, cRest)
        IF nPos = 1
            #IFDEF __DEBUG__
                DebOut32("End of Attachment found !!!" )
            #ENDIF
            EXIT
        ENDIF

        nPos := At2(cBound, cRest)
        n++
    ENDDO

	//	UH 10/13/1999
	IF lNewsGroupMessage
    	IF ALen(SELF:aAttachList) > 0
			SELF:cBody := SELF:cBody + SELF:FakeAttachmentList() + cRest
		ENDIF
	ENDIF

    RETURN NIL


METHOD GetHeaderInfo()
    //SE-040717
    LOCAL cHeader AS STRING

    cHeader := SELF:MailHeader

    SELF:__GetMailTime(__GetMailInfo(cHeader, TEMP_DATE, FALSE))

    SELF:@@From       := __GetMailInfo(cHeader, TEMP_FROM, FALSE)
    SELF:ReplyTo    := __GetMailInfo(cHeader, TEMP_REPLY, FALSE)
    SELF:Subject    := __GetMailInfo(cHeader, TEMP_SUBJECT, FALSE)
    SELF:MessageID  := __GetMailInfo(cHeader, TEMP_MESSAGEID, FALSE) // , TRUE
    SELF:References := __GetMailInfo(cHeader, TEMP_REFERENCES, FALSE)

    RETURN TRUE


ACCESS HEADER()
    RETURN SELF:cHeader

ASSIGN HEADER(c)
    IF IsString(c)
       SELF:cHeader  := c
    ENDIF
    RETURN SELF:cHeader


CONSTRUCTOR()

    SELF:aAttachList       := {}
    SELF:aFileList         := {}
    SELF:aTransferEncoding := {}
    SELF:aContentType      := {}

    RETURN


ACCESS MailBody()
    RETURN SELF:cBody

ASSIGN MailBody(cNew)
    RETURN SELF:Body := cNew

ACCESS MailDate
    RETURN SELF:dDate


ACCESS MailHeader()
    RETURN SELF:cHeader

ASSIGN MailHeader(cNew)
    RETURN SELF:Header := cNew

ACCESS MailTime
    RETURN SELF:cTime


ACCESS MessageID
	RETURN SELF:cMessageID

ASSIGN MessageID(uValue)
	RETURN SELF:cMessageID := uValue

ACCESS Priority()
    RETURN SELF:nPriority

ASSIGN Priority(nNew)

    IF IsNumeric(nNew)
        SELF:nPriority := nNew
    ENDIF

    RETURN SELF:nPriority

ACCESS References
	RETURN SELF:cReferences

ASSIGN References(uValue)
	RETURN SELF:cReferences := uValue

ACCESS ReplyTo
	RETURN SELF:cReplyTo

ASSIGN ReplyTo(uValue)
	SELF:cReplyTo := uValue
	RETURN

METHOD SaveAs(cPath, cFile, n)

    LOCAL nRet      AS DWORD
    LOCAL nCodeType AS DWORD

    Default(@n, 1)

    IF n < 1
        RETURN .F.
    ELSE
        IF n > SELF:AttachmentCount
            RETURN .F.
        ENDIF
    ENDIF

    // Updated to better handle UNC paths
    cPath := __AdJustPath(cPath)

    Default(@cFile, "")
    IF SLen(cFile) = 0
        cFile := SELF:aFileList[n]
    ENDIF

    nCodeType := SELF:aTransferEncoding[n]

    DO CASE
    CASE nCodeType = CODING_TYPE_UUENCODE
        nRet := __UUDecodeMail(cPath, cFile, SELF:aAttachList[n])
    CASE nCodeType = CODING_TYPE_BASE64
        nRet := __DecodeB64(cPath, cFile, SELF:aAttachList[n])

    CASE nCodeType = CODING_TYPE_PRINTABLE
        nRet := __DecodeQPrintable(cPath, cFile, SELF:aAttachList[n])

    CASE nCodeType = CODING_TYPE_7BIT
        nRet := __SaveAs(cPath, cFile, SELF:aAttachList[n])

    OTHERWISE
        SELF:Error := ERR_UNKNOWN_CODE_TYPE
        nRet := SELF:DecodeAndSaveAs(cPath, cFile, SELF:aAttachList[n])
    ENDCASE

    RETURN (nRet > 0)


METHOD SetMailTime()

    SELF:dDate  := Today()
    SELF:cTime  := Time()
    SELF:cTimeStamp := GetMailTimeStamp(TRUE)	// force the date format to be British

    RETURN .T.


ACCESS Subject
	RETURN SELF:cSubject

ASSIGN Subject(uValue)
	RETURN SELF:cSubject := uValue

ACCESS  TimeStamp
    RETURN SELF:cTimeStamp


ACCESS TransferEncoding
	RETURN SELF:nTransferEncoding

ASSIGN TransferEncoding(uValue)
	RETURN SELF:nTransferEncoding := uValue
END CLASS

