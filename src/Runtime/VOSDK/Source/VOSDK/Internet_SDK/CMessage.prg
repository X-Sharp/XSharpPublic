/// <include file="Internet.xml" path="doc/CMessage/*" />
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
	PROTECT cCargo              AS STRING
	PROTECT cReplyTo            AS STRING
	PROTECT nPriority			    AS INT
	PROTECT cMessageID          AS STRING
	PROTECT cReferences         AS STRING
	PROTECT nError			 	    AS DWORD
	PROTECT cCharSet				  AS STRING


 /// <exclude />
METHOD __CheckAttachment(cMail, nPart)
    LOCAL dwPos      AS DWORD
    LOCAL dwNewBound	AS DWORD
    LOCAL dwStop     AS DWORD
    LOCAL cTemp      AS STRING
    LOCAL dwSize     AS DWORD


    DEFAULT(@nPart, 1)


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






 /// <exclude />
METHOD __DecodeContent(cValue AS STRING) AS STRING STRICT
   LOCAL dwType AS DWORD


   dwType := SELF:nTransferEncoding
   DO CASE
      CASE dwType = CODING_TYPE_BASE64
           cValue := B64DecodeString(cValue)
      CASE dwType = CODING_TYPE_UUENCODE
            NOP
      CASE dwType = CODING_TYPE_PRINTABLE
           cValue := QPDecode(cValue)
      CASE dwType = CODING_TYPE_7BIT
            NOP
      CASE dwType = CODING_TYPE_UNKNOWN
            NOP

   ENDCASE
   IF SELF:cCharSet == CHARSET_UTF8
   	 cValue := ConvertFromCodePageToCodePage(cValue, CP_UTF8, CP_ACP)
   ENDIF
   RETURN cValue


 /// <exclude />
METHOD  __GetMailTime(cDate)
    LOCAL dDate AS DATE
    LOCAL cTime AS STRING


    IF DecodeMailTimeStamp(cDate, @dDate, @cTime)
       SELF:dDate      := dDate
       SELF:cTime      := cTime
       SELF:cTimeStamp := cDate
       RETURN TRUE
    ENDIF


    RETURN FALSE






/// <include file="Internet.xml" path="doc/CMessage.AttachmentCount/*" />
ACCESS AttachmentCount AS DWORD STRICT
   RETURN ALen(SELF:aFileList)


/// <include file="Internet.xml" path="doc/CMessage.AttachmentFileList/*" />
ACCESS AttachmentFileList()


	// For NNTP class
    RETURN SELF:aFileList


/// <include file="Internet.xml" path="doc/CMessage.AttachmentFileList/*" />
ASSIGN AttachmentFileList(xNew)


	// For NNTP class
    IF IsString(xNew)
        SELF:aFileList := __StrList2Array(xNew)
    ELSEIF IsArray(xNew)
        SELF:aFileList := xNew
    ENDIF


    RETURN SELF:aFileList


/// <include file="Internet.xml" path="doc/CMessage.AttachmentList/*" />
ACCESS  AttachmentList()
    RETURN SELF:aAttachList


/// <include file="Internet.xml" path="doc/CMessage.AttachmentList/*" />
ASSIGN  AttachmentList(xNew)
    IF IsString(xNew)
        SELF:aAttachList := __StrList2Array(xNew)
    ELSEIF IsArray(xNew)
        SELF:aAttachList := xNew
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CMessage.Body/*" />
ACCESS Body
    RETURN SELF:cBody


/// <include file="Internet.xml" path="doc/CMessage.Body/*" />
ASSIGN Body(cNew)
    IF IsString(cNew)
       // no StrTran() needed for CEmail class
       IF SELF IS CEMail
          SELF:cBody := cNew
       ELSE
          SELF:cBody := StrTran(cNew, DEFAULT_STOPDATA, MY_STOPDATA)
       ENDIF
    ENDIF
    RETURN


/// <include file="Internet.xml" path="doc/CMessage.BodyExtract/*" />
METHOD BodyExtract(c)


    LOCAL cTemp     AS STRING
    LOCAL cRet      AS STRING
    LOCAL nPos      AS DWORD


    DEFAULT(@c, SELF:cBody)


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


/// <include file="Internet.xml" path="doc/CMessage.BodyHtml/*" />
ACCESS BodyHtml
    RETURN SELF:cBodyHtml


/// <include file="Internet.xml" path="doc/CMessage.BodyHtml/*" />
ASSIGN BodyHtml(cNew)


    IF IsString(cNew)
       // no StrTran() needed for CEmail class
       IF SELF IS CEMail
          SELF:cBodyHtml := cNew
       ELSE
          SELF:cBodyHtml := StrTran(cNew, DEFAULT_STOPDATA, MY_STOPDATA)
       ENDIF
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CMessage.Cargo/*" />
ACCESS Cargo
	RETURN SELF:cCargo


/// <include file="Internet.xml" path="doc/CMessage.Cargo/*" />
ASSIGN Cargo(uValue)
	SELF:cCargo := uValue


/// <include file="Internet.xml" path="doc/CMessage.ContentType/*" />
ACCESS ContentType
	RETURN SELF:cContentType


/// <include file="Internet.xml" path="doc/CMessage.ContentType/*" />
ASSIGN ContentType(uValue)
	SELF:cContentType := uValue


/// <include file="Internet.xml" path="doc/CMessage.Decode/*" />
METHOD Decode(cMail)
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
        lRet := .T.
        SELF:cHeader := SubStr3(cMail, 1, dwPos - 1) + CRLF
        cTemp := SubStr2(cMail, dwPos + SLen(cBound))
    ELSE
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




/// <include file="Internet.xml" path="doc/CMessage.DecodeAndSaveAs/*" />
METHOD DecodeAndSaveAs(cPath, cFile, cMail)
    RETURN __SaveAs(cFile, cPath, cMail)


/// <include file="Internet.xml" path="doc/CMessage.Error/*" />
ACCESS Error()
    RETURN SELF:nError


/// <include file="Internet.xml" path="doc/CMessage.Error/*" />
ASSIGN Error(n)
    IF IsNumeric(n)
        SELF:nError := n
    ENDIF


    RETURN




/// <include file="Internet.xml" path="doc/CMessage.ErrorMsg/*" />
ACCESS ErrorMsg
    RETURN SystemErrorString(SELF:nError, "Message Error " + NTrim(SELF:nError))




/// <include file="Internet.xml" path="doc/CMessage.FakeAttachmentList/*" />
METHOD FakeAttachmentList()


	LOCAL i,n		AS DWORD
	LOCAL cRet		AS STRING


	n := SELF:AttachmentCount


	cRet := CRLF


	FOR i := 1 TO n
		cRet += "<<" + SELF:aFileList[i] + ">>" + CRLF
	NEXT


	RETURN cRet


/// <include file="Internet.xml" path="doc/CMessage.From/*" />
ACCESS From
   RETURN __FormatAddress(SELF:cFromAddress, SELF:cFromName)


/// <include file="Internet.xml" path="doc/CMessage.From/*" />
ASSIGN From(cValue)
   LOCAL cAddress AS STRING
   LOCAL cName    AS STRING


	IF (cAddress := __ParseAddress(cValue,  OUT cName)) == NULL_STRING
	   cName := NULL_STRING
   ENDIF


   SELF:cFromAddress := cAddress
   SELF:cFromName    := cName


   RETURN


/// <include file="Internet.xml" path="doc/CMessage.FromAddress/*" />
ACCESS FromAddress
	RETURN SELF:cFromAddress


/// <include file="Internet.xml" path="doc/CMessage.FromAddress/*" />
ASSIGN FromAddress(uValue)
	SELF:cFromAddress := uValue


/// <include file="Internet.xml" path="doc/CMessage.FromName/*" />
ACCESS FromName
	RETURN SELF:cFromName


/// <include file="Internet.xml" path="doc/CMessage.FromName/*" />
ASSIGN FromName(uValue)
	SELF:cFromName := uValue


/// <include file="Internet.xml" path="doc/CMessage.GetAttachInfo/*" />
METHOD  GetAttachInfo (c, lNewsGroupMessage)


	// For newsgroup use only
    LOCAL nPos      AS DWORD
    LOCAL cTemp     AS STRING
    LOCAL cRest     AS STRING
    LOCAL cBound    AS STRING
    LOCAL n			AS DWORD


    cBound := SELF:cBoundary


    DEFAULT(@c, SELF:cAttach)
	 DEFAULT(@lNewsGroupMessage, .F. )


    cRest  := c


    nPos := At2(cBound, cRest)
    n    := 1


    DO WHILE nPos > 0


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


	IF lNewsGroupMessage
    	IF ALen(SELF:aAttachList) > 0
			SELF:cBody := SELF:cBody + SELF:FakeAttachmentList() + cRest
		ENDIF
	ENDIF


    RETURN NIL




/// <include file="Internet.xml" path="doc/CMessage.GetHeaderInfo/*" />
METHOD GetHeaderInfo()
    LOCAL cHeader AS STRING


    cHeader := SELF:MailHeader


    SELF:__GetMailTime(__GetMailInfo(cHeader, TEMP_DATE, FALSE))


    SELF:From       := __GetMailInfo(cHeader, TEMP_FROM, FALSE)
    SELF:ReplyTo    := __GetMailInfo(cHeader, TEMP_REPLY, FALSE)
    SELF:Subject    := __GetMailInfo(cHeader, TEMP_SUBJECT, FALSE)
    SELF:MessageID  := __GetMailInfo(cHeader, TEMP_MESSAGEID, FALSE) // , TRUE
    SELF:References := __GetMailInfo(cHeader, TEMP_REFERENCES, FALSE)


    RETURN TRUE




/// <include file="Internet.xml" path="doc/CMessage.HEADER/*" />
ACCESS HEADER()
    RETURN SELF:cHeader


/// <include file="Internet.xml" path="doc/CMessage.HEADER/*" />
ASSIGN HEADER(c)
    IF IsString(c)
       SELF:cHeader  := c
    ENDIF
    RETURN




/// <include file="Internet.xml" path="doc/CMessage.ctor/*" />
CONSTRUCTOR()


    SELF:aAttachList       := {}
    SELF:aFileList         := {}
    SELF:aTransferEncoding := {}
    SELF:aContentType      := {}


    RETURN




/// <include file="Internet.xml" path="doc/CMessage.MailBody/*" />
ACCESS MailBody()
    RETURN SELF:cBody


/// <include file="Internet.xml" path="doc/CMessage.MailBody/*" />
ASSIGN MailBody(cNew)
     SELF:Body := cNew


/// <include file="Internet.xml" path="doc/CMessage.MailDate/*" />
ACCESS MailDate
    RETURN SELF:dDate




/// <include file="Internet.xml" path="doc/CMessage.MailHeader/*" />
ACCESS MailHeader()
    RETURN SELF:cHeader


/// <include file="Internet.xml" path="doc/CMessage.MailHeader/*" />
ASSIGN MailHeader(cNew)
    SELF:Header := cNew


/// <include file="Internet.xml" path="doc/CMessage.MailTime/*" />
ACCESS MailTime
    RETURN SELF:cTime




/// <include file="Internet.xml" path="doc/CMessage.MessageID/*" />
ACCESS MessageID
	RETURN SELF:cMessageID


/// <include file="Internet.xml" path="doc/CMessage.MessageID/*" />
ASSIGN MessageID(uValue)
	SELF:cMessageID := uValue


/// <include file="Internet.xml" path="doc/CMessage.Priority/*" />
ACCESS Priority()
    RETURN SELF:nPriority


/// <include file="Internet.xml" path="doc/CMessage.Priority/*" />
ASSIGN Priority(nNew)


    IF IsNumeric(nNew)
        SELF:nPriority := nNew
    ENDIF


    RETURN


/// <include file="Internet.xml" path="doc/CMessage.References/*" />
ACCESS References
	RETURN SELF:cReferences


/// <include file="Internet.xml" path="doc/CMessage.References/*" />
ASSIGN References(uValue)
	SELF:cReferences := uValue


/// <include file="Internet.xml" path="doc/CMessage.ReplyTo/*" />
ACCESS ReplyTo
	RETURN SELF:cReplyTo


/// <include file="Internet.xml" path="doc/CMessage.ReplyTo/*" />
ASSIGN ReplyTo(uValue)
	SELF:cReplyTo := uValue
	RETURN


/// <include file="Internet.xml" path="doc/CMessage.SaveAs/*" />
METHOD SaveAs(cPath, cFile, n)


    LOCAL nRet      AS DWORD
    LOCAL nCodeType AS DWORD


    DEFAULT(@n, 1)


    IF n < 1
        RETURN .F.
    ELSE
        IF n > SELF:AttachmentCount
            RETURN .F.
        ENDIF
    ENDIF


    // Updated to better handle UNC paths
    cPath := __AdJustPath(cPath)


    DEFAULT(@cFile, "")
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




/// <include file="Internet.xml" path="doc/CMessage.SetMailTime/*" />
METHOD SetMailTime()


    SELF:dDate  := Today()
    SELF:cTime  := Time()
    SELF:cTimeStamp := GetMailTimeStamp(TRUE)	// force the date format to be British


    RETURN .T.




/// <include file="Internet.xml" path="doc/CMessage.Subject/*" />
ACCESS Subject
	RETURN SELF:cSubject


/// <include file="Internet.xml" path="doc/CMessage.Subject/*" />
ASSIGN Subject(uValue)
	SELF:cSubject := uValue


/// <include file="Internet.xml" path="doc/CMessage.TimeStamp/*" />
ACCESS  TimeStamp
    RETURN SELF:cTimeStamp




/// <include file="Internet.xml" path="doc/CMessage.TransferEncoding/*" />
ACCESS TransferEncoding
	RETURN SELF:nTransferEncoding


/// <include file="Internet.xml" path="doc/CMessage.TransferEncoding/*" />
ASSIGN TransferEncoding(uValue)
	SELF:nTransferEncoding := uValue
END CLASS


