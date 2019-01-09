CLASS CEmail INHERIT CMessage

	PROTECT aDestList       AS ARRAY
	PROTECT aCCList         AS ARRAY
	PROTECT aBCCList        AS ARRAY

	//PROTECT cHtmlBody       AS STRING //not used
	PROTECT cReturnReceipt  AS STRING
	PROTECT cDispositionNotification AS STRING
	PROTECT aBoundary       AS ARRAY

	PROTECT dwStreamStatus  AS DWORD  //Used for streaming
	PROTECT aStreamOut      AS ARRAY  //Used for StreamOut()
	PROTECT cContentPart    AS STRING //Used for StreamIn()

	PROTECT oStorage        AS CStorage;

#ifndef __VULCAN__
    ~"ONLYEARLY+"
    DECLARE METHOD __CreateCommands
    DECLARE METHOD __CreateContentType
    DECLARE METHOD __GetFileContentType
    DECLARE METHOD __GetContentInfo
    DECLARE METHOD __IsBoundary
    DECLARE METHOD CloneAttachments
    DECLARE METHOD CreateHtml
    DECLARE METHOD CreateReplyBody
    DECLARE METHOD DeleteAttachment
    DECLARE METHOD GetAttachmentInfo
    DECLARE METHOD SetAttachmentInfo
    DECLARE METHOD StreamIn
    DECLARE METHOD StreamOut
    ~"ONLYEARLY-"
#endif

METHOD __CreateCommands() AS ARRAY STRICT
	LOCAL lAlternate AS LOGIC
	LOCAL dwFiles    AS DWORD
	LOCAL dwI        AS DWORD
	LOCAL aCommands  AS ARRAY
	LOCAL lText, lHtml AS LOGIC

	aCommands := {}

	dwFiles    	:= SELF:AttachmentCount
	lText 		:= SLen(SELF:cBody) > 0
	lHtml 		:= SLen(SELF:cBodyHtml) > 0
	lAlternate 	:= lText .AND. lHtml

	IF dwFiles>0
		AAdd(aCommands, CEMail_CMixed)
		DO CASE
		CASE lAlternate	// Mixed
			AAdd(aCommands, CEMail_CAlternate)
			AAdd(aCommands, _OR(CEMAIL_CBody, CEMail_Body))
			AAdd(aCommands, _OR(CEMAIL_CBodyHtml, CEMail_BodyHtml))
			AAdd(aCommands, CEMail_BoundaryEnd)
		CASE lText	// Text Only
			AAdd(aCommands, _OR(CEMAIL_CBody, CEMail_Body))
		OTHERWISE	// HTML Only
			AAdd(aCommands, _OR(CEMAIL_CBodyHtml, CEMail_BodyHtml))
		ENDCASE
		FOR dwI := 1 UPTO dwFiles
			AAdd(aCommands, MakeLong(CEMAIL_Attachment, WORD(dwI)))
		NEXT //dwI
		AAdd(aCommands, CEMail_BoundaryEnd)
	ELSE
		DO CASE
		CASE lAlternate    // Mixed
			AAdd(aCommands, CEMail_CAlternate)
			AAdd(aCommands, _OR(CEMAIL_CBody, CEMail_Body))
			AAdd(aCommands, _OR(CEMAIL_CBodyHtml, CEMail_BodyHtml))
			AAdd(aCommands, CEMail_BoundaryEnd)
		CASE lText         // Text Only
			AAdd(aCommands, CEMAIL_CBody)
			AAdd(aCommands, CEMail_Body)
		OTHERWISE			 // HTML Only
			AAdd(aCommands, CEMAIL_CBodyHtml)
			AAdd(aCommands, CEMail_BodyHtml)
		ENDCASE
	ENDIF

	RETURN aCommands

METHOD __CreateContentType(dwType AS DWORD, dwCode REF DWORD) AS STRING STRICT
	LOCAL cRet AS STRING

	IF _AND(dwType, CEMail_CMixed)         = CEMail_CMixed
		cRet   := CONTENT_MULTIPART_MIXED
		dwCode := CODING_TYPE_NONE
	ELSEIF _AND(dwType, CEMail_CAlternate) = CEMail_CAlternate
		cRet   := CONTENT_MULTIPART_ALTERNATE
		dwCode := CODING_TYPE_NONE
	ELSEIF _AND(dwType, CEMAIL_CBodyHtml)  = CEMAIL_CBodyHtml
		cRet   := CONTENT_TEXT_HTML
		dwCode := CODING_TYPE_PRINTABLE
	ELSEIF _AND(dwType, CEMAIL_CBody)      = CEMAIL_CBody
		cRet   := CONTENT_TEXT_PLAIN
		dwCode := CODING_TYPE_PRINTABLE
	ENDIF

	RETURN cRet

METHOD __GetContentInfo(cPart AS STRING) AS DWORD STRICT
    //SE-090420
    //SE-070523
    LOCAL cTemp    AS STRING
    LOCAL dwType   AS DWORD
    LOCAL dwTEnc   AS DWORD
    LOCAL cContent AS STRING
    LOCAL cName    AS STRING
    LOCAL cDisposition  AS STRING

    dwType := STREAM_END

    cTemp := __GetMailInfo(cPart, TEMP_CONTENT, FALSE)
    IF cTemp == NULL_STRING
        cTemp := CONTENT_DEFAULT //Default content type
    ENDIF

    SELF:cContentType := cTemp
    //RvdH 070125 Save charset, SE-070406
    SELF:cCharSet :=  __GetToken(cTemp, TEMP_CHARSET, ";", TRUE)
    SELF:cCharSet := StrTran (SELF:cCharSet, '"')	// added this line KB 20-5-2010 because sometimes the charset is between quotes

    cContent := Lower(__GetToken(cTemp, NULL_STRING, ";", TRUE))
    IF cContent = CONTENT_MULTIPART
        cTemp := __GetToken(cTemp, TEMP_BOUNDARY, CRLF, TRUE)
        IF cTemp == NULL_STRING
            dwType := STREAM_END
        ELSE
            IF cTemp = e"\""
                cTemp := __GetToken(cTemp, e"\"", e"\"")
                IF cTemp == NULL_STRING
                    dwType := STREAM_END
                ENDIF
            ELSE
                cTemp := __GetToken(cTemp, NULL_STRING, ";", TRUE)
            ENDIF
            SELF:Boundary := BOUNDARY_DELIMITER + cTemp
            dwType := STREAM_Boundary
        ENDIF

    ELSEIF cContent = CONTENT_TEXT_PLAIN .OR. cContent = CONTENT_TEXT_HTML

        cDisposition := __GetMailInfo(cPart, TEMP_CONTENTDISPOSITION, FALSE)
        IF Lower(cDisposition) = TEMP_ATTACHMENT
            dwType := STREAM_Attachment
        ELSE
            IF cContent = CONTENT_TEXT_PLAIN
                dwType := STREAM_Plain
            ELSE
                dwType := STREAM_Html
            ENDIF
        ENDIF
    ELSE
        //message, image, audio, video, application
        //SE-070728
        cDisposition := __GetMailInfo(cPart, TEMP_CONTENTDISPOSITION, FALSE)
        dwType := STREAM_Attachment
    ENDIF

    dwTEnc := CODING_TYPE_UNKNOWN

    cTemp := __GetMailInfo(cPart, TEMP_ENCODE, FALSE)
    IF SLen(cTemp) > 0
        cTemp := Lower(cTemp)
        DO CASE
        CASE cTemp = CODING_BASE64
            dwTEnc := CODING_TYPE_BASE64
        CASE cTemp = CODING_UUENCODE
            dwTEnc := CODING_TYPE_UUENCODE
        CASE cTemp = CODING_QP
            dwTEnc := CODING_TYPE_PRINTABLE
        CASE cTemp = CODING_7BIT
            dwTEnc := CODING_TYPE_7BIT
        CASE cTemp = CODING_8BIT
            dwTEnc := CODING_TYPE_8BIT
        ENDCASE
    ENDIF

    SELF:nTransferEncoding := dwTEnc

    IF dwType = STREAM_Attachment
        //SE-070728
        cName := __GetToken(cDisposition, TEMP_FNAME, ";", TRUE)

        IF cName == NULL_STRING
            cTemp := __GetMailInfo(cPart, TEMP_CONTENT, FALSE)
            cName := __GetToken(cTemp, TEMP_NAME, ";", TRUE)
        ENDIF
        IF cName = e"\""
            cName := __GetToken(cName, e"\"", e"\"")
        ENDIF
        IF cName == NULL_STRING
            cName := "_" + NTrim(SELF:AttachmentCount + 1)
            IF cContent = CONTENT_MESSAGE
                cName := Proper(cContent) + cName + ".eml"
            ELSEIF cContent = CONTENT_TEXT
                cTemp := SubStr2(cContent,6)
                IF SLen(SELF:cCharSet)> 0
                    cName := "Text-"+ cTemp + "_" + SELF:cCharSet + cName
                ELSE
                    cName := "Text-"+ cTemp + cName
                ENDIF
                IF cTemp = "rtf" .OR. cTemp = "richtext"
                    cName += ".rtf"
				ELSEIF cTemp = "enriched" //SE-080513 added "enriched"
				   cName += "_enriched.txt"
                ELSE
                    cName += ".txt"
                ENDIF
            ELSE
                cName := Proper(cContent) + cName + ".mim"
            ENDIF
        ENDIF
        cName := __GoodFileName(cName)
        //SE-090420
        IF Empty(cDisposition)
        cTemp := __GetMailInfo(cPart, TEMP_CONTENTID, FALSE)
        IF cTemp = "<"
            cTemp := __GetToken(cTemp, "<", ">")
           ENDIF
        ELSE
           //for attachments no content id is needed
           cTemp := NULL_STRING
        ENDIF

        AAdd(SELF:aFileList,         cName)
        AAdd(SELF:aTransferEncoding, dwTEnc)
        AAdd(SELF:aContentType,      SELF:cContentType)
        AAdd(SELF:aAttachList,       {oStorage:AttachmentAdd(cName, dwTEnc), cTemp, 0})   //Open saving
    ELSE
        SELF:cContentType := cContent
    ENDIF

    RETURN dwType

METHOD __GetFileContentType(cFile AS STRING) AS STRING STRICT
	//Here we could implement a routine which calculates
	//the conten-type of a file.
	RETURN CONTENT_APPLICATION

METHOD __IsBoundary(cLine AS STRING, lEnd := FALSE AS LOGIC) AS LOGIC STRICT
	LOCAL cBound AS STRING

	IF cLine = ATTACH_BEGIN
		cBound := SELF:Boundary
		IF cBound == NULL_STRING
			RETURN FALSE
		ENDIF

		IF lEnd
			IF cLine = cBound + "--" + CRLF
				SELF:Boundary := NULL_STRING
				RETURN TRUE
			ENDIF
		ELSE
			IF cLine = cBound + CRLF
				RETURN TRUE
			ELSEIF cLine = cBound + "--" + CRLF
				RETURN TRUE
			ENDIF
		ENDIF
	ELSEIF cLine == NULL_STRING
		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD AddAttachment(cFullPath, cContentType, dwEncodeType, cContentID, cFilename)
	LOCAL cTemp  AS STRING
	LOCAL cID    AS STRING
	LOCAL cFName AS STRING

	IF IsString(cFullPath)
		cID := cFullPath
		IF FFirst(String2Psz(cID), 0)
			IF IsString(cFilename)
				cFName := cFilename
			ELSE
				cFName := __GetFileName(cID)
			ENDIF
			AAdd(SELF:aFileList, cFName)
			IF IsString(cContentID)
				cTemp := cContentID
			ELSE
				cTemp := NULL_STRING
			ENDIF
			AAdd(SELF:aAttachList, {ATTACHID_PATHFLAG+cID, cTemp, FSize()})
			IF IsString(cContentType)
				cTemp := cContentType
			ELSE
				cTemp := SELF:__GetFileContentType(cFName)
			ENDIF
			AAdd(SELF:aContentType, cTemp)
			IF ! IsNumeric(dwEncodeType)
				dwEncodeType := CODING_TYPE_BASE64
			ENDIF
			AAdd(SELF:aTransferEncoding, dwEncodeType)
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

ASSIGN  AttachmentFileList(xNew)
	LOCAL dwCount AS DWORD
	LOCAL dwI     AS DWORD
	LOCAL aFiles  AS ARRAY

	SUPER:AttachmentFileList := xNew

	aFiles := SELF:aFileList

	SELF:aFileList         := {}
	SELF:aAttachList       := {}
	SELF:aContentType      := {}
	SELF:aTransferEncoding := {}

	dwCount := ALen(aFiles)
	FOR dwI := 1 UPTO dwCount
		SELF:AddAttachment(aFiles[dwI])
	NEXT //dwI

	RETURN

ACCESS  AttachmentInfo
	//Returns the complete attachment info as a single string
	LOCAL cTemp   AS STRING
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL aProps  AS ARRAY
	LOCAL aFile   AS ARRAY

	dwCount := SELF:AttachmentCount
	IF dwCount > 0
		cTemp := NTrim(dwCount)+CRLF

		FOR dwI := 1 UPTO dwCount
			aProps := SELF:aAttachList[dwI]
			aFile  := {SELF:aFileList[dwI], SELF:aContentType[dwI], NTrim(SELF:aTransferEncoding[dwI]), ;
				aProps[ATTACH_STOREID], NTrim(aProps[ATTACH_FILESIZE]), aProps[ATTACH_CONTENTID]}
			cTemp += __Array2StrList(aFile) + CRLF
		NEXT //dwI
	ENDIF

	RETURN cTemp

ASSIGN  AttachmentInfo(cValue)
	//Stores an attachment info string into the CEMail arrays
	LOCAL cInfo   AS STRING
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL aFile   AS ARRAY
	LOCAL dwStart AS DWORD
	LOCAL dwPos   AS DWORD

	cInfo := cValue

	IF (dwPos := At2(CRLF, cInfo)) > 0
		dwCount := Val(SubStr3(cInfo, 1, dwPos-1))
		IF dwCount > 0
			SELF:aFileList         := ArrayCreate(dwCount)
			SELF:aAttachList       := ArrayCreate(dwCount)
			SELF:aContentType      := ArrayCreate(dwCount)
			SELF:aTransferEncoding := ArrayCreate(dwCount)
			dwStart := dwPos + 2
			FOR dwI := 1 UPTO dwCount
				dwStart := dwPos + 2
				IF (dwPos  := At3(CRLF, cInfo, dwStart - 1)) > 0
					aFile   := __StrList2Array(SubStr3(cInfo, dwStart, dwPos - dwStart))
					SELF:aFileList[dwI]         := aFile[1]
					SELF:aContentType[dwI]      := aFile[2]
					SELF:aTransferEncoding[dwI] := Val(aFile[3])
					SELF:aAttachList[dwI]       := {aFile[4], aFile[6], Val(aFile[5])}
				ENDIF
			NEXT //dwI
		ENDIF
	ENDIF

	IF dwCount = 0
		SELF:aFileList         := {}
		SELF:aAttachList       := {}
		SELF:aContentType      := {}
		SELF:aTransferEncoding := {}
	ENDIF

	RETURN

ACCESS BCCList
	RETURN SELF:aBCCList

ASSIGN BCCList(xNew)

	IF IsString(xNew)
		//SELF:aBCCList := __StrList2Array(xNew)
		SELF:aBCCList := __GetAddressList(xNew)
	ELSEIF IsArray(xNew)
		SELF:aBCCList := xNew
	ENDIF

	RETURN SELF:aBCCList

ACCESS Boundary
	RETURN SELF:cBoundary

ASSIGN Boundary(uValue)
	LOCAL dwCount AS DWORD

	SELF:cBoundary := uValue

	IF SELF:cBoundary == NULL_STRING
		dwCount := ALen(aBoundary)
		IF dwCount > 0
			ADel(aBoundary, dwCount)
			dwCount--
			ASize(aBoundary, dwCount)
			IF dwCount > 0
				SELF:cBoundary := aBoundary[dwCount]
			ENDIF
		ENDIF
	ELSE
		IF aBoundary = NULL_ARRAY
			aBoundary := {}
		ENDIF
		AAdd(aBoundary, SELF:cBoundary)
	ENDIF

	RETURN

ACCESS CCList
	RETURN SELF:aCCList

ASSIGN CCList(xNew)

	IF IsString(xNew)
		//SELF:aCCList := __StrList2Array(xNew)
		SELF:aCCList := __GetAddressList(xNew)
	ELSEIF IsArray(xNew)
		SELF:aCCList := xNew
	ENDIF

	RETURN SELF:aCCList

METHOD CloneAttachments() AS VOID STRICT
	LOCAL dwI       AS DWORD
	LOCAL dwCount   AS DWORD
	LOCAL cFullPath AS STRING
	LOCAL cID       AS STRING

	dwCount := SELF:AttachmentCount
	FOR dwI := 1 UPTO dwCount
		cID := SELF:GetAttachmentInfo(dwI, ATTACH_FULLPATH)
		IF cID != ATTACHID_PATHFLAG
			cFullPath := SELF:GetAttachmentInfo(dwI, ATTACH_FULLPATH)
			SELF:SetAttachmentInfo(dwI, ATTACH_STOREID, ATTACHID_PATHFLAG + cFullPath)
		ENDIF
	NEXT //dwI
	RETURN

METHOD CreateHtml(cText AS STRING) AS STRING STRICT
   //SE-040715
	//convert plain text to HTML
	LOCAL cHTM, cLine AS STRING
	LOCAL dwStart, dwEnd, dwLen AS DWORD

	// add the header
	cHTM := e"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">" +;
		e"<HTML><HEAD><META content=\"text/html; charset=iso-8859-1 \" " + CRLF + "http-equiv=Content-Type>" +;
		e"<META content=\"MSHTML 5.00.3103.1000\" name=GENERATOR><STYLE></STYLE></HEAD>" + CRLF +;
		"<BODY bgColor=#ffffff>" + CRLF
	// now, search the text for CRLF pairs and insert back into HTML sections per line
	dwLen   := SLen(cText)
	dwStart := 1
	DO WHILE dwStart < dwLen
		dwEnd := At3(CRLF, cText, dwStart-1)
		IF dwEnd = dwStart+2
			cLine   := ""
			dwStart := dwEnd+2
		ELSEIF dwEnd > 0
			cLine   := SubStr3(cText, dwStart, dwEnd-dwStart)
			dwStart := dwEnd+2
		ELSE
			cLine   := SubStr2(cText, dwStart)
			dwStart := dwLen
		ENDIF
		IF Empty(cLine)
			cHTM += "<DIV>&nbsp;</DIV>" + CRLF
		ELSE
			cHTM += "<DIV>" + cLine + "</DIV>" + CRLF
		ENDIF
	ENDDO
	// close off the HTML
	cHTM += "</BODY></HTML>" + CRLF

	RETURN cHTM

METHOD CreateReplyBody(lReFormat := TRUE AS LOGIC) AS STRING
   //SE-040707
	// Converts Text into a forward/Reply to format - does not consider HTML component
	LOCAL cTemp       AS STRING
	LOCAL cOutput     AS STRING
	LOCAL cLine       AS STRING
	LOCAL cFrom       AS STRING
	LOCAL dwStart     AS DWORD
	LOCAL dwEnd       AS DWORD
	LOCAL dwLen       AS DWORD

	// the HTML component is now left untouched therefor you may use it from your own code


	// rebuild the message as a forwarded message
	cFrom := __FormatAddress(SELF:FromAddress, SELF:FromName)

	cOutput := CRLF + CRLF + "---------- Original Message ----------" + CRLF + CRLF + ;
		"Message From: " + cFrom + CRLF + ;
		"Message To: "   + __Array2StrList(SELF:DestList) + CRLF + ;
		"Message Date: " + __GetMailInfo(SELF:MailHeader, TEMP_DATE, FALSE) + CRLF + CRLF

	IF lReFormat
		cTemp   := SELF:Body
		dwLen   := SLen(cTemp)
		dwStart := 1
		IF (dwEnd := At3(CRLF, cTemp, 1)) = 0
			dwEnd := dwLen
		ENDIF
		DO WHILE dwEnd < dwLen
			cLine   := SubStr3(cTemp, dwStart, dwEnd-dwStart+2)	// include the CRLF
			cOutput += "> " + cLine
			dwStart := dwEnd+2
			dwEnd   := At3(CRLF, cTemp, dwStart-1)
			IF dwEnd = 0 .OR. dwStart > dwLen
				dwEnd := dwLen
			ENDIF
		ENDDO
		cOutput += "> " + cLine
	ELSE
		cOutput += SELF:Body
	ENDIF

	RETURN cOutput

METHOD Decode(cMail)
   //SE-070423
   LOCAL dwPos  AS DWORD
   LOCAL dwEnd  AS DWORD
   LOCAL dwStop AS DWORD
   LOCAL cRaw   AS STRING

   cRaw  := cMail
   dwEnd := SLen(cRaw)
   dwPos := 1

   SELF:StreamStart()

   DO WHILE dwPos <= dwEnd
      IF (dwStop := At3(CRLF, cRaw, dwPos-1)) > 0
         SELF:StreamIn(SubStr3(cRaw, dwPos, dwStop - dwPos + 2))
         dwPos := dwStop + 2
      ELSE
         SELF:StreamIn(SubStr2(cRaw, dwPos))
         dwPos := dwEnd + 1
      ENDIF
   ENDDO

   SELF:StreamIn(NULL_STRING)

   RETURN TRUE

METHOD DeleteAttachment(dwIndex AS DWORD) AS LOGIC
	LOCAL uValue    AS USUAL
	LOCAL dwCount   AS DWORD
	LOCAL cID       AS STRING

	dwCount := SELF:AttachmentCount

	IF dwCount > 0 .AND. dwIndex <= dwCount
		IF dwIndex = 0
			dwIndex := dwCount
		ENDIF

		uValue := SELF:GetAttachmentInfo(dwIndex, ATTACH_STOREID)
		IF IsString(uValue)
			cID := uValue
			IF ! cID == NULL_STRING
				oStorage:AttachmentDelete(cID)
			ENDIF
		ENDIF

		ADel(SELF:aFileList,         dwIndex)
		ADel(SELF:aAttachList,       dwIndex)
		ADel(SELF:aContentType,      dwIndex)
		ADel(SELF:aTransferEncoding, dwIndex)

		dwCount -= 1

		ASize(SELF:aFileList,         dwCount)
		ASize(SELF:aAttachList,       dwCount)
		ASize(SELF:aContentType,      dwCount)
		ASize(SELF:aTransferEncoding, dwCount)

		RETURN TRUE
	ENDIF

	RETURN FALSE



ACCESS DestList()


	RETURN SELF:aDestList

ASSIGN DestList(xNew)

	IF IsString(xNew)
		//SELF:aDestList := __StrList2Array(xNew)
		SELF:aDestList := __GetAddressList(xNew)
	ELSEIF IsArray(xNew)
		SELF:aDestList := xNew
	ENDIF

	RETURN SELF:aDestList

ACCESS DispositionNotification
	RETURN SELF:cDispositionNotification

ASSIGN DispositionNotification(uValue)
	IF IsString(uValue)
		SELF:cDispositionNotification := __Array2StrList(__GetAddressList(uValue))
	ENDIF
	RETURN

METHOD GetAttachmentInfo(dwIndex AS DWORD, dwType AS DWORD) AS USUAL STRICT

	IF dwIndex > 0 .AND. dwIndex <= SELF:AttachmentCount
        IF dwType > 0 .AND. dwType < 4 //ATTACH_STOREID, ATTACH_CONTENTID, ATTACH_FILESIZE
            RETURN SELF:aAttachList[dwIndex, dwType]
        ELSEIF dwType = ATTACH_FILENAME
			RETURN SELF:aFileList[dwIndex]
		ELSEIF dwType = ATTACH_CONTENTTYPE
			RETURN SELF:aContentType[dwIndex]
		ELSEIF dwType = ATTACH_TRANSFERENCODING
			RETURN SELF:aTransferEncoding[dwIndex]
		ELSEIF dwType = ATTACH_FULLPATH
			RETURN oStorage:AttachmentFullPath(SELF:aAttachList[dwIndex, ATTACH_STOREID])
        //ELSEIF dwType > 0 .AND. dwType < 4
        //    RETURN SELF:aAttachList[dwIndex, dwType]
		ELSEIF dwType = ATTACH_SIZE
			dwType := SELF:aAttachList[dwIndex, ATTACH_FILESIZE]
			IF SELF:aTransferEncoding[dwIndex] = CODING_TYPE_BASE64
				dwType := ((dwType + 2) / 3) * 4
				dwType := dwType + (dwType / 76) * 2
			ENDIF
			RETURN dwType
		ENDIF
	ENDIF

	RETURN NIL

METHOD GetHeaderInfo()
   //SE-040622
	LOCAL cHeader AS STRING

	cHeader := SELF:MailHeader

	SUPER:GetHeaderInfo()

	// Your class might choose to pass in details of what to do with new addessees
	// or even extract the name lists in terms of our contact DBF
	SELF:DestList := __GetMailInfo(cHeader, TEMP_TO, FALSE)
	SELF:CCList   := __GetMailInfo(cHeader, TEMP_CC, FALSE)
	SELF:BCCList  := __GetMailInfo(cHeader, TEMP_BCC, FALSE)

	// Added for Jan Timmer - 15/04/04
	SELF:ReturnReceipt           := __GetMailInfo(cHeader, TEMP_RETURN_RECEIPT, .F. )
	SELF:Priority                := Val(__GetMailInfo(cHeader, TEMP_PRIORITY, TRUE))
	SELF:DispositionNotification := __GetMailInfo(cHeader, TEMP_DISPOSITIONNOTIFICATION, FALSE)

	RETURN TRUE

ACCESS HTMLText
    //SE-090420 processing of CID's without enclosing quotation marks
    //SE-040622
    // either read the HTML text or convert plain text to HTML
    //SE-071226
    LOCAL cHTM, cTag, cContentID, cTmp AS STRING
    LOCAL cFilename AS STRING
    LOCAL dwStart, dwLen, dwStop, dwPos AS DWORD
    LOCAL lQM AS LOGIC

	cHTM := SELF:BodyHtml
	IF Empty(cHTM)
		cHTM := SELF:CreateHtml(SELF:Body)
	ENDIF

	// Now - check through the string to see if there are embedded ControlIDs
	dwStart := 1
	dwLen   := SLen(cHTM)
    cTag    := "cid:"
	DO WHILE dwStart < dwLen
		// search for next CID
		IF (dwStop := At3(cTag, cHTM, dwStart)) > 0
			dwStart := dwStop + SLen(cTag)
           cTmp    := SubStr3(cHTM, dwStop-5, 5)
           IF lQM := (cTmp = e"src=\"")
              dwStop := At3(e"\"", cHTM, dwStart)
           ELSEIF cTmp = " src="
              dwStop := At3(">", cHTM, dwStart)
              dwPos  := At3(" ", cHTM, dwStart)
              IF dwPos >0 .AND. dwPos < dwStop
                 dwStop := dwPos
              ENDIF
           ELSE
              dwStop := dwStart
           ENDIF
           IF dwStart < dwStop
            // develop and scan for this CID
            cContentID := SubStr3(cHTM, dwStart, dwStop-dwStart)
            IF (dwPos := AScan(SELF:aAttachList, {|aProp| aProp[ATTACH_CONTENTID] = cContentID})) > 0
                  cFilename := "file://localhost/" + SELF:GetAttachmentInfo(dwPos, ATTACH_FULLPATH) //SE-071226
                  // replace CID in the HTML string with the disk file name
                  IF ! lQM
                     cFilename := e"\"" + cFilename + e"\""
                  ENDIF
                  cHTM := Left(cHTM, dwStart-5) + cFilename + SubStr2(cHTM, dwStop)
                  dwStart := dwStop + 1 + SLen(cFilename) - SLen(cContentID) //SE-071226
              ENDIF
            ENDIF
        ELSE
            EXIT
        ENDIF
    ENDDO

	RETURN cHTM

CONSTRUCTOR(cRawMail, uStorage)

	SUPER()

	IF IsInstanceOfUsual(uStorage, #CStorage)
		oStorage := uStorage
		//    ENDIF
		//    IF oStorage = Null_Object
	ELSE
	   //SE-070524 uStorage can be a path also.
		oStorage := CStorage{uStorage}
	ENDIF

	IF IsString(cRawMail)
		SELF:Decode(cRawMail)
	ENDIF

	RETURN

ACCESS  MailPriority
    //SE-040707
	RETURN SELF:Priority

ASSIGN MailPriority(nNew)
    //SE-040707
	//Sets the mail priority, 3 = Normal, 1 = High, 5 = Low
	RETURN SELF:Priority := nNew


METHOD MimeEncode(c, nCode)

	DEFAULT(@c, SELF:MailBody)
	DEFAULT(@nCode, SELF:TransferEncoding)

	IF nCode = 0
		nCode := CODING_TYPE_PRINTABLE
	ENDIF

	DO CASE
	CASE nCode == CODING_TYPE_PRINTABLE
		RETURN QPEncode(c)

	CASE nCode == CODING_TYPE_BASE64
		RETURN B64EncodeString(c)

	ENDCASE

   //SE-071017
	RETURN StrTran(c, CRLF + ".", CRLF + "..")

METHOD MimeHeader(nCode, xContentType, cFile, cCID)
    //SE-070419
    // Added " before and after the filenames KB 16-6-2009
    // reason: some clients could not open attachments with long filenames
    // note: Microsoft Outlook does it too
	LOCAL cRet     AS STRING
	LOCAL cTemp    AS STRING
	LOCAL lEncode  AS LOGIC
	LOCAL cContent AS STRING
	LOCAL cFName   AS STRING
    LOCAL cDispositionType AS STRING

	IF IsString(xContentType)
		cContent := xContentType
	ELSE
		cContent := SELF:cContentType
	ENDIF

	DEFAULT(@nCode, SELF:TransferEncoding)

	lEncode := TRUE

	IF SLen(cContent) = 0
		cContent := CONTENT_TEXT_PLAIN
	ENDIF

	cTemp := Lower(cContent)

	IF cTemp = CONTENT_TEXT_PLAIN .OR. cTemp = CONTENT_TEXT_HTML
		IF AtC2(TEMP_CHARSET, cContent) = 0
			cContent +=";"+CRLF + TAB + TEMP_CHARSET + CHARSET_ISO1
		ENDIF
	ELSEIF cTemp = CONTENT_MULTIPART
		cTemp := "===Part_"+StrZero(ALen(SELF:aBoundary),3,0) + "_" + BOUNDARY_VO_ID + "==="
		SELF:Boundary := BOUNDARY_DELIMITER + cTemp
		cContent += ";" + CRLF + TAB + TEMP_BOUNDARY + e"\"" + cTemp +e"\""
		lEncode := FALSE
	ELSE
		IF IsString(cFile) .AND. ! Empty(cFile)
          cFName := __EncodeField(AllTrim(cFile), SLen(TEMP_FNAME)+1)
			IF AtC2(TEMP_NAME, cContent) = 0
				cContent += ";" + CRLF + TAB + TEMP_NAME +e"\"" +cFName+e"\""
            ENDIF
            IF IsString(cCID) .AND. ! Empty(cCID)
                cDispositionType := TEMP_INLINE
            ELSE
                cDispositionType := TEMP_ATTACHMENT
			ENDIF
		ENDIF
	ENDIF

	cRet := TEMP_CONTENT + " " + cContent

	IF lEncode
		cTemp := NULL_STRING
		DO CASE
		CASE nCode = CODING_TYPE_BASE64
			cTemp := CODING_BASE64
		CASE nCode = CODING_TYPE_UUENCODE
			cTemp := CODING_UUENCODE
		CASE nCode = CODING_TYPE_PRINTABLE
			cTemp := CODING_QP
		CASE nCode = CODING_TYPE_7BIT
			cTemp := CODING_7BIT
		CASE nCode = CODING_TYPE_8BIT
			cTemp := CODING_8BIT
		ENDCASE
		IF ! cTemp == NULL_STRING
			cRet += CRLF + TEMP_ENCODE + " " + cTemp
		ENDIF
	ENDIF

	IF ! cFName == NULL_STRING
        cRet += CRLF + TEMP_CONTENTDISPOSITION + " " + cDispositionType + ";" + CRLF +;
			TAB + TEMP_FNAME + e"\"" +cFName+e"\""
	ENDIF

	RETURN cRet

ACCESS ReturnReceipt
	RETURN SELF:cReturnReceipt

ASSIGN ReturnReceipt(cFrom)
	IF IsString(cFrom)
		SELF:cReturnReceipt := __Array2StrList(__GetAddressList(cFrom))
	ENDIF
	RETURN


METHOD SaveAs(cPath, cFile, n)

	DEFAULT(@n, 1)

	IF n < 1
		RETURN FALSE
	ELSE
		IF n > SELF:AttachmentCount
			RETURN FALSE
		ENDIF
	ENDIF

    // Updated to better handle UNC paths
    cPath := __AdJustPath(cPath)

	DEFAULT(@cFile, "")
	IF SLen(cFile) = 0
		cFile := __GetFileName(SELF:aFileList[n])
	ENDIF

	RETURN oStorage:AttachmentSave(SELF:aAttachList[n, ATTACH_STOREID], cPath+cFile)

METHOD SetAttachmentInfo(dwIndex AS DWORD, dwType AS DWORD, uNewValue AS USUAL) AS USUAL STRICT
	LOCAL uOldValue AS USUAL
	LOCAL dwCount   AS DWORD

	dwCount := SELF:AttachmentCount

	IF dwIndex <= dwCount
		IF dwIndex = 0
			dwIndex := dwCount
		ENDIF
		uOldValue := SELF:GetAttachmentInfo(dwIndex, dwType)
		IF ValType(uOldValue) = ValType(uNewValue)
			IF dwType = ATTACH_FILENAME
				SELF:aFileList[dwIndex] := uNewValue
			ELSEIF dwType = ATTACH_CONTENTTYPE
				SELF:aContentType[dwIndex] := uNewValue
			ELSEIF dwType = ATTACH_TRANSFERENCODING
				SELF:aTransferEncoding[dwIndex] := uNewValue
			ELSEIF dwType > 0 .AND. dwType < 4
				SELF:aAttachList[dwIndex, dwType] := uNewValue
			ENDIF
		ENDIF
	ENDIF

	RETURN uOldValue

METHOD SetHeaderInfo()
   //SE-070419
	LOCAL cBuffer AS STRING

	SELF:SetMailTime()

	cBuffer := TEMP_DATE + " " + SELF:TimeStamp + CRLF +;
	           __CreateAddressList(TEMP_FROM, {__FormatAddress(SELF:FromAddress, SELF:FromName)})

	// Build the To: list
	IF ALen(SELF:DestList) > 0
		cBuffer += __CreateAddressList(TEMP_TO, SELF:DestList)
	ENDIF

	cBuffer += TEMP_SUBJECT + " " + __EncodeField(SELF:Subject, SLen(TEMP_SUBJECT)+1) + CRLF

	// Build the Cc: list
	IF ALen(SELF:CCList) > 0
		cBuffer += __CreateAddressList(TEMP_CC, SELF:CCList)
	ENDIF

	// Build the Bcc: list (don't use this list for a reply or a forwarding)
	//RvdH 070325 NEVER include the BCC list in the header...
	//IF ALen(SELF:BCCList) > 0
	//	cBuffer += TEMP_BCC + " " + __CreateAddressList(SELF:BCCList)
	//ENDIF

	IF SLen(SELF:ReplyTo) > 0
      cBuffer += __CreateAddressList(TEMP_REPLY, {SELF:ReplyTo}, FALSE)
	ENDIF

	IF SLen(SELF:ReturnReceipt) > 0
	   cBuffer += __CreateAddressList(TEMP_RETURN_RECEIPT, __GetAddressList(SELF:ReturnReceipt), FALSE)
	ENDIF

   IF SLen(SELF:DispositionNotification) > 0
	   cBuffer += __CreateAddressList(TEMP_DISPOSITIONNOTIFICATION, __GetAddressList(SELF:DispositionNotification), FALSE)
	ENDIF

   IF SELF:MailPriority > 0
      cBuffer += TEMP_PRIORITY + " " + NTrim(SELF:MailPriority) + CRLF
   ENDIF

   cBuffer += TEMP_MIMEVERSION + CRLF + SELF:MimeHeader(,SELF:cContentType) + CRLF

	IF SLen(SELF:Cargo) > 0
		cBuffer += SELF:Cargo
	ENDIF

   SELF:cHeader := cBuffer

   RETURN cBuffer


ACCESS Size
	LOCAL dwSize      AS DWORD
	LOCAL dwI         AS DWORD
	LOCAL dwCount     AS DWORD
	LOCAL cMailHeader AS STRING

	cMailHeader := SELF:MailHeader
	SELF:SetHeaderInfo()

	dwSize := SLen(SELF:MailHeader) + SLen(SELF:Body) + SLen(SELF:BodyHtml)
	dwCount := SELF:AttachmentCount
	FOR dwI := 1 UPTO dwCount
		dwSize += SELF:GetAttachmentInfo(dwI, ATTACH_SIZE)
	NEXT //dwI

	SELF:MailHeader := cMailHeader

	RETURN dwSize

METHOD StreamIn(cData AS STRING) AS LOGIC STRICT
   //SE-070421
	IF dwStreamStatus = STREAM_RESET  //Reset Header
		oStorage:RawNew(SELF)
		SELF:cHeader      := NULL_STRING
		SELF:cBoundary    := NULL_STRING
		SELF:aBoundary    := NULL_ARRAY
        SELF:cBody        := NULL_STRING
        SELF:cBodyHtml    := NULL_STRING
		dwStreamStatus    := STREAM_Header
	ENDIF

	oStorage:RawWrite(cData)

	IF dwStreamStatus = STREAM_Header //Read Header
		IF cData == CRLF
			SELF:GetHeaderInfo()
			dwStreamStatus := SELF:__GetContentInfo(cHeader)
		ELSE
			SELF:cHeader += cData
		ENDIF

	ELSEIF dwStreamStatus = STREAM_Plain .OR. dwStreamStatus = STREAM_Html //Read plain text or html text
		IF SELF:__IsBoundary(cData)
			IF dwStreamStatus = STREAM_Plain
                SELF:cBody += SELF:__DecodeContent(cContentPart)
			ELSE
                SELF:cBodyHtml += SELF:__DecodeContent(cContentPart)
			ENDIF

			IF SELF:__IsBoundary(cData, TRUE)
				IF SELF:Boundary == NULL_STRING
					dwStreamStatus := STREAM_END
				ELSE
					dwStreamStatus := STREAM_Boundary
				ENDIF
			ELSE
				//next part
				dwStreamStatus := STREAM_Part
				cContentPart   := NULL_STRING
			ENDIF
		ELSE
			cContentPart += cData
		ENDIF

	ELSEIF dwStreamStatus = STREAM_Attachment //Read attachment
		IF SELF:__IsBoundary(cData)
			//Close saving

			SELF:SetAttachmentInfo(0, ATTACH_FILESIZE, oStorage:AttachmentSize)
			oStorage:AttachmentClose()

			IF SELF:__IsBoundary(cData, TRUE)
				IF SELF:cBoundary == NULL_STRING
					dwStreamStatus := STREAM_END
				ELSE
					dwStreamStatus := STREAM_Boundary
				ENDIF
			ELSE
				//next part
				dwStreamStatus := STREAM_Part
				cContentPart   := NULL_STRING
			ENDIF
		ELSE
			//Save attachment line
			oStorage:AttachmentWrite(cData)
		ENDIF

	ELSEIF dwStreamStatus = STREAM_Boundary //Next boundary
        IF !SELF:__IsBoundary(cData, TRUE)	// ignore a closing boundary KB 7-12-2009
	        IF SELF:__IsBoundary(cData)
	            dwStreamStatus := STREAM_Part
	            cContentPart   := NULL_STRING
	        ENDIF
        ENDIF

	ELSEIF dwStreamStatus = STREAM_Part
		IF cData == CRLF
			dwStreamStatus := SELF:__GetContentInfo(cContentPart)
			cContentPart   := NULL_STRING
		ELSE
			cContentPart += cData
		ENDIF
	ENDIF

   IF cData == NULL_STRING
      dwStreamStatus := STREAM_END
		oStorage:RawClose()
		RETURN FALSE
	ENDIF

	RETURN TRUE

METHOD StreamOut() AS STRING STRICT
	LOCAL cData  AS STRING
	LOCAL dwType AS DWORD
	LOCAL cTemp  AS STRING
	LOCAL dwFile AS DWORD
	LOCAL dwCode AS DWORD

	IF dwStreamStatus = STREAM_RESET
		aStreamOut        := SELF:__CreateCommands()
		dwStreamStatus    := 1
		dwType            := aStreamOut[dwStreamStatus]
		SELF:cContentType := SELF:__CreateContentType(dwType, @dwCode)
		SELF:nTransferEncoding := dwCode
		cData             := SELF:SetHeaderInfo()

		IF _AND(dwType, CEMail_CMixed) ==  CEMail_CMixed ;
				.OR. _AND(dwType, CEMail_CAlternate)  == CEMail_CAlternate
			cData += CRLF + TEMP_MULIPARTINFO + CRLF
		ENDIF
		dwStreamStatus := 2
	ELSE
		IF dwStreamStatus <= ALen(aStreamOut)
			dwType := aStreamOut[dwStreamStatus]
			dwFile := HiWord(dwType)
			dwType := LoWord(dwType)
			dwStreamStatus += 1

			IF dwType = CEMAIL_Attachment
				IF dwFile>0
					cData := CRLF + SELF:Boundary + CRLF
                    cTemp := aAttachList[dwFile, ATTACH_CONTENTID] //SE-080513
		              cData += SELF:MimeHeader(aTransferEncoding[dwFile], aContentType[dwFile], __GetFileName(aFileList[dwFile]), cTemp) + CRLF
		              /*
                    cTemp := __GetFileName(aFileList[dwFile])
                    cData += SELF:MimeHeader(aTransferEncoding[dwFile], aContentType[dwFile], cTemp) + CRLF
                    cTemp := aAttachList[dwFile, ATTACH_CONTENTID]
                    */

					IF cTemp == NULL_STRING
						cData += CRLF
					ELSE
						//RvdH 030720 Some clients want <> delimiters around the content-id
						cData += TEMP_CONTENTID + " <" + cTemp +">"+ CRLF + CRLF
					ENDIF
					oStorage:AttachmentOpen(SELF:aAttachList[dwFile, ATTACH_STOREID], aTransferEncoding[dwFile])
					dwStreamStatus -= 1
					aStreamOut[dwStreamStatus] := dwType
				ELSE
					cData := oStorage:AttachmentRead()
					IF cData == NULL_STRING
					   //SE-070523
					   oStorage:AttachmentClose()
						cData := CRLF
					ELSE
						dwStreamStatus -= 1
					ENDIF
				ENDIF
			ELSE
				cTemp := SELF:Boundary
				IF ! cTemp == NULL_STRING
					IF _AND(dwType, CEMail_BoundaryEnd) > 0
						cTemp += BOUNDARY_DELIMITER
						SELF:Boundary := NULL_STRING
					ENDIF
					cData += CRLF + cTemp + CRLF
				ENDIF

				cTemp := SELF:__CreateContentType(dwType, @dwCode)
				IF ! cTemp == NULL_STRING
					cData += SELF:MimeHeader(dwCode, cTemp) + CRLF
				ENDIF

				cTemp := NULL_STRING
				IF _AND(dwType, CEMail_Body) > 0
					cTemp := SELF:cBody
				ELSEIF _AND(dwType, CEMail_BodyHtml) > 0
					cTemp := SELF:cBodyHtml
				ENDIF
				IF ! cTemp == NULL_STRING
					cData += CRLF + SELF:MimeEncode(cTemp) + CRLF
				ENDIF
			ENDIF

		ELSE
			aStreamOut := NULL_ARRAY
		ENDIF
	ENDIF

	RETURN cData

METHOD StreamStart()
   //SE-070421
   dwStreamStatus := STREAM_RESET
	RETURN TRUE
END CLASS



#region defines
STATIC DEFINE CEMAIL_Attachment  := 0x0100
STATIC DEFINE CEMail_Body        := 0b00000001
STATIC DEFINE CEMail_BodyHtml    := 0b00000010
STATIC DEFINE CEMail_BoundaryEnd := 0b10000000
STATIC DEFINE CEMail_CAlternate  := 0b00001100
STATIC DEFINE CEMAIL_CBody       := 0b00000100
STATIC DEFINE CEMAIL_CBodyHtml   := 0b00001000
STATIC DEFINE CEMail_CMixed      := 0b00010000
STATIC DEFINE CEMail_CRelated    := 0b00010100
STATIC DEFINE STREAM_Attachment := 4
STATIC DEFINE STREAM_Boundary := 5
STATIC DEFINE STREAM_END := 9
STATIC DEFINE STREAM_Header  := 1
STATIC DEFINE STREAM_Html    := 3
STATIC DEFINE STREAM_Part := 6
STATIC DEFINE STREAM_Plain   := 2
STATIC DEFINE STREAM_RESET   := 0
#endregion
