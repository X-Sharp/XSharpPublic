PARTIAL CLASS CStorage
   PROTECT _cPath           AS STRING
   PROTECT _lNoSave         AS LOGIC
   PROTECT _hAttFile        AS PTR
   PROTECT _dwCode          AS DWORD
   PROTECT _dwCharCount     AS DWORD
   PROTECT _cRest           AS STRING
   #ifndef __VULCAN__
    ~"ONLYEARLY+"
    DECLARE METHOD __CreateNewID
    DECLARE METHOD __StreamDecode
    DECLARE METHOD __StreamEncode
    DECLARE METHOD AttachmentAdd
    DECLARE METHOD AttachmentClose
    DECLARE METHOD AttachmentDelete
    DECLARE METHOD AttachmentFullPath
    DECLARE METHOD AttachmentOpen
    DECLARE METHOD AttachmentRead
    DECLARE METHOD AttachmentSave
    DECLARE ACCESS AttachmentSize
    DECLARE METHOD AttachmentWrite
    DECLARE METHOD CreateNewEMail
    DECLARE METHOD LoadEMail
    DECLARE ASSIGN NoSave
    DECLARE METHOD RawClose
    DECLARE METHOD RawNew
    DECLARE METHOD RawWrite
    DECLARE METHOD SaveAttachments
    DECLARE METHOD SaveEMail
    ~"ONLYEARLY-"
	#endif
METHOD __CreateNewID(cFileName AS STRING) AS STRING
   LOCAL cID      AS STRING
   LOCAL cExt     AS STRING
   LOCAL cName    AS STRING
   LOCAL dwPos    AS DWORD
   LOCAL cToFile  AS STRING

   cID     := __GetFileName(cFileName)
   cToFile := _cPath + cID

   IF File(cToFile)

      IF (dwPos := RAt2(".", cID)) > 0
         cName := SubStr3(cID, 1, dwPos - 1)
         cExt  := SubStr2(cID, dwPos)
      ELSE
         cName := cFileName
         cExt  := NULL_STRING
      ENDIF
      dwPos := 1
      DO WHILE TRUE
         cID     := cName+"~"+NTrim(dwPos)+cExt
         cToFile := _cPath + cID
         IF ! File(cToFile)
            EXIT
         ENDIF
         dwPos++
      ENDDO
   ENDIF

   RETURN cID

METHOD __StreamDecode(cData AS STRING, dwCode AS DWORD) AS STRING
   LOCAL dwDecoded AS DWORD
   LOCAL dwLength  AS DWORD
   LOCAL pBuffer   AS PTR

    IF dwCode == CODING_TYPE_BASE64
       cData    := _cRest + cData
       dwLength := SLen(cData)
       IF dwLength > 3 //SE-070523
          pBuffer := MemAlloc((dwLength / 4) * 3)
          IF pBuffer != NULL_PTR
             dwDecoded := 0
             //SE-070523
             IF (dwLength := B64Decode(String2Psz(cData), pBuffer, dwLength, @dwDecoded)) > 0
                _cRest := SubStr2(cData, dwLength+1)
                cData := Mem2String(pBuffer, dwDecoded)
             ENDIF
             MemFree(pBuffer)
          ENDIF
       ELSE
          _cRest := cData
          cData  := NULL_STRING
       ENDIF
    ELSEIF dwCode == CODING_TYPE_PRINTABLE //SE-070728 decoding of QP encoded attachments
        cData := QPDecode(cData)
    ENDIF

    RETURN cData

METHOD __StreamEncode(cData AS STRING, dwCode AS DWORD) AS STRING
    LOCAL dwCharCount AS DWORD

    IF dwCode == CODING_TYPE_BASE64
       IF ! cData == NULL_STRING
          dwCharCount := _dwCharCount
          cData := B64EncodeStream(cData, @dwCharCount)
          _dwCharCount := dwCharCount
       ENDIF
    ENDIF

    RETURN cData

METHOD AttachmentAdd(cFile AS STRING, dwCode := 0 AS DWORD) AS STRING
   LOCAL cID AS STRING

   //SE-070524
   SELF:AttachmentClose()

   IF _lNoSave
      RETURN NULL_STRING
   ENDIF

   cID := SELF:__CreateNewID(cFile)

   _dwCode   := dwCode

   _hAttFile := FCreate(_cPath + cID, FC_NORMAL)
   IF _hAttFile = F_ERROR
      SELF:AttachmentClose()
      cID := NULL_STRING
   ENDIF

   RETURN cID


METHOD AttachmentClose() AS VOID STRICT
   //Close the current attachmentfile
   IF _hAttFile != NULL_PTR
      FClose(_hAttFile)
      _hAttFile := NULL_PTR
   ENDIF

   _dwCode := _dwCharCount := 0
    _cRest  := NULL_STRING  //SE-070728

   RETURN

METHOD AttachmentDelete(cID AS STRING) AS LOGIC
   //SE-070621
   IF ! cID = ATTACHID_PATHFLAG
      //only stored attachments will be deleted and not the originals.
      IF ! (cID := SELF:AttachmentFullPath(cID)) == NULL_STRING
         RETURN DeleteFile(String2Psz(cID))
      ENDIF
   ENDIF

   RETURN TRUE

METHOD AttachmentFullPath(cAttachID AS STRING) AS STRING STRICT
   IF cAttachID = ATTACHID_PATHFLAG
      RETURN SubStr2(cAttachID, 2)
   ENDIF
   RETURN _cPath + cAttachID

METHOD AttachmentOpen(cAttachID AS STRING, dwCode := 0 AS DWORD) AS LOGIC STRICT
   //Opens an attachment file for reading
   //If cAttachID is empty, cFile should contain the full path of the file

   //SE-070524
   SELF:AttachmentClose()

   _dwCode   := dwCode

   _hAttFile := FOpen(SELF:AttachmentFullPath(cAttachID), _OR(FO_SHARED, FO_READ))
   IF _hAttFile = F_ERROR
      SELF:AttachmentClose()
      RETURN FALSE
   ENDIF

   RETURN TRUE

METHOD AttachmentRead() AS STRING STRICT
   //Read from the current attachmentfile
   LOCAL ptrData 	AS PTR
   LOCAL cData		AS STRING
   LOCAL nData		AS DWORD
   IF _hAttFile != NULL_PTR
   	//RvdH 070615 FreadStr() depends on SetAnsi!
      //RETURN SELF:__StreamEncode(FReadStr(_hAttFile, 1536), _dwCode)
		ptrData := MemAlloc(1536)
		nData := FRead3(_hAttFile, ptrData, 1536)
		cData := Mem2String(ptrData, nData)
		MemFree(ptrData)
		RETURN SELF:__StreamEncode(cData, _dwCode)
   ENDIF

   RETURN NULL_STRING

METHOD AttachmentSave(cAttachID AS STRING, cToFile AS STRING) AS LOGIC
   //Save the attachment with the ID cAttachID to the file cToFile
   //cToFile must be a full path name
   //If cAttachID is empty, cFile should contain the full path name of the file

   RETURN FCopy(SELF:AttachmentFullPath(cAttachID), cToFile)

ACCESS AttachmentSize AS DWORD STRICT
   //Calculates the size of the current opened attachment file
   LOCAL dwPos, dwSize AS LONGINT
	IF _hAttFile != NULL_PTR
      dwPos  := FTell(_hAttFile)
      dwSize := FSeek3(_hAttFile, 0, FS_END)
      FSeek3(_hAttFile, LONGINT(_CAST,dwPos), FS_SET)
   ENDIF
   RETURN DWORD(dwSize)

METHOD AttachmentWrite(cData AS STRING) AS VOID STRICT
   //Write to the current attachmentfile
   IF _hAttFile != NULL_PTR
      cData := SELF:__StreamDecode(cData, _dwCode)
      FWrite(_hAttFile, cData, SLen(cData))
   ENDIF
   RETURN

METHOD CreateNewEMail() AS CEMail STRICT
    RETURN CEMail{NIL, SELF}

CONSTRUCTOR(cPath)

   IF IsString(cPath)
      _cPath := cPath
   ELSE
      _cPath := GetDefault()
   ENDIF

   IF Right(_cPath,1) != "\"
      _cPath += "\"
   ENDIF
   RETURN

METHOD LoadEMail(cId AS STRING)
   //Load an EMail with the ID cID into an empty CEMail object and fill the object
   //You should do the following:

   LOCAL oEMail AS CEMail

   oEMail := SELF:CreateNewEmail()

   // 1.) assign oEMail:MailHeader
   // 2.) Call oEMail:GetHeaderInfo()
   // 3.) assign oEMail:Body
   // 4.) assign oEMail:Html
   // 5.) assign oEMail:AttachmentInfo


   RETURN oEMail

ASSIGN NoSave(lValue AS LOGIC)
   RETURN _lNoSave := lValue

METHOD RawClose() AS VOID STRICT
   RETURN

METHOD RawNew(oEMail AS CEmail) AS VOID STRICT
   //SE-070420
   RETURN

METHOD RawWrite(cData AS STRING) AS VOID STRICT
   RETURN

METHOD SaveAttachments(oEMail AS CEMail, lClone := FALSE AS LOGIC) AS LOGIC STRICT
   LOCAL dwI       AS DWORD
   LOCAL dwCount   AS DWORD
   LOCAL cID       AS STRING
   LOCAL cNewID    AS STRING
   LOCAL cFileName AS STRING

   IF oEMail != NULL_OBJECT
      dwCount := oEmail:AttachmentCount
      FOR dwI := 1 UPTO dwCount
          cID := oEmail:GetAttachmentInfo(dwI, ATTACH_STOREID)
          IF cID = ATTACHID_PATHFLAG .OR. lClone
             cFileName := oEmail:GetAttachmentInfo(dwI, ATTACH_FILENAME)
             cNewID    := SELF:__CreateNewID(cFileName)
             SELF:AttachmentSave(cID, _cPath + SELF:__CreateNewID(cFileName))
             oEmail:SetAttachmentInfo(dwI, ATTACH_STOREID, cNewID)
          ENDIF
      NEXT  // dwI
   ENDIF

   RETURN TRUE

METHOD SaveEMail(cId AS STRING, oEMail AS CEMail) AS LOGIC STRICT
   //Save oEMail with the current ID
   //You should save the following
   // 1.) oEMail:MailHeader for received mails, for not sended mails
   //     call at first oEMail:SetHeaderInfo() to create the MailHeader
   // 2.) oEMail:Body
   // 3.) oEMail:Html
   // 4.) Call first Self:SaveAttachments(oEMail) and than store
   //     oEMail:AttachmentInfo
   RETURN TRUE
END CLASS



#region defines
DEFINE ATTACHID_PATHFLAG := ">"
#endregion
