CLASS Contacts INHERIT DBSERVER
	

CONSTRUCTOR(cDBF, lShare, lRO, xRdd) 
	LOCAL oFS AS FILESPEC
	
	oFS 	   := FileSpec{"CONTACTS.DBF"}
	oFS:Path := "C:\Cavo28SP3\Samples\Email\"

	SUPER(oFS, TRUE, FALSE , "DBFCDX" )

	oHyperLabel := HyperLabel{#Contacts, "Contacts", "", "Contacts"}

	RETURN SELF

END CLASS
CLASS EmailStore INHERIT DBSERVER
		

METHOD AddEmail(oEmail AS CEmail, cBoxID AS STRING, cUser AS STRING, lUnread AS LOGIC) AS LOGIC PASCAL 
	LOCAL cToName AS STRING

	// find the first name in the destination list
	IF ALen(oEmail:DestList) > 0
	   cToName := oEmail:DestList[1]
   ENDIF

	// set into outgoing mail box - it awaits for transmission here
	SELF:Append()
	SELF:FIELDPUT( #BOXID, cBoxID)
	SELF:FIELDPUT( #E_DATE, Today())	
	SELF:FIELDPUT( #E_TIME, Time())
	SELF:FIELDPUT( #E_USER, cUser)
	SELF:FIELDPUT( #E_TO, cToName)
	SELF:FIELDPUT( #E_FROM, oEmail:FromName)
	SELF:FIELDPUT( #E_UNREAD, lUnread)
	SELF:FIELDPUT( #E_SUBJECT, oEmail:Subject )
	SELF:FIELDPUT( #E_PRIORITY, oEmail:Priority )
	SELF:FIELDPUT( #E_SIZE, oEmail:Size)
	SELF:FIELDPUT( #E_HEADER, oEmail:MailHeader)
	IF ! Empty(oEmail:Body)
	   SELF:FIELDPUT( #E_BODY, oEmail:Body)
	ENDIF
   IF ! Empty(oEmail:BodyHtml)
	   SELF:FIELDPUT( #E_BODYHTML, oEmail:BodyHtml)
	ENDIF
	IF oEmail:AttachmentCount > 0
	   ogStorage:SaveAttachments(oEmail)
		SELF:FIELDPUT( #E_ATCH, TRUE)
		SELF:FIELDPUT( #E_ATTACHS, oEmail:AttachmentInfo)
	ENDIF
	
	RETURN TRUE


METHOD AddEmailForTransmission (oEmail AS CEmail) AS LOGIC PASCAL 
   oEMail:SetHeaderInfo()
	SELF:AddEmail(oEmail, OUTBOX, "ADMIN", FALSE) // OutBox
	SELF:Commit()
	
	RETURN TRUE


METHOD AddReceivedEmail(oMail) 
	
	SELF:AddEmail(oMail, INBOX, "ADMIN", TRUE) // InBox
	SELF:Commit()

	RETURN SELF

METHOD ApplyInboxRules() 
	
	// assuems a prepared and decoded email
	// applies inbox business rules to determine the CORRECT inbox to receive the email
	
	RETURN INBOX

METHOD ApplySentboxRules() 
	
	// assuems a prepared and decoded email
	// applies inbox business rules to determine the CORRECT inbox to receive the email
	
	RETURN SENTBOX

METHOD Delete() 
   LOCAL oEmail  AS CEMail
   LOCAL dwI     AS DWORD
   LOCAL dwCount AS DWORD

   IF SELF:FIELDGET(#E_ATCH)
      oEMail := ogStorage:CreateNewEmail()
      oEMail:AttachmentInfo := SELF:FIELDGET(#E_ATTACHS)
      dwCount := oEMail:AttachmentCount
      FOR dwI := dwCount DOWNTO 1
          oEMail:DeleteAttachment(dwI)
      NEXT dwI
   ENDIF

   RETURN SUPER:Delete()






METHOD FileSaveAs(oOwner, oEMail) 
	LOCAL oDlg  AS SaveAsDialog
	LOCAL cTemp AS STRING
   LOCAL cFile AS STRING
   LOCAL oMail AS CEMail
   LOCAL cExt  AS STRING
   LOCAL oWindow AS Window

   IF IsInstanceOfUsual(oEMail, #CEMail)
      oMail := oEMail
   ENDIF

   oWindow := oOwner
		
	oDlg := SaveAsDialog{oWindow,"*.eml"}
	oDlg:InitialDirectory := aMailInfo[DEF_ATTACHPATH]
	oDlG:SetFilter({"*.txt", "*.htm", "*.eml"}, {"Email plain text", "Email html text", "Email file"}, 1)
	oDlg:Show()
	
	cFile := oDlg:FileName

	IF !Empty(cFile)
		IF File(cFile)
			IF MessageBox(oWindow:Handle(), String2Psz("Do you wish to replace the existing file ?"), String2Psz("FILE EXISTENCE WARNING"), MB_ICONINFORMATION+MB_YESNO) != IDYES
				RETURN SELF
			ENDIF
			FErase(cFile)
		ENDIF
	   cExt := Lower(Right(cFile, 4))
	   IF  cExt = ".txt"
         IF oMail == NULL_OBJECT
            cTemp := oEmailServer:FIELDGET(#E_Body)
         ELSE
            cTemp := oMail:Body
         ENDIF
      ELSEIF cExt = ".htm"
         IF oMail == NULL_OBJECT
            cTemp := oEmailServer:FIELDGET(#E_BodyHtml)
         ELSE
            cTemp := oMail:HtmlText
         ENDIF
	   ELSEIF cExt = ".eml" 
	      oOwner:Pointer := Pointer{POINTERHOURGLASS}
         IF oMail == NULL_OBJECT 
            SELF:SendMailAsFile(cFile, oEmailServer:GetEMail())
            //cTemp := oEmailServer:FIELDGET(#E_Header)
         ELSE 
            SELF:SendMailAsFile(cFile, oMail)
            //cTemp := oMail:MailHeader
         ENDIF
         oOwner:Pointer := NULL_OBJECT 
         oDlg:Destroy()
         RETURN SELF
      ENDIF
      IF ! Empty(cTemp)
   		oOwner:Pointer := Pointer{POINTERHOURGLASS}
   		MemoWrit(cFile, cTemp)
   		oOwner:Pointer := NULL_OBJECT
   	ELSE
	      MessageBox(oWindow:Handle(), String2Psz("Contents is empty, no file save!"), String2Psz("FILE EXISTENCE WARNING"), MB_ICONINFORMATION+MB_OK)
   	ENDIF	
	ENDIF

	oDlg:Destroy()


	RETURN SELF

METHOD GetEMail() 
   LOCAL oEmail AS CEMail

   oEMail := ogStorage:CreateNewEmail()
   oEMail:MailHeader     := SELF:FIELDGET(#E_HEADER)
   oEMail:GetHeaderInfo()
   oEMail:Body           := SELF:FIELDGET(#E_BODY)
   oEMail:BodyHtml       := SELF:FIELDGET(#E_BODYHTML)
   IF SELF:FIELDGET(#E_ATCH)
      oEMail:AttachmentInfo := SELF:FIELDGET(#E_ATTACHS)
   ENDIF

   RETURN oEMail

CONSTRUCTOR(cDBF, lShare, lRO, xRdd)  
   LOCAL oFS AS FILESPEC
	
	oFS 	   := FileSpec{"EMAIL.DBF"}
	oFS:Path := "C:\Cavo28SP3\Samples\Email\"

	SUPER(oFS, TRUE, FALSE , "DBFCDX" )

	oHyperLabel := HyperLabel{#EmailStore, "EmailStore", "", "EmailStore"}

	RETURN SELF

ACCESS MailSize 
   RETURN SELF:FIELDGET(#E_SIZE)

METHOD SendMailAsFile(cFile, oMail) 
	LOCAL cData  AS STRING
	LOCAL oEMail AS CEMail
	LOCAL hFile  AS PTR  
	
	oEMail := oMail
	
	hFile := FCreate(cFile, FC_NORMAL)
   IF hFile = F_ERROR
      RETURN SELF
   ENDIF

   oEMail:StreamStart()
   DO WHILE TRUE
      cData := oEMail:StreamOut()
      IF cData == Null_String
         EXIT
      ENDIF
      FWrite(hFile, cData, SLen(cData))
   ENDDO 
   
   FClose(hFile)

	RETURN SELF
	


END CLASS
