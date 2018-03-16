#region DEFINES
STATIC DEFINE EMAILCOMPOSEDIALOG_TOMLE := 100 
STATIC DEFINE EMAILCOMPOSEDIALOG_CCMLE := 101 
STATIC DEFINE EMAILCOMPOSEDIALOG_BCCMLE := 102 
STATIC DEFINE EMAILCOMPOSEDIALOG_SUBJECTSLE := 103 
STATIC DEFINE EMAILCOMPOSEDIALOG_ATTACHMENTS := 104 
STATIC DEFINE EMAILCOMPOSEDIALOG_TOBUTTON := 105 
STATIC DEFINE EMAILCOMPOSEDIALOG_CCBUTTON := 106 
STATIC DEFINE EMAILCOMPOSEDIALOG_BCCBUTTON := 107 
STATIC DEFINE EMAILCOMPOSEDIALOG_ATTACHBUTTON := 108 
STATIC DEFINE EMAILCOMPOSEDIALOG_SUBJECT_FT := 109 
STATIC DEFINE EMAILCOMPOSEDIALOG_BODY := 110 
#endregion

CLASS EMailDialog INHERIT DATAWINDOW

   PROTECT oCaller AS OBJECT
	PROTECT oEmail AS CEmail
	PROTECT _dwFileCount AS DWORD
	
	PROTECT oAttachControl AS Control
	PROTECT oAttachments   AS AttachmentListView
	PROTECT oBody          AS Control
	PROTECT dwAttachXPos   AS DWORD

METHOD ListViewKeyDown(oListViewKeyEvent) 

	SELF:EventReturnValue := 1L	// don't call SUPER

	DO CASE
	CASE oListViewKeyEvent:KeyCode == VK_DELETE
	   IF IsMethod(SELF, #DeleteAttachment)
		   Send(SELF, #DeleteAttachment)
		ENDIF
	CASE oListViewKeyEvent:KeyCode == KEYENTER
		SELF:OpenButton()
	ENDCASE

	RETURN NIL


METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent) 

	SELF:OpenButton()

	RETURN NIL


METHOD __ResizeBody() 
	LOCAL oSize AS Dimension
	LOCAL liY   AS LONG
	LOCAL sRect IS _winRect
	
	IF SELF:IsIconic()
		RETURN NIL
	ENDIF

   IF oSurface != NULL_OBJECT .AND. oAttachments != NULL_OBJECT .AND. oBody != Null_Object

      oSize := oSurface:Size

      IF _dwFileCount = 0
         liY := 90
      ELSE

         SendMessage(oAttachments:Handle(), LVM_GETVIEWRECT , _dwFileCount-1, LONG(_CAST, @sRect))

         liY := sRect.bottom + 1l
         IF sRect.right > oSize:Width - LONG(dwAttachXPos) - 20L .and. liY < 34l
            liY += 17
         ENDIF

         liY := Min(liY, 64) + 4l
         self:SetControlSize(oAttachments, oSize:Width - LONG(dwAttachXPos), liY)
         liY := 86l + liY + 2l
      ENDIF

      SetWindowPos(oBody:Handle(), Null_Ptr, 0, liY, oSize:Width, oSize:Height - liY, _Or(SWP_NOACTIVATE, SWP_NOZORDER))
   ENDIF
   RETURN SELF

METHOD Resize(oResizeEvent) 
	
	SUPER:Resize(oResizeEvent)

	IF ! SELF:IsIconic()
		SELF:__ResizeBody()
	ENDIF

   RETURN SELF

	

METHOD FileSaveAs() 

   oEMailServer:FileSaveAs(SELF, SELF:oEmail)
	
	RETURN SELF

METHOD AddressBook() 
	
	SELF:oCaller:AddAddress()
   RETURN SELF


METHOD SetAttachments() 

   dwAttachXPos := oAttachments:Origin:X

   SELF:SetControlSize(oAttachments, oSurface:Size:Width - dwAttachXPos)

   _dwFileCount := oAttachments:Fill(SELF:oEmail)

	IF _dwFileCount > 0
		oAttachControl:Show()
		oAttachments:Show()
	ELSE
		oAttachControl:Hide()
		oAttachments:Hide()
	ENDIF
   RETURN SELF

METHOD SetControlSize(oControl, nWidth, nHeight) 
    Default(@nHeight, 23)
    SetWindowPos(oControl:Handle(), Null_Ptr, 0, 0, nWidth, nHeight, _Or(SWP_NOMOVE, SWP_NOACTIVATE, SWP_NOZORDER))		
		
    RETURN SELF

METHOD Dispatch (oEvent) 

	IF oEvent:Message == WM_COMMAND
      IF LoWord(oEvent:wParam) == IDCANCEL .or. LoWord(oEvent:wParam) == KEYESCAPE
    		// abort window
		   SELF:EndWindow()
		   RETURN 1l
		ENDIF
   ENDIF
	
	RETURN SUPER:Dispatch (oEvent)


METHOD SelectAllButton() 
   oAttachments:SelectAll()
   RETURN SELF

METHOD OpenButton() 

	oAttachments:Open(SELF:oEmail)
   RETURN SELF

METHOD SaveButton() 

   oAttachments:Save(SELF:oEmail, Null_String)
   RETURN SELF


END CLASS
class EmailComposeDialog inherit EMailDialog 

	protect oDCToMLE as MULTILINEEDIT
	protect oDCCcMLE as MULTILINEEDIT
	protect oDCBccMLE as MULTILINEEDIT
	protect oDCSubjectSLE as SINGLELINEEDIT
	protect oDCAttachments as ATTACHMENTLISTVIEW
	protect oCCToButton as PUSHBUTTON
	protect oCCCCButton as PUSHBUTTON
	protect oCCBccButton as PUSHBUTTON
	protect oCCAttachButton as PUSHBUTTON
	protect oDCSubject_FT as FIXEDTEXT
	protect oDCBody as MULTILINEEDIT

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

METHOD AcceptAndSaveEmail(lSend) 
   LOCAL lRet  AS LOGIC
	LOCAL oMail AS EmailStore
	LOCAL dwRecno AS DWORD

	IF Empty(SELF:oDCToMLE:Value)
		MessageBox(NULL_PTR, PSZ("Sorry - you must supply at least a dummy 'TO' name"), PSZ("Error"), MB_ICONSTOP+MB_OK)
		RETURN FALSE
	ENDIF

	// first - make sure MLE's have proper VALUE available
	SELF:Pointer := Pointer{POINTERHOURGLASS}
	//SELF:oDlg:UpdateControls()
	SELF:oDCBody:__Update()
	SELF:oEmail:Body     := Trim(SELF:oDCBody:Value)
	SELF:oEmail:DestList := Trim(SELF:oDCToMLE:Value)
	SELF:oEmail:CCList   := Trim(SELF:oDCCCMLE:Value)
	SELF:oEmail:BCCList  := Trim(SELF:oDCBCCMLE:Value)
	SELF:oEmail:Subject  := Trim(SELF:oDCSubjectSLE:Value)

	// save the email into the outbox
	oMail := EmailStore{}
	oMail:AddEmailForTransmission(SELF:oEmail)
	
	dwRecno := oMail:RecNo
	
	oMail:Close()
	oMail := NULL_OBJECT
	
	SELF:Pointer := NULL_OBJECT
	
	IF IsLogic(lSend) .AND. lSend
	   IF ogINetDial:Verifyconnection() 
	      lRet := SELF:oCaller:SendEmails(dwRecno) 
	      ogINetDial:HangUp()
	   ENDIF   
	ELSE
      lRet := TRUE
   ENDIF

	RETURN lRet

METHOD AddAttachment() 

	// Open a file dialog to make attachments. Drag and drop also works.
	LOCAL oFileDLG AS OpenDialog
	LOCAL uFilename AS USUAL
	LOCAL nN, nFiles AS DWORD

	oFileDLG := OpenDialog{SELF, "*.*"}
	oFileDLG:Caption := "Select Email Attachments"
	oFileDLG:InitialDirectory := aMailInfo[DEF_ATTACHPATH]
	oFileDLG:SetFilter({"*.*"}, {"All Files"})
	oFileDLG:SetStyle(OFN_HIDEREADONLY)
	oFileDLG:SetStyle(OFN_ALLOWMULTISELECT)
	oFileDLG:Show()

	uFileName := oFileDLG:FileName
	SELF:Pointer := Pointer{POINTERHOURGLASS}
	
	IF ! Empty(uFileName)
	   IF IsString(uFilename)
         uFilename := {uFilename}
      ENDIF
      IF IsArray(uFilename)
   		// add across each element
   		nFiles := ALen(uFilename)
   		FOR nN := 1 UPTO nFiles
   			IF ! SELF:oEmail:AddAttachment(uFilename[nN])
   				MessageBox(NULL_PTR,String2Psz(uFilename[nN]), PSZ("Attachment Error"), MB_OK+MB_ICONERROR)
   			ENDIF
   		NEXT nN
   	   SELF:SetAttachments()
   	   SELF:__ResizeBody()
   	ENDIF
   ENDIF

	SELF:Pointer := NULL_OBJECT

	RETURN SELF


METHOD MenuCommand(oMenuCommandEvent) 

	DO CASE
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Send_ID		//	Send this email
		SELF:SendMail()
		
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Send_Later_ID		//	Send this email later
		SELF:SendMailLater()
		
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Print_ID	//	Print out email
		SELF:PrintMail()
		
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Address_ID	//	Goto address book
		SELF:AddressBook()
		
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Attach_ID	// add an attachment
		SELF:AttachButton()
		
	CASE oMenuCommandEvent:ItemID = EmailDisplayToolBar_Close_ID	//	Close display window
		SELF:EndWindow()

	ENDCASE

	RETURN NIL


METHOD PrintMail() 
   LOCAL nN, nLines AS DWORD
	LOCAL cFilename, cLine AS STRING
	LOCAL ptrHandle AS PTR
	
	// G SCHALLER 10/10/03 - print the header and the text part only
	// as an alternate, you might like to use the subject to name the file

	cFilename := GetTempFilePath()+"Email.txt"

	ptrHandle := FCreate(cFilename, FC_NORMAL)
	IF ptrHandle = F_ERROR
		MessageBox(NULL_PTR, PSZ(DosErrString(FError())), PSZ("ERROR OPENING FILE"), MB_OK+MB_ICONERROR)
	ELSE
		nLines := MLCount(SELF:oEmail:MailHeader, 120, 9, TRUE)
		FOR nN := 1 UPTO nLines
			cLine :=MemoLine(SELF:oEmail:MailHeader, 120, nN, 9, TRUE)	// this is the full line
			FWriteLine(ptrHandle, cLine)
		NEXT
		FWriteLine(ptrHandle, "")
		nLines := MLCount(SELF:oEmail:Body,120,9,TRUE)
		FOR nN := 1 UPTO nLines
			cLine :=MemoLine(SELF:oEmail:Body,120,nN,9,TRUE)	// this is the full line
			FWriteLine(ptrHandle, cLine)
		NEXT
		FCommit(ptrHandle)
	ENDIF
	FClose(ptrHandle)

	ShellExecute(NULL_PTR, PSZ("PRINT"),String2Psz(cFilename),NULL_PSZ,NULL_PSZ,SW_SHOWNORMAL)
	
	RETURN SELF

METHOD SendMail() 

	IF SELF:AcceptAndSaveEmail(TRUE)
		SELF:EndWindow()
	ELSE
		ErrorBox{SELF, "Failed TO transmit email - try again"}:Show()
	ENDIF

	RETURN SELF


METHOD SendMailLater() 

	IF SELF:AcceptAndSaveEmail()
		SELF:EndWindow()
	ENDIF

	RETURN SELF

METHOD SetToolbar() 

	LOCAL oTB AS ToolBar

	oTB := ToolBar{} // ,,,,FALSE }

	oTB:ButtonStyle := TB_TEXTANDICON

	oTB:AppendItem(IDT_MAIL,EmailDisplayToolBar_Send_ID,,1,"Send")
	oTB:AddTipText(IDT_MAIL,EmailDisplayToolBar_Send_ID,"Send Now")

	oTB:AppendItem(IDT_MAIL,EmailDisplayToolBar_Send_Later_ID,,2,"Send Later")
	oTB:AddTipText(IDT_MAIL,EmailDisplayToolBar_Send_Later_ID,"Place in the Outbox")

	oTB:AppendItem(IDT_SEPARATOR)
	
	oTB:AppendItem(IDT_FUNCTION,EmailDisplayToolBar_Attach_ID,,4,"Attachments")
	oTB:AddTipText(IDT_FUNCTION,EmailDisplayToolBar_Attach_ID,"Add Attachment")

	oTB:AppendItem(IDT_PRINT,EmailDisplayToolBar_Print_ID,,5,"Print")
	oTB:AddTipText(IDT_PRINT,EmailDisplayToolBar_Print_ID,"Print Email")
	
	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CLOSE,EmailDisplayToolBar_Close_ID,,6,"Close")
	oTB:AddTipText(IDT_CLOSE,EmailDisplayToolBar_Close_ID,"Close Email")

	oTB:Flat := TRUE

	SELF:ToolBar := oTB

	RETURN SELF


METHOD BccButton() 

   SELF:__GetAdress(SELF:oDCBccMLE)
   RETURN SELF

METHOD CCButton() 
	
	SELF:__GetAdress(SELF:oDCCcMLE)
   RETURN SELF

METHOD AttachButton() 
	
	SELF:AddAttachment()
   RETURN SELF

METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 
	
	SELF:Icon			:= Aap_Email_Icon{}
	SELF:Caption		:= " New Email"
	
	SELF:oEmail			:= uExtra[1]
	SELF:oCaller      := uExtra[2]  // the calling window FOR Send/Receive
	
	oAttachControl := oCCAttachButton
	oAttachments   := oDCAttachments
	oBody          := oDCBody
	
	SELF:EnableStatusBar(TRUE)
	SELF:StatusBar:DisplayMessage()
	SELF:StatusBar:DisplayTime()
	
	SELF:SetToolbar()
	
	SELF:MinSize := SELF:Size
	
	SELF:oDCBody:Value   := SELF:oEmail:Body
	SELF:oDCToMLE:Value  := __Array2StrList(SELF:oEmail:DestList, ", ")
	SELF:oDCCcMLE:Value  := __Array2StrList(SELF:oEmail:CCList, ", ")
	SELF:oDCBccMLE:Value := __Array2StrList(SELF:oEmail:BCCList, ", ")

	SELF:oDCSubjectSLE:Value := SELF:oEmail:Subject
	
	SELF:oDCAttachments:AddColumn(ListViewColumn{200, HyperLabel{#FilesAttached, "Attached Files"}})
	oDCAttachments:SetStyle(WS_BORDER, FALSE)

	SELF:SetAttachments()
	
	SELF:Size := Dimension{500, 300}

	RETURN NIL


CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  
local dim aFonts[2] AS OBJECT

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"EmailComposeDialog",_GetInst()},iCtlID)

aFonts[1] := Font{,8,"Microsoft Sans Serif"}
aFonts[1]:Bold := TRUE
aFonts[2] := Font{,8,"Courier New"}

oDCToMLE := MultiLineEdit{self,ResourceID{EMAILCOMPOSEDIALOG_TOMLE,_GetInst()}}
oDCToMLE:HyperLabel := HyperLabel{#ToMLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCToMLE:OwnerAlignment := OA_WIDTH

oDCCcMLE := MultiLineEdit{self,ResourceID{EMAILCOMPOSEDIALOG_CCMLE,_GetInst()}}
oDCCcMLE:HyperLabel := HyperLabel{#CcMLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCcMLE:OwnerAlignment := OA_WIDTH

oDCBccMLE := MultiLineEdit{self,ResourceID{EMAILCOMPOSEDIALOG_BCCMLE,_GetInst()}}
oDCBccMLE:HyperLabel := HyperLabel{#BccMLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCBccMLE:OwnerAlignment := OA_WIDTH

oDCSubjectSLE := SingleLineEdit{self,ResourceID{EMAILCOMPOSEDIALOG_SUBJECTSLE,_GetInst()}}
oDCSubjectSLE:HyperLabel := HyperLabel{#SubjectSLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSubjectSLE:OwnerAlignment := OA_WIDTH

oDCAttachments := AttachmentListView{self,ResourceID{EMAILCOMPOSEDIALOG_ATTACHMENTS,_GetInst()}}
oDCAttachments:HyperLabel := HyperLabel{#Attachments,NULL_STRING,NULL_STRING,NULL_STRING}
oDCAttachments:ContextMenu := ComposeEmailContextmenu{}

oCCToButton := PushButton{self,ResourceID{EMAILCOMPOSEDIALOG_TOBUTTON,_GetInst()}}
oCCToButton:HyperLabel := HyperLabel{#ToButton,_chr(38)+"TO:",NULL_STRING,NULL_STRING}

oCCCCButton := PushButton{self,ResourceID{EMAILCOMPOSEDIALOG_CCBUTTON,_GetInst()}}
oCCCCButton:HyperLabel := HyperLabel{#CCButton,_chr(38)+"CC:",NULL_STRING,NULL_STRING}

oCCBccButton := PushButton{self,ResourceID{EMAILCOMPOSEDIALOG_BCCBUTTON,_GetInst()}}
oCCBccButton:HyperLabel := HyperLabel{#BccButton,_chr(38)+"BCC:",NULL_STRING,NULL_STRING}

oCCAttachButton := PushButton{self,ResourceID{EMAILCOMPOSEDIALOG_ATTACHBUTTON,_GetInst()}}
oCCAttachButton:HyperLabel := HyperLabel{#AttachButton,_chr(38)+"Attach:",NULL_STRING,NULL_STRING}

oDCSubject_FT := FixedText{self,ResourceID{EMAILCOMPOSEDIALOG_SUBJECT_FT,_GetInst()}}
oDCSubject_FT:HyperLabel := HyperLabel{#Subject_FT,"Subject:",NULL_STRING,NULL_STRING}
oDCSubject_FT:Font(aFonts[1], FALSE)

oDCBody := MultiLineEdit{self,ResourceID{EMAILCOMPOSEDIALOG_BODY,_GetInst()}}
oDCBody:HyperLabel := HyperLabel{#Body,NULL_STRING,NULL_STRING,NULL_STRING}
oDCBody:Font(aFonts[2], FALSE)

self:Caption := ""
self:HyperLabel := HyperLabel{#EmailComposeDialog,NULL_STRING,NULL_STRING,NULL_STRING}

if !IsNil(oServer)
	self:Use(oServer)
endif

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD DeleteAttachment() 
   oAttachments:Delete(SELF:oEmail)
   SELF:SetAttachments()
   SELF:__ResizeBody()
	
	RETURN SELF

METHOD ToButton() 
	
	SELF:__GetAdress(SELF:oDCToMLE)
   RETURN SELF

METHOD __GetAdress(oControl) 
	LOCAL oDlg      AS AddressFromBook
	LOCAL oContacts AS Contacts
	
	oContacts := Contacts{}
	oContacts:SetOrder("CONTACT")	// use THIS order here
	oContacts:GoTop()
	
	oDlg := AddressFromBook{SELF:Owner,,oContacts, oControl}
	oDlg:Show()

	RETURN SELF

METHOD AddressBook() 
	
	SELF:oCaller:AddAddress()

   RETURN SELF

END CLASS
