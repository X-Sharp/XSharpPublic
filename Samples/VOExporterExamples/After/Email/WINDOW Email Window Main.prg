#region DEFINES
DEFINE EMailForward := 3
DEFINE EMailNewFrom := 0
DEFINE EMailReply    := 1
DEFINE EMailReplyAll := 2
STATIC DEFINE EMAILWINDOWMAIN__STATUSFT := 103 
STATIC DEFINE EMAILWINDOWMAIN_EMAILLISTVIEW := 101 
STATIC DEFINE EMAILWINDOWMAIN_EMAILTREEVIEW := 102 
STATIC DEFINE EMAILWINDOWMAIN_FTMODE := 100 
DEFINE ERR_LOGON_FAILED     := 223
DEFINE ERR_WRONG_PASSWORD   := 224
DEFINE IMAGE_CLOSEBOOK := 1	
DEFINE IMAGE_OPENBOOK := 2
#endregion

class EmailWindowMain inherit DATAWINDOW 

	export oDCFTMode as FIXEDTEXT
	export oDCEmailListView as LISTVIEW
	export oDCEmailTreeview as MAILTREEVIEW
	export oDC_StatusFT as FIXEDTEXT

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)
   PROTECT oReplyMenu AS Menu
   EXPORT MailBox AS STRING
	
	~"ONLYEARLY+"
	~"ONLYEARLY-"
	

METHOD __CreateNewMailFrom(dwType AS DWORD) AS VOID PASCAL 

	LOCAL oWin     AS EmailComposeDialog
	LOCAL oEmail   AS CEmail
	LOCAL oServer  AS EmailStore
	LOCAL cSubject AS STRING
	LOCAL cCaption AS STRING
	LOCAL oLVI     AS ListViewItem
	LOCAL nRecno   AS INT

	oLVI := SELF:oDCEmailListView:GetSelectedItem()
	IF oLVI != NULL_OBJECT

		nRecno := oLVI:GetValue(#E_DATE)
		oServer := EmailStore{}
		oServer:Recno := nRecno

		oEmail := oServer:GetEMail()
		oServer:Close()
		
		oEMail:CloneAttachments()
		
		IF Empty(oEmail:BodyHtml) .and. dwType != EMailNewFrom
		   oEMail:Body := oEmail:CreateReplyBody()
		   //oEmail:BodyHtml := oEmail:CreateHtml(oEMail:Body)
      ENDIF

   	cSubject := oEmail:Subject
		
		IF dwType = EMailReply .or. dwType = EMailReplyAll
		   cCaption := " Reply To: " + cSubject
		   cSubject := "Re: "      + cSubject
		   IF dwType = EMailReply
      		oEmail:DestList := oEmail:From
   		   oEmail:CCList   := ""
         ENDIF
   	ELSEIF dwType = EMailForward
   	   cCaption := " Forward: " + cSubject
		   cSubject := "Fwd: "    + cSubject

   		// wipe out the addressees
   		oEmail:CCList   := ""
   		oEmail:DestList := ""
   	ELSE
	      cCaption := " New EMail from: " + cSubject
	   ENDIF
	
	   oEmail:FromAddress := aMailInfo[DEF_ADDRESS]
   	oEmail:FromName    := aMailInfo[DEF_FULLNAME]
   	oEmail:ReplyTo     := aMailInfo[DEF_ADDRESS]
   	oEmail:BCCList     := ""
	   oEmail:Subject     := cSubject
	
	   oWin := EmailComposeDialog{GetAppObject(),,,{oEmail, SELF}}
   	oWin:Caption := cCaption
		oWin:Show(SHOWCENTERED)

	ENDIF

	RETURN

METHOD AddAddress() 
	
	LOCAL oWin AS AddressFrombook
	LOCAL oContacts AS Contacts
	
	oContacts := Contacts{}
	oContacts:SetOrder("CONTACT")		// use this order here
	oContacts:GoTop()

	oWin := AddressFrombook{SELF,,oContacts}
	oWin:Show()
	
	RETURN SELF

METHOD BuildList() 

	LOCAL oListCol AS ListViewColumn

	oListCol := ListviewColumn{3,#E_STATE}
	oListCol:Caption := "."
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{8,#E_Date}
	oListCol:Caption := "Date"
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{8,#E_Time}
	oListCol:Caption := "Time"
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{15,#E_From}
	oListCol:Caption := "From"
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{25,#E_Subject}
	oListCol:Caption := "Subject"
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{7,#E_Size,LVCFMT_RIGHT}
	oListCol:Caption := "Size"
	oDCEmailListView:AddColumn(oListCol)

	oListCol := ListviewColumn{3,#E_Atch}
	oListCol:Caption := "#"
	oDCEmailListView:AddColumn(oListCol)

	RETURN SELF

METHOD BuildTree() 

	LOCAL oTreeItem AS TreeviewItem
	LOCAL cMailBox AS STRING
	LOCAL oImageList

	cMailBox := aMailInfo[DEF_FULLNAME]

	oImageList := ImageList{2, Dimension{16, 16}}
	oImageList:Add(BOOK{})
	oImageList:Add(OPENBOOK{})
	oDCEmailTreeView:ImageList   := oImageList


	oTreeItem := TreeviewItem{#EmailName,"Mailbox-"+cMailBox, "ROOT"}
	oTreeItem:Bold := TRUE
	oDCEmailTreeView:AddItem(#Root,oTreeItem)

	//Deleted
	oTreeItem := TreeViewItem{#Deleted,"Deleted", DELBOX}
	oDCEmailTreeView:AddItem(#EmailName,oTreeItem)	

	//inbox
	oTreeItem := TreeViewItem{#Inbox,"Inbox", INBOX}
	oDCEmailTreeView:AddItem(#EmailName,oTreeItem)	

	//Outbox
	oTreeItem := TreeViewItem{#Outbox,"Outbox", OUTBOX}
	oDCEmailTreeView:AddItem(#EmailName,oTreeItem)	

	//Sent Item
	oTreeItem := TreeViewItem{#SentItems,"Sent Items", SENTBOX}
	oDCEmailTreeView:AddItem(#EmailName,oTreeItem)	

	RETURN NIL


METHOD CheckEmailHeaders() 

	LOCAL oDlg AS ChooseDownloadDialog

	SELF:KillTheTimer()

	// display and make list selection
	oDlg := ChooseDownloadDialog{SELF}
	oDlg:Show()

	SELF:FillListView()
	SELF:SetMyTimer()

	RETURN NIL


METHOD CheckEmails() 
	LOCAL oPop 			AS MyPop
	LOCAL nList, nMail 	AS DWORD
	LOCAL oProgWin		AS ProgressWindow
	LOCAL sMessage, cTitle AS STRING

	// download all emails and process/delete. Show progress on ProgWin
	oPop := MyPop{SELF, aMailInfo[DEF_POPSERVER]}
	IF oPop:Logon(aMailInfo[DEF_ACCOUNT],aMailInfo[DEF_PASSWORD])
		oPop:GetStatus()
		
		// find out the mail count and set the progwin total and loop count
		nList := oPop:MailCount
		
		oProgWin := ProgressWindow{SELF, "Download " + NTrim(nList) + " EMail" + if(nList=1,"","s"), "",oPop:TotalBytes}
		oProgWin:AVIResource := "WebReceiveAVI"
		oProgWin:Show()

		oPop:Progress := oProgWin
		
		FOR nMail := 1 UPTO nList
			oProgWin:Message("Downloading " + NTrim(nMail) + " of " + NTrim(nList)) 
			IF oPop:GetMail(nMail)
				oEmailServer:AddReceivedEmail(oPop:Email)
				oEmailServer:Commit()
				SELF:FillListView()
				IF aMailInfo[DEF_DELETEMAIL]
					// delete the mail after successful download - maybe you can put another registry value here for this
					IF !oPop:DeleteMail(nMail)
						cTitle   := "Email Deletion Failure"
						sMessage := "Could not delete the message from the server."
					ENDIF
				ENDIF
			ELSE
				cTitle   := "Email Download Failure"
				sMessage := "Email no longer available on server"
			ENDIF
			
			IF oProgWin:Cancel
				cTitle   := "Email Download Interrupted"
				sMessage := "Only retrieved " + NTrim(nMail) + " emails."
				EXIT
			ENDIF
		NEXT nMail
	
	   oProgWin:EndDialog()
      oPop:Progress := Null_Object
	ELSE
		cTitle   := "Email Download Failure"
		sMessage := "Failed to log on to a server - TRY AGAIN LATER"
	ENDIF

	oPop:Disconnect()
	oPop:Close()

	IF !Empty(sMessage)
		Textbox{,cTitle, sMessage}:Show()
	ENDIF

	SELF:SetMyTimer()

	RETURN NIL


METHOD Config() 
	
	LOCAL oWin AS EmailConfig
	
	oWin := EmailConfig{SELF}
	oWin:Show()
	
	RETURN SELF

METHOD DeleteSelected(nCount) 

	LOCAL nRec AS DWORD
	LOCAL oLVI AS ListViewItem
	LOCAL oWarning AS WarningBox
	LOCAL cMessage AS STRING

	IF SELF:MailBox = DELBOX
		cMessage := "Permanently delete these " + NTrim(nCount) + "  messages ?"
	ELSE
		cMessage := "Move " + NTrim(nCount) + " messages to the Deleted Box ?"
	ENDIF

	oWarning := WarningBox{,"Email Delete",cMessage}
	oWarning:Type := BUTTONYESNO	

	IF oWarning:Show() == BOXREPLYYES
		oLVI := SELF:oDCEmailListView:GetSelectedItem()
		WHILE oLVI != Null_Object
			nRec := oLVI:GetValue(#E_DATE)
			oEmailServer:Goto(nRec)
			IF SELF:MailBox = DELBOX
				oEmailServer:Delete()
			ELSE
				oEmailServer:FIELDPUT(#BOXID, DELBOX)
			ENDIF
			SELF:oDCEmailListView:SelectItem( oLVI:ItemIndex, FALSE )
			oLVI := SELF:oDCEmailListView:GetSelectedItem()
		ENDDO
		oEmailServer:Commit()
		SELF:FillListView()	
	ELSE
		TextBox{,"Email Delete","Delete action aborted...."}:Show()
	ENDIF

	RETURN SELF


METHOD DeleteSingle(oLVI) 

	LOCAL nRec AS DWORD
	LOCAL oTextBox AS TextBox	
	LOCAL cMessage AS STRING

	IF oLVI != Null_Object
		nRec := oLVI:GetValue(#E_DATE)
		oEmailServer:Goto(nRec)
		IF SELF:MailBox = DELBOX
			cMessage := "Do you want to permanently delete this message"
		ELSE
			cMessage := "Move this message to the Deleted Box"
		ENDIF
		oTextBox := TextBox{,"Email Delete", cMessage}
		oTextBox:Type := BUTTONYESNO + BOXICONQUESTIONMARK
		IF oTextBox:Show() == BoxReplyYes
			IF SELF:MailBox = DELBOX
				oEmailServer:Delete()
			ELSE
				oEmailServer:FIELDPUT(#BOXID, DELBOX)
				oEmailServer:Commit()
			ENDIF
			SELF:FillListView()
		ENDIF
	ENDIF		

	RETURN SELF

METHOD Dispatch(oEvent) 

	IF oEvent:Message==WM_TIMER
	   IF oEvent:wParam == TIMER_EVENT_GETMAIL
		   SELF:SendReceive()
		   RETURN 1L
	   ENDIF
	ENDIF

	RETURN SUPER:Dispatch(oEvent)		


METHOD EMailDelete(oLVI) 	

	LOCAL nCount AS DWORD

	IF oLVI = Null_Object
		oLVI := SELF:oDCEmailListView:GetSelectedItem()
	ENDIF

	nCount :=  SELF:oDCEmailListView:SelectedCount
	IF nCount > 1
		SELF:DeleteSelected(nCount)
	ELSE
		SELF:DeleteSingle(oLVI)
	ENDIF		

	RETURN SELF


METHOD EmptyDeleted() 

	LOCAL oTextBox AS TextBox

	oTextBox := TextBox{,"Email Delete", "Are you sure you want to clean out all deleted messages"}
	oTextBox:Type := BUTTONYESNO + BOXICONQUESTIONMARK
	IF oTextBox:Show() == BoxReplyYes

		oEmailServer:OrderTopScope := DELBOX
		oEmailServer:OrderBottomScope := DELBOX
		oEmailServer:GoTop()
		WHILE !oEmailServer:Eof
			oEmailServer:Delete()
			oEmailServer:Skip()
		ENDDO

		oEmailServer:Pack()

		IF SELF:oDCEmailTreeView:SelectItem(#Deleted)
			SELF:FillListView()
		ENDIF		

	ENDIF

	RETURN SELF


METHOD FileSaveAs() 
	LOCAL oLVI AS ListViewItem

	oLVI := SELF:oDCEmailListView:GetSelectedItem()
	IF oLVI != NULL_OBJECT
		oEmailServer:Recno := oLVI:GetValue(#E_DATE)
      oEmailServer:FileSaveAs(SELF)
	ENDIF

	RETURN SELF


METHOD FillListView( oTVI ) 

	LOCAL oLVI AS ListViewItem
	LOCAL oLVC AS ListViewColumn
	LOCAL cEbox AS STRING
	LOCAL uValue AS USUAL

	IF oTVI = Null_Object
	   oTVI := SELF:oDCEmailTreeView:GetSelectedItem()
	   IF oTVI = Null_Object
         RETURN SELF
      ENDIF
	ENDIF

	cEbox := oTVI:@@Value
	SELF:oDCEmailListView:DeleteAll()

	IF cEbox == "ROOT"
		SELF:oDCFTMode:Caption := 'Personal Folder of '+aMailInfo[DEF_FULLNAME]
	ELSE
		SELF:oDCFTMode:Caption := oTVI:TextValue

		oEmailServer:ClearOrderScope()
		oEmailServer:OrderTopScope := cEbox
		oEmailServer:OrderBottomScope := cEbox
		oEmailServer:Gotop()

		DO WHILE !oEmailServer:EoF
			oLVI := ListViewItem{}
			IF oEmailServer:FIELDGET(#E_SENT)
				oLVI:ImageIndex := MAILBOXSENT
				oLVI:StateImageIndex := MAILBOXSENT
			ELSE
				IF oEmailServer:FIELDGET(#E_UNREAD)
					oLVI:ImageIndex := MAILBOXUNREAD
					oLVI:StateImageIndex := MAILBOXUNREAD
				ELSE
					oLVI:ImageIndex := MAILBOXREAD
					oLVI:StateImageIndex := MAILBOXREAD
				ENDIF					
			ENDIF								

			uValue := oEmailServer:FIELDGET(#E_DATE)
			oLVI:SetText(DToC(uValue), #E_DATE)
			oLVI:SetValue(oEmailServer:RecNo, #E_DATE)		// place holder to find the real server record

			uValue := oEmailServer:FIELDGET(#E_Atch)
			oLVI:SetText( IF(!Empty(uValue),"#",""), #E_Atch)

			oLVC := SELF:oDCEmailListView:GetColumn(#E_FROM)
			IF cEbox = INBOX
				oLVC:Caption := "From"
				uValue := Trim(oEmailServer:FIELDGET(#E_From))
			ELSE
				oLVC:Caption := "To"
				uValue := Trim(oEmailServer:FIELDGET(#E_To))
			ENDIF
			oLVI:SetText( uValue, #E_From)

			uValue := oEmailServer:FIELDGET(#E_TIME)
			oLVI:SetText( uValue, #E_TIME)

			uValue := FileSizeString(oEmailServer:FIELDGET(#E_SIZE))
			oLVI:SetText( uValue, #E_SIZE)

			uValue := Trim(oEmailServer:FIELDGET(#E_Subject))
			oLVI:SetText( uValue, #E_SUBJECT)

			SELF:oDCEmailListView:AddItem( oLVI )

			oEmailServer:Skip()
		ENDDO
	ENDIF		

	RETURN SELF


METHOD ImportMail() 
	LOCAL oEmail AS CEmail
	LOCAL oDlg  AS OpenDialog
   LOCAL cFile AS STRING
   LOCAL hFile AS PTR 
   LOCAL cLine AS STRING
   		
	oDlg := OpenDialog{SELF,"*.eml"} 
	oDlg:Caption := "Import email"
	oDlg:InitialDirectory := aMailInfo[DEF_ATTACHPATH]
	oDlg:SetFilter({"*.txt", "*.eml"}, {"Email eml-files", "Email text files"}, 1)
	oDlg:Show()
	
	cFile := oDlg:FileName 
	
   IF IsString(cFile) .AND. SLen(cFile) > 0
      
      hFile := FOpen(cFile, _OR(FO_READ,FO_SHARED))
      IF hFile != NULL_PTR 
         ApplicationExec(EXECWHILEEVENT) 
         SELF:Pointer := Pointer{POINTERHOURGLASS} 
         
         oEmail := ogStorage:CreateNewEMail()
   	
   	   oEmail:StreamStart()
      	DO WHILE TRUE
      		cLine := FReadLine2(hFile, 256)
      		IF FError() = 257
      			EXIT
      		ENDIF
      	   oEmail:StreamIn(cLine + CRLF)
      	ENDDO
      	
      	oEmail:StreamIn(NULL_STRING) //closes the stream
      
   	   oEmailServer:AddReceivedEmail(oEmail)
   	   oEmailServer:Commit()
   	   SELF:FillListView() 
   	   
   	   SELF:Pointer := NULL_OBJECT
      ENDIF   
	
	ENDIF 
	
	RETURN SELF	


CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  
local dim aFonts[2] AS OBJECT
local dim aBrushes[2] AS OBJECT

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"EmailWindowMain",_GetInst()},iCtlID)

aFonts[1] := Font{,26,"Microsoft Sans Serif"}
aFonts[1]:Bold := TRUE
aFonts[2] := Font{,8,"Microsoft Sans Serif"}
aFonts[2]:Bold := TRUE
aBrushes[1] := Brush{Color{128,128,128}}
aBrushes[2] := Brush{Color{190,255,255}}

oDCFTMode := FixedText{self,ResourceID{EMAILWINDOWMAIN_FTMODE,_GetInst()}}
oDCFTMode:HyperLabel := HyperLabel{#FTMode,"Fixed Text",NULL_STRING,NULL_STRING}
oDCFTMode:TextColor := Color{COLORWHITE}
oDCFTMode:Font(aFonts[1], FALSE)
oDCFTMode:BackGround := aBrushes[1]
oDCFTMode:OwnerAlignment := OA_WIDTH

oDCEmailListView := ListView{self,ResourceID{EMAILWINDOWMAIN_EMAILLISTVIEW,_GetInst()}}
oDCEmailListView:HyperLabel := HyperLabel{#EmailListView,NULL_STRING,NULL_STRING,NULL_STRING}
oDCEmailListView:FullRowSelect := True
oDCEmailListView:ContextMenu := EmailBrowserContextMenu{}
oDCEmailListView:OwnerAlignment := OA_WIDTH_HEIGHT

oDCEmailTreeview := MailTreeView{self,ResourceID{EMAILWINDOWMAIN_EMAILTREEVIEW,_GetInst()}}
oDCEmailTreeview:HyperLabel := HyperLabel{#EmailTreeview,NULL_STRING,NULL_STRING,NULL_STRING}
oDCEmailTreeview:BackGround := aBrushes[2]
oDCEmailTreeview:OwnerAlignment := OA_HEIGHT

oDC_StatusFT := FixedText{self,ResourceID{EMAILWINDOWMAIN__STATUSFT,_GetInst()}}
oDC_StatusFT:HyperLabel := HyperLabel{#_StatusFT,"EMail - Folders",NULL_STRING,NULL_STRING}
oDC_StatusFT:Font(aFonts[2], FALSE)

self:Caption := "Visual Objects 2.8 Mail Client"
SELF:HyperLabel := HyperLabel{#EmailWindowMain,"Visual Objects 2.8 Mail Client","Visual Objects 2.8 Mail Client",NULL_STRING}
self:Icon := AAP_EMAIL_ICON{}

if !IsNil(oServer)
	self:Use(oServer)
endif

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD InternetStatus(nStatus, xStatus) 
	LOCAL cMsg AS STRING

	DO CASE
   	CASE nStatus == INTERNET_STATUS_RECEIVING_RESPONSE

   	CASE nStatus == INTERNET_STATUS_RESPONSE_RECEIVED

      CASE nStatus == INTERNET_STATUS_SENDING_REQUEST

   	CASE nStatus == INTERNET_STATUS_REQUEST_SENT

	OTHERWISE
		IF nStatus > WSABASEERR
			cMsg := AsString(xStatus)
		ELSE
			cMsg := SubStr3( AsString(xStatus), 1, 64)
		ENDIF
		cMsg := StrZero(nStatus, 5, 0) + ": " + cMsg
		
		cMsg := StrTran(cMsg, CRLF, "")
	   cMsg := StrTran(cMsg, CHR(10), "")
	
	   SELF:StatusMessage(cMsg)
	ENDCASE

	RETURN TRUE

METHOD KillTheTimer() 

	KillTimer(SELF:Handle(), TIMER_EVENT_GETMAIL)
	
	RETURN SELF


METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent) 

	SELF:MailRead()

	RETURN SELF


METHOD MailForward() 
   SELF:__CreateNewMailFrom(EMailForward)
   RETURN SELF

METHOD MailNewFrom() 
   SELF:__CreateNewMailFrom(EMailNewFrom)
   RETURN SELF

METHOD MailRead() 

	LOCAL oWin AS EmailDisplayDialog
	LOCAL oEmail AS CEmail
	LOCAL oLVI AS ListViewItem
	LOCAL nRecno AS INT

	oLVI := SELF:oDCEmailListView:GetSelectedItem()
	IF oLVI != NULL_OBJECT

		nRecno := oLVI:GetValue(#E_DATE)
		oEmailServer:Recno := nRecno

		// mark off as being read
		IF oLVI:ImageIndex = MAILBOXUNREAD
			oEmailServer:FIELDPUT(#E_UNREAD, FALSE)
			oEmailServer:Commit()
			oLVI:ImageIndex := MAILBOXREAD
			oLVI:StateImageIndex := MAILBOXREAD
			SELF:oDCEmailListView:SetItemAttributes(oLVI)
		ENDIF
		
		oEmail := oEmailServer:GetEMail()
		oWin := EmailDisplayDialog{GetAppObject(),,,{oEmail, SELF}}
		oWin:Caption := oEmail:Subject
		oWin:Show(SHOWCENTERED)

	ENDIF

	RETURN SELF



METHOD MailReply() 
   SELF:__CreateNewMailFrom(EMailReply)
   RETURN SELF

METHOD MailReplyAll() 
   SELF:__CreateNewMailFrom(EMailReplyAll)
   RETURN SELF

METHOD MenuCommand(oMenuCommandEvent) 
   LOCAL dwItemID AS DWORD

	SUPER:MenuCommand(oMenuCommandEvent)

	SELF:KillTheTimer()
	
	dwItemID := oMenuCommandEvent:ItemID

	DO CASE
	CASE dwItemID == EmailToolBar_New_ID
		SELF:NewMail()
	CASE dwItemID == EmailToolBar_Address_ID
		SELF:AddAddress()
	CASE dwItemID == EmailToolBar_Config_ID
		SELF:Config()
	CASE dwItemID == EmailToolBar_Reply_ID
		SELF:ReplyMenu()
	CASE dwItemID ==  IDM_ReplyMenu_Reply_Reply_to_sender_ID
		SELF:MailReply()
	CASE dwItemID ==  IDM_ReplyMenu_Reply_Reply_to_all_ID
		SELF:MailReplyAll()	
	CASE dwItemID == EmailToolBar_Forward_ID
		SELF:MailForward()
	CASE dwItemID == EmailToolBar_Send_ID
		SELF:SendReceive()			
	CASE dwItemID == EmailToolBar_Delete_ID
		SELF:EmailDelete()
	CASE dwItemID == EmailToolBar_Empty_Deleted_ID
		SELF:EmptyDeleted()
	CASE dwItemID == EmailToolBar_Close_ID
		SELF:EndWindow()        
	CASE dwItemID == EmailToolBar_Import_ID
		SELF:ImportMail()	
	CASE dwItemID == EmailToolBar_Help_ID
	   HelpAbout{SELF}:Show()
	ENDCASE			

	SELF:SetMyTimer()  // Set it again

	RETURN NIL

METHOD NewMail() 
   LOCAL oWin AS EmailComposeDialog
	LOCAL oEmail AS CEmail

	oEmail := ogStorage:CreateNewEmail()
	oEmail:FromAddress := aMailInfo[DEF_ADDRESS]
	oEmail:FromName    := aMailInfo[DEF_FULLNAME]
	oEmail:ReplyTo     := aMailInfo[DEF_ADDRESS]

	oWin := EmailComposeDialog{GetAppObject(),,,{oEmail, SELF}}
	oWin:Show(SHOWCENTERED)

	RETURN SELF 

METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 
	LOCAL oImageList AS ImageList 
	
	oReplyMenu := ReplyMenu{SELF}
	
	SELF:SetToolbar()	
	
	SELF:EnableStatusBar(TRUE)
	SELF:StatusBar:DisplayMessage()
	SELF:StatusBar:DisplayTime()
	
	SELF:Minsize := SELF:Size
	
	oDCEmailTreeview:SetStyle(WS_BORDER, FALSE)
   oDCEmailListView:SetStyle(WS_BORDER, FALSE)

	SELF:SetMenuAvailable()
	SELF:oDCFTMode:Caption := 'Personal Folder of '+aMailInfo[DEF_FULLNAME]
	SELF:SetMyTimer()

	oImageList := ImageList{3, Dimension{16, 16},,_OR(ILC_COLOR24,ILC_MASK)}
	oImageList:Add(Readico{})
	oImageList:Add(UnReadico{})
	oImageList:Add(Sentico{})
	oDCEmailListView:SmallImageList := oImageList
	
	SELF:BuildTree()
	SELF:MailBox := "ROOT"
	SELF:BuildList()
	
	IF aMailInfo[DEF_STARTINBOX]
		// open up the treeview automatically to the InBox
		SELF:oDCEmailTreeview:Expand(#ROOT)
		SELF:oDCEmailTreeview:SelectItem(#INBOX)
	ENDIF

	RETURN NIL



METHOD QueryClose(oEvent) 
	LOCAL lAllowClose AS LOGIC
	
	lAllowClose := SUPER:QueryClose(oEvent)
	
	ASend(ogOpenWindows:Elements, #EndWindow) 
	   
	SELF:KillTheTimer()

	RETURN lAllowClose


METHOD RegisterEmailClient() 

	// Add values in registry to recognize this app as the default e-mail client
	// Code thanks to Dick van Kooten 1-4-2004

// 	LOCAL oReg AS Class_HKLM
// 	LOCAL oRegRoot AS Class_HKCR
// 	LOCAL cKey AS STRING
// 	LOCAL cSubkey AS STRING
// 	LOCAL cValue AS STRING
//
// 	cKey :=  "\SOFTWARE\Clients\Mail\EmailClient"     // Mail prg
// 	oReg := class_hklm{}
// 	oReg:CreateKey(cKey)
// 	oReg:WriteString(ckey,"","EmailClient")
// 	cSubkey := cKey + "\Protocols\mailto"      // Mailto link to protocol
// 	cValue := "URL:MailTo Protocol"
// 	oReg:CreateKey(cSubkey)
// 	oReg:WriteString(cSubkey,"",cValue)
//
// 	cSubkey := cKey + "\Protocols\mailto\shell\open\command"    // Program with full path and paramaters
// 	cValue := CurDrive() + ":\" + CurDir() + "\EmailClient.exe /m %1"
// 	oReg:CreateKey(cSubkey)
// 	oReg:WriteString(cSubkey,"",cValue)
//
// 	cKey:="\mailto\shell\open\command"       // Apparently a copy of Program with full path and paramaters
// 	oRegRoot:=Class_HKCR{}
// 	oRegRoot:CreateKey(cKey)
// 	oRegRoot:WriteString(ckey,"",cValue)
//
// 	TexBbox{SELF, "Registering Default Mail Client", "Your email client is now registered as the default MAIL TO client"}:Show()

	RETURN NIL

METHOD ReplyMenu() 

   ((Toolbar) SELF:ToolBar):ShowButtonMenu(EmailToolBar_Reply_ID, SELF:oReplyMenu)
   RETURN SELF

METHOD SendAllMail() 
	LOCAL dwCount AS DWORD
	LOCAL aRecords AS ARRAY
	LOCAL dwTotalBytes AS DWORD
	
	oEmailServer:SetOrder("BOXDATE")
	oEmailServer:OrderTopScope    := OUTBOX
	oEmailServer:OrderBottomScope := OUTBOX
	oEmailServer:GoTop()
	
	dwCount      := 0
	dwTotalBytes := 0
	aRecords     := {}
	DO WHILE ! oEmailServer:Eof
	   dwCount      += 1
	   dwTotalBytes += oEmailServer:MailSize
	   AAdd(aRecords, oEmailServer:Recno)
	   oEmailServer:Skip()
	ENDDO

   oEmailServer:ClearOrderScope()

	IF dwCount > 0 .and. MessageBox(null_ptr, ;
			Cast2Psz("Confirm Transmission"), Cast2Psz("Send all " + NTrim(dwCount) + " unsent messages now..."), ;
			DWORD(MB_ICONINFORMATION+MB_YESNO)) = IDYES
		
		SELF:SendEmails(aRecords, dwTotalBytes)

		SELF:FillListView()

	ENDIF

	RETURN NIL	


METHOD SendEmails(aMails, nTotalBytes) 
	LOCAL lOK      AS LOGIC
	LOCAL oSmtp    AS  MySmtp
	LOCAL oEMail   AS CEmail
	LOCAL oProgWin AS ProgressWindow
	LOCAL aEMails  AS ARRAY
   LOCAL dwRecno  AS DWORD
   LOCAL dwTotalBytes AS DWORD
   LOCAL dwI      AS DWORD
   LOCAL dwCount  AS DWORD

	// Send this email right now - build the email object and populate the SMTP object
	// put the username/password values into the class so the secure logon can be effected if required
	dwRecno := oEmailServer:Recno
	
	IF IsArray(aMails)
      aEMails      := aMails
      dwTotalBytes := nTotalBytes
   ELSEIF IsNumeric(aMails)
      aEMails      := {aMails}
      oEmailServer:Goto(aMails)
      dwTotalBytes := oEmailServer:MailSize
   ELSE
      aEMails      := {dwRecno}
      dwTotalBytes := oEmailServer:MailSize
   ENDIF

   dwCount := ALen(aEMails)
   IF dwCount = 0
      RETURN TRUE
   ENDIF

	oSmtp := MySmtp{SELF, Null_Object, aMailInfo[DEF_SMTPSERVER]}
	oSmtp:SecureSMTP := aMailInfo[DEF_SMTPAUTH]
	oSmtp:UserName := aMailInfo[DEF_SMTPNAME]
	oSmtp:PassWord := aMailInfo[DEF_SMTPPASSWD]
	
	
	oProgWin := ProgressWindow{SELF, "Send Email", "", dwTotalBytes}
	oProgWin:AVIResource := "WebSendAVI"
	oProgWin:Show()
	
   oSmtp:Progress := oProgWin

   lOk := TRUE

   FOR dwI := 1 UPTO dwCount
       oProgWin:Message("Sending " + NTrim(dwI) + " of " + NTrim(dwCount))
	    oEmailServer:GoTo(aEMails[dwI])
       oSmtp:Email := oEmail := oEmailServer:GetEMail()
	    IF oSmtp:SendMail()
	       // if successful, place into the Sent box for this user
   		 // apply outgoing message rules here....
   		 oEmailServer:FIELDPUT(#BOXID, oEmailServer:ApplySentboxRules(oEmail))		// Errors box
   	 ELSE	
   		 lOK := FALSE
   	 ENDIF
	    oEmailServer:FIELDPUT(#E_HEADER, oEmail:MailHEADER)
	    oEmailServer:FIELDPUT(#E_SENT, TRUE)
       oEmailServer:Commit()
   NEXT dwI

	oSmtp:Progress := NULL_OBJECT
	
	oProgWin:EndDialog()
	
	oEmailServer:Goto(dwRecno)
	
	IF lOK
	   IF SELF:MailBox = OUTBOX
	      SELF:FillListView()
	   ENDIF
	ELSE
      Textbox{SELF,"Error...", IF(dwCount=1,"The", "One or more") + " Email was NOT sent successfully"}:Show()
   ENDIF

	RETURN lOK


METHOD SendReceive() 
	
	SELF:KillTheTimer()	// suspend whilst waiting for the user to answer boxes
	
	IF ogINetDial:Verifyconnection()
   	SELF:SendAllMail()
   
   	IF aMailInfo[DEF_HEADERS]
   		IF MessageBox(NULL_PTR, PSZ("Confirm Header Only Download"), PSZ("Download all available headers"), MB_ICONINFORMATION+MB_YESNO) = IDYES
   		   SELF:CheckEmailHeaders()
   		ENDIF
   	ELSE
   		IF MessageBox(NULL_PTR, PSZ("Confirm Mail Download"), PSZ("Download all available mail in full"), MB_ICONINFORMATION+MB_YESNO) = IDYES
   		   SELF:CheckEmails()
   		ENDIF
   	ENDIF
		
	   ogINetDial:HangUp() 
	ENDIF  	

	SELF:FillListView()
	
	SELF:SetMyTimer()

	RETURN SELF

METHOD SetMenuAvailable(oTVI) 

	LOCAL sBox AS STRING
	LOCAL oToolbar AS Toolbar

	oToolBar := SELF:Toolbar
	IF oTVI != Null_Object
		sBox := oTVI:TextValue
	ELSE
		oTVI := SELF:oDCEmailTreeView:GetSelectedItem()	
	ENDIF		

	IF oToolBar != Null_Object .and. oTVI != Null_Object
		DO CASE
		CASE Upper(sBox) == "DELETED"
			oToolBar:DisableItem(EmailToolBar_Reply_ID )
			oToolBar:DisableItem(EmailToolBar_Forward_ID)
			oToolBar:EnableItem(EmailToolBar_Empty_Deleted_ID)
			oToolBar:EnableItem(EmailToolBar_Delete_ID )
		CASE Upper(sBox) == "INBOX"
			oToolBar:EnableItem(EmailToolBar_Reply_ID)
			oToolBar:EnableItem(EmailToolBar_Forward_ID)
			oToolBar:DisableItem(EmailToolBar_Empty_Deleted_ID)
			oToolBar:EnableItem(EmailToolBar_Delete_ID )
		CASE Upper(sBox) == "OUTBOX"
			oToolBar:DisableItem(EmailToolBar_Reply_ID )
			oToolBar:DisableItem(EmailToolBar_Forward_ID)
			oToolBar:DisableItem(EmailToolBar_Empty_Deleted_ID)
			oToolBar:EnableItem(EmailToolBar_Delete_ID )
		CASE Upper(sBox) == "SENT ITEMS"
			oToolBar:DisableItem(EmailToolBar_Reply_ID )			
			oToolBar:EnableItem(EmailToolBar_Forward_ID)
			oToolBar:DisableItem(EmailToolBar_Empty_Deleted_ID)
			oToolBar:EnableItem(EmailToolBar_Delete_ID )
		OTHERWISE
			oToolBar:DisableItem(EmailToolBar_Reply_ID )
			oToolBar:DisableItem(EmailToolBar_Forward_ID)
			oToolBar:EnableItem(EmailToolBar_Empty_Deleted_ID)
			oToolBar:DisableItem(EmailToolBar_Delete_ID )						
		ENDCASE		
	ENDIF

   RETURN SELF

METHOD SetMyTimer( ) 

	LOCAL nPeriod as DWORD

	nPeriod := aMailInfo[DEF_DELAY]

	IF nPeriod > 0
		SetTimer(SELF:Handle(), TIMER_EVENT_GETMAIL, nPeriod*60000, NULL_PTR)	
	ENDIF

	RETURN SELF


METHOD SetToolbar() 

	LOCAL oTB AS ToolBar

	oTB := ToolBar{ }

	oTB:ButtonStyle := TB_TEXTANDICON

	oTB:AppendItem(IDT_MAIL,EmailToolBar_New_ID,,1,"New")
	oTB:AddTipText(IDT_MAIL,EmailToolBar_New_ID,"Compose new Email")
	
	oTB:AppendItem(IDT_SCROLLPLUS,EmailToolBar_Reply_ID,,2,"Reply")
	oTB:AddTipText(IDT_SCROLLPLUS,EmailToolBar_Reply_ID,"Reply to sender")

	oTB:AppendItem(IDT_GOMACRO,EmailToolBar_Forward_ID,,3,"Forward")
	oTB:AddTipText(IDT_GOMACRO,EmailToolBar_Forward_ID,"Forward message") 
	
	oTB:AppendItem(IDT_DELETE,EmailToolBar_Delete_ID,,4,"Delete")
	oTB:AddTipText(IDT_DELETE,EmailToolBar_Delete_ID,"Delete message")
	
	oTB:AppendItem(IDT_SEPARATOR) 
	
	oTB:AppendItem(IDT_MAIL,EmailToolBar_Send_ID,,5,"Send/Recv")
	oTB:AddTipText(IDT_MAIL,EmailToolBar_Send_ID,"Send and Receive Email messages")
	
	oTB:AppendItem(IDT_OPEN,EmailToolBar_Import_ID,,6,"Import")
	oTB:AddTipText(IDT_OPEN,EmailToolBar_Import_ID,"Imports email from file")

	oTB:AppendItem(IDT_STOPMACRO,EmailToolBar_Empty_Deleted_ID,,7,"Empty")
	oTB:AddTipText(IDT_STOPMACRO,EmailToolBar_Empty_Deleted_ID,"Empty deleted message folder") 
	
	oTB:AppendItem(IDT_SEPARATOR)
	
	oTB:AppendItem(IDT_PREVIEW,EmailToolBar_Address_ID,,8,"Address")
	oTB:AddTipText(IDT_PREVIEW,EmailToolBar_Address_ID,"Add contact to address book") 
	
	oTB:AppendItem(IDT_Object,EmailToolBar_Config_ID,,9,"Config")
	oTB:AddTipText(IDT_Object,EmailToolBar_Config_ID,"Configuration") 
	
	//oTB:AppendItem(IDT_CLOSE,EmailToolBar_Close_ID,,10,"Close")
	//oTB:AddTipText(IDT_CLOSE,EmailToolBar_Close_ID,"Close Application")
	
	oTB:AppendItem(IDT_HELP,EmailToolBar_Help_ID,,11, "Help About")
	oTB:AddTipText(IDT_HELP,EmailToolBar_Help_ID,"Help About")
	

	oTB:Flat := TRUE

	SELF:ToolBar := oTB

	RETURN SELF

METHOD ShowEmailProperties 

	LOCAL oDlg AS EmailPropertiesDialog
	LOCAL oLVI AS ListViewItem

	oLVI := SELF:oDCEmailListView:GetSelectedItem()
	IF oLVI != Null_Object
		oDlg := EmailPropertiesDialog{SELF, oLVI:GetValue(#E_DATE)}		// pass in the server recno to the current item
		oDlg:Show(SHOWCENTERED)
	ENDIF

	RETURN SELF


METHOD ToggleRead() 

	LOCAL oLVI AS ListViewItem
	LOCAL nRecno AS INT
	LOCAL lUnRead AS LOGIC

	oLVI := SELF:oDCEmailListView:GetSelectedItem()
	IF oLVI != NULL_OBJECT

		// update server
		nRecno := oLVI:GetValue(#E_DATE)
		oEmailServer:Recno := nRecno
		IF ! oEmailServer:FIELDGET(#E_SENT)
   		lUnRead := oEmailServer:FIELDGET(#E_UNREAD)
   		oEmailServer:FIELDPUT(#E_UNREAD, !lUnRead)
   		oEmailServer:COmmit()

   		// now update listview
   		IF lUnRead
   			oLVI:ImageIndex := MAILBOXREAD
   			oLVI:StateImageIndex := MAILBOXREAD
   		ELSE
   			oLVI:ImageIndex := MAILBOXUNREAD
   			oLVI:StateImageIndex := MAILBOXUNREAD
   		ENDIF					
   		SELF:oDCEmailListView:SetItemAttributes(oLVI)
   	ENDIF	
	
	ENDIF

	RETURN SELF


METHOD TreeViewItemExpanded(oEvt) 

	LOCAL oTVI AS TreeViewItem

	oTVI := oEvt:TreeViewItem

	IF (oTVI != NULL_OBJECT)
		IF oEvt:Expanded
			oTVI:ImageIndex         := IMAGE_OPENBOOK
			oTVI:SelectedImageIndex := IMAGE_OPENBOOK
		ELSE
			oTVI:ImageIndex          := IMAGE_CLOSEBOOK
			oTVI:SelectedImageIndex  := IMAGE_CLOSEBOOK
		ENDIF
		SELF:oDCEmailTreeView:SetItemAttributes(oTVI)
	ENDIF

	RETURN SUPER:TreeViewItemExpanded(oEvt)

METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent) 
	LOCAL oTVI AS TreeViewItem
	LOCAL lChanged AS LOGIC
	
	SUPER:TreeViewSelectionChanged(oTreeViewSelectionEvent)

	oTVI := oTreeViewSelectionEvent:NewTreeViewItem
	IF oTVI != Null_Object
		lChanged := oTreeViewSelectionEvent:SelectionChanged
		IF lChanged
			oTVI:ImageIndex         := IMAGE_OPENBOOK
			oTVI:SelectedImageIndex := IMAGE_OPENBOOK		
			oDCEmailTreeView:SetItemAttributes(oTVI)
			SELF:MailBox := oTVI:@@Value
			SELF:SetMenuAvailable(oTVI)
			SELF:FillListView(oTVI)
		ENDIF	
	ENDIF

	RETURN NIL


METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent) 
	LOCAL oTVI AS TreeViewItem
	
	SUPER:TreeViewSelectionChanging(oTreeViewSelectionEvent)

	oTVI := oTreeViewSelectionEvent:OldTreeViewItem	
	IF oTVI != Null_Object .and. oTVI:namesym != #ROOT	
		oTVI:ImageIndex          := IMAGE_CLOSEBOOK
		oTVI:SelectedImageIndex  := IMAGE_CLOSEBOOK
		SELF:oDCEmailTreeView:SetItemAttributes(oTVI)
	ENDIF

	RETURN NIL


END CLASS
