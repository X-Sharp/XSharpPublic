#region DEFINES
STATIC DEFINE EMAILDISPLAYDIALOG_ATTACH_FT := 110 
STATIC DEFINE EMAILDISPLAYDIALOG_ATTACHMENTS := 111 
STATIC DEFINE EMAILDISPLAYDIALOG_CC_FT := 106 
STATIC DEFINE EMAILDISPLAYDIALOG_CCMLE := 107 
STATIC DEFINE EMAILDISPLAYDIALOG_DATE_FT := 102 
STATIC DEFINE EMAILDISPLAYDIALOG_DATETEXT := 103 
STATIC DEFINE EMAILDISPLAYDIALOG_FROM_FT := 100 
STATIC DEFINE EMAILDISPLAYDIALOG_FROMTEXT := 101 
STATIC DEFINE EMAILDISPLAYDIALOG_SUBJECT_FT := 108 
STATIC DEFINE EMAILDISPLAYDIALOG_SUBJECTTEXT := 109 
STATIC DEFINE EMAILDISPLAYDIALOG_TO_FT := 104 
STATIC DEFINE EMAILDISPLAYDIALOG_TOMLE := 105 
STATIC DEFINE EMAILDISPLAYDIALOG_WEBBROWSER := 112 
#endregion

CLASS EmailDisplayDialog INHERIT EMailDialog 

	PROTECT oDCFrom_FT AS FIXEDTEXT
	PROTECT oDCFromText AS FIXEDTEXT
	PROTECT oDCDate_FT AS FIXEDTEXT
	PROTECT oDCDateText AS FIXEDTEXT
	PROTECT oDCTo_FT AS FIXEDTEXT
	PROTECT oDCToMLE AS MULTILINEEDIT
	PROTECT oDCCC_FT AS FIXEDTEXT
	PROTECT oDCCcMLE AS MULTILINEEDIT
	PROTECT oDCSubject_FT AS FIXEDTEXT
	PROTECT oDCSubjectText AS FIXEDTEXT
	PROTECT oDCAttach_FT AS FIXEDTEXT
	PROTECT oDCAttachments AS ATTACHMENTLISTVIEW
	PROTECT oDCWebBrowser AS WEBBROWSER

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)
   PROTECT oLVI AS ListViewItem
   EXPORT oServer AS EmailStore
   PROTECT oReplyMenu AS Menu


METHOD AttachButton( ) 
	LOCAL oDlg AS AttachmentsDialog
	LOCAL oEmail AS CEmail
	
	oEmail := oEmailServer:GetEMail()
	oDlg := AttachmentsDialog{SELF,oEmail}		// send the decoded email object
	oDlg:Show()
	
	RETURN SELF
	

METHOD DeleteMail() 
	
	SELF:oCaller:DeleteSingle(SELF:oLVI)
	SELF:EndWindow()
	
	RETURN SELF

METHOD Destroy( ) 

   ogOpenWindows:UnRegister(SELF)
   
   RETURN SUPER:Destroy()

CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  
LOCAL DIM aFonts[1] AS OBJECT

self:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"EmailDisplayDialog",_GetInst()},iCtlID)

aFonts[1] := Font{,8,"Microsoft Sans Serif"}
aFonts[1]:Bold := TRUE

oDCFrom_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_FROM_FT,_GetInst()}}
oDCFrom_FT:HyperLabel := HyperLabel{#From_FT,"From:",NULL_STRING,NULL_STRING}
oDCFrom_FT:Font(aFonts[1], FALSE)

oDCFromText := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_FROMTEXT,_GetInst()}}
oDCFromText:HyperLabel := HyperLabel{#FromText,"Fixed Text",NULL_STRING,NULL_STRING}
oDCFromText:OwnerAlignment := OA_WIDTH

oDCDate_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_DATE_FT,_GetInst()}}
oDCDate_FT:HyperLabel := HyperLabel{#Date_FT,"Date:",NULL_STRING,NULL_STRING}
oDCDate_FT:Font(aFonts[1], FALSE)
oDCDate_FT:OwnerAlignment := OA_X

oDCDateText := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_DATETEXT,_GetInst()}}
oDCDateText:HyperLabel := HyperLabel{#DateText,"Fixed Text",NULL_STRING,NULL_STRING}
oDCDateText:OwnerAlignment := OA_X

oDCTo_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_TO_FT,_GetInst()}}
oDCTo_FT:HyperLabel := HyperLabel{#To_FT,"To:",NULL_STRING,NULL_STRING}
oDCTo_FT:Font(aFonts[1], FALSE)

oDCToMLE := MultiLineEdit{SELF,ResourceID{EMAILDISPLAYDIALOG_TOMLE,_GetInst()}}
oDCToMLE:HyperLabel := HyperLabel{#ToMLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCToMLE:OwnerAlignment := OA_WIDTH

oDCCC_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_CC_FT,_GetInst()}}
oDCCC_FT:HyperLabel := HyperLabel{#CC_FT,"Cc:",NULL_STRING,NULL_STRING}
oDCCC_FT:Font(aFonts[1], FALSE)

oDCCcMLE := MultiLineEdit{SELF,ResourceID{EMAILDISPLAYDIALOG_CCMLE,_GetInst()}}
oDCCcMLE:HyperLabel := HyperLabel{#CcMLE,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCcMLE:OwnerAlignment := OA_WIDTH

oDCSubject_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_SUBJECT_FT,_GetInst()}}
oDCSubject_FT:HyperLabel := HyperLabel{#Subject_FT,"Subject:",NULL_STRING,NULL_STRING}
oDCSubject_FT:Font(aFonts[1], FALSE)

oDCSubjectText := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_SUBJECTTEXT,_GetInst()}}
oDCSubjectText:HyperLabel := HyperLabel{#SubjectText,"Fixed Text",NULL_STRING,NULL_STRING}
oDCSubjectText:OwnerAlignment := OA_WIDTH

oDCAttach_FT := FixedText{SELF,ResourceID{EMAILDISPLAYDIALOG_ATTACH_FT,_GetInst()}}
oDCAttach_FT:HyperLabel := HyperLabel{#Attach_FT,"Attach:",NULL_STRING,NULL_STRING}
oDCAttach_FT:Font(aFonts[1], FALSE)

oDCAttachments := AttachmentListView{SELF,ResourceID{EMAILDISPLAYDIALOG_ATTACHMENTS,_GetInst()}}
oDCAttachments:HyperLabel := HyperLabel{#Attachments,NULL_STRING,NULL_STRING,NULL_STRING}
oDCAttachments:ContextMenu := SaveAttachmentsContextMenu{}

oDCWebBrowser := WebBrowser{SELF,ResourceID{EMAILDISPLAYDIALOG_WEBBROWSER,_GetInst()}}
oDCWebBrowser:HyperLabel := HyperLabel{#WebBrowser,NULL_STRING,NULL_STRING,NULL_STRING}

SELF:Caption := ""
SELF:HyperLabel := HyperLabel{#EmailDisplayDialog,NULL_STRING,NULL_STRING,NULL_STRING}

if !IsNil(oServer)
	SELF:Use(oServer)
ENDIF

self:PostInit(oWindow,iCtlID,oServer,uExtra)

return self


METHOD MailForward() 

	SELF:EndWindow()
	SELF:oCaller:MailForward()

	RETURN SELF


METHOD MailNewFrom() 

	SELF:EndWindow()
	SELF:oCaller:MailNewFrom()

	RETURN SELF


METHOD MailReply() 
	
	SELF:EndWindow()
	SELF:oCaller:MailReply()

	RETURN SELF

METHOD MailReplyAll() 
	
	SELF:EndWindow()
	SELF:oCaller:MailReplyAll()

	RETURN SELF

METHOD MenuCommand(oMenuCommandEvent) 
   LOCAL dwItemID AS DWORD
	
	dwItemID := oMenuCommandEvent:ItemID
	
	DO CASE
	CASE dwItemID = EmailDisplayToolBar_Reply_ID	
		SELF:ReplyMenu()
		
	CASE dwItemID ==  IDM_ReplyMenu_Reply_Reply_to_sender_ID //	Reply only to sender
		SELF:MailReply()
		
	CASE dwItemID ==  IDM_ReplyMenu_Reply_Reply_to_all_ID //	Reply to all addressees list in the email
		SELF:MailReplyAll()	
		
	CASE dwItemID = EmailDisplayToolBar_Forward_ID	//	Forward/New to as yet unselected addressee
	   IF SELF:oCaller:MailBox = SENTBOX
	      SELF:MailNewFrom()
	   ELSE
		   SELF:MailForward()
		ENDIF
		
	CASE dwItemID = EmailDisplayToolBar_Send_ID		//	Send email
		SELF:SendMail()
		
	CASE dwItemID = EmailDisplayToolBar_Attach_ID	// add an attachment
		SELF:AttachButton()	
		
	CASE dwItemID = EmailDisplayToolBar_Print_ID	//	Print out email
		SELF:PrintMail()
		
	CASE dwItemID = EmailDisplayToolBar_Preview_ID	//	Preview email	
		SELF:PreviewMail()
		
	CASE dwItemID = EmailDisplayToolBar_Delete_ID	//	Delete the email currently selected
		SELF:DeleteMail()

	CASE dwItemID = EmailDisplayToolBar_Address_ID	//	Goto address book
		SELF:AddressBook()
		
	CASE dwItemID = EmailDisplayToolBar_Close_ID	//	Close display window
		SELF:EndWindow()

	CASE dwItemID = EmailDisplayToolBar_Back_Page_ID	//	Close display window
		SELF:oDCWebBrowser:HTMLPageGoBack()
		
	CASE dwItemID = EmailDisplayToolBar_Fwd_Page_ID	//	Close display window
		SELF:oDCWebBrowser:HTMLPageGoForward()
		
	ENDCASE

	RETURN NIL


METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 
	LOCAL nRecno AS INT 
	
	ogOpenWindows:Register(SELF)
	
	SELF:oEmail			:= uExtra[1]
	SELF:oCaller      := uExtra[2]  // the calling window FOR Send/Receive
	
	oAttachControl := oDCAttach_FT
	oAttachments   := oDCAttachments
	oBody          := oDCWebBrowser

	SELF:HyperLabel 	:= HyperLabel{#EmailDisplayDialog}
	SELF:Icon			:= Aap_Email_Icon{}
	SELF:Caption		:= " Email Client"
	
	SELF:EnableStatusBar(TRUE)
	SELF:StatusBar:DisplayMessage()
	SELF:StatusBar:DisplayTime()
	
	_dwFileCount := SELF:oEmail:AttachmentCount
	
	SELF:SetToolbar()
		
	SELF:MinSize := SELF:Size
	
	SELF:oLVI := SELF:oCaller:oDCEmailListView:GetSelectedItem()		// store it for deletion purposes  
	IF oLVI != NULL_OBJECT
   	nRecno := oLVI:GetValue(#E_DATE)
   	SELF:oServer := EmailStore{}
   	SELF:oServer:SetOrder("BOXDATE")
   	SELF:oServer:Goto(nRecno) 
	ENDIF 	
	
	SELF:oDCFromText:@@Value := SELF:oEmail:From
	
	SELF:oDCDateText:@@Value := DToC(SELF:oEmail:MailDate)
	
	SELF:oDCToMLE:ReadOnly := TRUE
	SELF:oDCToMLE:SetExStyle(WS_EX_CLIENTEDGE, FALSE)
	SELF:oDCCcMLE:ReadOnly := TRUE
	SELF:oDCCcMLE:SetExStyle(WS_EX_CLIENTEDGE, FALSE)
	
	SELF:oDCToMLE:@@Value  := __Array2StrList(SELF:oEmail:DestList, ", ")
	SELF:oDCCcMLE:@@Value  := __Array2StrList(SELF:oEmail:CCList, ", ")

	SELF:oDCSubjectText:@@Value := SELF:oEmail:Subject 
	
	IF SLen(oEmail:BodyHtml) > 0
	   SELF:oDCWebBrowser:Display(SELF:oEmail:BodyHtml, TRUE)
	ELSEIF SLen(oEmail:Body) > 0
	   SELF:oDCWebBrowser:Display(SELF:oEmail:Body, FALSE)
	ENDIF
	
	//SELF:oDCWebBrowser:Display(SELF:oEmail:HtmlText, TRUE)	// pass in the current email object
	
	oDCAttachments:AddColumn(ListViewColumn{200, HyperLabel{#FilesAttached, "Attached Files"}})
	oDCAttachments:SetStyle(WS_BORDER, FALSE)
	oDCAttachments:SetExStyle(WS_EX_CLIENTEDGE, FALSE)

	SELF:SetAttachments()
	
	SELF:Size := Dimension{640, 350}

	RETURN NIL


METHOD PreviewMail() 
	
	SELF:oDCWebBrowser:PrintPreview()
	
   RETURN SELF

METHOD PrintMail() 
	AltD()
	SELF:oDCWebBrowser:Print()
   RETURN SELF

METHOD ReplyMenu() 

   IF oReplyMenu = Null_Object
      oReplyMenu := ReplyMenu{SELF}
   ENDIF

   ((Toolbar) SELF:ToolBar):ShowButtonMenu(EmailDisplayToolBar_Reply_ID, SELF:oReplyMenu)
   RETURN SELF

METHOD SendMail() 
   LOCAL oShell  AS EmailWindowMain
   LOCAL dwRecno AS DWORD

   oShell  := SELF:oCaller
   dwRecno := SELF:oServer:Recno
   
   IF ogINetDial:Verifyconnection()
	   oShell:SendEMails(dwRecno)
		ogINetDial:HangUp()
   ENDIF	 
   
	RETURN SELF

METHOD SetToolbar() 

	LOCAL oTB AS ToolBar

	//	oTB := ToolBar{,,,,FALSE }
	oTB := ToolBar{}

	oTB:ButtonStyle := TB_TEXTANDICON //TB_ICONONLY //

	IF SELF:oCaller:MailBox = SENTBOX
	   oTB:AppendItem(IDT_GOMACRO,EmailDisplayToolBar_Forward_ID,,,"New")
	   oTB:AddTipText(IDT_GOMACRO,EmailDisplayToolBar_Forward_ID, "New email")
	ELSEIF SELF:oCaller:MailBox = OUTBOX
      oTB:AppendItem(IDT_MAIL,EmailDisplayToolBar_Send_ID,,,"Send")
	   oTB:AddTipText(IDT_MAIL,EmailDisplayToolBar_Send_ID, "Send Email")
	ELSE
		// don't allow these for this type of email box
		oTB:AppendItem(IDT_SCROLLPLUS,EmailDisplayToolBar_Reply_ID,,,"Reply")
		oTB:AddTipText(IDT_SCROLLPLUS,EmailDisplayToolBar_Reply_ID,"Reply to sender/all")

		oTB:AppendItem(IDT_GOMACRO,EmailDisplayToolBar_Forward_ID,,,"Fwd")
		oTB:AddTipText(IDT_GOMACRO,EmailDisplayToolBar_Forward_ID,"Forward message")
	ENDIF

	oTB:AppendItem(IDT_SEPARATOR)
	
	oTB:AppendItem(IDT_DELETE,EmailDisplayToolBar_Delete_ID,,,"Del")
	oTB:AddTipText(IDT_DELETE,EmailDisplayToolBar_Delete_ID,"Delete Email")
	
	IF _dwFileCount > 0
	   oTB:AppendItem(IDT_FUNCTION,EmailDisplayToolBar_Attach_ID,,4,"Attachments")
	   oTB:AddTipText(IDT_FUNCTION,EmailDisplayToolBar_Attach_ID,"Open, Save Attachments")
	ENDIF
	
	oTB:AppendItem(IDT_SEPARATOR)
	
	oTB:AppendItem(IDT_PREVREC,EmailDisplayToolBar_Back_Page_ID,,,"Back Pge")
	oTB:AddTipText(IDT_PREVREC,EmailDisplayToolBar_Back_Page_ID,"Previous Internet Page")

	oTB:AppendItem(IDT_NEXTREC,EmailDisplayToolBar_Fwd_Page_ID,,,"Fwd Page")
	oTB:AddTipText(IDT_NEXTREC,EmailDisplayToolBar_Fwd_Page_ID,"Next Internet Page")

   oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_PRINT,EmailDisplayToolBar_Print_ID,,,"Print")
	oTB:AddTipText(IDT_PRINT,EmailDisplayToolBar_Print_ID,"Print Email")
	
	oTB:AppendItem(IDT_PREVIEW,EmailDisplayToolBar_Preview_ID,,,"Preview")
	oTB:AddTipText(IDT_PREVIEW,EmailDisplayToolBar_Preview_ID,"Preview Email")

	oTB:AppendItem(IDT_SEPARATOR)

	oTB:AppendItem(IDT_CLOSE,EmailDisplayToolBar_Close_ID,,,"Close")
	oTB:AddTipText(IDT_CLOSE,EmailDisplayToolBar_Close_ID,"Close Email")

	oTB:Flat := TRUE

	SELF:ToolBar := oTB

	RETURN SELF


END CLASS
