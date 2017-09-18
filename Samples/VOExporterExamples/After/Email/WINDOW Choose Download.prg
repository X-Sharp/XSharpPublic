#region DEFINES
STATIC DEFINE CHOOSEDOWNLOADDIALOG_SELECTIONOPTIONS := 100
STATIC DEFINE CHOOSEDOWNLOADDIALOG_DELETEBUTTON := 101
STATIC DEFINE CHOOSEDOWNLOADDIALOG_DOWNLOADBUTTON := 102
STATIC DEFINE CHOOSEDOWNLOADDIALOG_OKBUTTON := 103
STATIC DEFINE CHOOSEDOWNLOADDIALOG_CLOSEBUTTON := 104
STATIC DEFINE CHOOSEDOWNLOADDIALOG_DELETEDOWNLOADEDMAIL := 105
STATIC DEFINE CHOOSEDOWNLOADDIALOG_LISTVIEW1 := 106
STATIC DEFINE CHOOSEDOWNLOADDIALOG_REPROCESSHEADERS := 107
STATIC DEFINE CHOOSEDOWNLOADDIALOG_SELECTALLBUTTON := 108
STATIC DEFINE CHOOSEDOWNLOADDIALOG_UNSELECTALLBUTTON := 109
STATIC DEFINE CHOOSEDOWNLOADDIALOG_INVERTSELECTIONBUTTON := 110
STATIC DEFINE CHOOSEDOWNLOADDIALOG_TESTMAILIDBUTTON := 111
#endregion

CLASS ChooseDownloadDialog INHERIT DIALOGWINDOW
	PROTECT oDCSelectionOptions AS RADIOBUTTONGROUP
	PROTECT oCCDeleteButton AS RADIOBUTTON
	PROTECT oCCDownloadButton AS RADIOBUTTON
	PROTECT oCCOKButton AS PUSHBUTTON
	PROTECT oCCCloseButton AS PUSHBUTTON
	PROTECT oDCDeleteDownloadedMail AS CHECKBOX
	PROTECT oDCListView1 AS LISTVIEW
	PROTECT oCCReprocessHeaders AS PUSHBUTTON
	PROTECT oCCSelectAllButton AS PUSHBUTTON
	PROTECT oCCUnSelectAllButton AS PUSHBUTTON
	PROTECT oCCInvertSelectionButton AS PUSHBUTTON
	PROTECT oCCTestMailIDButton AS PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  

  	PROTECT lSetUpComplete AS LOGIC		// used to prevent resizing messages when no controls available
  	PROTECT lInResize AS LOGIC			// prevent recursive calls to our resize method
  	PROTECT oOriginalSize AS Dimension	// minimum size for the window
  	PROTECT oLVOrigin AS Point			// original position for Listview


METHOD CloseButton( ) 
	
	SELF:EndDialog()
	
	RETURN SELF

CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"ChooseDownloadDialog" , _GetInst()} , TRUE)

	SELF:oDCSelectionOptions := RADIOBUTTONGROUP{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_SELECTIONOPTIONS  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCSelectionOptions:Font( oFont )
	SELF:oDCSelectionOptions:HyperLabel := HyperLabel{#SelectionOptions , "Selection Option:" , NULL_STRING , NULL_STRING}

	SELF:oCCDeleteButton := RADIOBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_DELETEBUTTON  , _GetInst() } }
	SELF:oCCDeleteButton:HyperLabel := HyperLabel{#DeleteButton , "Delete Selections" , NULL_STRING , NULL_STRING}

	SELF:oCCDownloadButton := RADIOBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_DOWNLOADBUTTON  , _GetInst() } }
	SELF:oCCDownloadButton:HyperLabel := HyperLabel{#DownloadButton , "Download Selections" , NULL_STRING , NULL_STRING}

	SELF:oCCOKButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_OKBUTTON  , _GetInst() } }
	oFont := Font{  , 10 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oCCOKButton:Font( oFont )
	SELF:oCCOKButton:OwnerAlignment := OA_X
	SELF:oCCOKButton:HyperLabel := HyperLabel{#OKButton , "&Process" , NULL_STRING , NULL_STRING}

	SELF:oCCCloseButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_CLOSEBUTTON  , _GetInst() } }
	SELF:oCCCloseButton:OwnerAlignment := OA_X
	SELF:oCCCloseButton:HyperLabel := HyperLabel{#CloseButton , "&Abort" , NULL_STRING , NULL_STRING}

	SELF:oDCDeleteDownloadedMail := CHECKBOX{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_DELETEDOWNLOADEDMAIL  , _GetInst() } }
	SELF:oDCDeleteDownloadedMail:HyperLabel := HyperLabel{#DeleteDownloadedMail , "&Delete Downloaded Emails" , NULL_STRING , NULL_STRING}

	SELF:oDCListView1 := LISTVIEW{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_LISTVIEW1  , _GetInst() } }
	SELF:oDCListView1:OwnerAlignment := OA_WIDTH_HEIGHT
	SELF:oDCListView1:FullRowSelect := True
	SELF:oDCListView1:CheckBoxes := False
	SELF:oDCListView1:HyperLabel := HyperLabel{#ListView1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCReprocessHeaders := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_REPROCESSHEADERS  , _GetInst() } }
	SELF:oCCReprocessHeaders:OwnerAlignment := OA_X
	SELF:oCCReprocessHeaders:HyperLabel := HyperLabel{#ReprocessHeaders , "&Get Headers" , NULL_STRING , NULL_STRING}

	SELF:oCCSelectAllButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_SELECTALLBUTTON  , _GetInst() } }
	SELF:oCCSelectAllButton:HyperLabel := HyperLabel{#SelectAllButton , "&Select All Headers" , NULL_STRING , NULL_STRING}

	SELF:oCCUnSelectAllButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_UNSELECTALLBUTTON  , _GetInst() } }
	SELF:oCCUnSelectAllButton:HyperLabel := HyperLabel{#UnSelectAllButton , "&UnSelect All Headers" , NULL_STRING , NULL_STRING}

	SELF:oCCInvertSelectionButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_INVERTSELECTIONBUTTON  , _GetInst() } }
	SELF:oCCInvertSelectionButton:HyperLabel := HyperLabel{#InvertSelectionButton , "&Invert Headers Selection" , NULL_STRING , NULL_STRING}

	SELF:oCCTestMailIDButton := PUSHBUTTON{SELF , ResourceID{ CHOOSEDOWNLOADDIALOG_TESTMAILIDBUTTON  , _GetInst() } }
	SELF:oCCTestMailIDButton:HyperLabel := HyperLabel{#TestMailIDButton , "Test Mail IDs" , NULL_STRING , NULL_STRING}

	SELF:oDCSelectionOptions:FillUsing({ ;
										{SELF:oCCDeleteButton, "DELETE"}, ;
										{SELF:oCCDownloadButton, "DOWNLOAD"} ;
										})

	SELF:Caption := "Choose Mail to Download or Delete"
	SELF:HyperLabel := HyperLabel{#ChooseDownloadDialog , "Choose Mail to Download or Delete" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD PostInit(oParent,uExtra) 

	LOCAL oImageList AS ImageList

	// Set up ListView
	oImageList	:= ImageList{ 2, Dimension{ 16,16 } }
	oImageList:Add( Box_Icon{} )
	oImageList:Add( BoxTick_Icon{} )
	SELF:oDCListView1:SmallImageList := oImageList

	// set up Listview columns
	SELF:oDCListView1:AddColumn(ListViewColumn{10, HyperLabel{#MailDate, "Date"}})
	SELF:oDCListView1:AddColumn(ListViewColumn{6, HyperLabel{#MailTime, "Time"}})
	SELF:oDCListView1:AddColumn(ListViewColumn{20, HyperLabel{#MailSender, "Sender"}})
	SELF:oDCListView1:AddColumn(ListViewColumn{25, HyperLabel{#MailSubject, "Subject"}})
	SELF:oDCListView1:AddColumn(ListViewColumn{7, HyperLabel{#MailSize, "Size"},,LVCFMT_RIGHT})
	SELF:oDCListView1:AddColumn(ListViewColumn{0, HyperLabel{#MailNum, "Number"}})

	// set up download option
	SELF:oDCDeleteDownloadedMail:Checked := aMailInfo[DEF_DELETEMAIL]
	SELF:oDCSelectionOptions:Value := "DOWNLOAD"

	// allow resizing events from now
	SELF:lSetupComplete := TRUE
	SELF:oOriginalSize := SELF:Size
	SELF:oLVOrigin := SELF:oDCListView1:Origin

	RETURN NIL

METHOD ListViewMouseButtonDown(oListViewMouseEvent) 

	LOCAL oListViewItem	AS ListViewItem

	SUPER:ListViewMouseButtonDown( oListViewMouseEvent )

	IF oListViewMouseEvent  <> NULL_OBJECT
		oListViewItem := oListViewMouseEvent:ListViewItem

		IF oListViewItem <> NULL_OBJECT
			IF oListViewItem:ImageIndex == 1
				oListViewItem:ImageIndex := 2
			ELSE
				oListViewItem:ImageIndex := 1
			ENDIF
			SELF:oDCListView1:SetItemAttributes( oListViewItem )
		ENDIF

	ENDIF

	RETURN NIL



METHOD Show(kShowState) 
	
	SELF:Size := SELF:Size	// cause one resize event to get things stable
	
	SUPER:Show(kShowState)
	
	RETURN NIL

METHOD ButtonClick(oControlEvent) 

	LOCAL oControl AS OBJECT

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)
	SUPER:ButtonClick(oControlEvent)

	IF IsInstanceOf(oControl, #RadioButton)
		IF SELF:oDCSelectionOptions:Value = "DOWNLOAD"
			SELF:oDCDeleteDownloadedMail:Enable()
		ELSE
			SELF:oDCDeleteDownloadedMail:Disable()
		ENDIF
	ENDIF

	RETURN NIL


METHOD AddListViewItem(oMessage, nMail, cMailSize) 
	LOCAL oItem AS ListViewItem
	
	oItem := ListViewItem{}
	oItem:SetText(DToC(oMessage:MailDate),  #MailDate)
	oItem:SetText(oMessage:MailTime,        #MailTime)
	oItem:SetText(oMessage:From,            #MailSender)
	oItem:SetText(oMessage:Subject,         #MailSubject)
	oItem:SetValue(nMail,                   #MailNum)
	oItem:SetText(cMailSize,                #MailSize)
	oItem:ImageIndex  := 1
	
	SELF:oDCListView1:AddItem(oItem)

	RETURN SELF	


METHOD PostShowDialog() 

	LOCAL oPop AS MyPop
	LOCAL nList, nMail AS DWORD
	LOCAL aHeaders, aTop AS ARRAY
	LOCAL oProgWin AS ProgressWindow
	LOCAL cTitle, cMessage AS STRING
	LOCAL oMessage AS cMessage

	oProgWin := ProgressWindow{SELF, "Downloading Email Headers", "Logging on to mail server..."}
	oProgWin:AVIResource := "WebReceiveAVI"
	oProgWin:Show()

	oPop := MyPop{SELF:Owner, aMailInfo[DEF_POPSERVER]}
	IF oPop:Logon(aMailInfo[DEF_ACCOUNT],aMailInfo[DEF_PASSWORD])
		oPop:GetStatus()
		// construct an array of mail numbers
		oProgWin:Message("Obtain initial list of headers...")
		
		aHeaders := oPop:GetList()	// aHeaders = {...{Mail No., Mail size}...}
		nList := ALen(aHeaders)
		oProgWin:Count := nList
		
		oMessage := CMessage{}
		
		FOR nMail := 1 UPTO nList
			oProgWin:Message("Downloading " + NTrim(nMail) + " of " + NTrim(nList))
			aTop := oPop:GetMailTop(aHeaders[nMail,1], 1)
			IF ! aTop[1] == Null_String
            oMessage:MailHeader := aTop[1]
            oMessage:GetHeaderInfo()
			
			   SELF:AddListviewItem(oMessage, nMail, aHeaders[nMail,2])
			ENDIF

			oProgWin:StepIt(1)
			IF oProgWin:Cancel
				cTitle := "Download In-Complete"
				cMessage := "User aborted process part way through"
				EXIT
			ENDIF
		NEXT
		IF nList = 0
			cTitle := "Download Complete"
			cMessage := "There was no mail to collect"
		ENDIF
	ELSE
		cTitle := "Download Failure"
		cMessage := "Failed to log on to a server - TRY AGAIN"
	ENDIF
	oPop:Disconnect()
	oPop:Close()
	
	oProgWin:EndDialog()

	IF !Empty(cMessage)
		Textbox{,cTitle,cMessage}:Show()
	ENDIF

	RETURN SELF


METHOD ReprocessHeaders( ) 
	
	SELF:oDCListView1:DeleteAll()
	ApplicationExec(EXECWHILEEVENT)
	SELF:PostShowDialog()
	
	RETURN SELF

METHOD OKButton() 
	LOCAL oListViewItem	AS ListViewItem
	LOCAL dwTotal, dwMail AS DWORD
	LOCAL dwTotalSize, dwSize AS DWORD
	LOCAL dwPos AS DWORD
	LOCAL cMessage AS STRING
	LOCAL oPop AS MyPop
	LOCAL aList AS ARRAY
	LOCAL lAborted, lNoQuit	AS LOGIC
	LOCAL oProgWin AS ProgressWindow
	
	IF Empty(SELF:oDCSelectionOptions:Value)
		MessageBox(NULL_PTR, PSZ("Please select a download mode"), PSZ("Missing Information"), MB_ICONSTOP+MB_OK)
		RETURN NIL
	ENDIF

	IF SELF:oDCSelectionOptions:Value = "DOWNLOAD"	
		cMessage := "Download selected headers ?"
	ELSE
		cMessage := "Delete selected headers ?"
	ENDIF

	IF MessageBox(NULL_PTR, PSZ(cMessage), PSZ("Process Selections"), MB_ICONINFORMATION+MB_YESNO) != IDYES
		RETURN NIL
	ENDIF

	// find all the selected items for action and build an array to send back
	aList := {}
	dwTotalSize := 0
	dwTotal	:= SELF:oDCListView1:ItemCount
	FOR dwMail := 1 UPTO dwTotal
		oListViewItem := SELF:oDCListView1:GetItemAttributes( dwMail )
		IF oListViewItem:ImageIndex = 2
		   dwSize := Val(oListViewItem:GetText(#MailSize))
			AAdd(aList, {dwMail, dwSize})
			dwTotalSize += dwSize
		ENDIF
	NEXT

	dwTotal := ALen(aList) 		// now get length of list to process
	IF dwTotal = 0
		ErrorBox{,"No records to process"}:Show()
		RETURN SELF
	ENDIF

	// reset the progress bar to process the selections
	oProgWin := ProgressWindow{SELF, "Processing Selected Mail", "Logging on to mail server..."}
	oProgWin:AVIResource := "WebReceiveAVI"
	oProgWin:Show()
	
	oPop := MyPop{SELF, aMailInfo[DEF_POPSERVER]}
	cMessage := ""
	IF oPop:Logon(aMailInfo[DEF_ACCOUNT],aMailInfo[DEF_PASSWORD])
		oPop:GetStatus()
		IF SELF:oDCSelectionOptions:Value = "DOWNLOAD"
		   oPop:Progress  := oProgWin
		   oProgWin:Count := dwTotalSize
			FOR dwMail := 1 UPTO dwTotal
				oProgWin:Message("Retrieving " + NTrim(dwMail) + " of " + NTrim(dwTotal))
				dwPos := oProgWin:Position
				IF oPop:GetMail(aList[dwMail, 1])
					oEmailServer:AddReceivedEmail(oPop:Email)
					oEmailServer:Commit()
				ELSE
				   oProgWin:Position := dwPos + aList[dwMail, 2]
					cMessage += "Email Download Failure - Email no longer available on server" + CRLF
				ENDIF
				
				IF oProgWin:Cancel
					cMessage += "User aborted Download" + CRLF
					lAborted := TRUE
					EXIT
				ENDIF
			NEXT dwMail
		   oPop:Progress := NULL_OBJECT
			// delete selected headers from server in reverse order (ie delete highest mail number first)
			IF SELF:oDCDeleteDownloadedMail:Checked
				IF lAborted
					// user stopped the process of download - only delete successful downloads
					dwTotal := dwMail-1
				ENDIF
				oProgWin:Count := dwTotal
				FOR dwMail := dwTotal DOWNTO 1
					oProgWin:Message("Deleting " + NTrim(dwMail) + " of " + NTrim(dwTotal))
					IF !oPop:DeleteMail(aList[dwMail,1])
						cMessage += "Email Deletion Failure - Could not delete the message from the server" + CRLF
					ENDIF
					oProgWin:StepIt()
				NEXT dwMail
			ENDIF
		ELSE
			// delete selected headers from server in reverse order (ie delete highest mail number first)
			// but let's display it as if its 1 - dwTotal to prevent confusion
			oProgWin:Count := dwTotal
			FOR dwMail := dwTotal DOWNTO 1
				oProgWin:Message("Deleting " + NTrim(dwTotal+1-dwMail) + " of " + NTrim(dwTotal))
				IF !oPop:DeleteMail(aList[dwMail, 1])
					cMessage += "Email Deletion Failure - Could not delete the message from the server" + CRLF
				ENDIF
				oProgWin:StepIt()
			NEXT dwMail
		ENDIF
	ELSE
		lNoQuit := TRUE
		cMessage := "Failed to log on to a server - TRY AGAIN"
	ENDIF

	oPop:Disconnect()
	oPop:Close()
	oProgWin:EndDialog()

	IF ! Empty(cMessage)
		Textbox{,"Email Process Failure", cMessage}:Show()
	ENDIF

	IF ! lNoQuit
		SELF:EndDialog()
	ENDIF

	RETURN NIL


METHOD SelectAllButton( ) 

	LOCAL oListViewItem AS ListViewItem
	LOCAL dwMail, dwTotal AS DWORD

	dwTotal	:= SELF:oDCListView1:ItemCount
	FOR dwMail := 1 UPTO dwTotal
		oListViewItem := SELF:oDCListView1:GetItemAttributes( dwMail )
		oListViewItem:ImageIndex := 2
		SELF:oDCListView1:SetItemAttributes(oListViewItem)
	NEXT

	RETURN SELF


METHOD UnSelectAllButton( ) 
	
	LOCAL oListViewItem AS ListViewItem
	LOCAL dwMail, dwTotal AS DWORD

	dwTotal	:= SELF:oDCListView1:ItemCount
	FOR dwMail := 1 UPTO dwTotal
		oListViewItem := SELF:oDCListView1:GetItemAttributes( dwMail )
		oListViewItem:ImageIndex := 1
		SELF:oDCListView1:SetItemAttributes(oListViewItem)
	NEXT

	RETURN SELF
	

METHOD InvertSelectionButton( ) 
	
	LOCAL oListViewItem AS ListViewItem
	LOCAL dwMail, dwTotal AS DWORD

	dwTotal	:= SELF:oDCListView1:ItemCount
	FOR dwMail := 1 UPTO dwTotal
		oListViewItem := SELF:oDCListView1:GetItemAttributes( dwMail )
		IF oListViewItem:ImageIndex = 2
			oListViewItem:ImageIndex := 1
		ELSE
			oListViewItem:ImageIndex := 2
		ENDIF
		SELF:oDCListView1:SetItemAttributes(oListViewItem)
	NEXT

	RETURN SELF
	

METHOD TestMailIDButton( ) 
	LOCAL oPop AS MyPop
	LOCAL nList, nMail AS DWORD
	LOCAL aIDs    AS ARRAY
	LOCAL oProgWin AS ProgressWindow
	LOCAL cTitle, cMessage AS STRING
	LOCAL cText   AS STRING

	oProgWin := ProgressWindow{SELF, "Downloading Email IDs", "Logging on to mail server..."}
	oProgWin:AVIResource := "WebReceiveAVI"
	oProgWin:Show()

	oPop := MyPop{SELF:Owner, aMailInfo[DEF_POPSERVER]}
	IF oPop:Logon(aMailInfo[DEF_ACCOUNT],aMailInfo[DEF_PASSWORD])
		oPop:GetStatus()
		// construct an array of mail numbers
		oProgWin:Message("Obtain initial list of IDs")
		aIDs := oPop:GetListIDs()	// aIDs = {...{Mail No., cID}...}
		nList := ALen(aIDs)
		oProgWin:Count := nList
		cText := "Mail-No.  Mail-ID" + CRLF
		cText += "-----------------------------" + CRLF
		FOR nMail := 1 UPTO nList
			cText += Str(aIDs[nMail,1], 4, 0) + Space(6) + aIDs[nMail,2] + CRLF
			oProgWin:StepIt(1)
		NEXT
		IF nList = 0
			cTitle := "Download Complete"
			cMessage := "There was no mail to review"
		ENDIF
	ELSE
		cTitle := "Download Failure"
		cMessage := "Failed to log on to a server - TRY AGAIN"
	ENDIF
	oPop:Disconnect()
	oPop:Close()
	oProgWin:EndDialog()

	IF Empty(cMessage)
	   MailIDsDialog{SELF, cText}:Show()
	ELSE
		Textbox{,cTitle,cMessage}:Show()
	ENDIF

	RETURN SELF


END CLASS
