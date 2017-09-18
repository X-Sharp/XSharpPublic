#region DEFINES
STATIC DEFINE ATTACHMENTSDIALOG_SAVEBUTTON := 100
STATIC DEFINE ATTACHMENTSDIALOG_OPENBUTTON := 101
STATIC DEFINE ATTACHMENTSDIALOG_SELECTALLBUTTON := 102
STATIC DEFINE ATTACHMENTSDIALOG_CLOSEBUTTON := 103
STATIC DEFINE ATTACHMENTSDIALOG_LOCATION := 104
STATIC DEFINE ATTACHMENTSDIALOG_ATTACHMENTS := 105
STATIC DEFINE ATTACHMENTSDIALOG_FIXEDTEXT1 := 106
STATIC DEFINE ATTACHMENTSDIALOG_SELECTDIR := 107
STATIC DEFINE ATTACHMENTASKDIALOG_USERCHOICE := 100
STATIC DEFINE ATTACHMENTASKDIALOG_OPENBUTTON := 101
STATIC DEFINE ATTACHMENTASKDIALOG_SAVEBUTTON := 102
STATIC DEFINE ATTACHMENTASKDIALOG_MESSAGETEXT := 103
STATIC DEFINE ATTACHMENTASKDIALOG_CLOSEBUTTON := 104
STATIC DEFINE ATTACHMENTASKDIALOG_PROCEEDBUTTON := 105
STATIC DEFINE ATTACHMENTASKDIALOG_GROUPBOX1 := 106
STATIC DEFINE ATTACHMENTASKDIALOG_VIRUSWARNING := 107
STATIC DEFINE ATTACHMENTASKDIALOG_ATTACHMENTNAME := 108
#endregion

CLASS AttachmentsDialog INHERIT DIALOGWINDOW
	PROTECT oCCSaveButton AS PUSHBUTTON
	PROTECT oCCOpenButton AS PUSHBUTTON
	PROTECT oCCSelectAllButton AS PUSHBUTTON
	PROTECT oCCCloseButton AS PUSHBUTTON
	PROTECT oDCLocation AS SINGLELINEEDIT
	PROTECT oDCAttachments AS AttachmentListView
	PROTECT oDCFixedText1 AS FIXEDTEXT
	PROTECT oCCSelectDir AS PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  
   PROTECT oEmail AS CEmail

CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"AttachmentsDialog" , _GetInst()} , TRUE)

	SELF:oCCSaveButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTSDIALOG_SAVEBUTTON  , _GetInst() } }
	SELF:oCCSaveButton:HyperLabel := HyperLabel{#SaveButton , "&Save" , NULL_STRING , NULL_STRING}

	SELF:oCCOpenButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTSDIALOG_OPENBUTTON  , _GetInst() } }
	SELF:oCCOpenButton:HyperLabel := HyperLabel{#OpenButton , "&Open" , NULL_STRING , NULL_STRING}

	SELF:oCCSelectAllButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTSDIALOG_SELECTALLBUTTON  , _GetInst() } }
	SELF:oCCSelectAllButton:HyperLabel := HyperLabel{#SelectAllButton , "&Select All" , NULL_STRING , NULL_STRING}

	SELF:oCCCloseButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTSDIALOG_CLOSEBUTTON  , _GetInst() } }
	SELF:oCCCloseButton:HyperLabel := HyperLabel{#CloseButton , "&Close" , NULL_STRING , NULL_STRING}

	SELF:oDCLocation := SINGLELINEEDIT{SELF , ResourceID{ ATTACHMENTSDIALOG_LOCATION  , _GetInst() } }
	SELF:oDCLocation:HyperLabel := HyperLabel{#Location , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCAttachments := AttachmentListView{SELF , ResourceID{ ATTACHMENTSDIALOG_ATTACHMENTS  , _GetInst() } }
	SELF:oDCAttachments:ContextMenu := SaveAttachmentsContextMenu{}
	SELF:oDCAttachments:HyperLabel := HyperLabel{#Attachments , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText1 := FIXEDTEXT{SELF , ResourceID{ ATTACHMENTSDIALOG_FIXEDTEXT1  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Underline := TRUE
	SELF:oDCFixedText1:Font( oFont )
	SELF:oDCFixedText1:HyperLabel := HyperLabel{#FixedText1 , "Save to:" , NULL_STRING , NULL_STRING}

	SELF:oCCSelectDir := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTSDIALOG_SELECTDIR  , _GetInst() } }
	SELF:oCCSelectDir:HyperLabel := HyperLabel{#SelectDir , "..." , NULL_STRING , NULL_STRING}

	SELF:Caption := "Save Attachments"
	SELF:Icon := AAP_EMAIL_ICON{}
	SELF:HyperLabel := HyperLabel{#AttachmentsDialog , "Save Attachments" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent) 

	SELF:OpenButton()	

	RETURN NIL


METHOD OpenButton( ) 

	oDCAttachments:Open(SELF:oEmail)
   RETURN SELF

METHOD PostInit(oParent,uExtra) 
	LOCAL cFolder AS STRING
	
	SELF:oEmail := uExtra	// the email object	
	
	SELF:oDCAttachments:AddColumn(ListViewColumn{40, HyperLabel{#FilesAttached, "Attached Files"}})
	
   SELF:oDCAttachments:Fill(SELF:oEmail)
	
	SELF:oDCLocation:FieldSpec := FieldSpec{#CHAR100, "C", 100, 0}
	
	cFolder := aMailInfo[DEF_ATTACHPATH]
	IF Right(cFolder, 1) = "\"
		cFolder := Left(cFolder, SLen(cFolder)-1)
	ENDIF

	SELF:oDCLocation:Value := cFolder
	
	RETURN NIL


METHOD SelectAllButton( ) 
	
	oDCAttachments:SelectAll()
   RETURN SELF

METHOD CloseButton( ) 
	SELF:EndDialog()
   RETURN SELF

METHOD SaveButton() 

   oDCAttachments:Save(SELF:oEmail, Trim(SELF:oDCLocation:TextValue))
   RETURN SELF

METHOD SelectDir( ) 
   LOCAL oFolderDLG   AS StandardFolderDialog 
   LOCAL cStartFolder AS STRING

	IF Empty(oDCLocation:TextValue)
		cStartFolder := DiskName()+":\"
	ELSE
		cStartFolder := oDCLocation:TextValue
	ENDIF

	oFolderDLG := StandardFolderDialog{SELF,"Select Attachment Folder",Trim(cStartFolder),BIF_RETURNONLYFSDIRS}
	oFolderDLG:show()
	cStartFolder :=  oFolderDLG:FolderName
	IF ! Empty(cStartFolder)
		oDCLocation:TextValue := cStartFolder
	ENDIF
	oDCLocation:SetFocus()

   RETURN SELF


END CLASS
CLASS AttachmentAskDialog INHERIT DIALOGWINDOW
	PROTECT oDCUserChoice AS RADIOBUTTONGROUP
	PROTECT oCCOpenButton AS RADIOBUTTON
	PROTECT oCCSaveButton AS RADIOBUTTON
	PROTECT oDCMessageText AS FIXEDTEXT
	PROTECT oCCCloseButton AS PUSHBUTTON
	PROTECT oCCProceedButton AS PUSHBUTTON
	PROTECT oDCGroupBox1 AS GROUPBOX
	PROTECT oDCVirusWarning AS CHECKBOX
	PROTECT oDCAttachmentName AS FIXEDTEXT

	// {{%UC%}} User code starts here (DO NOT remove this line)  

  	EXPORT cResult AS STRING


METHOD PostInit(oParent,uExtra) 

	SUPER:PostInit()
	
	SELF:oDCAttachmentName:Caption := Trim(uExtra)
	
	SELF:oDCMessageText:Caption := "VIRUS WARNING - You need to be aware that attachments can carrry "
	SELF:oDCMessageText:Caption += "viruses. Even htm pages, Word documents and Excel spreadsheets "
	SELF:oDCMessageText:Caption += "can contain macros which cause implications. If you are in doubt "
	SELF:oDCMessageText:Caption += "about the origin of this attachment, your best course of action "
	SELF:oDCMessageText:Caption += "is to save the file to disk and scan the saved file for viruses."
	
	SELF:oDCVirusWarning:Checked := TRUE
	
	SELF:oCCProceedButton:Disable()		// don't enable until a choice is made

	RETURN NIL


CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"AttachmentAskDialog" , _GetInst()} , TRUE)

	SELF:oDCUserChoice := RADIOBUTTONGROUP{SELF , ResourceID{ ATTACHMENTASKDIALOG_USERCHOICE  , _GetInst() } }
	SELF:oDCUserChoice:TextColor := Color{ 0 , 128 , 0 }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCUserChoice:Font( oFont )
	SELF:oDCUserChoice:HyperLabel := HyperLabel{#UserChoice , "Select what you wish to do:" , NULL_STRING , NULL_STRING}

	SELF:oCCOpenButton := RADIOBUTTON{SELF , ResourceID{ ATTACHMENTASKDIALOG_OPENBUTTON  , _GetInst() } }
	SELF:oCCOpenButton:HyperLabel := HyperLabel{#OpenButton , "Open this attachment - I accept the risk of viruses." , NULL_STRING , NULL_STRING}

	SELF:oCCSaveButton := RADIOBUTTON{SELF , ResourceID{ ATTACHMENTASKDIALOG_SAVEBUTTON  , _GetInst() } }
	SELF:oCCSaveButton:HyperLabel := HyperLabel{#SaveButton , "Save this attachment to another location." , NULL_STRING , NULL_STRING}

	SELF:oDCMessageText := FIXEDTEXT{SELF , ResourceID{ ATTACHMENTASKDIALOG_MESSAGETEXT  , _GetInst() } }
	SELF:oDCMessageText:TextColor := Color{ COLORRED }
	SELF:oDCMessageText:HyperLabel := HyperLabel{#MessageText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oCCCloseButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTASKDIALOG_CLOSEBUTTON  , _GetInst() } }
	SELF:oCCCloseButton:HyperLabel := HyperLabel{#CloseButton , "&Abort" , NULL_STRING , NULL_STRING}

	SELF:oCCProceedButton := PUSHBUTTON{SELF , ResourceID{ ATTACHMENTASKDIALOG_PROCEEDBUTTON  , _GetInst() } }
	SELF:oCCProceedButton:HyperLabel := HyperLabel{#ProceedButton , "&Proceed" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox1 := GROUPBOX{SELF , ResourceID{ ATTACHMENTASKDIALOG_GROUPBOX1  , _GetInst() } }
	SELF:oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCVirusWarning := CHECKBOX{SELF , ResourceID{ ATTACHMENTASKDIALOG_VIRUSWARNING  , _GetInst() } }
	SELF:oDCVirusWarning:HyperLabel := HyperLabel{#VirusWarning , "Show this warning always" , NULL_STRING , NULL_STRING}

	SELF:oDCAttachmentName := FIXEDTEXT{SELF , ResourceID{ ATTACHMENTASKDIALOG_ATTACHMENTNAME  , _GetInst() } }
	SELF:oDCAttachmentName:HyperLabel := HyperLabel{#AttachmentName , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCUserChoice:FillUsing({ ;
										{SELF:oCCOpenButton, "O"}, ;
										{SELF:oCCSaveButton, "S"} ;
										})

	SELF:Caption := "Attachment Virus Warning"
	SELF:Icon := AAP_EMAIL_ICON{}
	SELF:HyperLabel := HyperLabel{#AttachmentAskDialog , "Attachment Virus Warning" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD ProceedButton( ) 
	
	SELF:cResult := SELF:oDCUserChoice:Value
	SELF:EndDialog()

	RETURN SELF


METHOD CloseButton( ) 
	
	SELF:EndDialog()
	
   RETURN SELF

METHOD ButtonClick(oControlEvent) 

	LOCAL oControl AS Control

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)

	SUPER:ButtonClick(oControlEvent)

	IF IsInstanceOfUsual(oControl, #RadioButton)
		SELF:oCCProceedButton:Enable()
	ENDIF

	RETURN NIL


END CLASS
