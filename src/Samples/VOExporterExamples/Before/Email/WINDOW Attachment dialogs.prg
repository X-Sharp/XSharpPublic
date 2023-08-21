#region DEFINES
STATIC DEFINE ATTACHMENTASKDIALOG_ATTACHMENTNAME := 108 
STATIC DEFINE ATTACHMENTASKDIALOG_CLOSEBUTTON := 104 
STATIC DEFINE ATTACHMENTASKDIALOG_GROUPBOX1 := 106 
STATIC DEFINE ATTACHMENTASKDIALOG_MESSAGETEXT := 103 
STATIC DEFINE ATTACHMENTASKDIALOG_OPENBUTTON := 101 
STATIC DEFINE ATTACHMENTASKDIALOG_PROCEEDBUTTON := 105 
STATIC DEFINE ATTACHMENTASKDIALOG_SAVEBUTTON := 102 
STATIC DEFINE ATTACHMENTASKDIALOG_USERCHOICE := 100 
STATIC DEFINE ATTACHMENTASKDIALOG_VIRUSWARNING := 107 
STATIC DEFINE ATTACHMENTSDIALOG_ATTACHMENTS := 105 
STATIC DEFINE ATTACHMENTSDIALOG_CLOSEBUTTON := 103 
STATIC DEFINE ATTACHMENTSDIALOG_FIXEDTEXT1 := 106 
STATIC DEFINE ATTACHMENTSDIALOG_LOCATION := 104 
STATIC DEFINE ATTACHMENTSDIALOG_OPENBUTTON := 101 
STATIC DEFINE ATTACHMENTSDIALOG_SAVEBUTTON := 100 
STATIC DEFINE ATTACHMENTSDIALOG_SELECTALLBUTTON := 102 
STATIC DEFINE ATTACHMENTSDIALOG_SELECTDIR := 107 
#endregion

class AttachmentAskDialog inherit DIALOGWINDOW 

	protect oDCUserChoice as RADIOBUTTONGROUP
	protect oCCOpenButton as RADIOBUTTON
	protect oCCSaveButton as RADIOBUTTON
	protect oDCMessageText as FIXEDTEXT
	protect oCCCloseButton as PUSHBUTTON
	protect oCCProceedButton as PUSHBUTTON
	protect oDCGroupBox1 as GROUPBOX
	protect oDCVirusWarning as CHECKBOX
	protect oDCAttachmentName as FIXEDTEXT

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

  	EXPORT cResult AS STRING


METHOD ButtonClick(oControlEvent) 

	LOCAL oControl AS Control

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)

	SUPER:ButtonClick(oControlEvent)

	IF IsInstanceOfUsual(oControl, #RadioButton)
		SELF:oCCProceedButton:Enable()
	ENDIF

	RETURN NIL


METHOD CloseButton( ) 
	
	SELF:EndDialog()
	
   RETURN SELF

CONSTRUCTOR(oParent,uExtra)  
local dim aFonts[1] AS OBJECT

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"AttachmentAskDialog",_GetInst()},TRUE)

aFonts[1] := Font{,8,"Microsoft Sans Serif"}
aFonts[1]:Bold := TRUE

oCCOpenButton := RadioButton{self,ResourceID{ATTACHMENTASKDIALOG_OPENBUTTON,_GetInst()}}
oCCOpenButton:HyperLabel := HyperLabel{#OpenButton,"Open this attachment - I accept the risk of viruses.",NULL_STRING,NULL_STRING}

oCCSaveButton := RadioButton{self,ResourceID{ATTACHMENTASKDIALOG_SAVEBUTTON,_GetInst()}}
oCCSaveButton:HyperLabel := HyperLabel{#SaveButton,"Save this attachment to another location.",NULL_STRING,NULL_STRING}

oDCMessageText := FixedText{self,ResourceID{ATTACHMENTASKDIALOG_MESSAGETEXT,_GetInst()}}
oDCMessageText:HyperLabel := HyperLabel{#MessageText,"Fixed Text",NULL_STRING,NULL_STRING}
oDCMessageText:TextColor := Color{COLORRED}

oCCCloseButton := PushButton{self,ResourceID{ATTACHMENTASKDIALOG_CLOSEBUTTON,_GetInst()}}
oCCCloseButton:HyperLabel := HyperLabel{#CloseButton,_chr(38)+"Abort",NULL_STRING,NULL_STRING}

oCCProceedButton := PushButton{self,ResourceID{ATTACHMENTASKDIALOG_PROCEEDBUTTON,_GetInst()}}
oCCProceedButton:HyperLabel := HyperLabel{#ProceedButton,_chr(38)+"Proceed",NULL_STRING,NULL_STRING}

oDCGroupBox1 := GroupBox{self,ResourceID{ATTACHMENTASKDIALOG_GROUPBOX1,_GetInst()}}
oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1,NULL_STRING,NULL_STRING,NULL_STRING}

oDCVirusWarning := CheckBox{self,ResourceID{ATTACHMENTASKDIALOG_VIRUSWARNING,_GetInst()}}
oDCVirusWarning:HyperLabel := HyperLabel{#VirusWarning,"Show this warning always",NULL_STRING,NULL_STRING}

oDCAttachmentName := FixedText{self,ResourceID{ATTACHMENTASKDIALOG_ATTACHMENTNAME,_GetInst()}}
oDCAttachmentName:HyperLabel := HyperLabel{#AttachmentName,"Fixed Text",NULL_STRING,NULL_STRING}

oDCUserChoice := RadioButtonGroup{self,ResourceID{ATTACHMENTASKDIALOG_USERCHOICE,_GetInst()}}
oDCUserChoice:FillUsing({ ;
							{oCCOpenButton,"O"}, ;
							{oCCSaveButton,"S"} ;
							})
oDCUserChoice:HyperLabel := HyperLabel{#UserChoice,"Select what you wish to do:",NULL_STRING,NULL_STRING}
oDCUserChoice:TextColor := Color{0,128,0}
oDCUserChoice:Font(aFonts[1], FALSE)

self:Caption := "Attachment Virus Warning"
self:HyperLabel := HyperLabel{#AttachmentAskDialog,"Attachment Virus Warning",NULL_STRING,NULL_STRING}
self:Icon := AAP_EMAIL_ICON{}

self:PostInit(oParent,uExtra)

return self


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


METHOD ProceedButton( ) 
	
	SELF:cResult := SELF:oDCUserChoice:@@Value
	SELF:EndDialog()

	RETURN SELF


END CLASS
CLASS AttachmentsDialog INHERIT DIALOGWINDOW 

	PROTECT oCCSaveButton AS PUSHBUTTON
	PROTECT oCCOpenButton AS PUSHBUTTON
	PROTECT oCCSelectAllButton AS PUSHBUTTON
	PROTECT oCCCloseButton AS PUSHBUTTON
	PROTECT oDCLocation AS SINGLELINEEDIT
	PROTECT oDCAttachments AS ATTACHMENTLISTVIEW
	PROTECT oDCFixedText1 AS FIXEDTEXT
	PROTECT oCCSelectDir AS PUSHBUTTON

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)
   PROTECT oEmail AS CEmail

METHOD CloseButton( ) 
	SELF:EndDialog()
   RETURN SELF

CONSTRUCTOR(oParent,uExtra)  
LOCAL DIM aFonts[1] AS OBJECT

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"AttachmentsDialog",_GetInst()},TRUE)

aFonts[1] := Font{,8,"Microsoft Sans Serif"}
aFonts[1]:Underline := TRUE

oCCSaveButton := PushButton{SELF,ResourceID{ATTACHMENTSDIALOG_SAVEBUTTON,_GetInst()}}
oCCSaveButton:HyperLabel := HyperLabel{#SaveButton,_chr(38)+"Save",NULL_STRING,NULL_STRING}

oCCOpenButton := PushButton{SELF,ResourceID{ATTACHMENTSDIALOG_OPENBUTTON,_GetInst()}}
oCCOpenButton:HyperLabel := HyperLabel{#OpenButton,_chr(38)+"Open",NULL_STRING,NULL_STRING}

oCCSelectAllButton := PushButton{SELF,ResourceID{ATTACHMENTSDIALOG_SELECTALLBUTTON,_GetInst()}}
oCCSelectAllButton:HyperLabel := HyperLabel{#SelectAllButton,_chr(38)+"Select All",NULL_STRING,NULL_STRING}

oCCCloseButton := PushButton{SELF,ResourceID{ATTACHMENTSDIALOG_CLOSEBUTTON,_GetInst()}}
oCCCloseButton:HyperLabel := HyperLabel{#CloseButton,_chr(38)+"Close",NULL_STRING,NULL_STRING}

oDCLocation := SingleLineEdit{SELF,ResourceID{ATTACHMENTSDIALOG_LOCATION,_GetInst()}}
oDCLocation:HyperLabel := HyperLabel{#Location,NULL_STRING,NULL_STRING,NULL_STRING}

oDCAttachments := AttachmentListView{SELF,ResourceID{ATTACHMENTSDIALOG_ATTACHMENTS,_GetInst()}}
oDCAttachments:HyperLabel := HyperLabel{#Attachments,NULL_STRING,NULL_STRING,NULL_STRING}
oDCAttachments:ContextMenu := SaveAttachmentsContextMenu{}

oDCFixedText1 := FixedText{SELF,ResourceID{ATTACHMENTSDIALOG_FIXEDTEXT1,_GetInst()}}
oDCFixedText1:HyperLabel := HyperLabel{#FixedText1,"Save to:",NULL_STRING,NULL_STRING}
oDCFixedText1:Font(aFonts[1], FALSE)

oCCSelectDir := PushButton{SELF,ResourceID{ATTACHMENTSDIALOG_SELECTDIR,_GetInst()}}
oCCSelectDir:HyperLabel := HyperLabel{#SelectDir,"...",NULL_STRING,NULL_STRING}

SELF:Caption := "Save Attachments"
SELF:HyperLabel := HyperLabel{#AttachmentsDialog,"Save Attachments",NULL_STRING,NULL_STRING}
SELF:Icon := AAP_EMAIL_ICON{}

self:PostInit(oParent,uExtra)

return self


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

	SELF:oDCLocation:@@Value := cFolder
	
	RETURN NIL


METHOD SaveButton() 

   oDCAttachments:Save(SELF:oEmail, Trim(SELF:oDCLocation:TextValue))
   RETURN SELF

METHOD SelectAllButton( ) 
	
	oDCAttachments:SelectAll()
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
