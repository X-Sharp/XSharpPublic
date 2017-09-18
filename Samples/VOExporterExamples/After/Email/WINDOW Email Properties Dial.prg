#region DEFINES
STATIC DEFINE EMAILPROPERTIESDIALOG_CLOSEBUTTON := 100
STATIC DEFINE EMAILPROPERTIESDIALOG_SOURCEBUTTON := 101
STATIC DEFINE EMAILPROPERTIESDIALOG_TOTEXT := 102
STATIC DEFINE EMAILPROPERTIESDIALOG_FROMINFOTEXT := 103
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLEINFOTEXT := 104
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT := 105
STATIC DEFINE EMAILPROPERTIESDIALOG_LETTER_ICON := 106
STATIC DEFINE EMAILPROPERTIESDIALOG_GROUPBOX1 := 107
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT1 := 108
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT2 := 109
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT3 := 110
STATIC DEFINE EMAILPROPERTIESDIALOG_GROUPBOX2 := 111
STATIC DEFINE EMAILPROPERTIESDIALOG_FROMINFOTEXT1 := 112
STATIC DEFINE EMAILPROPERTIESDIALOG_LOCATIONTEXT := 113
STATIC DEFINE EMAILPROPERTIESDIALOG_SIZETEXT := 114
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT4 := 115
STATIC DEFINE EMAILPROPERTIESDIALOG_PRIORITYTEXT := 116
STATIC DEFINE EMAILPROPERTIESDIALOG_SENTTEXT := 117
STATIC DEFINE EMAILPROPERTIESDIALOG_RECEIVEDTEXT := 118
#endregion

CLASS EmailPropertiesDialog INHERIT DIALOGWINDOW
	PROTECT oCCCloseButton AS PUSHBUTTON
	PROTECT oCCSourceButton AS PUSHBUTTON
	PROTECT oDCToText AS FIXEDTEXT
	PROTECT oDCFromInfoText AS FIXEDTEXT
	PROTECT oDCTitleInfoText AS FIXEDTEXT
	PROTECT oDCTitleText AS FIXEDTEXT
	PROTECT oDCGroupBox1 AS GROUPBOX
	PROTECT oDCTitleText1 AS FIXEDTEXT
	PROTECT oDCTitleText2 AS FIXEDTEXT
	PROTECT oDCTitleText3 AS FIXEDTEXT
	PROTECT oDCGroupBox2 AS GROUPBOX
	PROTECT oDCFromInfoText1 AS FIXEDTEXT
	PROTECT oDCLocationText AS FIXEDTEXT
	PROTECT oDCSizeText AS FIXEDTEXT
	PROTECT oDCTitleText4 AS FIXEDTEXT
	PROTECT oDCPriorityText AS FIXEDTEXT
	PROTECT oDCSenttext AS FIXEDTEXT
	PROTECT oDCReceivedText AS FIXEDTEXT

	// {{%UC%}} User code starts here (DO NOT remove this line)  

	PROTECT oEmail AS CEmail
	PROTECT oServer AS EmailStore

CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"EmailPropertiesDialog" , _GetInst()} , TRUE)

	SELF:oCCCloseButton := PUSHBUTTON{SELF , ResourceID{ EMAILPROPERTIESDIALOG_CLOSEBUTTON  , _GetInst() } }
	SELF:oCCCloseButton:HyperLabel := HyperLabel{#CloseButton , "&Close" , NULL_STRING , NULL_STRING}

	SELF:oCCSourceButton := PUSHBUTTON{SELF , ResourceID{ EMAILPROPERTIESDIALOG_SOURCEBUTTON  , _GetInst() } }
	SELF:oCCSourceButton:HyperLabel := HyperLabel{#SourceButton , "&Message Header" , NULL_STRING , NULL_STRING}

	SELF:oDCToText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TOTEXT  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCToText:Font( oFont )
	SELF:oDCToText:HyperLabel := HyperLabel{#ToText , "Type:" , NULL_STRING , NULL_STRING}

	SELF:oDCFromInfoText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_FROMINFOTEXT  , _GetInst() } }
	SELF:oDCFromInfoText:HyperLabel := HyperLabel{#FromInfoText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCTitleInfoText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLEINFOTEXT  , _GetInst() } }
	SELF:oDCTitleInfoText:HyperLabel := HyperLabel{#TitleInfoText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCTitleText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLETEXT  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCTitleText:Font( oFont )
	SELF:oDCTitleText:HyperLabel := HyperLabel{#TitleText , "Location:" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox1 := GROUPBOX{SELF , ResourceID{ EMAILPROPERTIESDIALOG_GROUPBOX1  , _GetInst() } }
	SELF:oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCTitleText1 := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLETEXT1  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCTitleText1:Font( oFont )
	SELF:oDCTitleText1:HyperLabel := HyperLabel{#TitleText1 , "Sent:" , NULL_STRING , NULL_STRING}

	SELF:oDCTitleText2 := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLETEXT2  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCTitleText2:Font( oFont )
	SELF:oDCTitleText2:HyperLabel := HyperLabel{#TitleText2 , "Received:" , NULL_STRING , NULL_STRING}

	SELF:oDCTitleText3 := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLETEXT3  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCTitleText3:Font( oFont )
	SELF:oDCTitleText3:HyperLabel := HyperLabel{#TitleText3 , "Size:" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox2 := GROUPBOX{SELF , ResourceID{ EMAILPROPERTIESDIALOG_GROUPBOX2  , _GetInst() } }
	SELF:oDCGroupBox2:HyperLabel := HyperLabel{#GroupBox2 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFromInfoText1 := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_FROMINFOTEXT1  , _GetInst() } }
	SELF:oDCFromInfoText1:HyperLabel := HyperLabel{#FromInfoText1 , "Mail Message" , NULL_STRING , NULL_STRING}

	SELF:oDCLocationText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_LOCATIONTEXT  , _GetInst() } }
	SELF:oDCLocationText:HyperLabel := HyperLabel{#LocationText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCSizeText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_SIZETEXT  , _GetInst() } }
	SELF:oDCSizeText:HyperLabel := HyperLabel{#SizeText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCTitleText4 := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_TITLETEXT4  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCTitleText4:Font( oFont )
	SELF:oDCTitleText4:HyperLabel := HyperLabel{#TitleText4 , "Priority:" , NULL_STRING , NULL_STRING}

	SELF:oDCPriorityText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_PRIORITYTEXT  , _GetInst() } }
	SELF:oDCPriorityText:HyperLabel := HyperLabel{#PriorityText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCSenttext := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_SENTTEXT  , _GetInst() } }
	SELF:oDCSenttext:HyperLabel := HyperLabel{#Senttext , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:oDCReceivedText := FIXEDTEXT{SELF , ResourceID{ EMAILPROPERTIESDIALOG_RECEIVEDTEXT  , _GetInst() } }
	SELF:oDCReceivedText:HyperLabel := HyperLabel{#ReceivedText , "Fixed Text" , NULL_STRING , NULL_STRING}

	SELF:Caption := "Email Properties"
	SELF:HyperLabel := HyperLabel{#EmailPropertiesDialog , "Email Properties" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD PostInit(oParent,uExtra) 
	
	SELF:oServer := EmailStore{}
	SELF:oServer:Recno := uExtra
	
	SELF:oEmail := SELF:oServer:GetEMail()
	
	oDCFromInfoText:Caption  := SELF:oEmail:FromAddress
	oDCTitleInfoText:Caption := SELF:oEmail:Subject
	oDCLocationText:Caption  := oParent:MailBox
	
	oDCPriorityText:Caption  := NTrim(SELF:oEmail:PRIORITY)
	oDCSizeText:Caption      := FileSizeString(SELF:oServer:FIELDGET(#E_SIZE))
	
	oDCSenttext:Caption      := SELF:oEmail:TimeStamp
	oDCReceivedText:Caption  := DToC(SELF:oEmail:MailDate) + " " + SELF:oEmail:MailTime

	RETURN NIL


METHOD SourceButton() 
	
	MessageSourceDialog{SELF, oServer:FIELDGET(#E_HEADER)}:Show(SHOWCENTERED)
	
	RETURN SELF


METHOD CloseButton( ) 
	
	SELF:EndDialog()
   RETURN SELF

METHOD Close(oEvent) 
	SUPER:Close(oEvent)
	
	SELF:oServer:close()

	RETURN NIL



END CLASS
