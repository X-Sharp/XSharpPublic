#region DEFINES
STATIC DEFINE EMAILPROPERTIESDIALOG_CLOSEBUTTON := 100 
STATIC DEFINE EMAILPROPERTIESDIALOG_FROMINFOTEXT := 103 
STATIC DEFINE EMAILPROPERTIESDIALOG_FROMINFOTEXT1 := 112 
STATIC DEFINE EMAILPROPERTIESDIALOG_GROUPBOX1 := 107 
STATIC DEFINE EMAILPROPERTIESDIALOG_GROUPBOX2 := 111 
STATIC DEFINE EMAILPROPERTIESDIALOG_LETTER_ICON := 106 
STATIC DEFINE EMAILPROPERTIESDIALOG_LOCATIONTEXT := 113 
STATIC DEFINE EMAILPROPERTIESDIALOG_PRIORITYTEXT := 116 
STATIC DEFINE EMAILPROPERTIESDIALOG_RECEIVEDTEXT := 118 
STATIC DEFINE EMAILPROPERTIESDIALOG_SENTTEXT := 117 
STATIC DEFINE EMAILPROPERTIESDIALOG_SIZETEXT := 114 
STATIC DEFINE EMAILPROPERTIESDIALOG_SOURCEBUTTON := 101 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLEINFOTEXT := 104 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT := 105 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT1 := 108 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT2 := 109 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT3 := 110 
STATIC DEFINE EMAILPROPERTIESDIALOG_TITLETEXT4 := 115 
STATIC DEFINE EMAILPROPERTIESDIALOG_TOTEXT := 102 
#endregion

class EmailPropertiesDialog inherit DIALOGWINDOW 

	protect oCCCloseButton as PUSHBUTTON
	protect oCCSourceButton as PUSHBUTTON
	protect oDCToText as FIXEDTEXT
	protect oDCFromInfoText as FIXEDTEXT
	protect oDCTitleInfoText as FIXEDTEXT
	protect oDCTitleText as FIXEDTEXT
	protect oDCGroupBox1 as GROUPBOX
	protect oDCTitleText1 as FIXEDTEXT
	protect oDCTitleText2 as FIXEDTEXT
	protect oDCTitleText3 as FIXEDTEXT
	protect oDCGroupBox2 as GROUPBOX
	protect oDCFromInfoText1 as FIXEDTEXT
	protect oDCLocationText as FIXEDTEXT
	protect oDCSizeText as FIXEDTEXT
	protect oDCTitleText4 as FIXEDTEXT
	protect oDCPriorityText as FIXEDTEXT
	protect oDCSenttext as FIXEDTEXT
	protect oDCReceivedText as FIXEDTEXT

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

	PROTECT oEmail AS CEmail
	PROTECT oServer AS EmailStore

METHOD Close(oEvent) 
	SUPER:Close(oEvent)
	
	SELF:oServer:close()

	RETURN NIL


METHOD CloseButton( ) 
	
	SELF:EndDialog()
   RETURN SELF

CONSTRUCTOR(oParent,uExtra)  
local dim aFonts[1] AS OBJECT

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"EmailPropertiesDialog",_GetInst()},TRUE)

aFonts[1] := Font{,8,"Microsoft Sans Serif"}
aFonts[1]:Bold := TRUE

oCCCloseButton := PushButton{self,ResourceID{EMAILPROPERTIESDIALOG_CLOSEBUTTON,_GetInst()}}
oCCCloseButton:HyperLabel := HyperLabel{#CloseButton,_chr(38)+"Close",NULL_STRING,NULL_STRING}

oCCSourceButton := PushButton{self,ResourceID{EMAILPROPERTIESDIALOG_SOURCEBUTTON,_GetInst()}}
oCCSourceButton:HyperLabel := HyperLabel{#SourceButton,_chr(38)+"Message Header",NULL_STRING,NULL_STRING}

oDCToText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TOTEXT,_GetInst()}}
oDCToText:HyperLabel := HyperLabel{#ToText,"Type:",NULL_STRING,NULL_STRING}
oDCToText:Font(aFonts[1], FALSE)

oDCFromInfoText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_FROMINFOTEXT,_GetInst()}}
oDCFromInfoText:HyperLabel := HyperLabel{#FromInfoText,"Fixed Text",NULL_STRING,NULL_STRING}

oDCTitleInfoText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLEINFOTEXT,_GetInst()}}
oDCTitleInfoText:HyperLabel := HyperLabel{#TitleInfoText,"Fixed Text",NULL_STRING,NULL_STRING}

oDCTitleText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLETEXT,_GetInst()}}
oDCTitleText:HyperLabel := HyperLabel{#TitleText,"Location:",NULL_STRING,NULL_STRING}
oDCTitleText:Font(aFonts[1], FALSE)

oDCGroupBox1 := GroupBox{self,ResourceID{EMAILPROPERTIESDIALOG_GROUPBOX1,_GetInst()}}
oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1,NULL_STRING,NULL_STRING,NULL_STRING}

oDCTitleText1 := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLETEXT1,_GetInst()}}
oDCTitleText1:HyperLabel := HyperLabel{#TitleText1,"Sent:",NULL_STRING,NULL_STRING}
oDCTitleText1:Font(aFonts[1], FALSE)

oDCTitleText2 := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLETEXT2,_GetInst()}}
oDCTitleText2:HyperLabel := HyperLabel{#TitleText2,"Received:",NULL_STRING,NULL_STRING}
oDCTitleText2:Font(aFonts[1], FALSE)

oDCTitleText3 := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLETEXT3,_GetInst()}}
oDCTitleText3:HyperLabel := HyperLabel{#TitleText3,"Size:",NULL_STRING,NULL_STRING}
oDCTitleText3:Font(aFonts[1], FALSE)

oDCGroupBox2 := GroupBox{self,ResourceID{EMAILPROPERTIESDIALOG_GROUPBOX2,_GetInst()}}
oDCGroupBox2:HyperLabel := HyperLabel{#GroupBox2,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFromInfoText1 := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_FROMINFOTEXT1,_GetInst()}}
oDCFromInfoText1:HyperLabel := HyperLabel{#FromInfoText1,"Mail Message",NULL_STRING,NULL_STRING}

oDCLocationText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_LOCATIONTEXT,_GetInst()}}
oDCLocationText:HyperLabel := HyperLabel{#LocationText,"Fixed Text",NULL_STRING,NULL_STRING}

oDCSizeText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_SIZETEXT,_GetInst()}}
oDCSizeText:HyperLabel := HyperLabel{#SizeText,"Fixed Text",NULL_STRING,NULL_STRING}

oDCTitleText4 := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_TITLETEXT4,_GetInst()}}
oDCTitleText4:HyperLabel := HyperLabel{#TitleText4,"Priority:",NULL_STRING,NULL_STRING}
oDCTitleText4:Font(aFonts[1], FALSE)

oDCPriorityText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_PRIORITYTEXT,_GetInst()}}
oDCPriorityText:HyperLabel := HyperLabel{#PriorityText,"Fixed Text",NULL_STRING,NULL_STRING}

oDCSenttext := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_SENTTEXT,_GetInst()}}
oDCSenttext:HyperLabel := HyperLabel{#Senttext,"Fixed Text",NULL_STRING,NULL_STRING}

oDCReceivedText := FixedText{self,ResourceID{EMAILPROPERTIESDIALOG_RECEIVEDTEXT,_GetInst()}}
oDCReceivedText:HyperLabel := HyperLabel{#ReceivedText,"Fixed Text",NULL_STRING,NULL_STRING}

self:Caption := "Email Properties"
self:HyperLabel := HyperLabel{#EmailPropertiesDialog,"Email Properties",NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


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


END CLASS
