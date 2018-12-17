#region DEFINES
STATIC DEFINE EMAILCONFIG_AUTHENTICATION := 113 
STATIC DEFINE EMAILCONFIG_DELETEDOWNLOADS := 126 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT1 := 106 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT10 := 129 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT2 := 108 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT3 := 104 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT4 := 100 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT5 := 102 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT6 := 121 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT7 := 111 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT8 := 124 
STATIC DEFINE EMAILCONFIG_FIXEDTEXT9 := 128 
STATIC DEFINE EMAILCONFIG_GROUPBOX1 := 122 
STATIC DEFINE EMAILCONFIG_GROUPBOX2 := 123 
STATIC DEFINE EMAILCONFIG_GROUPBOX3 := 125 
STATIC DEFINE EMAILCONFIG_INBOXSTARTUP := 130 
STATIC DEFINE EMAILCONFIG_INSPECTHEADERS := 110 
STATIC DEFINE EMAILCONFIG_PBCANCEL := 120 
STATIC DEFINE EMAILCONFIG_PBOK := 119 
STATIC DEFINE EMAILCONFIG_PERIOD := 127 
STATIC DEFINE EMAILCONFIG_SLEACCOUNT := 107 
STATIC DEFINE EMAILCONFIG_SLEDIRECTORY := 118 
STATIC DEFINE EMAILCONFIG_SLEEMAIL := 103 
STATIC DEFINE EMAILCONFIG_SLEFULLNAME := 101 
STATIC DEFINE EMAILCONFIG_SLEPASSWORD := 109 
STATIC DEFINE EMAILCONFIG_SLEPOPSERVER := 105 
STATIC DEFINE EMAILCONFIG_SLESMTPPASSWORD := 117 
STATIC DEFINE EMAILCONFIG_SLESMTPSERVER := 112 
STATIC DEFINE EMAILCONFIG_SLESMTPUSERNAME := 115 
STATIC DEFINE EMAILCONFIG_SMTPPASSWORDFT := 116 
STATIC DEFINE EMAILCONFIG_SMTPUSERNAMEFT := 114 
#endregion

class EmailConfig inherit DIALOGWINDOW 

	protect oDCFixedText4 as FIXEDTEXT
	protect oDCSLEFullname as SINGLELINEEDIT
	protect oDCFixedText5 as FIXEDTEXT
	protect oDCSLEEmail as SINGLELINEEDIT
	protect oDCFixedText3 as FIXEDTEXT
	protect oDCSLEPopServer as SINGLELINEEDIT
	protect oDCFixedText1 as FIXEDTEXT
	protect oDCSLEAccount as SINGLELINEEDIT
	protect oDCFixedText2 as FIXEDTEXT
	protect oDCSLEPassword as SINGLELINEEDIT
	protect oDCInspectHeaders as CHECKBOX
	protect oDCFixedText7 as FIXEDTEXT
	protect oDCSLESmtpServer as SINGLELINEEDIT
	protect oDCAuthentication as CHECKBOX
	protect oDCSmtpUserNameFT as FIXEDTEXT
	protect oDCSLESmtpUserName as SINGLELINEEDIT
	protect oDCSmtpPasswordFT as FIXEDTEXT
	protect oDCSLESmtpPassword as SINGLELINEEDIT
	protect oDCSLEDirectory as SINGLELINEEDIT
	protect oCCPBOK as PUSHBUTTON
	protect oCCPBCancel as PUSHBUTTON
	protect oDCFixedText6 as FIXEDTEXT
	protect oDCGroupBox1 as GROUPBOX
	protect oDCGroupBox2 as GROUPBOX
	protect oDCFixedText8 as FIXEDTEXT
	protect oDCGroupBox3 as GROUPBOX
	protect oDCDeleteDownloads as CHECKBOX
	protect oDCPeriod as SINGLELINEEDIT
	protect oDCFixedText9 as FIXEDTEXT
	protect oDCFixedText10 as FIXEDTEXT
	protect oDCInboxStartup as CHECKBOX

  //{{%UC%}}
  //USER CODE STARTS HERE (do NOT remove this line)

METHOD ButtonClick(oControlEvent) 

	LOCAL oControl AS OBJECT

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)

	SUPER:ButtonClick(oControlEvent)
	
	IF oControl:NameSym = #Authentication
		SELF:SetSmtpAuthentication(oControl:Checked)
	ENDIF

	RETURN NIL


CONSTRUCTOR(oParent,uExtra)  
local dim aFonts[4] AS OBJECT

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"EmailConfig",_GetInst()},TRUE)

aFonts[1] := Font{,8,"MS Sans Serif"}
aFonts[1]:Bold := TRUE
aFonts[2] := Font{,8,"Microsoft Sans Serif"}
aFonts[2]:Bold := TRUE
aFonts[3] := Font{,24,"MS Sans Serif"}
aFonts[3]:Bold := TRUE
aFonts[4] := Font{,10,"MS Sans Serif"}
aFonts[4]:Bold := TRUE

oDCFixedText4 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT4,_GetInst()}}
oDCFixedText4:HyperLabel := HyperLabel{#FixedText4,_chr(38)+"Full name",NULL_STRING,NULL_STRING}
oDCFixedText4:Font(aFonts[1], FALSE)

oDCSLEFullname := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEFULLNAME,_GetInst()}}
oDCSLEFullname:HyperLabel := HyperLabel{#SLEFullname,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFixedText5 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT5,_GetInst()}}
oDCFixedText5:HyperLabel := HyperLabel{#FixedText5,_chr(38)+"Email address",NULL_STRING,NULL_STRING}
oDCFixedText5:Font(aFonts[1], FALSE)

oDCSLEEmail := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEEMAIL,_GetInst()}}
oDCSLEEmail:HyperLabel := HyperLabel{#SLEEmail,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFixedText3 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT3,_GetInst()}}
oDCFixedText3:HyperLabel := HyperLabel{#FixedText3,_chr(38)+"Server Name",NULL_STRING,NULL_STRING}
oDCFixedText3:Font(aFonts[1], FALSE)

oDCSLEPopServer := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEPOPSERVER,_GetInst()}}
oDCSLEPopServer:HyperLabel := HyperLabel{#SLEPopServer,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFixedText1 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT1,_GetInst()}}
oDCFixedText1:HyperLabel := HyperLabel{#FixedText1,_chr(38)+"Account name",NULL_STRING,NULL_STRING}
oDCFixedText1:Font(aFonts[1], FALSE)

oDCSLEAccount := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEACCOUNT,_GetInst()}}
oDCSLEAccount:HyperLabel := HyperLabel{#SLEAccount,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFixedText2 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT2,_GetInst()}}
oDCFixedText2:HyperLabel := HyperLabel{#FixedText2,_chr(38)+"Password",NULL_STRING,NULL_STRING}
oDCFixedText2:Font(aFonts[1], FALSE)

oDCSLEPassword := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEPASSWORD,_GetInst()}}
oDCSLEPassword:HyperLabel := HyperLabel{#SLEPassword,NULL_STRING,NULL_STRING,NULL_STRING}

oDCInspectHeaders := CheckBox{self,ResourceID{EMAILCONFIG_INSPECTHEADERS,_GetInst()}}
oDCInspectHeaders:HyperLabel := HyperLabel{#InspectHeaders,_chr(38)+"Inspect Headers before Download",NULL_STRING,NULL_STRING}
oDCInspectHeaders:Font(aFonts[2], FALSE)

oDCFixedText7 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT7,_GetInst()}}
oDCFixedText7:HyperLabel := HyperLabel{#FixedText7,"Server name",NULL_STRING,NULL_STRING}
oDCFixedText7:Font(aFonts[1], FALSE)

oDCSLESmtpServer := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLESMTPSERVER,_GetInst()}}
oDCSLESmtpServer:HyperLabel := HyperLabel{#SLESmtpServer,NULL_STRING,NULL_STRING,NULL_STRING}

oDCAuthentication := CheckBox{self,ResourceID{EMAILCONFIG_AUTHENTICATION,_GetInst()}}
oDCAuthentication:HyperLabel := HyperLabel{#Authentication,"Smtp Server Requires "+_chr(38)+"Authentication",NULL_STRING,NULL_STRING}
oDCAuthentication:Font(aFonts[2], FALSE)

oDCSmtpUserNameFT := FixedText{self,ResourceID{EMAILCONFIG_SMTPUSERNAMEFT,_GetInst()}}
oDCSmtpUserNameFT:HyperLabel := HyperLabel{#SmtpUserNameFT,_chr(38)+"User name",NULL_STRING,NULL_STRING}
oDCSmtpUserNameFT:Font(aFonts[1], FALSE)

oDCSLESmtpUserName := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLESMTPUSERNAME,_GetInst()}}
oDCSLESmtpUserName:HyperLabel := HyperLabel{#SLESmtpUserName,NULL_STRING,NULL_STRING,NULL_STRING}

oDCSmtpPasswordFT := FixedText{self,ResourceID{EMAILCONFIG_SMTPPASSWORDFT,_GetInst()}}
oDCSmtpPasswordFT:HyperLabel := HyperLabel{#SmtpPasswordFT,_chr(38)+"Password",NULL_STRING,NULL_STRING}
oDCSmtpPasswordFT:Font(aFonts[1], FALSE)

oDCSLESmtpPassword := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLESMTPPASSWORD,_GetInst()}}
oDCSLESmtpPassword:HyperLabel := HyperLabel{#SLESmtpPassword,NULL_STRING,NULL_STRING,NULL_STRING}

oDCSLEDirectory := SingleLineEdit{self,ResourceID{EMAILCONFIG_SLEDIRECTORY,_GetInst()}}
oDCSLEDirectory:HyperLabel := HyperLabel{#SLEDirectory,NULL_STRING,NULL_STRING,NULL_STRING}

oCCPBOK := PushButton{self,ResourceID{EMAILCONFIG_PBOK,_GetInst()}}
oCCPBOK:HyperLabel := HyperLabel{#PBOK,_chr(38)+"OK",NULL_STRING,NULL_STRING}

oCCPBCancel := PushButton{self,ResourceID{EMAILCONFIG_PBCANCEL,_GetInst()}}
oCCPBCancel:HyperLabel := HyperLabel{#PBCancel,_chr(38)+"Cancel",NULL_STRING,NULL_STRING}

oDCFixedText6 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT6,_GetInst()}}
oDCFixedText6:HyperLabel := HyperLabel{#FixedText6,"Internet Mail",NULL_STRING,NULL_STRING}
oDCFixedText6:Font(aFonts[3], FALSE)

oDCGroupBox1 := GroupBox{self,ResourceID{EMAILCONFIG_GROUPBOX1,_GetInst()}}
oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1,"Incoming Mail Server (POP3):",NULL_STRING,NULL_STRING}
oDCGroupBox1:Font(aFonts[4], FALSE)

oDCGroupBox2 := GroupBox{self,ResourceID{EMAILCONFIG_GROUPBOX2,_GetInst()}}
oDCGroupBox2:HyperLabel := HyperLabel{#GroupBox2,"Personal Information:",NULL_STRING,NULL_STRING}
oDCGroupBox2:Font(aFonts[4], FALSE)

oDCFixedText8 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT8,_GetInst()}}
oDCFixedText8:HyperLabel := HyperLabel{#FixedText8,"Attachments "+_chr(38)+"Directory:",NULL_STRING,NULL_STRING}
oDCFixedText8:Font(aFonts[1], FALSE)

oDCGroupBox3 := GroupBox{self,ResourceID{EMAILCONFIG_GROUPBOX3,_GetInst()}}
oDCGroupBox3:HyperLabel := HyperLabel{#GroupBox3,"Outgoing Mail Server (SMTP):",NULL_STRING,NULL_STRING}
oDCGroupBox3:Font(aFonts[4], FALSE)

oDCDeleteDownloads := CheckBox{self,ResourceID{EMAILCONFIG_DELETEDOWNLOADS,_GetInst()}}
oDCDeleteDownloads:HyperLabel := HyperLabel{#DeleteDownloads,_chr(38)+"Delete downloaded mail from Server",NULL_STRING,NULL_STRING}
oDCDeleteDownloads:Font(aFonts[2], FALSE)

oDCPeriod := SingleLineEdit{self,ResourceID{EMAILCONFIG_PERIOD,_GetInst()}}
oDCPeriod:HyperLabel := HyperLabel{#Period,NULL_STRING,NULL_STRING,NULL_STRING}

oDCFixedText9 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT9,_GetInst()}}
oDCFixedText9:HyperLabel := HyperLabel{#FixedText9,_chr(38)+"Check for new Mail every",NULL_STRING,NULL_STRING}
oDCFixedText9:Font(aFonts[1], FALSE)

oDCFixedText10 := FixedText{self,ResourceID{EMAILCONFIG_FIXEDTEXT10,_GetInst()}}
oDCFixedText10:HyperLabel := HyperLabel{#FixedText10,"minutes",NULL_STRING,NULL_STRING}
oDCFixedText10:Font(aFonts[1], FALSE)

oDCInboxStartup := CheckBox{self,ResourceID{EMAILCONFIG_INBOXSTARTUP,_GetInst()}}
oDCInboxStartup:HyperLabel := HyperLabel{#InboxStartup,"Startup at my Inbox",NULL_STRING,NULL_STRING}
oDCInboxStartup:Font(aFonts[2], FALSE)

self:Caption := "Email Sample Configuration"
self:HyperLabel := HyperLabel{#EmailConfig,"Email Sample Configuration",NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

return self


METHOD PBCancel( ) 
	SELF:EndDialog()
   RETURN SELF

METHOD PBOK( ) 

	LOCAL oReg AS RegSetup
	LOCAL cDirectory AS STRING
	LOCAL nError AS INT

	cDirectory := Trim(SELF:oDCSLEDirectory:Value)	// trailing spaces will cause file name errors

	nError := DirMake(PSZ(cDirectory))
	IF nError != NO_ERROR
		IF nError != ERROR_ALREADY_EXISTS
			ErrorBox{,"Cannot Create Folder - Choose again"}:Show()
			RETURN SELF
		ENDIF
	ENDIF

	// put back the slash if the user did not specify
	IF Right(cDirectory,1) != "\"
		cDirectory += "\"
	ENDIF

	oReg := RegSetup{}
	oReg:SetString("Email_Address",      Trim(SELF:oDCSLEEmail:Value))
	oReg:SetString("Email_SmtpServer",   Trim(SELF:oDCSLESmtpServer:Value))
	oReg:SetString("Email_PopServer",    Trim(SELF:oDCSLEPopServer:Value))	
	oReg:SetString("Email_Fullname",     Trim(SELF:oDCSLEFullname:Value))
	oReg:SetString("Email_Account",      Trim(SELF:oDCSLEAccount:Value))
	oReg:SetString("Email_Password",     Crypt(Trim(SELF:oDCSLEPassword:Value),"VO SO GOOD"))
	oReg:SetString("Email_Directory",    cDirectory)
	oReg:SetString("Email_SmtpUserName", Trim(SELF:oDCSLESmtpUserName:Value))
	oReg:SetString("Email_SmtpPassword", Crypt(Trim(SELF:oDCSLESmtpPassword:Value),"VO SO GOOD"))

	oReg:SetInt("Email_SmtpAuthentication", if(SELF:oDCAuthentication:Checked, 1, 0))
	oReg:SetInt("Email_InspectPopHeaders",  if(SELF:oDCInspectHeaders:Checked, 1, 0))
	oReg:SetInt("Email_DeleteDownloads",    if(SELF:oDCDeleteDownloads:Checked, 1, 0))
	oReg:SetInt("Email_CheckDelay",         Val(SELF:oDCPeriod:TextValue))
	oReg:SetInt("Email_StartupInbox",       if(SELF:oDCInboxStartup:Checked, 1, 0))

	SetUserInfo()
	
	// reset the timer with the new delay (or disable it)
	SELF:Owner:KillTheTimer()
	SELF:Owner:SetMyTimer()
	
	SELF:EndDialog()
	
	RETURN SELF

METHOD PostInit(oParent,uExtra) 

	LOCAL oReg AS RegSetup
	LOCAL cFolder AS STRING

	oReg := RegSetup{}	
	
	cFolder := oReg:QueryString("Email_Directory")	
	// strip off the trailing slash for visual appeal only
	IF Right(cFolder, 1) = "\"
		cFolder := Left(cFolder, SLen(cFolder)-1)
	ENDIF
	
	SELF:oDCSLEEmail:Value         := oReg:QueryString("Email_Address")
	SELF:oDCSLESmtpServer:Value    := oReg:QueryString("Email_SmtpServer")
	SELF:oDCSLEPopServer:Value     := oReg:QueryString("Email_PopServer")	
	SELF:oDCSLEFullname:Value      := oReg:QueryString("Email_Fullname")
	SELF:oDCSLEAccount:Value       := oReg:QueryString("Email_Account")
	SELF:oDCSLEPassword:Value      := Crypt(oReg:QueryString("Email_Password"),"VO SO GOOD")
	SELF:oDCSLEDirectory:Value     := cFolder
	SELF:oDCSLESmtpPassword:Value  := Crypt(oReg:QueryString("Email_SmtpPassword"),"VO SO GOOD")
	SELF:oDCSLESmtpUserName:Value  := oReg:QueryString("Email_SmtpUserName")	
	SELF:oDCAuthentication:Value   := oReg:QueryInt("Email_SmtpAuthentication") = 1
	SELF:oDCInspectHeaders:Value   := oReg:QueryInt("Email_InspectPopHeaders") = 1
	SELF:oDCDeleteDownloads:Value  := oReg:QueryInt("Email_DeleteDownloads") = 1
	SELF:oDCInboxStartup:Value     := oReg:QueryInt("Email_StartupInbox") = 1
	SELF:oDCPeriod:Value           := oReg:QueryInt("Email_CheckDelay")
	
	SELF:SetSmtpAuthentication(SELF:oDCAuthentication:Checked)

    RETURN NIL


METHOD SetSmtpAuthentication(lEnable) 
	
	Default(@lEnable, TRUE)
	
	IF lEnable
		SELF:oDCSLESmtpPassword:Enable()
		SELF:oDCSLESmtpUserName:Enable()
		SELF:oDCSmtpUserNameFT:Enable()
		SELF:oDCSmtpPasswordFT:Enable()
	ELSE
		SELF:oDCSLESmtpPassword:Disable()
		SELF:oDCSLESmtpUserName:Disable()
		SELF:oDCSmtpUserNameFT:Disable()
		SELF:oDCSmtpPasswordFT:Disable()
	ENDIF

	RETURN NIL

END CLASS
