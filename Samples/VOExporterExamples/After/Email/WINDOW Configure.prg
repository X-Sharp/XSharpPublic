#region DEFINES
STATIC DEFINE EMAILCONFIG_FIXEDTEXT4 := 100
STATIC DEFINE EMAILCONFIG_SLEFULLNAME := 101
STATIC DEFINE EMAILCONFIG_FIXEDTEXT5 := 102
STATIC DEFINE EMAILCONFIG_SLEEMAIL := 103
STATIC DEFINE EMAILCONFIG_FIXEDTEXT3 := 104
STATIC DEFINE EMAILCONFIG_SLEPOPSERVER := 105
STATIC DEFINE EMAILCONFIG_FIXEDTEXT1 := 106
STATIC DEFINE EMAILCONFIG_SLEACCOUNT := 107
STATIC DEFINE EMAILCONFIG_FIXEDTEXT2 := 108
STATIC DEFINE EMAILCONFIG_SLEPASSWORD := 109
STATIC DEFINE EMAILCONFIG_INSPECTHEADERS := 110
STATIC DEFINE EMAILCONFIG_FIXEDTEXT7 := 111
STATIC DEFINE EMAILCONFIG_SLESMTPSERVER := 112
STATIC DEFINE EMAILCONFIG_AUTHENTICATION := 113
STATIC DEFINE EMAILCONFIG_SMTPUSERNAMEFT := 114
STATIC DEFINE EMAILCONFIG_SLESMTPUSERNAME := 115
STATIC DEFINE EMAILCONFIG_SMTPPASSWORDFT := 116
STATIC DEFINE EMAILCONFIG_SLESMTPPASSWORD := 117
STATIC DEFINE EMAILCONFIG_SLEDIRECTORY := 118
STATIC DEFINE EMAILCONFIG_PBOK := 119
STATIC DEFINE EMAILCONFIG_PBCANCEL := 120
STATIC DEFINE EMAILCONFIG_FIXEDTEXT6 := 121
STATIC DEFINE EMAILCONFIG_GROUPBOX1 := 122
STATIC DEFINE EMAILCONFIG_GROUPBOX2 := 123
STATIC DEFINE EMAILCONFIG_FIXEDTEXT8 := 124
STATIC DEFINE EMAILCONFIG_GROUPBOX3 := 125
STATIC DEFINE EMAILCONFIG_DELETEDOWNLOADS := 126
STATIC DEFINE EMAILCONFIG_PERIOD := 127
STATIC DEFINE EMAILCONFIG_FIXEDTEXT9 := 128
STATIC DEFINE EMAILCONFIG_FIXEDTEXT10 := 129
STATIC DEFINE EMAILCONFIG_INBOXSTARTUP := 130
#endregion

CLASS EmailConfig INHERIT DIALOGWINDOW
	PROTECT oDCFixedText4 AS FIXEDTEXT
	PROTECT oDCSLEFullname AS SINGLELINEEDIT
	PROTECT oDCFixedText5 AS FIXEDTEXT
	PROTECT oDCSLEEmail AS SINGLELINEEDIT
	PROTECT oDCFixedText3 AS FIXEDTEXT
	PROTECT oDCSLEPopServer AS SINGLELINEEDIT
	PROTECT oDCFixedText1 AS FIXEDTEXT
	PROTECT oDCSLEAccount AS SINGLELINEEDIT
	PROTECT oDCFixedText2 AS FIXEDTEXT
	PROTECT oDCSLEPassword AS SINGLELINEEDIT
	PROTECT oDCInspectHeaders AS CHECKBOX
	PROTECT oDCFixedText7 AS FIXEDTEXT
	PROTECT oDCSLESmtpServer AS SINGLELINEEDIT
	PROTECT oDCAuthentication AS CHECKBOX
	PROTECT oDCSmtpUserNameFT AS FIXEDTEXT
	PROTECT oDCSLESmtpUserName AS SINGLELINEEDIT
	PROTECT oDCSmtpPasswordFT AS FIXEDTEXT
	PROTECT oDCSLESmtpPassword AS SINGLELINEEDIT
	PROTECT oDCSLEDirectory AS SINGLELINEEDIT
	PROTECT oCCPBOK AS PUSHBUTTON
	PROTECT oCCPBCancel AS PUSHBUTTON
	PROTECT oDCFixedText6 AS FIXEDTEXT
	PROTECT oDCGroupBox1 AS GROUPBOX
	PROTECT oDCGroupBox2 AS GROUPBOX
	PROTECT oDCFixedText8 AS FIXEDTEXT
	PROTECT oDCGroupBox3 AS GROUPBOX
	PROTECT oDCDeleteDownloads AS CHECKBOX
	PROTECT oDCPeriod AS SINGLELINEEDIT
	PROTECT oDCFixedText9 AS FIXEDTEXT
	PROTECT oDCFixedText10 AS FIXEDTEXT
	PROTECT oDCInboxStartup AS CHECKBOX

	// {{%UC%}} User code starts here (DO NOT remove this line)  
  //USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oParent,uExtra)
	LOCAL oFont AS Font

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"EmailConfig" , _GetInst()} , TRUE)

	SELF:oDCFixedText4 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT4  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText4:Font( oFont )
	SELF:oDCFixedText4:HyperLabel := HyperLabel{#FixedText4 , "&Full name" , NULL_STRING , NULL_STRING}

	SELF:oDCSLEFullname := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEFULLNAME  , _GetInst() } }
	SELF:oDCSLEFullname:HyperLabel := HyperLabel{#SLEFullname , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText5 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT5  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText5:Font( oFont )
	SELF:oDCFixedText5:HyperLabel := HyperLabel{#FixedText5 , "&Email address" , NULL_STRING , NULL_STRING}

	SELF:oDCSLEEmail := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEEMAIL  , _GetInst() } }
	SELF:oDCSLEEmail:HyperLabel := HyperLabel{#SLEEmail , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText3 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT3  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText3:Font( oFont )
	SELF:oDCFixedText3:HyperLabel := HyperLabel{#FixedText3 , "&Server Name" , NULL_STRING , NULL_STRING}

	SELF:oDCSLEPopServer := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEPOPSERVER  , _GetInst() } }
	SELF:oDCSLEPopServer:HyperLabel := HyperLabel{#SLEPopServer , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText1 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT1  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText1:Font( oFont )
	SELF:oDCFixedText1:HyperLabel := HyperLabel{#FixedText1 , "&Account name" , NULL_STRING , NULL_STRING}

	SELF:oDCSLEAccount := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEACCOUNT  , _GetInst() } }
	SELF:oDCSLEAccount:HyperLabel := HyperLabel{#SLEAccount , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText2 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT2  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText2:Font( oFont )
	SELF:oDCFixedText2:HyperLabel := HyperLabel{#FixedText2 , "&Password" , NULL_STRING , NULL_STRING}

	SELF:oDCSLEPassword := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEPASSWORD  , _GetInst() } }
	SELF:oDCSLEPassword:HyperLabel := HyperLabel{#SLEPassword , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCInspectHeaders := CHECKBOX{SELF , ResourceID{ EMAILCONFIG_INSPECTHEADERS  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCInspectHeaders:Font( oFont )
	SELF:oDCInspectHeaders:HyperLabel := HyperLabel{#InspectHeaders , "&Inspect Headers before Download" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText7 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT7  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText7:Font( oFont )
	SELF:oDCFixedText7:HyperLabel := HyperLabel{#FixedText7 , "Server name" , NULL_STRING , NULL_STRING}

	SELF:oDCSLESmtpServer := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLESMTPSERVER  , _GetInst() } }
	SELF:oDCSLESmtpServer:HyperLabel := HyperLabel{#SLESmtpServer , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCAuthentication := CHECKBOX{SELF , ResourceID{ EMAILCONFIG_AUTHENTICATION  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCAuthentication:Font( oFont )
	SELF:oDCAuthentication:HyperLabel := HyperLabel{#Authentication , "Smtp Server Requires &Authentication" , NULL_STRING , NULL_STRING}

	SELF:oDCSmtpUserNameFT := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_SMTPUSERNAMEFT  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCSmtpUserNameFT:Font( oFont )
	SELF:oDCSmtpUserNameFT:HyperLabel := HyperLabel{#SmtpUserNameFT , "&User name" , NULL_STRING , NULL_STRING}

	SELF:oDCSLESmtpUserName := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLESMTPUSERNAME  , _GetInst() } }
	SELF:oDCSLESmtpUserName:HyperLabel := HyperLabel{#SLESmtpUserName , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSmtpPasswordFT := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_SMTPPASSWORDFT  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCSmtpPasswordFT:Font( oFont )
	SELF:oDCSmtpPasswordFT:HyperLabel := HyperLabel{#SmtpPasswordFT , "&Password" , NULL_STRING , NULL_STRING}

	SELF:oDCSLESmtpPassword := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLESMTPPASSWORD  , _GetInst() } }
	SELF:oDCSLESmtpPassword:HyperLabel := HyperLabel{#SLESmtpPassword , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSLEDirectory := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_SLEDIRECTORY  , _GetInst() } }
	SELF:oDCSLEDirectory:HyperLabel := HyperLabel{#SLEDirectory , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCPBOK := PUSHBUTTON{SELF , ResourceID{ EMAILCONFIG_PBOK  , _GetInst() } }
	SELF:oCCPBOK:HyperLabel := HyperLabel{#PBOK , "&OK" , NULL_STRING , NULL_STRING}

	SELF:oCCPBCancel := PUSHBUTTON{SELF , ResourceID{ EMAILCONFIG_PBCANCEL  , _GetInst() } }
	SELF:oCCPBCancel:HyperLabel := HyperLabel{#PBCancel , "&Cancel" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText6 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT6  , _GetInst() } }
	oFont := Font{  , 24 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText6:Font( oFont )
	SELF:oDCFixedText6:HyperLabel := HyperLabel{#FixedText6 , "Internet Mail" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox1 := GROUPBOX{SELF , ResourceID{ EMAILCONFIG_GROUPBOX1  , _GetInst() } }
	oFont := Font{  , 10 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCGroupBox1:Font( oFont )
	SELF:oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1 , "Incoming Mail Server POP3:" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox2 := GROUPBOX{SELF , ResourceID{ EMAILCONFIG_GROUPBOX2  , _GetInst() } }
	oFont := Font{  , 10 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCGroupBox2:Font( oFont )
	SELF:oDCGroupBox2:HyperLabel := HyperLabel{#GroupBox2 , "Personal Information:" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText8 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT8  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText8:Font( oFont )
	SELF:oDCFixedText8:HyperLabel := HyperLabel{#FixedText8 , "Attachments &Directory:" , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox3 := GROUPBOX{SELF , ResourceID{ EMAILCONFIG_GROUPBOX3  , _GetInst() } }
	oFont := Font{  , 10 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCGroupBox3:Font( oFont )
	SELF:oDCGroupBox3:HyperLabel := HyperLabel{#GroupBox3 , "Outgoing Mail Server SMTP:" , NULL_STRING , NULL_STRING}

	SELF:oDCDeleteDownloads := CHECKBOX{SELF , ResourceID{ EMAILCONFIG_DELETEDOWNLOADS  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCDeleteDownloads:Font( oFont )
	SELF:oDCDeleteDownloads:HyperLabel := HyperLabel{#DeleteDownloads , "&Delete downloaded mail from Server" , NULL_STRING , NULL_STRING}

	SELF:oDCPeriod := SINGLELINEEDIT{SELF , ResourceID{ EMAILCONFIG_PERIOD  , _GetInst() } }
	SELF:oDCPeriod:HyperLabel := HyperLabel{#Period , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText9 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT9  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText9:Font( oFont )
	SELF:oDCFixedText9:HyperLabel := HyperLabel{#FixedText9 , "&Check for new Mail every" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedText10 := FIXEDTEXT{SELF , ResourceID{ EMAILCONFIG_FIXEDTEXT10  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCFixedText10:Font( oFont )
	SELF:oDCFixedText10:HyperLabel := HyperLabel{#FixedText10 , "minutes" , NULL_STRING , NULL_STRING}

	SELF:oDCInboxStartup := CHECKBOX{SELF , ResourceID{ EMAILCONFIG_INBOXSTARTUP  , _GetInst() } }
	oFont := Font{  , 8 , "Microsoft Sans Serif" }
	oFont:Bold := TRUE
	SELF:oDCInboxStartup:Font( oFont )
	SELF:oDCInboxStartup:HyperLabel := HyperLabel{#InboxStartup , "Startup at my Inbox" , NULL_STRING , NULL_STRING}

	SELF:Caption := "Email Sample Configuration"
	SELF:HyperLabel := HyperLabel{#EmailConfig , "Email Sample Configuration" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


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

METHOD PBCancel( ) 
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


METHOD ButtonClick(oControlEvent) 

	LOCAL oControl AS OBJECT

	oControl := IIf(oControlEvent == NULL_OBJECT, NULL_OBJECT, oControlEvent:Control)

	SUPER:ButtonClick(oControlEvent)
	
	IF oControl:NameSym = #Authentication
		SELF:SetSmtpAuthentication(oControl:Checked)
	ENDIF

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
