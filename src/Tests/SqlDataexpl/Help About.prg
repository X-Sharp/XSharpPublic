#region DEFINES
STATIC DEFINE HELPABOUT_ABOUTTEXT := 100
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 101
STATIC DEFINE HELPABOUT_FIXEDBITMAP1 := 102
STATIC DEFINE HELPABOUT_ICONDB := 103
#endregion

class HelpAbout inherit DIALOGWINDOW
	PROTECT oDCAboutText AS FIXEDTEXT
	PROTECT oCCPushButton1 AS PUSHBUTTON
	PROTECT oDCFixedBitmap1 AS FIXEDBITMAP

	// {{%UC%}} User code starts here (DO NOT remove this line)

CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"HelpAbout" , _GetInst()} , TRUE)

	SELF:oDCAboutText := FIXEDTEXT{SELF , ResourceID{ HELPABOUT_ABOUTTEXT  , _GetInst() } }
	SELF:oDCAboutText:HyperLabel := HyperLabel{#AboutText , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCPushButton1 := PUSHBUTTON{SELF , ResourceID{ HELPABOUT_PUSHBUTTON1  , _GetInst() } }
	SELF:oCCPushButton1:HyperLabel := HyperLabel{#PushButton1 , "OK" , NULL_STRING , NULL_STRING}

	SELF:oDCFixedBitmap1 := FIXEDBITMAP{SELF , ResourceID{ HELPABOUT_FIXEDBITMAP1  , _GetInst() } }
	SELF:oDCFixedBitmap1:HyperLabel := HyperLabel{#FixedBitmap1 , "POWVOBMP" , NULL_STRING , NULL_STRING}

	SELF:Caption := "About Simple DataExplorer"
	SELF:HyperLabel := HyperLabel{#HelpAbout , "About Simple DataExplorer" , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD PostInit(oParent,uExtra)
	oDCAboutText:CurrentText := Version() +_Chr(13) ;
															+"Simple DataExplorer Application "+_Chr(13)+_Chr(13)

    var conn := SqlDbGetConnection("DEFAULT")
    oDCAboutText:CurrentText += "Connection String: "+ conn:ConnectionString+ _Chr(13)
    oDCAboutText:CurrentText += "Server Version: "+conn:DbConnection:ServerVersion+ _Chr(13)

	RETURN NIL


method PushButton1()

	self:EndDialog()

	RETURN SELF

END CLASS
