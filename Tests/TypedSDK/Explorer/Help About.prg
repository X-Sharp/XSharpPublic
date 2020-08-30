#region DEFINES
STATIC DEFINE HELPABOUTDIALOG_LABEL1 := 100
STATIC DEFINE HELPABOUTDIALOG_LABEL2 := 101
STATIC DEFINE HELPABOUTDIALOG_ENDDIALOG := 102
#endregion

PARTIAL CLASS HelpAboutDialog INHERIT DIALOGWINDOW
	PROTECT oDCLabel1 AS FIXEDTEXT
	PROTECT oDCLabel2 AS FIXEDTEXT
	PROTECT oCCEndDialog AS PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  

CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"HelpAboutDialog" , _GetInst()} , TRUE)

	SELF:oDCLabel1 := FIXEDTEXT{SELF , ResourceID{ HELPABOUTDIALOG_LABEL1  , _GetInst() } }
	SELF:oDCLabel1:HyperLabel := HyperLabel{#Label1 , "CA-Visual Objects 2.5a" , NULL_STRING , NULL_STRING}

	SELF:oDCLabel2 := FIXEDTEXT{SELF , ResourceID{ HELPABOUTDIALOG_LABEL2  , _GetInst() } }
	SELF:oDCLabel2:HyperLabel := HyperLabel{#Label2 , "Explorer Sample" , NULL_STRING , NULL_STRING}

	SELF:oCCEndDialog := PUSHBUTTON{SELF , ResourceID{ HELPABOUTDIALOG_ENDDIALOG  , _GetInst() } }
	SELF:oCCEndDialog:HyperLabel := HyperLabel{#EndDialog , "OK" , NULL_STRING , NULL_STRING}

	SELF:Caption := "About Explorer Sample..."
	SELF:HyperLabel := HyperLabel{#HelpAboutDialog , "About Explorer Sample..." , NULL_STRING , NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


END CLASS
