// Application : _TesterMdi
// File1.prg , Created : 29/9/2018   12:31 pµ
// User : Cpc
PARTIAL CLASS DIALOGWINDOW1 INHERIT DIALOGWINDOW
	PROTECT oDCSingleLineEdit1 AS SINGLELINEEDIT

	// User code starts here (DO NOT remove this line)  ##USER##
	CONSTRUCTOR(oParent,uExtra)

		SELF:PreInit(oParent,uExtra)

		SUPER(oParent , ResourceID{"DIALOGWINDOW1" , _GetInst()} , TRUE)

		SELF:oDCSingleLineEdit1 := SINGLELINEEDIT{SELF , ResourceID{ DIALOGWINDOW1_SINGLELINEEDIT1  , _GetInst() } }
		SELF:oDCSingleLineEdit1:HyperLabel := HyperLabel{#SingleLineEdit1 , NULL_STRING , NULL_STRING , NULL_STRING}

		SELF:Caption := "Dialog Caption"
		SELF:HyperLabel := HyperLabel{#DIALOGWINDOW1 , "Dialog Caption" , NULL_STRING , NULL_STRING}

		SELF:PostInit(oParent,uExtra)

	RETURN

END CLASS
STATIC DEFINE DIALOGWINDOW1_SINGLELINEEDIT1 := 100
