// Application : _TesterMdi
// File2.prg , Created : 29/9/2018   12:36 pµ
// User : Cpc
PARTIAL CLASS DATAWINDOW1 INHERIT DATAWINDOW
	PROTECT oDCSingleLineEdit1 AS SINGLELINEEDIT
	PROTECT oDCCheckBox1 AS CHECKBOX

	// User code starts here (DO NOT remove this line)  ##USER##
	CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)

		SELF:PreInit(oWindow,iCtlID,oServer,uExtra)

		SUPER(oWindow , ResourceID{"DATAWINDOW1" , _GetInst()},iCtlID)

		SELF:oDCSingleLineEdit1 := SINGLELINEEDIT{SELF , ResourceID{ DATAWINDOW1_SINGLELINEEDIT1  , _GetInst() } }
		SELF:oDCSingleLineEdit1:HyperLabel := HyperLabel{#SingleLineEdit1 , NULL_STRING , NULL_STRING , NULL_STRING}

		SELF:oDCCheckBox1 := CHECKBOX{SELF , ResourceID{ DATAWINDOW1_CHECKBOX1  , _GetInst() } }
		SELF:oDCCheckBox1:HyperLabel := HyperLabel{#CheckBox1 , "CheckBox" , NULL_STRING , NULL_STRING}

		SELF:Caption := "DataWindow Caption"
		SELF:HyperLabel := HyperLabel{#DATAWINDOW1 , "DataWindow Caption" , NULL_STRING , NULL_STRING}
		IF !IsNil(oServer)
			SELF:Use(oServer)
		ENDIF


		SELF:PostInit(oWindow,iCtlID,oServer,uExtra)

	RETURN


	ACCESS SingleLineEdit1
	RETURN SELF:FieldGet( #SingleLineEdit1 )

	ASSIGN SingleLineEdit1( uValue )
	SELF:FieldPut( #SingleLineEdit1 , uValue )

	ACCESS CheckBox1
	RETURN SELF:FieldGet( #CheckBox1 )

	ASSIGN CheckBox1( uValue )
	SELF:FieldPut( #CheckBox1 , uValue )

END CLASS
STATIC DEFINE DATAWINDOW1_SINGLELINEEDIT1 := 100
STATIC DEFINE DATAWINDOW1_CHECKBOX1 := 101
