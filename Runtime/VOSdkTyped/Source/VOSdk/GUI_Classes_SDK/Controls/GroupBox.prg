

CLASS GroupBox INHERIT TextControl


    PROPERTY ControlType AS ControlType GET ControlType.GroupBox

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oGroup := (VOGroupBox) oC
		oGroup:SendToBack()
		oGroup:VisibleChanged += OnVisibleChanged
		oGroup:IsRadioGroup  := SELF IS RadioButtonGroup


	METHOD OnVisibleChanged(sender AS OBJECT, e AS EventArgs) AS VOID
		IF oCtrl != NULL_OBJECT .and. oCtrl:Visible
			SELF:RegisterTimer(1, TRUE)
			
		ENDIF
		
	METHOD Timer() AS USUAL CLIPPER
		IF oCtrl != NULL_OBJECT
			FOREACH oChild AS System.Windows.Forms.Control IN oCtrl:Controls
				oChild:Refresh()
			NEXT
		ENDIF
		RETURN NIL

	ACCESS __GroupBox AS VOGroupBox
		RETURN (VOGroupBox) oCtrl
	
	METHOD AsString () 
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

	ACCESS CurrentText AS STRING
		RETURN NULL_STRING

	ASSIGN CurrentText(cValue AS STRING) 
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
		Default(@lDataAware, FALSE)

		IF IsInstanceOfUsual(xID,#ResourceID)
			SUPER(oOwner, xID, oPoint, oDimension, , , lDataAware)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, "Button", _Or(BS_GroupBox, WS_TabStop,WS_EX_TRANSPARENT), lDataAware)
			IF !IsNil(cText)
				SELF:Caption := cText
			ENDIF
		ENDIF

		RETURN 

END CLASS

