/// <include file="Gui.xml" path="doc/GroupBox/*" />
[XSharp.Internal.TypesChanged];
CLASS GroupBox INHERIT TextControl
	/// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.GroupBox
	/// <inheritdoc />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oGroup := (IVOGroupBox) oC
		oGroup:SendToBack()
		oGroup:VisibleChanged += OnVisibleChanged
		oGroup:IsRadioGroup  := SELF IS RadioButtonGroup

	/// <inheritdoc />
	METHOD OnVisibleChanged(sender AS OBJECT, e AS EventArgs) AS VOID
        // Set timer to refresh all child controls after 1 second
		IF oCtrl != NULL_OBJECT .AND. oCtrl:Visible
			SELF:RegisterTimer(1, TRUE)
		ENDIF

	/// <inheritdoc />
	METHOD Timer() AS USUAL CLIPPER
		IF oCtrl != NULL_OBJECT
			FOREACH oChild AS System.Windows.Forms.Control IN oCtrl:Controls
				oChild:Refresh()
			NEXT
		ENDIF
		RETURN NIL

	ACCESS __GroupBox AS IVOGroupBox
		RETURN (IVOGroupBox) oCtrl

/// <include file="Gui.xml" path="doc/GroupBox.AsString/*" />
	METHOD AsString ()
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

/// <include file="Gui.xml" path="doc/GroupBox.CurrentText/*" />
	ACCESS CurrentText AS STRING
		RETURN NULL_STRING

/// <include file="Gui.xml" path="doc/GroupBox.CurrentText/*" />
	ASSIGN CurrentText(cValue AS STRING)
		RETURN

/// <include file="Gui.xml" path="doc/GroupBox.ctor/*" />
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

