//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/GroupBox/*" />
[XSharp.Internal.TypesChanged];
CLASS GroupBox INHERIT TextControl
	/// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.GroupBox
	/// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oGroup := (VOGroupBox) oC
		oGroup:SendToBack()
		oGroup:VisibleChanged += OnVisibleChanged
		oGroup:IsRadioGroup  := SELF IS RadioButtonGroup

	/// <exclude />
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

	PROPERTY __GroupBox AS VOGroupBox GET (VOGroupBox) oCtrl

/// <include file="Gui.xml" path="doc/GroupBox.AsString/*" />
	METHOD AsString () as string strict
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

/// <include file="Gui.xml" path="doc/GroupBox.CurrentText/*" />
	PROPERTY CurrentText AS STRING GET NULL_STRING SET

/// <include file="Gui.xml" path="doc/GroupBox.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware)
		Default(REF lDataAware, FALSE)

		IF xID IS ResourceID
			SUPER(oOwner, xID, oPoint, oDimension, , , lDataAware)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, "Button", _Or(BS_GroupBox, WS_TabStop,WS_EX_TRANSPARENT), lDataAware)
			IF !IsNil(cText)
				SELF:Caption := cText
			ENDIF
		ENDIF

		RETURN

END CLASS

