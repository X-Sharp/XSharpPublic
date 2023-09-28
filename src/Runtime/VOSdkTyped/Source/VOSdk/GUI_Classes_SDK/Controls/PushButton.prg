//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/PushButton/*" />
CLASS PushButton INHERIT Button

    PROPERTY ControlType AS ControlType GET ControlType.Button

/// <include file="Gui.xml" path="doc/PushButton.ctor/*" />
	CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle)
		SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, FALSE)
		IF xID IS ResourceID
			SELF:SetStyle(BS_PushButton)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/PushButton.Value/*" />
	ACCESS Value()
		RETURN FALSE

/// <include file="Gui.xml" path="doc/PushButton.Value/*" />
	ASSIGN Value(uNewValue)
		RETURN

END CLASS

