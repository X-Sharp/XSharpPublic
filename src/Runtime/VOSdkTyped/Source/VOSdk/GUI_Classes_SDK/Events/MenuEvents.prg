//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// MenuEvents.prg




USING System.Diagnostics

/// <include file="Gui.xml" path="doc/MenuEvent/*" />
CLASS MenuEvent INHERIT @@Event IMPLEMENTS INamedEvent
	PROTECT oMenu AS Menu
	PROTECT nID AS LONG

	METHOD AsString() AS STRING STRICT
		RETURN SELF:HyperLabel:Caption

	ACCESS HyperLabel AS HyperLabel STRICT
		LOCAL oHyperLabel AS HyperLabel
		IF oMenu != NULL_OBJECT
			oHyperLabel := oMenu:HyperLabel(nID)
			IF oHyperLabel == NULL
				oHyperLabel := HyperLabel{#Item}
			ENDIF
		ENDIF
		RETURN oHyperLabel

	[DebuggerStepThrough];
	CONSTRUCTOR(uMenu AS USUAL, uWin AS USUAL, uID AS USUAL) STRICT
		SUPER()
		if uMenu is Menu var oM
			self:oMenu		:= oM
		ENDIF
		if uWin is Window var oW
			oWindow		:= oW
		ENDIF
		IF IsLong(uID)
			SELF:nID := uID
		ENDIF
		RETURN

	ACCESS ItemID AS LONGINT STRICT
		RETURN nID

/// <include file="Gui.xml" path="doc/MenuSelectEvent.Menu/*" />
	ACCESS Menu AS Menu
		RETURN oMenu

/// <include file="Gui.xml" path="doc/MenuSelectEvent.Name/*" />
	ACCESS Name AS STRING STRICT
		LOCAL retVal AS STRING
		IF SELF:HyperLabel != NULL_OBJECT
			retVal := SELF:HyperLabel:Name
		ENDIF
		RETURN retVal

	ACCESS NameSym AS SYMBOL STRICT
		LOCAL retVal AS SYMBOL
		IF SELF:HyperLabel != NULL_OBJECT
			retVal := SELF:HyperLabel:NameSym
		ENDIF
		RETURN retVal

END CLASS


CLASS MenuCommandEvent INHERIT MenuEvent
	[DebuggerStepThrough];
	CONSTRUCTOR(uMenu AS USUAL, uWin AS USUAL, uID AS USUAL)
		SUPER(uMenu, uWin, uID)

END CLASS
/// <include file="Gui.xml" path="doc/MenuSelectEvent/*" />
CLASS MenuSelectEvent INHERIT MenuEvent
	[DebuggerStepThrough];
	CONSTRUCTOR(uMenu AS USUAL, uWin AS USUAL, uID AS USUAL) STRICT
		SUPER(uMenu, uWin, uID)

END CLASS

/// <include file="Gui.xml" path="doc/MenuInitEvent/*" />
CLASS MenuInitEvent INHERIT MenuEvent

	[DebuggerStepThrough];
	CONSTRUCTOR(uMenu AS USUAL, uWin AS USUAL, uID AS USUAL) STRICT
		SUPER(uMenu, uWin, uID)

END CLASS

