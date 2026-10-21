// MenuFunctions.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// VFP-compatible menu information functions.
// BAR(), PAD(), POPUP(), MENU() read from MenuState, which Bar.OnVFPClick
// populates before dispatching each handler.
// MRKBAR/MRKPAD/PRMBAR/PRMPAD look up the named registry on Popup/Menu.

USING XSharp.VFP.UI

/// <summary>
/// Returns the 1-based number of the most recently selected menu bar item.
/// Returns 0 if no item was selected or the user pressed ESC.
/// </summary>
FUNCTION BAR() AS INT
	RETURN MenuState.LastBar

/// <summary>
/// Returns the name of the most recently selected menu pad (top-level title).
/// Returns an empty string if no pad was selected.
/// </summary>
FUNCTION PAD() AS STRING
	RETURN MenuState.LastPad

/// <summary>
/// Returns the name of the popup that contained the last selected bar.
/// Returns an empty string if no popup was active.
/// </summary>
FUNCTION POPUP() AS STRING
	RETURN MenuState.LastPopup

/// <summary>
/// Returns the name of the menu bar that was active when the last selection occurred.
/// Returns an empty string if no menu was active.
/// </summary>
FUNCTION MENU() AS STRING
	RETURN MenuState.LastMenu

/// <summary>
/// Returns .T. if the specified bar item is currently checked (marked).
/// <paramref name="cPopupName"/> — name of the Popup; <paramref name="nBar"/> — 1-based bar number.
/// </summary>
FUNCTION MRKBAR( cPopupName AS STRING, nBar AS INT ) AS LOGIC
	LOCAL oPopup := Popup.Find(cPopupName) AS Popup
	IF oPopup != NULL .AND. nBar >= 1 .AND. nBar <= oPopup:BarCount
		RETURN oPopup:Bars[nBar]:Checked
	ENDIF
	RETURN FALSE

/// <summary>
/// Returns .T. if the specified pad is currently checked (marked).
/// <paramref name="cMenuName"/> — name of the Menu; <paramref name="cPadName"/> — pad name.
/// </summary>
FUNCTION MRKPAD( cMenuName AS STRING, cPadName AS STRING ) AS LOGIC
	LOCAL oMenu := Menu.Find(cMenuName) AS Menu
	IF oMenu != NULL
		LOCAL i AS INT
		FOR i := 1 TO oMenu:PadCount
			IF String.Compare( oMenu:Pads[i]:Name, cPadName, TRUE ) == 0
				RETURN oMenu:Pads[i]:Checked
			ENDIF
		NEXT
	ENDIF
	RETURN FALSE

/// <summary>
/// Returns the prompt (caption) text of the specified bar item.
/// <paramref name="cPopupName"/> — name of the Popup; <paramref name="nBar"/> — 1-based bar number.
/// </summary>
FUNCTION PRMBAR( cPopupName AS STRING, nBar AS INT ) AS STRING
	LOCAL oPopup := Popup.Find(cPopupName) AS Popup
	IF oPopup != NULL .AND. nBar >= 1 .AND. nBar <= oPopup:BarCount
		RETURN oPopup:Bars[nBar]:Caption
	ENDIF
	RETURN ""

/// <summary>
/// Returns the prompt (caption) text of the specified pad.
/// <paramref name="cMenuName"/> — name of the Menu; <paramref name="cPadName"/> — pad name.
/// </summary>
FUNCTION PRMPAD( cMenuName AS STRING, cPadName AS STRING ) AS STRING
	LOCAL oMenu := Menu.Find(cMenuName) AS Menu
	IF oMenu != NULL
		LOCAL i AS INT
		FOR i := 1 TO oMenu:PadCount
			IF String.Compare( oMenu:Pads[i]:Name, cPadName, TRUE ) == 0
				RETURN oMenu:Pads[i]:Caption
			ENDIF
		NEXT
	ENDIF
	RETURN ""
