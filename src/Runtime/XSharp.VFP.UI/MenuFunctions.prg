// MenuFunctions.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// VFP-compatible menu information functions.
// BAR(), PAD(), POPUP(), MENU() read from MenuState, which Bar.OnVFPClick
// populates before dispatching each handler.
// MRKBAR/MRKPAD/PRMBAR/PRMPAD, CNTBAR/CNTPAD and SKPBAR/SKPPAD look up the
// named registry on Popup/Menu. BARCOUNT/BARPROMPT are the dBASE names of
// CNTBAR/PRMBAR.

USING XSharp.VFP.UI

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/bar/*" />
[FoxProFunction("BAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION BAR() AS INT
	RETURN MenuState.LastBar

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/pad/*" />
[FoxProFunction("PAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION PAD() AS STRING
	RETURN MenuState.LastPad

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/popup/*" />
[FoxProFunction("POPUP", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION POPUP() AS STRING
	RETURN MenuState.LastPopup

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/menu/*" />
[FoxProFunction("MENU", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION MENU() AS STRING
	RETURN MenuState.LastMenu

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mrkbar/*" />
[FoxProFunction("MRKBAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION MRKBAR( cPopupName AS STRING, nBar AS INT ) AS LOGIC
	// nBar is the number given in DEFINE BAR, not the position: a separator or a gap
	// in the numbering must not shift the result
	LOCAL oBar := __VFPFindBarByNumber( __VFPFindPopupContainer( cPopupName ), nBar ) AS Bar
	IF oBar != NULL
		RETURN oBar:Checked
	ENDIF
	RETURN FALSE

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mrkpad/*" />
[FoxProFunction("MRKPAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
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

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/prmbar/*" />
[FoxProFunction("PRMBAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION PRMBAR( cPopupName AS STRING, nBar AS INT ) AS STRING
	LOCAL oBar := __VFPFindBarByNumber( __VFPFindPopupContainer( cPopupName ), nBar ) AS Bar
	IF oBar != NULL
		RETURN MenuPrompt.WithoutHotkey( oBar:Caption )
	ENDIF
	RETURN ""

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/prmpad/*" />
[FoxProFunction("PRMPAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION PRMPAD( cMenuName AS STRING, cPadName AS STRING ) AS STRING
	LOCAL oMenu := Menu.Find(cMenuName) AS Menu
	IF oMenu != NULL
		LOCAL i AS INT
		FOR i := 1 TO oMenu:PadCount
			IF String.Compare( oMenu:Pads[i]:Name, cPadName, TRUE ) == 0
				RETURN MenuPrompt.WithoutHotkey( oMenu:Pads[i]:Caption )
			ENDIF
		NEXT
	ENDIF
	RETURN ""

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cntbar/*" />
[FoxProFunction("CNTBAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CNTBAR( cPopupName AS STRING ) AS INT
	LOCAL nCount := 0 AS INT
	IF String.IsNullOrEmpty( cPopupName )
		RETURN 0
	ENDIF
	// the separators are items of the popup but not Bar objects, so go through the items
	IF __VFPFindPopupContainer( cPopupName ) IS System.Windows.Forms.ToolStrip VAR oStrip
		LOCAL i AS INT
		FOR i := 0 TO oStrip:Items:Count - 1
			LOCAL nNumber AS INT
			// a bar created from code without a numeric name counts by its position
			IF ! Int32.TryParse( oStrip:Items[i]:Name, OUT nNumber )
				nNumber := i + 1
			ENDIF
			nCount := Math.Max( nCount, nNumber )
		NEXT
	ENDIF
	RETURN nCount

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cntpad/*" />
[FoxProFunction("CNTPAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CNTPAD( cMenuName AS STRING ) AS INT
	IF String.IsNullOrEmpty( cMenuName )
		RETURN 0
	ENDIF
	LOCAL oMenu := Menu.Find( cMenuName ) AS Menu
	IF oMenu != NULL
		RETURN oMenu:PadCount
	ENDIF
	RETURN 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/skpbar/*" />
[FoxProFunction("SKPBAR", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SKPBAR( cMenuName AS STRING, MenuItemNumber AS INT ) AS LOGIC
	IF String.IsNullOrEmpty( cMenuName ) .OR. MenuItemNumber < 1 .OR. MenuItemNumber > CNTBAR( cMenuName )
		RETURN FALSE
	ENDIF
	LOCAL oPopup := __VFPFindPopupContainer( cMenuName ) AS IVfpPopupContainer
	LOCAL oBar := __VFPFindBarByNumber( oPopup, MenuItemNumber ) AS Bar
	// a separator and a gap in the numbering are not Bar objects, and VFP reports
	// both as skipped. SET SKIP OF POPUP skips every bar of the popup
	RETURN oBar == NULL .OR. oPopup:Skip .OR. oBar:Skip

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/skppad/*" />
[FoxProFunction("SKPPAD", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SKPPAD( cMenuBarName AS STRING, cMenuTitleName AS STRING ) AS LOGIC
	IF String.IsNullOrEmpty( cMenuBarName )
		RETURN FALSE
	ENDIF
	LOCAL oMenu := Menu.Find( cMenuBarName ) AS Menu
	LOCAL oPad := __VFPFindPadByName( oMenu, cMenuTitleName ) AS Pad
	IF oPad == NULL
		RETURN FALSE
	ENDIF
	// SET SKIP OF MENU skips every pad of the menu
	RETURN oMenu:Skip .OR. oPad:Skip

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/barcount/*" />
[FoxProFunction("BARCOUNT", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION BARCOUNT( cPopupName := NULL AS STRING ) AS INT
	// the dBASE name of CNTBAR(). Without a name it takes the active popup
	IF cPopupName == NULL
		cPopupName := POPUP()
	ENDIF
	RETURN CNTBAR( cPopupName )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/barprompt/*" />
[FoxProFunction("BARPROMPT", FoxFunctionCategory.UIAndWindow, FoxEngine.UI, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION BARPROMPT( nBar AS INT, cPopupName := NULL AS STRING ) AS STRING
	// the dBASE name of PRMBAR(), with the bar first. Without a name it takes the active popup
	IF cPopupName == NULL
		cPopupName := POPUP()
	ENDIF
	RETURN PRMBAR( cPopupName, nBar )


/// <summary>The prompt of a bar or pad the way PRMBAR() and PRMPAD() report it.</summary>
INTERNAL STATIC CLASS MenuPrompt

	// VFP keeps the caption with its \< hotkey marker, but PRMBAR() and PRMPAD() return
	// the text without it, so "\<Open" comes back as "Open".
	INTERNAL STATIC METHOD WithoutHotkey( cCaption AS STRING ) AS STRING
		IF String.IsNullOrEmpty( cCaption )
			RETURN ""
		ENDIF
		RETURN cCaption:Replace( "\<", "" )
	END METHOD

END CLASS
