// MenuCommands.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Runtime helpers for VFP procedural menu commands (DEFINE MENU, ON PAD, etc.).
// These functions are called by the UDCs in VFPMenuSupport.xh.

USING System
USING System.Collections.Generic
USING XSharp.VFP.UI

// ── Private helpers ──────────────────────────────────────────────────────────

// Returns the Bar in oPopup whose Name equals nBar.ToString(), or NULL.
FUNCTION __VFPFindBarByNumber( oPopup AS Popup, nBar AS INT ) AS Bar
    IF oPopup == NULL_OBJECT
        RETURN NULL_OBJECT
    ENDIF
    LOCAL cKey AS STRING
    cKey := nBar:ToString()
    LOCAL i AS INT
    FOR i := 1 TO (INT) oPopup:BarCount
        IF String.Compare( oPopup:Bars[i]:Name, cKey, TRUE ) == 0
            RETURN oPopup:Bars[i]
        ENDIF
    NEXT
    RETURN NULL_OBJECT

// Returns the first Pad on oMenu whose Name matches (case-insensitive).
FUNCTION __VFPFindPadByName( oMenu AS Menu, cPadName AS STRING ) AS Pad
    IF oMenu == NULL_OBJECT
        RETURN NULL_OBJECT
    ENDIF
    LOCAL i AS INT
    FOR i := 1 TO (INT) oMenu:PadCount
        IF String.Compare( oMenu:Pads[i]:Name, cPadName, TRUE ) == 0
            RETURN oMenu:Pads[i]
        ENDIF
    NEXT
    RETURN NULL_OBJECT

// ── DEFINE helpers ────────────────────────────────────────────────────────────

/// <summary>Implements DEFINE MENU &lt;name&gt; BAR. Creates and registers a new Menu.</summary>
FUNCTION __VFPDefineMenu( cName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu       := Menu{}
    oMenu:Name  := cName

/// <summary>Implements DEFINE PAD &lt;pad&gt; OF &lt;menu&gt; PROMPT &lt;caption&gt;. Adds a Pad to an existing Menu.</summary>
FUNCTION __VFPDefinePad( cPadName AS STRING, cMenuName AS STRING, cCaption AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu != NULL_OBJECT
        oMenu:AddPad( cPadName, cCaption )
    ENDIF

/// <summary>Implements DEFINE POPUP &lt;name&gt;. Creates and registers a new Popup.</summary>
FUNCTION __VFPDefinePopup( cName AS STRING ) AS VOID
    LOCAL oPopup AS Popup
    oPopup       := Popup{}
    oPopup:Name  := cName

/// <summary>Implements DEFINE POPUP &lt;name&gt; SHORTCUT. Creates and registers a new ContextMenu.</summary>
FUNCTION __VFPDefineContextMenu( cName AS STRING ) AS VOID
    LOCAL oCtx AS ContextMenu
    oCtx       := ContextMenu{}
    oCtx:Name  := cName

/// <summary>
/// Implements DEFINE BAR &lt;n&gt; OF &lt;popup&gt; PROMPT &lt;caption&gt;.
/// The bar number is stored as the bar's Name so ON SELECTION BAR can locate it later.
/// A PROMPT of "\-" inserts a separator.
/// </summary>
FUNCTION __VFPDefineBar( nBar AS INT, cPopupName AS STRING, cCaption AS STRING ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup != NULL_OBJECT
        LOCAL cActualCaption AS STRING
        cActualCaption := IIF( cCaption == "\-", "--", cCaption )
        oPopup:AddBar( nBar:ToString(), cActualCaption )
    ENDIF

// ── ON helpers ────────────────────────────────────────────────────────────────

/// <summary>
/// Implements ON PAD &lt;pad&gt; OF &lt;menu&gt; ACTIVATE POPUP &lt;popup&gt; (or empty to detach).
/// </summary>
FUNCTION __VFPOnPad( cPadName AS STRING, cMenuName AS STRING, cPopupName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oPad AS Pad
    oPad := __VFPFindPadByName( oMenu, cPadName )
    IF oPad == NULL_OBJECT
        RETURN
    ENDIF
    IF String.IsNullOrEmpty( cPopupName )
        oPad:Popup := NULL_OBJECT
    ELSE
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cPopupName )
        IF oPopup != NULL_OBJECT
            oPad:Popup := oPopup
        ENDIF
    ENDIF

/// <summary>
/// Implements ON BAR &lt;n&gt; OF &lt;popup&gt; ACTIVATE POPUP &lt;subPopup&gt; (or empty to detach).
/// </summary>
FUNCTION __VFPOnBar( nBar AS INT, cPopupName AS STRING, cSubPopupName AS STRING ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oBar AS Bar
    oBar := __VFPFindBarByNumber( oPopup, nBar )
    IF oBar == NULL_OBJECT
        RETURN
    ENDIF
    IF String.IsNullOrEmpty( cSubPopupName )
        oBar:Popup := NULL_OBJECT
    ELSE
        LOCAL oSub AS Popup
        oSub := Popup.Find( cSubPopupName )
        IF oSub != NULL_OBJECT
            oBar:Popup := oSub
        ENDIF
    ENDIF

/// <summary>Implements ON SELECTION PAD … DO &lt;procName&gt;.</summary>
FUNCTION __VFPOnSelectionPad( cPadName AS STRING, cMenuName AS STRING, cProc AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oPad AS Pad
    oPad := __VFPFindPadByName( oMenu, cPadName )
    IF oPad != NULL_OBJECT
        oPad:vfpClick := cProc
    ENDIF

/// <summary>Implements ON SELECTION PAD … &lt;arbitrary code&gt; (codeblock variant).</summary>
FUNCTION __VFPOnSelectionPadCmd( cPadName AS STRING, cMenuName AS STRING, block AS USUAL ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oPad AS Pad
    oPad := __VFPFindPadByName( oMenu, cPadName )
    IF oPad != NULL_OBJECT
        oPad:SetClickAction( block )
    ENDIF

/// <summary>Implements ON SELECTION BAR &lt;n&gt; OF &lt;popup&gt; DO &lt;procName&gt;.</summary>
FUNCTION __VFPOnSelectionBar( nBar AS INT, cPopupName AS STRING, cProc AS STRING ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oBar AS Bar
    oBar := __VFPFindBarByNumber( oPopup, nBar )
    IF oBar != NULL_OBJECT
        oBar:vfpClick := cProc
    ENDIF

/// <summary>Implements ON SELECTION BAR &lt;n&gt; OF &lt;popup&gt; &lt;arbitrary code&gt; (codeblock variant).</summary>
FUNCTION __VFPOnSelectionBarCmd( nBar AS INT, cPopupName AS STRING, block AS USUAL ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oBar AS Bar
    oBar := __VFPFindBarByNumber( oPopup, nBar )
    IF oBar != NULL_OBJECT
        oBar:SetClickAction( block )
    ENDIF

/// <summary>
/// Implements ON SELECTION POPUP &lt;popup&gt; | ALL DO &lt;procName&gt;.
/// When cPopupName is "*", applies to every registered popup.
/// </summary>
FUNCTION __VFPOnSelectionPopup( cPopupName AS STRING, cProc AS STRING ) AS VOID
    IF cPopupName == "*"
        __VFPOnSelectionPopupAll( NULL, cProc )
    ELSE
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cPopupName )
        IF oPopup != NULL_OBJECT
            __VFPSetPopupSelectionProc( oPopup, cProc )
        ENDIF
    ENDIF

/// <summary>
/// Implements ON SELECTION POPUP &lt;popup&gt; | ALL &lt;arbitrary code&gt; (codeblock variant).
/// </summary>
FUNCTION __VFPOnSelectionPopupCmd( cPopupName AS STRING, block AS USUAL ) AS VOID
    IF cPopupName == "*"
        __VFPOnSelectionPopupAll( block, NULL )
    ELSE
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cPopupName )
        IF oPopup != NULL_OBJECT
            __VFPSetPopupSelectionBlock( oPopup, block )
        ENDIF
    ENDIF

// Applies a string-based proc handler to every bar in one popup.
FUNCTION __VFPSetPopupSelectionProc( oPopup AS Popup, cProc AS STRING ) AS VOID
    LOCAL i AS INT
    FOR i := 1 TO (INT) oPopup:BarCount
        oPopup:Bars[i]:vfpClick := cProc
    NEXT

// Applies a codeblock handler to every bar in one popup.
FUNCTION __VFPSetPopupSelectionBlock( oPopup AS Popup, block AS USUAL ) AS VOID
    LOCAL i AS INT
    FOR i := 1 TO (INT) oPopup:BarCount
        oPopup:Bars[i]:SetClickAction( block )
    NEXT

// Applies proc/block to ALL registered popups.
// Exactly one of cProc / block should be non-null.
FUNCTION __VFPOnSelectionPopupAll( block AS USUAL, cProc AS STRING ) AS VOID
    LOCAL names AS List<STRING>
    names := Popup.GetAllNames()
    FOREACH VAR cName IN names
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cName )
        IF oPopup == NULL_OBJECT
            LOOP
        ENDIF
        IF cProc != NULL
            __VFPSetPopupSelectionProc( oPopup, cProc )
        ELSE
            __VFPSetPopupSelectionBlock( oPopup, block )
        ENDIF
    NEXT

// ── ACTIVATE / DEACTIVATE helpers ────────────────────────────────────────────

/// <summary>
/// Implements ACTIVATE MENU &lt;name&gt;.
/// Attaches the menu to the active top-level form if one is focused; otherwise to _Screen.
/// </summary>
FUNCTION __VFPActivateMenu( cName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cName )
    IF oMenu == NULL_OBJECT
        RETURN
    ENDIF
    // Prefer an open MDI form that is currently active, fall back to _Screen.
    LOCAL oTarget AS Form
    oTarget := NULL_OBJECT
    IF System.Windows.Forms.Application.OpenForms != NULL
        LOCAL i AS INT
        FOR i := 0 TO System.Windows.Forms.Application.OpenForms:Count - 1
            LOCAL f AS System.Windows.Forms.Form
            f := System.Windows.Forms.Application.OpenForms[i]
            IF f IS Form VAR vfpForm .AND. vfpForm:ContainsFocus
                oTarget := vfpForm
                EXIT
            ENDIF
        NEXT
    ENDIF
    IF oTarget == NULL_OBJECT
        oTarget := MainWindow.Current
    ENDIF
    IF oTarget != NULL_OBJECT
        oMenu:Activate( oTarget )
    ENDIF

/// <summary>
/// Implements ACTIVATE POPUP &lt;name&gt; [AT &lt;nRow&gt;, &lt;nCol&gt;].
/// Shows the popup as a context menu at the given screen coordinates.
/// Pass nRow = nCol = 0 to show at the current mouse position.
/// </summary>
FUNCTION __VFPActivatePopup( cName AS STRING, nRow AS INT, nCol AS INT ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cName )
    IF oPopup == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL pt AS System.Drawing.Point
    IF nRow == 0 .AND. nCol == 0
        pt := System.Windows.Forms.Control.MousePosition
    ELSE
        // VFP AT clause uses screen row/col in characters; approximate with mouse pos.
        pt := System.Drawing.Point{ nCol, nRow }
    ENDIF
    oPopup:Show( pt )

/// <summary>Implements DEACTIVATE MENU &lt;name&gt;. Detaches the menu from its host form.</summary>
FUNCTION __VFPDeactivateMenu( cName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cName )
    IF oMenu != NULL_OBJECT
        oMenu:Deactivate()
    ENDIF

/// <summary>
/// Implements DEACTIVATE POPUP [&lt;name&gt;].
/// Empty string deactivates the most recently activated popup (best-effort: closes all).
/// </summary>
FUNCTION __VFPDeactivatePopup( cName AS STRING ) AS VOID
    IF !String.IsNullOrEmpty( cName )
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cName )
        IF oPopup != NULL_OBJECT
            oPopup:Hide()
        ENDIF
    ELSE
        // No name given — close any open drop-down on _Screen's menu by
        // briefly toggling visibility, which collapses any open items.
        IF MainWindow.Current != NULL_OBJECT .AND. MainWindow.Current:ActiveMenu != NULL_OBJECT
            VAR oMenu := MainWindow.Current:ActiveMenu
            oMenu:Visible := FALSE
            oMenu:Visible := TRUE
        ENDIF
    ENDIF

// ── HIDE / SHOW helpers ───────────────────────────────────────────────────────

/// <summary>Implements HIDE MENU &lt;name&gt;.</summary>
FUNCTION __VFPHideMenu( cName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cName )
    IF oMenu != NULL_OBJECT
        oMenu:Visible := FALSE
    ENDIF

/// <summary>Implements SHOW MENU &lt;name&gt;.</summary>
FUNCTION __VFPShowMenu( cName AS STRING ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cName )
    IF oMenu != NULL_OBJECT
        oMenu:Visible := TRUE
    ENDIF

/// <summary>Implements HIDE POPUP [&lt;name&gt;]. Empty string hides all.</summary>
FUNCTION __VFPHidePopup( cName AS STRING ) AS VOID
    IF !String.IsNullOrEmpty( cName )
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cName )
        IF oPopup != NULL_OBJECT
            oPopup:Visible := FALSE
        ENDIF
    ENDIF

/// <summary>Implements SHOW POPUP [&lt;name&gt;].</summary>
FUNCTION __VFPShowPopup( cName AS STRING ) AS VOID
    IF !String.IsNullOrEmpty( cName )
        LOCAL oPopup AS Popup
        oPopup := Popup.Find( cName )
        IF oPopup != NULL_OBJECT
            oPopup:Visible := TRUE
        ENDIF
    ENDIF

// ── RELEASE helpers ───────────────────────────────────────────────────────────

/// <summary>
/// Implements RELEASE MENUS [&lt;list&gt;] [EXTENDED].
/// aNames = NULL releases all registered menus.
/// </summary>
FUNCTION __VFPReleaseMenus( aNames AS USUAL[], lExtended AS LOGIC ) AS VOID
    IF aNames == NULL .OR. aNames:Length == 0
        Menu.ReleaseAll()
    ELSE
        LOCAL i AS INT
        FOR i := 0 TO aNames:Length - 1
            LOCAL oMenu AS Menu
            oMenu := Menu.Find( (STRING) aNames[i] )
            IF oMenu != NULL_OBJECT
                oMenu:Release()
            ENDIF
        NEXT
    ENDIF

/// <summary>
/// Implements RELEASE POPUPS [&lt;list&gt;] [EXTENDED].
/// aNames = NULL releases all registered popups.
/// </summary>
FUNCTION __VFPReleasePopups( aNames AS USUAL[], lExtended AS LOGIC ) AS VOID
    IF aNames == NULL .OR. aNames:Length == 0
        Popup.ReleaseAll()
    ELSE
        LOCAL i AS INT
        FOR i := 0 TO aNames:Length - 1
            LOCAL oPopup AS Popup
            oPopup := Popup.Find( (STRING) aNames[i] )
            IF oPopup != NULL_OBJECT
                oPopup:Release()
            ENDIF
        NEXT
    ENDIF

// ── SET SKIP OF helpers ───────────────────────────────────────────────────────

/// <summary>Implements SET SKIP OF MENU &lt;name&gt; &lt;expr&gt;.</summary>
FUNCTION __VFPSetSkipOfMenu( cMenuName AS STRING, lSkip AS LOGIC ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu != NULL_OBJECT
        oMenu:Skip := lSkip
    ENDIF

/// <summary>Implements SET SKIP OF PAD &lt;pad&gt; OF &lt;menu&gt; &lt;expr&gt;.</summary>
FUNCTION __VFPSetSkipOfPad( cPadName AS STRING, cMenuName AS STRING, lSkip AS LOGIC ) AS VOID
    LOCAL oMenu AS Menu
    oMenu := Menu.Find( cMenuName )
    IF oMenu == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oPad AS Pad
    oPad := __VFPFindPadByName( oMenu, cPadName )
    IF oPad != NULL_OBJECT
        oPad:Skip := lSkip
    ENDIF

/// <summary>Implements SET SKIP OF POPUP &lt;name&gt; &lt;expr&gt;.</summary>
FUNCTION __VFPSetSkipOfPopup( cPopupName AS STRING, lSkip AS LOGIC ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup != NULL_OBJECT
        oPopup:Skip := lSkip
    ENDIF

/// <summary>Implements SET SKIP OF BAR &lt;n&gt; OF &lt;popup&gt; &lt;expr&gt;.</summary>
FUNCTION __VFPSetSkipOfBar( nBar AS INT, cPopupName AS STRING, lSkip AS LOGIC ) AS VOID
    LOCAL oPopup AS Popup
    oPopup := Popup.Find( cPopupName )
    IF oPopup == NULL_OBJECT
        RETURN
    ENDIF
    LOCAL oBar AS Bar
    oBar := __VFPFindBarByNumber( oPopup, nBar )
    IF oBar != NULL_OBJECT
        oBar:Skip := lSkip
    ENDIF

// ── SET SYSMENU helper ────────────────────────────────────────────────────────

/// <summary>Implements SET SYSMENU ON/OFF/AUTOMATIC.</summary>
FUNCTION __VFPSetSysMenu( lVisible AS LOGIC ) AS VOID
    IF MainWindow.Current != NULL_OBJECT .AND. MainWindow.Current:ActiveMenu != NULL_OBJECT
        MainWindow.Current:ActiveMenu:Visible := lVisible
    ENDIF
