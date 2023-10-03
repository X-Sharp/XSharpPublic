//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic


CLASS SplitView INHERIT Control
	PROTECT	dwDeferPaintCount	AS DWORD
	PROTECT	oPanes			 	AS Dimension
	PROTECT	oBackgroundBrush	AS Brush
	PROTECT	oBarBrush		 	AS Brush
	PROTECT	oBarFrameBrush		AS Brush
    PROTECT _aSplits            AS List<VOSplitContainer>
    PROTECT _aPanes             AS List<IGuiObject>

	/// <exclude />
    PROPERTY ControlType    AS ControlType GET ControlType.SplitContainer
    PROPERTY HorizontalPanes AS LONG GET oPanes:Width
    PROPERTY VerticalPanes   AS LONG GET oPanes:Height

    PRIVATE PROPERTY PaneCount AS LONG GET HorizontalPanes * VerticalPanes

    PRIVATE METHOD __AdjustSize() AS VOID
        IF SELF:_aPanes != NULL
            DO WHILE SELF:_aPanes:Count > SELF:PaneCount
                SELF:_aPanes:Remove(SELF:_aPanes[SELF:_aPanes:Count- __ARRAYBASE__])
            ENDDO
            DO WHILE SELF:_aPanes:Count < SELF:PaneCount
                SELF:_aPanes:Add(NULL)
            ENDDO
        ENDIF
        VAR oMain := (VOSplitContainer) oCtrl
        IF oMain != NULL
            LOCAL oChild AS VOSplitContainer
            _aSplits:Clear()
            _aSplits:Add(oMain)
            IF SELF:PaneCount == 2
                IF HorizontalPanes == 2
                    oMain:Orientation := System.Windows.Forms.Orientation.Vertical
                ELSE
                    oMain:Orientation := System.Windows.Forms.Orientation.Horizontal
                ENDIF
            ELSE
                // Some common scenarios
                SWITCH PaneCount
                CASE 3

                    IF HorizontalPanes == 3
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Vertical)
                        oChild:IsSplitterFixed := !SELF:HorizontalDrag
                    ELSE
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:VerticalDrag
                    ENDIF

                    oMain:Panel1:Controls:Add(oChild)

                CASE 4
                    oMain:Orientation := System.Windows.Forms.Orientation.Vertical
                    IF HorizontalPanes == 2 .AND. VerticalPanes == 2
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:VerticalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:VerticalDrag
                        oMain:Panel2:Controls:Add(oChild)
                        IF ! SELF:VerticalDrag
                            oMain:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
                            oMain:Panel1:AutoSize := FALSE
                        ENDIF
                    ELSEIF HorizontalPanes == 4 .AND. VerticalPanes == 1
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Vertical)
                        oChild:IsSplitterFixed := !SELF:HorizontalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Vertical)
                        oChild:IsSplitterFixed := !SELF:HorizontalDrag
                        oMain:Panel2:Controls:Add(oChild)
                        IF ! SELF:HorizontalDrag
                            oMain:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
                            oMain:Panel1:AutoSize := FALSE
                        ENDIF
                    ELSE // VerticalPanes == 4 .and HorizontalPanes == 1
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:VerticalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:VerticalDrag
                        oMain:Panel2:Controls:Add(oChild)
                        IF ! SELF:VerticalDrag
                            oMain:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
                            oMain:Panel1:AutoSize := FALSE
                        ENDIF
                    ENDIF
                OTHERWISE
                    NOP
                END SWITCH
            ENDIF
        ENDIF

    PRIVATE METHOD __GetPanel(nPanel AS LONG) AS System.Windows.Forms.SplitterPanel
        SWITCH nPanel
        CASE 1
            SWITCH SELF:PaneCount
            CASE 2
            CASE 3
                RETURN SELF:_aSplits[0]:Panel1
            CASE 4
                RETURN SELF:_aSplits[1]:Panel1
            END SWITCH
        CASE 2
            SWITCH SELF:PaneCount
            CASE 2
                RETURN SELF:_aSplits[0]:Panel2
            CASE 3
                RETURN SELF:_aSplits[1]:Panel1
            CASE 4
                RETURN SELF:_aSplits[2]:Panel1
            END SWITCH
        CASE 3
            SWITCH SELF:PaneCount
            CASE 3
                RETURN SELF:_aSplits[1]:Panel2
            CASE 4
                RETURN SELF:_aSplits[1]:Panel2
            END SWITCH
        CASE 4
            SWITCH SELF:PaneCount
            CASE 4
                RETURN SELF:_aSplits[2]:Panel2
            END SWITCH
        END SWITCH
        RETURN NULL_OBJECT


    PRIVATE METHOD __GetPaneObject(oObject AS IGUIObject) AS IVOUIObject
        IF oObject IS Control VAR oCtrl
            RETURN oCtrl:__Control ASTYPE IVOUIObject
        ELSEIF oObject IS DataWindow VAR oDW
            RETURN oDw:__Surface
        ELSEIF oObject IS DialogWindow VAR oDlg
            RETURN oDlg:__Surface
        ENDIF
        RETURN NULL_OBJECT


    METHOD __SetPanelClient(oPanel AS System.Windows.Forms.SplitterPanel, oObject AS IGuiObject) AS VOID
        IF oPanel != NULL_OBJECT
            VAR oObj := SELF:__GetPaneObject(oObject)
            oPanel:Controls:Clear()
            IF oObj != NULL
                oPanel:Controls:Add((System.Windows.Forms.Control) oObj)
                oObj:Dock := System.Windows.Forms.DockStyle.Fill
            ENDIF
        ENDIF


    PRIVATE METHOD __SetPanes() AS VOID
        VAR oMain := SELF:_aSplits[0]
        oMain:SuspendLayout()
        FOR VAR nI :=1 TO SELF:PaneCount
            SELF:__SetPanelClient(SELF:__GetPanel(nI), SELF:_aPanes[nI-1])
        NEXT
        oMain:ResumeLayout(TRUE)
        oMain:PerformLayout()
        RETURN

    PRIVATE METHOD _AddControl(nOrientation AS System.Windows.Forms.Orientation) AS VOSplitContainer
        VAR oCtrl := (VOSplitContainer) GuiFactory.Instance:CreateControl(SELF:ControlType, SELF, 0, 0)
        oCtrl:Orientation := nOrientation
        oCtrl:SplitterMoved  += SplitterMoved
        oCtrl:SplitterMoving += SplitterMoving
        oCtrl:IsSplitterFixed := TRUE
        oCtrl:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
        SELF:_aSplits:Add(oCtrl)
        RETURN oCtrl

    METHOD SplitterMoved(sender AS OBJECT, e AS System.Windows.Forms.SplitterEventArgs) AS VOID
        RETURN

    METHOD SplitterMoving(sender AS OBJECT, e AS System.Windows.Forms.SplitterCancelEventArgs ) AS VOID
        VAR oSplit := (VOSplitContainer) sender
        IF oSplit:Orientation == System.Windows.Forms.Orientation.Vertical
            IF ! SELF:VerticalDrag
                e:Cancel := TRUE
            ENDIF
        ELSE
            IF ! SELF:HorizontalDrag
                 e:Cancel := TRUE
            ENDIF
        ENDIF
        RETURN

	method Create() as System.Windows.Forms.Control strict
		VAR oMain := SUPER:Create()
        SELF:__AdjustSize()
        RETURN oMain


	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment)
		LOCAL oWin AS Window

		// the owner must be a window
		IF !(oOwner IS Window )
			WCError{#Init, #SplitView, __WCSTypeError, oOwner, 1}:Throw()
		ENDIF
		oWin := oOwner

        DEFAULT( REF lHorizontalDrag, FALSE)
        DEFAULT( REF lVerticalDrag , FALSE)
        DEFAULT( REF kAlignment, SPLIT_VERTALIGN)

        SELF:HorizontalDrag    := lHorizontalDrag
        SELF:VerticalDrag      := lVerticalDrag
        SELF:HorizontalAlign   := _AND(kAlignment, SPLIT_HORZALIGN ) != 0
        SELF:VerticalAlign     := _AND(kAlignment, SPLIT_VERTALIGN ) != 0
        SELF:Layout  := Dimension{2,1}
        SELF:_aPanes := List<IGuiObject>{}
        SELF:_aSplits:= List<VOSplitContainer>{}

		SUPER(oWin, xID, oPoint, oDimension, "", _OR(WS_VISIBLE, WS_CHILD))

		RETURN

	METHOD __EditChange() AS VOID STRICT
		SELF:Modified := TRUE
		RETURN

	PROPERTY Background AS Brush GET oBackgroundBrush SET SELF:ChangeBackground(value, SPLTCOLOR_WINDOW)

	METHOD ChangeBackground(oBrush AS Brush, kWhere := 0 AS LONG)  AS VOID
        // Todo SplitView.ChangeBackGround
		// change the particular color using the supplied brush,
		// or the appropriate system color if the brush is null
        LOCAL dwNewColor as DWORD
		SWITCH kWhere
		CASE SPLTCOLOR_WINDOW
			oBackgroundBrush := oBrush
			IF oBackgroundBrush == NULL_OBJECT
                dwNewColor := GuiWin32.GetSysColor(COLOR_APPWORKSPACE)
			ENDIF

		CASE SPLTCOLOR_BAR
			oBarBrush := oBrush
			IF oBarBrush == NULL_OBJECT
				IF _AND(GuiWin32.GetVersion(), 0X80000000) != 0
					// Use Windows95 constants
					dwNewColor := GuiWin32.GetSysColor(COLOR_3DFACE)
				ELSE
					// Use WindowsNT constants
					dwNewColor := GuiWin32.GetSysColor(COLOR_BTNFACE)
                ENDIF
			ENDIF

		CASE SPLTCOLOR_BARFRAME
			oBarFrameBrush := oBrush
			IF oBarFrameBrush == NULL_OBJECT
				IF _AND(GuiWin32.GetVersion(), 0X80000000) != 0
					// Use Windows95 constants
					dwNewColor := GuiWin32.GetSysColor(COLOR_3DFACE)
				ELSE
					// Use WindowsNT constants
					dwNewColor := GuiWin32.GetSysColor(COLOR_BTNFACE)
				ENDIF
			ENDIF
		END SWITCH

		IF oBrush != NULL_OBJECT
			dwNewColor := (Color) oBrush
		ENDIF
		//PCALL(gpfnSpltColorSet, SELF:Handle(), kWhere, INT(_CAST, dwNewColor))
        FOREACH var Split in _aSplits
            Split:ForeColor := Color{dwNewColor}
        NEXT

		RETURN

	PROPERTY deferPaintCount AS DWORD GET SELF:dwDeferPaintCount

	METHOD Destroy() AS USUAL clipper
		LOCAL liPane  AS LONGINT
		LOCAL liCount AS LONGINT

		IF SELF:_aPanes != NULL
            FOREACH VAR oPane IN SELF:_aPanes
                IF oPane != NULL
                    oPane:Destroy()
                ENDIF
            NEXT
        ENDIF
        IF SELF:_aSplits != NULL
            FOREACH VAR oSplit IN SELF:_aSplits
                IF oSplit != NULL
                    oSplit:Dispose()
                ENDIF
            NEXT
        ENDIF
        _aPanes         := NULL_OBJECT
        _aSplits        := NULL_OBJECT
		oPanes          := NULL_OBJECT
		oBackgroundBrush := NULL_OBJECT
		oBarBrush       := NULL_OBJECT
		oBarFrameBrush  := NULL_OBJECT

		SUPER:Destroy()

		RETURN SELF

	METHOD Dispatch(oEvent  AS @@Event)
		//LOCAL oSize 	AS Dimension
		//LOCAL lResize 	AS LOGIC
		//LOCAL oEvt := oEvent AS @@Event

		//IF (oEvt:Message == WM_CAPTURECHANGED)
		//	oSize := SELF:GetPaneSize(1)
		//	IF (oSize != NULL_OBJECT .and. oSize:Height <= 1)
		//		oSize:Height := 2
		//		lResize := TRUE
		//	ENDIF
		//	IF (oSize != NULL_OBJECT .and. oSize:Width <= 1)
		//		oSize:Width := 2
		//		lResize := TRUE
		//	ENDIF
		//	IF (lResize)
		//		SELF:SetPaneSize(oSize, 1)
		//	ENDIF
		//ENDIF

		RETURN SUPER:Dispatch(oEvent)

	METHOD GetAllPaneClients(aChildren := NULL_ARRAY AS ARRAY)  AS ARRAY
		LOCAL aPanes  AS ARRAY
		IF aChildren == NULL_ARRAY
			aPanes := {}
		ELSE
            aPanes := aChildren
		ENDIF
        FOREACH VAR oPane IN SELF:_aPanes
            AAdd(aPanes, oPane)
        NEXT
		RETURN aPanes

	METHOD GetPaneClient(nPane AS LONG) AS IGuiObject
		LOCAL oRet AS OBJECT
        IF nPane <= SELF:_aPanes:Count
            RETURN SELF:_aPanes[nPane- __ARRAYBASE__]
        ENDIF
		RETURN NULL_OBJECT

	METHOD GetPaneSize(nPane AS LONG) AS Dimension
		LOCAL oRet AS OBJECT
        IF nPane <= SELF:_aPanes:Count
            VAR panel := SELF:__GetPanel(nPane)
            RETURN panel:Size
        ENDIF
		RETURN NULL_OBJECT

	METHOD HidePane(nPane := -1 AS LONG)  AS VOID
		LOCAL oRet AS OBJECT
        IF nPane > 0
            IF nPane <= SELF:_aPanes:Count
                SELF:_aPanes[nPane- __ARRAYBASE__]:Hide()
            ENDIF
        ELSE
            FOREACH VAR oPane IN SELF:_aPanes
                oPane:Hide()
            NEXT
        ENDIF
        SELF:__SetPanes()
		RETURN

	PROPERTY HorizontalAlign AS LOGIC AUTO GET PRIVATE SET
	PROPERTY HorizontalDrag  AS LOGIC AUTO GET PRIVATE SET


    PROPERTY Layout AS Dimension
    GET
		RETURN oPanes
    END GET
    SET
		oPanes := value
        SELF:__AdjustSize()
		RETURN
    END SET
    END PROPERTY
	METHOD RestoreUpdate() AS VOID STRICT
		IF (dwDeferPaintCount != 0)
			--dwDeferPaintCount
		ENDIF
        IF dwDeferPaintCount == 0
            FOREACH VAR oSplit IN SELF:_aSplits
                oSplit:ResumeLayout(TRUE)
            NEXT
        ENDIF
		RETURN

	METHOD SetPaneClient(oWindow AS IGuiObject, nPane AS LONG )  AS VOID
        IF nPane > 0 .AND. nPane <= SELF:PaneCount
            SELF:_aPanes[nPane- __ARRAYBASE__] := oWindow
        ENDIF
        SELF:__SetPanes()
        oWindow:Show()
		RETURN

	METHOD SetPaneSize(oDimension AS Dimension, nPane AS LONG)  AS LOGIC
		IF nPane > 0 .AND. nPane <= SELF:_aPanes:Count
            VAR panel := SELF:__GetPanel(nPane)
            panel:Size := oDimension
            VAR split := (VOSplitContainer) panel:Parent
            IF panel == split:Panel1 .AND. split:Orientation == System.Windows.Forms.Orientation.Vertical
                split:SplitterDistance := oDimension:Width
            ELSEIF panel == split:Panel1 .AND. split:Orientation == System.Windows.Forms.Orientation.Horizontal
                split:SplitterDistance := oDimension:Height
            ELSEIF panel == split:Panel2 .AND. split:Orientation == System.Windows.Forms.Orientation.Vertical
                split:SplitterDistance := split:Width - oDimension:Width
            ELSEIF panel == split:Panel2 .AND. split:Orientation == System.Windows.Forms.Orientation.Horizontal
                split:SplitterDistance := split:Height - oDimension:Height
            ENDIF
        ENDIF
        SELF:__SetPanes()
		RETURN FALSE

	METHOD ShowPane(nPane := -1 AS LONG)  AS VOID
		IF nPane > 0 .AND. nPane <= SELF:_aPanes:Count
            VAR oPane := SELF:_aPanes[nPane- __ARRAYBASE__]
            oPane:Show()
        ELSEIF nPane < 0
            FOREACH VAR oPane IN SELF:_aPanes
                oPane:Show()
            NEXT
        ENDIF
        SELF:__SetPanes()
        RETURN

	PROPERTY SplitBarBackground AS Brush GET oBarBrush SET SELF:ChangeBackground(value, SPLTCOLOR_BAR)

	PROPERTY SplitBarFrameBackground AS Brush GET oBarFrameBrush SET SELF:ChangeBackground(value, SPLTCOLOR_BARFRAME)

	METHOD SuspendUpdate()  AS VOID STRICT
		IF (dwDeferPaintCount == 0)
            FOREACH VAR oSplit IN SELF:_aSplits
                oSplit:SuspendLayout()
            NEXT
		ENDIF
		dwDeferPaintCount := dwDeferPaintCount + 1
		RETURN

	PROPERTY VerticalAlign  AS LOGIC AUTO GET PRIVATE SET
	PROPERTY VerticalDrag   AS LOGIC AUTO GET PRIVATE SET


END CLASS

