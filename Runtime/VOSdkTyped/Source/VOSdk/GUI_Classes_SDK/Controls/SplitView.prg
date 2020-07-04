USING System.Collections.Generic


CLASS SplitView INHERIT Control
	PROTECT	dwDeferPaintCount	AS DWORD
	PROTECT	oPanes			 	AS Dimension
	PROTECT	oBackgroundBrush	AS Brush
	PROTECT	oBarBrush		 	AS Brush
	PROTECT	oBarFrameBrush		AS Brush
    PROTECT _aSplits            AS List<VOSplitContainer>
    PROTECT _aPanes             AS List<IGuiObject>
    PROTECT lHorizontalAlign    AS LOGIC
    PROTECT lVerticalAlign      AS LOGIC
    PROTECT lHorizontalDrag     AS LOGIC
    PROTECT lVerticalDrag       AS LOGIC

    PROPERTY ControlType AS ControlType GET ControlType.SplitContainer
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
                        oChild:IsSplitterFixed := !SELF:lHorizontalDrag
                    ELSE
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:lVerticalDrag
                    ENDIF
                    
                    oMain:Panel1:Controls:Add(oChild)
                    
                CASE 4
                    oMain:Orientation := System.Windows.Forms.Orientation.Vertical
                    IF HorizontalPanes == 2 .AND. VerticalPanes == 2
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:lVerticalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:lVerticalDrag
                        oMain:Panel2:Controls:Add(oChild)
                        IF ! SELF:VerticalDrag
                            oMain:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
                            oMain:Panel1:AutoSize := FALSE
                        ENDIF
                    ELSEIF HorizontalPanes == 4 .AND. VerticalPanes == 1
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Vertical)
                        oChild:IsSplitterFixed := !SELF:lHorizontalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Vertical)
                        oChild:IsSplitterFixed := !SELF:lHorizontalDrag
                        oMain:Panel2:Controls:Add(oChild)
                        IF ! SELF:lHorizontalDrag
                            oMain:FixedPanel := System.Windows.Forms.FixedPanel.Panel1
                            oMain:Panel1:AutoSize := FALSE
                        ENDIF
                    ELSE // VerticalPanes == 4 .and HorizontalPanes == 1
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:lVerticalDrag
                        oMain:Panel1:Controls:Add(oChild)
                        oChild := SELF:_AddControl(System.Windows.Forms.Orientation.Horizontal)
                        oChild:IsSplitterFixed := !SELF:lVerticalDrag
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
         

    PRIVATE METHOD __GetPaneObject(oObject AS IGUIObject) AS System.Windows.Forms.Control
        IF oObject IS Control VAR oCtrl
            RETURN oCtrl:__Control
        ELSEIF oObject IS DataWindow VAR oDW
            RETURN oDw:__Surface
        ELSEIF oObject IS DialogWindow VAR oDlg
            RETURN oDlg:__Surface
        ENDIF
        RETURN NULL_OBJECT


    METHOD __SetPanelClient(oPanel AS System.Windows.Forms.SplitterPanel, oObject AS IGuiObject) AS VOID
        IF oPanel != NULL_OBJECT
            VAR oObj := __GetPaneObject(oObject)
            oPanel:Controls:Clear()
            IF oObj != NULL
                oPanel:Controls:Add(oObj)
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
            IF ! lVerticalDrag
                e:Cancel := TRUE
            ENDIF
        ELSE
            IF ! SELF:lHorizontalDrag
                 e:Cancel := TRUE
            ENDIF
        ENDIF
        RETURN



	METHOD Create() AS System.Windows.Forms.Control
		VAR oMain := SUPER:Create()
        SELF:__AdjustSize()
        RETURN oMain


	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lHorizontalDrag, lVerticalDrag, kAlignment) 
		LOCAL oWin AS Window

		// the owner must be a window
		IF !IsInstanceOfUsual(oOwner, #Window)
			WCError{#Init, #SplitView, __WCSTypeError, oOwner, 1}:@@Throw()
		ENDIF
		oWin := oOwner

        DEFAULT(@lHorizontalDrag, FALSE)
        DEFAULT(@lVerticalDrag , FALSE)
        DEFAULT(@kAlignment, SPLIT_VERTALIGN)

        SELF:lHorizontalDrag    := lHorizontalDrag
        SELF:lVerticalDrag      := lVerticalDrag
        SELF:lHorizontalAlign   := _AND(kAlignment, SPLIT_HORZALIGN ) != 0
        SELF:lVerticalAlign     := _AND(kAlignment, SPLIT_VERTALIGN ) != 0
        SELF:Layout  := Dimension{2,1}
        SELF:_aPanes := List<IGuiObject>{}
        SELF:_aSplits:= List<VOSplitContainer>{}

		SUPER(oWin, xID, oPoint, oDimension, "", _OR(WS_VISIBLE, WS_CHILD))

		RETURN 

	METHOD __EditChange() AS VOID STRICT 
		SELF:Modified := TRUE
		RETURN

	ACCESS Background AS Brush
		RETURN oBackgroundBrush

	ASSIGN Background(oBrush AS Brush) 
		SELF:ChangeBackground(oBrush, SPLTCOLOR_WINDOW)
		RETURN 

	METHOD ChangeBackground(oBrush AS Brush, kWhere := 0 AS LONG)  AS VOID
//		LOCAL dwNewColor	AS DWORD
//
//		// change the particular color using the supplied brush,
//		// or the appropriate system color if the brush is null
//		DO CASE
//		CASE kWhere == SPLTCOLOR_WINDOW
//			oBackgroundBrush := oBrush
//			IF oBackgroundBrush == NULL_OBJECT
//				dwNewColor := GuiWin32.GetSysColor(COLOR_APPWORKSPACE)
//			ENDIF
//
//		CASE kWhere == SPLTCOLOR_BAR
//			oBarBrush := oBrush
//			IF oBarBrush == NULL_OBJECT
//				IF _AND(GuiWin32.GetVersion(), 0X80000000) != 0
//					// Use Windows95 constants
//					dwNewColor := GuiWin32.GetSysColor(COLOR_3DFACE)
//				ELSE
//					// Use WindowsNT constants
//					dwNewColor := GuiWin32.GetSysColor(COLOR_BTNFACE)
//				ENDIF
//			ENDIF
//
//		CASE kWhere == SPLTCOLOR_BARFRAME
//			oBarFrameBrush := oBrush
//			IF oBarFrameBrush == NULL_OBJECT
//				IF _AND(GuiWin32.GetVersion(), 0X80000000) != 0
//					// Use Windows95 constants
//					dwNewColor := GuiWin32.GetSysColor(COLOR_3DFACE)
//				ELSE
//					// Use WindowsNT constants
//					dwNewColor := GuiWin32.GetSysColor(COLOR_BTNFACE)
//				ENDIF
//			ENDIF
//		END CASE
//
//		IF oBrush != NULL_OBJECT
//			dwNewColor := WC.GetBrushColor(oBrush)
//		ENDIF
//		//PCALL(gpfnSpltColorSet, SELF:Handle(), kWhere, INT(_CAST, dwNewColor))
//		//dwNewColor := dwNewColor
//		
		RETURN 

	ACCESS deferPaintCount 
		RETURN SELF:dwDeferPaintCount

	METHOD Destroy() AS USUAL CLIPPER
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
        _aPanes  := NULL_OBJECT
        _aSplits := NULL_OBJECT
		oPanes  := NULL_OBJECT
		oBackgroundBrush := NULL_OBJECT
		oBarBrush := NULL_OBJECT
		oBarFrameBrush := NULL_OBJECT

		SUPER:Destroy()

		RETURN SELF

	METHOD Dispatch(oEvent) 
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

	PROPERTY HorizontalAlign AS LOGIC GET SELF:lHorizontalAlign 
	PROPERTY HorizontalDrag  AS LOGIC GET SELF:lHorizontalDrag 


	ACCESS Layout AS Dimension
		RETURN oPanes

	ASSIGN Layout(oDimension AS Dimension)
        LOCAL nHorizontal, nVertical AS LONG
		oPanes := oDimension
        SELF:__AdjustSize()
		RETURN 

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

	ACCESS SplitBarBackground 
		RETURN oBarBrush

	ASSIGN SplitBarBackground(oBrush) 
		SELF:ChangeBackground(oBrush, SPLTCOLOR_BAR)
		RETURN 

	ACCESS SplitBarFrameBackground 
		RETURN oBarFrameBrush

	ASSIGN SplitBarFrameBackground(oBrush) 
		SELF:ChangeBackground(oBrush, SPLTCOLOR_BARFRAME)
		RETURN 

	METHOD SuspendUpdate()  AS VOID STRICT
		IF (dwDeferPaintCount == 0)
            FOREACH VAR oSplit IN SELF:_aSplits
                oSplit:SuspendLayout()
            NEXT
		ENDIF
		dwDeferPaintCount := dwDeferPaintCount + 1 
		RETURN 

	PROPERTY VerticalAlign  AS LOGIC GET SELF:lVerticalAlign 
	PROPERTY VerticalDrag   AS LOGIC GET SELF:lVerticalDrag  


END CLASS

