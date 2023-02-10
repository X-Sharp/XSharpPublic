


USING System.Runtime.InteropServices


/// <include file="Gui.xml" path="doc/DialogWindow/*" />
CLASS DialogWindow INHERIT Window IMPLEMENTS ILastFocus
	PROTECT oResourceID 	AS ResourceID
	PROTECT bModal 			AS LOGIC
	PROTECT nResult 		AS LONG
	PROTECT aRadioGroups 	AS ARRAY
	PROTECT oLastFocus      AS Control
	PROTECT oSurface		AS VOPanel

	ACCESS __Dialog AS VODialogForm
		RETURN (VODialogForm) oWnd

	STATIC METHOD GetShell(oWin AS OBJECT) AS ShellWindow
		DO WHILE oWin != NULL_OBJECT
			IF oWin IS ShellWindow
				EXIT
			ENDIF
			IF IsAccess(oWin, #Owner)
				oWin := Ivarget(oWin,#Owner)
			ELSE
				oWin := NULL_OBJECT
			ENDIF
		ENDDO
		RETURN oWin

	METHOD __CreateForm AS VOForm STRICT
		LOCAL oDlg AS VODialogForm
        oDlg := GuiFactory.Instance:CreateDialogWindow(SELF, SELF:oResourceDialog)
		// Set owner window and prevent from showing on the taskbar
		IF SELF:Owner IS Window
			LOCAL oShell as ShellWindow
			oShell := GetShell(SELF:Owner)
			IF oShell != NULL_OBJECT
				oDlg:Owner := oShell:__Form
			ENDIF
			oDlg:ShowInTaskbar := FALSE
            		oDlg:Text := SELF:Caption
		ENDIF
		SELF:oSurface := oDlg:Surface
		SELF:SetExStyle(WS_EX_APPWINDOW, FALSE)
		SELF:SetExStyle(WS_EX_DLGMODALFRAME, TRUE)
		SELF:SetStyle(WS_CLIPCHILDREN, FALSE)
		SELF:SetStyle(4, TRUE)
		IF _AND( SELF:dwStyle, WS_THICKFRAME ) = WS_THICKFRAME
			oDlg:SetSizable(TRUE)
			oDlg:MinimizeBox := TRUE
			oDlg:MaximizeBox := TRUE
		ELSE
			oDlg:SetSizable(FALSE)
			oDlg:MinimizeBox := FALSE
			oDlg:MaximizeBox := FALSE
		ENDIF
		IF _AND(SELF:dwStyle, WS_SYSMENU) == WS_SYSMENU
			oDlg:ControlBox := TRUE
		ELSE
			oDlg:ControlBox := FALSE
		ENDIF
		RETURN oDlg

 /// <exclude />
	METHOD __Close(oEvent AS @@Event) AS VOID STRICT
		SELF:Close(oEvent)

		SELF:Destroy()
		SELF:EventReturnValue := 1L

		RETURN
 /// <exclude />

	METHOD __RestoreLastFocus() AS VOID STRICT
		IF oParent != NULL_OBJECT .AND. oParent IS ILastFocus VAR oLastFocus
			IF oLastFocus:LastFocus != NULL_OBJECT
				oLastFocus:LastFocus:SetFocus()
			ENDIF
		ENDIF
 /// <exclude />
	ACCESS __HasSurface AS LOGIC
		RETURN TRUE

 /// <exclude />
	ACCESS __Surface AS IVOControlContainer
		RETURN oSurface
 /// <exclude />
	METHOD __SetupDataControl(oDC AS Control) AS VOID
		IF oDC IS RadioButtonGroup
			AAdd(aRadioGroups, oDC)
		ENDIF
		RETURN

 /// <exclude />
	METHOD __SetFont() AS VOID STRICT
		IF SELF:oFont != NULL_OBJECT .and. oSurface != NULL_OBJECT
			oSurface:Font := SELF:oFont
		ELSE
			SUPER:__SetFont()
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/DialogWindow.Activate/*" />
	METHOD Activate(oEvent  AS Event)
		WC.AppSetDialogWindow(SELF:oSurface)
		RETURN SUPER:Activate(oEvent)

/// <include file="Gui.xml" path="doc/DialogWindow.Active/*" />
	METHOD Active() AS LOGIC
		RETURN __Dialog:IsShown

/// <include file="Gui.xml" path="doc/DialogWindow.ButtonClick/*" />
	METHOD ButtonClick(oControlEvent AS ControlEvent)
		LOCAL oButton AS Control
		LOCAL dwI, dwCount AS DWORD
		LOCAL oRBG AS RadioButtonGroup
		LOCAL oCE := oControlEvent	AS ControlEvent
		SUPER:ButtonClick(oControlEvent)

		oButton := oCE:Control
		IF oButton IS Button
			oButton:Modified := TRUE // assume its modified
			IF oButton IS RadioButton
				//SE-060526
				dwCount := ALen(aRadioGroups)
				FOR dwI := 1 UPTO dwCount
					oRBG := aRadioGroups[dwI]
					IF oRBG:__IsElement(OBJECT(_CAST,oButton))
						oRBG:__SetOn(OBJECT(_CAST,oButton))
						oButton:__Update()
						oButton := oRBG
						EXIT
					ENDIF
				NEXT  //Vulcan.NET-Transporter: dwI
			ENDIF
			oButton:__Update() // Update radio button group
		ENDIF
		RETURN 0


/// <include file="Gui.xml" path="doc/DialogWindow.ChangeFont/*" />
	METHOD ChangeFont(oFont, lRescale)
		SELF:Font := oFont
		RETURN TRUE

	ACCESS ClipperKeys() AS LOGIC
		RETURN FALSE

	ASSIGN ClipperKeys(lNewValue AS LOGIC)
		RETURN

/// <include file="Gui.xml" path="doc/DialogWindow.ControlFocusChange/*" />
	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL STRICT
		LOCAL oCFCE := oControlFocusChangeEvent AS ControlFocusChangeEvent
		IF oCFCE:GotFocus
			SELF:LastFocus := oCFCE:Control
			WC.AppSetDialogWindow(oSurface)
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/DialogWindow.DeActivate/*" />
	METHOD DeActivate(oEvent  AS Event)
		RETURN SUPER:DeActivate(oEvent)


/// <include file="Gui.xml" path="doc/DialogWindow.Default/*" />
	METHOD Default(oEvent AS Event)
		SELF:EventReturnValue := 0
		RETURN SELF


/// <include file="Gui.xml" path="doc/DialogWindow.Destroy/*" />
	METHOD Destroy() AS USUAL
		IF SELF:oSurface != NULL_OBJECT
			IF (WC.AppGetDialogWindow() == SELF:oSurface)
				WC.AppSetDialogWindow(NULL_OBJECT)
			ENDIF
			SELF:oSurface:CleanUp()
			// Surface is not disposed. We may want to access the controls on the surface

		ENDIF
		SELF:oLastFocus := NULL_OBJECT
		SUPER:Destroy()
		RETURN NIL

/// <include file="Gui.xml" path="doc/DialogWindow.EditFocusChange/*" />
	METHOD EditFocusChange(oEditFocusChangeEvent AS EditFocusChangeEvent)
		LOCAL uRetCode AS USUAL
		LOCAL oEFCE AS EditFocusChangeEvent
		oEFCE := oEditFocusChangeEvent
		uRetCode := SUPER:EditFocusChange(oEFCE)

		IF !oEFCE:GotFocus
			IF oEFCE:Control != NULL_OBJECT
				oEFCE:Control:__Update()
			ENDIF
		ENDIF

		RETURN uRetCode


/// <include file="Gui.xml" path="doc/DialogWindow.EndDialog/*" />
	METHOD EndDialog(iResult)
		Default(@iResult, 0)
		nResult := iResult
		IF SELF:__IsValid
            		// prevent owner invalidation and visual noise
			SELF:oWnd:Owner := NULL
			SELF:oWnd:Close()
		ENDIF
		SELF:__RestoreLastFocus()
		RETURN iResult


/// <include file="Gui.xml" path="doc/DialogWindow.ExecModal/*" />
	METHOD ExecModal()
		oApp:Exec(EXECNORMAL, SELF)
		RETURN SELF

	//METHOD HelpRequest(oHelpRequestEvent)
	//	SUPER:HelpRequest(oHelpRequestEvent)
	//	RETURN SELF


	ACCESS HyperLabel AS HyperLabel
		RETURN SUPER:HyperLabel

	ASSIGN HyperLabel (oHL AS HyperLabel)
		SUPER:HyperLabel := oHL
		IF oHL != NULL_OBJECT
			SELF:__Surface:Text := "Surface:"+oHL:Name
		ENDIF


/// <include file="Gui.xml" path="doc/DialogWindow.ctor/*" />
	CONSTRUCTOR(oOwner, xResourceID, lModal)

		IF IsInstanceOfUsual(oOwner, #App)
			oOwner := NIL
		ENDIF

		IF !IsNil(oOwner) .AND. !IsInstanceOfUsual(oOwner, #Window) .AND. !IsInstanceOfUsual(oOwner, #ToolBar) .AND. !IsPtr(oOwner)
			WCError{#Init,#DialogWindow,__WCSTypeError,oOwner,1}:Throw()
		ENDIF

		IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
			oResourceID := ResourceID{xResourceID}
		ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
			oResourceID := xResourceID
		ENDIF
		SELF:__ReadResource(oResourceID, oOwner)
		DEFAULT(@lModal, TRUE)
		bModal := lModal

		SUPER(oOwner)


		aRadioGroups := {}
		IF (SELF:oParent != NULL_OBJECT .AND. IsAccess(SELF:oParent, #HELPDISPLAY ))
			SELF:HelpDisplay := IVarGet(oParent,#HelpDisplay)
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/DialogWindow.IsModal/*" />
	ACCESS IsModal AS LOGIC
		RETURN bModal


/// <include file="Gui.xml" path="doc/DialogWindow.LastFocus/*" />
	ACCESS LastFocus AS Control
		RETURN oLastFocus

/// <include file="Gui.xml" path="doc/DialogWindow.LastFocus/*" />
	ASSIGN LastFocus (oControl AS Control)
		LOCAL nStyle AS LONG
		nStyle := SELF:GetStyle()
		IF _AND(nStyle, WS_CHILD) =  WS_CHILD
			IF oParent != NULL_OBJECT .AND. oParent IS ILastFocus VAR oLastFocus
				oLastFocus:LastFocus := oControl
			ENDIF
		ENDIF
		oLastFocus := oControl

/// <include file="Gui.xml" path="doc/DialogWindow.ListBoxClick/*" />
	METHOD ListBoxClick(oControlEvent AS ControlEvent)
		LOCAL oListBox := NULL_OBJECT AS ListBox
		LOCAL oCE AS ControlEvent
		oCE := oControlEvent
		oListBox := (OBJECT) oCE:Control
		oListBox:Modified := TRUE // assume its modified
		oListBox:__Update()
		RETURN SELF

/// <include file="Gui.xml" path="doc/DialogWindow.ListBoxSelect/*" />
	METHOD ListBoxSelect(oControlEvent AS ControlEvent)
		LOCAL oListBox := NULL_OBJECT AS ListBox
		LOCAL oCE AS ControlEvent
		oCE := oControlEvent
		oListBox := (OBJECT) oCE:Control
		oListBox:Modified := TRUE // assume its modified
		oListBox:__SetText(oListBox:CurrentItem)
		oListBox:__Update()
		RETURN SELF

/// <include file="Gui.xml" path="doc/DialogWindow.Owner/*" />
	ACCESS Owner AS OBJECT
		IF oParent == NULL_OBJECT
			RETURN oApp
		ENDIF
		RETURN oParent

/// <include file="Gui.xml" path="doc/DialogWindow.PostShowDialog/*" />
	METHOD PostShowDialog()
		RETURN NIL

/// <include file="Gui.xml" path="doc/DialogWindow.Result/*" />
	ACCESS Result() AS LONG
		RETURN nResult

	ACCESS Surface AS VOPanel
		RETURN oSurface

	ASSIGN Size (oSize AS Dimension)
		IF ! SELF:__Dialog:IsShown
			SELF:__Dialog:InitialSize := oSize
		ENDIF
		SUPER:Size := oSize


/// <include file="Gui.xml" path="doc/DialogWindow.Show/*" />
	METHOD Show(kShowState := SHOWCENTERED AS LONG ) AS VOID
		IF bModal
			oWnd:StartPosition := SELF:__GetStartPosFromShowState(kShowState)
			SELF:ShowModal(TRUE)
		ELSE
			SUPER:Show(kShowState)
			//SELF:PostShowDialog()		// Is now called from the OnShown method on the Form
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/DialogWindow.ShowModal/*" />
	METHOD ShowModal(lActive AS LOGIC)
		LOCAL oShell AS ShellWindow
		IF SELF:Owner IS Window
			oShell := GetShell(SELF:Owner)
		ENDIF
		IF lActive
			oWnd:Visible := FALSE
			IF oShell != NULL_OBJECT
				oWnd:ShowDialog(oShell:__Form)
			ELSE
				oWnd:ShowDialog()
			ENDIF
			// Set Focus to Last Control of Parent that had focus
			SELF:__RestoreLastFocus()
		ELSE
			oWnd:Visible := FALSE
			oWnd:Close()
		ENDIF
		RETURN TRUE

END CLASS
