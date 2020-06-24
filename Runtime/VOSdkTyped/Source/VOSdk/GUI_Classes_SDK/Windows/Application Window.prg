


#USING System.Windows.Forms
CLASS AppWindow INHERIT Window
	PROTECT oVertScroll AS WindowVerticalScrollBar
	PROTECT oHorzScroll AS WindowHorizontalScrollBar
	PROTECT oStatusBar AS StatusBar
	PROTECT lQuitOnClose AS LOGIC


	METHOD __CreateForm() AS VOForm STRICT
        RETURN GuiFactory.Instance:CreateAppWindow(SELF)
	
	
	//PP-030828 Strong typing
	METHOD __StatusMessageFromEvent(oEvent AS OBJECT, nType AS LONGINT) AS VOID STRICT 
		//PP-030828 Strong typing
		LOCAL oHL AS HyperLabel
		
		oHL := oEvent:HyperLabel
		IF (oHL != NULL_OBJECT)
			SELF:@@StatusMessage(oHL, nType)
		ENDIF
		RETURN
	

    METHOD Activate(oEvent)
        SUPER:Activate(oEvent)
        IF SELF:__Form:IsMdiChild
            VAR oShell  := SELF:__Form:ParentForm
            oSHell:Menu := SELF:__Form:Menu
        ENDIF
        RETURN NIL
        


	METHOD Default(oEvent) 
		RETURN SELF
	

	METHOD Destroy() AS USUAL CLIPPER
		
		IF (oVertScroll != NULL_OBJECT)
			oVertScroll:Destroy()
			oVertScroll := NULL_OBJECT
		ENDIF
		IF oHorzScroll != NULL_OBJECT
			oHorzScroll:Destroy()
			oHorzScroll := NULL_OBJECT
		ENDIF
		IF oStatusBar != NULL_OBJECT
			oStatusBar:Destroy()
			oStatusBar := NULL_OBJECT
		ENDIF
		
		IF SELF:lQuitOnClose .and. oApp != NULL_OBJECT
			lQuitOnClose := FALSE
			oApp:Quit()
		ENDIF
		SUPER:Destroy()
		RETURN SELF
	
	METHOD Dispatch(oEvt) 
		//Todo
		//LOCAL oEvt := oEvent AS @@Event
		//LOCAL dwMsg AS DWORD
		
		//LOCAL lHelpEnable AS LOGIC
		
		
		//dwMsg := oEvt:Message
		
		
		//CASE (dwMsg == WM_SETCURSOR)
		//	IF lHelpOn
		//		IF lHelpcursorOn
		//			lHelpEnable := TRUE
		//		ELSE
		//			lHelpEnable := FALSE
		//		ENDIF
		//	ELSE
		//		lHelpEnable := FALSE
		//	ENDIF
		//	// last par. changed from true (bug report l. atkins)
		//	SELF:__HandlePointer(oEvt, lHelpEnable, FALSE)
		//	RETURN SELF:EventReturnValue
		
		
		//CASE (dwMsg == WM_NCLBUTTONDOWN)
		//	IF lHelpOn
		//		IF lHelpcursorOn
		//			RETURN SELF:EventReturnValue
		//		ENDIF
		//	ENDIF
		
		//CASE (dwMsg == WM_LBUTTONDOWN)
		//	// ***********************************
		//	// Ignore if we're in Help Cursor mode
		//	// ***********************************
		//	IF !lHelpOn .OR. !lHelpcursorOn
		//		SELF:MouseButtonDown(MouseEvent{oEvt})
		//		//SELF:MouseButtonDown(__ObjectCastClassPtr(oEvt, __pCMouseEvent))
		//	ENDIF
		//	RETURN SELF:EventReturnValue
		
		//ENDCASE
		
		
		RETURN SUPER:Dispatch(oEvt)

    METHOD MenuSelect(oEvent) 
        SELF:__StatusMessageFromEvent(oEvent, MESSAGEMENU)
        RETURN NIL

	
	METHOD EnableBorder(kBorderStyle) 
		
		DEFAULT(@kBorderStyle, WINDOWSIZINGBORDER)
		IF SELF:__IsValid
			DO CASE
			CASE kBorderStyle == WINDOWNOBORDER
				SELF:oWnd:FormBorderStyle := FormBorderStyle.None
			CASE kBorderStyle == WINDOWNONSIZINGBORDER
				SELF:oWnd:FormBorderStyle := FormBorderStyle.FixedSingle
			CASE kBorderStyle == WINDOWSIZINGBORDER
				SELF:oWnd:FormBorderStyle := FormBorderStyle.Sizable
			END CASE
		ENDIF
		RETURN NIL
	

	METHOD EnableHorizontalScroll(lEnable := TRUE AS LOGIC) AS VOID
		IF oWnd IS VOAppForm VAR oAppForm 
			oAppForm:EnableHorizontalScroll(lEnable)
		ENDIF
		RETURN 
	
	METHOD EnableMaxBox(lEnable := TRUE AS LOGIC) AS VOID
		IF SELF:__IsValid
			SELF:oWnd:MaximizeBox := lEnable
		ENDIF
		RETURN 
	

	METHOD EnableMinBox(lEnable:= TRUE AS LOGIC) AS VOID
		IF SELF:__IsValid
			SELF:oWnd:MinimizeBox := lEnable
		ENDIF
		RETURN 
	

	METHOD EnableOleDropTarget(lEnable:= TRUE AS LOGIC) AS VOID
		//Todo EnableOleDropTarget
		RETURN 

	METHOD EnableStatusBar(lEnable:= TRUE AS LOGIC) AS StatusBar
		
		IF lEnable
			IF (SELF:StatusBar == NULL_OBJECT)
				SELF:StatusBar := StatusBar{SELF}
			ENDIF
			SELF:StatusBar:DisplayMessage()
			SELF:StatusBar:Create()
		ELSEIF SELF:StatusBar != NULL_OBJECT
			SELF:StatusBar:Destroy()
			SELF:StatusBar := NULL_OBJECT
		ENDIF
		
		RETURN SELF:StatusBar
	

	METHOD EnableSystemMenu(lEnable:= TRUE AS LOGIC) AS VOID
		IF SELF:__IsValid
			oWnd:ControlBox := lEnable
		ENDIF
		RETURN 
	

	METHOD EnableToolBar(lEnable:= TRUE AS LOGIC) AS VOID
		IF (SELF:ToolBar != NULL_OBJECT)
			IF lEnable
				SELF:ToolBar:Show()
			ELSE
				SELF:ToolBar:Hide()
			ENDIF
		ENDIF
		
		RETURN 
	

	METHOD EnableVerticalScroll(lEnable:= TRUE AS LOGIC) AS VOID
		IF SELF:__IsValid .AND. oWnd IS VOAppForm VAR oAppForm
			oAppForm:EnableVerticalScroll(lEnable)
		ENDIF
		RETURN 
	

	METHOD EndWindow(lSendMsg) 
		IF SELF:__IsValid
			IF IsLogic(lSendMsg) .AND. lSendMsg
				oWnd:Close()	
			ELSE
				GuiWin32.PostMessage(SELF:Handle(), WM_CLOSE, 0,0)
			ENDIF
		ENDIF
		RETURN NIL
	
	METHOD ErrorMessage(uText) 
		ErrorBox{SELF, uText}
		RETURN SELF   
	



	CONSTRUCTOR(oOwner) 
		SUPER(oOwner)
		RETURN 

	METHOD OLEDragEnter(oOleDragEvent)
		//Also empty in GUI Classes
		RETURN TRUE

	METHOD OLEDragLeave(oOleDragEvent)
		//Also empty in GUI Classes
		RETURN TRUE

	METHOD OLEDragOver(oOleDragEvent)
		//Also empty in GUI Classes
		RETURN TRUE

	METHOD OLEDrop(oOleDragEvent)
		//Also empty in GUI Classes
		RETURN TRUE

	METHOD OLEInPlaceActivate() 
		//Also empty in GUI Classes
		RETURN NIL
	
	METHOD OLEInPlaceDeactivate() 
		//Also empty in GUI Classes
		RETURN NIL

	ACCESS QuitOnClose AS LOGIC
		RETURN lQuitOnClose

	ASSIGN QuitOnClose(lNewValue AS LOGIC) 
		lQuitOnClose := lNewValue
	
	METHOD ReportException(oRQ) 
		//Also empty in GUI Classes
		RETURN NIL

	METHOD ReportNotification(oRQ) 
		//Also empty in GUI Classes
		RETURN NIL

	METHOD Show(nShowState AS LONG ) AS VOID
		IF oStatusBar != NULL_OBJECT
			oStatusBar:Show()
		ENDIF
		SUPER:Show(nShowState)
		RETURN 
	
	ACCESS StatusBar AS StatusBar
		RETURN oStatusBar
	
	ASSIGN StatusBar(oNewStatusBar AS StatusBar) 
		oStatusBar := oNewStatusBar
		RETURN 

	METHOD @@StatusMessage(oHL, nType) 
		LOCAL Message AS STRING
		LOCAL oStatBar AS StatusBar
		LOCAL oOwner AS OBJECT
		
		Default(@nType, MESSAGETRANSIENT)
		
		IF SELF:StatusBar != NULL_OBJECT
			oStatBar := SELF:StatusBar
		ELSE
			oOwner := SELF:Owner
			IF oOwner != NULL_OBJECT .AND. IsAccess(oOwner, #StatusBar)
				oStatBar := oOwner:StatusBar
			ENDIF
		ENDIF
		
		IF oStatBar != NULL_OBJECT
			DO CASE
			CASE IsInstanceOfUsual(oHL, #HyperLabel) .AND. IsString(((HyperLabel)oHL):Description)
				Message := oHL:Description
			CASE IsString(oHL)
				Message := oHL
			OTHERWISE
				Message := ""
			ENDCASE
			
			oStatBar:SetMessage(Message, nType)
		ENDIF
		RETURN NIL
	
	PROPERTY HorizontalScrollBar AS WindowHorizontalScrollBar GET oHorzScroll
	PROPERTY VerticalScrollBar as WindowVerticalScrollBar GET oVertScroll
	
	
	METHOD WarningMessage(aPlace1, aPlace2) 
		RETURN WarningBox{SELF, aPlace1, aPlace2}:Show()

END CLASS


