


USING System.Reflection
USING SWF := System.Windows.Forms

CLASS Control INHERIT VObject IMPLEMENTS IGuiObject, ITimer
	PROTECT oCtrl                AS SWF.Control
	PROTECT oParent              AS IControlParent 
	
	// The surface window that owns the control. This is the same as oParent except
	// when oParent                                   is a data window. Then oFormSurface is the DataWindow's dialog
	PROTECT oFormSurface         AS IControlParent
	PROTECT wId                  AS LONGINT   
	PROTECT lDataAware           AS LOGIC 
	PROTECT lIsDestroyed         AS LOGIC
	PROTECT oContextMenu         AS Menu
	
	PROTECT oSize                AS Dimension
	PROTECT oOrigin              AS Point
	PROTECT cWindowName          AS STRING
	PROTECT cClassName           AS STRING
	PROTECT dwStyle              AS LONG
	PROTECT dwExStyle            AS LONG
	PROTECT cCaption             AS STRING
	PROTECT uValue               AS USUAL
	PROTECT oFieldSpec           AS FieldSpec
	PROTECT lChanged             AS LOGIC
	PROTECT __lModified          AS LOGIC
	PROTECT oHyperLabel        AS HyperLabel
	PROTECT lExplicitHL          AS LOGIC
	PROTECT oServer              AS DataServer		
	PROTECT lBaseServer          AS LOGIC
	PROTECT siDataField          AS LONGINT      
	PROTECT symDataField         AS SYMBOL
	PROTECT oHLStatus            AS HyperLabel
	PROTECT oControlBackground   AS Brush
	PROTECT lExplicitFS          AS LOGIC
	PROTECT oDataField           AS DataField
	PROTECT uGetSetOwner         AS USUAL
	PROTECT cbGetSetBlock        AS CODEBLOCK
	PROTECT dwTimerCount         AS INT
	PROTECT dwTimerInterval      AS INT
	PROTECT lTimerRegistered     AS LOGIC
	PROTECT sToolTipText         AS STRING
	PROTECT lUseHLForToolTip     AS LOGIC
	PROTECT oControlWindow		 AS ControlWindow
	
	EXPORT EventReturnValue      AS LONGINT

	PROPERTY __IsValid		AS LOGIC GET oCtrl != NULL_OBJECT .and. ! oCtrl:IsDisposed
    PROPERTY ControlType    AS Controltype GET ControlType.Control

    PROPERTY __Handle as IntPtr GET SELF:Handle()

	ASSIGN Parent(oP AS IControlParent)
		SELF:oParent := oP

	ASSIGN __ControlWindow (oWin AS ControlWindow)
		oControlWindow := oWin
		IF SELF:oParent == NULL_OBJECT
			oParent := oWin
		ENDIF
		
	ACCESS __ControlWindow AS ControlWindow
		RETURN oControlWindow

	METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		IF oFormSurface != NULL_OBJECT
			oFormSurface:__AddTool(SELF)
		ENDIF
		lIsDestroyed := FALSE
		RETURN

	METHOD OnHandleDestroyed(o AS OBJECT, e AS EventArgs) AS VOID
		lIsDestroyed := TRUE
		RETURN

	METHOD __CreateControl(liStyle AS LONG, liExStyle AS LONG) AS SWF.Control
		RETURN GuiFactory.Instance:CreateControl(SELF:ControlType, SELF, liStyle, liExStyle)

    METHOD OnControlCreated(oC AS SWF.Control) AS VOID
        RETURN

	ACCESS __Control AS SWF.Control
		RETURN oCtrl
	
	METHOD __AddTool(oControl AS Control) AS LOGIC STRICT 
		RETURN oParent:__AddTool(oControl)
	
	
	
	ACCESS __DataField AS DataField STRICT 
		RETURN oDataField
	
	METHOD __EnsureVisibity() AS LOGIC STRICT 
		//Todo
		/*
		LOCAL rCtl, rParent IS _winRECT
		LOCAL oPoint AS Point
		LOCAL lVScrollPos, lHScrollPos AS LONGINT
		LOCAL iXCorrect AS INT
		LOCAL hFFrame AS PTR
		IF (oParent:__FormWindow == NULL_OBJECT)
		RETURN FALSE
		ENDIF
		
		hFFrame := oParent:__FormWindow:Handle()
		IF (hFFrame == NULL_PTR)
		RETURN FALSE
		ENDIF
		
		GetWindowRect(SELF:Handle(), @rCtl)
		GetClientRect(hFFrame, @rParent)
		#ifdef __VULCAN__	
		ClientToScreen(hFFrame, (_winPOINT PTR) @rParent)
		#else	
		ClientToScreen(hFFrame, @rParent)
		#endif	
		ClientToScreen(hFFrame, (_winPOINT PTR) @(rParent:right))
		
		IF (rCtl:left < rParent:left) .OR. (rCtl:right > rParent:right) .OR.;
		(rCtl:top < rParent:top) .OR. (rCtl:bottom > rParent:bottom)
		oPoint := oFormSurface:Origin
		
		lHScrollPos := GetScrollPos(hFFrame, SB_HORZ)
		lVScrollPos := GetScrollPos(hFFrame, SB_VERT)
		
		IF (rCtl:left < rParent:left)
		oPoint:X += (rParent:left - rCtl:left)
		SetScrollPos(hFFrame, SB_HORZ, lHScrollPos - (rParent:left - rCtl:left), TRUE)
		ELSEIF (rCtl:right > rParent:right)
		iXCorrect := Min((rCtl:right - rParent:right), (rCtl:left - rParent:left))
		oPoint:X -= iXCorrect
		SetScrollPos(hFFrame, SB_HORZ, lHScrollPos + iXCorrect, TRUE)
		ENDIF
		
		IF (rCtl:top < rParent:top)
		oPoint:Y -= (rParent:top - rCtl:top)
		SetScrollPos(hFFrame, SB_VERT, lVScrollPos - (rParent:top - rCtl:top), TRUE)
		ELSEIF (rCtl:bottom > rParent:bottom)
		oPoint:Y += (rCtl:bottom - rParent:bottom)
		SetScrollPos(hFFrame, SB_VERT, lVScrollPos + (rCtl:bottom - rParent:bottom), TRUE)
		ENDIF
		
		oFormSurface:Origin := oPoint
		ENDIF
		*/
		RETURN TRUE
	

	ACCESS __FormSurface AS OBJECT STRICT 
		RETURN oFormSurface
	

	METHOD __Gather() AS LOGIC STRICT 
		LOCAL lReturn:=TRUE AS LOGIC
		// Get value from control if necessary
		SELF:__Update()
		
		IF lChanged
			IF oServer != NULL_OBJECT
				IF lBaseServer // if not subclassing
					oServer:FIELDPUT(siDataField,SELF:Value) //use FieldPut
				ELSEIF (symDataField != NULL_SYMBOL) //else use assigns
					IVarPut(oServer, symDataField ,SELF:Value)
				ELSE
					IVarPut(oServer, SELF:NameSym ,SELF:Value)
				ENDIF
				lReturn := (oServer:Status == NIL)
				IF lReturn
					SELF:Modified := FALSE
					SELF:ValueChanged := FALSE
				ENDIF
			ELSEIF cbGetSetBlock != NULL_CODEBLOCK
				IF (uGetSetOwner == NIL)
					Eval(cbGetSetBlock, SELF:Value)
				ELSE
					Eval(cbGetSetBlock, uGetSetOwner, SELF:Value)
				ENDIF
			ENDIF
		ENDIF
		RETURN lReturn
	

	ACCESS __GetDataFldPos AS LONGINT STRICT 
		RETURN siDataField
	
	[Obsolete];
	METHOD __GetDispInfo(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT 
		//processes LVN_GETDISPINFO, TVN_GETDISPINFO, CBEN_GETDISPINFO, TBN_GETDISPINFO, HDN_GETDISPINFO
		//see Window:ControlNotify()
		RETURN

	ACCESS __Parent AS IControlParent STRICT 
		RETURN oParent
	

	METHOD __Scatter() AS Control STRICT 
		LOCAL newValue AS USUAL
		LOCAL cFSValType AS STRING
		
		IF oServer != NULL_OBJECT
			IF lBaseServer // if not subclassing
				newValue := oServer:FIELDGET(symDataField) //use fieldget
			ELSEIF symDataField != NULL_SYMBOL //else use Access
					newValue := IVarGet(oServer, symDataField)
			ELSE
				newValue := IVarGet(oServer, SELF:NameSym)
			ENDIF
			IF IsString(newValue)
				SELF:__Value:=RTrim(newValue)
			ELSE
				SELF:__Value:=newValue
			ENDIF
			oHLStatus := NULL_OBJECT
			SELF:ValueChanged := FALSE
		ELSEIF (SELF:Value == NIL) .AND. oFieldSpec != NULL_OBJECT
			cFSValType := oFieldSpec:ValType
			SWITCH cFSValType
            CASE "C"
            CASE "M"
				SELF:Value := NULL_STRING
			CASE "D"
				SELF:Value := NULL_DATE
			CASE "N"
				SELF:Value := 0
			CASE "L"
				SELF:Value := FALSE
			END SWITCH
		ENDIF
		
		RETURN SELF
	

	[Obsolete];
	METHOD __SetColors(_hDC AS IntPtr) AS IntPtr STRICT 
		RETURN IntPtr.Zero

	METHOD __Timer() AS OBJECT STRICT 
		dwTimerCount := dwTimerCount - 1
		IF (dwTimerCount == 0)
			SELF:Timer()
			dwTimerCount:=dwTimerInterval
			IF (dwTimerCount == 0)
				WC.UnregisterTimer(SELF)
				lTimerRegistered:=FALSE
			ENDIF
		ENDIF
		
		RETURN SELF
	

	[Obsolete];
	METHOD __ToolTipHandle() AS IntPtr STRICT 
		// Todo
		//	RETURN oParent:__ToolTipHandle()
		RETURN IntPtr.Zero

	METHOD __Unlink(oDataServer := NIL AS USUAL) AS Control STRICT 
		
		IF SELF:Modified
			SELF:__Update()
			IF SELF:PerformValidations(uValue)
				SELF:__Gather()
			ENDIF
		ENDIF
		
		Default(@oDataServer,oServer)
		
		// Do actual unlinking
		IF IsNil(oDataServer)
			uGetSetOwner:= NIL
			cbGetSetBlock:= NULL_CODEBLOCK
			oDataField:= NULL_OBJECT
			oServer:= NULL_OBJECT
		ELSE
			IF !IsInstanceOfUsual(oDataServer,#DataServer)
				WCError{#__Unlink,#Control,__WCSTypeError,oDataServer,1}:@@Throw()
			ENDIF
			IF (oDataServer == oServer)
				uGetSetOwner:= NIL
				cbGetSetBlock:= NULL_CODEBLOCK
				oDataField := NULL_OBJECT
				oServer := NULL_OBJECT
			ENDIF
		ENDIF
		RETURN SELF

	METHOD __Update() AS VOID STRICT
		LOCAL cText AS STRING
		LOCAL uOldValue AS USUAL
		
		IF SELF:Modified
			cText := SELF:TextValue
			uOldValue := uValue
			IF oFieldSpec != NULL_OBJECT
				uValue := oFieldSpec:Val(cText)
				IF SLen(oFieldSpec:Picture) > 0
					SELF:TextValue := oFieldSpec:Transform(uValue)
				ENDIF
			ELSE
				uValue := cText
			ENDIF
			SELF:Modified := FALSE
			
			SELF:lChanged := !(uOldValue == uValue)
		ENDIF
		RETURN 

	ASSIGN __Value(uNewValue AS USUAL)  STRICT 
		LOCAL uTemp AS USUAL
		
		IF uNewValue == NIL
			uValue := NIL
			SELF:TextValue := ""
			oHLStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ELSEIF oFieldSpec != NULL_OBJECT
			uTemp := oFieldSpec:Transform(uNewValue)
			IF IsNil(uTemp)
				oHLStatus := oFieldSpec:Status
				WCError{#__Value,#Control,__WCSTypeError}:@@Throw()
			ELSE
				SELF:uValue := oFieldSpec:Val(uTemp)
				SELF:TextValue := uTemp
				oHLStatus := NULL_OBJECT
				SELF:Modified := FALSE
				SELF:lChanged := TRUE
			ENDIF
		ELSE
			uTemp := AsString(uNewValue)
			SELF:uValue := uNewValue
			SELF:TextValue := uTemp
			oHLStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ENDIF
		
		RETURN 
	
	METHOD Activate(oEvent ) 
		// Also empty in GUI Classes
		RETURN NIL

	METHOD AddChild(oC AS OBJECT) AS VOID STRICT
		IF SELF:__IsValid
			DO CASE
			CASE oC IS Control
				SELF:oCtrl:Controls:Add(((Control)oC):__Control)
			CASE oC IS DataWindow
				SELF:oCtrl:Controls:Add(((DataWIndow) oC):__Frame)
			CASE oC IS Window
				SELF:oCtrl:Controls:Add(((Window) oC):__Surface)
			ENDCASE
			ENDIF
		RETURN
	
	METHOD AsString() 
		IF !IsNil(SELF:uValue)
			RETURN AsString(SELF:uValue)
		ENDIF
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

	ACCESS Background AS Brush
		RETURN oControlBackground

	ASSIGN Background(oNewBrush AS Brush) 
		oControlBackground := oNewBrush
		IF SELF:__IsValid .and. oNewBrush != NULL_OBJECT
			oCtrl:BackColor := oNewBrush:Color 
		ENDIF
		RETURN 
	
	METHOD BringToFront() AS VOID CLIPPER
		IF SELF:ValidateControl()
			oCtrl:BringToFront()
		ENDIF

	PROPERTY Caption AS STRING GET cCaption SET cCaption := Value

	PROPERTY ContextMenu AS Menu 
	GET 
		RETURN oContextMenu 
	END GET
	SET 
		IF SELF:__IsValid 
			oContextMenu := Value
			IF oContextMenu != NULL_OBJECT
				oCtrl:ContextMenu := oContextMenu:__Menu:AsContextMenu()
			ELSE
				oCtrl:ContextMenu := NULL_OBJECT
			ENDIF
		ENDIF
	END SET
	END PROPERTY
	
	PROPERTY SWFControl AS SWF.Control GET oCtrl
	PROPERTY ControlID AS LONG GET SELF:wId
	
	METHOD CreateWindowEx(dwExStyle AS LONG, sCaption AS STRING, dwStyle AS LONG,;
		nX AS LONG, nY AS LONG, nWidth AS LONG, nHeight AS LONG, oOwner AS Window) AS SWF.Control
		oCtrl  := 	SELF:__CreateControl(dwStyle , dwExStyle ) 

		IF oCtrl != NULL_OBJECT
			// Determine where to add the control
			// Some controls are added to the surface, others to the form 
			oCtrl:HandleDestroyed += OnHandleDestroyed
			oCtrl:HandleCreated   += OnHandleCreated

			IF !STRING.IsNullOrEmpty(sCaption)
				oCtrl:Text := sCaption
			ENDIF
			IF oOwner != NULL_OBJECT
				LOCAL oType AS System.Type
				oType := oCtrl:GetType()
				IF oType == typeof(VOStatusStrip)
					oOwner:__Form:Controls:Add(oCtrl)
				ELSEIF oType == typeof(VOToolBar)
					oOwner:__Form:Controls:Add(oCtrl)
				ELSEIF oType == typeof(VODataGridView)
					oOwner:__Form:Controls:Add(oCtrl)
					IF oOwner IS DataWindow
						LOCAL oDataForm AS VODataForm
						oDataForm := (VODataForm) oOwner:__Form
						oDataForm:DataBrowser := oCtrl
					ENDIF
				ELSEIF oOwner:__HasSurface
					oOwner:__Surface:Controls:Add(oCtrl)
				ELSE
					oOwner:__Form:Controls:Add(oCtrl)
				ENDIF
			ENDIF
			oCtrl:Location := Point{nX, nY}
			oCtrl:Size     := Dimension{nWidth, nHeight}
		ENDIF
		RETURN oCtrl
	
	METHOD Create() AS SWF.Control
		LOCAL oDlgItem AS ResourceDialogItem
		LOCAL oFont AS System.Drawing.Font
		LOCAL oDevPoint as Point
		LOCAL oOwner AS OBJECT
		LOCAL nTabIndex AS LONG
		// don't create twice
		IF SELF:__IsValid 
			RETURN oCtrl
		ENDIF
		
		IF !lIsDestroyed
			
			IF (oCtrl == NULL_OBJECT) 
				// See if the control has been defined in a native resource. When so, then copy some of the properties
				// from the resource, such as position, size, styles and caption
				IF oFormSurface != NULL_OBJECT
					oDlgItem := SELF:oFormSurface:GetDlgItem(SELF:wId)
				ENDIF
				IF oDlgItem != NULL_OBJECT
					oOrigin := oDlgItem:Origin
					oSize   := oDlgItem:Size
					IF STRING.IsNullOrEmpty(cClassName)
						cClassName := oDlgItem:WindowClassName
					ENDIF
					dwStyle		:= oDlgItem:Style
					dwExStyle	:= oDlgItem:ExStyle
					cCaption	:= oDlgItem:Caption
					oDevPoint	:= oDlgItem:Origin
					nTabIndex   := oDlgItem:TabIndex
				ELSE
					nTabIndex   := -1
					oDevPoint := Point{oOrigin:X, oOrigin:Y}
                    IF WC.CoordinateSystem == WC.CartesianCoordinates
					    oDevPoint:Y := oDevPoint:Y - oSize:Height
    					oDevPoint := WC.ConvertPoint(oFormSurface, oDevPoint)
                    ENDIF
				ENDIF
				oOwner := oParent
				DO WHILE oOwner != NULL_OBJECT .AND. ! (oOwner IS Window)
					IF IsAccess(oOwner, #Owner)
						oOwner := IVarGet(oOwner, #Owner)
					ELSE
						oOwner := NULL_OBJECT
					ENDIF
				ENDDO
				oCtrl:= SELF:CreateWindowEx(dwExStyle, cWindowName, dwStyle, oDevPoint:X, oDevPoint:Y, oSize:Width, oSize:Height, (Window) oOwner)
				IF oCtrl != NULL_OBJECT .and. nTabIndex > 0
					oCtrl:TabIndex := nTabIndex
				ENDIF
			ENDIF
			
			IF (oCtrl != NULL_OBJECT)
				oFont := System.Drawing.SystemFonts.DefaultFont
				oCtrl:Font := oFont

			ELSE
				// !!! Maybe we should remove this before shipping !!!
				WCError{#Create,#Control,__WCSCreateCtlFailed,SELF}:@@Throw()
			ENDIF
		ENDIF
		RETURN oCtrl
	
	PROPERTY DataField  AS SYMBOL GET SELF:symDataField

	METHOD Deactivate(oEvent AS @@Event) AS VOID 
		//Also empty in GUI Classes
		RETURN 
	
	METHOD Default(oEvent) 		
		//Todo
		// LOCAL oEvt := oEvent AS @@event
		// SELF:EventReturnValue := CallWindowProc(SELF:__lpfnDefaultProc, oCtrl, oEvt:umsg, oEvt:wParam, oEvt:lParam)

		RETURN 1L
	
	METHOD Destroy() AS USUAL CLIPPER
		
		IF oCtrl != NULL_OBJECT
			IF ! oCtrl:IsDisposed
				// Prevent calling Dispose in a background thread
				IF SWF.Application.MessageLoop
					oCtrl:Dispose()
				ENDIF
			ENDIF
			oCtrl := NULL_OBJECT
		ENDIF
		oContextMenu := NULL_OBJECT
		UnregisterAxit(SELF)
		lTimerRegistered:=FALSE
		SUPER:Destroy()
		lIsDestroyed := TRUE
		RETURN SELF
	

	METHOD Disable() AS VOID STRICT
		IF SELF:__IsValid 
			oCtrl:Enabled := FALSE
			oCtrl:Invalidate()
		ENDIF
		SELF:SetStyle(WS_DISABLED, TRUE)
		
		RETURN 
	

	METHOD DisableTheme() AS VOID STRICT
		//Todo
		//RETURN SetWindowTheme(SELF:handle(),"","")
		
	METHOD Dispatch(oEvent) 
		/*
		LOCAL oEvt := oEvent AS @@event
		LOCAL msg AS DWORD
		LOCAL uRet AS USUAL
		
		
		SELF:EventReturnValue := 0L
		msg := oEvt:uMsg
		
		DO CASE
		CASE msg == WM_ERASEBKGND
			//RH PaintBackground is defined in our patch file
			IF SELF:PaintBackground(PTR(_CAST, oEvt:wParam))
				SELF:EventReturnValue := 1L
				RETURN 1L
			ENDIF
			
		CASE msg == WM_PAINT
			uRet := SELF:Expose(ExposeEvent{oEvt})
			
		CASE msg == WM_DRAWITEM
			//PP-031006 owner draw support, thanks to SEbert
			//Used for owner drawn menus or controls
			RETURN __Dispatch_DrawItem(oEvt, SELF)
			
		CASE msg == WM_MEASUREITEM
			//PP-031006 owner draw support
			//Used for owner drawn menus or controls
			RETURN __Dispatch_MeasureItem(oEvt, SELF)
			
		CASE msg == WM_MENUCHAR
			//PP-031006 owner draw support
			//Used for owner drawn menus or controls
			RETURN __Dispatch_MenuChar(oEvt, SELF)
		CASE msg == WM_WCHELP
			oParent:HelpRequest(HelpRequestEvent{oEvt})
			
		CASE msg == WM_ACTIVATE
			IF LoWord(oEvt:wParam) != 0
				uRet := SELF:Activate(oEvt)
			ELSE
				uRet := SELF:Deactivate(oEvt)
			ENDIF
			
			// WM_COMMAND trigged by (popup) menu
		CASE (msg == WM_COMMAND) .AND. (HiWord(oEvt:wParam) == 0) .AND. (oEvt:lParam == 0L)
			IF (oContextMenu != NULL_OBJECT)
				oParent:__PreMenuCommand(MenuCommandEvent{oEvt}:__SetMenu(oContextMenu))
				RETURN 1L
			ENDIF
			
			//PP-040421 Improved focus handling
		CASE msg == WM_SETFOCUS .OR. msg == WM_KILLFOCUS
			uRet := SELF:FocusChange(FocusChangeEvent{oEvt})
		CASE msg == WM_HSCROLL
			uRet := SELF:HorizontalScroll(ScrollEvent{oEvt})
			
		CASE msg == WM_KEYUP
			uRet := SELF:KeyUp(KeyEvent{oEvt})
			
		CASE msg == WM_KEYDOWN
			uRet := SELF:KeyDown(KeyEvent{oEvt})
			
			
		CASE msg == WM_LBUTTONDBLCLK .OR.;
			msg == WM_RBUTTONDBLCLK .OR.;
			msg == WM_MBUTTONDBLCLK .OR. ;
			msg == WM_XBUTTONDBLCLK
			uRet := SELF:MouseButtonDoubleClick(MouseEvent{oEvt})
			
		CASE msg == WM_LBUTTONDOWN .OR.;
			msg == WM_RBUTTONDOWN .OR.;
			msg == WM_MBUTTONDOWN .OR. ;
			msg == WM_XBUTTONDOWN
			uRet := SELF:MouseButtonDown(MouseEvent{oEvt})
			
		CASE msg == WM_LBUTTONUP .OR.;
			msg == WM_RBUTTONUP .OR.;
			msg == WM_MBUTTONUP .OR. ;
			msg == WM_XBUTTONUP
			uRet := SELF:MouseButtonUp(MouseEvent{oEvt})
			
		CASE msg == WM_VSCROLL
			uRet := SELF:VerticalScroll(ScrollEvent{oEvt})
			
			
		CASE msg == WM_MOUSEMOVE
			//IF IsMethod(oFormSurface, #__ToolTipHandle) .AND. (oFormSurface:__ToolTipHandle() != NULL_PTR)
			//	struMsg:oCtrl := oCtrl
			//	struMsg:message := WM_MOUSEMOVE
			//	struMsg:wParam := oEvt:wParam
			//	struMsg:lParam := oEvt:lParam
			//	GetCursorPos(@struMsg:pt)
			//	struMsg:time := DWORD(GetMessageTime())
			//	SendMessage(oFormSurface:__ToolTipHandle(), TTM_RELAYEVENT, 0, LONGINT(_CAST, @struMsg))
			//ENDIF
			
			IF oEvt:wParam != 0
				uRet := SELF:MouseDrag(MouseEvent{oEvt})
			ELSE
				uRet := SELF:MouseMove(MouseEvent{oEvt})
			ENDIF
			
		CASE msg == WM_MOVE
			uRet := SELF:Move(MoveEvent{oEvt})
			
		CASE msg == WM_SIZE
			uRet := SELF:Resize(ResizeEvent{oEvt})
			
		CASE (msg == WM_DROPFILES)
			IF IsMethod(oParent, #Drop)
				oParent:Drop(DragEvent{oEvt, SELF})
			ENDIF
			IF __LoadShellDll()
				PCALL(gpfnDragFinish, PTR(_CAST, oEvt:wParam))
			ENDIF
			RETURN 1L
			
		CASE (msg == WM_INITMENU) .OR. (msg == WM_INITMENUPOPUP)
			uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
			//if IsMethod(oParent, #MenuInit)
			//oParent:MenuInit(__ObjectCastClassPtr(oEvt, __pCMenuInitEvent))
			//endif
			
		CASE msg == WM_MENUSELECT
			uRet := SendMessage(oParent:Handle(), msg, oEvt:wParam, oEvt:lParam)
			//if (oEvent:lParam != 0) .and. IsMethod(oParent, #MenuSelect)
			//oParent:MenuSelect(__ObjectCastClassPtr(oEvt, __pCMenuSelectEvent))
			//endif
			
			//RvdH 050809 Removed next line
			//CASE msg == WM_CLOSE
			// __WCUnregisterControl(self)
			
			//PP-040425 improved context menu support, following code replaced with subsequent code
			// 	CASE msg == WM_CONTEXTMENU
			// 		IF (oContextMenu != NULL_OBJECT)
			// 			RETURN (SELF:EventReturnValue := 1L)
			// 		ENDIF
			
			//PP-040410 improved context menu support
		CASE msg == WM_CONTEXTMENU
			IF (oContextMenu != NULL_OBJECT)
				oContextMenu:ShowAsPopup(SELF, oEvt:lParam)
				RETURN (SELF:EventReturnValue := 1L)
			ENDIF
			
		CASE msg == WM_THEMECHANGED //SE-060526
			VerifyThemeState()
			InvalidateRect(oCtrl, NULL_PTR, TRUE)
			
		ENDCASE
		
		IF IsLong(uRet)
			SELF:EventReturnValue := uRet
			RETURN 1L
		ENDIF
		*/
		RETURN SELF:EventReturnValue
	

	METHOD Drop(oDragEvent) 
		// Also empty in GUI Classes
		RETURN NIL
	

	METHOD Enable() AS VOID STRICT
		IF SELF:ValidateControl()
			oCtrl:Enabled := TRUE
			oCtrl:Invalidate()
		ENDIF
		SELF:SetStyle(WS_DISABLED, FALSE)
		RETURN 
	

	METHOD Expose(oExposeEvent ) 
		// Also empty in GUI Classes
		RETURN NIL
	

	ACCESS FieldSpec AS FieldSpec
		RETURN oFieldSpec
	

	ASSIGN FieldSpec(oDSAssign AS FieldSpec) 
		oFieldSpec := oDSAssign
		lExplicitFS := TRUE
		
		// We need to update the control if is visible and connected to the server
		IF oServer != NIL .AND. oDataField!=NIL .OR. cbGetSetBlock != NULL_CODEBLOCK
			IF SELF:IsVisible()
				// If its modified update it to server
				// update it from server
				SELF:__Scatter()
			ENDIF
		ENDIF
		RETURN 
	

	METHOD FocusChange(oFocusChangeEvent ) 
		IF oParent != NULL_OBJECT
			oParent:ControlFocusChange(ControlFocusChangeEvent{oFocusChangeEvent, SELF})
		ENDIF
		RETURN NIL

	METHOD Handle() AS IntPtr CLIPPER
		IF (oCtrl == NULL_OBJECT )
			SELF:Create()
		ENDIF
		IF SELF:__IsValid 
			RETURN oCtrl:Handle
		ENDIF
		RETURN Intptr.Zero
	
	PROTECTED PROPERTY hWnd AS IntPtr GET Handle()
	
	METHOD HasBorder() AS LOGIC STRICT
		LOCAL lBorder		AS LOGIC
		LOCAL nStyle		AS LONG    

		nStyle := Win32.GetWindowLong(SELF:hWnd,GWL_EXSTYLE)
		IF _AND(nStyle,WS_EX_DLGMODALFRAME)>0
			lBorder := TRUE
		ELSEIF _AND(nStyle,WS_EX_WINDOWEDGE)>0
			lBorder := TRUE
		ELSEIF _AND(nStyle,WS_EX_STATICEDGE)>0
			lBorder := TRUE
		ELSEIF _AND(nStyle,WS_EX_CLIENTEDGE)>0
			lBorder := TRUE
		ELSE
			nStyle := Win32.GetWindowLong(SELF:hWnd,GWL_STYLE)
			IF _AND(nStyle,WS_BORDER)>0
				lBorder := TRUE
			ENDIF
		ENDIF
		RETURN lBorder

	METHOD Hide() 
		IF SELF:__IsValid 
			oCtrl:Visible := FALSE
		ENDIF		
		RETURN NIL
	

	METHOD HorizontalScroll(oScrollEvent ) 
		//Also empty in GUI Classes
		RETURN NIL

	ACCESS HyperLabel AS HyperLabel
		RETURN oHyperLabel
	
	ASSIGN HyperLabel(oNewHL AS HyperLabel) 
		oHyperLabel := oNewHL
		lExplicitHL := TRUE
		IF oNewHL != NULL_OBJECT
			SELF:Caption := oNewHL:Caption
			IF SELF:ValidateControl()
				oCtrl:AccessibleDescription := oNewHL:Description
				oCtrl:AccessibleName        := oNewHL:Name
			ENDIF
			IF SELF:UseHLForToolTip
				SELF:__AddTool(SELF)
			ENDIF
		ENDIF
		RETURN 
	

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cRegClass, kStyle, lDataAware) 
		SUPER()
		IF oOwner != NULL_OBJECT 
            local oOwnerObject := oOwner as OBJECT
			IF oOwnerObject IS SWF.Control
				SELF:oCtrl := (SWF.Control) oOwner
				SELF:oOrigin := oCtrl:Location
				SELF:oSize	 := oCtrl:Size
				IF oOwnerObject is IVOControl
					LOCAL oWrapper AS IVOControl
					oWrapper := (IVOControl) oOwner
					oWrapper:SetOwner(SELF)
				ENDIF
				RETURN
			ENDIF
		ENDIF		

		IF !IsNil(oPoint)
			SELF:oOrigin 	:= Point{oPoint:x, oPoint:y}
		ENDIF
		IF !IsNil(oDimension)
			SELF:oSize 		:= Dimension{oDimension:Width, oDimension:Height}
		ENDIF
		dwStyle := WS_CHILD
		IF !IsNil(kStyle)
			dwStyle := _OR(dwStyle, (LONG) kStyle)
		ENDIF
		
		oParent := oOwner

		
		IF IsObject(oOwner)
			oParent := oOwner
		ELSEIF IsPtr(oOwner)
			oParent := __ForeignWindow{oOwner}
		ELSE
			WCError{#Init,#Control,__WCSTypeError,oOwner,1}:@@Throw()
		ENDIF
		
		oFormSurface := oParent		
		IF IsLong(xID)   
			IF IsString(oPoint) .AND. IsNil(oDimension)
				xID := ResourceID{ xID }
			ELSE
				wID := xID
			ENDIF
		ELSE
			IF !IsInstanceOfUsual(xID,#ResourceID)
				WCError{#Init,#Control,__WCSTypeError,xID,2}:@@Throw()
			ENDIF               
		ENDIF
		
		
		
		IF (oFormSurface IS DialogWindow .OR. oFormSurface IS DataWindow)  .AND. ;
			(IsInstanceOfUsual(xID,#ResourceID) .OR. (IsLong(xID) .AND.;
			IsNil(oDimension) .AND. IsNil(oPoint)))
			
			IF IsInstanceOfUsual(xID,#ResourceID)
				wId 		:= xID:ID
			ELSE
				wId 		:= xID
			ENDIF
			
			dwStyle := WS_CHILD
			IF !IsNil(kStyle)
				dwStyle := _OR(dwStyle, (LONG) kStyle)
			ENDIF
			
			//oCtrl := GetDlgItem(oFormSurface:Handle(), wId)
			
			//__WCRegisterControl(SELF) //register after we get the handle
			//SetWindowLong(oCtrl, GWL_STYLE, _OR(GetWindowLong(oCtrl, GWL_STYLE), LONGINT(_CAST, dwStyle)))
			//SetWindowLong(oCtrl, GWL_EXSTYLE, LONGINT(_CAST, _AND(GetWindowLong(oCtrl, GWL_EXSTYLE), _NOT(WS_EX_NOPARENTNOTIFY))))
			
			//__lpfnDefaultProc := PTR(_CAST, GetWindowLong(oCtrl, GWL_WNDPROC))
			//SetWindowLong(oCtrl, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))
			
		ELSEIF (oFormSurface IS Window .OR. oFormSurface IS Control) .AND. IsLong(xID)
			IF !IsInstanceOfUsual(oPoint,#Point)
				WCError{#Init,#Control,__WCSTypeError,oPoint,3}:@@Throw()
			ENDIF
			IF !IsInstanceOfUsual(oDimension,#Dimension)
				WCError{#Init,#Control,__WCSTypeError,oDimension,4}:@@Throw()
			ENDIF
			
			IF !IsNil(cRegClass) .AND. !IsString(cRegClass)
				WCError{#Init,#Control,__WCSTypeError,cRegClass,5}:@@Throw()
			ENDIF
			
			cClassName := cRegClass
			
			IF (cClassName == "Edit" .OR. cClassName == "ListBox" .OR. cClassName == "SysAnimate32" .OR.;
				cClassName == "SysTreeView32" .OR. cClassName == "SysListView32" .OR. cClassName == "msctls_hotkey32")
				dwExStyle := _OR(dwExStyle, (LONG) WS_EX_CLIENTEDGE)
			ENDIF
			IF cClassName == "msctls_progress32"
				dwExStyle := _OR(dwExStyle, (LONG) WS_EX_CLIENTEDGE, (LONG) WS_EX_STATICEDGE)
			ENDIF
		ELSE
			WCError{#Init,#Control,__WCSTypeError}:@@Throw()
		ENDIF
		
		IF IsLogic(lDataAware)
			SELF:lDataAware:=lDataAware
		ENDIF
		
		IF SELF:lDataAware .AND. !(SELF IS DataBrowser) .AND. IsMethod(oParent, #__SetUpDataControl)
			SELF:Create()
			oParent:__SetupDataControl(SELF)
		ELSEIF oParent IS DataWindow VAR oDw
			SELF:Create()
			oDw:__SetupNonDataControl(SELF)
		ELSE
			SELF:Create()	
		ENDIF
		RETURN 
	
	PROPERTY IsDestroyed AS LOGIC GET SELF:lIsDestroyed

	ACCESS IsEditable AS LOGIC
		LOCAL lEditable	AS LOGIC
		lEditable := FALSE
		IF SELF:__IsValid 
			IF oCtrl:Visible
				IF oCtrl:Enabled
					IF  SELF IS BASELISTBOX .OR. ;
					    SELF IS MONTHCALENDAR .OR. ;
					    SELF IS DATETIMEPICKER .OR. ;
						SELF IS IPADDRESS
						lEditable := TRUE
					ENDIF
					IF SELF IS Edit VAR oEdit
						IF oEdit:ReadOnly
							lEditable := FALSE
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		RETURN lEditable

	METHOD IsEnabled() AS LOGIC CLIPPER
		IF SELF:__IsValid 
			RETURN oCtrl:Enabled
		ENDIF
		RETURN FALSE
	
	METHOD IsReadOnly() AS LOGIC STRICT
		IF SELF:__IsValid .AND. SELF IS Edit
			RETURN ((VOTextBox) SELF:oCtrl):ReadOnly
		ELSE
			RETURN FALSE
		ENDIF

	METHOD IsVisible 
		RETURN SELF:__IsValid .and. oCtrl:Visible	

	METHOD KeyDown(oKeyEvent) 
		//Also empty in GUI Classes
		RETURN NIL
	
	METHOD KeyUp(oKeyEvent) 
		//Also empty in GUI Classes
		RETURN NIL
	
	METHOD LinkDF(oDS, siDF) 
		LOCAL tmpDF AS OBJECT
		LOCAL symClassName AS SYMBOL
		
		IF !IsNumeric(siDF)
			siDF:=oDS:FieldPos(siDF)
		ENDIF	
		
		IF !IsInstanceOfUsual(oDS,#DataServer)
			WCError{#LinkDF,#Control,__WCSTypeError,oDS,1}:@@Throw()
		ENDIF
		
		IF (!IsNil(oServer) .AND. (oDS!=oServer))
			SELF:__Unlink()
		ENDIF
		
		oServer 	 := oDS
		siDataField  := siDF
		symDataField := oServer:FieldSym(siDataField)
		IF IsMethod(oServer, #IsBaseField)
			lBaseServer := Send(oServer, #IsBaseField, symDataField)
		ELSE   
			symClassName := ClassName(oServer)
			lBaseServer  := symClassName==#DBServer .OR. symClassName==#SQLSelect .OR. symClassName==#SQLTable .OR. symClassName==#JDataServer
		ENDIF
		
		// Propogate data field if no explicit one
		IF ((tmpDF := oServer:DataField(siDataField)) != NULL_OBJECT)
			oDataField := tmpDF
			
			IF !lExplicitFS
				// propogate field spec if no explicit one
				SELF:FieldSpec := SELF:oDataField:FieldSpec
				
				// propogate field spec
				IF !lExplicitHL
					// CHECK if NameSym and hyperlabel are same
					IF !IsNil(SELF:oDataField:HyperLabel) .AND. (oDataField:NameSym == oDataField:HyperLabel:NameSym)
						oHyperLabel := oDataField:HyperLabel
					ELSE
						//RvdH 060608 optimized: cCaption is a string
						//oHyperLabel := HyperLabel { oDataField:NameSym, iif(!Empty(cCaption), cCaption, oDataField:Name) }
						oHyperLabel := HyperLabel { oDataField:NameSym, IIF(SLen(cCaption)>0, cCaption, oDataField:Name) }
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		
		uGetSetOwner := NIL
		cbGetSetBlock := NULL_CODEBLOCK
		
		RETURN SELF
	

	METHOD MenuInit(oMenuInitEvent )  
		//Also empty in GUI Classes
		RETURN NIL
	
	METHOD MenuSelect(oMenuSelectEvent )  
		//Also empty in GUI Classes
		RETURN NIL
	ACCESS Modified AS LOGIC
		RETURN __lModified
	

	ASSIGN Modified(lChangedFlag  AS LOGIC) 
		__lModified := lChangedFlag
		RETURN  
	
	METHOD MouseButtonDoubleClick(oMouseEvent )  
		//Also empty in GUI Classes
		RETURN NIL 

	METHOD MouseButtonDown(oMouseEvent)  
		//Also empty in GUI Classes
		RETURN NIL 
	
	METHOD MouseButtonUp(oMouseEvent )  
		//Also empty in GUI Classes
		RETURN NIL
	
	METHOD MouseDrag(oMouseEvent )  
		//Also empty in GUI Classes
		RETURN NIL 
	
	METHOD MouseMove(oMouseEvent ) 
		//Also empty in GUI Classes
		RETURN NIL 
	
	METHOD Move(oMoveEvent ) 
		//Also empty in GUI Classes
		RETURN NIL
	
	ACCESS Name  AS STRING
		IF (SELF:HyperLabel  != NULL_OBJECT)
			RETURN SELF:HyperLabel :Name
		ENDIF
		
		RETURN NULL_STRING
	
	ACCESS NameSym AS SYMBOL
		IF (SELF:HyperLabel != NULL_OBJECT)
			RETURN SELF:HyperLabel:NameSym
		ENDIF
		RETURN NULL_SYMBOL
	
	ACCESS __OwnerOffSet AS Point
		LOCAL oCtrlOwner AS SWF.Control
		LOCAL oPoint AS Point
		LOCAL oPoint2 AS Point
		oPoint := Point{}
		IF SELF:oCtrl != NULL_OBJECT		
			oCtrlOwner 	:= SELF:oCtrl:Parent
			DO WHILE oCtrlOwner != NULL 
				IF oCtrlOwner is VOSurfacePanel
					EXIT
				ENDIF
				oPoint2 := oCtrlOwner:Location
				oPoint:X += oPoint2:X
				oPoint:Y += oPoint2:Y
				oCtrlOwner := oCtrlOwner:Parent
			ENDDO
		ENDIF
		RETURN oPoint
	
	ACCESS Origin AS Point
		IF (oCtrl == NULL_OBJECT .AND. oOrigin != NULL_OBJECT)
			RETURN oOrigin
		ELSE
			LOCAL oPoint AS Point
			oPoint := WC.GetOrigin(SELF)
			oPoint := oPoint + SELF:__OwnerOffSet
			RETURN oPoint
		ENDIF

	ASSIGN Origin(oPoint AS Point) 
		IF (oCtrl == NULL_OBJECT)
			SELF:oOrigin := Point{oPoint:X, oPoint:Y}
		ELSE
			oPoint := oPoint - SELF:__OwnerOffSet
			WC.MoveWindow(oCtrl, oPoint, TRUE)
		ENDIF
		RETURN 
	
	METHOD OverRide(lEnable AS LOGIC) AS VOID PASCAL
		//Also empty in GUI Classes
		RETURN 
	
	ACCESS Owner AS OBJECT
		RETURN SELF:oParent

	STATIC METHOD OwnerAlignmentHandledByWinForms(oC AS SWF.Control, iNewType AS USUAL) AS LOGIC
		LOCAL lStandard AS LOGIC
		LOCAL iType AS DWORD
		lStandard := TRUE
		IF IsNumeric(iNewType)
			iType := (DWORD) iNewType
        ENDIF
        IF IsLong(iNewType)
		    SWITCH iType
		    CASE OA_PX_Y
			    oC:Anchor := SWF.AnchorStyles.Bottom
		    CASE OA_X
			    oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Right
		    CASE OA_Y
			    oC:Anchor := SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom
		    CASE OA_X_Y
			    oC:Anchor := SWF.AnchorStyles.Bottom | SWF.AnchorStyles.Right
		    CASE OA_WIDTH
			    oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Right
		    CASE OA_HEIGHT
			    oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom
		    CASE OA_WIDTH_HEIGHT
			    oC:Anchor := SWF.AnchorStyles.Top | SWF.AnchorStyles.Left | SWF.AnchorStyles.Bottom| SWF.AnchorStyles.Right
		    CASE OA_FULL_SIZE
			    oC:Dock := SWF.DockStyle.Fill
		    CASE OA_LEFT_AUTOSIZE
			    oC:Dock := SWF.DockStyle.Left
		    CASE OA_RIGHT_AUTOSIZE
			    oC:Dock := SWF.DockStyle.Right
		    CASE OA_TOP_AUTOSIZE
			    oC:Dock := SWF.DockStyle.Top
		    CASE OA_BOTTOM_AUTOSIZE
			    oC:Dock := SWF.DockStyle.Bottom
		    OTHERWISE
			    lStandard := FALSE
            END SWITCH
         ELSE
            lStandard := FALSE
         ENDIF
		RETURN lStandard
	
	ASSIGN OwnerAlignment(iNewType AS USUAL) 
	
		IF ! Control.OwnerAlignmentHandledByWinForms(oCtrl, iNewType)
			IF oFormSurface IS Window
				oFormSurface:__AddAlign(SELF, iNewType)
			ELSE
				oParent:__AddAlign(SELF, iNewType)
			ENDIF
		ENDIF
		RETURN 
	


	METHOD PaintBackGround() CLIPPER
		RETURN FALSE

	METHOD PerformValidations() CLIPPER
		// Perform validations for Control against supplied parameter
		// if it has a data spec, otherwise just return true
		oHLStatus := NULL_OBJECT
		IF oFieldSpec != NULL_OBJECT
			IF !oFieldSpec:PerformValidations(uValue)
				oHLStatus := oFieldSpec:Status
				RETURN FALSE
				// elseif !oDataField:PerformValidations(uValue)
				// oHLStatus:=oDataField:Status
				// return false
			ENDIF
		ENDIF
		RETURN TRUE
	

	METHOD RegisterTimer(nInterval,lOneTime) 
		IF !IsLong(nInterval)
			WCError{#RegisterTimer,#Control,__WCSTypeError,nInterval,1}:@@Throw()
		ENDIF
		
		IF !IsNil(lOneTime)
			IF !IsLogic(lOneTime)
				WCError{#RegisterTimer,#Control,__WCSTypeError,lOneTime,2}:@@Throw()
			ENDIF
			IF lOneTime
				dwTimerInterval:=0
			ELSE
				dwTimerInterval := nInterval
			ENDIF
		ELSE
			dwTimerInterval := nInterval
		ENDIF
		
		IF (nInterval > 0)
			dwTimerCount := nInterval
			IF !lTimerRegistered
				WC.RegisterTimer(SELF)
				lTimerRegistered := TRUE
			ENDIF
		ELSE
			WC.UnregisterTimer(SELF)
			lTimerRegistered := FALSE
		ENDIF
		
		RETURN SELF

	ACCESS ReadOnly AS LOGIC
		RETURN SELF:IsEnabled()

	ASSIGN ReadOnly(lNewValue AS LOGIC) 
		IF (lNewValue)
			SELF:Disable()
		ELSE
			SELF:Enable()
		ENDIF
		RETURN 



	METHOD RePaint() AS VOID STRICT
		IF SELF:__IsValid 
			oCtrl:Invalidate()
		ENDIF
		RETURN 

	METHOD Resize(oResizeEvent ) 
		RETURN NIL

	METHOD RestoreUpdate() AS VOID STRICT
		IF SELF:__IsValid 
			SELF:oCtrl:ResumeLayout()
		ENDIF
		RETURN 
	
	METHOD SendToBack() AS VOID CLIPPER
		IF SELF:ValidateControl()
			oCtrl:SendToBack()
		ENDIF

	ACCESS Server AS DataServer
		RETURN oServer

	METHOD SetExStyle(kExStyle AS LONG, lEnable := TRUE AS LOGIC) AS VOID STRICT
		LOCAL liTemp AS LONG
		
		IF oCtrl == NULL_OBJECT
			IF lEnable
				dwExStyle := _OR(kExStyle, dwExStyle)
			ELSE
				dwExStyle := _AND(dwExStyle, _NOT(kExStyle))
			ENDIF
		ELSE
			LOCAL iCtrl AS IVOControl
			iCtrl := (IVOControl) (OBJECT) oCtrl			
			liTemp := Win32.GetWindowLong(hWnd, GWL_EXSTYLE)
			iCtrl:ControlProperties:SetExStyle(kExStyle, lEnable)
			IF lEnable
				liTemp := _OR(kExStyle, liTemp)
			ELSE
				liTemp := _AND(liTemp, _NOT(kExStyle))
			ENDIF
			Win32.SetWindowLong(hWnd, GWL_EXSTYLE, liTemp)
		ENDIF
		
		RETURN 
	

	METHOD SetFocus() AS VOID
		
		IF SELF:ValidateControl()
			oCtrl:Focus()
		ENDIF
		RETURN 
		
	ACCESS HasFocus AS LOGIC
		RETURN SELF:__IsValid .and. oCtrl:Focused

	METHOD HasStyle(kStyle AS LONG)
		LOCAL liStyle	AS LONG
		liStyle := Win32.GetWindowLong(SELF:hWnd,GWL_STYLE)
		RETURN _AND(liStyle,kStyle) != 0
	
	ACCESS Style AS LONG
		RETURN Win32.GetWindowLong(SELF:hWnd,GWL_STYLE)

	METHOD SetStyle(kStyle AS LONG, lEnable := TRUE AS LOGIC) 
		LOCAL liTemp as LONG
		IF (oCtrl == NULL_OBJECT .or. ! oCtrl:IsHandleCreated)
			IF lEnable
				dwStyle := _OR( kStyle, dwStyle)
			ELSE
				dwStyle := _AND(dwStyle, _NOT(kStyle))
			ENDIF
		ELSE
			LOCAL iCtrl AS IVOControl
			iCtrl := (IVOControl) (OBJECT) oCtrl			
			liTemp := Win32.GetWindowLong(SELF:hWnd,GWL_STYLE)				
			iCtrl:ControlProperties:SetStyle(kStyle, lEnable)
			IF lEnable
				liTemp := _OR(kStyle, liTemp)
			ELSE
				liTemp := _AND(liTemp, _NOT(kStyle))
			ENDIF
			dwStyle := liTemp // without this line it does not work for custom drawn labels
			Win32.SetWindowLong(hWnd, GWL_STYLE,  liTemp)
		ENDIF
		
		RETURN SELF
	

	METHOD Show() AS VOID 
		
		IF (oCtrl == NULL_OBJECT)
			SELF:Create()
		ENDIF
		IF SELF:__IsValid 
			oCtrl:Show()
		ENDIF
		
		RETURN 
	
	
	METHOD ShowToolTip() AS VOID STRICT
		IF SELF:oFormSurface != NULL_OBJECT
			SELF:oFormSurface:__ShowToolTip(SELF)
		ENDIF
		RETURN

	ACCESS Size  AS Dimension
		LOCAL oResult AS Dimension
		IF SELF:ValidateControl()
			oResult := oCtrl:Size
		ELSE
			oResult := SELF:oSize
		ENDIF
		RETURN oResult
	

	ASSIGN Size(oDimension AS Dimension) 
		IF (oCtrl == NULL_OBJECT)
			oSize		:= oDimension:Clone()
		ELSE
			SELF:oCtrl:Size := oDimension
		ENDIF
		
		RETURN 
	

	ACCESS Status AS HyperLabel
		RETURN SELF:oHLStatus
	

	ASSIGN Status(oStatus AS HyperLabel) 
		oHLStatus := oStatus
		RETURN 
	
	METHOD SuspendUpdate() AS VOID STRICT
		IF SELF:__IsValid 
			SELF:oCtrl:SuspendLayout()
		ENDIF
		RETURN 
	
	ACCESS TextValue AS STRING
		//Also empty in GUI Classes
		RETURN ""

	ASSIGN TextValue (cNewText AS STRING) 
		//Also empty in GUI Classes
		RETURN 
	

	METHOD Timer()  CLIPPER
		//Also empty in GUI Classes
		RETURN NIL
	

	ACCESS ToolTipText AS STRING
		RETURN sToolTipText
	

	ASSIGN ToolTipText(sNewText AS STRING) 
		sToolTipText := sNewText
	
	ACCESS UseHLForToolTip AS LOGIC 
		RETURN lUseHLForToolTip

	ASSIGN UseHLForToolTip(lNewValue AS LOGIC) 
		lUseHLForToolTip := lNewValue
	
	METHOD ValidateControl() 
		IF (oCtrl == NULL_OBJECT)
			SELF:Create()
		ENDIF
		RETURN SELF:__IsValid 
	

	ACCESS Value 
		// returns last valid value
		// value is only updated on leaving the field
		RETURN uValue

	ASSIGN Value(uNewValue) 
		//
		// Value assignment
		// Note : value does not have to be same as what is displayed
		//
		LOCAL cOldValue AS STRING
		cOldValue := AsString(uValue)
		// !!! should be result of FIELDGET and located after FIELDPUT !!!
		
		SELF:__Value := uNewValue //Update the control
		IF oServer != NULL_OBJECT
			IF lBaseServer // if not subclassing
				oServer:FIELDPUT(siDataField,SELF:uValue) //update the DataServer
			ELSEIF symDataField != NULL_SYMBOL
				IVarPut(oServer, symDataField ,SELF:uValue)
			ELSE
				IVarPut(oServer, SELF:NameSym, SELF:uValue)
			ENDIF
		ENDIF
		SELF:ValueChanged := !(cOldValue == AsString(uValue))
		
		RETURN 
	

	PROPERTY ValueChanged AS LOGIC GET lChanged SET SELF:lChanged := VALUE
	

	METHOD VerticalScroll(oScrollEvent) 
		//Also empty in GUI Classes
		RETURN NIL

	#region Extensions
	ASSIGN DynAlignment( symMode AS SYMBOL) 
		// Ignored, but still here in case it is used in a script 
		RETURN 

	ACCESS IsDisabled AS LOGIC
		RETURN SELF:__IsValid  .and. !oCtrl:Enabled

	ASSIGN IsDisabled(lValue AS LOGIC) 
		IF lValue
			SELF:Disable()
		ELSE
			SELF:Enable()
		ENDIF
		RETURN 

	ACCESS IsHidden( )  AS LOGIC
		RETURN SELF:__IsValid  .and. !oCtrl:Visible

	ASSIGN IsHidden( lValue  AS LOGIC) 
		IF SELF:__IsValid 
			oCtrl:Visible := ! lValue
		ENDIF
		RETURN 
		
	#endregion
	
END CLASS
