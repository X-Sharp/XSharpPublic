//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



#define SYSTEM_KEYCODE 0x20000000
USING System.Diagnostics

INTERFACE INamedEvent
    PROPERTY Name    AS STRING GET
    PROPERTY NameSym AS SYMBOL GET
    PROPERTY Window  AS Window GET
END INTERFACE

CLASS @@Event //inherit object
	PROPERTY hWnd 	    AS IntPtr       AUTO GET PROTECTED SET
	PROPERTY uMsg 	    AS DWORD        AUTO GET PROTECTED SET
	PROPERTY wParam 	AS DWORD        AUTO GET PROTECTED SET
	PROPERTY lParam 	AS LONGINT      AUTO GET PROTECTED SET
	PROPERTY oWindow    AS OBJECT       AUTO GET PROTECTED SET
	ACCESS Handle AS IntPtr
		RETURN hWnd


	[DebuggerStepThrough];
	CONSTRUCTOR() STRICT
		SUPER()

	[DebuggerStepThrough];
	CONSTRUCTOR(oEvt AS @@Event)
		SUPER()
		hWnd	:= oEvt:hWnd
		uMsg	:= oEvt:uMsg
		lParam	:= oEvt:lParam
		wParam	:= oEvt:wParam
		RETURN

	[DebuggerStepThrough];
	CONSTRUCTOR (m REF System.Windows.Forms.Message)
		SUPER()
		hWnd	:= m:HWnd
		uMsg	:= (DWORD)	m:Msg
		lParam	:= (INT)	m:LParam
		wParam	:= (DWORD)	m:WParam

	[DebuggerStepThrough];
	CONSTRUCTOR(_hWnd AS IntPtr, _uMsg AS DWORD, _wParam AS DWORD, _lParam AS LONG, _oWindow := NULL_OBJECT AS Window)
		SUPER()
		hWnd	:= _hWnd
		uMsg	:= _uMsg
		wParam	:= _wParam
		lParam	:= _lParam
		oWindow := _oWindow
		RETURN

	ACCESS Message AS DWORD STRICT
		RETURN uMsg

	ACCESS Window AS Window STRICT
		IF (oWindow == NULL_OBJECT)
			RETURN WC.GetWindowByHandle(hWnd)
		ENDIF
		RETURN oWindow

END CLASS

CLASS MinMaxInfoEvent INHERIT @@Event

	[DebuggerStepThrough];
	CONSTRUCTOR(m REF System.Windows.Forms.Message)
		SUPER(m)


	ACCESS MaxPosition AS Point STRICT
		LOCAL sMinMax AS _WINMINMAXINFO

		sMinMax := IntPtr{lParam}
		RETURN Point{sMinMax:ptMaxPosition:X, sMinMax:ptMaxPosition:Y}

	ASSIGN MaxPosition(oPoint AS Point)  STRICT
		LOCAL oMaxPos  AS Point
		LOCAL sMinMax  AS _WINMINMAXINFO

		oMaxPos  := oPoint
		sMinMax  := IntPtr{lParam}
		sMinMax:ptMaxPosition:X := oMaxPos:X
		sMinMax:ptMaxPosition:Y := oMaxPos:Y
		RETURN

	ACCESS MaxSize AS Dimension STRICT
		LOCAL sMinMax AS _WINMINMAXINFO

		sMinMax := IntPtr{lParam}
		RETURN Dimension{sMinMax:ptMaxSize:X, sMinMax:ptMaxSize:Y}

	ASSIGN MaxSize(oSize AS Dimension)  STRICT
		LOCAL oMaxSize AS Dimension
		LOCAL sMinMax  AS _WINMINMAXINFO

		oMaxSize := oSize
		sMinMax  := IntPtr{lParam}
		sMinMax:ptMaxSize:X := oMaxSize:Width
		sMinMax:ptMaxSize:Y := oMaxSize:Height
		RETURN

	ACCESS MaxTrackSize AS Dimension STRICT
		LOCAL sMinMax AS _WINMINMAXINFO

		sMinMax := IntPtr{lParam}
		RETURN Dimension{sMinMax:ptMaxTrackSize:X, sMinMax:ptMaxTrackSize:Y}

	ASSIGN MaxTrackSize(oSize AS Dimension)  STRICT
		LOCAL oMaxSize AS Dimension
		LOCAL sMinMax  AS _WINMINMAXINFO

		oMaxSize := oSize
		sMinMax  := IntPtr{lParam}
		sMinMax:ptMaxTrackSize:X := oMaxSize:Width
		sMinMax:ptMaxTrackSize:Y := oMaxSize:Height
		RETURN

	ACCESS MinTrackSize AS Dimension STRICT
		LOCAL sMinMax AS _WINMINMAXINFO

		sMinMax := IntPtr{lParam}
		RETURN Dimension{sMinMax:ptMinTrackSize:X, sMinMax:ptMinTrackSize:Y}

	ASSIGN MinTrackSize(oSize AS Dimension)  STRICT
		LOCAL oMinSize AS Dimension
		LOCAL sMinMax  AS _WINMINMAXINFO

		oMinSize := oSize
		sMinMax  := IntPtr{lParam}
		sMinMax:ptMinTrackSize:X := oMinSize:Width
		sMinMax:ptMinTrackSize:Y := oMinSize:Height
		RETURN

END CLASS

CLASS ResizeEvent INHERIT @@Event

	[DebuggerStepThrough];
	CONSTRUCTOR() STRICT
		SUPER()

	[DebuggerStepThrough];
	CONSTRUCTOR(m REF System.Windows.Forms.Message)
		SUPER(m)


	ACCESS Height AS LONGINT STRICT
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		RETURN HiWord(dw)

	ACCESS Size AS Dimension STRICT
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		RETURN Dimension{LoWord(dw), HiWord(dw)}

	ACCESS Width AS LONGINT STRICT
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		RETURN LoWord(dw)

END CLASS

CLASS FocusChangeEvent INHERIT @@Event
	PROTECT lGotFocus AS LOGIC
	PROPERTY GotFocus AS LOGIC GET lGotFocus
	CONSTRUCTOR() STRICT
		SUPER()
	CONSTRUCTOR(plGotFocus AS LOGIC) STRICT
		SUPER()
		lGotFocus := plGotFocus
	CONSTRUCTOR(_hWnd AS IntPtr, _uMsg AS DWORD, _wParam AS DWORD, _lParam AS LONG, _oWindow := NULL_OBJECT AS Window)
		SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)
		lGotFocus := _uMsg == WM_SETFOCUS
END CLASS



CLASS HelpRequestEvent INHERIT @@Event

	PROPERTY HelpType		AS LONG AUTO
	PROPERTY HelpContext	AS STRING GET SELF:HyperLabel:HelpContext
	PROPERTY HyperLabel		AS HyperLabel AUTO
	PROPERTY Control		AS Control GET oControl
	PROPERTY Position		AS POINT GET ev:MousePos
	PROTECTED ev AS System.Windows.Forms.HelpEventArgs
	PROTECTED oControl AS Control

	CONSTRUCTOR(e AS System.Windows.Forms.HelpEventArgs, sender AS OBJECT)
		SUPER()
		IF sender IS IVOControlProperties VAR oC
			SELF:oControl := oC:Control
			SELF:oWindow  := oControl:Owner
			SELF:HyperLabel := oControl:HyperLabel
			SELF:HelpType	:= HELPCONTROL
        elseif sender is IVOForm  var oW
            local oWin as Window
			oWin := oW:Window
			self:HyperLabel := oWin:HyperLabel
			SELF:HelpType	:= HELPWINDOW
		// HELPMENU
		ELSE
			SELF:HelpType  := HELPINFO
		ENDIF
		RETURN

	/*
	ACCESS @@HelpInfo AS PTR STRICT
	//SE-060522


	IF uMsg = WM_HELP
	RETURN PTR(_CAST, lParam)
	ENDIF

	RETURN NULL_PTR

	ACCESS HelpType AS DWORD STRICT


	IF uMsg = WM_HELP
	RETURN HELPINFO
	ENDIF

	RETURN wParam


	ACCESS ItemID AS DWORD STRICT
	LOCAL dwID AS DWORD

	IF wParam == HELPMENU
	RETURN LoWord(DWORD(_CAST,lParam))
	ELSEIF wParam == HelpControl
	IF (dwID := DWORD(GetWindowLong(INT(LoWord(DWORD(lParam))), GWL_ID))) == 0xFFFF
	RETURN dwID
	ELSE
	RETURN _AND(dwID,_NOT(WC_CTRL_Mask))
	ENDIF
	ENDIF
	RETURN 0

	ACCESS Position AS POINT STRICT
	LOCAL strucPoint IS _WinPoint



	GetCursorPos(@strucPoint)
	ScreenToClient(oWindow:Handle(), @strucPoint)

	RETURN __WCConvertPoint(oWindow,Point{strucPoint:X,strucPoint:Y})

	ACCESS WindowRegion() AS LONGINT STRICT


	IF wParam == HelpWindow
	RETURN lParam
	ENDIF

	RETURN RegionUnknown
	*/
END CLASS


CLASS AppCommandEvent INHERIT @@Event

	#region static methods
	STATIC METHOD Get_Flags_lParam(lParam AS DWORD) AS WORD
		RETURN LoWord(lParam)

	STATIC METHOD Get_AppCommand_lParam(lParam AS DWORD) AS DWORD
		RETURN _and(HiWord(lParam),_not(FAPPCOMMAND_MASK))

	STATIC METHOD Get_KeyState_lParam(lParam AS DWORD) AS WORD
		RETURN AppCommandEvent.Get_Flags_lParam(lParam)

	STATIC METHOD Get_Device_lParam(lParam AS DWORD) AS WORD
		RETURN  (WORD) _and(HiWord(lParam), FAPPCOMMAND_MASK)

	STATIC METHOD Get_MouseOrKey_lParam(lParam AS DWORD) AS WORD
		RETURN AppCommandEvent.Get_Device_lParam(lParam)

	#endregion

	CONSTRUCTOR(m REF System.Windows.Forms.Message)
		SUPER(m)

	ACCESS Command AS DWORD STRICT
		RETURN AppCommandEvent.Get_AppCommand_lParam(DWORD(_CAST,SELF:lParam))


	ACCESS IsControl AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_CONTROL) > 0

	ACCESS IsDeviceKey AS LOGIC STRICT
		RETURN AppCommandEvent.Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_KEY

	ACCESS IsDeviceMouse AS LOGIC STRICT
		RETURN AppCommandEvent.Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_MOUSE

	ACCESS IsDeviceOEM AS LOGIC STRICT
		RETURN AppCommandEvent.Get_Device_lParam(DWORD(_CAST,SELF:lParam)) == FAPPCOMMAND_OEM

	ACCESS IsLeftButton AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_LBUTTON) > 0

	ACCESS IsMiddleButton AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_MBUTTON) > 0

	ACCESS IsRightButton AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_RBUTTON) > 0

	ACCESS IsShift AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_SHIFT) > 0

	ACCESS IsXButton1 AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON1) > 0

	ACCESS IsXButton2 AS LOGIC STRICT
		RETURN _AND(WORD(AppCommandEvent.Get_KeyState_lParam(DWORD(_CAST,SELF:lParam))),MK_XBUTTON2) > 0

	ACCESS oTarget AS OBJECT STRICT
		RETURN WC.GetObjectByHandle(IntPtr{(LONG) SELF:wParam})


END CLASS




CLASS DragEvent INHERIT @@Event
	PROTECT Args AS System.Windows.Forms.DragEventArgs
	PROTECT oControl AS Control
	ACCESS Control AS OBJECT STRICT
	RETURN SELF:oControl

	CONSTRUCTOR( e AS System.Windows.Forms.DragEventArgs, sender AS VOPanel)
		LOCAL oPt AS System.Drawing.Point
		LOCAL oChild AS OBJECT

		//LOCAL oParent AS OBJECT
		//LOCAL lFoundToolBar AS LOGIC
		//LOCAL nX, nY AS System.Int32
		//lFoundToolBar := FALSE
		//nX := e:X
		//nY := e:Y
		//oParent := sender
		//// To find the control where the drop action took place, we need to find the
		//// right position of the Form, starting from the global screen position.
		//// So we need to substract the bounds of all parents
		//DO WHILE oParent != NULL_OBJECT
		//	IF IsAccess(oParent,#Bounds)
		//		LOCAL oBounds AS System.Drawing.Rectangle
		//		oBounds := oParent:Bounds
		//		nX := nX - oBounds:X
		//		nY := nY - oBounds:X
		//		// Substract the height of the Toolbar once
		//		IF !lFoundToolBar .AND. IsAccess(oParent,#ToolBar)
		//			nY := nY - oParent:ToolBar:Height
		//			lFoundToolBar := TRUE
		//		ENDIF
		//		oParent := oParent:Parent
		//	ELSE
		//		oParent := NULL_OBJECT
		//	ENDIF
		//ENDDO
		//// Create a point from the calculated coordinates and get the
		//// control at this position
		//oPt := System.Drawing.Point{nX,nY}

		//For Testing Purpose since the calculation above did not work:
		// let Winforms try to find the right control by working some magic :-)
		oPt := sender:PointToClient(System.Windows.Forms.Cursor.Position)
		oChild := sender:GetChildAtPoint(oPt)
		IF oChild != NULL_OBJECT .AND. IsAccess(oChild,#Control)
			SELF:oControl := IVarGet(oChild,#Control)
		ENDIF
		SUPER()
		uMsg := WM_DROPFILES
		SELF:Args := e

	METHOD FileName(nfile)
	LOCAL aFiles AS STRING[]
	aFiles := (STRING[])SELF:Args:Data:GetData(System.Windows.Forms.DataFormats.FileDrop)
	RETURN aFiles[ nfile ]

	ACCESS FileCount
	LOCAL aFiles AS STRING[]
	aFiles := (STRING[])SELF:Args:Data:GetData(System.Windows.Forms.DataFormats.FileDrop)
	RETURN aFiles:Length
	/*
	//RvdH 061218 Declared properties for performance
	PROTECT oControl AS Control
	ACCESS Control AS OBJECT STRICT


	RETURN SELF:oControl

	ACCESS FileCount
	//local strucDragInfo as __WCDragInfo


	//if uMsg==WM_QueryDropObject //Riz This is undocumented windows stuff
	//Riz This is undocumented windows stuff
	//strucDragInfo := ptr(_cast,lParam)
	// Get source app (File Manager, usually) DS
	//wSourceDS := _Or( _And( GetWindowWord(StrucDragInfo.hWndSource,GWW_HINSTANCE), 0xFFFC), 1)
	//pszP := psz(_cast,(dword(wSourceDS)<<16) + StrucDragInfo.pszList)
	//return Occurs(Psz2String(pszP)," ")
	//elseif uMsg==WM_DropFiles

	IF (uMsg == WM_DROPFILES) .AND. __LoadShellDll()
	RETURN PCALL(gpfnDragQueryFile, wParam, 0Xffffffff, NULL_PSZ, 0)
	ENDIF

	RETURN 0

	METHOD FileName(nfile)
	LOCAL dwSize AS DWORD
	LOCAL pszBuf AS PSZ
	LOCAL cBuf AS STRING



	IF !IsLong(nFile)
	WCError{#FileName,#DragEvent,__WCSTypeError,nfile,1}:Throw()
	ENDIF

	nfile--

	IF ((uMsg == WM_DROPFILES) .OR. ((uMsg == WM_QUERYDROPOBJECT) .AND. (wParam != 0))) .AND. __LoadShellDll()
	dwSize := PCALL(gpfnDragQueryFile, PTR(_CAST, wParam), nfile, NULL_PSZ, 0) + 1
	IF (dwSize > 0)
	pszBuf := MemAlloc(dwSize)
	PCALL(gpfnDragQueryFile, PTR(_CAST, wParam), nfile, pszBuf, dwSize)
	cBuf := Psz2String(pszBuf)
	MemFree(pszBuf)
	ENDIF
	ENDIF

	RETURN cBuf

	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)

	IF !IsPtr(_hWnd)
	IF IsInstanceOfUsual(_uMsg, #Control)
	SELF:oControl := _uMsg
	ENDIF
	ENDIF

	RETURN

	ACCESS Origin
	LOCAL strucPoint IS _WinPoint
	LOCAL strucRect IS _WinRect

	//if uMsg==WM_QueryDropObject //Riz This is undocumented windows stuff
	//Riz Gets the current cursor position, not the position when the
	//message was created. To get the position of the cursor when the message
	//was created we would have to use undocumented windows stuff.
	GetCursorPos(@strucPoint) //current cursor position
	GetWindowRect(hWnd,@strucRect) //client window rectangle
	RETURN Point{strucPoint:X-strucRect:Left,strucRect:Bottom-strucPoint:Y}
	//elseif uMsg==WM_DropFiles
	//RvdH 070116 Unreachable code.
	// 	IF (uMsg == WM_DropFiles) .and. __LoadShellDll()
	// 		PCALL(gpfnDragQueryPoint, wParam, @strucPoint)
	// 		RETURN Point{strucPoint.X,strucPoint.Y}
	// 	ENDIF

	// 	RETURN Point{0,0}
	*/
END CLASS



CLASS ExposeEvent INHERIT @@Event
	PROTECT Args AS System.Windows.Forms.PaintEventArgs
	CONSTRUCTOR( e AS System.Windows.Forms.PaintEventArgs)
		SUPER()
		uMsg := WM_PAINT
		SELF:Args := e

	ACCESS ExposedArea AS BoundingBox STRICT
		RETURN Args:ClipRectangle

	ACCESS Graphics AS System.Drawing.Graphics
		RETURN Args:Graphics
END CLASS



CLASS KeyEvent INHERIT @@Event

	PROPERTY ASCIIChar	    AS DWORD    AUTO GET PRIVATE SET
	PROPERTY KeyCode		AS LONG     AUTO GET PRIVATE SET
	PROPERTY RepeatCount	AS LONGINT  AUTO GET PRIVATE SET
	PROPERTY System		    AS LOGIC    AUTO GET PRIVATE SET

	[DebuggerStepThrough];
	CONSTRUCTOR(oKeyEvent AS System.Windows.Forms.KeyEventArgs)
		SUPER()
		uMsg := WM_KEYDOWN
		ASCIIChar := (DWORD) oKeyEvent:KeyValue
		KeyCode   := oKeyEvent:KeyValue
		System	  := oKeyEvent:Modifiers != 0
		RETURN

	[DebuggerStepThrough];
	CONSTRUCTOR(oKeyEvent AS System.Windows.Forms.KeyPressEventArgs)
		SUPER()
		uMsg := WM_CHAR
		ASCIIChar := oKeyEvent:KeyChar
		wParam    := oKeyEvent:KeyChar
		KeyCode   := oKeyEvent:KeyChar
		RETURN

	[DebuggerStepThrough];
	CONSTRUCTOR() STRICT
		SUPER()
END CLASS






CLASS MouseEvent INHERIT @@Event
	PROTECT me AS System.Windows.Forms.MouseEventArgs
	PROTECT keys AS System.Windows.Forms.Keys
	[DebuggerStepThrough];
	CONSTRUCTOR(oE AS System.Windows.Forms.MouseEventArgs, oK AS System.WIndows.Forms.Keys) STRICT
		SUPER()
		me := oE
		keys := oK


	//RvdH 061218 Declared properties for performance
	ACCESS ButtonID AS LONGINT STRICT
		RETURN (INT) me:Button

	ACCESS Height AS LONGINT STRICT
		RETURN HiWord(DWORD(lParam))

	ACCESS IsControlButton AS LOGIC STRICT
		RETURN _AND(keys, System.Windows.Forms.Keys.Control) != 0

	ACCESS IsLeftButton AS LOGIC STRICT
		RETURN me:Button == System.Windows.Forms.MouseButtons.Left

	ACCESS IsMiddleButton AS LOGIC STRICT
		RETURN me:Button == System.Windows.Forms.MouseButtons.Middle

	ACCESS IsRightButton AS LOGIC STRICT
		RETURN me:Button == System.Windows.Forms.MouseButtons.Right

	ACCESS IsShiftButton AS LOGIC STRICT
		RETURN _AND(keys, System.Windows.Forms.Keys.Shift) != 0

	ACCESS IsXButton1 AS LOGIC STRICT
		RETURN me:Button == System.Windows.Forms.MouseButtons.XButton1

	ACCESS IsXButton2 AS LOGIC STRICT
		RETURN me:Button == System.Windows.Forms.MouseButtons.XButton2

	ACCESS Position AS Point STRICT
		RETURN Point{me:X, me:Y}

	ACCESS Size AS Dimension STRICT
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		RETURN Dimension{LoWord(dw), HiWord(dw)}

	ACCESS Width AS LONGINT STRICT
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		RETURN LoWord(dw)

END CLASS

CLASS MoveEvent INHERIT @@Event
	[DebuggerStepThrough];
	CONSTRUCTOR()
		SUPER()

	ACCESS Origin AS Point STRICT
		//SE-080520
		LOCAL dw := DWORD(_CAST,lParam) AS DWORD
		//RETURN __WCConvertPoint(oWindow, Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))})
		RETURN Point{SHORT(_CAST, LoWord(dw)), SHORT(_CAST, HiWord(dw))}

END CLASS








