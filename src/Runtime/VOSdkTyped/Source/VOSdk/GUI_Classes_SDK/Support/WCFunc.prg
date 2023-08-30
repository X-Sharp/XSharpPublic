//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//




USING System.Runtime.InteropServices
USING VOSDK := XSharp.VO.SDK
/// <exclude />
FUNCTION __SetAppObject(oNewApp AS App) AS App STRICT

	BEGIN LOCK WC.CSApp
		oApp := oNewApp
	END LOCK

	RETURN oApp

/// <exclude />
FUNCTION GetObjectByHandle(hwnd AS IntPtr) AS OBJECT STRICT
	RETURN WC.GetObjectByHandle(hwnd)

/// <exclude />

INTERNAL DELEGATE TimerProcDelegate( hWnd AS IntPtr, uMsg AS DWORD, idEvent AS DWORD, dwTime AS DWORD ) AS VOID

/// <exclude />
STATIC CLASS WC
	STATIC PROPERTY CoordinateSystem AS LOGIC AUTO
	STATIC EXPORT INITONLY CartesianCoordinates := TRUE AS LOGIC
	STATIC EXPORT INITONLY WindowsCoordinates := FALSE AS LOGIC

	#region Timer Fields
	STATIC HIDDEN TimerProcDelegate AS TimerProcDelegate
	STATIC HIDDEN TimerProcPtr AS IntPtr
	STATIC HIDDEN TimerObjectID AS DWORD
	STATIC HIDDEN TimerObjects AS System.Collections.Generic.List<ITimer>
	#endregion
	//STATIC EXPORT CSHDC AS OBJECT
	STATIC EXPORT CSApp AS OBJECT
	//STATIC EXPORT DCCurHDCOwner AS Window
	//STATIC EXPORT __WCMenuList AS ARRAY


	STATIC CONSTRUCTOR
		//AppDomain.CurrentDomain:ProcessExit += System.EventHandler{ NULL, @GUIExit() }
		TimerProcDelegate := TimerProcDelegate{ NULL, @TimerProc() }
		TimerProcPtr        := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) TimerProcDelegate )
		TimerObjects		:= System.Collections.Generic.List<ITimer>{}
		WC.CSApp := OBJECT{}
		//WC.CoordinateSystem := WC.CartesianCoordinates
		//WC.CSHDC := OBJECT{}

		RETURN


	// This is registered as an ProcessExit event handler, so it needs
	// a different signature in Vulcan than it does in VO, which uses
	// _RegisterWEP() (which isn't supported in Vulcan).
	//STATIC METHOD GUIExit( o AS OBJECT, args AS EventArgs ) AS VOID
		//WC.DCClear()
		//WC.DeleteCriticalSections()
		//WC.UnregisterClasses()
	//	RETURN


	STATIC METHOD ConvertPoint(oWindow AS OBJECT, oPoint AS Point) AS Point STRICT
		LOCAL sRect  := WINRECT{} AS WINRECT
		LOCAL yCoord AS INT

		IF CoordinateSystem == WC.CartesianCoordinates
			IF oWindow == NULL_OBJECT .OR. oWindow IS App
				yCoord := GuiWin32.GetSystemMetrics(SM_CYSCREEN) - oPoint:Y
			elseif oWindow is Window var oWin
				GuiWin32.GetClientRect(oWin:Handle(4), ref sRect)
				yCoord :=  sRect:bottom - oPoint:Y
			elseif oWindow is IGuiObject var OC
				GuiWin32.GetClientRect(OC:__Handle, ref sRect)
				yCoord :=  sRect:bottom - oPoint:Y
			endif
		ELSE // Windows Coordinate System
			yCoord := oPoint:Y
		ENDIF

		RETURN Point{oPoint:X, yCoord}

	//STATIC METHOD DeleteCriticalSections() AS VOID
	//	RETURN


	STATIC METHOD DIBFromBitmap(hbm AS IntPtr) AS IntPtr
		//LOCAL bm IS _winBITMAP
		//LOCAL bi IS _winBITMAPINFOHEADER
		//LOCAL lpbi AS _winBITMAPINFO
		//LOCAL dwLen AS DWORD
		//LOCAL hdib AS PTR
		//LOCAL h AS PTR
		//LOCAL hdc AS PTR
		//LOCAL hpal AS PTR
		//LOCAL ptemp AS PTR
		//LOCAL biStyle AS DWORD
		//LOCAL wBits AS WORD
		//LOCAL iTemp AS INT
		//LOCAL iRet AS INT

		//hpal := GetStockObject(DEFAULT_PALETTE)

		//GetObject(hbm, _SIZEOF(_winBITMAP), @bm)

		//biStyle := BI_RGB
		//wBits := bm:bmPlanes * bm:bmBitsPixel

		//bi:biSize := _SIZEOF(_winBITMAPINFOHEADER)
		//bi:biWidth := bm:bmWidth
		//bi:biHeight := bm:bmHeight
		//bi:biPlanes := 1
		//bi:biBitCount := wBits
		//bi:biCompression := biStyle
		//bi:biSizeImage := 0
		//bi:biXPelsPerMeter := 0
		//bi:biYPelsPerMeter := 0
		//bi:biClrUsed := 0
		//bi:biClrImportant := 0

		//dwLen := bi:biSize + __WCPaletteSize(@bi)

		//hdc := GetDC(NULL_PTR)
		//hpal := SelectPalette(hdc, hpal, FALSE)
		//RealizePalette(hdc)

		//hdib := GlobalAlloc(GHND, dwLen)

		//IF (hdib == NULL_PTR)
		//	SelectPalette(hdc, hpal, FALSE)
		//	ReleaseDC(0, hdc)
		//	RETURN NULL_PTR
		//ENDIF

		//lpbi := GlobalLock(hdib)

		//MemCopy(lpbi, @bi, _SIZEOF(_winBITMAPINFOHEADER))

		//iRet := GetDIBits(hdc, hbm, 0, DWORD(bi:biHeight), 0, lpbi, DIB_RGB_COLORS)

		//MemCopy(@bi, lpbi, _SIZEOF(_winBITMAPINFOHEADER))

		//GlobalUnlock(hdib)

		///* If the driver did not fill in the biSizeImage field, make one up */
		//IF (bi:biSizeImage == 0)
		//	iTemp := bm:bmWidth * wBits
		//	iTemp := iTemp + 31
		//	iTemp := (iTemp / 32 * 4)
		//	bi:biSizeImage := DWORD(iTemp * bm:bmHeight)
		//	IF (biStyle != BI_RGB)
		//		bi:biSizeImage := (bi:biSizeImage * 3) / 2
		//	ENDIF
		//ENDIF

		///* realloc the buffer big enough to hold all the bits */
		//dwLen := bi:biSize + __WCPaletteSize(@bi) + bi:biSizeImage

		//h := GlobalReAlloc(hdib, dwLen, 0)
		//IF (h != NULL_PTR)
		//	hdib := h
		//ELSE
		//	GlobalFree(hdib)
		//	hdib := 0

		//	SelectPalette(hdc, hpal, FALSE)
		//	ReleaseDC(0, hdc)
		//	RETURN hdib
		//ENDIF

		//lpbi := GlobalLock(hdib)

		//pTemp := PTR(_CAST, LONGINT(_CAST, lpbi) + LONGINT(_CAST, lpbi:bmiHeader:biSize) + LONGINT(_CAST, __WCPaletteSize(lpbi)))

		//iRet := GetDIBits(hdc, hbm, 0, DWORD(bi:biHeight), ptemp, lpbi, DIB_RGB_COLORS)
		//IF (iRet == 0)
		//	GlobalUnlock(hdib)
		//	hdib := 0
		//	SelectPalette(hdc, hpal, FALSE)
		//	ReleaseDC(0, hdc)
		//	RETURN 0
		//ENDIF

		//MemCopy(@bi, lpbi, _SIZEOF(_winBITMAPINFOHEADER))
		//GlobalUnlock(hdib)

		//SelectPalette(hdc, hpal, FALSE)
		//ReleaseDC(0, hdc)

		//RETURN hdib
		RETURN IntPtr.Zero

	STATIC METHOD GetBrushColor(oBrush AS Brush) AS DWORD
		IF oBrush != NULL_OBJECT
			RETURN oBrush:Color:ColorRef
		ENDIF
		RETURN 0

	// New generic function that returns an object from a handle, assuming
	// it is registered as property
	//STATIC METHOD GetObjectByProperty(hWnd AS PTR) AS OBJECT STRICT
	//	LOCAL oObject AS OBJECT
		//LOCAL strucSelf AS SelfPtr
		//IF (hWnd != NULL_PTR)
		//	strucSelf   := WC.GetProperty(hWnd)
		//	oObject     := WC.SelfPtr2Object(strucSelf)
		//ENDIF
	//	RETURN oObject

	// This function now returns a Control
	STATIC METHOD GetControlByHandle(hWnd AS IntPtr) AS VOSDK.Control STRICT
		LOCAL oC AS System.Windows.Forms.Control
		oC := System.Windows.Forms.Control.FromHandle(hWnd)
		IF oC IS IVOControlProperties VAR oVOC
    		RETURN oVOC:Control
		ENDIF
		RETURN NULL_OBJECT

	STATIC METHOD GetMenuByHandle(hMenu AS IntPtr) AS Menu STRICT
		LOCAL oMenu AS OBJECT
		//LOCAL dwIndex, iLen AS DWORD
		//LOCAL p AS PTR PTR
		//IF (hMenu != NULL_PTR)
		//	iLen := ALen(WC.MenuList)
		//	FOR dwIndex := 1 TO iLen
		//		IF (WC.MenuList[dwIndex][2] == hMenu)
		//			EXIT
		//		ENDIF
		//	NEXT
		//	//dwIndex := AScan(__WCMenuList, {|x| x[2] == hMenu})
		//	IF (dwIndex != (iLen+1))
		//		p := (PTR) WC.MenuList[dwIndex][1]
		//		oMenu := GCHandle.FromIntPtr( PTR( p ) ):Target
		//	ENDIF
		//ENDIF

		RETURN oMenu


	static method GetOrigin(oObject as IGuiObject) as point strict
		LOCAL hWnd AS PTR
		LOCAL oParent AS Window
		LOCAL rect := WINRECT{} AS WinRect
		LOCAL point := WINPOINT{} AS WinPoint
		if WC.CoordinateSystem == WC.WindowsCoordinates
			IF oObject IS Control VAR oC
				RETURN oC:__Control:Location
			elseif oObject is Window var oWin1
				LOCAL oForm AS VOForm
				oForm := oWin1:__Form
				IF oForm != NULL_OBJECT
					RETURN oForm:Location
				ELSE
					RETURN Point{}
				ENDIF
			ENDIF
		ENDIF
		GuiWin32.GetWindowRect(oObject:__Handle, ref rect)
		point:x := rect:left
		IF WC.CoordinateSystem // Cartesian Coordinate System
			point:y := rect:bottom
		ELSE // Windows Coordinate System
			point:y := rect:top
		ENDIF

		if oObject is Control var oC
			oParent := oC:__FormSurface
		elseif oObject is Window var oWin1
			oParent := oWin1:__Parent
		ENDIF

		if oParent != null_object .and. oParent is Window var oWin
			hWnd := oWin:Handle(4)
			GuiWin32.ScreenToClient(hWnd, REF point)
		ENDIF

		RETURN ConvertPoint(oParent, Point{point:x, point:y})
	//RETURN Point{}*/

	//FUNCTION __WCGetPictureCoordinates(hWnd AS PTR, hDCPrinter AS PTR, lpRC AS _winRect) AS VOID STRICT
	//	//LOCAL pt IS _winPoint
	//	LOCAL hDCTemp:= GetDC(hWnd) AS PTR
	//	LOCAL ratioX := 0 AS INT
	//	LOCAL ratioY := 0 AS INT
	//	LOCAL nShrinkX, nShrinkY AS FLOAT
	//	LOCAL nWidth, nHeight AS INT
	//   LOCAL nX, nY AS INT
	//	GetWindowRect(hWnd, lpRc)
	//	OffsetRect(lpRc, - lpRc:left, - lpRc:top)

	//	IF (hDCTemp != NULL_PTR)
	//		ratioX := GetDeviceCaps(hDCTemp, LOGPIXELSX)
	//		ratioY := GetDeviceCaps(hDCTemp, LOGPIXELSY)
	//		ReleaseDC(hWnd, hDCTemp)
	//	ENDIF

	//	IF ((ratioX == 0) .OR. (ratioY == 0))
	//		RETURN
	//	ENDIF

	//	lpRc:right := MulDiv(lpRc:right, GetDeviceCaps(hDCPrinter, LOGPIXELSX), ratioX)
	//	lpRc:bottom := MulDiv(lpRc:bottom, GetDeviceCaps(hDCPrinter, LOGPIXELSY), ratioY)

	//   nWidth 	:= GetDeviceCaps(hDCPrinter, PHYSICALWIDTH)
	//   nHeight 	:= GetDeviceCaps(hDCPrinter, PHYSICALHEIGHT)
	//   nX 		:= GetDeviceCaps(hDCPrinter, PHYSICALOFFSETX)
	//   nY 		:= GetDeviceCaps(hDCPrinter, PHYSICALOFFSETY)

	//   IF (lpRC:right > nWidth-nX .OR. lpRC:bottom> nHeight-nY)
	//	// e.g. Image = 800 - 600, paper = 200 - 300
	//	// shrinkX = 200/800 = 0.25
	//	// shrinkY = 300/600 = 0.50
	//	// we must shrink with 0.25 to fit.
	//	nShrinkX := FLOAT(nWidth-nX) / FLOAT(lpRC:Right)
	//	nShrinkY := FLOAT(nHeight-nY) / FLOAT(lpRC:bottom)

	//	IF nShrinkY < nShrinkX
	//			lpRC:right 	:= INT(FLOAT(lpRC:Right) * nShrinkY)
	//			lpRC:bottom := INT(FLOAT(lpRC:bottom) * nShrinkY)
	//	ELSE
	//			lpRC:right 	:= INT(FLOAT(lpRC:Right) * nShrinkX)
	//			lpRC:bottom := INT(FLOAT(lpRC:bottom) * nShrinkX)
	//	ENDIF
	//		lpRC:left 	:= nX
	//	lpRC:top		:= nY
	//   ELSE
	//		OffsetRect(lpRC, (nWidth / 2) - (lpRC:right / 2), (nHeight / 2) - (lpRC:bottom / 2))
	//		OffsetRect(lpRC, - nX, - nY)
	//   ENDIF

	//	//IF (Escape(hDCPrinter, GETPHYSPAGESIZE, 0, NULL, @pt) >= 0)
	//	//	OffsetRect(lpRc, (pt.x / 2) - (lpRc.right / 2), (pt.y / 2) - (lpRc.bottom / 2))
	//	//ENDIF
	//	//IF (Escape(hDCPrinter, GETPRINTINGOFFSET, 0, NULL, @pt) >= 0)
	//	//	OffsetRect(lpRC, - pt.X, - pt.Y)
	//	//ENDIF
	//	RETURN

// 	STATIC METHOD GetTopLeftPoint(oControl AS OBJECT) AS Point STRICT
// 		LOCAL oPoint AS Point
// 		IF CoordinateSystem // Cartesian Coordinate System
// 			oPoint := oControl:Origin
// 			RETURN Point{oPoint:X, oPoint:Y + oControl:Size:Height - 1}
// 		ENDIF
// 		//else Windows Coordinate System
// 		RETURN oControl:Origin

	STATIC METHOD GetWindowByHandle(hWnd AS IntPtr) AS Window STRICT
		LOCAL oForm AS OBJECT
		oForm := System.Windows.Forms.Form.FromHandle(hWnd)
		IF oForm IS IVOForm VAR oVOF
			RETURN oVOF:Window
		ENDIF
		RETURN NULL_OBJECT

	STATIC METHOD GetWindowMenuPosition(hMenu AS IntPtr) AS INT STRICT
		// Used in CAVOOLE to retrieve window menu position for in-place activation
		LOCAL o AS Menu
		o := GetMenuByHandle(hMenu)
		IF (o != NULL_OBJECT)
			RETURN o:GetAutoUpdate()
		ENDIF
		RETURN -1

	//STATIC METHOD GetWindowObjectByHandle(hWnd AS IntPtr) AS OBJECT STRICT
		// Wrapper for compatibility only
	//	RETURN GetControlByHandle(hWnd)


	STATIC METHOD IsTrueTypeEnabled() AS LOGIC STRICT
		//RETURN GetProfileInt(PSZ(_CAST, "TrueType"), PSZ(_CAST, "TTEnable"), 1) == 1
		RETURN TRUE

	//STATIC METHOD LogicalBackgroundBrush(oWindow AS Window, strucLogBrush AS _WinLogBrush) AS PTR STRICT
	//	LOCAL hBr AS PTR
	//	LOCAL oBrush AS Brush

	//	oBrush:=oWindow:Background
	//	IF oBrush != NULL_OBJECT
	//		hBr := oBrush:Handle()
	//	ELSE
	//		hBr := GetClassLong(oWindow:Handle(),GCL_HBRBACKGROUND)
	//	ENDIF


	//	IF (hBr == NULL_PTR)
	//		IF IsInstanceOf(oWindow, #DialogWindow)
	//			hBr := GetSysColorBrush(COLOR_3DFACE)
	//		ELSE
	//			hBr := GetSysColorBrush(COLOR_WINDOW)
	//		ENDIF
	//	ENDIF

	//	GetObject(hBr, _SIZEOF(_WinLogBrush), strucLogBrush)

	//	RETURN hBr


	//STATIC METHOD LogicalBrush(oBrush AS Brush, strucLogBrush AS _WinLogBrush) AS VOID STRICT
	//	IF oBrush==NULL_OBJECT
	//		strucLogBrush:lbColor := Color{ColorBlack}:ColorRef
	//		strucLogBrush:lbHatch := 0
	//		strucLogBrush:lbStyle := BS_SOLID
	//	ELSE
	//		GetObject( oBrush:Handle(), _SIZEOF(_WinLogBrush), strucLogBrush)
	//	ENDIF
	//	RETURN

	//FUNCTION __WCLogicalPen(oPen AS Pen, strucLogPen AS _WinLogPen) AS VOID STRICT
	//	IF oPen == NULL_OBJECT
	//		strucLogPen:lopnStyle := PS_Solid
	//		strucLogPen:lopnColor := Color{ColorBlack}:ColorRef
	//		strucLogPen:lopnWidth:X := 1
	//	ELSE
	//		GetObject( oPen:Handle(), _SIZEOF(_WinLogPen), strucLogPen)
	//	ENDIF

	//	RETURN

	STATIC EXPORT MenuList := {} AS ARRAY
	STATIC METHOD PaletteSize(pIH AS IntPtr) AS DWORD STRICT
		RETURN 0

	//STATIC METHOD RegisterControl(oControl AS Control) AS VOID
		//LOCAL strucSelf AS SelfPtr
		//strucSelf := WC.SelfPtrAlloc(oControl)
		//WC.RegisterProperty(oControl:Handle(), strucSelf)
		//RETURN


	//[Obsolete];
	//STATIC METHOD RegisterMenu(oObject AS Menu, hMenu AS PTR) AS VOID
		//LOCAL idx, iLen AS DWORD
		//LOCAL p AS PTR PTR// as SelfPtr
		//LOCAL lFound	AS LOGIC


		//BEGIN LOCK __WCMenuList

		//	iLen := ALen(__WCMenuList)
		//	lFound := FALSE
		//	FOR idx := 1 TO iLen
		//		IF (__WCMenuList[idx][2] == hMenu)
		//			lFound := TRUE
		//			EXIT
		//		ENDIF
		//	NEXT
		//	IF ! lFound
		//		p := MemAlloc( (DWORD) sizeof( IntPtr ) )
		//		PTR(p) := GCHandle.ToIntPtr( GCHandle.Alloc( oObject ) )
		//		AAdd(__WCMenuList, {p, hMenu}) // menu object, menu handle, ref count
		//	ENDIF

		//END LOCK

		//RETURN


	STATIC METHOD SetROP(hDC AS IntPtr, rop AS DWORD) AS IntPtr STRICT
		LOCAL hLastRop AS IntPtr


		//hLastRop := GetROP2(hDC)

		//DO CASE
		//CASE (rop == ROPInvert)
		//	SetROP2(hDC, R2_NOT)
		//CASE (rop == ROPXOR)
		//	SetROP2(hDC, R2_XORPEN)
		//OTHERWISE
		//	SetROP2(hDC, R2_COPYPEN)
		//ENDCASE

		RETURN hLastRop





	//STATIC METHOD UnregisterClasses()
	//	RETURN NIL


	//STATIC METHOD UnRegisterControl(hwndCtl AS IntPtr) AS VOID
	//	WC.UnregisterProperty(hwndCtl) // This also frees the memory of the SelfPtr structure
	//	RETURN

	STATIC METHOD UseTrueTypeOnly() AS LOGIC STRICT

		//RETURN GetProfileInt(PSZ(_CAST, "TrueType"), PSZ(_CAST, "TTOnly"), 0) == 1
		RETURN FALSE

	STATIC METHOD GetObjectByHandle(hWnd AS IntPtr) AS OBJECT STRICT
		LOCAL w  AS Window
		LOCAL c  AS Control
		w := WC.GetWindowByHandle(hWnd)
		IF w != NULL_OBJECT
			RETURN w
		ENDIF
		c := WC.GetControlByHandle(hWnd)
		IF (c != NULL_OBJECT)
			w := c:__ControlWindow
			IF (w != NULL_OBJECT)
				RETURN w
			ELSE
				RETURN c
			ENDIF
		ENDIF
		// When not a control and not a window, then maybe a menu ?
		RETURN WC.GetMenuByHandle(hWnd)



	//STATIC METHOD DCTop(oObject AS OBJECT) AS VOID STRICT
	//
	//	RETURN

	STATIC METHOD GetCoordinateSystem() AS LOGIC
	    RETURN WC.CoordinateSystem

	//STATIC METHOD NewControlsAvailable() AS LOGIC
	//	RETURN (gpfnInitCommonControlsEx != NULL_PTR)

	STATIC METHOD SetCoordinateSystem(system AS LOGIC) AS LOGIC
	    RETURN WC.CoordinateSystem := system

	STATIC METHOD AppGetDialogWindow() AS OBJECT STRICT
		LOCAL oRet AS OBJECT

		IF (oApp != NULL_OBJECT)
			oRet := oApp:GetDialogWindow()
		ENDIF

		RETURN oRet

 	STATIC METHOD AppSetDialogWindow(oSurface AS VOPanel) AS VOID STRICT

		IF (oApp != NULL_OBJECT)
			oApp:SetDialogWindow(oSurface)
		ENDIF
		RETURN


	/*
	STATIC METHOD DCAdd(oObject AS OBJECT) AS VOID STRICT
		BEGIN LOCK WC.CSHDC
			IF (WC.DCCurHDCOwner != NULL_OBJECT)
				WC.DCCurHDCOwner:__ReleaseDC()
			ENDIF
			WC.DCCurHDCOwner := oObject
		END LOCK
		RETURN


	STATIC METHOD DCClear() AS VOID STRICT

		BEGIN LOCK WC.CSHDC
			IF (WC.DCCurHDCOwner != NULL_OBJECT)
				WC.DCCurHDCOwner:__ReleaseDC()
			ENDIF
		END LOCK
		RETURN

	STATIC METHOD DCDelete(oObject AS OBJECT) AS VOID STRICT

		BEGIN LOCK WC.CSHDC
			IF (oObject == WC.DCCurHDCOwner)
				WC.DCCurHDCOwner := NULL_OBJECT
			ENDIF
		END LOCK
		RETURN


	*/

	STATIC METHOD StretchDibBlt(hdc AS IntPtr, x AS INT, y AS INT, dx AS INT, dy AS INT, hdib AS IntPtr) AS LOGIC STRICT
		//LOCAL pBuf AS PSZ
		//LOCAL lRet AS LONGINT
		//LOCAL lpbi AS _winBITMAPINFO

		//lpbi := GlobalLock(hdib)

		//IF (lpbi == NULL_PTR)
		//	RETURN FALSE
		//ENDIF

		//pBuf := PSZ(_CAST , DWORD(_CAST, lpbi) + lpbi:bmiHeader:biSize + __WCPaletteSize(lpbi))
		//lRet := StretchDIBits(hdc, x, y, dx, dy, 0, 0, lpbi:bmiHeader:biWidth, lpbi:bmiHeader:biHeight, pBuf, lpbi, DIB_RGB_COLORS, SRCCOPY)
		//GlobalUnlock(hdib)

		//RETURN (lRet > 0)
		RETURN FALSE




	STATIC METHOD MoveWindow(oCtrl AS IVOUIObject, oPoint AS Point, bRepaint AS LOGIC) AS VOID
		LOCAL lX, lY AS LONGINT

		lX := oPoint:X
		lY := oPoint:Y
		IF WC.CoordinateSystem // Cartesian Coordinate System
			lY := lY - oCtrl:Height
		ENDIF
		oPoint := Point{lX, lY}

		IF ((Point) oCtrl:Location) != oPoint
			oCtrl:Location := oPoint
		ENDIF

		IF bRepaint
			oCtrl:PerformLayout()
		ENDIF
		RETURN




	#region Timers
	STATIC METHOD RegisterTimer(oNew AS ITimer) AS DWORD
		BEGIN LOCK WC.TimerObjects
			IF ! WC.TimerObjects:Contains(oNew)
				WC.TimerObjects:Add(oNew)
			ENDIF
			IF WC.TimerObjects:Count == 1
				WC.TimerObjectID := GuiWin32.SetTimer(NULL_PTR, 0, 956, TimerProcPtr )
			ENDIF

		END LOCK

		RETURN WC.TimerObjectID

	STATIC METHOD UnregisterTimer(oDel AS ITimer) AS VOID STRICT

		BEGIN LOCK TimerObjects
			IF WC.TimerObjects:Contains(oDel)
				WC.TimerObjects:Remove(oDel)
			ENDIF
		END LOCK


		RETURN

	STATIC METHOD TimerProc(hWnd AS IntPtr, uMsg AS DWORD, idEvent AS DWORD, dwTime AS DWORD) AS VOID
		IF (ErrorLevel() > ES_WHOCARES)
			RETURN
		ENDIF

		BEGIN LOCK WC.TimerObjects
			LOCAL IMPLIED aObjects := WC.TimerObjects:ToArray()
			FOREACH IMPLIED oTimer IN aObjects
				oTimer:__Timer()
			NEXT
		END LOCK


		RETURN
	#endregion

END CLASS



FUNCTION Enable3dControls() AS LOGIC
	//LOCAL lCTL3DEnabledFlag AS LOGIC

	//lCTL3DEnabledFlag:=Globals.CTL3DEnabledFlag //Old value
	////	Ctl3dRegister( _GetInst())
	////	Ctl3dAutoSubclass( _GetInst())
	//Ctl3DEnabledFlag:=TRUE

	RETURN TRUE

FUNCTION GetAppObject() AS App STRICT
	LOCAL oRet AS App

	BEGIN LOCK WC.CSApp
		oRet := oApp
	END LOCK

	RETURN oRet

FUNCTION GetFocusedObject() AS OBJECT STRICT
	RETURN WC.GetObjectByHandle(GuiWin32.GetFocus())

FUNCTION IsCtl3dEnabled() AS LOGIC
	RETURN TRUE

FUNCTION SetClassStyle(hWnd AS IntPtr, dwSetStyle AS DWORD, lEnable := TRUE AS LOGIC) AS DWORD STRICT
	LOCAL dwOldStyle AS DWORD

	IF (hWnd != NULL_PTR)
		dwOldStyle := (DWORD) GuiWin32.GetClassLong(hWnd, GCL_STYLE)

		IF lEnable
			dwSetStyle := _OR(dwOldStyle, dwSetStyle)
		ELSE
			dwSetStyle := _AND(dwOldStyle, _NOT(dwSetStyle))
		ENDIF

		IF dwOldStyle != dwSetStyle
			GuiWin32.SetClassLong(hWnd, GCL_STYLE, LONGINT(_CAST, dwSetStyle))
		ENDIF
	ENDIF

	RETURN dwSetStyle




//INTERNAL VOSTRUCT SelfPtr
//MEMBER ptrSelf AS IntPtr



