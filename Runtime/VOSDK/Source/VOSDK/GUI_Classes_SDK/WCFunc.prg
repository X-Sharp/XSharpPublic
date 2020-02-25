#ifdef __VULCAN__
   #using System.Runtime.InteropServices
#endif

//INTERNAL VOSTRUCT __ptr
//	MEMBER p AS PTR

FUNCTION __SetAppObject(oNewApp AS App) AS App STRICT

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
	oApp := oNewApp
	END LOCK
#else
	EnterCriticalSection(@__WCCSApp)

	oApp := oNewApp

	LeaveCriticalSection(@__WCCSApp)
#endif	

	RETURN oApp

FUNCTION __WCConvertPoint(oWindow AS OBJECT, oPoint AS Point) AS Point STRICT
    //SE-080520 optimized version 
    LOCAL sRect  IS _WINRECT
    LOCAL yCoord AS INT
    
    IF __WCCoordinateSystem // Cartesian Coordinate System
        IF oWindow == NULL_OBJECT .OR. IsInstanceOf(oWindow,#App)
            yCoord := GetSystemMetrics(SM_CYSCREEN) - oPoint:Y
        ELSEIF IsInstanceOf(oWindow,#Window)
            GetClientRect(oWindow:Handle(4), @sRect)
            yCoord :=  sRect:bottom - oPoint:Y
        ELSE // The parent is a control 
            GetWindowRect(oWindow:Handle(), @sRect)
            yCoord := sRect:bottom - sRect:top - oPoint:Y
        ENDIF
    ELSE // Windows Coordinate System
        yCoord := oPoint:Y
    ENDIF

    RETURN Point{oPoint:X, yCoord}


STATIC GLOBAL __WCCoordinateSystem := WCCartesianCoordinates AS LOGIC

#ifdef __VULCAN__ 

   // We don't use critical sections in the Vulcan version because __WCDeleteCriticalSection()
   // ends up being called before the GC finalizes all of the window objects.  When a window is
   // destroyed, its destructor (Axit) calls WCUnregisterMenu(), and since the critical section in
   // __WCCSMenu has already been destroyed and this causes an access violation on the call to
   // EnterCriticalSection() in WCUnregisterMenu(). 
   
   // So, rather than using native CriticalSection objects, we use .NET locks.  For menus and
   // timers, the critical sections protected the arrays __WCMenuList and __WCTimerObjects so we can
   // just lock on those objects.  For oApp and __WCDCCurHDCOwner, we need something constant to
   // lock on so we create two objects here and use those as the lock targets. This eliminates
   // timing issues, since none of these objects will be destroyed until the last reference goes
   // out of scope, and we don't have to worry about the timing of the garbage collector and the
   // invocation of window finalizers.
   
   STATIC GLOBAL __WCCSHDC := OBJECT{} AS OBJECT
   GLOBAL __WCCSApp := OBJECT{} AS OBJECT
#else
STATIC GLOBAL __WCCopyright := "Copyright (C) 1993-1995 Computer Associates International Inc." AS STRING
GLOBAL __WCCSApp IS _winRTL_CRITICAL_SECTION

STATIC GLOBAL __WCCSHDC IS _winRTL_CRITICAL_SECTION
STATIC GLOBAL __WCCSMenu IS _winRTL_CRITICAL_SECTION
STATIC GLOBAL __WCCSTimer IS _winRTL_CRITICAL_SECTION
#endif
STATIC GLOBAL __WCDCCurHDCOwner AS Window

FUNCTION __WCDeleteCriticalSections()
   #ifndef __VULCAN__
	DeleteCriticalSection(@__WCCSMenu)
	DeleteCriticalSection(@__WCCSTimer)
	DeleteCriticalSection(@__WCCSHDC)
	DeleteCriticalSection(@__WCCSApp)
	#endif   
	RETURN NIL

FUNCTION __WCDIBFromBitmap(hbm AS PTR) AS PTR STRICT
	LOCAL bm IS _winBITMAP
	LOCAL bi IS _winBITMAPINFOHEADER
	LOCAL lpbi AS _winBITMAPINFO
	LOCAL dwLen AS DWORD
	LOCAL hdib AS PTR
	LOCAL h AS PTR
	LOCAL hdc AS PTR
	LOCAL hpal AS PTR
	LOCAL ptemp AS PTR
	LOCAL biStyle AS DWORD
	LOCAL wBits AS WORD
	LOCAL iTemp AS INT
	LOCAL iRet AS INT

	hpal := GetStockObject(DEFAULT_PALETTE)

	GetObject(hbm, _SIZEOF(_winBITMAP), @bm)

	biStyle := BI_RGB
	wBits := bm:bmPlanes * bm:bmBitsPixel

	bi:biSize := _SIZEOF(_winBITMAPINFOHEADER)
	bi:biWidth := bm:bmWidth
	bi:biHeight := bm:bmHeight
	bi:biPlanes := 1
	bi:biBitCount := wBits
	bi:biCompression := biStyle
	bi:biSizeImage := 0
	bi:biXPelsPerMeter := 0
	bi:biYPelsPerMeter := 0
	bi:biClrUsed := 0
	bi:biClrImportant := 0

	dwLen := bi:biSize + __WCPaletteSize(@bi)

	hdc := GetDC(NULL_PTR)
	hpal := SelectPalette(hdc, hpal, FALSE)
	RealizePalette(hdc)

	hdib := GlobalAlloc(GHND, dwLen)

	IF (hdib == NULL_PTR)
		SelectPalette(hdc, hpal, FALSE)
		ReleaseDC(0, hdc)
		RETURN NULL_PTR
	ENDIF

	lpbi := GlobalLock(hdib)

	MemCopy(lpbi, @bi, _SIZEOF(_winBITMAPINFOHEADER))

	iRet := GetDIBits(hdc, hbm, 0, DWORD(bi:biHeight), 0, lpbi, DIB_RGB_COLORS)

	MemCopy(@bi, lpbi, _SIZEOF(_winBITMAPINFOHEADER))

	GlobalUnlock(hdib)

	/* If the driver did not fill in the biSizeImage field, make one up */
	IF (bi:biSizeImage == 0)
		iTemp := bm:bmWidth * wBits
		iTemp := iTemp + 31
		iTemp := (iTemp / 32 * 4)
		bi:biSizeImage := DWORD(iTemp * bm:bmHeight)
		IF (biStyle != BI_RGB)
			bi:biSizeImage := (bi:biSizeImage * 3) / 2
		ENDIF
	ENDIF

	/* realloc the buffer big enough to hold all the bits */
	dwLen := bi:biSize + __WCPaletteSize(@bi) + bi:biSizeImage

	h := GlobalReAlloc(hdib, dwLen, 0)
	IF (h != NULL_PTR)
		hdib := h
	ELSE
		GlobalFree(hdib)
		hdib := 0

		SelectPalette(hdc, hpal, FALSE)
		ReleaseDC(0, hdc)
		RETURN hdib
	ENDIF

	lpbi := GlobalLock(hdib)

	pTemp := PTR(_CAST, LONGINT(_CAST, lpbi) + LONGINT(_CAST, lpbi:bmiHeader:biSize) + LONGINT(_CAST, __WCPaletteSize(lpbi)))

	iRet := GetDIBits(hdc, hbm, 0, DWORD(bi:biHeight), ptemp, lpbi, DIB_RGB_COLORS)
	IF (iRet == 0)
		GlobalUnlock(hdib)
		hdib := 0
		SelectPalette(hdc, hpal, FALSE)
		ReleaseDC(0, hdc)
		RETURN 0
	ENDIF

	MemCopy(@bi, lpbi, _SIZEOF(_winBITMAPINFOHEADER))
	GlobalUnlock(hdib)

	SelectPalette(hdc, hpal, FALSE)
	ReleaseDC(0, hdc)

	RETURN hdib

FUNCTION __WCGetBrushColor(oBrush AS Brush) AS DWORD
	LOCAL strucLogBrush IS _WinLogBrush

	GetObject (oBrush:Handle(), _SIZEOF(_WinLOGBRUSH), @strucLogBrush)

	RETURN strucLogBrush:lbColor


// New generic function that returns an object from a handle, assuming
// it is registered as property
FUNCTION __WCGetObjectByProperty(hWnd AS PTR) AS OBJECT STRICT
	LOCAL oObject AS OBJECT
	LOCAL strucSelf AS SelfPtr
	IF (hWnd != NULL_PTR)
		strucSelf   := __WCGetProperty(hWnd)
		oObject     := __WCSelfPtr2Object(strucSelf)
	ENDIF
	RETURN oObject

// This function now returns a Control
FUNCTION __WCGetControlByHandle(hWnd AS PTR) AS Control STRICT
	LOCAL oObject AS OBJECT
	oObject := __WCGetObjectByProperty(hWnd)
	IF oObject != NULL_OBJECT .and. IsInstanceOf(oObject, #Control)
	   RETURN (Control) oObject
   ENDIF
	RETURN NULL_OBJECT

FUNCTION __WCGetMenuByHandle(hMenu AS PTR) AS Menu STRICT
	LOCAL dwIndex, iLen AS DWORD
	LOCAL oMenu AS OBJECT
	LOCAL p AS PTR PTR
	IF (hMenu != NULL_PTR)
		iLen := ALen(__WCMenuList)
		FOR dwIndex := 1 TO iLen
			IF (__WCMenuList[dwIndex][2] == hMenu)
				EXIT
			ENDIF
		NEXT
		//dwIndex := AScan(__WCMenuList, {|x| x[2] == hMenu})
		IF (dwIndex != (iLen+1))
#ifdef __VULCAN__
			p := (PTR) __WCMenuList[dwIndex][1]
			oMenu := GCHandle.FromIntPtr( PTR( p ) ):Target
#else			
			p := __WCMenuList[dwIndex][1]
			oMenu := OBJECT(_CAST, PTR(p))
#endif			
		ENDIF
	ENDIF

	RETURN oMenu

FUNCTION GetObjectByHandle(hwnd AS PTR) AS OBJECT STRICT
   RETURN __WCGetObjectByHandle(hwnd)

FUNCTION __WCGetOrigin(oObject AS OBJECT) AS Point STRICT
	LOCAL hWnd AS PTR
	LOCAL oParent AS OBJECT
	LOCAL rect IS _WinRect
	LOCAL point IS _WinPoint
	LOCAL oControl AS Control
	GetWindowRect(oObject:Handle(), @rect)
	point:x := rect:left
	IF __WCCoordinateSystem // Cartesian Coordinate System
		point:y := rect:bottom
	ELSE // Windows Coordinate System
		point:y := rect:top
	ENDIF

	IF IsInstanceOf(oObject,#Control)
		oControl := oObject
		oParent := oControl:__FormSurface
	ELSE
		oParent := oObject:__Parent
	ENDIF

	IF oParent != NULL_OBJECT .AND. IsInstanceOf(oParent,#Window)
		hWnd := oParent:Handle(4)
		ScreenToClient(hWnd, @point)
		//		hDC := GetDC(hWnd)
		//		if hDC != 0
		//			DPtoLP(hDC, @point,1)
		//			ReleaseDC(hWnd, hDC)
		//		endif
	ENDIF

	RETURN __WCConvertPoint(oParent, Point{point:x, point:y})

FUNCTION __WCGetPictureCoordinates(hWnd AS PTR, hDCPrinter AS PTR, lpRC AS _winRect) AS VOID STRICT
	//LOCAL pt IS _winPoint
	LOCAL hDCTemp:= GetDC(hWnd) AS PTR
	LOCAL ratioX := 0 AS INT
	LOCAL ratioY := 0 AS INT  
	LOCAL nShrinkX, nShrinkY AS FLOAT
	LOCAL nWidth, nHeight AS INT  
   LOCAL nX, nY AS INT
	GetWindowRect(hWnd, lpRc)
	OffsetRect(lpRc, - lpRc:left, - lpRc:top)

	IF (hDCTemp != NULL_PTR)
		ratioX := GetDeviceCaps(hDCTemp, LOGPIXELSX)
		ratioY := GetDeviceCaps(hDCTemp, LOGPIXELSY)
		ReleaseDC(hWnd, hDCTemp)
	ENDIF

	IF ((ratioX == 0) .OR. (ratioY == 0))
		RETURN
	ENDIF

	lpRc:right := MulDiv(lpRc:right, GetDeviceCaps(hDCPrinter, LOGPIXELSX), ratioX)
	lpRc:bottom := MulDiv(lpRc:bottom, GetDeviceCaps(hDCPrinter, LOGPIXELSY), ratioY)

   nWidth 	:= GetDeviceCaps(hDCPrinter, PHYSICALWIDTH)
   nHeight 	:= GetDeviceCaps(hDCPrinter, PHYSICALHEIGHT)
   nX 		:= GetDeviceCaps(hDCPrinter, PHYSICALOFFSETX)
   nY 		:= GetDeviceCaps(hDCPrinter, PHYSICALOFFSETY)
   
   IF (lpRC:right > nWidth-nX .OR. lpRC:bottom> nHeight-nY)
   	// e.g. Image = 800 - 600, paper = 200 - 300
   	// shrinkX = 200/800 = 0.25
   	// shrinkY = 300/600 = 0.50
   	// we must shrink with 0.25 to fit.
   	nShrinkX := FLOAT(nWidth-nX) / FLOAT(lpRC:Right)  
   	nShrinkY := FLOAT(nHeight-nY) / FLOAT(lpRC:bottom)
   	 
   	IF nShrinkY < nShrinkX
			lpRC:right 	:= INT(FLOAT(lpRC:Right) * nShrinkY)
			lpRC:bottom := INT(FLOAT(lpRC:bottom) * nShrinkY)
   	ELSE
			lpRC:right 	:= INT(FLOAT(lpRC:Right) * nShrinkX)
			lpRC:bottom := INT(FLOAT(lpRC:bottom) * nShrinkX)
   	ENDIF   		     
		lpRC:left 	:= nX 
   	lpRC:top		:= nY
   ELSE
		OffsetRect(lpRC, (nWidth / 2) - (lpRC:right / 2), (nHeight / 2) - (lpRC:bottom / 2))
		OffsetRect(lpRC, - nX, - nY)
   ENDIF
   
	//IF (Escape(hDCPrinter, GETPHYSPAGESIZE, 0, NULL, @pt) >= 0)
	//	OffsetRect(lpRc, (pt.x / 2) - (lpRc.right / 2), (pt.y / 2) - (lpRc.bottom / 2))
	//ENDIF
	//IF (Escape(hDCPrinter, GETPRINTINGOFFSET, 0, NULL, @pt) >= 0)
	//	OffsetRect(lpRC, - pt.X, - pt.Y)
	//ENDIF
  	RETURN

FUNCTION __WCGetTopLeftPoint(oControl AS OBJECT) AS Point STRICT
	LOCAL oPoint AS Point
	IF __WCCoordinateSystem // Cartesian Coordinate System
		oPoint := oControl:Origin
		RETURN Point{oPoint:X, oPoint:Y + oControl:Size:Height - 1}
	ENDIF
	//else Windows Coordinate System
	RETURN oControl:Origin

FUNCTION __WCGetWindowByHandle(hWnd AS PTR) AS Window STRICT
	LOCAL strucSelfPtr AS SelfPtr
	IF IsWindow(hWnd)
		strucSelfPtr := PTR(_CAST, GetWindowLong(hWnd, DWL_USER))
		RETURN __WCSelfPtr2Object(strucSelfPtr)
	ENDIF

	RETURN NULL_OBJECT

FUNCTION __WCGetWindowMenuPosition(hMenu AS PTR) AS INT STRICT
	// Used in CAVOOLE to retrieve window menu position for in-place activation
	LOCAL o AS Menu
	o := __WCGetMenuByHandle(hMenu)
	IF (o != NULL_OBJECT)
		RETURN o:GetAutoUpdate()
	ENDIF
	RETURN -1

FUNCTION __WCGetWindowObjectByHandle(hWnd AS PTR) AS OBJECT STRICT
	// Wrapper for compatibility only
	RETURN __WCGetControlByHandle(hWnd)

PROCEDURE __WCInitCriticalSections() _INIT1
#ifdef __VULCAN__
   AppDomain.CurrentDomain:ProcessExit += System.EventHandler{ NULL, @GUIExit() }
#else	
	InitializeCriticalSection(@__WCCSMenu)
	InitializeCriticalSection(@__WCCSTimer)
	InitializeCriticalSection(@__WCCSHDC)
	InitializeCriticalSection(@__WCCSApp)
	_RegisterWEP(@GUIExit(), _GetInst())
#endif	
  	RETURN

FUNCTION __WCIsTrueTypeEnabled() AS LOGIC
	RETURN GetProfileInt(PSZ(_CAST, "TrueType"), PSZ(_CAST, "TTEnable"), 1) == 1

FUNCTION __WCLogicalBackgroundBrush(oWindow AS Window, strucLogBrush AS _WinLogBrush) AS PTR STRICT
	LOCAL hBr AS PTR
	LOCAL oBrush AS Brush

	oBrush:=oWindow:Background
	IF oBrush != NULL_OBJECT
		hBr := oBrush:Handle()
	ELSE
		hBr := GetClassLong(oWindow:Handle(),GCL_HBRBACKGROUND)
	ENDIF

	/*
	 if (hBr == NULL_PTR)
	 if IsinstanceOf(oWindow, #DialogWindow)
	 strucLogBrush.lbColor := GetSysColor(COLOR_WINDOWFRAME)
	 else
	 strucLogBrush.lbColor := GetSysColor(COLOR_WINDOW)
	 endif

	 strucLogBrush.lbHatch := 0
	 strucLogBrush.lbStyle := BS_SOLID
	 else
	 if DWORD(_cast, hBr) <= COLOR_ENDCOLORS+1
	 strucLogBrush.lbColor := GetSysColor(LONG(_cast, hBr)-2)
	 strucLogBrush.lbHatch := 0
	 strucLogBrush.lbStyle := BS_SOLID
	 else
	 GetObject( hBr , _SizeOf(_WinLogBrush), strucLogBrush)
	 endif
	 endif
	*/

	IF (hBr == NULL_PTR)
		IF IsInstanceOf(oWindow, #DialogWindow)
			hBr := GetSysColorBrush(COLOR_3DFACE)
		ELSE
			hBr := GetSysColorBrush(COLOR_WINDOW)
		ENDIF
	ENDIF

	GetObject(hBr, _SIZEOF(_WinLogBrush), strucLogBrush)

	RETURN hBr


FUNCTION __WCLogicalBrush(oBrush AS Brush, strucLogBrush AS _WinLogBrush) AS VOID STRICT
	IF oBrush==NULL_OBJECT
		strucLogBrush:lbColor := Color{ColorBlack}:ColorRef
		strucLogBrush:lbHatch := 0
		strucLogBrush:lbStyle := BS_SOLID
	ELSE
		GetObject( oBrush:Handle(), _SIZEOF(_WinLogBrush), strucLogBrush)
	ENDIF
	RETURN

FUNCTION __WCLogicalPen(oPen AS pen, strucLogPen AS _WinLogPen) AS VOID STRICT
	IF oPen == NULL_OBJECT
		strucLogPen:lopnStyle := PS_Solid
		strucLogPen:lopnColor := Color{ColorBlack}:ColorRef
		strucLogPen:lopnWidth:X := 1
	ELSE
		GetObject( oPen:Handle(), _SIZEOF(_WinLogPen), strucLogPen)
	ENDIF

	RETURN

STATIC GLOBAL __WCMenuList := {} AS ARRAY
FUNCTION __WCPaletteSize(pIH AS PTR) AS DWORD STRICT
	LOCAL pbi AS _winBITMAPINFOHEADER
	LOCAL iNumCol AS DWORD

	pbi := pIH

	DO CASE
	CASE (pbi:biBitCount == 1)
		iNumCol := 2
	CASE (pbi:biBitCount == 4)
		iNumCol := 16
	CASE (pbi:biBitCount == 8)
		iNumCol := 256
	OTHERWISE
		iNumCol := 0
	ENDCASE

	RETURN  iNumCol * _SIZEOF(_winRGBQUAD)

FUNCTION __WCRegisterControl(oControl AS Control) AS VOID
	LOCAL strucSelf AS SelfPtr
	strucSelf := __WCSelfPtrAlloc(oControl)
	__WCRegisterProperty(oControl:Handle(), strucSelf)
	RETURN 

FUNCTION __WCRegisterMenu(oObject AS Menu, hMenu AS PTR) AS VOID
	LOCAL idx, iLen AS DWORD
	LOCAL p AS PTR PTR// as SelfPtr
	LOCAL lFound	AS LOGIC
	
#ifdef __VULCAN__

   BEGIN LOCK __WCMenuList
   
	   iLen := ALen(__WCMenuList)
	   lFound := FALSE
	   FOR idx := 1 TO iLen
		   IF (__WCMenuList[idx][2] == hMenu)
			   lFound := TRUE
			   EXIT
		   ENDIF
	   NEXT
	   IF ! lFound
		   p := MemAlloc( sizeof( IntPtr ) )
         PTR(p) := GCHandle.ToIntPtr( GCHandle.Alloc( oObject ) )
		   AAdd(__WCMenuList, {p, hMenu}) // menu object, menu handle, ref count
	   ENDIF

   END LOCK	

#else

	EnterCriticalSection(@__WCCSMenu)

	//idx := AScan(__WCMenuList, {|x| x[2] == hMenu})
	iLen := ALen(__WCMenuList)
	lFound := FALSE
	FOR idx := 1 TO iLen
		IF (__WCMenuList[idx][2] == hMenu)
			lFound := TRUE
			EXIT
		ENDIF
	NEXT
	IF ! lFound
		p := MemAlloc(4)
		// RvdH 050823 Problem solved in runtime.:
		// So mark this as 'Special' KID: Only adjust KID PTR, don't 'salvage' the object
		// RegisterKid(p, 1, FALSE)
		RegisterKid(p, 0x20000001, FALSE)
		PTR(p) := PTR(_CAST, oObject)
		//RvdH 050825 Removed RefCount element from array
		//AAdd(__WCMenuList, {p, hMenu, 1}) // menu object, menu handle, ref count
		AAdd(__WCMenuList, {p, hMenu}) // menu object, menu handle, ref count
	//ELSE
	//	__WCMenuList[idx][3] := __WCMenuList[idx][3] + 1
	ENDIF

	LeaveCriticalSection(@__WCCSMenu)
	
#endif	

	RETURN

FUNCTION __WCRegisterTimer(oNew AS OBJECT) AS VOID STRICT
	LOCAL idx, iLen AS DWORD
	
#ifdef __VULCAN__

   STATIC LOCAL WCTimerProcDelegate AS __WCTimerProcDelegate
   STATIC LOCAL TimerProcPtr AS IntPtr
   
   IF WCTimerProcDelegate == NULL
      WCTimerProcDelegate := __WCTimerProcDelegate{ NULL, @__WCTimerProc() }
      TimerProcPtr        := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCTimerProcDelegate )
   ENDIF
   
   BEGIN LOCK __WCTimerObjects
   
	   iLen := ALen(__WCTimerObjects)
	   FOR idx := 1 TO iLen
		   IF (__WCTimerObjects[idx] == oNew)
			   EXIT
		   ENDIF
		   IF IsNil(__WCTimerObjects[idx])
			   __WCTimerObjects[idx] := oNew
			   EXIT
		   ENDIF
	   NEXT

	   IF (idx == (iLen + 1)) // no slot found
		   AAdd(__WCTimerObjects, oNew)

		   IF (ALen(__WCTimerObjects) == 1)
			   __WCTimerObjectID := SetTimer(NULL_PTR, 0, 956, TimerProcPtr )
		   ENDIF
	   ENDIF

   END LOCK
   
#else   
   
	EnterCriticalSection(@__WCCSTimer)

	iLen := ALen(__WCTimerObjects)
	FOR idx := 1 TO iLen
		IF (__WCTimerObjects[idx] == oNew)
			EXIT
		ENDIF
		IF IsNil(__WCTimerObjects[idx])
			__WCTimerObjects[idx] := oNew
			EXIT
		ENDIF
	NEXT

	IF (idx == (iLen + 1)) // no slot found
		AAdd(__WCTimerObjects, oNew)

		IF (ALen(__WCTimerObjects) == 1)
			__WCTimerObjectID := SetTimer(NULL_PTR, 0, 956, @__WCTimerProc())
		ENDIF
	ENDIF

	LeaveCriticalSection(@__WCCSTimer)
	
#endif	

	RETURN

FUNCTION __WCSetROP(hDC AS PTR, rop AS DWORD) AS PTR STRICT
	LOCAL hLastRop AS PTR


	hLastRop := GetROP2(hDC)

	DO CASE
	CASE (rop == ROPInvert)
		SetROP2(hDC, R2_NOT)
	CASE (rop == ROPXOR)
		SetROP2(hDC, R2_XORPEN)
	OTHERWISE
		SetROP2(hDC, R2_COPYPEN)
	ENDCASE

	RETURN hLastRop

STATIC GLOBAL __WCTimerObjectID AS DWORD
/*STATIC*/ GLOBAL __WCTimerObjects := {} AS ARRAY

#ifdef __VULCAN__
   DELEGATE __WCTimerProcDelegate( hWnd AS PTR, uMsg AS DWORD, idEvent AS DWORD, dwTime AS DWORD ) AS VOID
#endif

FUNCTION __WCTimerProc(hWnd AS PTR, uMsg AS DWORD, idEvent AS DWORD, dwTime AS DWORD) AS VOID /* WINCALL */
	LOCAL i, iLen AS INT
	LOCAL oObject AS OBJECT

	IF (ErrorLevel() > ES_WHOCARES)
		RETURN
	ENDIF

#ifdef __VULCAN__

   BEGIN LOCK __WCTimerObjects
	   iLen := INT(_CAST, ALen(__WCTimerObjects))
	   FOR i:= 1 TO iLen
		   IF IsObject(__WCTimerObjects[i]) 
			   oObject := __WCTimerObjects[i] 
			   IF IsMethod(oObject, #__Timer)
				   oObject:__Timer()
			   ENDIF
		   ENDIF
	   NEXT
   END LOCK
   
#else   

	EnterCriticalSection(@__WCCSTimer)

	iLen := INT(_CAST, ALen(__WCTimerObjects))
	FOR i:= 1 TO iLen
		IF IsObject(__WCTimerObjects[i]) 
			oObject := __WCTimerObjects[i] 
			IF IsMethod(oObject, #__Timer)
				oObject:__Timer()
			ENDIF
		ENDIF
	NEXT

	LeaveCriticalSection(@__WCCSTimer)
	
#endif	

	RETURN

FUNCTION __WCUnregisterClasses()
	LOCAL hInst AS PTR

	hInst := _GetInst()

	UnregisterClass(PSZ(_CAST, __WCCustomControlClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCDocAppWindowClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCWndAppWindowClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCMMContWindowClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCGBNotifyWindowClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCShellWindowClass), hInst)
	UnregisterClass(PSZ(_CAST, __WCTopAppWindowClass), hInst)
  	RETURN NIL


FUNCTION __WCUnRegisterControl(hwndCtl AS PTR) AS VOID
   __WCUnRegisterProperty(hwndCtl) // This also frees the memory of the SelfPtr structure
	RETURN

FUNCTION __WCUnregisterTimer(oDel AS OBJECT) AS VOID STRICT
	LOCAL idx, iLen AS DWORD
	
#ifdef __VULCAN__

   BEGIN LOCK __WCTimerObjects
	   iLen := ALen(__WCTimerObjects)
	   FOR idx := 1 TO iLen
		   IF (__WCTimerObjects[idx] == oDel)
			   EXIT
		   ENDIF
	   NEXT

	   IF (idx != (iLen+1))
		   __WCTimerObjects[idx] := NIL
	   ENDIF
   END LOCK
   
#else   	

	EnterCriticalSection(@__WCCSTimer)

	iLen := ALen(__WCTimerObjects)
	FOR idx := 1 TO iLen
		IF (__WCTimerObjects[idx] == oDel)
			EXIT
		ENDIF
	NEXT


	IF (idx != (iLen+1))
		//ADel(__WCTimerObjects, idx)
		__WCTimerObjects[idx] := NIL
		//if (ALen(__WCTimerObjects) == 0)
		// KillTimer(NULL_PTR, __WCTimerObjectID)
		//endif
	ENDIF

	LeaveCriticalSection(@__WCCSTimer)
	
#endif	

	RETURN

FUNCTION __WCUseTrueTypeOnly() AS LOGIC

	RETURN GetProfileInt(PSZ(_CAST, "TrueType"), PSZ(_CAST, "TTOnly"), 0) == 1

STATIC GLOBAL Ctl3DEnabledFlag AS LOGIC

FUNCTION Enable3dControls() AS LOGIC
	LOCAL lCTL3DEnabledFlag AS LOGIC

	lCTL3DEnabledFlag:=CTL3DEnabledFlag //Old value
	//	Ctl3dRegister( _GetInst())
	//	Ctl3dAutoSubclass( _GetInst())
	Ctl3DEnabledFlag:=TRUE

	RETURN lCTL3DEnabledFlag

FUNCTION GetAppObject() AS App STRICT
	LOCAL oRet AS App

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
   oRet := oApp
   END LOCK
#else   
	EnterCriticalSection(@__WCCSApp)
	oRet := oApp
	LeaveCriticalSection(@__WCCSApp)
#endif

	RETURN oRet

FUNCTION GetFocusedObject() AS OBJECT STRICT

	RETURN __WCGetObjectByHandle(GetFocus())

FUNCTION __WCGetObjectByHandle(hWnd AS PTR) AS OBJECT STRICT
	LOCAL w  AS Window
   LOCAL c  AS Control
	w := __WCGetWindowByHandle(hWnd)
	IF w != NULL_OBJECT
		RETURN w
   ENDIF
	c := __WCGetControlByHandle(hWnd)
	IF (c != NULL_OBJECT)
	   w := c:__ControlWindow
	   IF (w != NULL_OBJECT)
	      RETURN w
	   ELSE
	      RETURN c
	   ENDIF
	ENDIF
	// When not a control and not a window, then maybe a menu ?
   RETURN __WCGetMenuByHandle(hWnd)
	
#ifdef __VULCAN__
   // This is registered as an ProcessExit event handler, so it needs
   // a different signature in Vulcan than it does in VO, which uses
   // _RegisterWEP() (which isn't supported in Vulcan).
   FUNCTION GUIExit( o AS OBJECT, args AS EventArgs ) AS VOID
	   WCDCClear()
	   __WCDeleteCriticalSections()
	   __WCUnregisterClasses()
      RETURN
#else
FUNCTION GUIExit() AS VOID STRICT
	__WCDeleteCriticalSections()
	__WCUnregisterClasses()
	RETURN
#endif

FUNCTION IsCtl3dEnabled() AS LOGIC
	RETURN Ctl3dEnabledFlag

FUNCTION WCAppGetDialogWindow() AS PTR STRICT
	LOCAL pRet AS PTR

	IF (oApp != NULL_OBJECT)
		pRet := oApp:GetDialogWindow()
	ENDIF

	RETURN pRet

FUNCTION WCAppSetDialogWindow(hDlg AS PTR) AS VOID STRICT

	IF (oApp != NULL_OBJECT)
		oApp:SetDialogWindow(hDlg)
	ENDIF
	RETURN


FUNCTION WCDCAdd(oObject AS OBJECT) AS VOID STRICT
#ifdef __VULCAN__
   BEGIN LOCK __WCCSHDC
	   IF (__WCDCCurHDCOwner != NULL_OBJECT)
		   __WCDCCurHDCOwner:__ReleaseDC()
	   ENDIF
	   __WCDCCurHDCOwner := oObject
   END LOCK
#else
	EnterCriticalSection(@__WCCSHDC)

	IF (__WCDCCurHDCOwner != NULL_OBJECT)
		__WCDCCurHDCOwner:__ReleaseDC()
	ENDIF
	__WCDCCurHDCOwner := oObject

	LeaveCriticalSection(@__WCCSHDC)
#endif
	RETURN


FUNCTION WCDCClear() AS VOID STRICT

#ifdef __VULCAN__
   BEGIN LOCK __WCCSHDC
	   IF (__WCDCCurHDCOwner != NULL_OBJECT)
		   __WCDCCurHDCOwner:__ReleaseDC()
	   ENDIF
   END LOCK
#else   
	EnterCriticalSection(@__WCCSHDC)

	IF (__WCDCCurHDCOwner != NULL_OBJECT)
		__WCDCCurHDCOwner:__ReleaseDC()
	ENDIF

	LeaveCriticalSection(@__WCCSHDC)
#endif
	RETURN

FUNCTION WCDCDelete(oObject AS OBJECT) AS VOID STRICT

#ifdef __VULCAN__
   BEGIN LOCK __WCCSHDC
	   IF (oObject == __WCDCCurHDCOwner)
		   __WCDCCurHDCOwner := NULL_OBJECT
	   ENDIF
   END LOCK
#else
	EnterCriticalSection(@__WCCSHDC)

	IF (oObject == __WCDCCurHDCOwner)
		__WCDCCurHDCOwner := NULL_OBJECT
	ENDIF

	LeaveCriticalSection(@__WCCSHDC)
#endif
	RETURN


FUNCTION WCDCTop(oObject AS OBJECT) AS VOID STRICT

	RETURN

FUNCTION WCGetCoordinateSystem() AS LOGIC

	RETURN __WCCoordinateSystem

FUNCTION WCNewControlsAvailable() AS LOGIC
	RETURN (gpfnInitCommonControlsEx != NULL_PTR)

FUNCTION WCSetCoordinateSystem(system AS LOGIC) AS LOGIC
	RETURN __WCCoordinateSystem := system

FUNCTION __WCStretchDibBlt(hdc AS PTR, x AS INT, y AS INT, dx AS INT, dy AS INT, hdib AS PTR) AS LOGIC STRICT
	LOCAL pBuf AS PSZ
	LOCAL lRet AS LONGINT
	LOCAL lpbi AS _winBITMAPINFO

	lpbi := GlobalLock(hdib)

	IF (lpbi == NULL_PTR)
		RETURN FALSE
	ENDIF

	pBuf := PSZ(_CAST , DWORD(_CAST, lpbi) + lpbi:bmiHeader:biSize + __WCPaletteSize(lpbi))
	lRet := StretchDIBits(hdc, x, y, dx, dy, 0, 0, lpbi:bmiHeader:biWidth, lpbi:bmiHeader:biHeight, pBuf, lpbi, DIB_RGB_COLORS, SRCCOPY)
	GlobalUnlock(hdib)

	RETURN (lRet > 0)


FUNCTION __WCUnregisterMenu(oObject AS Menu) AS VOID
// 	LOCAL idx := 1, iDel:= 0 , iLen, i, iEnd, iStart AS INT
// 	LOCAL p AS PTR PTR
//
//
// 	IF (oObject == NULL_OBJECT)
// 		RETURN
// 	ENDIF
//
// 	EnterCriticalSection(@__WCCSMenu)
//
// 	iLen := INT(_CAST, ALen(__WCMenuList))
//
// 	WHILE (TRUE)
// 		//idx := AScan(__WCMenuList, {|x| pSelf := x[1], !IsNil(x) .and. (pSelf.ptrSelf == p)}, idx, iLen - idx - iDel + 1)
// 		iStart := idx
// 		iEnd := iLen - iDel
// 		idx := 0
// 		FOR i:= iStart TO iEnd
// 			p := __WCMenuList[i][1]
// 			IF (PTR(p) == PTR(_CAST, oObject)) .or. (PTR(p) == NULL_PTR)
// 				idx := i
// 				EXIT
// 			ENDIF
// 		NEXT
//
// 		IF (idx == 0)
// 			EXIT
// 		ENDIF
// 			//RvdH 050825 Removed RefCount element from array
// 		//__WCMenuList[idx][3] := __WCMenuList[idx][3] - 1
// 		//IF (__WCMenuList[idx][3] < 1) .or. (PTR(p) == NULL_PTR)
// 		IF (PTR(p) == NULL_PTR)
// 			IF (p != NULL_PTR)
// 				UnRegisterKid(p)
// 				MemFree(p)
// 			ENDIF
// 			ADel(__WCMenuList, idx)
// 			iDel++
// 		ENDIF
// 	END
//
// 	IF (iDel > 0)
// 		ASize(__WCMenuList, INT(_CAST, ALen(__WCMenuList)) - iDel)
// 	ENDIF
//
// 	LeaveCriticalSection(@__WCCSMenu)
	//RvdH 050825 Optimized: removed reference count.
	LOCAL idx := 1, iDel:= 0 , iLen  AS DWORD
	LOCAL p AS PTR PTR


	IF (oObject == NULL_OBJECT)
		RETURN
	ENDIF
	
#ifdef __VULCAN__

   BEGIN LOCK __WCMenuList
   
	   iLen 		:= INT(_CAST, ALen(__WCMenuList))
	   FOR idx := iLen DOWNTO 1
		   p := __WCMenuList[idx][1]

		   IF PTR( p ) == NULL_PTR
			   ADel(__WCMenuList, idx)
			   iDel++
			   MemFree( p )
		   ELSEIF GCHandle.FromIntPtr( PTR( p ) ):Target == oObject
			   GCHandle.FromIntPtr( PTR( p ) ):Free()
			   ADel(__WCMenuList, idx)
			   iDel++
			   MemFree( p )
		   ENDIF
	   NEXT
	   IF (iDel > 0)
		   ASize(__WCMenuList, INT(_CAST, ALen(__WCMenuList)) - iDel)
	   ENDIF

   END LOCK	
   
#else   

	EnterCriticalSection(@__WCCSMenu)

	iLen 		:= ALen(__WCMenuList)
	FOR idx := iLen DOWNTO 1 
		p := __WCMenuList[idx][1]
		IF (PTR(p) == PTR(_CAST, oObject)) .OR. (PTR(p) == NULL_PTR)
			UnRegisterKid(p)
			MemFree(p)
			ADel(__WCMenuList, idx)
			iDel += 1
		ENDIF
	NEXT
	IF (iDel > 0)
		ASize(__WCMenuList, ALen(__WCMenuList) - iDel)
	ENDIF
	LeaveCriticalSection(@__WCCSMenu)
	
#endif	

	RETURN
FUNCTION SetClassStyle(hWnd AS PTR, dwSetStyle AS DWORD, lEnable := TRUE AS LOGIC) AS DWORD STRICT
	//PP-031129
	LOCAL dwOldStyle AS DWORD

	IF (hWnd != NULL_PTR)
		dwOldStyle := GetClassLong(hWnd, GCL_STYLE)

		IF lEnable
			dwSetStyle := _OR(dwOldStyle, dwSetStyle)
		ELSE
			dwSetStyle := _AND(dwOldStyle, _NOT(dwSetStyle))
		ENDIF

		IF dwOldStyle != dwSetStyle
			SetClassLong(hWnd, GCL_STYLE, LONGINT(_CAST, dwSetStyle))
		ENDIF
	ENDIF

	RETURN dwSetStyle

FUNCTION WCMoveWindow(oObject AS OBJECT, oPoint AS Point, oDimension AS Dimension, bRepaint AS LOGIC) AS VOID
	LOCAL strucPoint IS _winPOINT
	LOCAL oParent AS OBJECT
   LOCAL lX, lY AS LONGINT

	IF IsInstanceOf(oObject,#Control)
		oParent := oObject:__FormSurface
	ELSE
		oParent := oObject:__Parent
	ENDIF

	oPoint := __WCConvertPoint(oParent, oPoint)

	lX := oPoint:X
	lY := oPoint:Y
	IF __WCCoordinateSystem // Cartesian Coordinate System
		lY := lY - oDimension:Height
	ENDIF

	IF IsInstanceOf(oParent,#Window) .AND. _AND(WS_CHILD,(GetWindowLong( oObject:Handle(), GWL_STYLE)))==0
		strucPoint:X := lX
		strucPoint:Y := lY
		ClientToScreen(oParent:Handle(4), @strucPoint)
		lX := strucPoint:X
		lY := strucPoint:Y
	ENDIF

	SetWindowPos(oObject:Handle(), NULL_PTR, lX, lY, oDimension:Width, oDimension:Height, _OR(SWP_NOZORDER, SWP_NOACTIVATE))
	RETURN


// Three new functions that manage SelfPtr objects

FUNCTION __WCSelfPtrAlloc(oObject AS OBJECT) AS  SelfPtr
	LOCAL strucSelfPtr AS SelfPtr

#ifdef __VULCAN__
	strucSelfPtr := MemAlloc(_SizeOf(SelfPtr))
	strucSelfPtr:ptrSelf := GCHandle.ToIntPtr( GCHandle.Alloc( oObject ) )
#else
	RegisterAxit(SELF)
	strucSelfPtr := MemAlloc(_SizeOf(SelfPtr))
	RegisterKid(@strucSelfPtr:ptrSelf, 1, FALSE)
	strucSelfPtr:ptrSelf := PTR(_CAST, oObject)
#endif	
   RETURN strucSelfPtr
   
FUNCTION __WCSelfPtrFree(ptrSelfPtr AS SelfPtr) AS LOGIC
   LOCAL lOk := FALSE AS LOGIC
	IF (ptrSelfPtr != NULL_PTR)
#ifdef __VULCAN__
		GCHandle.FromIntPtr( ptrSelfPtr:ptrSelf ):Free()
		ptrSelfPtr:ptrSelf := NULL
#else
		UnRegisterKid(ptrSelfPtr)
#endif
		MemFree(ptrSelfPtr)
		lOk := TRUE
	ENDIF
   RETURN lOk

FUNCTION __WCSelfPtr2Object( p AS SelfPtr) AS OBJECT
   LOCAL oObject AS OBJECT
   IF p != NULL_PTR .and. MemCheckPtr(PTR(_CAST, p), _SIZEOF(SelfPtr))
      #ifdef __VULCAN__
         IF (p:ptrSelf != 0)
		     oObject := GCHandle.FromIntPtr( p:ptrSelf ):Target
		   ENDIF
      #else
		      oObject := OBJECT(_CAST, p:ptrSelf)
      #endif
      RETURN oObject  
   ENDIF   
   RETURN NULL_OBJECT
   
   
// Three new functions that manage the properties 
FUNCTION __WCRegisterProperty(hWnd AS PTR, hSelf AS SelfPtr) AS LOGIC
   IF (hWnd != NULL_PTR)
	   RETURN SetProp(hWnd, PSZ(_CAST, gatomVOObjPtr), hSelf)
	ENDIF
	RETURN FALSE

FUNCTION __WCUnRegisterProperty(hWnd AS PTR) AS LOGIC
	LOCAL strucSelf AS SelfPtr
   IF (hWnd != NULL_PTR)
	   strucSelf := GetProp(hWnd, PSZ(_CAST, gatomVOObjPtr))
	   IF (strucSelf != NULL_PTR)
	      __WCSelfPtrFree(strucSelf)
		   RETURN RemoveProp(hwnd, PSZ(_CAST, gatomVOObjPtr)) != NULL_PTR
	   ENDIF
   ENDIF
	RETURN FALSE

FUNCTION __WCGetProperty(hwnd AS PTR) AS SelfPtr
	IF (hWnd != NULL_PTR)
	   RETURN GetProp(hwnd, PSZ(_CAST, gatomVOObjPtr))
	ENDIF
   RETURN NULL_PTR



#region defines
DEFINE WCCartesianCoordinates := TRUE
DEFINE WCWindowsCoordinates := FALSE
#endregion
