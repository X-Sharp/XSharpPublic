PARTIAL CLASS AnimationControl INHERIT Control
	PROTECT oAVIFileSpec AS FileSpec
	PROTECT resID AS ResourceID
	PROTECT hInstance AS PTR

METHOD Create() 
	LOCAL oDevPoint AS Point


	IF hWnd == NULL_PTR
		IF WCGetCoordinateSystem() == WCCartesianCoordinates
			oOrigin:Y := oOrigin:Y + SELF:Size:Height
		ENDIF
		oDevPoint := __WCConvertPoint(oFormSurface, Point{oOrigin:x,oOrigin:y})
		hWnd := Animate_Create(oFormSurface:Handle(), wID, dwStyle, iif(IsNil(hInstance), _GetInst(), hInstance))

		IF _And(dwStyle, ACS_CENTER) == 1
			// if the style is ACS_CENTER, manually set the window position
			SetWindowPos(hWnd, NULL_PTR, oDevPoint:X, oDevPoint:Y, oSize:Width, oSize:Height, 0)
		ENDIF

		__lpfnDefaultProc := GetWindowLong(hWnd, GWL_WNDPROC)
		SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr())) // dcaton 070319 use helper to get ptr

		oSize := NULL_OBJECT
		oOrigin := NULL_OBJECT
		__WCRegisterControl(SELF) //register after we get the handle

	ENDIF

	RETURN hwnd

ACCESS FileSpec() 

	RETURN oAVIFileSpec

ASSIGN FileSpec(oFileSpec) 

	IF IsString(oFileSpec)
		oAVIFileSpec := FileSpec{oFileSpec}
	ELSE
		oAVIFileSpec := oFileSpec
	ENDIF

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, oFileSpec, kStyle, hInst) 

	IF IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, FALSE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, ANIMATE_CLASS, kStyle, FALSE)
	ENDIF

	IF IsString(oFileSpec)
		oFileSpec := FileSpec{oFileSpec}
	ENDIF

	IF IsInstanceOfUsual(oFileSpec, #FileSpec)
		oAVIFileSpec := oFileSpec
	ENDIF

	IF IsPtr(hInst)
		hInstance := hInst
	ENDIF

	RETURN 

METHOD Open() 
	LOCAL pszFileName	AS PSZ
	LOCAL lReturnValue	AS LOGIC


	pszFileName := StringAlloc(oAVIFileSpec:FullPath)
	IF (PTR(_CAST, pszFileName) != NULL_PTR)
		lReturnValue := Animate_Open(SELF:Handle(), pszFileName)
		MemFree(pszFileName)
	ENDIF

	RETURN lReturnValue

METHOD OpenResource(xID) 
	LOCAL pszResID 		AS PSZ
	LOCAL lReturnValue	AS LOGIC

	IF IsInstanceOfUsual(xID, #ResourceID)
		pszResID	:= PSZ(_CAST, xID:ID)
	ELSE
		pszResID	:= PSZ(_CAST, xID)
	ENDIF

	lReturnValue := Animate_Open(SELF:Handle(), pszResID)

	RETURN lReturnValue


METHOD Play(nFrom, nTo, nRepeatCount) 
	LOCAL wFrom			AS WORD
	LOCAL wTo			AS WORD
	LOCAL dwRepeatCount	AS DWORD


	Default(@nFrom, 0)
	Default(@nTo, -1)
	Default(@nRepeatCount, -1)

	wFrom := WORD(_CAST, nFrom)
	wTo := WORD(_CAST, nTo)
	dwRepeatCount := DWORD(_CAST, nRepeatCount)

	RETURN Animate_Play(SELF:Handle(), wFrom, wTo, dwRepeatCount)

METHOD Seek(nFrame) 


	RETURN Animate_Seek(SELF:Handle(), nFrame)

METHOD Stop() 


	RETURN Animate_Stop(SELF:Handle())
END CLASS

