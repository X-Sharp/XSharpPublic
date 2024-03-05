/// <include file="Gui.xml" path="doc/AnimationControl/*" />
CLASS AnimationControl INHERIT Control
	PROTECT oAVIFileSpec AS FileSpec
	PROTECT resID AS ResourceID
	PROTECT hInstance AS PTR


/// <include file="Gui.xml" path="doc/AnimationControl.Create/*" />
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


/// <include file="Gui.xml" path="doc/AnimationControl.FileSpec/*" />
ACCESS FileSpec()


	RETURN oAVIFileSpec


/// <include file="Gui.xml" path="doc/AnimationControl.FileSpec/*" />
ASSIGN FileSpec(oFileSpec)


	IF IsString(oFileSpec)
		oAVIFileSpec := FileSpec{oFileSpec}
	ELSE
		oAVIFileSpec := oFileSpec
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/AnimationControl.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, oFileSpec, kStyle, hInst)


	IF xID IS ResourceID
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, FALSE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, ANIMATE_CLASS, kStyle, FALSE)
	ENDIF


	IF IsString(oFileSpec)
		oFileSpec := FileSpec{oFileSpec}
	ENDIF


	IF oFileSpec IS FileSpec
		oAVIFileSpec := oFileSpec
	ENDIF


	IF IsPtr(hInst)
		hInstance := hInst
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/AnimationControl.Open/*" />
METHOD Open()
	LOCAL pszFileName	AS PSZ
	LOCAL lReturnValue	AS LOGIC




	pszFileName := StringAlloc(oAVIFileSpec:FullPath)
	IF (PTR(_CAST, pszFileName) != NULL_PTR)
		lReturnValue := Animate_Open(SELF:Handle(), pszFileName)
		MemFree(pszFileName)
	ENDIF


	RETURN lReturnValue


/// <include file="Gui.xml" path="doc/AnimationControl.OpenResource/*" />
METHOD OpenResource(xID)
	LOCAL pszResID 		AS PSZ
	LOCAL lReturnValue	AS LOGIC


	IF (xID IS ResourceID)
		pszResID	:= PSZ(_CAST, xID:ID)
	ELSE
		pszResID	:= PTR(_CAST, xID)
	ENDIF


	lReturnValue := Animate_Open(SELF:Handle(), pszResID)


	RETURN lReturnValue




/// <include file="Gui.xml" path="doc/AnimationControl.Play/*" />
METHOD Play(nFrom, nTo, nRepeatCount)
	LOCAL wFrom			AS WORD
	LOCAL wTo			AS WORD
	LOCAL dwRepeatCount	AS DWORD




	Default(@nFrom, 0)
	Default(@nTo, -1)
	Default(@nRepeatCount, -1)


	wFrom   := nFrom
	wTo     := nTo
	dwRepeatCount := DWORD(_CAST, nRepeatCount)


	RETURN Animate_Play(SELF:Handle(), wFrom, wTo, dwRepeatCount)


/// <include file="Gui.xml" path="doc/AnimationControl.Seek/*" />
METHOD Seek(nFrame)




	RETURN Animate_Seek(SELF:Handle(), nFrame)


/// <include file="Gui.xml" path="doc/AnimationControl.Stop/*" />
METHOD Stop()




	RETURN Animate_Stop(SELF:Handle())
END CLASS


