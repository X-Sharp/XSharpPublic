CLASS Icon INHERIT VObject
	PROTECT hIcon AS PTR

METHOD Destroy() 
	

	IF ! hIcon == NULL_PTR
		DestroyIcon(hIcon)
		hIcon := NULL_PTR
	ENDIF

	SUPER:Destroy()

	RETURN NIL

METHOD Handle() AS PTR
	
	RETURN hIcon

CONSTRUCTOR(xResourceID, kLoadOption, iWidth, iHeight) 
	LOCAL hInst AS PTR
	LOCAL lpszIcon AS PTR

	

	SUPER()

	IF IsNumeric(xResourceID)
		// This is loading a standard icon
		hIcon := LoadIcon(0, __WCConvertIcon(xResourceID))
	ELSEIF IsPtr(xResourceID)
		hIcon := xResourceID
	ELSE
		//PP-040425 Issue 12869 moved these initialisations from preceding
		DEFAULT(@xResourceID, ICONSTANDARD)
		DEFAULT(@kLoadOption, LR_DEFAULTCOLOR)
		IF ! IsLong(iWidth)
			iWidth := 0
		ENDIF
		IF ! IsLong(iHeight)
			iHeight := 0
		ENDIF

		IF IsSymbol(xResourceID) .OR. IsString(xResourceID)
			xResourceID := ResourceID{xResourceID}
		ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
			WCError{#Init, #Icon, __WCSTypeError, xResourceID, 1}:Throw()
		ENDIF

		hInst := xResourceID:Handle()
		lpszIcon := xResourceID:Address()

		//PP-031115
		// hIcon := LoadIcon(hInst, lpszIcon)
		hIcon := LoadImage(hInst, lpszIcon, IMAGE_ICON, iWidth, iHeight, kLOadOption)

		//PP-20040502
		//RegisterAxit(SELF) //SE-060525
	ENDIF

   //SE-060525
   IF hIcon != NULL_PTR
   	RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
   ENDIF

	RETURN 

ACCESS Size 
	//PP-031002
	//PP-040425 Issue 12869, need to DeleteObjects

	LOCAL bm IS _WINBITMAP
	LOCAL sIconInfo IS _winIconInfo
	LOCAL oDim AS Dimension

	GetIconInfo(hIcon, @sIconInfo)
	GetObject(sIconInfo:hbmColor, _SizeOf(_WINBITMAP), @bm)
	oDim := Dimension{bm:bmWidth, bm:bmHeight}
	DeleteObject(sIconInfo:hbmMask)
	DeleteObject(sIconInfo:hbmColor)

	RETURN oDim

END CLASS

FUNCTION __WCConvertIcon(iconType AS INT) AS PTR STRICT
	LOCAL retVal AS PTR

	SWITCH iconType
	CASE IconAsterisk
		retVal := IDI_ASTERISK
	CASE IconExclamation
		retVal := IDI_EXCLAMATION
	CASE IconHand
		retVal := IDI_HAND
	CASE IconQuestionMark
		retVal := IDI_QUESTION
	OTHERWISE
		retVal := IDI_APPLICATION
	END SWITCH

	RETURN retVal

