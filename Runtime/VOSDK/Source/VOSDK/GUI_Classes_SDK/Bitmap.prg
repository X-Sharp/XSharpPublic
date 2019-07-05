CLASS Bitmap INHERIT VObject
	PROTECT hBitmap AS PTR
	
	//PP-030828 Strong typing
	METHOD __SetHandle(hHandle AS PTR) AS PTR STRICT  
	//PP-030828 Strong typing
	
	//RvdH 20070204 Delete existting Bitmap
	IF (hBitmap != NULL_PTR)
		DeleteObject(hBitmap)
	ENDIF
	
	RETURN hBitmap := hHandle
	

METHOD Destroy() 
	
	IF (hBitmap != NULL_PTR)
		DeleteObject(hBitmap)
			hBitmap := NULL_PTR
	ENDIF
	SUPER:Destroy()
	
	RETURN NIL
	

METHOD Handle() AS PTR
	
	
	RETURN hBitmap
	

CONSTRUCTOR(xResourceID, kLoadOption, iWidth, iHeight) 
	LOCAL hInst AS PTR
	LOCAL lpszBitmap AS PTR
	//PP-031212 Added width and height option
	
	SUPER()
	
	IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
		xResourceID := ResourceID{xResourceID}
#ifdef __VULCAN__
      GC.SuppressFinalize( SELF )
#endif		

	ELSEIF IsPtr(xResourceID) //SE-070620
		hBitmap := xResourceID
#ifndef __VULCAN__		
		RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
#endif		
		RETURN 
		
	ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
		WCError{#Init, #Bitmap, __WCSTypeError, xResourceID, 1}:Throw()
#ifdef __VULCAN__
      GC.SuppressFinalize( SELF )
#endif		
	ENDIF

	Default(@kLoadOption, LR_DEFAULTCOLOR)
	IF ! IsLong(iWidth)
		iWidth := 0
	ENDIF
	IF ! IsLong(iHeight)
		iHeight := 0
	ENDIF
	
	hInst := xResourceID:Handle()
	lpszBitmap := xResourceID:Address()
	
	hBitmap := LoadImage(hInst, lpszBitmap, IMAGE_BITMAP, iWidth, iHeight, kLoadOption)
	
	
	
	RETURN 
	

ACCESS Size 
	LOCAL bm IS _WINBITMAP
	
	GetObject(hBitmap, _SIZEOF(_WINBITMAP), @bm)
	RETURN Dimension{bm:bmWidth, bm:bmHeight}
	

END CLASS

