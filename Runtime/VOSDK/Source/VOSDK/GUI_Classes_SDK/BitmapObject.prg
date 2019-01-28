CLASS BitmapObject INHERIT ShapeObject
	PROTECT oBitmap AS Bitmap
	

DESTRUCTOR() 
	
	
	IF !InCollect()
		oBitmap := NULL_OBJECT
	ENDIF
	
	SUPER:Destroy()
	
	RETURN 
	

METHOD Draw() 
	LOCAL hBitmap AS PTR
	LOCAL hMemDC AS PTR
	LOCAL hOldBitmap AS PTR
	LOCAL dwRop AS DWORD
	LOCAL oWndBrush AS Brush
	LOCAL oBMSize AS Dimension
	LOCAL oOrg AS Point
	LOCAL oSize AS Dimension
	LOCAL bRes AS LOGIC
	LOCAL hTmpDC AS PTR
	
	
	hBitmap := oBitmap:Handle()
	
	IF (hBitmap == NULL_PTR)
		RETURN SELF
	ENDIF
	
	dwRop := SELF:RasterOperation
	IF (dwRop == ROPBackground)
		oWndBrush := oWnd:Foreground
		oWnd:Foreground := oWnd:Background
		oWnd:PaintBoundingBox(SELF:BoundingBox)
		oWnd:Foreground:=oWndBrush
		RETURN SELF
	ENDIF
	
	// Convert rop code to the correct raster operation code
	DO CASE
	CASE (dwRop == ROPINVERT)
		dwRop := 0x00990066 // See MS Development library for details
	CASE (dwRop == ROPXOR)
		dwRop := SRCINVERT
	OTHERWISE
		dwRop := SRCCOPY
	ENDCASE
	
	oBMSize := oBitmap:Size
	hMemDC := CreateCompatibleDC(SELF:Handle())
	
	IF (hMemDC != NULL_PTR)
		hOldBitmap := SelectObject(hMemDC, hBitmap)
		IF (hOldBitmap == NULL_PTR)
			DeleteDC(hMemDC)
			hTmpDC := GetDC(NULL_PTR)
			hMemDC := CreateCompatibleDC(hTmpDC)
			hOldBitmap := SelectObject(hMemDC, hBitmap)
		ENDIF
		
		oOrg := SELF:Origin
		oSize := SELF:Size
		
		bRes := StretchBlt(SELF:Handle(), oOrg:X, oOrg:Y + oSize:Height -1, oSize:Width, -(oSize:Height), hMemDC,;
			0,	0, oBMSize:Width,	oBMSize:Height,	dwRop)
		// Debug !!
		// bRes := StretchBlt(self:Handle(), 0, 0, 500000, 500000, hMemDC,;
			// 				 				 0,	0, oBMSize:Width,	oBMSize:Height,	dwRop)
		
		
		SelectObject(hMemDC, hOldBitmap)
		DeleteDC (hMemDC)
		
		IF (hTmpDC != NULL_PTR)
			ReleaseDC(NULL_PTR, hTmpDC)
		ENDIF
	ENDIF
	
	oWnd:__SetDCInitialized(FALSE)
	
	RETURN SELF
	

CONSTRUCTOR(oPoint, oDimension, oBitmap) 
	
	
	SUPER(oPoint,oDimension)
	IF !IsInstanceOfUsual(oBitMap,#BitMap)
		WCError{#Init,#BitmapObject,__WCSTypeError,oBitmap,3}:@@Throw()
	ENDIF
	
	SELF:oBitmap:=oBitmap
	//if oDimension:Width==0 .and. oDimension:Height==0
	//	super:Size := oBitmap:Size()
	//endif
	
	RETURN 
	
END CLASS

