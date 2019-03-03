CLASS Clipboard INHERIT VObject

METHOD Clear() 
	

	IF OpenClipboard(NULL_PTR)
		EmptyClipboard()
		CloseClipboard()
	ENDIF

	RETURN SELF

ACCESS FileCount 
	//PP-031115 S Ebert
	LOCAL hClipData  AS PTR
	LOCAL oDragEvent AS DragEvent

	IF OpenClipboard(NULL_PTR)

		hClipData := GetClipboardData(CF_HDROP)
		IF (hClipData != NULL_PTR)
			oDragEvent := DragEvent{Null_PTR, WM_DROPFILES, DWORD(_CAST,hClipData), 0, Null_Object}
			RETURN oDragEvent:FileCount
		ENDIF

		CloseClipboard()
	ENDIF

	RETURN 0

METHOD GetItemSize(kFormat) 
	LOCAL dwFormat AS DWORD
	LOCAL dwSize AS DWORD
	LOCAL strucBM IS _WinBitmap
	LOCAL hClipData AS PTR
	LOCAL hdc AS PTR
	LOCAL hMemoryDC AS PTR

	

	IF !IsLong(kFormat)
		WCError{#GetItemSize,#Clipboard,__WCSTypeError,kFormat,1}:@@Throw()
	ENDIF

	DO CASE
	CASE (kFormat == STRINGFORMAT)
		dwFormat := CF_TEXT
	CASE (kFormat == BITMAPFORMAT)
		dwFormat := CF_BITMAP
	OTHERWISE
		RETURN 0
	ENDCASE

	IF !IsClipboardFormatAvailable(dwFormat)
		RETURN 0
	ENDIF

	IF !OpenClipboard(NULL_PTR)
		RETURN 0
	ENDIF

	DO CASE
	CASE (dwFormat == CF_TEXT)
		hClipData := GetClipboardData(CF_TEXT)
		dwSize := PszLen(GlobalLock(hClipData))
		GlobalUnlock(hClipData)

	CASE (dwFormat == CF_BITMAP)
		hdc := GetDC(NULL_PTR)
		IF (hdc != NULL_PTR)
			hMemoryDC := CreateCompatibleDC(hdc)
			IF (hMemoryDC != NULL_PTR)
				hClipData := GetClipboardData (CF_BITMAP)
				SelectObject(hMemoryDC, hClipData)
				GetObject(hClipData, _SizeOf (_WinBitmap), @strucBM)
				dwSize := ((( DWORD(strucBM:bmWidth) * DWORD(strucBM:bmBitsPixel) + 15 ) >> 4 ) * 2 * DWORD(strucBM:bmHeight)) * DWORD(strucBM:bmPlanes)
				DeleteDC(hMemoryDC)
			ENDIF
			ReleaseDC(NULL_PTR, hdc)
		ENDIF
	ENDCASE

	CloseClipboard()

	RETURN dwSize

CONSTRUCTOR() 
    
    SUPER()


RETURN 

METHOD Insert(xType) 
	LOCAL hGlobalMemory AS PTR
	LOCAL hdc AS PTR
	LOCAL hSrcDC AS PTR
	LOCAL hDestDC AS PTR
	LOCAL hBitmap AS PTR
	LOCAL pszStr AS PSZ
	LOCAL dwLen AS DWORD
	LOCAL strucBM IS _WinBitmap

	

	IF IsString(xType)
		dwLen := SLen(xType)
		hGlobalMemory := GlobalAlloc(_Or(DWORD(GMEM_MOVEABLE), GMEM_ZEROINIT), dwLen + 1)

		IF (hGlobalMemory == NULL_PTR)
			RETURN SELF
		ENDIF

		pszStr := GlobalLock (hGlobalMemory)
		IF !OpenClipboard(NULL_PTR) .or. (pszStr == NULL_PSZ)
			GlobalFree(hGlobalMemory)
			RETURN SELF
		ENDIF   
		// RvdH 061204 Empty anything already there...
      EmptyClipboard()
		MemCopyString(pszStr,xType,dwLen)
		GlobalUnlock(hGlobalMemory)
		SetClipboardData(CF_TEXT, hGlobalMemory)
		CloseClipboard()

	ELSEIF IsInstanceOfUsual(xType,#Bitmap)

		IF !OpenClipboard(NULL_PTR)
			RETURN SELF
		ENDIF           
		// RvdH 061204 Empty anything already there...
      EmptyClipboard()
		hdc := GetDC(NULL_PTR)
		IF (hdc != NULL_PTR)
			hSrcDC := CreateCompatibleDC(hdc)
			IF (hSrcDC != NULL_PTR)
				SelectObject(hSrcDC, xType:Handle())
				GetObject(xType:Handle(), _SizeOf (_WinBitMap), @strucBM)

				hBitmap := CreateBitmapIndirect(@strucBM)
				IF (hBitmap != NULL_PTR)
					hDestDC := CreateCompatibleDC(hdc)
					IF (hDestDC != NULL_PTR)
						SelectObject( hDestDC, hBitmap )
						BitBlt ( hDestDC,0,0,strucBM:bmWidth, strucBM:bmHeight,;
							hSrcDC,0,0,SRCCOPY)
						SetClipboardData (CF_BITMAP, hBitmap)
						DeleteDC(hDestDC)
					ENDIF
				ENDIF
				DeleteDC(hSrcDC)
			ENDIF
			ReleaseDC(NULL_PTR, hdc)
		ENDIF
		CloseClipboard()
	ELSE
		WCError{#Insert,#Clipboard,__WCSTypeError,xType,1}:@@Throw()
	ENDIF

	RETURN SELF

METHOD InsertRTF(cText) 
	//PP-030929 New method
	STATIC LOCAL dwId AS DWORD
	LOCAL hText AS PTR
	LOCAL pPTR AS PTR
	LOCAL dwLen AS DWORD

	// Get Clipboard format id for RTF.
	IF dwId == 0
		dwID := RegisterClipboardFormat(PSZ("Rich Text Format"))
	ENDIF

	IF IsString(cText) .and. dwId > 0
		// Allocate global memory for transfer...
		dwLen := SLen(cTEXT)
		hText := GlobalAlloc(_or(GMEM_MOVEABLE,GMEM_DDESHARE), dwLen+1)

		// Put our string in the global memory...
		pPTR := GlobalLock(hText)

		IF ! OpenClipboard(NULL_PTR) .or. (PTR(_CAST,pPTR) == NULL_PTR)
			GlobalFree(hText)
			RETURN SELF
		ENDIF

		// Empty anything already there...
		EmptyClipboard()
		MemCopyString(pPTR,cText,dwLen)
		GlobalUnlock(hText)

		// Put data on the clipboard!
		SetClipboardData(dwId, hText)

		// Free memory...
		// RvdH 061002 Memory may not be freed, is now owned by Clipboard
		// GlobalFree(hText)

		// Close clipboard...
		CloseClipboard()

	ENDIF
	RETURN SELF

METHOD RetrieveBitmap(oBitmap) 
	LOCAL hClipData AS PTR
	LOCAL hdc AS PTR
	LOCAL hSrcDC AS PTR
	LOCAL hDestDC AS PTR
	LOCAL hBitmap AS PTR
	LOCAL strucBM IS _WinBitmap
	LOCAL lRetVal AS LOGIC

	
	IF !IsInstanceOfUsual(oBitmap,#Bitmap)
		WCError{#RetrieveBitmap,#Clipboard,__WCSTypeError,oBitmap,1}:@@Throw()
	ENDIF

	IF !OpenClipboard(NULL_PTR)
		RETURN FALSE
	ENDIF

	hClipData := GetClipboardData (CF_BITMAP)
	IF (hClipData != 0)
		hdc := GetDC(NULL_PTR)
		IF (hdc != NULL_PTR)
			hSrcDC := CreateCompatibleDC(hdc)
			IF (hSrcDC != NULL_PTR)
				hDestDC := CreateCompatibleDC(hdc)
				IF (hDestDC != NULL_PTR)
					SelectObject (hSrcDC, hClipData)
					GetObject(hClipData, _SizeOf (_WinBitmap), @strucBM)

					hBitmap := CreateBitmapIndirect(@strucBM)
					IF (hBitmap != NULL_PTR)
						SelectObject(hDestDC, hBitmap)
						BitBlt(hDestDC, 0, 0, strucBM:bmWidth, strucBM:bmHeight, hSrcDC, 0, 0, SRCCOPY)
						//RvdH 070215 This is now done in __SetHandle().
						// DeleteObject( oBitmap:Handle() )
						oBitmap:__SetHandle(hBitmap)
						lRetVal := TRUE
					ENDIF
					DeleteDC(hDestDC)
				ENDIF
				DeleteDC(hSrcDC)
			ENDIF
			ReleaseDC(NULL_PTR, hdc)
		ENDIF
	ENDIF

	CloseClipboard()

	RETURN lRetVal

METHOD RetrieveFiles(lMustExist) 
	//PP-031115 S Ebert
	LOCAL aFiles AS ARRAY
	LOCAL hClipData AS PTR
	LOCAL oDragEvent AS DragEvent
	LOCAL nFileCount, nI AS DWORD
	LOCAL cFName AS STRING

	IF OpenClipboard(NULL_PTR)

		hClipData := GetClipboardData(CF_HDROP)
		IF (hClipData != NULL_PTR)
			oDragEvent := DragEvent{Null_PTR, WM_DROPFILES, DWORD(_CAST,hClipData), 0, Null_Object}
			nFileCount := oDragEvent:FileCount
			Default(@lMustExist, FALSE)
			IF nFileCount>0
				aFiles := {}
				FOR nI := 1 UPTO nFileCount
					cFName := oDragEvent:FileName(nI)
					IF ! lMustExist .or. File(cFName)
						AAdd(aFiles, cFName)
					ENDIF
				NEXT  // nI
				IF ALen(aFiles)=0
					aFiles := Null_Array
				ENDIF
			ENDIF
		ENDIF

		CloseClipboard()
	ENDIF

	RETURN aFiles

METHOD RetrieveRTF(nStringLength) 
	//PP-030929 New method
	LOCAL dwLen AS DWORD
	LOCAL cRetVal AS STRING
	LOCAL pszStr AS PSZ
	LOCAL hClipData AS PTR
	STATIC LOCAL dwId AS DWORD

	IF dwId == 0
		dwID := RegisterClipboardFormat(PSZ("Rich Text Format"))
	ENDIF

	IF !IsNil(nStringLength)
		dwLen := nStringLength
	ENDIF

	IF !OpenClipboard(NULL_PTR)
		RETURN cRetVal
	ENDIF

	hClipData := GetClipboardData(dwID)
	IF (hClipData != NULL_PTR)
		pszStr := GlobalLock( hClipData )
		IF (dwLen == 0)
			dwLen := PszLen(pszStr)
		ENDIF
		cRetVal := Mem2String(pszStr,dwLen)
		GlobalUnlock(hClipData)
	ENDIF

	CloseClipboard()

	RETURN cRetVal

METHOD RetrieveString(nStringLength) 
	LOCAL dwLen AS DWORD
	LOCAL cRetVal AS STRING
	LOCAL pszStr AS PSZ
	LOCAL hClipData AS PTR

	

	IF !IsNil(nStringLength)
		IF !IsLong(nStringLength)
			WCError{#RetrieveString,#Clipboard,__WCSTypeError,nStringLength,1}:@@Throw()
		ENDIF
		dwLen := nStringLength
	ENDIF

	IF !OpenClipboard(NULL_PTR)
		RETURN cRetVal
	ENDIF

	hClipData := GetClipboardData (CF_TEXT)
	IF (hClipData != NULL_PTR)
		pszStr := GlobalLock( hClipData )
		IF (dwLen == 0)
			dwLen := PszLen(pszStr)
		ENDIF
		cRetVal := Mem2String(pszStr,dwLen)
		GlobalUnlock(hClipData)
	ENDIF

	CloseClipboard()

	RETURN cRetVal

END CLASS

