CLASS PrintingDevice INHERIT VObject
	PROTECT cDriver AS STRING
	PROTECT cDevice AS STRING
	PROTECT cPort AS STRING
	PROTECT hPrinter AS PTR
	PROTECT pDevMode AS _winDEVMODE
	PROTECT lValid AS LOGIC
	PROTECT iSize AS INT

	//PP-030828 Strong typing
	METHOD __FillDevMode() AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	IF (pDevMode != NULL_PTR)
		MemFree(pDevMode)
		pDevMode := NULL_PTR
	ENDIF

	IF (hPrinter == NULL_PTR)
		lValid := FALSE
		RETURN FALSE
	ENDIF

	iSize := PCALL(gpfnDocumentProperties, NULL_PTR, hPrinter, String2Psz(cDevice), NULL_PTR, NULL_PTR, 0)

	IF (iSize <= 0)
		lValid := FALSE
		RETURN FALSE
	ENDIF

	pDevMode := MemAlloc(DWORD(iSize))

	PCALL(gpfnDocumentProperties, NULL_PTR, hPrinter, String2Psz(cDevice), pDevMode, NULL_PTR, DM_COPY)

	RETURN TRUE

ACCESS Copies 
	

	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmCopies
	ENDIF
	RETURN 0

ASSIGN Copies(nCopies) 
	

	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF

	pDevMode:dmCopies := nCopies
	SELF:UpdateDevMode()

	RETURN 

METHOD Destroy() 
	

	IF (hPrinter != NULL_PTR)
		PCALL(gpfnClosePrinter, hPrinter)
	ENDIF

	IF (pDevMode != NULL_PTR)
		MemFree(pDevMode)
	ENDIF

	IF !InCollect()
		//PP-030505
		lValid := FALSE

		//PP-040418 Issue 12733
		UnregisterAxit(SELF)
		hPrinter := NULL_PTR
		pDevMode := NULL_PTR
	ENDIF

	RETURN  SELF

ACCESS Device 
	

	RETURN cDevice

METHOD DeviceCapabilities(wCapability) 
	LOCAL pWord				AS WORD PTR
	LOCAL pDWord			AS DWORD PTR
	LOCAL pLong				AS LONGINT PTR
	LOCAL pByte				AS BYTE PTR  
	LOCAL dwReturn			AS DWORD 
	LOCAL dwTotal			AS DWORD
	LOCAL aReturn := {}	AS ARRAY
	LOCAL uReturn 			AS USUAL
	LOCAL i, dwTrail, dwStart 	AS DWORD 
	LOCAL nElementSize	AS DWORD 
	LOCAL bSave				AS BYTE     
	LOCAL pszData			AS PSZ

	// For DC_COPIES, DC_DRIVER, DC_DUPLEX, DC_EXTRA, DC_FIELDS, DC_MAXSIZE, DC_MINSIZE
	// DC_SIZE, DC_ORIENTATION, DC_TRUETYPE and DC_VERSION dwReturn is the value for the capabilty.
	// For all others capabalities it is the size of the buffer needed to hold the value.
	// A second call is then required.

	dwReturn := PCALL(gpfnDeviceCapabilities, String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
		NULL_PSZ, pDevMode)

	IF (dwReturn == 0xFFFFFFFF)					// unsupported capability
		uReturn := dwReturn


	ELSEIF (wCapability == DC_MAXEXTENT)	.OR. (wCapability == DC_MINEXTENT)
	   AAdd(aReturn, LoWord(dwReturn))
	   AAdd(aReturn, HiWord(dwReturn))
	   uReturn := aReturn
		
	ELSEIF (wCapability == DC_BINS) .OR. (wCapability == DC_PAPERS)
		// array of words
		pWord := MemAlloc(dwReturn* _SIZEOF(WORD))
		PCALL(gpfnDeviceCapabilities, String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
			pWord, pDevMode)

		FOR i := 1 TO dwReturn
			AAdd(aReturn, pWord[i])
		NEXT

		uReturn := aReturn
		MemFree(pWord)

	ELSEIF  wCapability == DC_BINNAMES .OR. wCapability == DC_FILEDEPENDENCIES ;
		.OR. wCapability == DC_PAPERNAMES .OR. wCapability == DC_MEDIATYPENAMES ;
		.OR. wCapability == DC_PERSONALITY   
		// Array of PSZs. 
		// Set element size
		nElementSize := 0
  		IF wCapability == DC_BINNAMES
  			nElementSize := CCHBINNAME 	// 24  
  			
  		ELSEIF  wCapability == DC_FILEDEPENDENCIES ;
  			.OR. wCapability == DC_PAPERNAMES;
  			.OR. wCapability == DC_MEDIATYPENAMES
  			nElementSize := CCHPAPERNAME	// 64
  			
  		ELSEIF wCapability == DC_PERSONALITY
  			nElementSize := 32
 		
  		ELSE
  			// What did I miss ?
			
  		ENDIF     
  		IF nElementSize > 0
			// array of nElementSize byte "c" strings
			dwTotal := dwReturn * nElementSize 
			pByte := MemAlloc(dwTotal+1)
			PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
				pByte, pDevMode)
	
			FOR i := 1 TO dwTotal STEP nElementSize   
				// If the Element = nElementSize characters we have no 0 terminator !
				// So make sure we set one 
				dwStart := i
				dwTrail := dwStart + nElementSize
	         bSave   := pByte[dwTrail]
				pByte[dwTrail] := 0      
				pszData := pByte + dwStart -1        
				AAdd(aReturn, Trim(Psz2String(pszData)))  
				pByte[dwTrail] := bSave
			NEXT
			MemFree(pByte)
  		ENDIF
		uReturn := aReturn

	ELSEIF wCapability == DC_ENUMRESOLUTIONS 
		// array of pairs of long ints  
		pLong := MemAlloc(dwReturn * 2 * _SIZEOF(LONGINT))
		PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
			pLong, pDevMode)

		FOR i := 1 TO dwReturn
			AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		NEXT

		uReturn := aReturn
		MemFree(pLong)

	ELSEIF wCapability == DC_MEDIATYPES .OR. wCapability == DC_NUP 
		// array of DWORDS
		pDWord := MemAlloc(dwReturn * _SIZEOF(DWORD))
		PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
			pDWord, pDevMode)

		FOR i := 1 TO dwReturn
			AAdd(aReturn, pDWord[i])
		NEXT

		uReturn := aReturn
		MemFree(pDWord)

	ELSEIF (wCapability == DC_PAPERSIZE)
		// array of pairs of Longs
		pLong  := MemAlloc(dwReturn*2 * _SIZEOF(LONGINT))

		PCALL(gpfnDeviceCapabilities,String2Psz(cDevice), String2Psz(cPort), WORD(wCapability),;
			pLong, pDevMode)

		FOR i := 1 TO dwReturn
			AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		NEXT

		uReturn := aReturn
		MemFree(pLong)
	ELSE	// dwReturn is desired value
		uReturn := dwReturn

	ENDIF

	RETURN uReturn

ACCESS Driver 
	

	RETURN cDriver

METHOD GetDevMode() 
	

	RETURN pDevMode

CONSTRUCTOR(uName) 
	LOCAL ptrTemp AS PTR
	LOCAL cTemp AS STRING
	LOCAL wTemp1 AS DWORD
	LOCAL wTemp2 AS DWORD

	

	__LoadWinSpoolDLL()

	SUPER()

	

	IF IsNil(uName)
		ptrTemp := MemAlloc(256)
        IF GetProfileString(String2Psz("windows"), String2Psz("Device"), String2Psz(_CHR(0)), ptrTemp, 256) == 0
			RETURN
		ENDIF
		cTemp := Psz2String(ptrTemp)
		MemFree(ptrTemp)
	ELSE
		IF !IsString(uName)
			WCError{#Init,#PrintingDevice,__WCSTypeError,uName,1}:Throw()
		ENDIF
		cTemp:=uName
	ENDIF

	wTemp1 := At2(",", cTemp) //position of first comma

	IF (wTemp1 != 0)
		cDevice := SubStr3(cTemp, 1, wTemp1-1) //extract device
		wTemp2 := At3(",", cTemp, wTemp1+1) //Position of second comma

		IF (wTemp2 != 0)
			cDriver := SubStr3(cTemp, wTemp1+1, wTemp2-wTemp1-1) //extract driver
			wTemp1 := wTemp2
			wTemp2 := SLen(cTemp)

			IF (wTemp2 > wTemp1)
				cPort := SubStr3(cTemp, wTemp1+1, wTemp2-wTemp1) //rest of string is port
			ENDIF
		ENDIF
	ENDIF

	lValid := PCALL(gpfnOpenPrinter, String2Psz(cDevice), PTR(_CAST, @hPrinter), NULL_PTR)
	isize := _SIZEOF(_winDEVMODE)

	SELF:__FillDevMode()

	RETURN 

METHOD IsValid() 
	

	RETURN lValid

ACCESS Orientation 
	

	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmOrientation
	ENDIF
	RETURN 0

ASSIGN Orientation(nOrientation) 
	

	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF

	pDevMode:dmOrientation := nOrientation
	SELF:UpdateDevMode()

	RETURN 


ACCESS PaperHeight 
	

	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperLength
	ENDIF
	RETURN 0


ASSIGN PaperHeight(nHeight) 
	

	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF

	pDevMode:dmPaperLength := nHeight
	SELF:UpdateDevMode()

	RETURN 


ACCESS PaperSize 
	

	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperSize
	ENDIF
	RETURN 0

ASSIGN PaperSize(nSize) 
	

	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF

	pDevMode:dmPaperSize := nSize
	SELF:UpdateDevMode()

	RETURN 

ACCESS PaperWidth 
	

	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperWidth
	ENDIF
	RETURN 0

ASSIGN PaperWidth(nWidth) 
	

	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF

	pDevMode:dmPaperWidth := nWidth
	SELF:UpdateDevMode()

	RETURN 

ACCESS Port 
	

	RETURN cPort

METHOD SetUp() 
	LOCAL pd 			IS _WinPrintDlg
	LOCAL struDevN 	AS _windevNames
	LOCAL lRetVal 		AS LOGIC
	LOCAL ptrHandle 	AS PTR
	LOCAL ptrString 	AS PTR
	LOCAL hDevMode 	AS PTR
	LOCAL pTemp 		AS PTR
	LOCAL pDVTemp 		AS _winDEVMODE

 	ptrHandle := GetActiveWindow()
	lRetVal := TRUE
	pd:lStructSize := _SIZEOF(_winPRINTDLG)

	IF (ptrHandle != NULL_PTR)
		pd:hwndOwner := ptrHandle
	ELSE
		pd:hwndOwner := NULL_PTR
	ENDIF

	hDevMode := GlobalAlloc(GHND, DWORD(iSize))
	IF (hDevMode != NULL_PTR) .AND. (pDevMode != NULL_PTR)
		pTemp := GlobalLock(hDevMode)
		MemCopy(pTemp, pDevMode, DWORD(iSize))
		GlobalUnlock(hDevMode)
	ENDIF

	pd:hDevMode := hDevMode
	pd:hDevNames := NULL_PTR
	pd:nFromPage := 0
	pd:nToPage := 0
	pd:nMinPage := 0
	pd:nMaxPage := 0
	pd:nCopies := 0
	pd:hInstance := _GetInst()
	pd:Flags := _OR(PD_USEDEVMODECOPIES, PD_COLLATE, PD_SELECTION, PD_PRINTSETUP)
	pd:lpfnSetupHook := NULL_PTR
	pd:lpSetupTemplateName := NULL_PTR
	pd:lpfnPrintHook := NULL_PTR
	pd:lpPrintTemplateName := NULL_PTR

	IF !__LoadComDlgDLL() .OR. !PCALL(gpfnPrintDlg, @pd)
		//PP-030505 Bug:122
		lValid := FALSE
		lRetVal := FALSE
	ELSE
		//PP-030505 Bug:122
		lValid := TRUE
		//Liuho01 05-30-96
		//Get the Devive name, Driver name, and port name which user selected
		struDevN := GlobalLock(pd:hDevNames)
		ptrString := PTR(_CAST,(DWORD(_CAST,struDevN) + struDevN:wDriverOffset ))
		cDriver := Psz2String(ptrString)
		ptrString := PTR(_CAST,(DWORD(_CAST,struDevN) + struDevN:wDeviceOffset ))
		cDevice := Psz2String(ptrString)
		ptrString := PTR(_CAST,(DWORD(_CAST,struDevN) + struDevN:wOutPutOffset ))
		cPort := Psz2String(ptrString)
		GlobalUnlock(pd:hDevNames)
		GlobalFree(pd:hDevNames)
		//Liuho01 05-30-96
		//Get the information about the printer envirenment which user select.

		pDVTemp := GlobalLock(pd:hDevMode)
		iSize := pDVTemp:dmSize + pDVTemp:dmDriverExtra
		IF (pDevMode == NULL_PTR)
			pDevMode := MemAlloc(DWORD(iSize))
		ELSE
			pDevMode := MemRealloc(pDevMode, DWORD(iSize))
		ENDIF
		MemCopy(pDevMode, pDVTemp, DWORD(iSize))
		GlobalUnlock(pd:hDevMode)
		GlobalFree(pd:hDevMode)
	ENDIF

	RETURN lRetVal

METHOD UpdateDevMode() 
	LOCAL iRes AS INT

	

	IF (pDevMode == NULL_PTR)
		RETURN FALSE
	ENDIF

	iRes := PCALL(gpfnDocumentProperties, NULL_PTR, hPrinter, String2Psz(cDevice), NULL_PTR, pDevMode, DM_MODIFY)

	RETURN (iRes >= 0)


END CLASS

FUNCTION __LoadWinSpoolDLL()
	LOCAL hDll AS PTR
	LOCAL rsFormat AS ResourceString

	IF glWinSpoolDllLoaded
		RETURN TRUE
	ENDIF

    hDll := LoadLibrary(String2Psz("WINSPOOL.DRV"))
	IF (hDll == NULL_PTR)
		rsFormat := ResourceString{__WCSLoadLibraryError}
		WCError{#LoadSplitWindowDLL, #SplitWindow, VO_Sprintf(rsFormat:value, "WINSPOOL.DRV"),,,FALSE}:Throw()
		RETURN FALSE
	ENDIF

    gpfnDocumentProperties  := GetProcAddress(hDll, String2Psz( "DocumentPropertiesA"))
    gpfnOpenPrinter         := GetProcAddress(hDll, String2Psz( "OpenPrinterA"))
    gpfnClosePrinter        := GetProcAddress(hDll, String2Psz( "ClosePrinter"))
    gpfnDeviceCapabilities  := GetProcAddress(hDll, String2Psz( "DeviceCapabilitiesA"))

	RETURN (glWinSpoolDllLoaded := TRUE)


//STRUCT __LongArrayStruct
//	MEMBER DIM aLong[1] AS LONG

//STRUCT __WordArrayStruct
//	MEMBER DIM aWord[1] AS WORD

STATIC GLOBAL glWinSpoolDllLoaded := FALSE AS LOGIC
STATIC GLOBAL gpfnClosePrinter AS TClosePrinter PTR
STATIC GLOBAL gpfnDeviceCapabilities AS TDeviceCapabilities PTR

STATIC GLOBAL gpfnDocumentProperties AS TDocumentProperties PTR
STATIC GLOBAL gpfnOpenPrinter AS TOpenPrinter PTR
STATIC FUNCTION TClosePrinter(hPrinter AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TDeviceCapabilities(lpszDevice AS PSZ, lpszPort AS PSZ, wCapability AS WORD, lpszOutput AS PTR,;   // 070625 changed param 4 type from PSZ to PTR
	pDevMode AS PTR) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TDocumentProperties(hWnd AS PTR, pPrinter AS PTR, pDevicename AS PSZ, pDevModeOutput AS _winDEVMODE,;
	pDevModeInput AS _winDEVMODE, fMode AS DWORD) AS LONGINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TOpenPrinter(pPrinterName AS PSZ, phPrinter AS PTR, pDefault AS _winPRINTER_DEFAULTS) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE



#region defines
DEFINE CAPABILITY_NOT_AVAILABLE := 0xFFFFFFFF
DEFINE CCHBINNAME := 24
DEFINE CCHPAPERNAME := 64
#endregion
