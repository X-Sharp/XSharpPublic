USING System.Runtime.InteropServices
USING Microsoft.Win32
/// <include file="Gui.xml" path="doc/PrintingDevice/*" />
CLASS PrintingDevice INHERIT VObject
	PROTECT cDriver AS STRING
	PROTECT cDevice AS STRING
	PROTECT cPort AS STRING
	PROTECT hPrinter AS PTR
	PROTECT pDevMode AS _winDEVMODE
	PROTECT lValid AS LOGIC
	PROTECT iSize AS INT


	//PP-030828 Strong typing
 /// <exclude />
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


	iSize := Win32.DocumentProperties( NULL_PTR, hPrinter, cDevice, NULL_PTR, NULL_PTR, 0)


	IF (iSize <= 0)
		lValid := FALSE
		RETURN FALSE
	ENDIF


	pDevMode := MemAlloc(DWORD(iSize))


	Win32.DocumentProperties(NULL_PTR, hPrinter, cDevice, pDevMode, NULL_PTR, DM_COPY)


	RETURN TRUE


/// <include file="Gui.xml" path="doc/PrintingDevice.Copies/*" />
ACCESS Copies




	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmCopies
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/PrintingDevice.Copies/*" />
ASSIGN Copies(nCopies)




	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF


	pDevMode:dmCopies := nCopies
	SELF:UpdateDevMode()


	RETURN


/// <include file="Gui.xml" path="doc/PrintingDevice.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF (hPrinter != NULL_PTR)
		Win32.ClosePrinter( hPrinter)
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


/// <include file="Gui.xml" path="doc/PrintingDevice.Device/*" />
ACCESS Device




	RETURN cDevice


/// <include file="Gui.xml" path="doc/PrintingDevice.DeviceCapabilities/*" />
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


	dwReturn := Win32.DeviceCapabilities(cDevice, cPort, WORD(wCapability),NULL, pDevMode)


	IF (dwReturn == 0xFFFFFFFF)					// unsupported capability
		uReturn := dwReturn




	ELSEIF (wCapability == DC_MAXEXTENT)	.OR. (wCapability == DC_MINEXTENT)
	   AAdd(aReturn, LoWord(dwReturn))
	   AAdd(aReturn, HiWord(dwReturn))
	   uReturn := aReturn


	ELSEIF (wCapability == DC_BINS) .OR. (wCapability == DC_PAPERS)
		// array of words
		pWord := MemAlloc(dwReturn* _SIZEOF(WORD))
		Win32.DeviceCapabilities( cDevice, cPort, WORD(wCapability),pWord, pDevMode)


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
			NOP
  		ENDIF
  		IF nElementSize > 0
			// array of nElementSize byte "c" strings
			dwTotal := dwReturn * nElementSize
			pByte := MemAlloc(dwTotal+1)
			Win32.DeviceCapabilities(cDevice, cPort, WORD(wCapability),pByte, pDevMode)


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
		Win32.DeviceCapabilities(cDevice, cPort, WORD(wCapability),pLong, pDevMode)


		FOR i := 1 TO dwReturn
			AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		NEXT


		uReturn := aReturn
		MemFree(pLong)


	ELSEIF wCapability == DC_MEDIATYPES .OR. wCapability == DC_NUP
		// array of DWORDS
		pDWord := MemAlloc(dwReturn * _SIZEOF(DWORD))
		Win32.DeviceCapabilities(cDevice, cPort, WORD(wCapability),pDWord, pDevMode)


		FOR i := 1 TO dwReturn
			AAdd(aReturn, pDWord[i])
		NEXT


		uReturn := aReturn
		MemFree(pDWord)


	ELSEIF (wCapability == DC_PAPERSIZE)
		// array of pairs of Longs
		pLong  := MemAlloc(dwReturn*2 * _SIZEOF(LONGINT))


		Win32.DeviceCapabilities(cDevice, cPort, WORD(wCapability),pLong, pDevMode)


		FOR i := 1 TO dwReturn
			AAdd(aReturn, { pLong[i*2-1], pLong[i*2]})
		NEXT


		uReturn := aReturn
		MemFree(pLong)
	ELSE	// dwReturn is desired value
		uReturn := dwReturn


	ENDIF


	RETURN uReturn


/// <include file="Gui.xml" path="doc/PrintingDevice.Driver/*" />
ACCESS Driver




	RETURN cDriver


/// <include file="Gui.xml" path="doc/PrintingDevice.GetDevMode/*" />
METHOD GetDevMode()




	RETURN pDevMode


/// <include file="Gui.xml" path="doc/PrintingDevice.ctor/*" />
CONSTRUCTOR(uName)
	LOCAL cTemp     AS STRING


	SUPER()






	IF IsNil(uName)
        LOCAL oKey AS  RegistryKey
        LOCAL cSKey AS STRING
        oKey := Registry.CurrentUser:OpenSubKey( "SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows\SessionDefaultDevices")
        IF okey != NULL_OBJECT
            FOREACH csubKeyName AS STRING IN okey:GetSubKeyNames()
                cSKey := csubKeyName
            NEXT
            cTemp  := (STRING) Registry.GetValue("HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows\SessionDefaultDevices\" + cSKey ,"Device","")
        ENDIF
        IF Empty(cTemp)
            cTemp := (STRING) Registry.GetValue("HKEY_CURRENT_USER\Software\Microsoft\Windows NT\CurrentVersion\Windows","Device","")
        ENDIF
	ELSE
		IF !IsString(uName)
			WCError{#Init,#PrintingDevice,__WCSTypeError,uName,1}:Throw()
		ENDIF
		cTemp:=uName
	ENDIF


    LOCAL sParts AS STRING[]
    sParts := cTemp:Split(",":ToCharArray())
    SELF:cDevice := sParts[1]
    IF sParts:Length > 1
        SELF:cDriver := sParts[2]
    ENDIF
    IF sParts:Length > 2
        SELF:cPort := sParts[3]
    ENDIF
	lValid := Win32.OpenPrinter( SELF:cDevice, REF hPrinter, NULL_PTR)
	isize := _SIZEOF(_winDEVMODE)


	SELF:__FillDevMode()


	RETURN


/// <include file="Gui.xml" path="doc/PrintingDevice.IsValid/*" />
METHOD IsValid()




	RETURN lValid


/// <include file="Gui.xml" path="doc/PrintingDevice.Orientation/*" />
ACCESS Orientation




	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmOrientation
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/PrintingDevice.Orientation/*" />
ASSIGN Orientation(nOrientation)




	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF


	pDevMode:dmOrientation := nOrientation
	SELF:UpdateDevMode()


	RETURN




/// <include file="Gui.xml" path="doc/PrintingDevice.PaperHeight/*" />
ACCESS PaperHeight




	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperLength
	ENDIF
	RETURN 0




/// <include file="Gui.xml" path="doc/PrintingDevice.PaperHeight/*" />
ASSIGN PaperHeight(nHeight)




	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF


	pDevMode:dmPaperLength := nHeight
	SELF:UpdateDevMode()


	RETURN




/// <include file="Gui.xml" path="doc/PrintingDevice.PaperSize/*" />
ACCESS PaperSize




	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperSize
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/PrintingDevice.PaperSize/*" />
ASSIGN PaperSize(nSize)




	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF


	pDevMode:dmPaperSize := nSize
	SELF:UpdateDevMode()


	RETURN


/// <include file="Gui.xml" path="doc/PrintingDevice.PaperWidth/*" />
ACCESS PaperWidth




	IF (pDevMode != NULL_PTR)
		RETURN pDevMode:dmPaperWidth
	ENDIF
	RETURN 0


/// <include file="Gui.xml" path="doc/PrintingDevice.PaperWidth/*" />
ASSIGN PaperWidth(nWidth)




	IF (pDevMode == NULL_PTR)
		RETURN 0
	ENDIF


	pDevMode:dmPaperWidth := nWidth
	SELF:UpdateDevMode()


	RETURN


/// <include file="Gui.xml" path="doc/PrintingDevice.Port/*" />
ACCESS Port




	RETURN cPort


/// <include file="Gui.xml" path="doc/PrintingDevice.SetUp/*" />
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


	IF !PrintDlg( @pd)
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


/// <include file="Gui.xml" path="doc/PrintingDevice.UpdateDevMode/*" />
METHOD UpdateDevMode()
	LOCAL iRes AS INT






	IF (pDevMode == NULL_PTR)
		RETURN FALSE
	ENDIF


	iRes := Win32.DocumentProperties(NULL_PTR, hPrinter, cDevice, NULL_PTR, pDevMode, DM_MODIFY)


	RETURN (iRes >= 0)




END CLASS


 /// <exclude />
FUNCTION __LoadWinSpoolDLL()
RETURN TRUE






INTERNAL STATIC CLASS Win32
    [DllImport("WinSpool.drv", SetLastError := TRUE, EntryPoint := "ClosePrinter", CharSet := CharSet.Ansi)];
    STATIC EXTERN METHOD ClosePrinter(hPrinter AS PTR) AS LOGIC STRICT


    [DllImport("WinSpool.drv", SetLastError := TRUE, EntryPoint := "DeviceCapabilitiesA", CharSet := CharSet.Ansi)];
    STATIC EXTERN METHOD DeviceCapabilities(lpszDevice AS STRING, lpszPort AS STRING, wCapability AS WORD, lpszOutput AS PTR,;   // 070625 changed param 4 type from PSZ to PTR
	pDevMode AS PTR) AS DWORD STRICT


    [DllImport("WinSpool.drv", SetLastError := TRUE, EntryPoint := "DocumentPropertiesA", CharSet := CharSet.Ansi)];
    STATIC EXTERN METHOD DocumentProperties(hWnd AS PTR, pPrinter AS PTR, pDevicename AS STRING, pDevModeOutput AS _winDEVMODE,;
	pDevModeInput AS _winDEVMODE, fMode AS DWORD) AS LONGINT STRICT


    [DllImport("WinSpool.drv", SetLastError := TRUE, EntryPoint := "OpenPrinterA", CharSet := CharSet.Ansi)];
    STATIC EXTERN METHOD OpenPrinter(pPrinterName AS STRING, phPrinter REF PTR, pDefault AS _winPRINTER_DEFAULTS) AS LOGIC STRICT
END CLASS




#region defines
DEFINE CAPABILITY_NOT_AVAILABLE := 0xFFFFFFFF
DEFINE CCHBINNAME := 24
DEFINE CCHPAPERNAME := 64
#endregion
