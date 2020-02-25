CLASS Printer INHERIT Window
	PROTECT oPerr AS PrinterErrorEvent
	PROTECT lprValid AS LOGIC
	EXPORT lprAbort AS LOGIC
	PROTECT lprPage AS LOGIC
	PROTECT lDocStarted AS LOGIC
	PROTECT pszStdFonts AS PSZ
	PROTECT PrStdfont AS LOGIC
	PROTECT sJobname AS STRING
	PROTECT iWidth AS INT
	PROTECT iHeight AS INT

#ifdef __VULCAN__
   HIDDEN PrinterAbortProcDelegate AS __PrinterAbortProcDelegate
#endif
	//PP-030828 Strong typing
	METHOD __GetDC() AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hFont AS PTR
	LOCAL oFont AS Font
	LOCAL oPen AS Pen
	LOCAL oForeground AS Brush
	LOCAL strucLogPen IS _WinLogPen

	

	IF (hDC == NULL_PTR)	 // Invalid printer
		SELF:__ResetDCFlags()
		lprValid := FALSE
		RETURN 0
	ENDIF

	IF (!DCInitialized)
		IF (WCGetCoordinateSystem() == WCCartesianCoordinates)
			SetMapMode(hDC, MM_TEXT)
			SetMapMode(hDC, MM_ANISOTROPIC)

			SetViewportExtEx(hDC, iWidth, iHeight, NULL_PTR) // logical coords used by GDI
			SetWindowExtEx(hDC, iWidth, -iHeight, NULL_PTR) // device coords
			SetViewportOrgEx(hDC, 0, iHeight-1, NULL_PTR)
		ELSE
			SetMapMode(hDC, MM_TEXT)

			SetViewportOrgEx(hDC, 0, 0, NULL_PTR)
		ENDIF

		DCInitialized := TRUE
	ENDIF

	IF SELF:DCFontNeeded .AND. !SUPER:DCFontInUse
		oFont := SELF:Font
		IF (oFont != NULL_OBJECT)
			oFont:Create(TRUE, hDC)
			hFont := oFont:Handle()
		ENDIF

		IF (hFont != NULL_PTR)
			SelectObject(hDC, hFont)
		ELSE
			SelectObject(hDC, GetStockObject(DEVICE_DEFAULT_FONT)) // * 0.5 *
		ENDIF

		DCFontInUse := TRUE
		DCFontNeeded := FALSE
		SetTextAlign(hDC, _OR(TA_LEFT, TA_BOTTOM))
	ENDIF

	IF SELF:DCPenNeeded .AND. !SUPER:DCPenInUse
		oPen := SELF:Pen
		IF (oPen != NULL_OBJECT)
			SelectObject(hDC, oPen:Handle())
			__WCLogicalPen(oPen, @strucLogPen)
			SetTextColor(hDC, strucLogPen:lopnColor)
		ELSE
			SelectObject(hDC, GetStockObject(BLACK_PEN))
			SetTextColor(hDC, GetSysColor(COLOR_WINDOWTEXT))
		ENDIF
		DCPenInUse := TRUE
		DCPenNeeded := FALSE
	ENDIF

	IF SELF:DCBrushNeeded .AND. !SELF:DCBrushInUse
		oForeground := SELF:Foreground
		IF (oForeground != NULL_OBJECT)
			SelectObject(hDC, oForeground:Handle())
		ELSE
			// Stock Object Black_Brush is default brush
			SelectObject(hDC, GetStockObject(Black_Brush))
		ENDIF

		DCBrushInUse := TRUE
		DCBrushNeeded := FALSE
	ENDIF

	RETURN hDC

METHOD __ResetDCFlags() AS Printer STRICT 
	//PP-030828 Strong typing
	

	DCInitialized := FALSE
	DCPenNeeded := TRUE
	DCBrushNeeded := TRUE
	DCFontNeeded := TRUE
	DCPenInUse := FALSE
	DCBrushInUse := FALSE
	DCFontInUse := FALSE

	RETURN SELF

METHOD Abort() 
	

	lprAbort := TRUE
	RETURN SELF

METHOD Aborted() 
	

	RETURN lprAbort

METHOD BeginDoc() 
	LOCAL iRetVal AS INT
	LOCAL DocInfo IS _winDOCINFO

	MemSet(@DocInfo, 0, _SIZEOF(_winDOCINFO))
	DocInfo:cbSize := _SIZEOF(_winDOCINFO)

	

	//if (NULL_STRING != sJobName)
	//	pszJob := String2Psz(sJobName)
	//endif

	//iRetVal := Escape(hDC, STARTDOC_, SLen(sJobName), pszJob, NULL_PTR)

	DocInfo:lpszDocName := String2Psz(sJobname)
	iRetVal := StartDoc(hDC, @DocInfo)

	lprValid := TRUE

	IF (iRetVal > 0)
		lDocStarted := TRUE
		iRetVal := StartPage(hDC)
		SELF:__ResetDCFlags()
	ELSE
		lprValid := FALSE
		hDC := 0
	ENDIF

	RETURN lprValid

ACCESS CanvasArea 
	

	RETURN BoundingBox{Point{0, 0}, Dimension{iWidth, iHeight}}

METHOD Destroy() 
	

	IF (hDC != NULL_PTR)
		IF	lprValid
			IF (lDocStarted)
				EndPage(hDC)
				EndDoc(hDC)
			ENDIF
			__WCDelPrinterFromArray(hDC)
		ENDIF
		DeleteDC(hDC)
		hDC := NULL_PTR
	ENDIF

	IF !InCollect()
		oPerr :=	NULL_OBJECT
		aPrinterhDCPrinter := {}
	ENDIF

	SUPER:Destroy()

	RETURN SELF

METHOD Handle(ServiceID) AS PTR
	

	RETURN hDC

	/*
	Default(@ServiceID, API_WINDOW_HDC)

	if ServiceID = API_WINDOW_HWND
	return self:hWnd
	endif

	if ServiceID = API_WINDOW_HDC
	return hDC
	endif */

METHOD Idle() 
	

	IF (oApp != NULL_OBJECT)
		oApp:Exec(ExecWhileEvent)
	ENDIF

	RETURN SELF

CONSTRUCTOR(cJobname, oDevice) 
	LOCAL cDevice AS STRING
	LOCAL cDriver AS STRING
	LOCAL cPort AS STRING
	LOCAL ptrDevMode AS PTR
	LOCAL oPrintingDev AS PrintingDevice

	

	lprValid		:= FALSE
	lprAbort		:= FALSE
	lprPage 		:= FALSE
	lDocStarted := FALSE
	aPrinterhDCPrinter := {}

	

	SUPER()

	IF !IsNil(oDevice)
		IF !IsInstanceOfUsual(oDevice, #PrintingDevice)
			WCError{#Init,#Printer,__WCSTypeError,oDevice,2}:Throw()
		ENDIF
		oPrintingDev := oDevice
	ELSE
		oPrintingDev := PrintingDevice{}
	ENDIF

	cDevice := oPrintingDev:Device
	cDriver := oPrintingDev:Driver
	cPort := oPrintingDev:Port
	ptrDevMode := oPrintingDev:GetDevMode()

	IF IsNil(cDevice) .AND. IsNil(cDriver) .AND. IsNil(cPort)
		hDC := NULL
		RETURN
	ENDIF

	hDC := CreateDC(String2Psz(cDriver), String2Psz(cDevice), String2Psz(cPort), ptrDevMode)
	IF (hDC != NULL_PTR)
		__WCAddPrinterToArray(hDC, SELF)
		iWidth := GetDeviceCaps(hDC, HORZRES)
		iHeight := GetDeviceCaps(hDC, VERTRES)
		SELF:__ResetDCFlags()
		SELF:__GetDc()
#ifdef __VULCAN__
      PrinterAbortProcDelegate := __PrinterAbortProcDelegate{ NULL, @__PrinterAbortProc() }
		SetAbortProc(hDC, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate((System.Delegate) PrinterAbortProcDelegate ) )
#else		
		SetAbortProc(hDC, @__PrinterAbortProc())
#endif		
		GetLastError()

		IF !IsNil(cJobName)
			IF !IsString(cJobname)
				WCError{#Init,#Printer, __WCStypeError, cJobname, 2}:Throw()
			ENDIF
			sJobName := cJobname
		ELSE
			sJobName := NULL_STRING
		ENDIF

		lprValid := TRUE
	ENDIF

	RETURN 

METHOD IsValid() 
	
	RETURN	lprValid

METHOD NewPage() 
	LOCAL error_val AS INT
	LOCAL repeat_flag AS LOGIC
	LOCAL repeatable AS LOGIC

	

	IF lprValid
		oPerr := PrinterErrorEvent{NULL_PTR,0,0,0,SELF}
		error_val := EndPage(hDC)
		IF error_val < 0
			DO	WHILE (repeat_flag .AND. repeatable .AND. (error_val < 0))
				DO CASE
				CASE error_val == SP_ERROR
					oPerr:wParam := PRINTERERRORGENERALERROR
					repeatable := TRUE

				CASE error_val == SP_APPABORT
					oPerr:wParam := PRINTERERRORGENERALERROR

				CASE error_val == SP_USERABORT
					oPerr:wParam := PRINTERERRORUSERABORT

				CASE error_val == SP_OUTOFDISK
					repeatable := TRUE
					oPerr:wParam := PRINTERERRORNODISKSPACE

				CASE error_val == SP_OUTOFMEMORY
					repeatable := TRUE
					oPerr:wParam := PRINTERERRORNOMEMSPACE

				OTHERWISE
					oPerr:wParam := PRINTERERRORGENERALERROR
				ENDCASE

				repeat_flag := SELF:PrinterError(oPerr)
				IF repeat_flag
					error_val := EndPage(hDC)
				ELSE
					lprAbort := TRUE
				ENDIF
			ENDDO

		ELSEIF lprAbort // user Abort
			error_val := SP_USERABORT
		ELSE
			SELF:__ResetDCFlags()
		ENDIF
		lprPage := TRUE

		// Do we need to clean up ?

		IF error_val >= 0
			error_val := StartPage(hDC)
			DCInitialized := FALSE
		ENDIF

		IF error_val < 0
			lprValid := FALSE
			AbortDoc(hDC)
			hDC := 0
		ENDIF
	ENDIF
	RETURN SELF

METHOD PrinterError(oPerr) 
	LOCAL rsTitle AS ResourceString
	LOCAL rsError AS ResourceString
	LOCAL wErrorType AS WORD
	LOCAL mb AS TextBox
	

	IF !IsNil(oPerr) .AND. !IsInstanceOfUsual(oPerr,#PrinterErrorEvent)
		WCError{#PrinterError,#printer,__WCSTypeError,oPerr,1}:Throw()
	ENDIF

	wErrorType := oPerr:ErrorType
	IF wErrorType = PRINTERERRORNODISKSPACE
		rsTitle := ResourceString{__WCSError}
		rsError := ResourceString{__WCSPrinterNoDiskSpace}
		mb := TextBox{SELF, rsTitle:value,rsError:value}
		mb:Type := BUTTONRETRYCANCEL + BOXICONEXCLAMATION
		IF (mb:Show() = BOXREPLYRETRY)
			RETURN TRUE
		ELSE
			RETURN FALSE
		ENDIF
	ELSE
		RETURN FALSE
	ENDIF

METHOD PrinterExpose(oPrinterExposeEvt) 
	

	IF !IsNil(oPrinterExposeEvt) .AND. !IsInstanceOfUsual(oPrinterExposeEvt,#PrinterExposeEvent)
		WCError{#PrinterExpose,#printer,__WCSTypeError,oPrinterExposeEvt,1}:Throw()
	ENDIF
	RETURN TRUE

METHOD Start(oRange) 
	LOCAL iPageNum AS INT
	LOCAL lAnotherPage AS LOGIC
	LOCAL PEE AS PrinterExposeEvent
	iPageNum := 1
	lAnotherPage := TRUE

	SELF:BeginDoc()

	IF !IsNil(oRange)
		IF !IsInstanceOfUsual(oRange, #Range)
			WCError{#Start,#Printer,__WCSTypeError,oRange,1}:Throw()
		ENDIF           
		iPageNum := oRange:Min
	ENDIF

	PEE := PrinterExposeEvent{NULL_PTR, 0, iPageNum, 0, SELF:CanvasArea}

	DO WHILE ((oRange == NULL_OBJECT .OR. iPageNum <= oRange:Max) .AND. lAnotherPage .AND. SELF:IsValid() .AND. !SELF:Aborted())
		PEE:wParam := DWORD(iPageNum)

		lAnotherPage := SELF:PrinterExpose(PEE)

		iPageNum++
		IF (lAnotherPage .AND. (oRange == NULL_OBJECT .OR. iPageNum <= oRange:Max))
			SELF:NewPage()
		ENDIF
	ENDDO

	RETURN SELF

ACCESS WindowArea 
	

	RETURN BoundingBox{Point{0, 0}, Dimension{iWidth, iHeight}}

END CLASS

#ifdef __VULCAN__
   DELEGATE __PrinterAbortProcDelegate( hDCPrinter AS PTR, _Code AS INT ) AS LOGIC
#endif  

FUNCTION __PrinterAbortProc(hDCPrinter AS PTR, _Code AS INT) AS LOGIC /* WINCALL */
	LOCAL oPri AS Printer

	oPri :=__WCGetPrinterFromArray(hDCPrinter)
	IF oPri != NULL_OBJECT
		IF !opri:lprAbort
			oPri:Idle()
		ENDIF
		RETURN !(opri:lprAbort)
	ELSEIF (oApp != NULL_OBJECT)
		oApp:Exec(ExecWhileEvent)
		RETURN TRUE
	ENDIF

	RETURN FALSE

STATIC GLOBAL aPrinterhDCPrinter AS ARRAY

FUNCTION __WCAddPrinterToArray(hDCPrinter AS PTR, oPrinter AS Printer) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aPrinterhDCPrinter)
	FOR dwI := 1 UPTO dwCount
		IF aPrinterhDCPrinter[dwI, 1] == hDCPrinter
			aPrinterhDCPrinter[dwI,2] := oPrinter
			RETURN
		ENDIF
	NEXT  // dwI

	AAdd(aPrinterhDCPrinter, {hDCPrinter, oPrinter})

	RETURN

FUNCTION __WCDelPrinterFromArray(hDCPrinter AS PTR) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aPrinterhDCPrinter)
	FOR dwI := 1 UPTO dwCount
		IF aPrinterhDCPrinter[dwI, 1] == hDCPrinter
			ADel(aPrinterhDCPrinter,dwI)
			ASize(aPrinterhDCPrinter, dwCount -1)
			RETURN
		ENDIF
	NEXT  // dwI

	RETURN

FUNCTION __WCGetPrinterFromArray(hDCPrinter AS PTR) AS OBJECT
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aPrinterhDCPrinter)
	FOR dwI := 1 UPTO dwCount
		IF aPrinterhDCPrinter[dwI, 1] == hDCPrinter
			RETURN aPrinterhDCPrinter[dwI, 2]
		ENDIF
	NEXT  // dwI

	RETURN NULL_OBJECT

