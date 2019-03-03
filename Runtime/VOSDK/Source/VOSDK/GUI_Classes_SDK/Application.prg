CLASS App INHERIT VObject
	PROTECT oParent AS OBJECT
	PROTECT liWindowCount AS LONGINT
	PROTECT hAccelWnd AS PTR
	PROTECT hAccel AS PTR
	PROTECT hDialogWnd AS PTR
	PROTECT hHelpWnd AS PTR
	PROTECT hHelpAccel AS PTR
	PROTECT hHelpCursor AS PTR
	PROTECT hMdiClientWnd AS PTR
	PROTECT lBeforeDisp AS LOGIC
	PROTECT lAfterDisp AS LOGIC

	//PP-030828 Strong typing
	ACCESS __HelpCursor() AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hRet AS PTR

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
#else
   EnterCriticalSection(@__WCCSApp)
#endif   
   hRet := hHelpCursor
#ifdef __VULCAN__
   END LOCK
#else   
   LeaveCriticalSection(@__WCCSApp)
#endif

	RETURN hRet

ACCESS __HelpWndHandle AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hRet AS PTR

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
#else
   EnterCriticalSection(@__WCCSApp)
#endif   
   hRet := hHelpWnd
#ifdef __VULCAN__
   END LOCK
#else   
   LeaveCriticalSection(@__WCCSApp)
#endif
	RETURN hRet

METHOD __SetHelpWind(hHandle AS PTR, wMode AS LONGINT) AS App STRICT 
	//PP-030828 Strong typing

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
#else
   EnterCriticalSection(@__WCCSApp)
#endif   

	hHelpWnd := hHandle

	DO CASE
	CASE (wMode == HM_GENERAL)
		hHelpAccel := LoadAccelerators(_GetInst(), String2Psz("GeneralHelp"))
	CASE (wMode == HM_MOUSE)
		hHelpAccel := LoadAccelerators(_GetInst(), String2Psz("CursorHelp"))
	OTHERWISE
		hHelpAccel := NULL_PTR
	ENDCASE

	IF (hHelpCursor == 0) //Load help cursor first time through
		hHelpCursor := LoadCursor(_GetInst(), String2Psz("HelpCursor"))
	ENDIF

#ifdef __VULCAN__
   END LOCK
#else   
   LeaveCriticalSection(@__WCCSApp)
#endif

	RETURN SELF

ACCESS __WindowCount AS LONGINT STRICT 
	//PP-030828 Strong typing
	LOCAL lRet AS LONGINT

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
#else
   EnterCriticalSection(@__WCCSApp)
#endif   
   lRet := liWindowCount
#ifdef __VULCAN__
   END LOCK
#else   
   LeaveCriticalSection(@__WCCSApp)
#endif

	RETURN lRet

ASSIGN __WindowCount(nValue AS LONGINT)  STRICT 
	//PP-030828 Strong typing

#ifdef __VULCAN__
   BEGIN LOCK __WCCSApp
#else
   EnterCriticalSection(@__WCCSApp)
#endif   
   liWindowCount := nValue
#ifdef __VULCAN__
   END LOCK
#else   
   LeaveCriticalSection(@__WCCSApp)
#endif

	RETURN 

METHOD Exec(kExecType, oObject) 
	// The Exec() method is designed to be used in three ways. One, as the
	// central event loop of the application. Two, a way of yeilding control
	// to Windows during an intensive CPU operation. Three, a nested loop
	// used by the DialogWindow class to mimic the behaviour of a modal dialog.
	LOCAL msg IS _winMSG
	LOCAL lObject AS LOGIC
	LOCAL retVal AS LOGIC
	LOCAL o,w AS OBJECT
	LOCAL DIM KbState[256] AS BYTE
	LOCAL lResetKB AS LOGIC
	LOCAL lTranslated AS LOGIC	//RvdH 050426


	Default(@kExecType, EXECNORMAL)

	lObject := !IsNil(oObject)

	DO WHILE !lObject .OR. oObject:Active()
		IF (KExecType == EXECNORMAL)
			retVal := GetMessage(@msg, 0, 0, 0)
		ELSE
			retVal := PeekMessage(@msg, 0, 0, 0, PM_REMOVE)
			IF retVal .AND. msg:message == WM_QUIT
				//EXIT
				retVal := FALSE //RvdH 070314 WM_QUIT needs to go through BeforeDispatch as well
			ENDIF
		ENDIF

		// RvdH 070314 Make sure WM_QUIT also goes to BeforeDispatch
		// IF !retVal
		// 	EXIT
		// ENDIF

		IF lBeforeDisp
			IF Send(SELF, #BeforeDispatch, msg:hwnd, msg:message, msg:wParam, msg:lParam) == FALSE
				IF retVal	// When processing WM_QUIT we want to EXIT below
					LOOP
				ENDIF
			ENDIF
		ENDIF

		IF !retVal
			EXIT
		ENDIF

		IF (hMdiClientWnd == NULL_PTR) .OR. (lObject) .OR. !TranslateMDISysAccel(hMDIClientWnd, @msg)
#ifndef __VULCAN__
         lTranslated := _VOOLETranslateMsg(@msg)
#else
         lTranslated := FALSE
#endif                  
			IF  !lTranslated
				IF (hHelpAccel == NULL_PTR) .OR. (TranslateAccelerator(hHelpWnd, hHelpAccel, @msg) == 0)
					IF (hAccel == NULL_PTR) .OR. (TranslateAccelerator(hAccelWnd, hAccel, @msg) == 0)
						IF (msg:message == WM_KEYDOWN) .AND. ;
								(IsInstanceOf((o := __WCGetWindowByHandle(GetParent(msg:hwnd))), #DialogWindow) .OR.;
								IsInstanceOf((o := __WCGetWindowByHandle(GetParent(GetParent(msg:hwnd)))), #DialogWindow))
							IF ((msg:wParam == VK_RETURN) .OR. (msg:wParam == VK_UP) .OR. (msg:wParam == VK_DOWN));
									.AND. o:ClipperKeys .AND. !IsInstanceOf(w := __WCGetObjectByHandle(msg:hWnd), #PushButton) .AND. ;
									!IsInstanceOf(w, #MultiLineEdit)
								IF (msg:wParam == VK_UP)
									GetKeyboardState(@KbState)
									KbState[VK_SHIFT+1] := _OR(KbState[VK_SHIFT+1], 0x80)
									SetKeyboardState(@KbState)
									lResetKB := TRUE
								ENDIF
								msg:wParam := VK_TAB
							ENDIF
							IsDialogMessage(o:Handle(), @msg)
							IF (lResetKB)
								lResetKB := FALSE
								KbState[VK_SHIFT+1] := _AND(KbState[VK_SHIFT+1], 0x7F)
								SetKeyboardState(@KbState)
							ENDIF
						ELSEIF (hDialogWnd == NULL_PTR) .OR. !IsDialogMessage(hDialogWnd, @msg)
							TranslateMessage(@msg)
							DispatchMessage(@msg)
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		//RvdH 050331 Moved from below
		IF lAfterDisp
			Send(SELF, #AfterDispatch, msg:hwnd, msg:message, msg:wParam, msg:lParam)
		ENDIF

	ENDDO
	//RvdH 050331 Moved inside DO WHILE
	//	IF lAfterDisp
	//		Send(SELF, #AfterDispatch, msg.hwnd, msg.message, msg.wParam, msg.lParam)
	//	ENDIF



	//RvdH 070314 PostQuitMessage should always happen and return 1 (below) as well
	IF /*(KExecType == ExecNormal) .and. */ lObject .AND. (msg:message == WM_QUIT)
		PostQuitMessage(0)
	ENDIF

	IF (msg:message != WM_QUIT)  // .AND. (KExecType == EXECNORMAL)
		RETURN 1
	ENDIF

	RETURN 0

METHOD GetAccel()  AS PTR
   LOCAL hCurrentAccel		AS PTR
	// DHer: 18/12/2008
	hCurrentAccel := SELF:hAccel

RETURN hCurrentAccel

METHOD GetAccelWindow()  AS PTR
   LOCAL hCurrentAccelWnd		AS PTR
   // DHer: 18/12/2008
   hCurrentAccelWnd := SELF:hAccelWnd
   RETURN hCurrentAccelWnd

METHOD GetMdiClientWindow()  AS PTR
   LOCAL hCurrentMdiClientWnd		AS PTR
	// DHer: 18/12/2008
	hCurrentMdiClientWnd := SELF:hMdiClientWnd
   RETURN hCurrentMdiClientWnd

METHOD GetDialogWindow() AS PTR
	RETURN hDialogWnd

METHOD Handle() AS PTR

	RETURN _GetInst()

CONSTRUCTOR(oOwner) 

	SUPER()

	IF !IsNil(oOwner)
		oParent := oOwner
	ENDIF

   // dcaton 070329 this is already done in __WCInitCriticalSections
   //InitializeCriticalSection(@__WCCSApp)

	lBeforeDisp := IsMethod(SELF, #BeforeDispatch)
	lAfterDisp := IsMethod(SELF, #AfterDispatch)
   #ifdef __VULCAN__
      // Set GLOBAL oApp since the application doesn't
      // have access to it
      oApp := SELF
   #endif

	RETURN 

METHOD Quit() 

	PostQuitMessage(0)

	RETURN NIL

METHOD Run(sCommand) 

	RETURN WinExec(String2Psz(sCommand), SW_SHOWNORMAL)

METHOD SetAccel(hNewAccel) 
   BEGIN LOCK __WCCSApp
      hAccel := hNewAccel
   END LOCK

	RETURN NIL

METHOD SetAccelWindow(hNewAccelWnd) 

   BEGIN LOCK __WCCSApp
      hAccelWnd := hNewAccelWnd
   END LOCK

	RETURN NIL

METHOD SetDialogWindow(hNewDialogWnd) 

   BEGIN LOCK __WCCSApp
      hDialogWnd := hNewDialogWnd
   END LOCK

	RETURN NIL

METHOD SetMdiClientWindow(hNewMdiClientWnd) 

   BEGIN LOCK __WCCSApp
      hMdiClientWnd := hNewMdiClientWnd
   END LOCK

	RETURN NIL
END CLASS

GLOBAL gatomVOObjPtr AS DWORD
GLOBAL gdwDragListMsg AS DWORD
GLOBAL glCAPaintInit := FALSE AS LOGIC

GLOBAL gpfnInitCommonControlsEx AS InitCommonControlsEx PTR
GLOBAL gsymBrowserDef AS SYMBOL

PROCEDURE __InitFunctionPointer() _INIT3
	LOCAL icex IS _winINITCOMMONCONTROLSEX

	//SE-070411
	LOCAL hModule AS PTR
	hModule := GetModuleHandle(String2Psz("COMCTL32.DLL"))
	IF hModule == NULL_PTR
		hModule := LoadLibrary(String2Psz("COMCTL32.DLL"))
	ENDIF
	gpfnInitCommonControlsEx := GetProcAddress(hModule, String2Psz("InitCommonControlsEx"))

	IF (gpfnInitCommonControlsEx != NULL_PTR)
		icex:dwSize := _SIZEOF(_winINITCOMMONCONTROLSEX)
		icex:dwICC := _OR(ICC_WIN95_CLASSES, ICC_DATE_CLASSES, ICC_USEREX_CLASSES, ICC_COOL_CLASSES, ICC_INTERNET_CLASSES, ICC_LINK_CLASS)
		IF !PCALL(gpfnInitCommonControlsEx, @icex)
			icex:dwICC := _AND(icex:dwICC, DWORD(_NOT(ICC_LINK_CLASS)))
			IF !PCALL(gpfnInitCommonControlsEx, @icex)
				icex:dwICC := _AND(icex:dwICC, _NOT(ICC_INTERNET_CLASSES))
				IF !PCALL(gpfnInitCommonControlsEx, @icex)
					gpfnInitCommonControlsEx := NULL_PTR
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	IF (gpfnInitCommonControlsEx == NULL_PTR)
		InitCommonControls()                                    
	ENDIF

	gdwDragListMsg := RegisterWindowMessage(String2Psz(DRAGLISTMSGSTRING))
	gatomVOObjPtr 	:= GlobalAddAtom(String2Psz("__VOObjPtr"))
	gsymBrowserDef := IIF(File("CATO3CNT.DLL"), #DataBrowser, #DataListView)
	RETURN

GLOBAL oApp AS App

FUNCTION ApplicationExec(kExecType)
	//For 1.0 compatibility

	IF (oApp != NULL_OBJECT)
		oApp:Exec(kExecType)
	ENDIF

	RETURN NIL

FUNCTION InitCommonControlsEx(lpicex AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

FUNCTION SetAccelerator(hWnd AS PTR, hAccel AS PTR) AS LOGIC STRICT

	IF (oApp != NULL_OBJECT)
		oApp:SetAccelWindow(hWnd)
		oApp:SetAccel(hAccel)
	ENDIF

	RETURN TRUE

#ifndef __VULCAN__
FUNCTION Start()

	oApp := App{}

	IF IsMethod(oApp, #Start)
		Send(oApp, #Start)
		// RvdH-030323 Report message when start method is missing
	ELSE
		TextBox{,"Visual Objects","App:Start method missing", BOXICONEXCLAMATION}:Show()
	ENDIF

	WCDCClear()

	RETURN NIL
#endif
