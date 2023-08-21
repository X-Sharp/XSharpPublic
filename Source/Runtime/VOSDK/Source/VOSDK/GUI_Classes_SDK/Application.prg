USING System.Reflection
USING System.Linq
/// <include file="Gui.xml" path="doc/App/*" />
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
    PROTECT oBeforeDisp AS MethodInfo
    PROTECT oAfterDisp  AS MethodInfo


    //PP-030828 Strong typing
 /// <exclude />
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


 /// <exclude />
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


 /// <exclude />
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


 /// <exclude />
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


 /// <exclude />
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
PRIVATE METHOD FindMethod (oObject AS OBJECT, cMethod AS STRING) AS MethodInfo
    cMethod := cMethod:ToUpper()
    RETURN oObject:GetType():GetMethods():Where( {x => x:Name:ToUpper() == cMethod}):FirstOrDefault()

/// <include file="Gui.xml" path="doc/App.Exec/*" />
METHOD Exec(kExecType, uObject)
    // The Exec() method is designed to be used in three ways. One, as the
    // central event loop of the application. Two, a way of yeilding control
    // to Windows during an intensive CPU operation. Three, a nested loop
    // used by the DialogWindow class to mimic the behaviour of a modal dialog.
    LOCAL msg IS _winMSG
    LOCAL lObject AS LOGIC
    LOCAL retVal AS LOGIC
    LOCAL DIM KbState[256] AS BYTE
    LOCAL lResetKB AS LOGIC
    LOCAL lTranslated AS LOGIC	//RvdH 050426
    LOCAL whileCondition AS System.Func<LOGIC>
    LOCAL oActive  AS MethodInfo
    LOCAL oObject  AS OBJECT
    LOCAL lNormalLoop AS LOGIC
    Default(@kExecType, EXECNORMAL)
    lNormalLoop := KExecType == EXECNORMAL
    lObject := IsObject(uObject) .AND. IsMethod(uObject, "ACTIVE")
    IF lObject
        oObject := uObject
        oActive := SELF:FindMethod(oObject,"ACTIVE")
        whileCondition := { => _Send(oObject, oActive)}
    ELSE
        whileCondition := { => TRUE }
    ENDIF


    DO WHILE whileCondition()
        IF lNormalLoop
            retVal := GetMessage(@msg, 0, 0, 0)
        ELSE
            retVal := PeekMessage(@msg, 0, 0, 0, PM_REMOVE)
            IF retVal .AND. msg:message == WM_QUIT
                retVal := FALSE //RvdH 070314 WM_QUIT needs to go through BeforeDispatch as well
            ENDIF
        ENDIF

        IF lBeforeDisp
            IF _Send(SELF, oBeforeDisp, msg:hwnd, msg:message, msg:wParam, msg:lParam ) == FALSE
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
                        LOCAL oDialog AS DialogWindow
                        IF msg:message == WM_KEYDOWN  .AND. __ParentIsDialogWindow(msg:hwnd, OUT oDialog)
                            // Change keydown for Enter, Down or Up ?
                            IF __HandleClipperKeys(msg:wParam, msg:hWnd, oDialog)
                                IF (msg:wParam == VK_UP)
                                    GetKeyboardState(@KbState)
                                    KbState[VK_SHIFT+1] := (BYTE) _OR(KbState[VK_SHIFT+1], 0x80)
                                    SetKeyboardState(@KbState)
                                    lResetKB := TRUE
                                ENDIF
                                // Redirect key to VK_TAB
                                msg:wParam := VK_TAB
                            ENDIF
                            // note IsDialogMessage not only checks but also processes the message
                            IsDialogMessage(oDialog:Handle(), @msg)
                            IF (lResetKB)
                                lResetKB := FALSE
                                KbState[VK_SHIFT+1] := (BYTE) _AND(KbState[VK_SHIFT+1], 0x7F)
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
        IF lAfterDisp
            _Send(SELF, oAfterDisp, msg:hwnd, msg:message, msg:wParam, msg:lParam ,)
        ENDIF
    ENDDO

    //RvdH 070314 PostQuitMessage should always happen and return 1 (below) as well
    IF lObject .AND. (msg:message == WM_QUIT)
        PostQuitMessage(0)
    ENDIF

    IF msg:message == WM_QUIT
        RETURN 0
    ENDIF

    RETURN 1


/// <include file="Gui.xml" path="doc/App.GetAccel/*" />
METHOD GetAccel()  AS PTR
   LOCAL hCurrentAccel		AS PTR
    // DHer: 18/12/2008
    hCurrentAccel := SELF:hAccel


RETURN hCurrentAccel


/// <include file="Gui.xml" path="doc/App.GetAccelWindow/*" />
METHOD GetAccelWindow()  AS PTR
   LOCAL hCurrentAccelWnd		AS PTR
   // DHer: 18/12/2008
   hCurrentAccelWnd := SELF:hAccelWnd
   RETURN hCurrentAccelWnd


/// <include file="Gui.xml" path="doc/App.GetMdiClientWindow/*" />
METHOD GetMdiClientWindow()  AS PTR
   LOCAL hCurrentMdiClientWnd		AS PTR
    // DHer: 18/12/2008
    hCurrentMdiClientWnd := SELF:hMdiClientWnd
   RETURN hCurrentMdiClientWnd


/// <include file="Gui.xml" path="doc/App.GetDialogWindow/*" />
METHOD GetDialogWindow() AS PTR
    RETURN hDialogWnd


/// <include file="Gui.xml" path="doc/App.Handle/*" />
METHOD Handle() AS PTR


    RETURN _GetInst()


/// <include file="Gui.xml" path="doc/App.ctor/*" />
CONSTRUCTOR(oOwner)


    SUPER()


    IF !IsNil(oOwner)
        oParent := oOwner
    ENDIF


   // dcaton 070329 this is already done in __WCInitCriticalSections
   //InitializeCriticalSection(@__WCCSApp)


    oBeforeDisp := SELF:FindMethod(SELF,"BEFOREDISPATCH")
    oAfterDisp  := SELF:FindMethod(SELF,"AFTERDISPATCH")
    lBeforeDisp := oBeforeDisp != NULL_OBJECT
    lAfterDisp := oAfterDisp != NULL_OBJECT
   #ifdef __VULCAN__
      // Set GLOBAL oApp since the application doesn't
      // have access to it
      oApp := SELF
   #endif


    RETURN


/// <include file="Gui.xml" path="doc/App.Quit/*" />
METHOD Quit()


    PostQuitMessage(0)


    RETURN NIL


/// <include file="Gui.xml" path="doc/App.Run/*" />
METHOD Run(sCommand)


	RETURN WinExec(String2Psz(sCommand), SW_SHOWNORMAL)


/// <include file="Gui.xml" path="doc/App.SetAccel/*" />
METHOD SetAccel(hNewAccel)
   BEGIN LOCK __WCCSApp
      hAccel := hNewAccel
   END LOCK


    RETURN NIL


/// <include file="Gui.xml" path="doc/App.SetAccelWindow/*" />
METHOD SetAccelWindow(hNewAccelWnd)


   BEGIN LOCK __WCCSApp
      hAccelWnd := hNewAccelWnd
   END LOCK


    RETURN NIL


/// <include file="Gui.xml" path="doc/App.SetDialogWindow/*" />
METHOD SetDialogWindow(hNewDialogWnd)


   BEGIN LOCK __WCCSApp
      hDialogWnd := hNewDialogWnd
   END LOCK


    RETURN NIL


/// <include file="Gui.xml" path="doc/App.SetMdiClientWindow/*" />
METHOD SetMdiClientWindow(hNewMdiClientWnd)


   BEGIN LOCK __WCCSApp
      hMdiClientWnd := hNewMdiClientWnd
   END LOCK


    RETURN NIL
END CLASS


/// <exclude/>
GLOBAL gatomVOObjPtr AS DWORD
/// <exclude/>
GLOBAL gdwDragListMsg AS DWORD
/// <exclude/>
GLOBAL glCAPaintInit := FALSE AS LOGIC


/// <exclude/>
GLOBAL gpfnInitCommonControlsEx AS InitCommonControlsEx PTR
/// <exclude/>
GLOBAL gsymBrowserDef AS SYMBOL


 /// <exclude />
PROCEDURE __InitFunctionPointer() _INIT3
    LOCAL icex IS _winINITCOMMONCONTROLSEX


    //SE-070411
    LOCAL hModule AS PTR
	hModule := GetModuleHandle(String2Psz("COMCTL32.DLL"))
	IF hModule == NULL_PTR
		hModule := LoadLibrary(String2Psz("COMCTL32.DLL"))
	ENDIF
	IF hModule != NULL_PTR
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
    // else
    // should throw an error here ?
    ENDIF
    RETURN


GLOBAL oApp AS App


/// <include file="Gui.xml" path="doc/ApplicationExec/*" />
FUNCTION ApplicationExec(kExecType)
    //For 1.0 compatibility


    IF (oApp != NULL_OBJECT)
        oApp:Exec(kExecType)
    ENDIF


    RETURN NIL


/// <exclude/>
FUNCTION InitCommonControlsEx(lpicex AS PTR) AS LOGIC STRICT
    //SYSTEM
    RETURN FALSE


/// <exclude/>
FUNCTION SetAccelerator(hWnd AS PTR, hAccel AS PTR) AS LOGIC STRICT

    IF (oApp != NULL_OBJECT)
        oApp:SetAccelWindow(hWnd)
        oApp:SetAccel(hAccel)
    ENDIF


    RETURN TRUE


#ifndef __VULCAN__
/// <include file="Gui.xml" path="doc/Start/*" />
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




INTERNAL FUNCTION __ParentIsDialogWindow(hWnd AS PTR, o OUT DialogWindow) AS LOGIC
    LOCAL hParent AS IntPtr
    LOCAL oTemp AS OBJECT
    hParent := GetParent(hWnd)
    oTemp :=  __WCGetWindowByHandle(hParent)
    IF oTemp IS dialogWindow
        o := (DialogWindow) oTemp
        RETURN TRUE
    ENDIF
    hParent := GetParent(hParent)
    oTemp :=  __WCGetWindowByHandle(hParent)
    IF oTemp IS dialogWindow
        o := (DialogWindow) oTemp
        RETURN TRUE
    ENDIF
    o := NULL
    RETURN FALSE


INTERNAL FUNCTION __HandleClipperKeys( wParam AS DWORD, hWnd AS PTR, o AS DialogWindow) AS LOGIC
    SWITCH wParam
    CASE VK_RETURN
    CASE VK_UP
    CASE VK_DOWN
        IF o:ClipperKeys
            VAR control := __WCGetObjectByHandle(hWnd)
            IF control IS Pushbutton
                RETURN FALSE
            ENDIF
            IF control IS MultiLineEdit
                RETURN FALSE
            ENDIF
            RETURN TRUE
        ENDIF
    END SWITCH
    RETURN FALSE
