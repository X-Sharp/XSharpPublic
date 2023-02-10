



USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
/// <include file="Gui.xml" path="doc/App/*" />
CLASS App INHERIT VObject
    PROTECT liWindowCount AS LONGINT
    PROTECT oDialogWnd AS VOPanel
    PROTECT oHelpWnd AS VOForm
    PROTECT hHelpAccel AS IntPtr
    PROTECT hHelpCursor AS IntPtr
    PROTECT oMsgFilter as MessageFilter

    ACCESS __HelpCursor() AS IntPtr STRICT
        LOCAL hRet AS IntPtr
        BEGIN LOCK WC.CSApp
            hRet := hHelpCursor
        END LOCK
        RETURN hRet

    /// <exclude />
    ACCESS __HelpWndHandle AS VOForm STRICT
        LOCAL hRet AS VOForm
        BEGIN LOCK WC.CSApp
            hRet := oHelpWnd
        END LOCK
        RETURN hRet

    /// <exclude />
    METHOD __SetHelpWind(hHandle AS VOForm, wMode AS LONGINT) AS App STRICT

        BEGIN LOCK WC.CSApp

            oHelpWnd := hHandle

            DO CASE
            CASE (wMode == HM_GENERAL)
                //Todo __SetHelpWind
                //hHelpAccel := LoadAccelerators(_GetInst(), String2Psz("GeneralHelp"))
            CASE (wMode == HM_MOUSE)
                //Todo __SetHelpWind
                //hHelpAccel := LoadAccelerators(_GetInst(), String2Psz("CursorHelp"))
            OTHERWISE
                hHelpAccel := NULL_PTR
            ENDCASE

            IF (hHelpCursor == NULL) //Load help cursor first time through
                //Todo __SetHelpWind
                //hHelpCursor := LoadCursor(_GetInst(), String2Psz("HelpCursor"))
            ENDIF

        END LOCK

        RETURN SELF

    /// <exclude />
    ACCESS __WindowCount AS LONGINT STRICT
        LOCAL lRet AS LONGINT
        BEGIN LOCK WC.CSApp
            lRet := liWindowCount
        END LOCK
        RETURN lRet

    /// <exclude />
    ASSIGN __WindowCount(nValue AS LONGINT)  STRICT
        BEGIN LOCK WC.CSApp
            liWindowCount := nValue
        END LOCK
        RETURN

    /// <include file="Gui.xml" path="doc/App.Exec/*" />
    METHOD Exec() AS LONG STRICT
        RETURN SELF:Exec(EXECNORMAL)
    /// <include file="Gui.xml" path="doc/App.Exec/*" />
    METHOD Exec(kExecType AS LONG) AS LONG
        IF kExecType == EXECWHILEEVENT
            Application.DoEvents()
        ELSE
            Application.Run()
        ENDIF
        RETURN 0

    /// <include file="Gui.xml" path="doc/App.Exec/*" />
    METHOD Exec(kExecType AS LONG, uObject as OBJECT) AS LONG
        IF kExecType == EXECWHILEEVENT
            Application.DoEvents()
        ELSE
            Application.Run()
        ENDIF
        RETURN 0



    /// <include file="Gui.xml" path="doc/App.Handle/*" />
    METHOD Handle() AS IntPtr STRICT
        RETURN _GetInst()

    CONSTRUCTOR() STRICT
        //Application.EnableVisualStyles()
        SUPER()
        IF oApp == NULL_OBJECT
            oApp := SELF
        ENDIF
        oMsgFilter := MessageFilter{SELF}
        //		System.Windows.Forms.Application.SetUnhandledExceptionMode(System.Windows.Forms.UnhandledExceptionMode.CatchException,TRUE)
        //		System.Windows.Forms.Application.ThreadException += System.Threading.ThreadExceptionEventHandler{SELF, @OnThreadException()}
        System.Windows.Forms.Application.ApplicationExit += EventHandler{SELF, @OnApplicationExit()}
        //		System.AppDomain.CurrentDomain:UnHandledException += UnhandledExceptionEventHandler {SELF, @OnUnhandledException()}
        RETURN

    METHOD OnThreadException (sender AS OBJECT, t AS System.Threading.ThreadExceptionEventArgs) AS VOID
        Debout32(t:Exception:Message+CRLF)
        RETURN

    METHOD OnApplicationExit(sender AS OBJECT, e AS EventArgs) AS VOID
        Debout32("Application Exit"+CRLF)
        RETURN

    METHOD OnUnhandledException(sender AS OBJECT, e AS UnhandledExceptionEventArgs ) AS VOID
        Debout32(e:ToString())
        RETURN


    /// <include file="Gui.xml" path="doc/App.Quit/*" />
    METHOD Quit()
        Application.Exit()
        RETURN NIL

    /// <include file="Gui.xml" path="doc/App.Run/*" />
    METHOD Run(sCommand)
        GuiWin32.WinExec(sCommand, SW_SHOWNORMAL)
        RETURN NIL

    METHOD BeforeDispatch(hWnd AS IntPtr, uMsg AS DWORD, wParam AS DWORD, lParam AS LONG) AS LOGIC STRICT
        RETURN TRUE

        [Obsolete];
    METHOD AfterDispatch(hWnd AS IntPtr, uMsg AS DWORD, wParam AS DWORD, lParam AS LONG) AS LOGIC STRICT
        RETURN TRUE


    METHOD GetDialogWindow() AS VOPanel STRICT
        RETURN oDialogWnd

    METHOD SetDialogWindow(oSurface AS OBJECT) AS VOID
        IF oSurface == NULL_OBJECT
            SELF:oDialogWnd := NULL_OBJECT
        ELSEIF oSurface IS VOPanel
            SELF:oDialogWnd := oSurface
        ENDIF
        RETURN

#region Obsolete Methods
        [Obsolete];
    METHOD GetAccel()  AS IntPtr STRICT
        RETURN IntPtr.Zero

        [Obsolete];
    METHOD GetAccelWindow()  AS IntPtr STRICT
        RETURN IntPtr.Zero

        [Obsolete];
    METHOD GetMdiClientWindow()  AS IntPtr STRICT
        RETURN IntPtr.Zero


        [Obsolete];
    METHOD SetAccel(hNewAccel AS IntPtr) AS VOID
        RETURN

        [Obsolete];
    METHOD SetAccelWindow(hNewAccelWnd AS IntPtr) AS VOID
        RETURN


        [Obsolete];
    METHOD SetMdiClientWindow(hNewMdiClientWnd AS IntPtr) AS VOID
        RETURN


#endregion


END CLASS

GLOBAL glCAPaintInit := FALSE AS LOGIC

    //GLOBAL gpfnInitCommonControlsEx AS InitCommonControlsEx PTR
GLOBAL gsymBrowserDef AS SYMBOL

PROCEDURE __InitFunctionPointer() _INIT3
    gsymBrowserDef := #DataBrowser
    RETURN

GLOBAL oApp AS App

/// <include file="Gui.xml" path="doc/ApplicationExec/*" />
FUNCTION ApplicationExec(kExecType)
    IF (oApp != NULL_OBJECT)
        oApp:Exec(kExecType)
    ENDIF

    RETURN NIL

/// <exclude/>
[Obsolete];
FUNCTION SetAccelerator(hWnd AS IntPtr, hAccel AS IntPtr) AS LOGIC STRICT
    RETURN TRUE


CLASS MessageFilter IMPLEMENTS System.Windows.Forms.IMessageFilter
    PROTECT oApp AS VOSDK.App
    CONSTRUCTOR (oA AS VOSDK.App)
        SELF:oApp := oA
        System.Windows.Forms.Application.AddMessageFilter(SELF)


    METHOD PreFilterMessage(m REF System.Windows.Forms.Message) AS LOGIC
        RETURN ! oApp:BeforeDispatch(m:HWnd, (DWORD) m:Msg, (DWORD) m:wParam, (INT) m:lParam)

END CLASS
