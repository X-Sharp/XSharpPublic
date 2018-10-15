CLASS Vulcan.WinFormVOWindow INHERIT System.Windows.Forms.Form
    PRIVATE winFormVOWindowHost AS Vulcan.WinFormVOWindowHost
    PROTECT currentFocusHandle AS PTR
    STATIC PROTECT oApp AS App
    
    STATIC METHOD Initialize() AS VOID
        // We need an instance of VO's app so that
        // it can handle the messages for modal windows
        IF oApp == NULL
    	    oApp := App{}
    	ENDIF
    	RETURN
    	
    STATIC METHOD CloseHostForm(window AS Window) AS VOID
        // This method receives a VO window
        // The VO window is normally hosted on a control, which in turn is on a form
        // So we find the VO window's parent - the host control
        // Then find the host control's parent - the form
        // Then tell the form to close
        LOCAL control AS System.Windows.Forms.Control
        LOCAL hWnd AS IntPtr
        
        hWnd := GetParent(window:Handle())
        control := System.Windows.Forms.Form.FromHandle(hWnd)
        
        IF ! control == NULL
            LOCAL form AS WinFormVOWindow
            form := (WinFormVOWindow)control:Parent
            IF ! form == NULL
                form:Close()
            ENDIF
        ENDIF

        RETURN

    ACCESS VOWindowHost AS WinFormVOWindowHost
        RETURN winFormVOWindowHost
        
    ASSIGN VOWindowHost(@@new AS Vulcan.WinFormVOWindowHost)
        winFormVOWindowHost := @@new
        RETURN
        
    PROTECTED METHOD OnActivated(e AS System.EventArgs) AS VOID
        SUPER:OnActivated(e)
        SELF:winFormVOWindowHost:Focus()
        IF ! currentFocusHandle == NULL
            SetFocus(currentFocusHandle)
        ENDIF
        
        RETURN

    PROTECTED METHOD OnClosed(e AS System.EventArgs) AS VOID
        IF ! SELF:winFormVOWindowHost == NULL
            SELF:winFormVOWindowHost:Close()
        ENDIF
        SUPER:OnClosed(e)
        RETURN

    PROTECTED METHOD OnDeactivate(e AS System.EventArgs) AS VOID
        currentFocusHandle := GetFocus()
        SUPER:OnDeactivate(e)
        RETURN

    PROTECTED METHOD OnLayout(e AS System.Windows.Forms.LayoutEventArgs) AS VOID
        SUPER:OnLayout(e)
        IF ! SELF:winFormVOWindowHost == NULL
            SELF:winFormVOWindowHost:AdjustVOWindow()
        ENDIF
        RETURN

    PROTECTED METHOD OnLoad(e AS System.EventArgs) AS VOID
        SUPER:OnLoad(e)
        IF ! SELF:winFormVOWindowHost == NULL
            SELF:winFormVOWindowHost:VOWindow:Show()
            SELF:winFormVOWindowHost:Focus()
        ENDIF
        RETURN
        
    PROTECTED METHOD OnResizeEnd(e AS System.EventArgs) AS VOID
        SUPER:OnResizeEnd(e)
        IF ! SELF:winFormVOWindowHost == NULL
            SELF:winFormVOWindowHost:AdjustVOWindow()
        ENDIF
        RETURN

    PROTECTED VIRTUAL METHOD ProcessDialogKey( keyData AS System.Windows.Forms.Keys ) AS LOGIC
        LOCAL keyProcessed := FALSE AS LOGIC
    
        IF ! SELF:winFormVOWindowHost == NULL
            // see if this control is held by a datawindow or directly the hosted window
            // note: datawindows are made up of multiple windows (varies with view mode), that's why we need to look at parents
            LOCAL isWanted := TRUE AS LOGIC
            LOCAL i AS INT
            LOCAL p AS PTR
            p := GetFocus()
            FOR i := 1 UPTO 3  // in a datawindow in form view, the number of layers above the focused control is 3
                p := GetParent(p)
                IF WindowStyle.ClassName(p) == __WCWndAppWindowClass .or. p == SELF:winFormVOWindowHost:VOWindow:handle()
                    isWanted := TRUE
                    EXIT
                ENDIF
            NEXT

            IF isWanted
                keyProcessed := SELF:winFormVOWindowHost:ProcessDialogKey(keyData)
            ENDIF

        ENDIF
        
        IF ! keyProcessed
            keyProcessed := SUPER:ProcessDialogKey(keyData)
        ENDIF
        
        RETURN keyProcessed

END CLASS
