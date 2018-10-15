CLASS Vulcan.ChildWinForm INHERIT ChildAppWindow
    PRIVATE form AS System.Windows.Forms.Form
    PRIVATE currentControl AS System.Windows.Forms.Control
    
    CONSTRUCTOR(owner,managed) 
        SUPER(owner,managed)

    ASSIGN WinForm(newForm AS System.Windows.Forms.Form) 
        form := newForm

        // Set the Windows styles of the form so that it 
        // can be used as a child of the VO window
        WindowStyle.SetStyle(SELF:form:Handle,WS_CHILDWINDOW,TRUE)
        WindowStyle.SetStyle(SELF:form:Handle,WS_CAPTION,FALSE)
        WindowStyle.SetStyle(SELF:form:Handle,WS_SYSMENU,FALSE)
        WindowStyle.SetStyle(SELF:form:Handle,DS_MODALFRAME,FALSE)
        WindowStyle.SetStyle(SELF:form:Handle,WS_THICKFRAME,FALSE)
        WindowStyle.SetStyle(SELF:form:Handle,WS_MINIMIZEBOX,FALSE)
        WindowStyle.SetStyle(SELF:form:Handle,WS_MAXIMIZEBOX,FALSE)
        WindowStyle.SetExStyle(SELF:form:Handle,WS_EX_APPWINDOW,FALSE)
        WindowStyle.SetExStyle(SELF:form:Handle,WS_EX_CONTROLPARENT,FALSE)

        // Put the form into the VO window
        SetParent(form:Handle,SELF:Handle())
        form:Show()

        SetWindowPos(form:Handle,HWND_TOP,0,0,0,0,_OR(SWP_NOACTIVATE,SWP_NOSIZE))

        // Size the form to fit in the VO window
	    LOCAL oldOrigin AS Point
	    LOCAL oldSize AS Dimension
	    oldOrigin := SELF:origin
	    oldSize := SELF:size

	    SELF:size := Dimension{form:ClientSize:Width+SELF:size:width-SELF:canvasarea:width,form:ClientSize:Height+SELF:size:height-SELF:canvasarea:height}
	    SELF:Origin := Point{oldOrigin:x,oldOrigin:y+oldSize:height-SELF:size:Height}

        form:Show()

        SELF:form:Width := SELF:canvasarea:width
        SELF:form:Height := SELF:canvasarea:Height

        // The application has a list of forms that are being hosted
        // Add this form to the list; it will be removed on close
        LOCAL oApp AS VOWinFormApp
        oApp := (VOWinFormApp)GetAppObject()
        oApp:RegisterWinForm(form)
        
        RETURN
        
    ACCESS WinForm AS System.Windows.Forms.Form
        RETURN form
        
 
    METHOD Resize(o)
        SUPER:Resize(o)
        
        IF SELF:IsVisible() .and. ! SELF:form == NULL
            SELF:form:Width := SELF:canvasarea:width
            SELF:form:Height := SELF:canvasarea:Height
        ENDIF
        
        RETURN NIL
        
    METHOD Close(o)
        LOCAL oApp AS VOWinFormApp
        oApp := (VOWinFormApp)GetAppObject()
        oApp:UnRegisterWinForm(form)

        SELF:form:Close()
        SUPER:Close(o)
        
        RETURN NIL
        
    METHOD Activate(o)
        LOCAL u AS USUAL
        
        u := SUPER:Activate(o)
        
        SELF:ActivateWinForm()

        RETURN u
        
    PROTECT METHOD ActivateWinForm() STRICT
        PostMessage(SELF:form:Handle,WM_SETFOCUS,0,0)

        IF SELF:form:Controls:Count > 0
            
	        IF currentControl == NULL
                currentControl := SELF:form:Controls[SELF:form:Controls:Count-1]
	        ENDIF

            SELF:form:ActiveControl := currentControl
            
        ENDIF

        RETURN NIL

	METHOD DeActivate(o)
	    LOCAL u AS USUAL

        SELF:DeActivateWinForm()
                
	    u := SUPER:DeActivate(o)
	    
	    RETURN u
	    
	PROTECT METHOD DeActivateWinForm() STRICT

        IF SELF:form:Controls:Count > 0
            currentControl := SELF:form:ActiveControl
        ENDIF

	    RETURN NIL

END CLASS
