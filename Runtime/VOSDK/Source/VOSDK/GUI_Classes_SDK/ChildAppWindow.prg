PARTIAL CLASS ChildAppWindow INHERIT AppWindow
	PROTECT oImp AS Window
	
	//PP-030828 Strong typing
	ASSIGN __Imp(oImpObject AS Window)  STRICT 
	//PP-030828 Strong typing
	
	RETURN oImp := oImpObject
	

ASSIGN ContextMenu(oNewMenu) 
	//PP-040603 S.Ebert
	IF SELF:oImp != NULL_OBJECT
		SELF:oImp:ContextMenu := oNewMenu
	ENDIF
	SUPER:ContextMenu := oNewMenu
	RETURN 
	

METHOD Default(oEvent) 
	
	
	IF (oImp != NULL_OBJECT)
		oImp:Default(oEvent)
	ENDIF
	
	RETURN SELF
	

METHOD Destroy() 
	
	
	IF !InCollect()
		IF (oImp != NULL_OBJECT)
			//oImp will already be destroyed if InCollect()=true because it was created
			//after ChileAppWindow
			oImp:Destroy()
			oImp := NULL_OBJECT
		ENDIF
	ENDIF
	
	SUPER:Destroy()
	
	RETURN SELF
	

METHOD EnableBorder(kBorderStyle) 
	LOCAL oWnd AS AppWindow
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		oWnd := OBJECT(oImp)
		oWnd:EnableBorder(kBorderStyle)
	ENDIF
	
	RETURN NIL
	

METHOD EnableHorizontalScroll(lEnable) 
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		RETURN Send(oImp, #EnableHorizontalScroll, lEnable)
	ENDIF
	
	RETURN NULL_OBJECT
	

METHOD EnableStatusBar(lEnable) 
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		RETURN Send(oImp, #EnableStatusBar, lEnable)
	ENDIF
	//RvdH 050810 Suggestion from S.Ebert
	//RETURN oStatusBar
	
	RETURN SELF:StatusBar
	
	

METHOD EnableVerticalScroll(lEnable) 
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		RETURN Send(oImp, #EnableVerticalScroll, lEnable)
	ENDIF
	
	RETURN NULL_OBJECT
	

CONSTRUCTOR(oOwner, lManaged, lImpl) 
	LOCAL lMng AS LOGIC
   LOCAL lClientEdge   AS LOGIC
	
	SUPER(oOwner)
	IF !IsNil(lManaged)
		IF !IsLogic(lManaged)
			WCError{#Init,#ChildAppWindow,__WCSTypeError,lManaged,2}:@@Throw()
		ELSE
			lMng := lManaged
		ENDIF
	ENDIF
	
	Default(@lImpl, TRUE)
	
	IF (lImpl)
        IF (oImp == NULL_OBJECT)
            IF lMng .AND. IsInstanceOf(oParent,#ShellWindow) // create an MDI child
                __DocApp{SELF} //This sets the oImp and hWnd variables
                SELF:EnableSystemMenu()
                SELF:EnableBorder()
                SELF:EnableMinBox()
                SELF:EnableMaxBox()
            ELSE // create an SDI window or a Child window
                //SE-070904 no clientedge if oParent:Owner of a __FormFrame is a DialogWindow
                //__WindApp{SELF, IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow)}
                lClientEdge := IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow) .AND. ! IsInstanceOf(oParent:Owner, #DialogWindow)
                __WindApp{SELF, lClientEdge }
                // if lManaged
                // self:EnableBorder(WindowNonSizingBorder)
                // endif
			ENDIF
		ENDIF
	ENDIF
	
	IF (oParent != NULL_OBJECT)
		SELF:HelpDisplay := oParent:HelpDisplay
	ENDIF
	
	RETURN 
	

ASSIGN Menu(oNewMenu) 
	
	
	IF (oImp != NULL_OBJECT)
		SUPER:Menu := oNewMenu
		oImp:Menu := oNewMenu
	ENDIF
	
	RETURN 
	

METHOD Show(kShowState) 
	
	
	IF (oImp != NULL_OBJECT)
		RETURN oImp:Show(kShowState)
	ENDIF
	
	RETURN SELF
	

ACCESS StatusBar 
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		RETURN IVarGet(oImp, #StatusBar)
	ENDIF
	
	RETURN NULL_OBJECT
	

ASSIGN StatusBar(oNewStatusBar) 
	
	
	IF IsInstanceOf(oImp, #AppWindow)
		RETURN IVarPut(oImp, #StatusBar, oNewStatusBar)
	ENDIF
	
	RETURN 
	
	

ACCESS ToolBar 
	
	
	IF IsInstanceOf(oImp, #Window)
		RETURN IVarGet(oImp, #Toolbar)
	ENDIF
	
	RETURN NULL_OBJECT
	

ASSIGN ToolBar(oNewToolBar) 
//	LOCAL oRet := NULL_OBJECT AS OBJECT
	LOCAL oTB  AS Toolbar
	
	IF IsInstanceOf(oImp, #Window) .and. IsInstanceOfUsual(oNewToolbar, #ToolBar)
		oTb := oNewToolBar 
		oImp:ToolBar := oTb
		oTb:__SetParent(SELF)
		
	ENDIF
	
	RETURN 
	
END CLASS
