/// <include file="Gui.xml" path="doc/ChildAppWindow/*" />
CLASS ChildAppWindow INHERIT AppWindow
	PROTECT oImp AS Window


	//PP-030828 Strong typing
 /// <exclude />
	ASSIGN __Imp(oImpObject AS Window)  STRICT
	//PP-030828 Strong typing


	RETURN oImp := oImpObject




/// <include file="Gui.xml" path="doc/ChildAppWindow.ContextMenu/*" />
ASSIGN ContextMenu(oNewMenu)
	//PP-040603 S.Ebert
	IF SELF:oImp != NULL_OBJECT
		SELF:oImp:ContextMenu := oNewMenu
	ENDIF
	SUPER:ContextMenu := oNewMenu
	RETURN




/// <include file="Gui.xml" path="doc/ChildAppWindow.Default/*" />
METHOD Default(oEvent)




	IF (oImp != NULL_OBJECT)
		oImp:Default(oEvent)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/ChildAppWindow.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




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




/// <include file="Gui.xml" path="doc/ChildAppWindow.EnableBorder/*" />
METHOD EnableBorder(kBorderStyle)
	if oImp is AppWindow var appWnd
		appWnd:EnableBorder(kBorderStyle)
	ENDIF
	return nil




/// <include file="Gui.xml" path="doc/ChildAppWindow.EnableHorizontalScroll/*" />
METHOD EnableHorizontalScroll(lEnable)
	if oImp is AppWindow var appWnd
		RETURN appWnd:EnableHorizontalScroll( lEnable)
	ENDIF
	return null_object




/// <include file="Gui.xml" path="doc/ChildAppWindow.EnableStatusBar/*" />
METHOD EnableStatusBar(lEnable)
	if oImp is AppWindow var appWnd
		RETURN appWnd:EnableStatusBar( lEnable)
	ENDIF
	//RvdH 050810 Suggestion from S.Ebert
	//RETURN oStatusBar


	RETURN SELF:StatusBar






/// <include file="Gui.xml" path="doc/ChildAppWindow.EnableVerticalScroll/*" />
METHOD EnableVerticalScroll(lEnable)




	IF oImp IS AppWindow VAR appWnd
		RETURN appWnd:EnableVerticalScroll( lEnable)
	ENDIF


	RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/ChildAppWindow.ctor/*" />
CONSTRUCTOR(oOwner, lManaged, lImpl)
	LOCAL lMng AS LOGIC
   LOCAL lClientEdge   AS LOGIC


	SUPER(oOwner)
	IF !IsNil(lManaged)
		IF !IsLogic(lManaged)
			WCError{#Init,#ChildAppWindow,__WCSTypeError,lManaged,2}:Throw()
		ELSE
			lMng := lManaged
		ENDIF
	ENDIF


	Default(@lImpl, TRUE)


	IF (lImpl)
        IF (oImp == NULL_OBJECT)
            IF lMng .AND. IsInstanceOf(oParent,#ShellWindow) // create an MDI child
                CreateInstance(DefaultDocAppClassname, SELF) //This sets the oImp and hWnd variables
                SELF:EnableSystemMenu()
                SELF:EnableBorder()
                SELF:EnableMinBox()
                SELF:EnableMaxBox()
            ELSE // create an SDI window or a Child window
                //SE-070904 no clientedge if oParent:Owner of a __FormFrame is a DialogWindow
                //__WindApp{SELF, IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow)}
                lClientEdge := IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow) .AND. ! IsInstanceOf(oParent:Owner, #DialogWindow)
                CreateInstance(DefaultWindAppClassName,SELF, lClientEdge )
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




/// <include file="Gui.xml" path="doc/ChildAppWindow.Menu/*" />
ASSIGN Menu(oNewMenu)




	IF (oImp != NULL_OBJECT)
		SUPER:Menu := oNewMenu
		oImp:Menu := oNewMenu
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/ChildAppWindow.Show/*" />
METHOD Show(kShowState)




	IF (oImp != NULL_OBJECT)
		RETURN oImp:Show(kShowState)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/ChildAppWindow.StatusBar/*" />
ACCESS StatusBar




	IF oImp IS AppWindow VAR appWnd
		RETURN appWnd:StatusBar
	ENDIF


	RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/ChildAppWindow.StatusBar/*" />
ASSIGN StatusBar(oNewStatusBar)




	IF oImp IS AppWindow VAR appWnd
        appWnd:StatusBar := oNewStatusBar
	ENDIF


	RETURN






/// <include file="Gui.xml" path="doc/ChildAppWindow.ToolBar/*" />
ACCESS ToolBar




	IF oImp IS AppWindow VAR appWnd
		RETURN appWnd:Toolbar
	ENDIF


	RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/ChildAppWindow.ToolBar/*" />
ASSIGN ToolBar(oNewToolBar)

	IF  ((OBJECT) oNewToolbar)  IS ToolBar VAR oTB
		oTb := oNewToolBar
		oImp:ToolBar := oTb
		oTb:__SetParent(SELF)


	ENDIF


	RETURN


END CLASS
