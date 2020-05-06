
#USING System.Windows.Forms
CLASS ChildAppWindow INHERIT AppWindow
	PROTECT oShell AS ShellWindow

	METHOD __CreateForm() AS VOForm STRICT
		LOCAL oChild AS VOForm
		IF oShell != NULL_OBJECT
			oChild := GuiFactory.Instance:CreateChildAppWindow(SELF, oShell:__Form)
		ELSE
			oChild := GuiFactory.Instance:CreateChildAppWindow(SELF, NULL_OBJECT)
		ENDIF
		RETURN oChild
	
	CONSTRUCTOR(oOwner, lManaged, lImpl) 
		LOCAL lMng AS LOGIC
		IF IsInstanceOfUsual(oOwner,#ShellWindow) // create an MDI child
			oShell := oOwner
		ENDIF

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
			//IF (oImp == NULL_OBJECT)
				IF lMng .AND. IsInstanceOf(oParent,#ShellWindow) // create an MDI child
					SELF:EnableSystemMenu()
					SELF:EnableBorder()
					SELF:EnableMinBox()
					SELF:EnableMaxBox()
				ELSE // create an SDI window or a Child window
					//SE-070904 no clientedge if oParent:Owner of a __FormFrame is a DialogWindow
					//__WindApp{SELF, IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow)}
					//lClientEdge := IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow) .AND. ! IsInstanceOf(oParent:Owner, #DialogWindow)
					//__WindApp{SELF, lClientEdge }
					 //IF lManaged
						//SELF:EnableBorder(WindowNonSizingBorder)
					 //ENDIF
				ENDIF
			//ENDIF
		ENDIF

	ASSIGN Menu(oNewMenu AS XSharp.VO.Menu) 
		LOCAL i as DWORD
		SUPER:Menu := oNewMenu
		if oShell != NULL_OBJECT
			i := 0
			FOREACH oItem as VOMenuItem in oNewMenu:__Menu:MenuItems
				if i == oNewMenu:GetAutoUpdate()
					oItem:MdiList := TRUE
				ENDIF
				++i
				oItem:MergeType := System.Windows.Forms.MenuMerge.Add
			NEXT
		ENDIF
		RETURN 
		
END CLASS

