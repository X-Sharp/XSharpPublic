


#USING System.Windows.Forms

CLASS TopAppWindow INHERIT AppWindow

	METHOD __CreateForm() AS VOForm STRICT
		RETURN GuiFactory.Instance:CreateTopAppWindow(SELF)

	METHOD __ResizeChild() AS TopAppWindow STRICT 
		IF SELF:__IsValid .and. oWnd:MdiChildren:Length > 0
			LOCAL oChild AS Form
			oChild :=  oWnd:MdiChildren[1]
			oChild:Location := System.Drawing.Point{0,0}
			oChild:Size := oWnd:ClientSize
		ENDIF
		RETURN SELF



	CONSTRUCTOR(oOwner) 
		SUPER(oOwner)
		
		SELF:EnableSystemMenu()
		SELF:EnableBorder()
		SELF:EnableMinBox()
		SELF:EnableMaxBox()
		IF oApp != NULL_OBJECT
			oApp:__WindowCount += 1
		ENDIF

		RETURN 
	
	METHOD Destroy() AS USUAL CLIPPER
		SUPER:Destroy()
		// Tests if this is the last TopAppWindow
		IF (oApp != NULL_OBJECT)
			oApp:__WindowCount := oApp:__WindowCount - 1
			IF (oApp:__WindowCount <= 0)
				oApp:Quit()
			ENDIF
		ENDIF
		RETURN SELF

	METHOD Resize(oResizeEvent) 
		SUPER:Resize(oResizeEvent)
		SELF:__ResizeChild()
		RETURN SELF

	METHOD ToolBarHeightChanged(oEvent) 
		SELF:__ResizeChild()
		RETURN SELF
END CLASS

#region defines
DEFINE __WCTopAppWindowClass := "TopAppWindow"
#endregion
