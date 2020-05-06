// AppWinForms.prg


#USING System.Windows.Forms

CLASS VOAppForm INHERIT VOForm

	CONSTRUCTOR(oWindow AS Window)
		SUPER(oWindow)
		SELF:Text := "AppForm"
	
	METHOD __ResizeChild() AS VOID STRICT
		IF SELF:Window != NULL_OBJECT
			SELF:Window:Resize(ResizeEvent{})
		ENDIF
		RETURN
	
	METHOD EnableHorizontalScroll(lEnable AS LOGIC) AS VOID
		SELF:HScroll := lEnable
		RETURN 

	METHOD EnableVerticalScroll(lEnable AS LOGIC) AS VOID
		SELF:VScroll := lEnable
		RETURN 

END CLASS

CLASS VOChildAppForm INHERIT VOAppForm
	CONSTRUCTOR(oWindow AS Window, oOwner AS Form)
		SUPER(oWindow)
		IF oOwner != NULL .and. oOwner:IsMdiContainer
			SELF:MdiParent := oOwner
		ENDIF
		SELF:Text := "ChildAppForm"
END CLASS

CLASS VOTopAppForm INHERIT VOAppForm
	CONSTRUCTOR(oWindow AS Window)
		SUPER(oWindow)
		SELF:IsMdiContainer := TRUE
		SELF:Text := "TopAppForm"
END CLASS

CLASS VOShellForm INHERIT VOAppForm
	CONSTRUCTOR(oWindow AS Window)
		SUPER(oWindow)
		SELF:IsMdiContainer := TRUE
		SELF:Text := "ShellForm"

END CLASS

