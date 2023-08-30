//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Windows.Forms

CLASS VOAppForm INHERIT VOForm

	CONSTRUCTOR(oWindow AS Window)
		SUPER(oWindow)
        self:Text := "AppForm"
        SELF:ShowInTaskbar := TRUE

	METHOD __ResizeChild() AS VOID STRICT
		IF SELF:Window != NULL_OBJECT
			SELF:Window:Resize(ResizeEvent{})
		ENDIF
		RETURN

	METHOD EnableHorizontalScroll(lEnable AS LOGIC) AS VOID
		SELF:HScroll := lEnable
		return

	METHOD EnableVerticalScroll(lEnable AS LOGIC) AS VOID
		SELF:VScroll := lEnable
		return

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

