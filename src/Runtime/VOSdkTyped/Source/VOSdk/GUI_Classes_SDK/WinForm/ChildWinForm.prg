//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// ChildWinForm.prg


CLASS ChildWinForm INHERIT ChildAppWindow
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
		//SetParent(form:Handle,SELF:Handle())
		form:Parent := SELF:__Form
		form:Show()

		//SetWindowPos(form:Handle,HWND_TOP,0,0,0,0,_OR(SWP_NOACTIVATE,SWP_NOSIZE))
		form:Location := System.Drawing.Point{0,0}

		// Size the form to fit in the VO window
		LOCAL oldOrigin AS Point
		LOCAL oldSize AS Dimension
		oldOrigin := SELF:Origin
		oldSize := SELF:Size

		SELF:Size := Dimension{form:ClientSize:Width+self:Size:Width-self:CanvasArea:Width,form:ClientSize:Height+self:Size:Height-self:CanvasArea:Height}
		SELF:Origin := Point{oldOrigin:X,oldOrigin:Y+oldSize:Height-SELF:Size:Height}

		form:Show()

		SELF:form:Width := SELF:CanvasArea:Width
		SELF:form:Height := SELF:CanvasArea:Height

		// The application has a list of forms that are being hosted
		// Add this form to the list; it will be removed on close
		LOCAL oApp AS VOWinFormApp
		oApp := (VOWinFormApp)GetAppObject()
		oApp:RegisterWinForm(form)

		RETURN

	ACCESS WinForm AS System.Windows.Forms.Form
		RETURN form


	METHOD Resize(o  as ResizeEvent) AS USUAL
		SUPER:Resize(o)

		IF SELF:IsVisible() .and. ! SELF:form == NULL
			SELF:form:Width := SELF:CanvasArea:Width
			SELF:form:Height := SELF:CanvasArea:Height
		ENDIF

		RETURN SELF

	METHOD Close(o as Event)  as USUAL
		LOCAL oApp AS VOWinFormApp
		oApp := (VOWinFormApp)GetAppObject()
		oApp:UnRegisterWinForm(form)

		SELF:form:Close()
		SUPER:Close(o)

		RETURN SELF

	METHOD Activate(o  AS Event) as USUAL
		VAR res := SUPER:Activate(o)
        SELF:ActivateWinForm()
        RETURN res


	PROTECT METHOD ActivateWinForm() AS VOID STRICT
		//Todo ActivateWinForm
// 		PostMessage(self:form:Handle,WM_SETFOCUS,0,0)
// 
// 		if self:form:Controls:Count > 0
// 
// 		if currentControl == null
// 			currentControl := self:form:Controls[self:form:Controls:Count-1]
// 		endif
// 
// 		self:form:ActiveControl := currentControl
// 
// 		endif

		RETURN

	METHOD DeActivate(o  AS Event) AS USUAL
		SELF:DeActivateWinForm()
		RETURN SUPER:DeActivate(o)

	PROTECT METHOD DeActivateWinForm() AS VOID STRICT

		IF SELF:form:Controls:Count > 0
			currentControl := SELF:form:ActiveControl
		ENDIF

		RETURN

END CLASS
