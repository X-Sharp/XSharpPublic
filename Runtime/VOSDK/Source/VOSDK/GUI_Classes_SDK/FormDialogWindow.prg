GLOBAL DefaultFormDialogClassName := #__FormDialogWindow AS SYMBOL
/// <exclude />
CLASS __FormDialogWindow INHERIT ModelessDialog
	PROTECT oSubFormsParent AS __FormDialogWindow //
	//PP-030627
	EXPORT symFormDialog AS SYMBOL


 /// <exclude />
METHOD __CommandFromEvent(oEvent AS OBJECT) AS LOGIC STRICT
	//PP-030828 Strong typing




	RETURN SELF:Owner:__CommandFromEvent(oEvent)


 /// <exclude />
METHOD ButtonClick(oEvent)


	oParent:__DoValidate(oEvent:Control)
	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD ButtonDoubleClick(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD ControlNotify(oEvent)


	oParent:Owner:Dispatch(oEvent)
	SELF:EventReturnValue := oParent:Owner:EventReturnValue
	RETURN SELF


 /// <exclude />
METHOD Destroy()  AS USUAL CLIPPER


	IF !InCollect()
		oLastFocus := NULL_OBJECT
	ENDIF
	SUPER:Destroy()


	RETURN SELF


 /// <exclude />
METHOD DrawBackground(hDC, oWindow)
	//PP-031129
	RETURN oParent:Owner:DrawBackground(hdc, SELF)


 /// <exclude />
METHOD EditChange(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD EditFocusChange(oEvent)
	LOCAL oControl AS OBJECT




	oControl := oEvent:Control


	// If we are loosing focus - See who gets it. If it is another
	// control in our form call the validation now. If not store the
	// control until focus returns to our form
	IF !oEvent:GotFocus
		IF oControl==NULL_OBJECT
			RETURN SELF
		ENDIF
		oParent:__DoValidate(oControl)
	ELSE
		// If a control is getting focus - See if there is one waiting to
		// be validated and if so call validate
		oLastFocus := oControl
	ENDIF


	// Allow user to handle focus change
	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD EditScroll(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD Expose(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD FocusChange(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD GrowToParent()
	LOCAL r IS _WinRECT


	IF oParent != NULL_OBJECT
		GetClientRect(oParent:Handle(), @r)
		IF (SELF:Size:Width < r:right) .or. (SELF:Size:Height < r:bottom)
			SetWindowPos(hwnd, NULL_PTR, 0, 0, r:right, r:bottom, SWP_NOZORDER)
		ENDIF
	ENDIF
	RETURN SELF


 /// <exclude />
METHOD HelpRequest(oHelpRequestEvent)
	LOCAL cHelpContext AS STRING


	IF IsInstanceOfUsual(oHelpRequestEvent, #HelpRequestEvent) ;
		.and. SELF:HelpDisplay!=NULL_OBJECT;
		.and. oHelpRequestEvent:Helptype==HELPCONTROL
		IF oHelpRequestEvent:ItemID==3244
			IF SELF:Hyperlabel!=NULL_OBJECT ;
				.and. !((cHelpContext:=SELF:Hyperlabel:HelpContext)==NULL_STRING)
				SELF:HelpDisplay:Show(cHelpContext)
			ELSE
				SELF:HelpDisplay:Show("Window_WindowCanvas")
			ENDIF
		ENDIF
	ELSE
		SUPER:HelpRequest(oHelpRequestEvent)
	ENDIF


	RETURN NIL


 /// <exclude />
METHOD HorizontalScroll(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD HorizontalSlide(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD HorizontalSpin(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
CONSTRUCTOR(oOwner, xResourceID)




	SUPER(oOwner, xResourceID, FALSE)


	RETURN


 /// <exclude />
METHOD IsValidWindow(hTestWnd,cClass)
	LOCAL liStyle AS LONGINT




	IF hTestWnd==0 .OR. hWnd!=GetParent(hTestWnd);
		.or. cClass=="Static";
		.or. cClass=="ScrollBar"
		RETURN FALSE
	ENDIF


	IF cClass=="Button"
		liStyle := GetWindowLong( hTestWnd, GWL_STYLE )
		liStyle := _And(liStyle,0x0000000F)
		IF liStyle == BS_GROUPBOX
			RETURN FALSE
		ENDIF
	ENDIF


	RETURN TRUE


 /// <exclude />
METHOD KeyDown(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD KeyUp(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD ListBoxClick(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD ListBoxSelect(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD MouseButtonDoubleClick(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD MouseButtonDown(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD MouseButtonUp(oEvent)




	oParent:Owner:Dispatch(oEvent)


	RETURN SELF


 /// <exclude />
METHOD MouseDrag(oEvent)
	LOCAL oObject AS OBJECT






	oObject := oParent
	//oObject:MouseDrag(__ObjectCastClassPtr(oEvent, __pCMouseEvent))
	oObject:MouseDrag(MouseEvent{oEvent})


	//	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD MouseMove(oEvent)


	oParent:Owner:Dispatch(oEvent)
	RETURN SELF


 /// <exclude />
METHOD SetFocusToPrev()




	IF oLastFocus != NULL_OBJECT
		oLastFocus:SetFocus()
		RETURN TRUE
		//oApp:SetDialogWindow(self:Handle())
		//oApp:SetDialogWindow(oLastFocus:Owner:Handle())
	ENDIF


	RETURN FALSE


 /// <exclude />
METHOD SetSubformParent(oNewParent)




	oSubFormsParent := oNewParent


	//PP-030319 Style below causes painting problems with groupboxes on datawindows
	// SELF:SetStyle(WS_CLIPCHILDREN, TRUE)


	RETURN SELF


 /// <exclude />
METHOD VerticalScroll(oEvent)




	oParent:Owner:Dispatch(oEvent)


	RETURN SELF


 /// <exclude />
METHOD VerticalSlide(oEvent)




	oParent:Owner:Dispatch(oEvent)


	RETURN SELF


 /// <exclude />
METHOD VerticalSpin(oEvent)




	oParent:Owner:Dispatch(oEvent)


	RETURN SELF
END CLASS


