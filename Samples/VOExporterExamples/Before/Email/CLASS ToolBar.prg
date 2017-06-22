#warning The following method did not include a CLASS declaration
CLASS ToolBar_external_class INHERIT ToolBar
METHOD ShowButtonMenu(nButtonID, oButtonMenu, symTB) 
	LOCAL sRect  IS _winRect
   LOCAL hwndTB AS PTR
   LOCAL oMenu  AS Menu

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	
	IF LOGIC(_CAST, SendMessage(hwndTB, TB_GETRECT, DWORD(nButtonID), LONG(_CAST, @sRect)))
	   oMenu := oButtonMenu
      ClientToScreen(hwndTB, @sRect)
      ClientToScreen(hwndTB, @sRect.right)
      oMenu:ShowAsPopup(SELF:Owner, MakeLong(WORD(_CAST,sRect.left), WORD(_CAST,sRect.bottom)),,,@sRect)
	ENDIF

	RETURN NULL_OBJECT


END CLASS
