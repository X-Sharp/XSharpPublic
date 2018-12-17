
STATIC CLASS ToolBarExtensions
  STATIC METHOD ShowButtonMenu(SELF tbSelf as Toolbar, nButtonID as LONG, oButtonMenu as Menu) AS VOID
     tbSelf:ShowButtonMenu(nButtonID, oButtonMenu, #MAINTOOLBAR)
    RETURN
STATIC METHOD ShowButtonMenu(SELF tbSelf as Toolbar, nButtonID as LONG, oButtonMenu as Menu,symTb as Symbol) AS VOID
 
	LOCAL sRect  IS _winRect
   LOCAL hwndTB AS PTR
   LOCAL oMenu  AS Menu

	
	hwndTB := tbSelf:__FindToolBarHandle(symTB)
	
	IF LOGIC(_CAST, SendMessage(hwndTB, TB_GETRECT, DWORD(nButtonID), LONG(_CAST, @sRect)))
	   oMenu := oButtonMenu
      ClientToScreen(hwndTB, @sRect)
      ClientToScreen(hwndTB, @sRect.right)
      oMenu:ShowAsPopup(tbSelf:Owner, MakeLong(WORD(_CAST,sRect.left), WORD(_CAST,sRect.bottom)),,,@sRect)
	ENDIF

	RETURN NULL_OBJECT


END CLASS
