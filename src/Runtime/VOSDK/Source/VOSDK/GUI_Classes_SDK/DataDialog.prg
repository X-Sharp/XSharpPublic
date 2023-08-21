/// <include file="Gui.xml" path="doc/DataDialog/*" />
CLASS DataDialog INHERIT DataWindow
/*
/// <include file="Gui.xml" path="doc/DataDialog.Activate/*" />
METHOD Activate(oEvent)


	IF (oSurface != NULL_OBJECT)
		WCAppSetDialogWindow(oSurface:Handle())
	ENDIF


	RETURN SELF:Default(oEvent)
*/
/// <include file="Gui.xml" path="doc/DataDialog.ctor/*" />
CONSTRUCTOR(oOwner, oSource, nResourceID, nDialogStyle)
   // The only thing now left in this methods is that it sets
   // the WS_DLGFRAME style. The rest is all handled inside DataWindow:Init()
   // You can also call a datawindow directly and pass the WS_DLGFRAME dialog
   // style


   IF IsLong(nDialogStyle)
	   dwDialogStyle := (DWORD) _OR(WS_DLGFRAME, (LONG) nDialogStyle)
   ELSE
      dwDialogStyle := WS_DLGFRAME
   ENDIF
   SUPER(oOwner, oSource, nResourceID, dwDialogStyle)
   RETURN


END CLASS


