#ifdef DONOTINCLUDE


VOSTRUCT OleProps
	MEMBER dwSelector AS DWORD
	MEMBER fAllowInPlace AS LOGIC
	MEMBER fActivateOnDblClk AS LOGIC
	MEMBER fAllowResize AS LOGIC
	MEMBER fAutoSizeOnCreate AS LOGIC
	MEMBER fReadOnly AS LOGIC
	MEMBER fIsActive AS LOGIC
	MEMBER fAllowDocView AS LOGIC

FUNCTION __OleDropTargetCallback(DragInfo AS OleDragEventInfo) AS LOGIC /* WINCALL */
	LOCAL oAppWnd   AS AppWindow
	LOCAL oWnd      AS OBJECT
	LOCAL oOleDragEvent AS OleDragEvent
	LOCAL lRet AS USUAL
	// RvdH 080814 Added the methods to the AppWindow class, so we could 
	// get rid of the IsMethod checks
	lRet := FALSE

	oWnd := __WCGetWindowByHandle(DragInfo:hDocWnd):owner
	IF !IsInstanceOf(oWnd, #AppWindow)
		RETURN lRet
	ENDIF
	oAppWnd := oWnd
	oOleDragEvent := OleDragEvent{DragInfo}
	DO CASE
	CASE DragInfo:dwDragEvent = OLE_DDEVENT_DRAGENTER
		//IF IsMethod(oAppWnd, #OleDragEnter)
			lRet := oAppWnd:OleDragEnter(oOleDragEvent)
		//ENDIF
	CASE DragInfo:dwDragEvent = OLE_DDEVENT_DRAGLEAVE
		//IF IsMethod(oAppWnd, #OleDragLeave)
			lRet := oAppWnd:OleDragLeave(oOleDragEvent)
		//ENDIF
	CASE DragInfo:dwDragEvent = OLE_DDEVENT_DRAGOVER
		//IF IsMethod(oAppWnd, #OleDragOver)
			lRet := oAppWnd:OleDragOver(oOleDragEvent)
		//ENDIF
	CASE DragInfo:dwDragEvent = OLE_DDEVENT_DROP
		//IF IsMethod(oAppWnd, #OleDrop)
			lRet := oAppWnd:OleDrop(oOleDragEvent)
		//ENDIF
	ENDCASE
	DragInfo:dwEffect := DWORD(_CAST, oOleDragEvent:Effect)

	IF UsualType(lRet) != LOGIC
		lRet := FALSE
	ENDIF
	RETURN lRet

FUNCTION __OleStatusCallback(hwnd AS PTR, StatusMsg AS PSZ) AS DWORD /* WINCALL */
	LOCAL oShellWindow AS OBJECT

	oShellWindow := __WCGetWindowByHandle(hwnd)
	IF (oShellWindow != NULL_OBJECT) .AND. IsInstanceOf(oShellWindow, #ShellWindow)
		IF IsMethod(oShellWindow, #OnOleStatusMessage)
			Send(oShellWindow,#OnOleStatusMessage,Psz2String(StatusMsg))
			RETURN 0
		ENDIF
	ENDIF
	RETURN 1

	// RvdH 030815 Moved method to AppWindow Module
	//METHOD EnableOleDropTarget(lEnable) CLASS AppWindow
	//   IF (lEnable)
	//      RETURN _VOOLERegisterDropTargetCallback(SELF:owner:handle(), SELF:Handle(), @__OleDropTargetCallback())
	//   ELSE
	//      RETURN _VOOLERegisterDropTargetCallback(SELF:owner:handle(), SELF:Handle(), NULL_PTR)
	//   ENDIF

	//RvdH 030825 Moved function below to the module CavoOle
	//FUNCTION DeleteStorage(cFileName, cStorage) AS LOGIC
	//   RETURN _VOOLEDeleteStorage(String2Psz(cFileName), String2Psz(cStorage))

#endif