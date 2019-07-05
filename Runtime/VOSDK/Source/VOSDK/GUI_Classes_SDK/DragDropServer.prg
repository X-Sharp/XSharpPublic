CLASS DragDropServer INHERIT VObject
	PROTECT oParent AS Window
	//Liuho01 05-15-96 for DragDraopServer
	PROTECT hDragSingleCursor AS PTR
	PROTECT hDragMultipCursor AS PTR

	//PP-030828 Strong typing
	METHOD __DragAppendFiles(hDrop AS PTR, acFilesToDrop AS ARRAY) AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL strDropFile AS __WCDropFiles
	LOCAL dwLen AS DWORD
	LOCAL iAlen AS LONGINT
	LOCAL i AS LONGINT
	LOCAL pCurPath AS BYTE PTR
	LOCAL dwPathNameLength AS DWORD

	

	iAlen := INT(_CAST, ALen(acFilesToDrop))
	FOR i:= 1 UPTO iAlen
		dwPathnameLength += SLen(acFilesToDrop[i]) + 1
	NEXT

	hDrop := GlobalReAlloc(hDrop, _SIZEOF(__WCDropFiles) + dwPathNameLength + 1, _OR(GMEM_MOVEABLE, GMEM_SHARE, GMEM_ZEROINIT))
	strDropFile := GlobalLock(hDrop)
	pCurPath := PTR(_CAST, strDropFile)
	pCurPath += _SIZEOF(__WCDropFiles)

	FOR i:= 1 UPTO iAlen
		dwLen := SLen(acFilesToDrop[i])
		MemCopy(pCurPath, String2Psz(acFilesToDrop[i]), dwLen + 1)
		pCurPath += dwLen + 1
	NEXT

	BYTE(pCurPath) := 0

	GlobalUnlock(hDrop)

	RETURN hDrop

METHOD __DragCreateMemHandle() AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hDrop AS PTR
	LOCAL strDropFile AS __WCDropFiles
	LOCAL strPoint IS _winPoint
	

	GetCursorPos(@strPoint) //Get the Drop mouse position
	hDrop := GlobalAlloc(_OR(GMEM_MOVEABLE,GMEM_SHARE,GMEM_ZEROINIT), _SIZEOF(__WCDropFiles) + 1)
	//Lock block and initialize the data members
	strDropFile := GlobalLock(hDrop)
	strDropFile:wSize := _SIZEOF(__WCDropFIles)
	strDropFile:ptMousePos:X:= strPoint:X
	strDropFile:ptMousePos:Y:= strPoint:Y
	strDropFile:fUnicode := FALSE //not in Unicode
	GlobalUnlock(hDrop)

	RETURN hDrop

METHOD __GetKeyState(bKey AS BYTE) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL DIM aKeyStates[256] AS BYTE

	

	GetKeyboardState(@aKeyStates)
	IF _AND(aKeyStates[bKey + 1], 1) == 1
		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD Destroy() 
	

	IF !InCollect()
		UnregisterAxit(SELF)
		oParent := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN NIL

CONSTRUCTOR(oOwner) 
	

	IF !IsInstanceOfUsual(oOwner,#Window)
		WCError{#Init,#DragDropServer,__WCSTypeError,oOwner,1}:Throw()
	ENDIF

	__LoadShellDll()

	
	oParent := oOwner

	IF hDragSingleCursor == NULL_PTR //load Cursor at first time
		hDragSingleCursor := LoadCursor(_GetInst(), String2Psz("DragSingle"))
	ENDIF
	IF hDragMultipCursor == NULL_PTR
		hDragMultipCursor := LoadCursor(_GetInst(), String2Psz("DragMultip"))
	ENDIF

	RETURN 

METHOD StartDrag(acFilesToDrag) 
	LOCAL strPoint IS _winPoint
	LOCAL hWndSubject AS PTR
	LOCAL hWndClient AS PTR
	LOCAL hDrop AS PTR
	LOCAL dwArrLen AS DWORD
	LOCAL oTB AS TextBox
	LOCAL lStyle AS LONGINT
	LOCAL hDragCursor AS PTR

	

	IF !IsArray(acFilesToDrag)
		WCError{#Init,#DragDropServer,__WCSTypeError,acFilesToDrag,1}:Throw()
	ENDIF

	dwArrlen := ALen(acFilesToDrag)
	IF (dwArrLen == 0)
		RETURN FALSE
	ENDIF

	IF (dwArrlen > 1) // have some files to be draged
		hDragCursor := hDragMultipCursor
	ELSEIF (dwArrLen == 1)
		hDragCursor := hDragSingleCursor
	ENDIF

	hDrop:= SELF:__DragCreateMemHandle() //Allocate a memory block
	hDrop:= SELF:__DragAppendFiles(hDrop, acFilesToDrag)

	GlobalLock(hDrop)

	IF (hDrop == NULL_PTR)
		oTB := TextBox{	oParent, ;
			"File Drag Error", ;
			"Insufficient memory to drop(s)"}
		oTB:Type := BUTTONOKAY
		oTB:Show()
		PCALL(gpfnDragFinish, hDrop) // release memory block
		RETURN FALSE
	ENDIF

	ReleaseCapture()

	DragObject(GetDesktopWindow(), oParent:Handle(), 0x8003, 0, hDragCursor)

	// lGetFileNo = false, it means a real drag-Drop operation
	GetCursorPos(@strPoint) //Get the Drop mouse position
	hWndSubject := WindowFromPoint(strPoint:X, strPoint:y) //Get the DropClient handle
	//See if the subject window or any of is parent windows are pepared to
	//accept dropped files
	DO WHILE IsWindow(hWndSubject)
		lStyle := _AND(GetWindowLong(hWndSubject,GWL_EXSTYLE), WS_EX_ACCEPTFILES)
		IF (lStyle > 0)
			hWndClient := hWndSubject
			EXIT
		ENDIF
		hWndSubject:= GetParent(hWndSubject)
	ENDDO

	IF (hWndClient == NULL_PTR)
		PCALL(gpfnDragFinish, hDrop)
		RETURN FALSE
	ENDIF
	//PP-031006 Bug 98 This used to be SendMessage but Windows needs it to be Post to properly handle hDrop
	PostMessage(hWndClient, WM_DROPFILES, DWORD(_CAST, hDrop), 0)
	RETURN TRUE
END CLASS

