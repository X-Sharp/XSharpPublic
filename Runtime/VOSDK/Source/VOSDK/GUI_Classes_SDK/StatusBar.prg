STATIC DEFINE SBGRIPSIZE := 15
STATIC DEFINE SBMAXITEMS := 255
STATIC DEFINE SBMAXMESSAGE := 512
STATIC DEFINE SBMINMESSAGE := 30

PARTIAL CLASS StatusBar INHERIT Control
	PROTECT aItems AS ARRAY
	PROTECT aMessages AS ARRAY
	PROTECT aTipText AS ARRAY

	PROTECT lMessageArea AS LOGIC
	PROTECT lKeyboardArea AS LOGIC
	PROTECT lMemoryArea AS LOGIC
	PROTECT lPositionArea AS LOGIC
	PROTECT lTimeArea AS LOGIC

	PROTECT iInsertOn AS INT
	PROTECT iNumLockOn AS INT
	PROTECT iScrollOn AS INT
	PROTECT iCapsLockOn AS INT

	PROTECT nCurrentPriority AS INT
	PROTECT nTimeOut AS INT
	PROTECT nTimeCount AS INT
	PROTECT lTimeOutSet AS LOGIC

	PROTECT cLastPermanentMessage AS STRING
	PROTECT cLastControlMessage AS STRING
	PROTECT cLastErrorMessage AS STRING
	PROTECT cLastMenuMessage AS STRING

	PROTECT lErrorMessageBeep AS LOGIC

	PROTECT nHorizontalBorder AS INT
	PROTECT nItemBorder AS INT
	PROTECT nVerticalBorder AS INT

	PROTECT oKeyColor AS Color
	PROTECT oDisabledColor AS Color

	//PP-030828 Strong typing
METHOD __AutoSize() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL dwStart AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL dwItemCount AS DWORD
	LOCAL nAdjustment AS INT
    LOCAL oItem      AS StatusBarItem
	dwItemCount := ALen(aItems)

	// IF SELF:Size:Width < aItems[dwItemCount]:__Edge
	// RETURN NIL
	// ENDIF

	IF dwItemCount == 0
		RETURN
	ENDIF

	// Calculate adjustment             
	oItem := aItems[dwItemCount] 
	nAdjustment := SELF:Size:Width - (SBGRIPSIZE + SELF:HorizontalBorder + oItem:__Edge)
	// IF nAdjustment > 0
	dwStart := SELF:__GetItemFromSymbol(#MessageArea)
	IF dwStart != 0
		// Adjust the width of the message area
		oItem := aItems[dwStart] 
		oItem:Width := oItem:Width + nAdjustment

		// Starting from the first message area, adjust the right edge of each StatusBarItem
		FOR dwCount := dwStart UPTO dwItemCount
		    oItem := aItems[dwCount]
			oItem:__Edge := oItem:__Edge + nAdjustment
		NEXT // dwCount
	ENDIF
	RETURN

METHOD __BuildItems() AS StatusBar STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL dwCount       AS DWORD
	LOCAL dwItemCount   AS DWORD
	LOCAL dwIntegerSize AS DWORD
	LOCAL ptrItemArray  AS INT PTR
	LOCAL oIcon         AS Icon 
	LOCAL oItem         AS StatusBarItem

	dwItemCount := ALen(aItems)
	dwIntegerSize := _SIZEOF(INT)

	// Fill out the extent information for the StatusBarItems
	SELF:__InitItems()

	// Adjust StatusBarItems to fit across StatusBar
	SELF:__AutoSize()

	// Allocate a static array and fill it with StatusBarItem information
	// 070309 DCaton . no need to do this.
	IF dwItemCount >0 
		ptrItemArray := MemCAlloc(dwItemCount, dwIntegerSize)
		IF (ptrItemArray != NULL_PTR)
			// Set the right edge coordinate for each StatusBarItem
			FOR dwCount := 1 UPTO dwItemCount
			    oItem := aItems[dwCount] 
				ptrItemArray[dwCount] := oItem:__Edge
			NEXT // dwCount
	
			// Set the edge of the last StatusBarItem to -1 to stretch it to the edge
			// of the StatusBar
			ptrItemArray[dwItemCount] := -1
	
			// Draw the StatusBarItems
			SendMessage(hwnd, SB_SETPARTS, dwItemCount, LONG(_CAST, ptrItemArray))
	
			// Free the static array
			MemFree(ptrItemArray)
		ENDIF
	   FOR dwCount := 1 UPTO dwItemCount
            oItem := aItems[dwCount] 
		    oIcon := oItem:Icon
	        IF oIcon = NULL_OBJECT
	       	    SendMessage(hwnd, SB_SETICON, dwCount-1u, 0l)
	        ELSE
				 SendMessage(hwnd, SB_SETICON, dwCount-1u, LONG(_CAST, oIcon:Handle()))
			ENDIF
		NEXT // dwCount
	ENDIF

	RETURN SELF

METHOD __GetBorderWidths() AS StatusBar STRICT 
	//PP-030828 Strong typing
	LOCAL dwIntegerSize AS DWORD
	LOCAL ptrBorderArray AS DWORD PTR

	dwIntegerSize := _SIZEOF(INT)

	// Allocate a static array to be filled with border information
	ptrBorderArray := MemCAlloc(3, dwIntegerSize)
	IF ptrBorderArray != NULL_PTR
		// Retrieve border information from Windows
		IF hWnd != 0
			SendMessage(hWnd, SB_GETBORDERS, 0, LONGINT(_CAST, ptrBorderArray))
			nHorizontalBorder := INT(_CAST, ptrBorderArray[1])
			nItemBorder := INT(_CAST, ptrBorderArray[2])
			nVerticalBorder := INT(_CAST, ptrBorderArray[3])
		ENDIF

		// Free the static array
		MemFree(ptrBorderArray)
	ENDIF

	RETURN SELF

METHOD __GetItemFromSymbol(symItemName AS SYMBOL) AS DWORD STRICT 
	//PP-030828 Strong typing
	LOCAL i, iLen AS DWORD
    LOCAL oItem AS StatusBarItem

	iLen := ALen(aItems)
	FOR i:= 1 TO iLen              
	    oItem := aItems[i] 
		IF oItem:NameSym == symItemName
			RETURN i
		ENDIF
	NEXT
	RETURN 0

METHOD __GetItemWidth(symItemName AS SYMBOL) AS DWORD STRICT 
	//PP-030828 Strong typing
	LOCAL _hDC AS PTR
	LOCAL strucSize1 IS _WinSize
	LOCAL strucSize2 IS _WinSize
	LOCAL dwReturnValue AS DWORD
	LOCAL sTime AS STRING

	

	IF hWnd != 0
		_hDC := GetDC(hWnd)

		// For each case, get the text extent of the characters that
		// are to be used in each StatusBarItem in the correct quantity
		DO CASE
		CASE symItemName == #InsArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "INS"), 3, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx)
			ENDIF

		CASE symItemName == #CapsLockArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "CAPS"), 4, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx)
			ENDIF

		CASE symItemName == #NumLockArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "NUM"), 3, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx)
			ENDIF

		CASE symItemName == #ScrollLockArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "SCROLL"), 6, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx)
			ENDIF

		CASE symItemName == #MemoryArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "8"), 1, @strucSize1) .AND. ;
				GetTextExtentPoint32(_hDC, PSZ(_CAST, " K"), 2, @strucSize2)
				dwReturnValue := DWORD(strucSize1:cx * 9 + strucSize2:cx)
			ENDIF

		CASE symItemName == #MessageArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "M"), 1, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx * SBMINMESSAGE)
			ENDIF

		CASE symItemName == #PositionArea
			IF GetTextExtentPoint32(_hDC, PSZ(_CAST, "8"), 1, @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx * 10)
			ENDIF

		CASE symItemName == #TimeArea
			sTime := Time()
			IF GetTextExtentPoint32(_hDC, String2Psz(sTime), INT(_CAST, SLen(sTime)), @strucSize1)
				dwReturnValue := DWORD(strucSize1:cx)
			ENDIF
		ENDCASE

		ReleaseDC(hWnd, _hDC)
	ENDIF

	RETURN dwReturnValue

METHOD __GetKeyState(bKey AS BYTE) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL DIM aKeyStates[256] AS BYTE

	GetKeyboardState(@aKeyStates)
	IF _AND(aKeyStates[bKey + 1], 1) == 1
		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD __GetSymbolFromItem(dwIndex AS DWORD) AS SYMBOL STRICT 
    LOCAL oItem AS StatusBarItem
    IF dwIndex <= ALen(aItems)
        oItem :=aItems[dwIndex] 	
    	RETURN oItem:NameSym
    ENDIF
    RETURN NULL_SYMBOL
METHOD __GetText(symItemName := NIL AS USUAL) AS STRING STRICT 
	//PP-030828 Strong typing
	LOCAL cText AS STRING
	LOCAL pszText AS PSZ
	LOCAL dwIndex AS DWORD
	LOCAL DIM aBuf[SBMAXMESSAGE+1] AS BYTE

	

	// Lookup message area index by default
	Default(@symItemName, #MessageArea)

	dwIndex := SELF:__GetItemFromSymbol(symItemName)
	IF (dwIndex != 0)
		pszText := @aBuf[1]
		SendMessage(SELF:Handle(), SB_GETTEXT, dwIndex - 1, LONGINT(_CAST, pszText))
		cText := Psz2String(pszText)
	ENDIF

	RETURN AllTrim(cText)

METHOD __InitItems() AS StatusBar STRICT 
	//PP-030828 Strong typing
	LOCAL nVBorder AS INT
	LOCAL nHBorder AS INT
	LOCAL nSBorder AS INT
	LOCAL dwCount AS DWORD
	LOCAL dwItemCount AS DWORD
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL oPrevStatusBarItem AS StatusBarItem

	

	// Get border information for use in calculations
	nHBorder := SELF:HorizontalBorder
	nVBorder := SELF:VerticalBorder
	nSBorder := SELF:ItemBorder

	dwItemCount := ALen(aItems)

	// Set the width of all system-defined StatusBarItems
	SELF:__SetSystemItemWidths()

	FOR dwCount := 1 UPTO dwItemCount
		oStatusBarItem := aItems[dwCount]

		// Set the right edge of the StatusBarItem
		IF dwCount == 1
			// Building the first StatusBarItem
			oStatusBarItem:__Edge := nVBorder + oStatusBarItem:Width
		ELSE
			// Use the right edge of the previous StatusBarItem to calculate
			// the right edge of the current StatusBarItem
			oPrevStatusBarItem := aItems[dwCount - 1]
			oStatusBarItem:__Edge := oPrevStatusBarItem:__Edge + nSBorder + oStatusBarItem:Width
		ENDIF
	NEXT  // dwCount

	RETURN SELF

ACCESS __IsTopAligned AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	IF (hWnd != NULL_PTR)
		IF _AND(GetWindowLong(hwnd, GWL_STYLE), CCS_TOP) == 1
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

METHOD __SetKeyState(bKey AS BYTE, lTurnOn AS LOGIC) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL lCurrentlyOn AS LOGIC

	

	lCurrentlyOn := SELF:__GetKeyState(bKey)

	IF (lCurrentlyOn .AND. !lTurnOn) .OR. (!lCurrentlyOn .AND. lTurnOn)
		SELF:__ToggleKeyState(bKey)
	ENDIF
   RETURN

METHOD __SetSystemItemWidths() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL symItemName AS SYMBOL
	LOCAL dwIndex AS DWORD

	

	FOR dwIndex := 1 UPTO ALen(aItems)
		oStatusBarItem := aItems[dwIndex]
		symItemName := oStatusBarItem:NameSym

		IF symItemName == #CapsLockArea .OR. symItemName == #InsArea .OR. ;
			symItemName == #MemoryArea .OR. symItemName == #MessageArea .OR. ;
			symItemName == #NumLockArea .OR. symItemName == #PositionArea .OR. ;
			symItemName == #ScrollLockArea .OR. symItemName == #TimeArea
			oStatusBarItem:Width := SELF:__GetItemWidth(symItemName)
		ENDIF
	NEXT  // dwIndex
	RETURN

METHOD __ToggleKeyState(bKey AS BYTE) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL DIM aKeyStates[256] AS BYTE

	

	GetKeyboardState(@aKeyStates)
	IF (_AND(aKeyStates[INT(bKey)+1], 1) > 0) // Is Key on
		aKeyStates[bKey+1] := _AND(aKeyStates[bKey+1], 0xFE) // Turn Key off
	ELSE
		aKeyStates[bKey+1] := _OR(aKeyStates[bKey+1], 0x01) // Turn Key on
	ENDIF

	SetKeyboardState(@aKeyStates)

	// Simulate the key press, then the key release
	// keybd_event(bKey, 0x45, 0, 0)
	// keybd_event(bKey, 0x45, KEYEVENTF_KEYUP, 0)
	RETURN

METHOD __UpdateKeyStates() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL DIM aKeyStates[256] AS BYTE
	LOCAL iSet AS LONGINT

	

	IF lKeyboardArea
		GetKeyboardState(@aKeyStates)

		IF (iSet := _AND(aKeyStates[VK_INSERT + 1], 1)) != iInsertOn
			SELF:SetValue(LOGIC(_CAST,iSet), #InsArea)
			iInsertOn := iSet
		ENDIF

		IF (iSet := _AND(aKeyStates[VK_CAPITAL + 1], 1)) != iCapsLockOn
			SELF:SetValue(LOGIC(_CAST,iSet), #CapsLockArea)
			iCapsLockOn := iSet
		ENDIF

		IF (iSet := _AND(aKeyStates[VK_NUMLOCK + 1], 1)) != iNumLockOn
			SELF:SetValue(LOGIC(_CAST,iSet), #NumLockArea)
			iNumLockOn := iSet
		ENDIF

		IF (iSet := _AND(aKeyStates[VK_SCROLL + 1], 1)) != iScrollOn
			SELF:SetValue(LOGIC(_CAST,iSet), #ScrollLockArea)
			iScrollOn := iSet
		ENDIF

	ENDIF
	RETURN

METHOD AddItem(oStatusBarItem) 

	

	AAdd(aItems, oStatusBarItem)
	SELF:__BuildItems()
	RETURN NIL

ACCESS AsString 
	

	RETURN cLastPermanentMessage

ASSIGN AsString(cMessage) 
	

	SELF:setmessage(cLastPermanentMessage := cMessage, MESSAGEPERMANENT)
	RETURN 

METHOD ClearItems() 
	LOCAL dwCount AS DWORD
	LOCAL dwItemStyle AS DWORD
	LOCAL oStatusBarItem AS StatusBarItem

	

	FOR dwCount := 1 UPTO ALen(aItems)
		oStatusBarItem := aItems[dwCount]
		dwItemStyle := _OR(dwCount - 1, DWORD(oStatusBarItem:Style))
		SendMessage(SELF:Handle(), SB_SETTEXT, dwItemStyle, LONGINT(_CAST, NULL_PSZ))
	NEXT  // dwCount

	RETURN SELF

ACCESS ControlText 
	

	RETURN cLastControlMessage

ASSIGN ControlText(cMessage) 
	

	SELF:setmessage(cLastControlMessage := cMessage, MESSAGECONTROL)
	RETURN 

METHOD Create() 
	

	SELF:SetStyle(_OR(SBT_TOOLTIPS, SBARS_SIZEGRIP), TRUE)

	IF (hWnd == NULL_PTR)
		hWnd := CreateStatusWindow(LONGINT(_CAST, dwStyle), NULL_PSZ, oParent:Handle(), SELF:ControlID)

		__lpfnDefaultProc := GetWindowLong(hWnd, GWL_WNDPROC)
		SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))

		oSize := NULL_OBJECT
		oOrigin := NULL_OBJECT

		__WCRegisterControl(SELF)

		SELF:__BuildItems()
		SELF:ClearItems()
		SELF:RegisterTimer(1)
	ENDIF

	RETURN hWnd

METHOD Destroy() 
	

	IF !InCollect()
		aMessages := NULL_ARRAY
		aItems := NULL_ARRAY
	ENDIF

	RETURN SUPER:Destroy()

ACCESS DisabledKeyIndicatorColor 
	

	RETURN oDisabledColor

ASSIGN DisabledKeyIndicatorColor(oColor) 
	

	oDisabledColor := oColor
	RETURN 

METHOD Dispatch(oEvent) 
	LOCAL dwMessage AS DWORD
	LOCAL strucPoint IS _winPoint
	LOCAL strucRect IS _winRect   
	LOCAL oEvt := oEvent AS @@Event

	

	dwMessage := oEvt:Message
	IF dwMessage == WM_LBUTTONDOWN
		IF lKeyboardArea
			strucPoint:x := LoWord(DWORD(oEvt:lParam))
			strucPoint:y := HiWord(DWORD(oEvt:lParam))

			SendMessage(SELF:Handle(), SB_GETRECT, SELF:__GetItemFromSymbol(#InsArea) - 1, LONGINT(_CAST, @strucRect))
			IF strucPoint:x > strucRect:left .AND. strucPoint:x < strucRect:right .AND. strucPoint:y > strucRect:top .AND. strucPoint:y < strucRect:bottom
				SELF:__ToggleKeyState(VK_INSERT)
			ENDIF

			SendMessage(SELF:Handle(), SB_GETRECT, SELF:__GetItemFromSymbol(#CapsLockArea) - 1, LONGINT(_CAST, @strucRect))
			IF strucPoint:x > strucRect:left .AND. strucPoint:x < strucRect:right .AND. strucPoint:y > strucRect:top .AND. strucPoint:y < strucRect:bottom
				SELF:__ToggleKeyState(VK_CAPITAL)
			ENDIF

			SendMessage(SELF:Handle(), SB_GETRECT, SELF:__GetItemFromSymbol(#NumLockArea) - 1, LONGINT(_CAST, @strucRect))
			IF strucPoint:x > strucRect:left .AND. strucPoint:x < strucRect:right .AND. strucPoint:y > strucRect:top .AND. strucPoint:y < strucRect:bottom
				SELF:__ToggleKeyState(VK_NUMLOCK)
			ENDIF

			SendMessage(SELF:Handle(), SB_GETRECT, SELF:__GetItemFromSymbol(#ScrollLockArea) - 1, LONGINT(_CAST, @strucRect))
			IF strucPoint:x > strucRect:left .AND. strucPoint:x < strucRect:right .AND. strucPoint:y > strucRect:top .AND. strucPoint:y < strucRect:bottom
				SELF:__ToggleKeyState(VK_SCROLL)
			ENDIF

			SELF:__UpdateKeyStates()
		ENDIF
	ENDIF

	RETURN SUPER:Dispatch(oEvt)

METHOD DisplayKeyboard() 
	LOCAL dwCount AS DWORD
	LOCAL dwItemCount AS DWORD
	LOCAL oStatusBarItem AS StatusBarKeyItem
	LOCAL aKeys AS ARRAY

	

	// Make sure this section hasn't already been added
	IF lKeyboardArea
		RETURN NIL
	ENDIF

	// Make sure we won't exceeded the maximum number of StatusBarItems
	dwItemCount := ALen(aItems)
	IF dwItemCount == SBMAXITEMS .OR. dwItemCount + 4 > SBMAXITEMS
		RETURN NIL
	ENDIF

	aKeys := {{#InsArea, "INS"}, {#CapsLockArea, "CAPS"}, {#NumLockArea, "NUM"}, {#ScrollLockArea, "SCROLL"}}
	FOR dwCount := 1 UPTO 4
		// Create and name the StatusBarItem
		oStatusBarItem := StatusBarKeyItem{aKeys[dwCount,1]}
		oStatusBarItem:KeyText := aKeys[dwCount,2]
		SELF:AddItem(oStatusBarItem)
	NEXT  // dwCount

	lKeyboardArea := TRUE

	RETURN SELF

METHOD DisplayMemory() 
	LOCAL dwItemCount AS DWORD
	LOCAL oStatusBarItem AS StatusBarItem

	

	// Make sure this section hasn't already been added
	IF lMemoryArea
		RETURN NIL
	ENDIF

	// Make sure we haven't exceeded the maximum number of StatusBarItems
	dwItemCount := ALen(aItems)
	IF dwItemCount == SBMAXITEMS
		RETURN NIL
	ENDIF

	// Create and name the StatusBarItem
	oStatusBarItem := StatusBarItem{}
	oStatusBarItem:NameSym := #MemoryArea
	SELF:AddItem(oStatusBarItem)
	lMemoryArea := TRUE

	RETURN SELF

METHOD DisplayMessage() 
	LOCAL dwItemCount AS DWORD

	

	// Make sure this section hasn't already been added
	IF lMessageArea
		RETURN NIL
	ENDIF

	// Make sure we haven't exceeded the maximum number of StatusBarItems
	dwItemCount := ALen(aItems)
	IF (dwItemCount == SBMAXITEMS)
		RETURN SELF
	ENDIF

	// Create and name the StatusBarItem
	SELF:AddItem(StatusBarItem{#MessageArea, , SBITEMFLAT})
	SELF:SetText(NULL_STRING, #MessageArea)
	//InvalidateRect(hwnd, NULL_PTR, true)
	lMessageArea := TRUE

	RETURN SELF

METHOD DisplayPosition() 
	LOCAL dwItemCount AS DWORD
	LOCAL oStatusBarItem AS StatusBarItem

	

	// Make sure this section hasn't already been added
	IF lPositionArea
		RETURN NIL
	ENDIF

	// Make sure we haven't exceeded the maximum number of StatusBarItems
	dwItemCount := ALen(aItems)
	IF dwItemCount == SBMAXITEMS
		RETURN SELF
	ENDIF

	// Create and name the StatusBarItem
	oStatusBarItem := StatusBarItem{}
	oStatusBarItem:NameSym := #PositionArea
	SELF:AddItem(oStatusBarItem)
	lPositionArea := TRUE

	RETURN SELF

METHOD DisplayTime() 
	LOCAL dwItemCount AS DWORD
	LOCAL oStatusBarItem AS StatusBarItem

	

	// Make sure this section hasn't already been added
	IF lTimeArea
		RETURN NIL
	ENDIF

	// Make sure we haven't exceeded the maximum number of StatusBarItems
	dwItemCount := ALen(aItems)
	IF dwItemCount == SBMAXITEMS
		RETURN SELF
	ENDIF

	// Create and name the StatusBarItem
	oStatusBarItem := StatusBarItem{}
	oStatusBarItem:NameSym := #TimeArea
	SELF:AddItem(oStatusBarItem)
	lTimeArea := TRUE

	RETURN SELF

ACCESS ErrorMessageBeep 
	

	RETURN lErrorMessageBeep

ASSIGN ErrorMessageBeep(lEnable) 
	

	RETURN lErrorMessageBeep := lEnable

ACCESS ErrorText() 
	

	RETURN cLastErrorMessage

ASSIGN ErrorText(cMessage) 
	

	SELF:setmessage(cLastErrorMessage := cMessage, MESSAGEERROR)

	RETURN 

METHOD GetItemBoundingBox(symItemName) 
	LOCAL dwIndex AS DWORD
	LOCAL strucRect IS _winRECT
	LOCAL oPoint AS Point
	LOCAL oDimension AS Dimension

	

	Default(@symItemName, #MessageArea)

	// 2.0a-1, changed from #symItemName
	dwIndex := SELF:__GetItemFromSymbol(symItemName)
	IF dwIndex != 0
		SendMessage(SELF:Handle(), SB_GETRECT, dwIndex - 1, LONGINT(_CAST, @strucRect))
	ENDIF

	oPoint := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
	oDimension := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

	RETURN BoundingBox{oPoint, oDimension}

METHOD GetTipText(symItemName) 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
	LOCAL symName AS SYMBOL

	

	Default(@symItemName, #MessageArea)

	IF IsLong(symItemName)
		symName := SELF:__GetSymbolFromItem(symItemName+1)
	ELSE
	   symName := symItemName
	ENDIF

   dwCount := ALen(aTipText)
	FOR dwI := 1 UPTO dwCount
	   IF aTipText[dwI, 1] == symName
	   	RETURN aTipText[dwI][2]
	   ENDIF
	NEXT  // dwI

	RETURN NULL_STRING

METHOD Hide() 
	

	SUPER:Hide()

	IF IsInstanceOf(SELF:Owner, #ShellWindow)
		SELF:Owner:__AdjustClient()
	ELSEIF IsInstanceOf(SELF:Owner, #DataWindow)
		SELF:Owner:__AdjustForm()
	ENDIF

	RETURN NIL

ACCESS HorizontalBorder 
	

	SELF:__GetBorderWidths()

	RETURN nHorizontalBorder

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	LOCAL dwStyle AS DWORD

	

	aMessages := ArrayNew(4)
	aItems := {}
	nTimeOut := 4
	lErrorMessageBeep := TRUE
	oKeyColor := Color{COLORGREEN}
	oDisabledColor := Color{128, 128, 128} // Gray

	iInsertOn := iNumLockOn := iScrollOn := iCapsLockOn := -1

	IF IsInstanceOfUsual(xID, #ResourceID)
		// Created by resource
		SUPER(oOwner, xID, oPoint, oDimension, , , FALSE)
	ELSE
		// Created dynamically
		dwStyle := _OR(WS_CHILD, WS_VISIBLE, WS_BORDER, SBS_SIZEGRIP)
		SUPER(oOwner, 0, Point{}, Dimension{}, STATUSCLASSNAME, dwStyle, FALSE)
	ENDIF

	RETURN 

ACCESS InsertMode 
	

	RETURN SELF:__GetKeyState(VK_INSERT)

ASSIGN InsertMode(lEnable) 
	

	Default(@lEnable, TRUE)

	SELF:__SetKeyState(VK_INSERT, lEnable)

	RETURN 

ACCESS ItemBorder 
	

	SELF:__GetBorderWidths()
	RETURN nItemBorder

ACCESS KeyIndicatorColor 
	

	RETURN oKeyColor

ASSIGN KeyIndicatorColor(oColor) 
	

	oKeyColor := oColor
	RETURN 

ACCESS Length 
	LOCAL dwIndex AS DWORD

	

	dwIndex := SELF:__GetItemFromSymbol(#MessageArea)
	IF dwIndex != 0
		RETURN SendMessage(SELF:Handle(), SB_GETTEXTLENGTH, dwIndex - 1, 0)
	ENDIF

	RETURN 0

ACCESS MenuText 
	

	RETURN cLastMenuMessage

ASSIGN MenuText(cMessage) 
	

	SELF:setmessage(cLastMenuMessage := cMessage, MESSAGEMENU)

	RETURN 

METHOD ODDrawItem(oEvent) 
	LOCAL oEvt AS @@Event
	LOCAL p1   AS _winDRAWITEMSTRUCT
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL dwId AS DWORD

	oEvt     := oEvent
	p1       := PTR(_CAST, oEvt:lParam)

	dwId := p1:ItemID + 1
	IF dwId > 0 .AND. dwId <= ALen(SELF:aItems)
		oStatusBarItem := aItems[dwId]
		IF IsMethod(oStatusBarItem, #ODDrawItem)
			Send(oStatusBarItem, #ODDrawItem, oEvt, SELF)
		ENDIF
	ENDIF
	RETURN SELF


ACCESS PermanentText 
	

	RETURN cLastPermanentMessage

ASSIGN PermanentText(cMessage) 
	

	SELF:setmessage(cLastPermanentMessage := cMessage, MESSAGEPERMANENT)

	RETURN 

ASSIGN Position(oPoint) 
	

	SELF:SetPair(oPoint)

	RETURN 

METHOD RefreshMemoryDisplay(kMemoryType) 
	

#ifndef __VULCAN__  // Memory() not supported in Vulcan
	// Default to SYSTEM_FREE (GetFreeSpace())
	Default(@kMemoryType, MEMORY_SYSTEM_FREE)

	SELF:SetText(AllTrim(AsString(Memory(kMemoryType))) + " K", #MemoryArea)
#endif
	RETURN NIL

METHOD SetIcon(oIcon, symItemName) 
	//SE-060525
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL dwIndex        AS DWORD

	Default(@symItemName, #MessageArea)

	dwIndex := SELF:__GetItemFromSymbol(symItemName)
	IF dwIndex != 0 .AND. SELF:ValidateControl()
		oStatusBarItem := aItems[dwIndex]
		IF IsInstanceOfUsual(oIcon, #Icon)
			oStatusBarItem:__Icon := oIcon
			SendMessage(hwnd, SB_SETICON, dwIndex-1u, LONGINT(_CAST, oIcon:Handle()))
		ELSE
			oStatusBarItem:__Icon := NULL_OBJECT
			SendMessage(hwnd, SB_SETICON, dwIndex-1u, 0l)
		ENDIF
	ENDIF

	RETURN oIcon

METHOD setmessage(cMessage, nMode) 
	LOCAL lValidMessage AS LOGIC
	LOCAL dwCount AS DWORD

	

	IF IsNil(cMessage) .OR. (aMessages == NULL_ARRAY)
		RETURN NIL
	ENDIF

	IF IsNil(nMode)
		nMode := MESSAGEPERMANENT
	ENDIF
	IF nMode < MESSAGEPERMANENT .OR. nMode > MESSAGEERROR
		RETURN NIL
	ENDIF

	DO CASE
	CASE (nMode == MESSAGEMENU)
		cLastMenuMessage := cMessage
	CASE (nMode == MESSAGECONTROL)
		cLastControlMessage := cMessage
	CASE (nMode == MESSAGEERROR)
		cLastErrorMessage := cMessage
		IF NULL_STRING != AllTrim(cMessage) .AND. SELF:ErrorMessageBeep
			MessageBeep(0xFFFFFFFF)
		ENDIF
	CASE nMode == MESSAGEPERMANENT
		cLastPermanentMessage := cMessage
	END CASE

	IF !Empty(cMessage) .OR. (nMode == MESSAGEPERMANENT)
		lValidMessage := TRUE
	ENDIF
	// IF lValidMessage
	aMessages[nMode] := cMessage
	// ENDIF

	IF lValidMessage .AND. nMode >= nCurrentPriority
		SELF:SetText(cMessage, #MessageArea)
		nCurrentPriority := nMode
		nTimeCount := 0
	ELSEIF !lValidMessage .AND. nMode >= nCurrentPriority
		FOR dwCount := DWORD(nCurrentPriority) DOWNTO 0
			IF dwCount == 0
				SELF:SetText(NULL_STRING, #MessageArea)
				nCurrentPriority := LONGINT(_CAST, dwCount)
				nTimeCount := 0
				EXIT
			ELSEIF (NULL_STRING != aMessages[dwCount]) .AND. !IsNil(aMessages[dwCount])
				SELF:SetText(aMessages[dwCount], #MessageArea)
				nCurrentPriority := LONGINT(_CAST, dwCount)
				nTimeCount := 0
				EXIT
			ENDIF
		NEXT  // dwCount
	ENDIF

	IF (nMode != MESSAGEPERMANENT) .AND. nTimeOut >= 1
		lTimeOutSet := TRUE
	ENDIF

	RETURN NIL

METHOD SetPair(oPoint) 
	LOCAL cText AS STRING

	

	cText := AllTrim(AsString(oPoint:X)) + ", " + AllTrim(AsString(oPoint:Y))
	SELF:SetText(cText, #PositionArea)

	RETURN NIL


METHOD SetText(cText, symItemName) 
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL dwIndex AS DWORD
	LOCAL dwParam AS DWORD

	

	// Lookup message area index by default
	Default(@symItemName, #MessageArea)

	dwIndex := SELF:__GetItemFromSymbol(symItemName)
	IF (dwIndex != 0) .AND. SELF:ValidateControl()
		oStatusBarItem := aItems[dwIndex]
		dwParam := _OR(DWORD(dwIndex - 1), DWORD(oStatusBarItem:Style))
		IF (NULL_STRING != cText)
			SendMessage(hwnd, SB_SETTEXT, dwParam, LONGINT(_CAST, String2Psz(cText)))
		ELSE
			SendMessage(hwnd, SB_SETTEXT, dwParam, 0)
		ENDIF
	ENDIF

	RETURN cText

METHOD SetTipText(cTipText, symItemName) 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
	LOCAL symName AS SYMBOL

	

	Default(@symItemName, #MessageArea)

	symName := symItemName

	dwCount := ALen(aTipText)
	FOR dwI := 1 UPTO dwCount
	   IF aTipText[dwI, 1] == symName
	   	aTipText[dwI][2] := cTipText
	   	RETURN SELF
	   ENDIF
	NEXT  // dwI

	IF aTipText = NULL_ARRAY
		aTipText := {}
	ENDIF
	AAdd(aTipText, {symName, cTipText})

   RETURN SELF

METHOD SetValue(uValue, symItemName) 
	LOCAL oStatusBarItem AS StatusBarItem
	LOCAL dwIndex AS DWORD

	

	// Lookup message area index by default
	Default(@symItemName, #MessageArea)

	dwIndex := SELF:__GetItemFromSymbol(symItemName)
	IF (dwIndex != 0) .AND. SELF:ValidateControl()
		oStatusBarItem       := aItems[dwIndex]
		oStatusBarItem:Value := uValue
		SendMessage(hwnd, SB_SETTEXT, _OR((dwIndex - 1), DWORD(oStatusBarItem:Style), SBT_OWNERDRAW), 0l)
	ENDIF

	RETURN uValue

METHOD Show() 
	

	SUPER:Show()

	IF IsInstanceOf(SELF:Owner, #ShellWindow)
		SELF:Owner:__AdjustClient()
	ENDIF
	SELF:RefreshMemoryDisplay()
	SELF:__UpdateKeyStates()

	RETURN SELF

ACCESS TextValue 
	

	RETURN SELF:__GetText()

ASSIGN TextValue(cText) 

	

	SELF:SetText(cText)
	RETURN 

ACCESS TimeOut 
	

	RETURN nTimeOut

ASSIGN TimeOut(nNewTimeOut) 
	

	IF nNewTimeOut < 0
		nNewTimeOut := 0
	ENDIF
	nTimeOut := nNewTimeOut

	RETURN 

METHOD Timer 
	

	SUPER:Timer()

	SELF:__UpdateKeyStates()
	SELF:SetText(AllTrim(Time()), #TimeArea)
	IF lTimeOutSet
		nTimeCount := nTimeCount + 1
		IF (nTimeCount == nTimeOut)
			// Message timeout has occurred
			nTimeCount := 0
			lTimeOutSet := FALSE
			nCurrentPriority := MESSAGEPERMANENT
			SELF:setmessage(SELF:PermanentText, MESSAGEPERMANENT)
		ENDIF
	ENDIF

	RETURN NIL

ACCESS Transient 
	

	RETURN cLastErrorMessage

ASSIGN Transient(cMessage) 
	

	SELF:setmessage(cLastErrorMessage := cMessage, MESSAGEERROR)
	RETURN 

ACCESS VerticalBorder 
	

	SELF:__GetBorderWidths()

	RETURN nVerticalBorder
END CLASS

PARTIAL CLASS StatusBarItem INHERIT VObject
	PROTECT symItemName AS SYMBOL
	PROTECT nEdge AS INT
	PROTECT nWidth AS INT
	PROTECT dwStyle AS DWORD
	PROTECT uValue AS USUAL
	PROTECT oSBIcon AS Icon //SE-060525

	//PP-030828 Strong typing
	ACCESS __Edge AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN nEdge

ASSIGN __Edge(nNewEdge AS INT)  STRICT 
	//PP-030828 Strong typing

	

	//PP-030828 Strong typing
	// Default(@nNewEdge, 0)

	RETURN nEdge := nNewEdge

ASSIGN __Icon(oIcon AS Icon)  STRICT 
	//SE-060525
	

	RETURN oSBIcon := oIcon

ACCESS Icon 
	//SE-060525
	

	RETURN oSBIcon

CONSTRUCTOR(symName, nWidth, kStyle, oIcon) 
	

	SELF:NameSym := symName
	SELF:Width := nWidth
	SELF:Style := kStyle

   //SE-060525
   IF IsInstanceOfUsual(oIcon, #Icon)
		oSBIcon := oIcon
	ENDIF

	RETURN 

ACCESS NameSym 
	

	RETURN symItemName

ASSIGN NameSym(symNewItemName) 

	

	IF IsString(symNewItemName)
		symNewItemName := String2Symbol(symNewItemName)
	ELSEIF !IsSymbol(symNewItemName)
		symNewItemName := NULL_SYMBOL
	ENDIF

	RETURN (symItemName := symNewItemName)

ACCESS Style 
	

	RETURN dwStyle

ASSIGN Style(kStyle) 

	

	IF !IsLong(kStyle)
		kStyle := SBITEMSUNKEN
	ENDIF

	RETURN (dwStyle := kStyle)

ACCESS Value 
	

	RETURN uValue

ASSIGN Value(uNewValue) 
	

	RETURN (uValue := uNewValue)

ACCESS Width 
	

	RETURN nWidth

ASSIGN Width(nNewWidth) 
	

	IF !IsLong(nNewWidth)
		nNewWidth := 0
	ENDIF

	RETURN (nWidth := nNewWidth)

END CLASS

PARTIAL CLASS StatusBarKeyItem INHERIT StatusBarItem
	PROTECT cKeyText AS STRING

CONSTRUCTOR(symName, nWidth, kStyle, oIcon) 
    
    SUPER(symName, nWidth, kStyle, oIcon)


RETURN 

ASSIGN KeyText (cValue) 
	RETURN cKeyText := cValue

METHOD ODDrawItem(oEvent, oStatusBar) 
	//PP-20040428 Owner draw, from S Ebert
	LOCAL oEvt          AS @@Event
	LOCAL strucDrawItem AS _WINDrawItemStruct
	LOCAL nMode         AS INT
	LOCAL lSet          AS LOGIC
	LOCAL oColor        AS Color
	LOCAL dwOldColor    AS DWORD
	LOCAL hFont         AS PTR

	oEvt          := oEvent
	strucDrawItem := PTR(_CAST, oEvt:lParam)

	// Set mode and color
	nMode := SetBkMode(strucDrawItem:hDC, TRANSPARENT)

	lSet := uValue
	IF lSet
		oColor := oStatusBar:KeyIndicatorColor
	ELSE
		oColor := oStatusBar:DisabledKeyIndicatorColor
	ENDIF
	dwOldColor := SetTextColor(strucDrawItem:hDC, oColor:ColorRef)

	// Draw the text
	IF (hFont := SendMessage(strucDrawItem:hwndItem, WM_GETFONT, 0l, 0l)) != NULL_PTR
		SelectObject(strucDrawItem:hDC, hFont)
	ENDIF

	DrawText(strucDrawItem:hDC, String2Psz(cKeyText), INT(SLen(cKeyText)), @strucDrawItem:rcItem, _OR(DT_VCENTER, DT_SINGLELINE))

	// Restore original mode and color
	SetTextColor(strucDrawItem:hDC, dwOldColor)
	SetBkMode(strucDrawItem:hDC, nMode)
	RETURN SELF
END CLASS

