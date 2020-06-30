


#define SBGRIPSIZE 15
#define SBMAXITEMS 255
#define SBMAXMESSAGE 512
#define SBMINMESSAGE 30

// The DotNet object is the StatusStrip class (which inherits from ToolStrip)
// The items on the StatusBar in dotNet are all ToolStripStatusLabel
// Dotnet allows more types but these are not used.
// The tag of the item contains the symbolic name
// 

CLASS StatusBar INHERIT Control
	PROTECT aItems                AS ARRAY
	PROTECT aMessages             AS ARRAY
	PROTECT aTipText              AS ARRAY


	PROTECT iInsertOn             AS INT
	PROTECT iNumLockOn            AS INT
	PROTECT iScrollOn             AS INT
	PROTECT iCapsLockOn           AS INT

	PROTECT nCurrentPriority      AS INT
	PROTECT nTimeOut              AS INT
	PROTECT nTimeCount            AS INT
	PROTECT lTimeOutSet           AS LOGIC

	PROTECT cLastPermanentMessage AS STRING
	PROTECT cLastControlMessage   AS STRING
	PROTECT cLastErrorMessage     AS STRING
	PROTECT cLastMenuMessage      AS STRING

	PROTECT lErrorMessageBeep     AS LOGIC

	PROTECT nHorizontalBorder     AS INT
	PROTECT nItemBorder           AS INT
	PROTECT nVerticalBorder       AS INT

	PROTECT oKeyColor             AS Color
	PROTECT oDisabledColor        AS Color


    PROPERTY ControlType AS ControlType GET ControlType.StatusBar

	METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
		VAR oControl := (VOStatusStrip) oC
		oControl:Stretch := TRUE
		oControl:ShowItemToolTips := TRUE
		oControl:CanOverflow := TRUE
		RETURN 

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



	ACCESS __StatusStrip AS VOStatusStrip
		IF oCtrl == NULL_OBJECT
			SELF:Create()
		ENDIF
		RETURN (VOStatusStrip) oCtrl


	
	METHOD __GetLabel(sName as STRING) as System.Windows.Forms.ToolStripStatusLabel
		FOREACH Label as System.Windows.Forms.ToolStripStatusLabel in __StatusStrip:Items
			IF Label:Name == sName
				RETURN Label
			ENDIF
		NEXT
		RETURN NULL_OBJECT
	

	METHOD __AutoSize() AS VOID STRICT 
		// Not needed
		RETURN

	METHOD __BuildItems() AS StatusBar STRICT 
		LOCAL dwCount       AS DWORD
		LOCAL dwItemCount   AS DWORD
		LOCAL oItem         AS StatusBarItem
		LOCAL oLabel        as System.Windows.Forms.ToolStripStatusLabel
		dwItemCount := ALen(aItems)

		// Adjust StatusBarItems to fit across StatusBar
		SELF:__AutoSize()
		SELF:__StatusStrip:Items:Clear()

		// Allocate a static array and fill it with StatusBarItem information
		FOR dwCount := 1 UPTO dwItemCount
			oItem := aItems[dwCount] 
			oLabel := VOStatusItem{SELF}
			oLabel:Name := oItem:NameSym
			oLabel:Text := oItem:Value
			oLabel:BorderStyle := System.Windows.Forms.Border3DStyle.Flat
			IF oItem:NameSym == #MessageArea
				oLabel:Spring := TRUE
				oLabel:TextAlign := System.Drawing.ContentAlignment.MiddleLeft
				oLabel:BorderSides := System.Windows.Forms.ToolStripStatusLabelBorderSides.None
			ELSE
				oLabel:BorderSides := System.Windows.Forms.ToolStripStatusLabelBorderSides.Right
				oLabel:AutoSize := TRUE
			ENDIF
			
			if oItem:Icon != null
				oLabel:Image  := (System.Drawing.Image) (OBJECT) oItem:Icon:__Icon
			ENDIF
			__StatusStrip:Items:Add(oLabel)	
		NEXT // dwCount
		iInsertOn := iNumLockOn := iScrollOn := iCapsLockOn := -1
		
		RETURN SELF

	METHOD __GetBorderWidths() AS StatusBar STRICT 
		//Todo: BorderWidths
		//local oPadding as System.Windows.Forms.Padding

		//oPadding := SELF:__StatusStrip:Padding
		//nHorizontalBorder	:= oPadding:Horizontal
		////nItemBorder			:= oPadding:
		//nVerticalBorder		:= oPadding:Vertical

		RETURN SELF

	METHOD __GetItemFromSymbol(symItemName AS SYMBOL) AS DWORD STRICT 
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


	METHOD __GetKeyState(bKey AS BYTE) AS LOGIC STRICT 
		LOCAL aKeyStates AS BYTE[]
		aKeyStates := BYTE[]{256}
		GuiWin32.GetKeyboardState(aKeyStates)
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
		LOCAL cText AS STRING
		// Lookup message area index by default
		Default(@symItemName, #MessageArea)
		LOCAL IMPLIED Label := SELF:__GetLabel((STRING) symItemName)
		IF Label != NULL_OBJECT
			cText := Label:Text
		ENDIF
		RETURN AllTrim(cText)

	METHOD __InitItems() AS StatusBar STRICT 
		// Not needed
		RETURN SELF


	METHOD __SetKeyState(bKey AS BYTE, lTurnOn AS LOGIC) AS VOID STRICT 
		LOCAL lCurrentlyOn AS LOGIC
		lCurrentlyOn := SELF:__GetKeyState(bKey)
		IF (lCurrentlyOn .AND. !lTurnOn) .OR. (!lCurrentlyOn .AND. lTurnOn)
			SELF:__ToggleKeyState(bKey)
		ENDIF
		RETURN


	METHOD __ToggleKeyState(bKey AS BYTE) AS VOID STRICT 
		LOCAL aKeyStates AS BYTE[]
		aKeyStates := BYTE[]{256}
		GuiWin32.GetKeyboardState(aKeyStates)
		IF (_AND(aKeyStates[INT(bKey)+1], 1) > 0) // Is Key on
			aKeyStates[bKey+1] := (byte) _AND(aKeyStates[bKey+1],  0xFE) // Turn Key off
		ELSE
			aKeyStates[bKey+1] := (byte) _OR(aKeyStates[bKey+1],  0x01) // Turn Key on
		ENDIF

		GuiWin32.SetKeyboardState(aKeyStates)

		RETURN

	METHOD __UpdateKeyStates() AS VOID STRICT 
		LOCAL iSet AS LONGINT
		LOCAL aKeyStates AS BYTE[]
		LOCAL Label as System.Windows.Forms.ToolStripStatusLabel
		aKeyStates := BYTE[]{256}
		

		IF __GetItemFromSymbol(#InsArea) > 0

			GuiWin32.GetKeyboardState(aKeyStates)

			IF (iSet := _AND(aKeyStates[VK_INSERT + 1], 1)) != iInsertOn
				Label := SELF:__GetLabel(#InsArea)
				if iSet != 0
					Label:ForeColor := oKeyColor
				ELSE
					Label:ForeColor := oDisabledColor
				ENDIF				
				iInsertOn := iSet
			ENDIF

			IF (iSet := _AND(aKeyStates[VK_CAPITAL + 1], 1)) != iCapsLockOn
				Label := SELF:__GetLabel(#CapsLockArea)
				if iSet!= 0
					Label:ForeColor := oKeyColor
				ELSE
					Label:ForeColor := oDisabledColor
				ENDIF				
				iCapsLockOn := iSet
			ENDIF

			IF (iSet := _AND(aKeyStates[VK_NUMLOCK + 1], 1)) != iNumLockOn
				Label := SELF:__GetLabel(#NumLockArea)
				if iSet!= 0
					Label:ForeColor := oKeyColor
				ELSE
					Label:ForeColor := oDisabledColor
				ENDIF				
				iNumLockOn := iSet
			ENDIF

			IF (iSet := _AND(aKeyStates[VK_SCROLL + 1], 1)) != iScrollOn
				Label := SELF:__GetLabel(#ScrollLockArea)
				if iSet!= 0
					Label:ForeColor := oKeyColor
				ELSE
					Label:ForeColor := oDisabledColor
				ENDIF				
				iScrollOn := iSet
			ENDIF

		ENDIF
		RETURN

	METHOD AddItem(oStatusBarItem) 
		AADD(aItems, oStatusBarItem)
		SELF:__BuildItems()
		RETURN NIL

	NEW ACCESS AsString 
		RETURN cLastPermanentMessage

	NEW ASSIGN AsString(cMessage) 
		SELF:SetMessage(cLastPermanentMessage := cMessage, MESSAGEPERMANENT)
		RETURN 

	METHOD ClearItems() 
		FOREACH Label as System.Windows.Forms.ToolStripStatusLabel in __StatusStrip:Items
			Label:Text := String.Empty
		NEXT
		RETURN SELF

	ACCESS ControlText AS STRING
		RETURN cLastControlMessage

	ASSIGN ControlText(cMessage AS STRING) 
		SELF:SetMessage(cLastControlMessage := cMessage, MESSAGECONTROL)
		RETURN 


	METHOD Destroy() AS USUAL CLIPPER
		aMessages := NULL_ARRAY
		aItems := NULL_ARRAY
		RETURN SUPER:Destroy()

	ACCESS DisabledKeyIndicatorColor As Color
		RETURN oDisabledColor

	ASSIGN DisabledKeyIndicatorColor(oColor As Color) 
		oDisabledColor := oColor
		RETURN 

	METHOD OnItemClicked(oItem as System.Windows.Forms.ToolStripItem) AS VOID
		do CASE
		CASE oItem == SELF:__GetLabel(#InsArea)
			SELF:__ToggleKeyState(VK_INSERT)
		CASE oItem == SELF:__GetLabel(#CapsLockArea)
			SELF:__ToggleKeyState(VK_CAPITAL)
		CASE oItem == SELF:__GetLabel(#NumLockArea)
			SELF:__ToggleKeyState(VK_NUMLOCK)
		CASE oItem == SELF:__GetLabel(#ScrollLockArea)
			SELF:__ToggleKeyState(VK_SCROLL)
		ENDCASE
		SELF:__UpdateKeyStates()
		RETURN
	
	METHOD DisplayKeyboard() 
		LOCAL dwCount AS DWORD
		LOCAL dwItemCount AS DWORD
		LOCAL oStatusBarItem AS StatusBarKeyItem
		LOCAL aKeys AS ARRAY
		// Make sure this section hasn't already been added
		IF __GetItemFromSymbol(#InsArea) > 0
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
			oStatusBarItem := StatusBarKeyItem{aKeys[dwCount][1]}
			oStatusBarItem:KeyText := aKeys[dwCount][2]
			SELF:AddItem(oStatusBarItem)
		NEXT  // dwCount
		self:__UpdateKeyStates()

		RETURN SELF

	METHOD DisplayMemory() 
		LOCAL dwItemCount AS DWORD
		LOCAL oStatusBarItem AS StatusBarItem


		// Make sure this section hasn't already been added
		IF __GetItemFromSymbol(#MemoryArea) > 0
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

		RETURN SELF

	METHOD DisplayMessage() 
		LOCAL dwItemCount AS DWORD
		// Make sure this section hasn't already been added
		IF __GetItemFromSymbol(#MessageArea) > 0
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

		RETURN SELF

	METHOD DisplayPosition() 
		LOCAL dwItemCount AS DWORD
		LOCAL oStatusBarItem AS StatusBarItem

		

		// Make sure this section hasn't already been added
		IF __GetItemFromSymbol(#PositionArea) > 0
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

		RETURN SELF

	METHOD DisplayTime() 
		LOCAL dwItemCount AS DWORD
		LOCAL oStatusBarItem AS StatusBarItem

		// Make sure this section hasn't already been added
		IF __GetItemFromSymbol(#TimeArea) > 0
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
		oStatusBarItem:Value := Time()
		SELF:AddItem(oStatusBarItem)

		RETURN SELF

	ACCESS ErrorMessageBeep AS LOGIC
		RETURN lErrorMessageBeep

	ASSIGN ErrorMessageBeep(lEnable as LOGIC) 
		

		lErrorMessageBeep := lEnable

	ACCESS ErrorText( ) as String
		RETURN cLastErrorMessage

	ASSIGN ErrorText(cMessage as String) 
		SELF:SetMessage(cLastErrorMessage := cMessage, MESSAGEERROR)

		RETURN 

	METHOD GetItemBoundingBox(symItemName) 
		LOCAL sName as STRING		
		Default(@symItemName, #MessageArea)
		sName := symItemName
		LOCAL IMPLIED Label := SELF:__GetLabel(sName)
		IF Label != NULL_OBJECT
			RETURN (BoundingBox) Label:Bounds
		ENDIF
		RETURN BoundingBox{}

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
			IF aTipText[dwI][1] == symName
				RETURN aTipText[dwI][2]
			ENDIF
		NEXT  // dwI

		RETURN NULL_STRING


	ACCESS HorizontalBorder  AS LONG
		//SELF:__GetBorderWidths()
		RETURN nHorizontalBorder

	ACCESS InsertMode AS LOGIC
		RETURN SELF:__GetKeyState(VK_INSERT)

	ASSIGN InsertMode(lEnable AS LOGIC) 
		SELF:__SetKeyState(VK_INSERT, lEnable)
		RETURN 

	ACCESS ItemBorder 
		
		//SELF:__GetBorderWidths()
		RETURN nItemBorder

	ACCESS KeyIndicatorColor as Color
		RETURN oKeyColor 

	ASSIGN KeyIndicatorColor(oColor as Color) 
		oKeyColor := oColor
		RETURN 

	ACCESS Length AS LONG
		LOCAL IMPLIED Label := SELF:__GetLabel(#MessageArea)
		IF Label != NULL_OBJECT
			RETURN Label:Text:Length
		ENDIF
		RETURN 0

	ACCESS MenuText AS STRING
		RETURN cLastMenuMessage

	ASSIGN MenuText(cMessage AS STRING) 
		SELF:SetMessage(cLastMenuMessage := cMessage, MESSAGEMENU)
		RETURN 
	//Todo: ODDrawItem
	//METHOD ODDrawItem(oEvent) 
	//	LOCAL oEvt AS @@Event
	//	LOCAL p1   AS _winDRAWITEMSTRUCT
	//	LOCAL oStatusBarItem AS StatusBarItem
	//	LOCAL dwId AS DWORD

	//	oEvt     := oEvent
	//	p1       := PTR(_CAST, oEvt:lParam)

	//	dwId := p1:ItemID + 1
	//	IF dwId > 0 .AND. dwId <= ALen(SELF:aItems)
	//		oStatusBarItem := aItems[dwId]
	//		IF IsMethod(oStatusBarItem, #ODDrawItem)
	//			Send(oStatusBarItem, #ODDrawItem, oEvt, SELF)
	//		ENDIF
	//	ENDIF
	//	RETURN SELF


	ACCESS PermanentText  AS STRING
		RETURN cLastPermanentMessage

	ASSIGN PermanentText(cMessage AS STRING) 
		SELF:SetMessage(cLastPermanentMessage := cMessage, MESSAGEPERMANENT)
		RETURN 

	ASSIGN Position(oPoint) 
		

		SELF:SetPair(oPoint)

		RETURN 

	METHOD RefreshMemoryDisplay(kMemoryType) 
		

		// Default to SYSTEM_FREE (GetFreeSpace())
		Default(@kMemoryType, MEMORY_SYSTEM_FREE)

		#ifndef __VULCAN__  // Memory() not supported in Vulcan
		SELF:SetText(AllTrim(AsString(Memory(kMemoryType))) + " K", #MemoryArea)
		#endif
		RETURN NIL

	METHOD SetIcon(oIcon, symItemName) 
		LOCAL sName as STRING		
		Default(@symItemName, #MessageArea)
		sName := symItemName
		LOCAL IMPLIED Label := SELF:__GetLabel(sName)
		IF Label != NULL_OBJECT
			Label:Image := oIcon:__Icon
		ENDIF
		RETURN oIcon

	METHOD SetMessage(cMessage, nMode) 
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
				GuiWin32.MessageBeep(0xFFFFFFFF)
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

	METHOD SetPair(oPoint as Point) 
		LOCAL cText AS STRING
		cText := AllTrim(AsString(oPoint:X)) + ", " + AllTrim(AsString(oPoint:Y))
		SELF:SetText(cText, #PositionArea)
		RETURN NIL


	METHOD SetText(cText, symItemName) 
		LOCAL sName as STRING
		Default(@symItemName, #MessageArea)
		sName := symItemName
		SELF:Create()
		IF __StatusStrip != NULL_OBJECT
			FOREACH Label AS System.Windows.Forms.ToolStripStatusLabel IN __StatusStrip:Items
				IF Label:Name == sName
					Label:Text := cText
				ENDIF
			NEXT
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
			IF aTipText[dwI][ 1] == symName
				aTipText[dwI][2] := cTipText
				RETURN SELF
			ENDIF
		NEXT  // dwI

		IF aTipText = NULL_ARRAY
			aTipText := {}
		ENDIF
		AADD(aTipText, {symName, cTipText})

		RETURN SELF

	METHOD SetValue(uValue, symItemName) 
		LOCAL sName as STRING
		Default(@symItemName, #MessageArea)
		sName := symItemName
		FOREACH Label as System.Windows.Forms.ToolStripStatusLabel in __StatusStrip:Items
			IF Label:Name == sName
				Label:Text := AsString(uValue)
			ENDIF
		NEXT
		RETURN uValue

	METHOD Show()  AS VOID STRICT
		SUPER:Show()
		SELF:RefreshMemoryDisplay()
		SELF:__UpdateKeyStates()

		RETURN 

	ACCESS TextValue  AS STRING
		RETURN SELF:__GetText()

	ASSIGN TextValue(cText AS STRING) 
		SELF:SetText(cText)
		RETURN 

	ACCESS TimeOut AS INT
		RETURN nTimeOut

	ASSIGN TimeOut(nNewTimeOut AS INT) 
		IF nNewTimeOut < 0
			nNewTimeOut := 0
		ENDIF
		nTimeOut := nNewTimeOut

		RETURN 

	METHOD Timer  CLIPPER
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
				SELF:SetMessage(SELF:PermanentText, MESSAGEPERMANENT)
			ENDIF
		ENDIF

		RETURN NIL

	ACCESS Transient AS STRING
		RETURN cLastErrorMessage

	ASSIGN Transient(cMessage AS STRING) 
		SELF:SetMessage(cLastErrorMessage := cMessage, MESSAGEERROR)
		RETURN 

	ACCESS VerticalBorder AS LONG
		//SELF:__GetBorderWidths()
		RETURN nVerticalBorder
	
END CLASS

CLASS StatusBarItem INHERIT VObject
	PROTECT symItemName AS SYMBOL
	PROTECT nWidth      AS INT
	PROTECT dwStyle     AS LONG
	PROTECT uValue      AS USUAL
	PROTECT oSBIcon     AS Icon 

	ASSIGN __Icon(oIcon AS Icon)  STRICT 
		oSBIcon := oIcon

	ACCESS Icon AS Icon
		RETURN oSBIcon

	CONSTRUCTOR(symName, nWidth, kStyle, oIcon) 
		SUPER()
		SELF:NameSym := symName
		SELF:Width := nWidth
		SELF:Style := kStyle

		IF IsInstanceOfUsual(oIcon, #Icon)
			oSBIcon := oIcon
		ENDIF

		RETURN 

	ACCESS NameSym AS SYMBOL
		RETURN symItemName

	ASSIGN NameSym(symNewItemName AS SYMBOL) 
		symItemName := symNewItemName

	ACCESS Style AS LONG
		RETURN dwStyle

	ASSIGN Style(kStyle AS LONG) 
		dwStyle := kStyle

	ACCESS Value 
		RETURN uValue

	ASSIGN Value(uNewValue) 
		uValue := uNewValue

	ACCESS Width AS LONG
		RETURN nWidth

	ASSIGN Width(nNewWidth AS LONG) 
		nWidth := nNewWidth

END CLASS

CLASS StatusBarKeyItem INHERIT StatusBarItem
	PROTECT cKeyText AS STRING

	CONSTRUCTOR(symName, nWidth, kStyle, oIcon) 
		SUPER(symName, nWidth, kStyle, oIcon)
		RETURN 

	ASSIGN KeyText (cValue AS STRING) 
		cKeyText := cValue
		SUPER:Value := cValue

END CLASS


CLASS VOStatusItem INHERIT System.Windows.Forms.ToolStripStatusLabel
	PROTECT oSB AS StatusBar
	CONSTRUCTOR(loSB AS StatusBar)
		SUPER()
		oSB := loSB
	

END CLASS
