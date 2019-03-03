CLASS ListBox INHERIT BaseListBox
	PROTECT aRetValues 		AS ARRAY
	PROTECT aDisplayValues 	AS ARRAY
	PROTECT wSelectNum 		AS DWORD
	// protect fpListFiles as DlgDirList ptr
	// protect fpSelectedFile as _DlgDirSelectEx ptr

	//PP-030828 Strong typing
	//DECLARE METHOD __Reset  //PP-040508 Update S.Ebert

METHOD __AddItem(cItem AS STRING, uRetValue AS USUAL, dwPosition AS DWORD) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL dwRetValLen, dwDispValLen AS DWORD

	Default(@uRetValue, cItem)

	dwDispValLen := ALen(aDisplayValues)
	dwRetValLen  := ALen(aRetValues)

	IF (dwPosition == dwDispValLen + 1)
		AAdd(aDisplayValues, cItem)
	ELSE
		ASize(aDisplayValues, dwDispValLen + 1)
		AIns(aDisplayValues, dwPosition)
		aDisplayValues[dwPosition] := cItem
	ENDIF

	IF (dwPosition == dwRetValLen + 1)
		AAdd(aRetValues, uRetValue)
	ELSE
		ASize(aRetValues, dwRetValLen + 1)
		AIns(aRetValues, dwPosition)
		aRetValues[dwPosition] := uRetValue
	ENDIF
	RETURN

ASSIGN __CurrentItemNo(nItemNo AS INT)  STRICT 
	//PP-030828 Strong typing
	//PP-040508 Update S.Ebert
	

	IF SELF:ValidateControl()
		IF SELF:MultiSelection
			IF nItemNo <= 0
				SendMessage(hWnd, LB_SETSEL, 0, LONGINT(_CAST, 0xFFFFFFFF))
			ELSE
				SendMessage(hWnd, LB_SETSEL, 1, LONGINT(_CAST, nItemNo-1))
			ENDIF
		ELSE
			SendMessage(hWnd, LBMessages[LBSetCurSel,MsgGroup], DWORD(_CAST, nItemNo-1), 0)
		ENDIF
	ENDIF

	RETURN 

METHOD __FindDisplayValue(cValue AS STRING) AS DWORD STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aDisplayValues)
	FOR dwI := 1 UPTO dwCount
	   IF AllTrim(aDisplayValues[dwI]) == cValue
	   	RETURN dwI
	   ENDIF
	NEXT  // dwI

   RETURN 0

METHOD __FindRetValue(cValue AS STRING) AS DWORD STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aRetValues)
	FOR dwI := 1 UPTO dwCount
	   IF AllTrim(AsString(aRetValues[dwI])) == cValue
	   	RETURN dwI
	   ENDIF
	NEXT  // dwI

   RETURN 0

METHOD __Update() AS Control STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL cOldValue AS STRING
	LOCAL dwIndex AS DWORD
	LOCAL cTemp AS STRING
	LOCAL liStyle AS LONGINT
	
	IF SELF:Modified
		cOldValue := AsString(uValue)
		//RvdH 070320 Changed to make sure that for comboboxes VO does the following:
		// - first it determines if an item was selected
		// - in that case it takes the item number
		// - otherwise it takes the text from the EditControl
		// - and tries to locate it in the Display Values
		// This should fix the problem that when someone has duplicate entries in the
		// combobox VO always selects one and never the other
		// Now remains the question why you would fill your combobox with duplicate values ?
		IF IsInstanceOf(SELF, #ComboBox)  
			liStyle := GetWindowLong(SELF:hWnd, GWL_STYLE)
			dwIndex := SELF:CurrentItemNo
			IF dwIndex > 0
				cTemp	:= SELF:aDisplayValues[dwIndex]
			ELSE
				cTemp := SELF:__GetText() // $$$
				//dwIndex := SELF:__FindDisplayValue(cText)
				dwIndex := SELF:__FindDisplayValue(cTemp)		//RvdH 070514 It should have been like this...
			ENDIF
		ELSE
			dwIndex := SELF:CurrentItemNo	
			IF dwIndex > 0
				cTemp	:= SELF:aDisplayValues[dwIndex]
			ELSE
				cTemp := SELF:CurrentItem
			ENDIF
		ENDIF
		IF (dwIndex > 0)			
			uValue := aRetValues[dwIndex]
			SELF:__SetText(aDisplayValues[dwIndex])
		ELSE
			SELF:TextValue := cTemp
		ENDIF
		SELF:ValueChanged := !(cOldValue == AsString(uValue))
		SELF:Modified := FALSE
	ENDIF
	RETURN SELF

ASSIGN __Value(uNewVal AS USUAL)  STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL cSelValue AS STRING
	LOCAL dwIndex AS DWORD
	LOCAL nFindItemNum AS INT

	

	//PP-030924 allow value of NIL to reset control
	IF IsNil(uNewVal)
		SELF:__CurrentItemNo := 0
		SUPER:TextValue := ""
		uValue := uNewVal
	ELSE
		//PP-030924 clear and reload box, clears selection
		//SELF:__reset()
		cSelValue := AllTrim(AsString(uNewVal))
		IF (dwIndex := SELF:__FindRetValue(cSelValue)) > 0
			// select the corresponding display string
			SELF:__CurrentItemNo := 0
			nFindItemNum := SELF:FindItem(aDisplayValues[dwIndex], TRUE)
			IF (nFindItemNum == 0)
				nFindItemNum := SELF:FindItem(aDisplayValues[dwIndex], FALSE)
			ENDIF
			IF nFindItemNum > 0
				SELF:__CurrentItemNo := nFindItemNum
			ENDIF
			SELF:__SetText(aDisplayValues[dwIndex])
			uValue := aRetValues[dwIndex]
		ELSE
			// select the corresponding display string
			SELF:__CurrentItemNo := 0
			nFindItemNum := SELF:FindItem(cSelValue, TRUE)
			IF (nFindItemNum == 0)
				nFindItemNum := SELF:FindItem(cSelValue, FALSE)
				IF nFindItemNum > 0
					IF INT(ALen(aDisplayValues)) >= nFindItemNum
						IF ! cSelValue == aDisplayValues[nFindItemNum]
							nFindItemNum := 0
						ENDIF
					ELSE
						nFindItemNum := 0
					ENDIF
				ENDIF
			ENDIF
			IF nFindItemNum > 0
				SELF:__CurrentItemNo := nFindItemNum
			ENDIF
			SUPER:TextValue := cSelValue
		ENDIF
	ENDIF

	RETURN 


METHOD AddItem(cItem, nItemNumber, uRetValue) 
	LOCAL dwPosition AS DWORD
	IF (dwPosition := SUPER:AddItem(cItem, nItemNumber)) != 0
		SELF:__AddItem(cItem, uRetValue, dwPosition)
	ENDIF

	RETURN dwPosition

ACCESS Caption 
	RETURN cCaption

ASSIGN Caption(cNewCaption) 
	IF !IsString(cNewCaption)
		WCError{#Caption,#ListBox,__WCSTypeError,cNewCaption,1}:@@Throw()
	ENDIF
	RETURN cCaption := cNewCaption

METHOD ChangeSelected(oRange, lEnabled) 
	LOCAL lSet AS LOGIC
	Default(@lEnabled, TRUE)

	IF !IsInstanceOfUsual(oRange,#Range)
		WCError{#ChangeSelected,#ListBox,__WCSTypeError,oRange,1}:@@Throw()
	ENDIF

	IF !IsLogic(lEnabled)
		WCError{#ChangeSelected,#ListBox,__WCSTypeError,lEnabled,2}:@@Throw()
	ENDIF

	lSet := lEnabled

	IF SELF:ValidateControl()
		RETURN SendMessage(SELF:Handle(), LB_SELITEMRANGE, DWORD(_CAST, lSet), (LONGINT(oRange:Max-1)<<16)+oRange:Min-1) != LB_ERR
	ENDIF

	RETURN FALSE

METHOD Clear() 
	aRetValues := {}
	aDisplayValues := {}
	//PP-030924 reset value
	//PP-040421 Issue 12657 if control has fieldspec clear to appropriate empty value
	// proposed change below

	IF SELF:FieldSpec == NULL_OBJECT
		SELF:uvalue := NIL
	ELSE
		SELF:uvalue := EmptyUsual(SELF:FieldSpec:UsualType)
	ENDIF

	RETURN SUPER:Clear()


METHOD ClearSelection() 
	//PP-040508 Update S.Ebert
	IF SELF:ValidateControl()
		SELF:__CurrentItemNo := 0
		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD Create() 
	IF (hWnd == 0) .AND. !IsInstanceOf(SELF, #ComboBox)
		SELF:SetStyle(_OR(LBS_Notify, LBS_NoIntegralHeight))
	ENDIF

	RETURN SUPER:Create()

ACCESS CurrentItem 
	//long boxstyle = GetWindowLong(Handle(), GWL_STYLE)
	//if ((boxstyle & LBS_MULTIPLESEL) && (!GetNoSelected()))
	//	return False
	//endif
	IF (hwnd != NULL_PTR)
		RETURN SELF:GetItem(0)
	ENDIF
	RETURN sSavedCurrentItem

ASSIGN CurrentItem(cValue) 
	IF !IsString(cValue)
		WCError{#CurrentItem,#ListBox,__WCSTypeError,cValue,1}:@@Throw()
	ENDIF
	SELF:CurrentItemNo := SELF:FindItem(cValue)

	RETURN 

ACCESS CurrentItemNo 
	//PP-040508 Update S.Ebert
	LOCAL i AS LONGINT
	LOCAL liLen AS LONGINT
	LOCAL ptrBuffer AS PTR
	LOCAL hHandle AS PTR
	LOCAL hChild AS PTR

	IF SELF:MultiSelection
		RETURN SELF:FirstSelected()
	ENDIF

	hHandle := SELF:Handle()

	IF (hHandle != NULL_PTR)
		i := SendMessage(hHandle, LBMessages[LBGetCurSel,MsgGroup], 0, 0)
		IF SELF:ValidateControl() .AND. LB_ERR != i
			// LB_GETCURSEL does not return -1 if the previous last line
			// has been deleted : it returns the last index which is equal
			// to the new count . We must return -1 as no item is
			// selected .
			IF i == SendMessage(hHandle, LBMessages[LBGetCount,MsgGroup], 0, 0)
				RETURN 0
			ELSE
				RETURN i+1
			ENDIF
		ELSE
			// The user could have typed something in the edit box
			hChild := GetWindow(hHandle, GW_CHILD)
			IF (hChild != 0)
				IF (_AND(GetWindowLong(hHandle, GWL_STYLE), CBS_SIMPLE) != 0)
					hChild := GetNextWindow(hChild, GW_HWNDNEXT)
					IF (hChild == 0)
						RETURN 0
					ENDIF
				ENDIF
				liLen := GetWindowTextLength(hChild)
				IF liLen != 0
					liLen++
					ptrBuffer := MemAlloc(DWORD(liLen))
					GetWindowText(hChild, ptrBuffer, liLen)
					i := SendMessage(hHandle, LBMessages[LBFindStringExact,MsgGroup], 65535, LONGINT(_CAST,ptrBuffer))
					MemFree(ptrBuffer)
					IF (i != LB_ERR)
						RETURN i+1
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ELSE
		RETURN liSavedCurrentItemNo
	ENDIF

	RETURN 0

ASSIGN CurrentItemNo(nItemNo) 
	//SE-060526
	LOCAL cSelValue AS USUAL
	LOCAL dwIndex   AS DWORD
	LOCAL uOldValue AS USUAL

	
	IF !IsLong(nItemNo)
		WCError{#CurrentItemNo,#ListBox,__WCSTypeError,nItemNo,1}:@@Throw()
	ENDIF
	SELF:__CurrentItemNo := nItemNo

	//PP-030923
	IF nItemNo > 0
		cSelValue := SELF:CurrentItem
	ENDIF
	uOldValue := AsString(uValue)
	IF ! IsNil(cSelValue) .AND. (dwIndex := SELF:__FindDisplayValue(AllTrim(cSelValue))) > 0
		//RvdH 060608 optimized
		//IF !Empty(aRetValues)
		IF ALen(aRetValues) > 0
			uValue := aRetValues[dwIndex]
		ENDIF
	ELSE
		uValue := cSelValue
	ENDIF
	SELF:ValueChanged := !(AsString(uValue) == uOldValue)

	RETURN 

ASSIGN CurrentText(cNewText) 
	
	IF !IsString(cNewText)
		WCError{#CurrentText,#ListBox,__WCSTypeError,cNewText,1}:@@Throw()
	ENDIF
	RETURN SELF:__SetText(cNewText)

METHOD DeleteItem(uItemNumber) 
	LOCAL lReturnValue AS LOGIC

	IF IsNil(uItemNumber) .OR. uItemNumber = 0
		uItemNumber := SELF:CurrentItemNo
	ENDIF

	IF lReturnValue := SUPER:DeleteItem(uItemNumber)
		ADel(aDisplayValues, uItemNumber)
		ADel(aRetValues, uItemNumber)
		ASize(aDisplayValues, ALen(aDisplayValues) - 1)
		ASize(aRetValues, ALen(aRetValues) - 1)
	ENDIF

	RETURN lReturnValue

METHOD DeselectItem(nItemId) 
	LOCAL dwItem AS DWORD

	IF !IsLong(nItemId)
		WCError{#DeSelectItem,#ListBox,__WCSTypeError,nItemId,1}:@@Throw()
	ENDIF

	dwItem := nItemId-1
	IF SELF:ValidateControl()
		RETURN SendMessage(SELF:Handle(), LB_SELITEMRANGE, 0, LONGINT(_CAST, (dwItem<<16)+dwItem)) != LB_ERR
	ENDIF

	RETURN FALSE

METHOD EnableItemDrag() 
	IF IsInstanceOf(oFormSurface, #DialogWindow)
		Send(oFormSurface, #__SubClassForDragList)
	ENDIF
	SELF:setstyle(LBS_SORT, FALSE)
	RETURN MakeDragList(SELF:Handle())

METHOD FillUsing(aContents, symField1, symField2) 
	LOCAL wArrLen AS DWORD
	LOCAL wElemLen AS DWORD
	LOCAL wIndex AS DWORD
	LOCAL uElement AS USUAL
	LOCAL uDisplayValue AS USUAL
	LOCAL uDefValue AS USUAL
	LOCAL wPosition AS DWORD

	IF IsInstanceOfUsual(aContents, #DataServer) .AND. IsMethod(aContents, #GetLookUpTable)
		aContents := Send(aContents, #GetLookUpTable, Min(0x7FFF, IVarGet(aContents, #RecCount)), symField1, symField2)
	ELSEIF IsArray(aContents)
		IF !IsNil(symField1) .OR. !IsNil(symField2)
			WCError{#FillUsing,#ListBox,__WCSTypeError,symField1,2}:@@Throw()
		ENDIF
	ELSE
		WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:@@Throw()
	ENDIF

	SELF:Clear()
	SELF:aDisplayValues := {}
	SELF:aRetValues := {}

	IF (wArrLen := ALen(aContents)) > 0
		FOR wIndex := 1 UPTO wArrLen
			uElement := aContents[wIndex]
			IF IsArray(uElement)
				wElemLen := ALen(uElement)
				IF wElemLen = 2
					uDisplayValue := uElement[1]
					uDefValue := uElement[2]
				ELSEIF wElemLen = 1
					uDisplayValue := uElement[1]
					uDefValue := uElement[1]
				ELSE
					WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:@@Throw()
				ENDIF
			ELSE
				uDisplayValue := uElement
				uDefValue := uElement
			ENDIF
			IF !IsString(uDisplayValue)
				uDisplayValue := AsString(uDisplayValue)
			ENDIF
			IF IsInstanceOf(SELF, #COMBOBOXEX)
				wPosition := SELF:AddItem(uDisplayValue)
			ELSE
				wPosition := SUPER:AddItem(uDisplayValue)
			ENDIF
			IF (wPosition != 0)
				IF wPosition > ALen(aDisplayValues)
					AAdd(aDisplayValues, uDisplayValue)
					AAdd(aRetValues, uDefValue)
				ELSE
					ASize(aDisplayValues, ALen(aDisplayValues) + 1)
					ASize(aRetValues, ALen(aRetValues) + 1)
					AIns(aDisplayValues, wPosition)
					AIns(aRetValues, wPosition)
					aDisplayValues[wPosition] := uDisplayValue
					aRetValues[wPosition] := uDefValue
				ENDIF
			ENDIF
		NEXT
	ENDIF

	RETURN SELF

METHOD FirstSelected ( ) 
	LOCAL dwNumSelected AS DWORD
	LOCAL iResult AS INT
	LOCAL ptrSelected	AS LONGINT PTR
	LOCAL hHandle AS PTR

	wSelectNum := 1
	hHandle := SELF:Handle()

	IF SELF:ValidateControl() .AND. (dwNumSelected := SELF:SelectedCount) > 0
		ptrSelected := MemAlloc(dwNumSelected * _SIZEOF(LONGINT))
		IF SendMessage(hHandle, LB_GETSELITEMS, dwNumSelected, LONGINT(_CAST,ptrSelected)) != LB_ERR
			iResult := ptrSelected[wSelectNum] + 1
		ENDIF
		MemFree(ptrSelected)
	ENDIF

	RETURN iResult

METHOD GetItemValue(nItemNumber) 
	
	IF (nItemNumber == 0)
		nItemNumber := SELF:CurrentItemNo
	ENDIF

	RETURN aRetValues[nItemNumber]

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	SUPER(oOwner, xID, oPoint, oDimension, kStyle, TRUE)
	aRetValues := {}
	aDisplayValues := {}

	//fpListFiles := @DlgDirList()
	//fpSelectedFile := @_DlgDirSelectEx()
	RETURN 

METHOD IsSelected(iIdx) 
	//PP-040508 Update S.Ebert
	LOCAL dwNumSelected AS DWORD
	LOCAL lResult AS LOGIC
	LOCAL ptrSelected	AS LONGINT PTR
	LOCAL dwPos 		AS DWORD
	LOCAL hHandle 		AS PTR

	hHandle := SELF:Handle()
	lResult := FALSE

	IF SELF:ValidateControl()
		IF ! SELF:MultiSelection
			RETURN (iIdx == SELF:CurrentItemNo)
		ELSE
			dwNumSelected := SELF:SelectedCount
			ptrSelected    := MemAlloc(dwNumSelected * _SIZEOF(LONGINT))
			IF SendMessage(SELF:Handle(), LB_GETSELITEMS, dwNumSelected, LONGINT(_CAST,ptrSelected)) != LB_ERR
				dwPos := 1        
				iIdx	-= 1
				FOR dwPos := 1 TO dwNumSelected
					IF ptrSelected[dwPos] == iIdx
						lResult := TRUE
						EXIT
					ENDIF
				NEXT
			ENDIF
			MemFree(ptrSelected)
		ENDIF
	ENDIF

	RETURN lResult

ACCESS ItemCount 
	//PP-030505 Bug:168
	RETURN ALen(SELF:aRetValues)

METHOD ListFiles(sStartDir, oFixedText, FileTypes) 
	LOCAL pPath AS PSZ
	LOCAL i,iRet AS INT
	LOCAL dwFileTypes AS DWORD
	LOCAL w AS DWORD

	Default(@sStartDir, "*.*")

	pPath := StringAlloc(sStartDir)
	pPath := MemRealloc(pPath, 261)

	IF !IsNil(FileTypes)
		dwFileTypes := FileTypes
	ELSE
		dwFileTypes := _OR(DDL_DIRECTORY, DDL_DRIVES)
	ENDIF

	IF !IsNil(oFixedText)
		w := oFixedText:ControlID
	ENDIF

	//iRet := call(fpListFiles, oParent:Handle(), pPath, wID, w, dwFileTypes)
	iRet := DlgDirList(oParent:Handle(), pPath, INT(wID), INT(w), dwFileTypes)

	MemFree(pPath)
	iRet := SUPER:ItemCount
	//RvdH 050602, Bug [12898] Now fill aRetValues and aDisplayValues array with the file names
   aRetValues 		:= ArrayNew(iRet)
   aDisplayValues := ArrayNew(iRet)
   FOR i := 1 TO iRet
   	aRetValues[i] 		:= SELF:GetItem(i)
   	aDisplayValues[i] := aRetValues[i]
   NEXT



	RETURN (iRet != 0)

ACCESS MultiSelection 
	//PP-040508 from S.Ebert

	IF MsgGroup = 1 //ListBox
		IF _AND(GetWindowLong(hWnd, GWL_STYLE), _OR(LBS_MULTIPLESEL, LBS_EXTENDEDSEL)) != 0
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

METHOD NextSelected() 
	LOCAL dwNumSelected AS DWORD
	LOCAL iResult AS INT
	LOCAL ptrSelected		AS LONGINT PTR
	LOCAL hHandle AS PTR

	hHandle := SELF:Handle()

	IF SELF:ValidateControl() .AND. (dwNumSelected := SELF:SelectedCount) > wSelectNum
		ptrSelected := MemAlloc(dwNumSelected * _SIZEOF(LONGINT))
		IF SendMessage(SELF:Handle(), LB_GETSELITEMS, dwNumSelected, LONGINT(_CAST, ptrSelected)) != LB_ERR
			wSelectNum 	:= wSelectNum + 1
			iResult 		:= ptrSelected[wSelectNum]+1								
		ENDIF
		MemFree(ptrSelected)
	ENDIF

	RETURN iResult

ACCESS SelectedCount 
	LOCAL liNumSelected AS LONGINT

	IF SELF:ValidateControl()
		liNumSelected := SendMessage(SELF:Handle(), LB_GETSELCOUNT, 0, 0)
		IF (liNumSelected >= LB_OKAY)
			RETURN liNumSelected
		ENDIF
	ENDIF

	RETURN 0

ACCESS SelectedFile 
	LOCAL pPath AS PSZ
	LOCAL sRet AS STRING

	pPath := MemAlloc(261)
	MemSet(pPath, 0, 261)

	//call(fpSelectedFile, oParent:Handle(), pPath,	260, wID)
	__DlgDirSelectEx(oParent:Handle(), pPath,	260, wID)

	sRet := Psz2String(pPath)
	MemFree(pPath)

	RETURN sRet

METHOD SelectItem(nItemId) 
	LOCAL dwItem AS DWORD

	IF !IsLong(nItemId)
		WCError{#SelectItem,#ListBox,__WCSTypeError,nItemId,1}:@@Throw()
	ENDIF

	dwItem := nItemId-1

	IF SELF:ValidateControl()
		RETURN SendMessage(SELF:Handle(), LB_SELITEMRANGE, 1, MAKELPARAM(WORD(_CAST, dwItem), WORD(_CAST, dwItem))) != LB_ERR
	ENDIF

	RETURN FALSE

METHOD SetTabs(aTabs) 
	LOCAL dwTabs AS DWORD
	LOCAL pTabs  AS INT PTR
	LOCAL dwI    AS DWORD
	//PP-030319 From S Ebert

	IF (dwTabs := ALen(aTabs)) > 0

		pTabs := MemAlloc(dwTabs*_SIZEOF(INT))
		FOR dwI := 1 UPTO dwTabs
			pTabs[dwI] := aTabs[dwI]
		NEXT  // dwI

		SendMessage(hWnd, LB_SETTABSTOPS, dwTabs, LONGINT(_CAST, pTabs))

		MemFree(pTabs)
	ENDIF
	RETURN SELF

ACCESS TextValue 
	//SE-060526
	LOCAL cDataBaseValue AS STRING
	LOCAL dwIndex AS DWORD

	cDataBaseValue := AsString(uValue)

	//RvdH 060608 optimized
	//IF !Empty(aRetValues)
	IF ALen(aRetValues) > 0
		dwIndex := SELF:__FindRetValue(AllTrim(cDataBaseValue))
		IF dwIndex > 0 .AND. ! IsNil(aDisplayValues[dwIndex])
			RETURN aDisplayValues[dwIndex]
		ELSE
			IF IsNil(uValue)
				RETURN NULL_STRING
			ENDIF
		ENDIF
	ENDIF

	IF IsNil(uValue)
		RETURN SELF:CurrentItem
	ENDIF

	RETURN cDataBaseValue

ASSIGN TextValue(cNewText) 
	//SE-060526
	LOCAL cSelValue AS STRING
	LOCAL dwIndex AS DWORD
	LOCAL cOldValue AS STRING

	IF !IsString(cNewText)
		WCError{#TextValue,#ListBox,__WCSTypeError,cNewText,1}:@@Throw()
	ENDIF

	cSelValue := cNewText
	IF (dwIndex := SELF:__FindDisplayValue(AllTrim(cSelValue))) > 0
		// select the corresponding display string
		SELF:__CurrentItemNo := SELF:FindItem(aDisplayValues[dwIndex], FALSE)
		//RvdH 060608 optimized
		//IF !Empty(aRetValues)
		IF ALen(aRetValues) > 0
			cOldValue 	:= AsString(uValue)
			uValue 		:= aRetValues[dwIndex]
			SELF:__SetText(aDisplayValues[dwIndex])
			SELF:ValueChanged := !(cOldValue == AsString(uValue))
		ELSE
			SUPER:TextValue := aDisplayValues[dwIndex]
		ENDIF
	ELSE
		SELF:__CurrentItemNo := SELF:FindItem(cSelValue, FALSE)
		SUPER:TextValue := cSelValue
	ENDIF

	RETURN 

END CLASS

_DLL FUNCTION __DlgDirSelectEx( hDlg AS PTR, lpString AS PSZ, nCOunt AS INT,;
	nIDListBox AS INT) AS LOGIC PASCAL:USER32.DlgDirSelectExA


