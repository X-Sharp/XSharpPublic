CLASS BaseListBox INHERIT TextControl
	PROTECT MsgGroup 					AS INT
	PROTECT liSavedCurrentItemNo 	AS LONGINT
	PROTECT sSavedCurrentItem 		AS STRING
	

METHOD AddItem(cItem, nItemNumber) 
	LOCAL dwPosition AS DWORD
	LOCAL dwMessType AS LONGINT
	LOCAL liRetVal AS  LONGINT
	
	
	IF !IsString(cItem)
		WCError{#AddItem,#BaseListBox,__WCSTypeError,cItem,1}:@@Throw()
	ENDIF
	IF !IsNil(nItemNumber)
		IF !IsLong(nItemNumber)
			WCError{#AddItem,#BaseListBox,__WCSTypeError,nItemNumber,2}:@@Throw()
		ENDIF
		IF nItemNumber==0
			dwMessType := LBAddString
		ELSE
			dwPosition	:=nItemNumber-1
			dwMessType 	:= LBInsertString
		ENDIF
	ELSE
		dwMessType := LBAddString
	ENDIF
	
	IF SELF:ValidateControl()
		liRetVal := SendMessage(hwnd, LBMessages[dwMessType,MsgGroup], dwPosition, LONGINT(_CAST, String2Psz(cItem)))
		
		IF liRetVal < LB_Okay
			liRetVal := 0
		ELSE
			liRetVal++
		ENDIF
	ENDIF
	
	RETURN liRetVal
	

METHOD Clear() 
	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), LBMessages[LBResetContent,MsgGroup], 0, 0)
	ENDIF
	RETURN SELF
	

ACCESS CurrentItem 
	RETURN NULL_STRING
	

ACCESS CurrentItemNo 
	RETURN 0
	

ACCESS CurrentText 
	IF IsInstanceOf(SELF, #ComboBox)
		RETURN SUPER:CurrentText
	ENDIF
	RETURN NULL_STRING
	

METHOD DeleteItem(nItemNumber) 
	LOCAL dwPos AS DWORD
	LOCAL dwTemp AS DWORD
	
	
	
	IF !IsNil(nItemNumber)
		IF !IsLong(nItemNumber)
			WCError{#DeleteItem,#BaseListBox,__WCSTypeError,nItemNumber,1}:@@Throw()
		ENDIF
		IF (nItemNumber != 0)
			dwPos := nItemNumber-1
		ENDIF
	ENDIF
	
	IF (nItemNumber == 0)
		IF ((dwPos := SELF:CurrentItemNo) == 0)
			RETURN FALSE
		ENDIF
		dwPos--
	ENDIF
	
	IF SELF:ValidateControl()
		dwTemp := LBMessages[LBDeleteString,MsgGroup]
		RETURN (SendMessage(hwnd, dwTemp, dwPos, 0) >= 0)
	ENDIF
	
	RETURN FALSE
	

METHOD Destroy() 
	IF IsWindow(hwnd)
		sSavedCurrentItem := SELF:CurrentItem
		liSavedCurrentItemNo := SELF:CurrentItemNo
	ENDIF
	
	RETURN SUPER:Destroy()
	

METHOD FindItem(cItem, lWholeItem, nStart) 
	LOCAL liIndex := LB_ERR AS LONGINT
	LOCAL iMessType AS LONGINT
	//RvdH 080516 Changed from DWORD to LONG (-1 means: search from start)
	LOCAL liStart AS LONGINT
	IF !IsString(cItem)
		WCError{#FindItem,#BaseListBox,__WCSTypeError,cItem,1}:@@Throw()
	ENDIF
	
	iMessType := LBFindStringExact //Default is exact match
	IF !IsNil(lWholeItem)
		IF !IsLogic(lWholeItem)
			WCError{#FindItem,#BaseListBox,__WCSTypeError,lWholeItem,2}:@@Throw()
		ENDIF
		IF !lWholeItem
			iMessType := LBFindString
		ENDIF
	ENDIF
	
	IF !IsNil(nStart)
		IF !IsLong(nstart)
			WCError{#FindItem,#BaseListBox,__WCSTypeError,nStart,3}:@@Throw()
		ENDIF
		liStart := nStart-1
	ENDIF
	
	IF (NULL_STRING == cItem)
		RETURN 0
	ENDIF
	
	IF SELF:ValidateControl()
		liIndex := SendMessage(hwnd, LBMessages[iMessType ,MsgGroup], DWORD(liStart), LONGINT(_CAST, String2Psz(cItem)))
	ENDIF
	
	IF (liIndex < LB_OKAY)
		RETURN 0
	ENDIF
	
	RETURN liIndex+1
	

METHOD GetItem(nItemNumber, nLength) 
	LOCAL liPosition AS LONGINT
	LOCAL liMaxLength AS LONGINT
	LOCAL ptrBuffer AS PTR
	LOCAL cRetVal AS STRING
	LOCAL liLength AS LONGINT
	LOCAL hHandle AS PTR
	IF !IsNil(nItemNumber)
		IF !IsLong(nItemNumber)
			WCError{#GetItem,#BaseListBox,__WCSTypeError,nItemNumber,1}:@@Throw()
		ENDIF
		liPosition := nItemNumber
	ENDIF
	
	IF !IsNil(nLength)
		IF !IsLong(nLength)
			WCError{#GetItem,#BaseListBox,__WCSTypeError,nLength,2}:@@Throw()
		ENDIF
		IF (nLength < 0)
			liLength := 65535
		ELSE
			liLength := nLength
		ENDIF
	ELSE
		liLength := 65535
	ENDIF
	
	IF (liPosition == 0) .AND. ((liPosition := SELF:CurrentItemNo) == 0)
		RETURN ""
	ENDIF
	
	liPosition--
	hHandle := SELF:Handle()
	liMaxLength := SendMessage(hHandle, LBMessages[LBGetTextLen,MsgGroup], DWORD(liPosition), 0 ) + 1
	ptrBuffer := MemAlloc(DWORD(liMaxLength))
	liMaxLength := SendMessage(hHandle, LBMessages[LBGetText,MsgGroup], DWORD(liPosition), LONGINT(_CAST, ptrBuffer))
	IF liMaxLength>=LB_Okay
		IF liMaxLength>liLength
			cRetVal := Mem2String(ptrBuffer,nLength)
		ELSE
			cRetVal := Psz2String(ptrBuffer)
		ENDIF
	ENDIF
	MemFree(ptrBuffer)
	
	RETURN cRetVal
	

CONSTRUCTOR( oOwner, xID, oPoint, oDimension, kStyle, lDataAware) 
	
	
	IF IsInstanceOfUsual(xID,#ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle,lDataAware)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "ListBox", kStyle, lDataAware)
		SELF:SetStyle(_OR(WS_VScroll,WS_Border))
	ENDIF
	//RvdH 060608 optimized
	//IF Empty(LBMessages)
	IF LBMessages[1,1] == 0
		//Initialize the message array
		LBMessages[ 1,1] := LB_ADDSTRING
		LBMessages[ 1,2] := CB_ADDSTRING
		LBMessages[ 2,1] := LB_INSERTSTRING
		LBMessages[ 2,2] := CB_INSERTSTRING
		LBMessages[ 3,1] := LB_DELETESTRING
		LBMessages[ 3,2] := CB_DELETESTRING
		LBMessages[ 4,1] := LB_FINDSTRING
		LBMessages[ 4,2] := CB_FINDSTRING
		LBMessages[ 5,1] := LB_GETCOUNT
		LBMessages[ 5,2] := CB_GETCOUNT
		LBMessages[ 6,1] := LB_GETCURSEL
		LBMessages[ 6,2] := CB_GETCURSEL
		LBMessages[ 7,1] := LB_GETTEXT
		LBMessages[ 7,2] := CB_GETLBTEXT
		LBMessages[ 8,1] := LB_GETTEXTLEN
		LBMessages[ 8,2] := CB_GETLBTEXTLEN
		LBMessages[ 9,1] := LB_RESETCONTENT
		LBMessages[ 9,2] := CB_RESETCONTENT
		LBMessages[10,1] := LB_SETCURSEL
		LBMessages[10,2] := CB_SETCURSEL
		LBMessages[11,1] := LB_FINDSTRINGEXACT
		LBMessages[11,2] := CB_FINDSTRINGEXACT                  
	ENDIF
	MsgGroup := 1
	
	RETURN 
	

ACCESS ItemCount 
	LOCAL dwTemp AS DWORD
	
	IF SELF:ValidateControl()
		dwTemp 	:= LBMessages[LBGetCount, MsgGroup]
		RETURN SendMessage(SELF:Handle(), dwTemp, 0, 0)
	ENDIF
	
	RETURN 0
	

METHOD SetTop(nItemNumber) 
	LOCAL liPosition AS DWORD
	
	IF !IsNil(nItemNumber)
		IF !IsLong(nItemNumber)
			WCError{#SetTop,#BaseListBox,__WCSTypeError,nItemNumber,1}:@@Throw()
		ENDIF
		(liPosition := nItemNumber)
	ENDIF
	IF (liPosition == 0) .AND. ((liPosition:=SELF:CurrentItemNo) == 0)
		RETURN SELF
	ENDIF
	liPosition--
	IF SELF:ValidateControl()
		SendMessage (SELF:Handle(), LB_SetTopIndex, DWORD(liPosition), 0)
	ENDIF
	
	RETURN SELF
	
END CLASS

GLOBAL DIM LBMessages [11,2] AS DWORD
	
	
