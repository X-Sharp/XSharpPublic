CLASS ComboBox INHERIT ListBox
	METHOD __EditChange() AS VOID STRICT 
	//PP-030923 Update value with control contents on editchange (triggered by window dispatch)
	//PP-040508 Update S.Ebert
	//RvdH 050509 Set Modified Flag and don't assign self:uValue. The rest is handled by the ListBox class
	//SELF:uValue := SELF:__GetText()
	SELF:Modified := TRUE
	RETURN

METHOD __InitTextMetrics() AS VOID STRICT 
	//PP-030828 Strong typing
	//PP-040508 Update S.Ebert
	LOCAL strucRect, strucRect1, strucRect2 IS _WinRect
	LOCAL liDBUnits AS LONGINT
	LOCAL liWidth AS LONGINT
	LOCAL liHeight AS LONGINT
	LOCAL hHandle AS PTR

	hHandle := SELF:Handle()

	GetWindowRect(hHandle, @strucRect2)
	SendMessage(hHandle, CB_GETDROPPEDCONTROLRECT, 0, LONGINT(_CAST,@strucRect1))
	UnionRect(@strucRect, @strucRect1, @strucRect2)

	liWidth   := strucRect:Right  - strucRect:Left
	liHeight  := strucRect:Bottom - strucRect:Top
	liDBUnits := GetDialogBaseUnits()
	wXChars   := DWORD(liWidth)  /LoWord(DWORD(liDBUnits))
	wYChars   := DWORD(liHeight) /HiWord(DWORD(liDBUnits))
	IF wYChars == 0
		wYchars := 1
	ENDIF
	RETURN

ASSIGN CurrentText(cNewText) 
	LOCAL cCurrentText AS STRING
	

	IF !IsString(cNewText)
		WCError{#CurrentText,#ComboBox,__WCSTypeError,cNewText,1}:@@Throw()
	ENDIF

	cCurrentText := SELF:__SetText(cNewText)
	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		uValue := SELF:FieldSpec:Val(cCurrentText)
	ELSE
		uValue := cCurrentText
	ENDIF
	RETURN 

ACCESS EditHandle 
	//PP-030902
	//SE-060519
	IF SELF:ValidateControl()
		RETURN GetWindow(hWnd, GW_CHILD)
	ENDIF
	RETURN NULL_PTR
	

ACCESS EditHeight 

	IF SELF:ValidateControl()
		RETURN SendMessage(hWnd, CB_GETITEMHEIGHT, DWORD(_CAST, -1L), 0L)
	ENDIF

	RETURN 0

ASSIGN EditHeight(liNewHeight) 

	IF SELF:ValidateControl()
		SendMessage(hWnd, CB_SETITEMHEIGHT, DWORD(_CAST, -1L), liNewHeight)
		RETURN liNewHeight
	ENDIF

	RETURN 0

METHOD EnableAutoComplete(dwFlags) 
	//PP-030902
	Default(@dwFlags,SHACF_DEFAULT)
	RETURN ShellAutoComplete(SELF:Edithandle, dwFlags)

METHOD Font(oNewFont, lRescal) 
	LOCAL uRet AS USUAL
	LOCAL hEdit AS PTR


	IF SELF:ValidateControl()
		uRet := SUPER:Font(oNewFont, lRescal)
		//SE-060519
		hEdit := SELF:EditHandle
		IF (hEdit != NULL_PTR)
			SendMessage(hEdit, EM_SETMARGINS, _OR(EC_LEFTMARGIN, EC_RIGHTMARGIN), 0L)
		ENDIF
	ENDIF

	RETURN uRet

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kComboType, kStyle) 
	LOCAL liComboType AS LONGINT
	LOCAL dwStyle AS DWORD


	SUPER(oOwner, xID, oPoint, oDimension, kStyle)

	IF !IsNil(kComboType)
		IF !IsLong(kComboType)
			WCError{#Init,#ComboBox,__WCSTypeError,kComboType,5}:@@Throw()
		ENDIF
		liComboType := kComboType
	ELSE
		liComboType := BoxDropDown
	ENDIF

	IF !IsInstanceOfUsual(xID,#ResourceID)
		IF !IsInstanceOf(SELF, #ComboBoxEx)
			SELF:SetStyle(_OR(WS_CLIPSIBLINGS,WS_CLIPCHILDREN), FALSE)
			dwStyle := _OR(WS_BORDER, WS_VSCROLL)
		ENDIF
		DO CASE
		CASE (liComboType == BOXSIMPLE)
			SELF:SetStyle(_OR(dwStyle, DWORD(_CAST, CBS_SIMPLE)))
		CASE (liComboType == BOXDROPDOWN)
			SELF:SetStyle(_OR(dwStyle, DWORD(_CAST, CBS_DROPDOWN)))
		CASE (liComboType == BOXDROPDOWNLIST)
			SELF:SetStyle(_OR(dwStyle, DWORD(_CAST, CBS_DROPDOWNLIST)))
		OTHERWISE
			WCError{#Init,#ComboBox,__WCSTypeError,liComboType,5}:@@Throw()
		ENDCASE
	ENDIF
	SELF:__ClassName := "ComboBox"

	SELF:MsgGroup := 2

	// fpListFiles := @DlgDirListComboBox()
	// fpSelectedFile := @DlgDirSelectComboBoxEx()

	RETURN 

METHOD RemoveEditBalloonTip() 
	//PP-030902
	RETURN SUPER:RemoveEditBalloonTip(SELF:EditHandle)

ACCESS ReadOnly()  
   LOCAL hEdit AS PTR 
   LOCAL liStyle AS LONG 
	hEdit := GetWindow( SELF:Handle(), GW_CHILD ) 
	liStyle := GetWindowLong( hEdit, GWL_STYLE ) 
   RETURN _AND( liStyle, LONG( ES_READONLY ) ) == ES_READONLY 

ASSIGN ReadOnly( lReadOnly )  
   LOCAL hEdit AS PTR 
	Default(@lReadOnly,TRUE) 
	IF .NOT. IsLogic(lReadOnly) 
 		lReadOnly := TRUE 
	ENDIF 
	hEdit := SELF:EditHandle
	IF lReadOnly 
 		SELF:Disable() 
 		IF (hEdit != NULL_PTR)
 		   EnableWindow( hEdit, TRUE ) 
 		   SendMessage( hEdit, EM_SETREADONLY, 1, 0 ) 
 		ENDIF
	ELSE 
 		SELF:Enable() 
 		IF (hEdit != NULL_PTR)
    		SendMessage( hEdit, EM_SETREADONLY, 0, 0 ) 
      ENDIF
	ENDIF 


RETURN 



METHOD SetCueBanner(cTitle) 
	//PP-030902
	RETURN SUPER:SetCueBanner(cTitle,SELF:EditHandle)

METHOD ShowEditBalloonTip(cTitle,cText,dwIcon) 
	//PP-030902
	RETURN SUPER:ShowEditBalloonTip(cTitle,cText,dwIcon,SELF:EditHandle)

END CLASS

CLASS ComboBoxEx INHERIT ComboBox
	PROTECT oImgList AS ImageList

METHOD AddItem(cItem, nItemNumber, uRetValue, iImageIdx, iSelectedIdx, iOverlayIdx, iIndent) 
	//SE-060519

	Default(@nItemNumber, 0)
	Default(@iSelectedIdx, iImageIdx)

	RETURN SELF:InsertItem(ComboBoxExItem{cItem, nItemNumber, uRetValue, iImageIdx, iSelectedIdx, iOverlayIdx, iIndent})

METHOD DeleteItem(nItemNumber) 
	//SE-060519
	LOCAL lReturnValue AS LOGIC
   LOCAL dwPos        AS DWORD


	IF ! IsNil(nItemNumber)
		IF !IsLong(nItemNumber)
			WCError{#DeleteItem,#ComboBoxEx,__WCSTypeError,nItemNumber,1}:@@Throw()
		ENDIF
		IF (nItemNumber != 0)
			dwPos := nItemNumber-1
		ENDIF
	ELSE
		nItemNumber := 0
	ENDIF

	IF (nItemNumber == 0)
		IF ((dwPos := SELF:CurrentItemNo) == 0)
			RETURN FALSE
		ENDIF
		dwPos--
	ENDIF

   lReturnValue := (SendMessage(SELF:Handle(), CBEM_DELETEITEM, dwPos, 0) != CB_ERR)

	IF lReturnValue
		dwPos++
		ADel(aDisplayValues, dwPos)
		ADel(aRetValues, dwPos)
		ASize(aDisplayValues, ALen(aDisplayValues) - 1)
		ASize(aRetValues, ALen(aRetValues) - 1)
	ENDIF

	RETURN lReturnValue

//	RETURN SUPER:DeleteItem(nItemNumber)

METHOD Dispatch (oEvent) 
	//SE-060518
   LOCAL oEvt AS @@event
   LOCAL hBr  AS PTR

   oEvt := oEvent
   IF oEvt:uMsg == WM_CTLCOLOREDIT //supports Background of ComboBoxEx control
      hBr := SendMessage(GetParent(hWnd), WM_CTLCOLOREDIT, oEvt:wParam, oEvt:lParam)
      IF (hBr != NULL_PTR)
         SELF:EventReturnValue := LONGINT(_CAST, hBr)
         RETURN 1L
      ENDIF
   ENDIF

   RETURN SUPER:Dispatch (oEvt)

ACCESS EditHandle 
	//SE-060519
	IF SELF:ValidateControl()
		RETURN PTR(_CAST, SendMessage(hWnd, CBEM_GETEDITCONTROL, 0, 0L))
	ENDIF
	RETURN NULL_PTR

METHOD GetExCBStyle(kExStyle) 
	//SE-060519
	LOCAL dwExStyle AS DWORD

	dwExStyle := DWORD(_CAST, SendMessage(SELF:handle(), CBEM_GETEXSTYLE,0,0)) //CBEM_GETEXTENDEDSTYLE, 0, 0))

	IF IsLong(kExStyle)
		RETURN (_AND(dwExStyle, DWORD(kExStyle)) > 0)
	ENDIF

   RETURN dwExStyle

METHOD GetItemAttributes(uItemNumber) 
	//SE-060519
	LOCAL oComboBoxExItem AS ComboBoxExItem
	LOCAL cbxi IS _winCOMBOBOXEXITEM

	IF IsNil(uItemNumber) .OR. uItemNumber = 0
		uItemNumber := SELF:CurrentItemNo
	ENDIF

   IF uItemNumber > 0
	   cbxi:mask  := _OR(CBEIF_IMAGE, CBEIF_SELECTEDIMAGE, CBEIF_OVERLAY, CBEIF_INDENT)
	   cbxi:iItem := uItemNumber -1l

	   IF SendMessage(SELF:Handle(), CBEM_GETITEM, 0, LONGINT(_CAST, @cbxi)) != 0
	   	oComboBoxExItem := ComboBoxExItem{aDisplayValues[uItemNumber],uItemNumber,aRetValues[uItemNumber]}
	   	oComboBoxExItem:__GetValues(@cbxi)
		ENDIF
	ENDIF

   RETURN oComboBoxExItem

ACCESS ImageList 

	RETURN oImgList

ASSIGN ImageList(oNewImageList) 
	LOCAL oDim AS Dimension

	oImgList := oNewImageList
	//SE-060520
	IF (SELF:Handle() != NULL_PTR)
		SendMessage(hWnd, CBEM_SETIMAGELIST, 0, LONGINT(_CAST, oImgList:Handle()))
		oDim := SELF:Size
		IF oDim:Height < 80
			oDim:Height := 80
		ENDIF
		SetWindowPos(hWnd, NULL_PTR, 0, 0, oDim:width, oDim:height, _OR(SWP_NOACTIVATE, SWP_NOZORDER,SWP_NOMOVE))
	ENDIF

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kComboType, kStyle) 

	Default(@kComboType, BOXDROPDOWN)
	SUPER(oOwner, xID, oPoint, oDimension, kComboType, kStyle)
	SELF:__ClassName := "ComboBoxEx32"

	dwExStyle := _AND(dwExStyle, DWORD(_CAST, _NOT(WS_EX_CLIENTEDGE)))
	dwStyle := _OR(dwStyle, DWORD(_CAST, CCS_NOMOVEY))
	RETURN 

METHOD InsertItem(uComboBoxExItem) 
	//SE-060519
	LOCAL oComboBoxExItem 	AS ComboBoxExItem
	LOCAL cbxi 					IS _winCOMBOBOXEXITEM
	LOCAL lPosition 			AS INT

   oComboBoxExItem := uComboBoxExItem
   oComboBoxExItem:__SetValues(@cbxi)

	lPosition := SendMessage(SELF:Handle(), CBEM_INSERTITEM, 0, LONGINT(_CAST, @cbxi)) + 1l
   
   //RvdH 070615 Free ptr allocated in __SetValues
	IF (cbxi:pszText != NULL_PSZ)
		MemFree(cbxi:pszText)
	ENDIF

	IF (lPosition != 0)
		SELF:__AddItem(oComboBoxExItem:TextValue, oComboBoxExItem:Value, DWORD(lPosition))
	ENDIF

	RETURN lPosition

ACCESS Length 
	LOCAL lRetVal AS LONGINT
	LOCAL hHandle AS PTR

	
   //SE-060519
	IF (hHandle := SELF:EditHandle) != NULL_PTR
		lRetVal := GetWindowTextLength(hHandle)
		IF (lRetVal == CB_ERR)
			lRetVal := 0
		ENDIF
	ENDIF

	RETURN lRetVal

METHOD SetExCBStyle(kExStyle, lEnable) 
	//SE-060519
	

	IF !IsLong(kExStyle)
		WCError{#SetExCBStyle,#ComboBoxEx,__WCSTypeError,kExStyle,}:@@Throw()
	ENDIF

	IF IsNil(lEnable) .OR. !IsLogic(lEnable)
		lEnable := TRUE
	ENDIF

	SendMessage(SELF:Handle(), CBEM_SETEXTENDEDSTYLE, kExStyle, IIF(lEnable, kExStyle, 0l))

	RETURN SELF

METHOD SetItemAttributes(uComboBoxExItem) 
	//SE-060519
	LOCAL oComboBoxExItem AS ComboBoxExItem
	LOCAL cbxi IS _winCOMBOBOXEXITEM

	oComboBoxExItem := uComboBoxExItem

   IF oComboBoxExItem:ItemIndex > 0
   	oComboBoxExItem:__SetValues(@cbxi)

	   IF SendMessage(SELF:Handle(), CBEM_SETITEM, 0, LONGINT(_CAST, @cbxi)) != 0
         aRetValues[oComboBoxExItem:ItemIndex] := oComboBoxExItem:Value
	   ENDIF
	   //RvdH 070615 Free ptr allocated in __SetValues
		IF (cbxi:pszText != NULL_PSZ)
			MemFree(cbxi:pszText)
		ENDIF

	ENDIF

   RETURN SELF

END CLASS

CLASS ComboBoxExItem INHERIT VObject
	//SE-060519
	EXPORT ItemIndex AS INT
	EXPORT ImageIndex AS INT
	EXPORT SelectedImageIndex AS INT
	EXPORT OverlayImageIndex AS INT
	EXPORT TextValue AS STRING
	EXPORT Value AS USUAL
	EXPORT Indent AS INT

	METHOD __GetValues(cbxi AS _winCOMBOBOXEXITEM) AS VOID STRICT 
	//SE-060519
	SELF:ItemIndex          := cbxi:iItem + 1l
	SELF:ImageIndex         := cbxi:iImage + IIF(cbxi:iImage >= 0, 1l, 0l)
	SELF:SelectedImageIndex := cbxi:iSelectedImage + IIF(cbxi:iSelectedImage >= 0, 1l, 0l)
	SELF:OverlayImageIndex  := cbxi:iOverlay
	SELF:Indent             := cbxi:iIndent
	RETURN

METHOD __SetValues(cbxi AS _winCOMBOBOXEXITEM) AS VOID STRICT 
	//SE-060519
	LOCAL cItem AS STRING

	cbxi:mask           := _OR(CBEIF_TEXT, CBEIF_INDENT)
   cbxi:iItem          := Max(SELF:ItemIndex - 1L,-1L)  	//RvdH 070611 Suggestion from Meinhard
   cbxi:iIndent        := SELF:Indent

   cItem               := SELF:TextValue
   //RvdH 060608 optimized: cItem is a string
   //IF !Empty(cItem)
   IF SLen(cItem) > 0
	   //cbxi.pszText := Cast2Psz(cItem)		
		cbxi:pszText := StringAlloc(cItem)		// RvdH 070615 Cast2Psz is not safe here !	   
   ENDIF

   IF SELF:ImageIndex != 0
   	cbxi:mask   := _OR(cbxi:mask, CBEIF_IMAGE)
   	cbxi:iImage := SELF:ImageIndex
   	IF cbxi:iImage > 0
   		cbxi:iImage -=1l
   	ENDIF
   ENDIF

   IF SELF:SelectedImageIndex != 0
   	cbxi:mask           := _OR(cbxi:mask, CBEIF_SELECTEDIMAGE)
   	cbxi:iSelectedImage := SELF:SelectedImageIndex
   	IF cbxi:iSelectedImage > 0
   		cbxi:iSelectedImage -=1l
   	ENDIF
   ENDIF

   IF SELF:OverlayImageIndex != 0
   	cbxi:mask     := _OR(cbxi:mask, CBEIF_OVERLAY)
   	cbxi:iOverlay := SELF:OverlayImageIndex
   ENDIF

   RETURN

CONSTRUCTOR(cItem, nItemNumber, uRetValue, iImageIdx, iSelectedIdx, iOverlayIdx, iIndent) 
	//SE-060519

	IF IsString(cItem)
		SELF:TextValue := cItem
	ENDIF

	IF IsNumeric(nItemNumber)
		SELF:ItemIndex := nItemNumber
	ENDIF

   SELF:Value := uRetValue

	IF IsNumeric(iImageIdx)
		SELF:ImageIndex := iImageIdx
	ENDIF

	IF IsNumeric(iSelectedIdx)
		SELF:SelectedImageIndex := iSelectedIdx
	ENDIF

	IF IsNumeric(iOverlayIdx)
		SELF:OverlayImageIndex := iOverlayIdx
	ENDIF

	IF IsNumeric(iIndent)
		SELF:Indent := iIndent
	ENDIF

   RETURN 
END CLASS

