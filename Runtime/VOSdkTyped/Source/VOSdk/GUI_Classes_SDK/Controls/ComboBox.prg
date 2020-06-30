


CLASS ComboBox INHERIT ListBox
	PROTECT liComboType AS LONG	// BOXSIMPLE, BOXDROPDOWN or BOXDROPDOWNLIST

    PROPERTY ControlType AS ControlType GET ControlType.ComboBox


    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
		VAR oCombo := (VOComboBox) oC
		oCombo:DropDownHeight := oSize:Height
		RETURN 

	METHOD __EditChange() AS VOID STRICT 
		SELF:Modified := TRUE
		RETURN

	[Obsolete];
	METHOD __InitTextMetrics() AS VOID STRICT 
		RETURN


	METHOD __Update() AS VOID STRICT 
		LOCAL cOldValue AS STRING
		LOCAL oItem AS ListBoxItemValue
		LOCAL nIndex AS LONG
		IF SELF:__IsValid .and. SELF:Modified
			cOldValue := AsString(uValue)
			IF (nIndex := SELF:__List:SelectedIndex) >= 0
				oItem := SELF:__Items[nIndex]
				uValue := oItem:Value
			ELSEIF SELF:__ComboBox:DropDownStyle != System.Windows.Forms.ComboBoxStyle.DropDownList
				uValue  := SELF:__ComboBox:Text
			ELSE
				uValue := NIL
			ENDIF
			SELF:ValueChanged	:= !(cOldValue == AsString(uValue))
			//debout(__ENTITY__, cOldValue + "  ==  " + asString(uValue))
			
			SELF:Modified		:= FALSE
		ENDIF
		RETURN 

	ASSIGN CurrentText(cNewText AS STRING) 
		LOCAL cCurrentText AS STRING
		cCurrentText := SELF:__SetText(cNewText)
		IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
			uValue := ((FieldSpec)SELF:FieldSpec):Val(cCurrentText)
		ELSE
			uValue := cCurrentText
		ENDIF
		RETURN 

	ACCESS EditHandle AS IntPtr
		IF SELF:ValidateControl()
			GuiWin32.GetWindow(SELF:oCtrl:Handle, GW_CHILD)
		ENDIF
		RETURN IntPtr.Zero
	

	ACCESS EditHeight  AS LONG
		IF SELF:ValidateControl()
			RETURN SELF:__ComboBox:Height
		ENDIF
		RETURN 0

	ASSIGN EditHeight(liNewHeight AS LONG) 

		IF SELF:ValidateControl()
			IF liNewHeight > 0
				SELF:__ComboBox:Height := liNewHeight
			ENDIF
		ENDIF
		RETURN 

	METHOD EnableAutoComplete(dwFlags AS DWORD) AS VOID STRICT 
		IF SELF:ValidateControl()
			SELF:__ComboBox:AutoCompleteSource := (System.Windows.Forms.AutoCompleteSource) dwFlags
		ENDIF

	METHOD FillUsing(aContents, symField1, symField2) 
		LOCAL uResult AS USUAL
		IF SELF:ValidateControl()
			uResult := SUPER:FillUsing(aContents, symField1, symField2)
		ENDIF
		RETURN uResult
		
	METHOD Font(oNewFont, lRescal) 
		LOCAL uRet AS USUAL
		IF SELF:ValidateControl()
			uRet := SUPER:Font(oNewFont, lRescal)
		ENDIF
		RETURN uRet


	METHOD Create() AS System.Windows.Forms.Control
		LOCAL oWnd AS System.Windows.Forms.Control
		oWnd := SUPER:Create()
		SELF:__SetComboStyle()
		RETURN oWnd

	PRIVATE METHOD __SetComboStyle() AS VOID STRICT
		IF SELF:__Combobox != NULL
		SWITCH liComboType 
		CASE BOXSIMPLE
			SELF:__ComboBox:DropDownStyle := System.Windows.Forms.ComboBoxStyle.Simple
		CASE BOXDROPDOWN
			SELF:__ComboBox:DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDown
		OTHERWISE // CASE (liComboType == BOXDROPDOWNLIST)
			SELF:__ComboBox:DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList
		END SWITCH
		SELF:__ComboBox:DropDownHeight := oSize:Height
		ENDIF
		RETURN
		

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kComboType, kStyle) 
		LOCAL dwStyle AS LONG
		SELF:cClassName := "ComboBox"
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
			SWITCH liComboType 
			CASE BOXSIMPLE
				SELF:SetStyle( CBS_SIMPLE, TRUE)
			CASE BOXDROPDOWN
				SELF:SetStyle(CBS_DROPDOWN, TRUE)
			CASE BOXDROPDOWNLIST
				SELF:SetStyle(CBS_DROPDOWNLIST, TRUE)
			OTHERWISE
				WCError{#Init,#ComboBox,__WCSTypeError,liComboType,5}:@@Throw()
			END SWITCH
		ELSE
			// When created from resource, make sure that the DotNet combo has the property style
			dwStyle := SELF:dwStyle
			DO CASE
			CASE _AND(dwStyle, CBS_DROPDOWNLIST) == CBS_DROPDOWNLIST		// 3
				liComboType := BOXDROPDOWNLIST
			CASE _AND(dwStyle, CBS_DROPDOWN) == CBS_DROPDOWN		// 2
				liComboType := BOXDROPDOWN
			CASE _AND(dwStyle, CBS_SIMPLE) == CBS_SIMPLE // 1
				liComboType := BOXSIMPLE
			OTHERWISE
				liComboType := BOXSIMPLE
			ENDCASE				
			SELF:__SetComboStyle()
		ENDIF

		RETURN 

	//METHOD RemoveEditBalloonTip() 
	//	RETURN SUPER:RemoveEditBalloonTip(SELF:EditHandle)

	ACCESS MultiSelection AS LOGIC
		RETURN FALSE

	ACCESS ReadOnly()  AS LOGIC
		RETURN SELF:__IsValid .and. SELF:oCtrl:Enabled

	ASSIGN ReadOnly( lReadOnly AS LOGIC)  
		IF SELF:ValidateControl()
			SELF:oCtrl:Enabled := !lReadOnly
		ENDIF
		RETURN 

	ACCESS Size AS Dimension
		LOCAL oSize AS Dimension
		oSize := SUPER:Size
		IF SELF:ValidateControl()
			oSize:Height := SELF:__ComboBox:DropDownHeight
		ENDIF
		RETURN oSize

	ASSIGN Size (oDim AS Dimension)
		LOCAL oSize AS Dimension
		IF SELF:ValidateControl()
			oSize := (Dimension) oDim:Clone()		
			oSize:Height := SELF:EditHeight
			SELF:__Combobox:Size := oSize
			SELF:__ComboBox:DropDownHeight := oDim:Height
		ENDIF
		
	METHOD SetCueBanner(cTitle AS STRING) AS LOGIC
		//PP-030902
		RETURN SUPER:SetCueBanner(cTitle,SELF:EditHandle)

	//METHOD ShowEditBalloonTip(cTitle,cText,dwIcon) 
	//	//PP-030902
	//	RETURN SUPER:ShowEditBalloonTip(cTitle,cText,dwIcon,SELF:EditHandle)

END CLASS

/*
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

ACCESS Length  AS LONG
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
*/
//CLASS ComboBoxExItem INHERIT VObject
//	//SE-060519
//	EXPORT ItemIndex AS INT
//	EXPORT ImageIndex AS INT
//	EXPORT SelectedImageIndex AS INT
//	EXPORT OverlayImageIndex AS INT
//	EXPORT TextValue AS STRING
//	EXPORT Value AS USUAL
//	EXPORT Indent AS INT

//	/*
//	METHOD __GetValues(cbxi AS _winCOMBOBOXEXITEM) AS VOID STRICT 
//	//SE-060519
//	SELF:ItemIndex          := cbxi:iItem + 1l
//	SELF:ImageIndex         := cbxi:iImage + IIF(cbxi:iImage >= 0, 1l, 0l)
//	SELF:SelectedImageIndex := cbxi:iSelectedImage + IIF(cbxi:iSelectedImage >= 0, 1l, 0l)
//	SELF:OverlayImageIndex  := cbxi:iOverlay
//	SELF:Indent             := cbxi:iIndent
//	RETURN

//	METHOD __SetValues(cbxi AS _winCOMBOBOXEXITEM) AS VOID STRICT 
//	//SE-060519
//	LOCAL cItem AS STRING

//	cbxi:mask           := _OR(CBEIF_TEXT, CBEIF_INDENT)
//	cbxi:iItem          := Max(SELF:ItemIndex - 1L,-1L)  	//RvdH 070611 Suggestion from Meinhard
//	cbxi:iIndent        := SELF:Indent

//	cItem               := SELF:TextValue
//	//RvdH 060608 optimized: cItem is a string
//	//IF !Empty(cItem)
//	IF SLen(cItem) > 0
//	//cbxi.pszText := Cast2Psz(cItem)		
//	cbxi:pszText := StringAlloc(cItem)		// RvdH 070615 Cast2Psz is not safe here !	   
//	ENDIF

//	IF SELF:ImageIndex != 0
//	cbxi:mask   := _OR(cbxi:mask, CBEIF_IMAGE)
//	cbxi:iImage := SELF:ImageIndex
//	IF cbxi:iImage > 0
//	cbxi:iImage -=1l
//	ENDIF
//	ENDIF

//	IF SELF:SelectedImageIndex != 0
//	cbxi:mask           := _OR(cbxi:mask, CBEIF_SELECTEDIMAGE)
//	cbxi:iSelectedImage := SELF:SelectedImageIndex
//	IF cbxi:iSelectedImage > 0
//	cbxi:iSelectedImage -=1l
//	ENDIF
//	ENDIF

//	IF SELF:OverlayImageIndex != 0
//	cbxi:mask     := _OR(cbxi:mask, CBEIF_OVERLAY)
//	cbxi:iOverlay := SELF:OverlayImageIndex
//	ENDIF

//	RETURN
//	*/
//	CONSTRUCTOR(cItem, nItemNumber, uRetValue, iImageIdx, iSelectedIdx, iOverlayIdx, iIndent) 
//		//SE-060519

//		IF IsString(cItem)
//			SELF:TextValue := cItem
//		ENDIF

//		IF IsNumeric(nItemNumber)
//			SELF:ItemIndex := nItemNumber
//		ENDIF

//		SELF:Value := uRetValue

//		IF IsNumeric(iImageIdx)
//			SELF:ImageIndex := iImageIdx
//		ENDIF

//		IF IsNumeric(iSelectedIdx)
//			SELF:SelectedImageIndex := iSelectedIdx
//		ENDIF

//		IF IsNumeric(iOverlayIdx)
//			SELF:OverlayImageIndex := iOverlayIdx
//		ENDIF

//		IF IsNumeric(iIndent)
//			SELF:Indent := iIndent
//		ENDIF

//		RETURN 
//END CLASS

