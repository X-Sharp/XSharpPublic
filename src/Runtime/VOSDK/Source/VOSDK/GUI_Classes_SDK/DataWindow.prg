#pragma options ("enforceself", on)
/// <include file="Gui.xml" path="doc/DataWindow/*" />
CLASS DataWindow INHERIT ChildAppWindow
	PROTECT sCurrentView AS SYMBOL
	PROTECT nCCMode AS INT
	PROTECT nLastLock AS INT


	PROTECT lKickingTheBucket AS LOGIC
	PROTECT lChanged AS LOGIC
	PROTECT lLinked AS LOGIC
	PROTECT lControlsEnabled AS LOGIC
	PROTECT lSubForm AS LOGIC
	PROTECT lTopApp AS LOGIC
	PROTECT lAlreadyHasFlock AS LOGIC
	PROTECT lLocked AS LOGIC
	PROTECT lRecordDirty AS LOGIC
	PROTECT lDeleted AS LOGIC
	PROTECT lValidFlag AS LOGIC
	PROTECT lPreventAutoLayout AS LOGIC
	PROTECT lDeferUse AS LOGIC
	PROTECT oDeferUseServer AS DataServer
	PROTECT lAutoScroll AS LOGIC


	PROTECT aSubForms AS ARRAY
	PROTECT aControls AS ARRAY
	PROTECT aConditionalControls AS ARRAY
	PROTECT aRadioGroups AS ARRAY
	// protect oCurrentHelp as HelpDisplay


	PROTECT oFormFrame AS __FormFrame
	PROTECT oGBrowse AS Control
	PROTECT oHLStatus AS Hyperlabel
	PROTECT oSurface AS __FormDialogWindow
	PROTECT oAttachedServer AS DataServer
	PROTECT oDCCurrentControl AS OBJECT
	PROTECT oDCInvalidControl AS Control
	PROTECT oDCInvalidColumn AS DataColumn
	PROTECT lPendingToolBarShow AS LOGIC
	PROTECT lAllowServerClose AS LOGIC
	PROTECT symBrowserClass AS SYMBOL
	// protect hLastControl as ptr
	EXPORT symFormDialog AS SYMBOL
	PROTECT dwDialogStyle  AS DWORD        //SE-070906




	//PP-030828 Strong typing
	//RvdH 041123 Typing of OLE methods
 /// <exclude />
	METHOD __AdjustForm() AS DataWindow STRICT
	//PP-030828 Strong typing
	LOCAL oBoundingBox AS BoundingBox
	LOCAL oTB AS ToolBar






	IF !lSubForm
		oTB := SELF:toolBar
		IF (oTB != NULL_OBJECT) .AND. (oTB:Owner == SELF)
			oBoundingBox := oTB:Clientarea
		ELSE
			oBoundingBox := SELF:CanvasArea
		ENDIF


		IF !lSubForm .AND. !IsNil(SELF:StatusBar)
			oBoundingBox:Size:Height -= SELF:StatusBar:Size:Height
			oBoundingBox:Origin:Y += SELF:statusBar:Size:Height
		ENDIF


		IF (oFormFrame != NULL_OBJECT)
			oFormFrame:ChangeFormSize(oBoundingBox:Origin, oBoundingBox:Size)
		ENDIF
	ELSE
		IF (oSurface != NULL_OBJECT)
			oSurface:GrowToParent()
		ENDIF
	ENDIF
	RETURN SELF




 /// <exclude />
METHOD __AdjustSurface() AS LOGIC STRICT
	//RvdH 030825 Method moved from Ole Classes
	LOCAL strucClientRect IS _winRect
	LOCAL oToolBar 		AS ToolBar
	LOCAL oStatusBar 		AS StatusBar
	LOCAL lToolBarValid := TRUE AS LOGIC
	LOCAL lStatusBarValid := TRUE AS LOGIC






	IF !lSubForm
		oToolBar := SELF:ToolBar
		oStatusBar := SELF:StatusBar


		IF oToolBar == NULL_OBJECT
			lToolBarValid := FALSE
		ELSEIF !oToolBar:IsVisible()
			lToolBarValid := FALSE
		ENDIF


		IF (oStatusBar == NULL_OBJECT)
			lStatusBarValid := FALSE
		ELSEIF !oStatusBar:IsVisible()
			lStatusBarValid := FALSE
		ENDIF


		IF !lToolBarValid .AND. !lStatusBarValid
			RETURN FALSE
		ENDIF


		GetClientRect(SELF:Handle(), @strucClientRect)


		IF lStatusBarValid
			IF oStatusBar:__IsTopAligned
				strucClientRect:top += oStatusBar:Size:Height
			ELSE
				strucClientRect:bottom -= oStatusBar:Size:Height
			ENDIF
		ENDIF


		IF lToolBarValid
			IF oToolBar:__IsTopAligned
				strucClientRect:top += oToolBar:Size:Height
			ELSE
				strucClientRect:bottom -= oToolBar:Size:Height
			ENDIF
		ENDIF
		if !empty(self:__FormWindow)
			MoveWindow(self:__FormWindow:Handle(), strucClientRect.left, strucClientRect.top, ;
				strucClientRect.right - strucClientRect.left, ;
				strucClientRect.bottom - strucClientRect.top, FALSE)
		endif
    	ENDIF


	RETURN TRUE






 /// <exclude />
ACCESS __aRadioGroups AS ARRAY STRICT
	//PP-030828 Strong typing


	RETURN aRadioGroups




 /// <exclude />
METHOD __AutoCreateBrowser() AS DataWindow STRICT
	//PP-030828 Strong typing


	// auto create Gbrowse




	IF IsNil(oGBrowse)
		oGBrowse := CreateInstance(symBrowserClass, SELF)
	ENDIF


	DO CASE
	CASE (sCurrentView == #ViewSwitch)
		Send(oGBrowse, #__NOTIFYChanges, GBNFY_VIEWSWITCH)
	CASE (sCurrentView == #BrowseView)
		Send(oGBrowse, #__NOTIFYChanges, GBNFY_VIEWASBROWSER)
	CASE (sCurrentView == #FormView)
		Send(oGBrowse ,#__NOTIFYChanges, GBNFY_VIEWASFORM)
	END CASE


	IF (lLinked .AND. oAttachedServer IS DataServer)
		Send(oGBrowse, #Use, oAttachedServer)
	ENDIF


	RETURN SELF




 /// <exclude />
METHOD __AutoLayout() AS DataWindow STRICT
	//PP-030828 Strong typing
	LOCAL cField AS STRING
	LOCAL oDFField AS OBJECT
	LOCAL liFieldLen AS LONGINT
	LOCAL liStart AS LONGINT
	LOCAL liFields, liField, liLines AS LONGINT
	LOCAL newControl AS Control
	LOCAL oPoint AS Point
	LOCAL oDimension AS Dimension
	LOCAL oLabelDim AS Dimension
	LOCAL maxLblSize AS INT
	LOCAL maxFldSize AS INT
	LOCAL maxEditSize AS INT
	LOCAL editHeight AS INT
	LOCAL editGap AS INT
	LOCAL TextMetric IS _winTextMetric
	LOCAL cType AS STRING
	LOCAL _hDC AS PTR
	LOCAL arUsedKeys AS ARRAY
	//LOCAL lOleAvail := IsClass(#OleObject) AS LOGIC
	LOCAL lOleAvail := TRUE AS LOGIC
	LOCAL lMCAvail := (gpfnInitCommonControlsEx != NULL)
	LOCAL lOleWarningShown := FALSE AS LOGIC
	LOCAL lBidi := IsBiDi() AS LOGIC
	LOCAL iMaxWidth AS INT
	LOCAL iNewWidth AS INT
#ifdef USE_OLEOBJECT
	LOCAL oOle			AS OleObject
#endif




	IF lPreventAutoLayout
		RETURN SELF
	ENDIF
	//RvdH 070415 limited iMMaxWidth to half the width of the desktop (and not two third)
	//iMaxWidth := (GetSystemMetrics(SM_CXFULLSCREEN) / 3) * 2
	iMaxWidth := (GetSystemMetrics(SM_CXFULLSCREEN) / 2)




	oFormFrame:AutoLayout := TRUE
	oFormFrame:ResetMinSize()
	//oFormFrame:__ResizeParent()
	arUsedKeys := {}


	// get text metrics
	_hDC := GetDC(oSurface:Handle())
	GetTextMetrics(_hDC, @TextMetric)
	ReleaseDC(oSurface:Handle(), _hDC)


	editHeight := TextMetric:tmHeight + 4
	editGap := editHeight + 6


	// Find maximum field label size && number of lines
	liFields := oAttachedServer:FCount
	FOR liField := 1 UPTO liFields
		oDFField := oAttachedServer:DataField(liField)
		IF (oDFField == NULL_OBJECT)
			LOOP
		ENDIF


		cType := oDFField:FieldSpec:ValType
		IF (cType != "O") .OR. lOleAvail
			liLines++
			// RvdH 041123 Adjusted call to __GetDfCaption
			cField := __GetDFCaption(oDFField,{})
			oDimension := oSurface:SizeText(cField)
			IF (oDimension:Width > maxLblSize)
				maxLblSize := oDimension:Width
				oLabelDim := oDimension
			ENDIF
			IF (cType == "M")
				liLines += 1
			ELSEIF ((cType == "O") .AND. lOleAvail) .OR. (cType == "X")
				liLines += 6
			ELSE
				maxFldSize := Max(maxFldSize, oDFField:FieldSpec:Length)
			ENDIF
		ELSEIF !lOLEWarningShown
			TextBox{SELF, ResourceString{__WCSWarning}:value, ResourceString{__WCSNoOLESupport}:value, BUTTONOKAY}:Show()
			lOleWarningShown := TRUE
		ENDIF
	NEXT //liStart


	// Add the fields
	liField := 0
	//maxEditSize := (maxFldSize+1) * TextMetric.tmAveCharWidth
	maxEditSize := Min(iMaxWidth, (maxFldSize+1) * IIF(lBidi, TextMetric:tmMaxCharWidth, TextMetric:tmAveCharWidth))


	oPoint := Point{IIF(lBidi, (25 + maxEditSize), 20), 0}


	FOR liStart:=1 UPTO liLines
		// get next datafield
		oDFField := NULL_OBJECT
		WHILE (oDFField == NULL_OBJECT) .AND. (liField <= liFields)
			liField++
			oDFField := SELF:oAttachedServer:DataField(liField)
		ENDDO


		IF liField>liFields
			EXIT
		ENDIF


		// Create label
		cType := oDFField:FieldSpec:ValType


		IF (cType != "O") .OR. lOleAvail
			// RvdH 041123 Adjusted call to __GetDfCaption
			//cField := __GetDFCaption(oDFField,, arUsedKeys)
			cField := __GetDFCaption(oDFField,arUsedKeys)
			oPoint:Y := editGap * (liLines - liStart + 1)


			newControl := FixedText{SELF, (100 + liField), oPoint, oLabelDim, cField}
			newControl:Show()


			// Create Data control
			liFieldLen := __GetFSDefaultLength(oDFField:FieldSpec)


			// get the new width first (we need that for BiDi)
			DO CASE
			CASE cType=="L"
				iNewWidth := 4*TextMetric:tmAveCharWidth
			CASE cType=="M"
				iNewWidth := (maxFldSize+1)*TextMetric:tmMaxCharWidth
			CASE cType=="O" .OR. cType=="X"
				iNewWidth := 300
			CASE cType=="D" .AND. lMCAvail
				iNewWidth := 190
			OTHERWISE
				IF (cType == "C")
					iNewWidth := Max(liFieldLen, 2) * TextMetric:tmMaxCharWidth
				ELSE
					iNewWidth := (liFieldLen +1 ) * TextMetric:tmAveCharWidth
				ENDIF
			ENDCASE


			IF (lBidi)
				// s.b. We have to add this otherwise the "C" type fields overlap the text captions.
				oPoint:X := 20 + maxEditSize - Min(iMaxWidth, iNewWidth)
			ELSE
				oPoint:X := 25 + maxLblSize
			ENDIF


			oPoint:Y := editGap * (liLines - liStart + 1)


			DO CASE
			CASE cType=="L"
				newControl := CheckBox{SELF, 200+liField, oPoint, Dimension{iNewWidth, editHeight}, " ", BS_AUTOCHECKBOX}
			CASE cType=="M"
				liStart += 1
				oPoint:Y := editGap * (liLines - liStart + 1)
				newControl := MultiLineEdit{SELF, 200+liField, oPoint, Dimension{iNewWidth, editHeight*2}, ES_AUTOVSCROLL}
#ifdef USE_OLEOBJECT
			CASE cType=="O"
				liStart += 6
				oPoint:Y := editGap * (liLines - liStart + 1)
				oOle := OleObject{ SELF, 200+liField, oPoint, Dimension{iNewWidth, editGap*6+editHeight}, TRUE}
				newControl := oOle
				oOle:AutoSizeOnCreate := FALSE
				oOle:AllowInPlace:= !IsInstanceOf(SELF, #DataDialog) /*.and. IsInstanceOf(oParent, #ShellWindow)*/
				oOle:ActivateOnDblClk:=  TRUE
				oOle:AllowResize:=  TRUE
#endif
			CASE cType=="X"
				liStart += 6
				oPoint:Y := editGap * (liLines - liStart + 1)
				newControl := MultiMediaContainer{SELF, 200+liField, oPoint, Dimension{iNewWidth, editGap*6+editHeight}}
			CASE cType=="D" .AND. lMCAvail
				newControl := DateTimePicker{SELF, 200+liField, oPoint, Dimension{iNewWidth, editHeight}, DTS_LONGDATEFORMAT}
			OTHERWISE
				IF (iNewWidth < iMaxWidth)
					newControl := SingleLineEdit{SELF, 200+liField, oPoint, Dimension{iNewWidth, editHeight}}
				ELSE
					newControl := SingleLineEdit{SELF, 200+liField, oPoint, Dimension{iMaxWidth, editHeight}, ES_AUTOHSCROLL}
				ENDIF
			ENDCASE
			// Link the data editor to the server
			newControl:LinkDF(oAttachedServer, liField)
			// Show it
			newControl:Show()


			oPoint:X := IIF(lBidi, (25 + maxEditSize), 20)
		ENDIF
	NEXT


	oFormFrame:__ResizeParent()


	RETURN SELF




 /// <exclude />
METHOD __CheckConditionalControls() AS DataWindow STRICT
	LOCAL lInvalid as LOGIC
	lInvalid := FALSE
	FOREACH oControl as Control in aControls
        	IF (oControl:Status != NULL_OBJECT)
			lInvalid := TRUE
			EXIT
		ENDIF
	NEXT


	IF !IsNil(oHLStatus) .OR. lInValid
		SELF:DisableConditionalControls()
	ELSE
		SELF:EnableConditionalControls()
	ENDIF
	RETURN SELF




 /// <exclude />
METHOD __CheckRecordStatus() AS LOGIC STRICT
	//PP-030828 Strong typing
	LOCAL oOldStatus AS Hyperlabel
	LOCAL oTempStatus AS Hyperlabel


	oOldStatus := oHLStatus


	IF (IsNil(oAttachedServer))
		oHLStatus := HyperLabel{#NoAttachedServer, #NoAttachedServer}
	ELSE
		SELF:__UpdateCurrent()
		IF !SELF:StatusOK()
			IF (sCurrentView == #FormView)
				IF (oDCInvalidControl != NULL_OBJECT)
					oTempStatus:=oHLStatus //Save status accross SetFocus
					oDCInvalidControl:SetFocus()
					oHLStatus := oTempStatus
				ENDIF
			ELSEIF (sCurrentView == #BrowseView)
				// Jump to error column for browse view
				IF (oDCInvalidColumn != NULL_OBJECT) .AND. IsMethod(oGBrowse, #SetColumnFocus)
					Send(oGBrowse, #SetColumnFocus, oDCInvalidColumn)
				ENDIF
			ENDIF
		ELSEIF IsMethod(SELF, #ValidateRecord)
			IF !Send(SELF, #ValidateRecord)
				IF IsNil(oHLStatus)
					oHLStatus := HyperLabel{#InvalidRecord, #RecInvalid}
				ENDIF
			ENDIF
		ENDIF
		// Prevalidate based on status change
		IF (oHLStatus != oOldStatus)
			SELF:PreValidate()
		ENDIF
	ENDIF
	IF (!IsNil(oHLStatus))
		SELF:__UpdateStatus()
		RETURN FALSE
	ENDIF


	RETURN TRUE




 /// <exclude />
METHOD __Delete() AS LOGIC STRICT
	//PP-030828 Strong typing
	// DataWindow : Delete
	// This method deletes the current record.
	// Depending on the setting of SET DELETE, the record may or may not be still
	// available for viewing. If SET DELETE is set to ON, the record will not be shown
	// in subsequent browsing of the DataServer.


	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:= NULL_OBJECT // assume success
	IF oAttachedserver!=NULL_OBJECT
		IF SELF:__CheckRecordStatus()
			lRetCode := oAttachedServer:Delete()
			IF lRetCode
				IF SetDeleted() .OR. IsInstanceOf(oAttachedServer,#SQLSelect)
					SELF:Skip(1)
					oHLStatus:=oAttachedServer:Status
					IF oAttachedServer:EOF
						SELF:Skip(-1)
						oHLStatus:=oAttachedServer:Status
					ENDIF
				ENDIF
			ELSE
				oHLStatus:=oAttachedServer:Status
				IF IsNil(oHLStatus) // need default status info
					oHLStatus := HyperLabel { #NoDelete, ResourceString{__WCSDeleteFailed}:value, ResourceString{__WCSDeleteFailedMSG}:value }
				ENDIF
			ENDIF
		ELSE
			oHLStatus:=oAttachedServer:Status
		ENDIF
	ENDIF
	RETURN SELF:__UpdateStatus()






 /// <exclude />
METHOD __DoValidate(oControl AS Control) AS DataWindow STRICT
	//PP-030828 Strong typing
	LOCAL dwI, dwCount AS DWORD
	LOCAL oRBG AS RadioButtonGroup


	IF oControl IS RadioButton
		//SE-060526
		dwCount := ALen(aRadioGroups)
		FOR dwI := 1 UPTO dwCount
			oRBG := aRadioGroups[dwI]
			IF oRBG:__IsElement(OBJECT(_CAST, oControl))
				oControl:__Update()
				oControl := oRBG
				EXIT
			ENDIF
		NEXT //dwI
	ENDIF


	IF oControl:Modified
		oControl:__Update()
		IF oControl:ValueChanged
			IF !oControl:PerformValidations()
				oHLStatus := oControl:Status
				SELF:__UpdateStatus()
			ELSE
				oHLStatus := oControl:Status
				SELF:StatusMessage(NULL_STRING, MessageError)
				IF !IsNil(oAttachedServer)
					IF !oControl:__Gather()
						oHLStatus := oAttachedServer:Status
						SELF:__UpdateStatus()
					ENDIF
				ENDIF
			ENDIF
			SELF:PreValidate()
			oControl:ValueChanged := TRUE
		ENDIF
	ENDIF


	RETURN SELF




 /// <exclude />
METHOD __DoValidateColumn(oColumn AS DataColumn) AS DataWindow STRICT
	//PP-030828 Strong typing


	IF oColumn:Modified
		oColumn:__Update()
		IF oColumn:ValueChanged
			IF !oColumn:PerformValidations()
				oHLStatus := oColumn:Status
				SELF:__UpdateStatus()
			ELSE
				oHLStatus := oColumn:Status
				SELF:StatusMessage(NULL_STRING, MessageError)
				IF !IsNil(oAttachedServer)
					oColumn:__Gather()
				ENDIF
			ENDIF
			SELF:PreValidate()
			oColumn:ValueChanged := TRUE
		ENDIF
	ENDIF


	RETURN SELF


 /// <exclude />
METHOD __EnableHelpCursor(lEnabled AS LOGIC) AS Window STRICT
	//PP-030828 Strong typing




	oFormFrame:__EnableHelpCursor(lEnabled)


	RETURN SUPER:__EnableHelpCursor(lEnabled)




 /// <exclude />
METHOD __FindControl(symName AS SYMBOL) AS Control STRICT
	//SE-060526


	FOREACH oControl as Control in aControls
		IF oControl:NameSym == SymName
			RETURN oControl
		ENDIF
	NEXT


	RETURN NULL_OBJECT




 /// <exclude />
METHOD __FindFieldSpec(SymName AS SYMBOL) AS FieldSpec STRICT
	//PP-030828 Strong typing
	//SE-060526
	LOCAL oControl AS Control


	IF (oControl := SELF:__FindControl(symName)) != NULL_OBJECT
		RETURN oControl:FieldSpec
	ENDIF


	RETURN NULL_OBJECT




 /// <exclude />
METHOD __FindHyperLabel(SymName AS SYMBOL) AS HyperLabel STRICT
	//PP-030828 Strong typing
	//SE-060526
	LOCAL oControl AS Control


	IF (oControl := SELF:__FindControl(SymName)) != NULL_OBJECT
		RETURN oControl:HyperLabel
	ENDIF


	RETURN NULL_OBJECT




 /// <exclude />
ACCESS __FormWindow AS __FormFrame STRICT
	//PP-030828 Strong typing


	RETURN oFormFrame




 /// <exclude />
METHOD __Gather() AS LOGIC STRICT
	//PP-030828 Strong typing




	ASend(aControls,#__Gather)


	RETURN IsNil(oHLStatus)




 /// <exclude />
METHOD __GetFormSurface() AS __FormDialogWindow STRICT
	//PP-030828 Strong typing




	RETURN oSurface




 /// <exclude />
METHOD __GetOLEObject(symMethod AS SYMBOL) AS DataWindow STRICT
#ifdef USE_OLEOBJECT
	//RvdH 030825 Methods moved from Ole Classes
	LOCAL hFocus := GetFocus() AS PTR
	LOCAL oOle AS OleObject
	LOCAL dwI, dwCount AS DWORD
	LOCAL oControl AS Control
	LOCAL lRet AS LOGIC


	// neccessary???
	//SE-060526
	dwCount := ALen(aControls)
	FOR dwI := 1 UPTO dwCount
		oControl := aControls[dwI]
		IF oControl:Handle() == hFocus
			oControl := aControls[dwI]
			EXIT
		ENDIF
	NEXT  // dwI


	IF IsInstanceOf(oControl, #OleObject)
		oOle := OBJECT(_CAST, oControl)
		oOle:DetachFromServer()
		lRet := Send(oOle, symMethod)
		IF (SELF:Server != NULL_OBJECT) .AND. (oOle:__GetDataFldPos > 0)
			IF (lRet)
				oOle:ValueChanged :=TRUE
				oOle:Modified :=TRUE
				SELF:Server:RLOCK()
				SELF:FIELDPUT(oOle:__GetDataFldPos, oOle)
				oOle:__Scatter() // ???!!! correct ??
				SELF:Server:Unlock()
			ELSE
				oOle:__Scatter() // ???!!! correct ??
			ENDIF
		ENDIF
		oOle:RePaint()
	ELSE
		MessageBox(0, String2Psz(ResourceString{IDS_NOINSERT}:Value),;
			String2Psz(ResourceString{IDS_OLERUNTIME}:Value),;
			_OR(MB_OK, MB_ICONHAND))
	ENDIF
#endif
	RETURN SELF




 /// <exclude />
METHOD __HandleScrolling(oEvent AS OBJECT) AS DataWindow STRICT
    //PP-030828 Strong typing
    LOCAL oControl as OBJECT
    DO CASE
    CASE oEvent IS ScrollEvent
        oControl := oEvent:Scrollbar
    CASE oEvent IS SpinnerEvent
        oControl := oEvent:Spinner
    CASE oEvent IS SliderEvent
        oControl := oEvent:Slider
    ENDCASE


    IF (oControl != null_object)
        oControl:ThumbPosition := oEvent:Position
        oControl:Modified := true // assume its modified
        SELF:__DoValidate((Control) oControl)
    ENDIF


    RETURN self




 /// <exclude />
METHOD __RegisterFieldLinks(oDataServer AS DataServer) AS LOGIC STRICT
	//PP-030828 Strong typing
	LOCAL dwControls AS DWORD
	LOCAL siDF AS SHORTINT
	dwControls := ALen(aControls)
	IF dwControls > 0
		FOREACH oObject AS OBJECT IN aControls
            IF oObject IS Control VAR oDC
				siDF := oDataServer:FieldPos(oDC:NameSym)
				IF siDF > 0 .AND. IsNil(oDC:Server) // Only one datafield per control
					oDC:LinkDF(oDataServer, siDF) // Exit here, only one control per
					lLinked := TRUE
				ENDIF
			ENDIF
		NEXT
	ELSE
		//PP-030808: Bug 12409
		// Only autolayout if there are _no_ controls
		IF ALen(SELF:GetAllChildren()) == 0
			SELF:__AutoLayout()
			lLinked := TRUE
		ENDIF
	ENDIF


	IF lLinked
		oDataServer:RegisterClient(SELF)
	ENDIF


	RETURN lLinked




 /// <exclude />
METHOD __RegisterSubform(oSubForm AS OBJECT) AS DataWindow STRICT
	//PP-030828 Strong typing


	AAdd(aSubForms, oSubForm)
	RETURN SELF




 /// <exclude />
METHOD __Scatter() AS DataWindow STRICT
    //PP-030828 Strong typing


    IF (sCurrentView == #FormView)
        ASend(aControls, #__Scatter)
        lRecordDirty := FALSE
        self:PreValidate()
    ELSEIF (oGBrowse != null_object)
        Send(oGBrowse, #__RefreshData)
        lRecordDirty := FALSE
        self:PreValidate()
    ENDIF


    RETURN self




 /// <exclude />
METHOD __SetupDataControl(oDC AS Control) AS DataWindow STRICT
    //PP-030828 Strong typing


    IF oDC IS RadioButtonGroup
        AAdd(aRadioGroups, oDC)
    ENDIF


    oFormFrame:AddControl(oDC)
    AAdd(aControls, oDC)


    RETURN self




 /// <exclude />
METHOD __SetupNonDataControl(oDC AS Control) AS DataWindow STRICT
    //PP-030828 Strong typing


    oFormFrame:AddControl(oDC)


    RETURN self




 /// <exclude />
METHOD __StatusMessage(uDescription AS USUAL, nMode AS LONGINT) AS DataWindow STRICT
	//PP-030828 Strong typing
	LOCAL uTemp AS USUAL
	if uDescription is HyperLabel var oHL
		uTemp := oHL:Description
		IF Empty(uTemp) .OR. IsNil(uTemp)
			uTemp := NULL_STRING
		ENDIF
	ELSE
		uTemp := uDescription
	ENDIF


	// sTmp is now a symbol or String


	IF IsSymbol(uTemp)
		uTemp := Symbol2String(uTemp)
	ELSEIF IsNil(uTemp)
		uTemp := NULL_STRING
	ENDIF


	IF !IsString(uTemp)
		uTemp := ResourceString{__WCSUnknownStatusMSG}:value
	ENDIF
	SELF:StatusMessage(uTemp, nMode)
	RETURN SELF




 /// <exclude />
ACCESS __SubForm AS LOGIC STRICT
	//PP-030828 Strong typing
	RETURN lSubForm




 /// <exclude />
METHOD __Unlink() AS LOGIC STRICT
	//PP-030828 Strong typing


	IF oAttachedServer == NULL_OBJECT
		RETURN FALSE
	ENDIF


	FOREACH oDC AS Control in aControls
        	oDC:__Unlink(NIL)
	NEXT
	IF oGBrowse != NULL_OBJECT
		oGBrowse:__Unlink()
	ENDIF
	oAttachedServer:UnRegisterClient(SELF, lAllowServerClose)
	oAttachedServer := NULL_OBJECT
	lLinked := FALSE


	RETURN TRUE




 /// <exclude />
METHOD __UnRegisterDataControl(oControl AS Control) AS DataWindow STRICT
	//PP-030828 Strong typing
	//SE-060526
	LOCAL dwI, dwCount AS DWORD




	// test???
	//RETURN SELF


	dwCount := ALen(aControls)
	FOR dwI := 1 UPTO dwCount
		IF aControls[dwI] = oControl
			ADel(aControls, dwI)
			ASize(aControls, dwCount-1)
			EXIT
		ENDIF
	NEXT  // dwI


	dwCount := ALen(aConditionalControls)
	FOR dwI := 1 UPTO dwCount
		IF aConditionalControls[dwI] = oControl
			ADel(aConditionalControls, dwI)
			ASize(aConditionalControls, dwCount-1)
			EXIT
		ENDIF
	NEXT  // dwI


	dwCount := ALen(aRadioGroups)
	FOR dwI := 1 UPTO dwCount
		IF aRadioGroups[dwI] = oControl
			ADel(aRadioGroups, dwI)
			ASize(aRadioGroups, dwCount-1)
			EXIT
		ENDIF
	NEXT  // dwI


	RETURN SELF




 /// <exclude />
METHOD __UnRegisterSubform(oSubForm AS OBJECT) AS DataWindow STRICT
	//PP-030828 Strong typing
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aSubforms)
	FOR dwI := 1 UPTO dwCount
		IF aSubforms[dwI] == oSubForm
			ADel(aSubForms, dwI)
			ASize(aSubForms, dwCount - 1)
			EXIT
		ENDIF
	NEXT  // dwI


	RETURN SELF




 /// <exclude />
METHOD __UpdateActiveObject() AS DataWindow STRICT
	//RvdH 030825 Method moved from Ole Classes
	//RvdH 041123 Added call to __GetMyOleObjects to retrieve the objects
	LOCAL oOle AS OBJECT
	LOCAL i AS DWORD
	LOCAL aObjects 	AS ARRAY
	aObjects := SELF:__GetMyOleObjects()
	FOR i:= 1 TO ALen(aObjects)
		oOle := aObjects[i]
		IF oOle:IsInPlaceActive
			oOle:UpdateTools()
		ENDIF
	NEXT
	RETURN SELF






 /// <exclude />
METHOD __UpdateCurrent() AS DataWindow STRICT
	//PP-030828 Strong typing
	LOCAL oColumn AS OBJECT


    IF (sCurrentView == #FormView)
		IF oDCCurrentControl IS Control VAR oC .AND. oC:Modified
			SELF:__DoValidate(oC)
		ENDIF
	ELSEIF (sCurrentView == #BrowseView) .AND. IsAccess(oGBrowse, #CurrentColumn)
		oColumn := IVarGet(oGBrowse, #CurrentColumn)
		IF oColumn IS DataColumn VAR oDC .AND. oDC:Modified
			SELF:__DoValidateColumn(oDC)
		ENDIF
	ENDIF
	RETURN SELF




 /// <exclude />
METHOD __UpdateStatus() AS LOGIC STRICT
	//PP-030828 Strong typing
	IF oHLStatus != NULL_OBJECT
    	SELF:__StatusMessage(ResourceString{__WCSError2}:value + oHLStatus:Description, MESSAGEERROR)
		RETURN FALSE
	ENDIF
	// No error
	SELF:__StatusMessage(NULL_STRING, MessageError)


	RETURN TRUE




 /// <exclude />
METHOD __VerifyDataServer(oDataServer AS USUAL) AS LOGIC STRICT
	//PP-030828 Strong typing
	LOCAL dwParmType AS DWORD


	dwParmType := UsualType(oDataServer)
	IF dwParmType == STRING
		oAttachedServer := CreateInstance(#DBServer, oDataServer)
		oAttachedServer:ConcurrencyControl := nCCMode
	ELSEIF dwParmType == SYMBOL
		oAttachedServer := CreateInstance(#DBServer, oDataServer)
		oAttachedServer:ConcurrencyControl := nCCMode
	ELSEIF dwParmType == OBJECT
		IF (oDataServer IS DataServer)
			oAttachedServer := oDataServer
		ELSEIF (oDataServer IS FileSpec)
			oAttachedServer := CreateInstance(#DBServer, oDataServer)
			oAttachedServer:ConcurrencyControl := nCCMode
		ELSE
			RETURN FALSE
		ENDIF
	ELSE
		RETURN FALSE
	ENDIF


	IF IsInstanceOf(oAttachedServer, #DBServer)
		//Use the Used access and not the Status access because the last
		//operation issued before the Use() method may have failed
		IF !IVarGet(oAttachedServer,#Used)
			oHLStatus := oAttachedServer:Status
			IF oHLStatus == NULL_OBJECT
				oHLStatus := Hyperlabel{#Use, , VO_Sprintf(__WCSDSNotOpen, AsString(oAttachedServer), IVarGet(oAttachedServer,#FileSpec):FullPath)}
			ENDIF
			RETURN FALSE
		ENDIF
	ELSEIF IsInstanceOf(oAttachedServer, #SQLSelect) .AND. ; //For SQL server, check for error
		oAttachedServer:Status != NULL_OBJECT .AND. ; //on the Init method
		IVarGet(oAttachedServer,#ErrInfo):FuncSym == #INIT
		oHLStatus := oAttachedServer:Status
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Gui.xml" path="doc/DataWindow.Activate/*" />
METHOD Activate (oEvent)
	//SE-120204
	IF SELF:dwDialogStyle > 0
	    IF (oSurface != NULL_OBJECT)
		    WCAppSetDialogWindow(oSurface:Handle())
	    ENDIF
        RETURN SELF:Default(oEvent)
    ENDIF
    RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.AllowServerClose/*" />
ASSIGN AllowServerClose(lNewVal)


	RETURN (lAllowServerClose := lNewVal)




/// <include file="Gui.xml" path="doc/DataWindow.Append/*" />
METHOD Append()
	// Adds new record to DataWindow
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus := NULL_OBJECT // assume success
	IF (oAttachedServer != NULL_OBJECT) .AND. SELF:__CheckRecordStatus()
		IF !(lRetCode := oAttachedServer:Append())
			oHLStatus := oAttachedServer:Status
			SELF:__UpdateStatus()
		ELSE
			lRecordDirty := TRUE
			// The notification procedure will set up the controls
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.AutoScroll/*" />
ACCESS AutoScroll


	RETURN lAutoScroll




/// <include file="Gui.xml" path="doc/DataWindow.AutoScroll/*" />
ASSIGN AutoScroll(lNewValue)


	RETURN (lAutoScroll := lNewValue)




/// <include file="Gui.xml" path="doc/DataWindow.Background/*" />
ACCESS Background


	//Only an optimization to avoid unneeded Window:PaintBackground() calls of
	//the DataWindow object itself or the __FormFrame.
	IF oSurface != NULL_OBJECT
		RETURN oSurface:Background
	ENDIF

	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/DataWindow.Background/*" />
ASSIGN Background(oBrush)


	//Only an optimization to avoid unneeded Window:PaintBackground() calls of
	//the DataWindow object itself or the __FormFrame.
	IF oSurface != NULL_OBJECT
		oSurface:Background := oBrush
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.Browser/*" />
ACCESS Browser

	RETURN oGBrowse

/// <include file="Gui.xml" path="doc/DataWindow.Browser/*" />
ASSIGN Browser(oDataBrowser)
	// If theres a browser already remove it and set up the new browser


	IF !(oDataBrowser IS Control) .OR. (oDataBrowser == NULL_OBJECT)
		WCError{#Browser,#DataWindow,__WCSTypeError,oDataBrowser,1}:Throw()
	ENDIF


	oGBrowse := oDataBrowser
	oFormFrame:SetGBrowse(oDataBrowser)
	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.BrowserClass/*" />
ACCESS BrowserClass

	RETURN symBrowserClass

/// <include file="Gui.xml" path="doc/DataWindow.BrowserClass/*" />
ASSIGN BrowserClass(symNewClass)

	RETURN (symBrowserClass := symNewClass)




/// <include file="Gui.xml" path="doc/DataWindow.ButtonClick/*" />
METHOD ButtonClick(oControlEvent)
	LOCAL oButton AS Control
	LOCAL oWindow AS DataWindow
	LOCAL dwI, dwCount AS DWORD
	LOCAL oRBG AS RadioButtonGroup
	LOCAL aRadioGrps AS ARRAY
	LOCAL lUnchanged AS LOGIC




	oButton := oControlEvent:Control


	IF (oButton:Owner IS DataWindow)
		oWindow := oButton:Owner
	ELSE
		oWindow := SELF
	ENDIF
	IF oButton IS Button
		oButton:Modified := TRUE // assume its modified
		IF oButton IS RadioButton
			//SE-060526
			aRadioGrps := oWindow:__aRadioGroups
			dwCount := ALen(aRadioGrps)
			FOR dwI := 1 UPTO dwCount
				oRBG := aRadioGrps[dwI]
				IF oRBG:__IsElement(OBJECT(_CAST,oButton))
					lUnchanged := oRBG:__AlreadyHasFocus(OBJECT(_CAST,oButton))
					oRBG:__SetOn(OBJECT(_CAST,oButton))
					EXIT
				ENDIF
			NEXT  // dwI
		ELSEIF ! (oButton IS CheckBox)
			lUnchanged := TRUE
        ENDIF
		oWindow:__DoValidate(oButton)
		oButton:ValueChanged := !lUnchanged
	ENDIF
	SUPER:ButtonClick(oControlEvent)
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Cancel/*" />
METHOD Cancel()
	LOCAL lRetCode AS LOGIC




	lRetCode := TRUE
	IF IsMethod(oAttachedServer,#Refresh)
		lRetCode := Send(oAttachedServer,#Refresh)
	ENDIF
	SELF:EndWindow()


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.CanvasErase/*" />
METHOD CanvasErase()


	IF oSurface != NULL_OBJECT
		oSurface:CanvasErase()
	ENDIF
	RETURN SELF

/// <include file="Gui.xml" path="doc/DataWindow.Caption/*" />
ASSIGN Caption(sNewCaption)

	IF !lTopApp .AND. (lSubForm) // .or. !IsInstanceOf(oParent, #ShellWindow))
		RETURN NULL_STRING
	ENDIF
	RETURN SUPER:Caption := sNewCaption

/// <include file="Gui.xml" path="doc/DataWindow.ChangeFont/*" />
METHOD ChangeFont(oFont, lUpdate)


	IF !(oFont IS Font)
		WCError{#ChangeFont,#DataWindow,__WCSTypeError,oFont,1}:Throw()
	ENDIF


	IF !IsNil(lUpdate)
		IF !IsLogic(lUpdate)
			WCError{#ChangeFont,#DataWindow,__WCSTypeError,lUpdate,2}:Throw()
		ENDIF
	ELSE
		lUpdate := FALSE
	ENDIF


	SELF:Font := oFont
	IF oSurface != NULL_OBJECT
		oSurface:ChangeFont(oFont, lUpdate)
	ENDIF


	RETURN oFont


/// <include file="Gui.xml" path="doc/DataWindow.CheckStatus/*" />
METHOD CheckStatus()
	LOCAL oOldStatus AS OBJECT


	oOldStatus := oHLStatus
	IF !SELF:StatusOK()
		IF sCurrentView == #FormView
			IF (oDCInvalidControl != NULL_OBJECT)
				oDCInvalidControl:SetFocus()
			ENDIF
		ELSEIF (sCurrentView == #BrowseView)
			IF (oDCInvalidColumn != NULL_OBJECT) .AND. IsMethod(oGBrowse, #SetColumnFocus)
				Send(oGBrowse, #SetColumnFocus, oDCInvalidColumn)
			ENDIF
		ENDIF
	ENDIF


	// Prevalidate based on status change
	IF (oHLStatus != oOldStatus)
		SELF:PreValidate()
	ENDIF


	IF (!IsNil(oHLStatus))
		SELF:__UpdateStatus()
		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="Gui.xml" path="doc/DataWindow.Clear/*" />
METHOD Clear()

    if sCurrentView == #FormView
		if oDCCurrentControl is Edit var oEdit
            oEdit:__SetText("")
        elseif oDCCurrentControl is EditWindow var oEW
            oEW:Clear()
		elseif oDCCurrentControl is ControlWindow var oCW
			if oCW:Control != null_object .and. IsMethod(oCW:Control, #Clear)
				oCW:Control:__SetText(null_string)
			ENDIF
		ENDIF
	ELSEIF sCurrentView == #BrowseView
		IF (oGBrowse != NULL_OBJECT) .AND. IsMethod(oGBrowse, #Clear)
			Send(oGBrowse, #Clear)
		ENDIF
	ENDIF

	return self


/// <include file="Gui.xml" path="doc/DataWindow.ClearRelations/*" />
METHOD ClearRelations


	IF oAttachedServer!=NULL_OBJECT
		RETURN Send(oAttachedServer,#ClearRelation)
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/DataWindow.ClipperKeys/*" />
ACCESS ClipperKeys
	IF oSurface != NULL_OBJECT
		RETURN oSurface:ClipperKeys
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/DataWindow.ClipperKeys/*" />
ASSIGN ClipperKeys(lNewValue)
	IF oSurface != NULL_OBJECT
		oSurface:ClipperKeys := lNewValue
		RETURN lNewValue
	ENDIF
	RETURN FALSE




/// <include file="Gui.xml" path="doc/DataWindow.Close/*" />
METHOD Close(oEvent)


	IF (oAttachedServer != NULL_OBJECT)
		IF lRecordDirty .AND. IsNil(oHLStatus )
			IF IsMethod (oAttachedServer, #Commit) .AND. oAttachedServer:Commit()
				lRecordDirty := FALSE
			ENDIF
		ENDIF
		SELF:__Unlink()
	ENDIF
	// Workaround for TreeView Control Loosing focus bug
	SetFocus(GetParent(hwnd))
	SUPER:Close(oEvent)


	RETURN TRUE




/// <include file="Gui.xml" path="doc/DataWindow.Commit/*" />
METHOD Commit()
	LOCAL lRetCode AS LOGIC

	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:= NULL_OBJECT // assume success
	IF oAttachedServer!=NULL_OBJECT .AND. SELF:__CheckRecordStatus()
		IF !(lRetCode:=oAttachedServer:Commit())
			oHLStatus := oAttachedServer:Status
			SELF:__UpdateStatus()
		ELSE
			lRecordDirty := FALSE
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.ConcurrencyControl/*" />
ACCESS ConcurrencyControl


	IF IsNil(oAttachedServer)
		RETURN SELF: nCCMode
	ENDIF


	RETURN oAttachedServer:ConcurrencyControl




/// <include file="Gui.xml" path="doc/DataWindow.ConcurrencyControl/*" />
ASSIGN ConcurrencyControl( nMode)
	LOCAL newMode AS INT




	IF IsString(nMode)
		nMode := String2Symbol(nMode)
	ENDIF


	IF IsSymbol(nMode)
		DO CASE
		CASE nMode == #ccNone
			newMode := ccNone
		CASE nMode == #ccOptimistic
			newMode := ccOptimistic
		CASE nMode == #ccStable
			newMode := ccStable
		CASE nMode == #ccRepeatable
			newMode := ccRepeatable
		CASE nMode == #ccFile
			newMode := ccFile
		OTHERWISE
			WCError{#ConcurrencyControl,#DataWindow,__WCSTypeError,nMode,1}:Throw()
		ENDCASE
	ELSEIF IsNumeric(nMode)
		newMode := nMode
	ELSE
		WCError{#ConcurrencyControl,#DataWindow,__WCSTypeError,nMode,1}:Throw()
	ENDIF

	SELF:nCCMode := newMode
	IF oAttachedServer!=NULL_OBJECT
		oAttachedServer:ConcurrencyControl:=nCCMode
	ENDIF
	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.ContextMenu/*" />
ASSIGN ContextMenu(oNewMenu)
    SELF:SetContextMenu(oNewMenu, #Both)
    RETURN


/// <include file="Gui.xml" path="doc/DataWindow.SetContextMenu/*" />
METHOD SetContextMenu(oNewMenu AS Menu, symView AS SYMBOL) AS VOID
	LOCAL lForm    AS LOGIC
	LOCAL lBrowser AS LOGIC


	IF symView = #BrowseView
		lBrowser := TRUE
	ELSEIF symView = #FormView
		lForm := TRUE
	ELSE
		lForm := lBrowser := TRUE
	ENDIF


	IF lForm
		IF oSurface != NULL_OBJECT
			oSurface:ContextMenu := oNewMenu
		ENDIF
		SUPER:ContextMenu := oNewMenu
	ENDIF


	IF lBrowser
		IF oFormFrame != NULL_OBJECT
			oFormFrame:ContextMenu := oNewMenu
		ENDIF
		IF oGBrowse != NULL_OBJECT
			oGBrowse:ContextMenu := oNewMenu
		ENDIF
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/DataWindow.Controls/*" />
ACCESS Controls
	// DHer: 18/12/2008
	RETURN SELF:aControls


/// <include file="Gui.xml" path="doc/DataWindow.ControlFocusChange/*" />
METHOD ControlFocusChange(oControlFocusChangeEvent)
	//PP-040524 S.Ebert
	LOCAL oControl AS Control
	LOCAL cMessage AS STRING
	LOCAL dwI, dwCount AS DWORD
	LOCAL oRBG AS RadioButtonGroup
	LOCAL oDCHyperLabel AS OBJECT
	LOCAL oCFCE := oControlFocusChangeEvent AS  ControlFocusChangeEvent




	IF oSurface != NULL_OBJECT


		oControl := oCFCE:Control


		IF oCFCE:GotFocus


			IF SELF:AutoScroll
				oControl:__EnsureVisibity()
			ENDIF
			SELF:LastFocus := oControl


			WCAppSetDialogWindow(oSurface:Handle())
			//PP-040410
			IF ! oControl == NULL_OBJECT
				IF oControl IS RadioButton
					//SE-060526
					dwCount := ALen(aRadioGroups)
					FOR dwI := 1 UPTO dwCount
						oRBG := aRadioGroups[dwI]
						IF oRBG:__IsElement(OBJECT(_CAST, oControl))
							oControl := oRBG
							EXIT
						ENDIF
					NEXT  // dwI
				ENDIF


				// save active control
				oDCCurrentControl := oControl


				// if there is an outstanding error on the control - display it
				IF oDCCurrentControl:Status != NULL_OBJECT
					IF IsString(oDCCurrentControl:Status:Description)
						cMessage := oDCCurrentControl:Status:Description
					ELSE
						cMessage := ResourceString{__WCSUnknownStatus}:value
					ENDIF
				ENDIF
				SELF:__StatusMessage(cMessage, MessageError)


				// Reset message for control
				cMessage := NULL_STRING


				// SetUp Prompt
				oDCHyperLabel := oDCCurrentControl:Hyperlabel
				IF oDCHyperLabel != NULL_OBJECT
					cMessage := oDCHyperLabel:Description
				ENDIF
				SELF:__StatusMessage(cMessage, MessageControl)
			ELSE
				SELF:__StatusMessage(NULL_STRING, MessageError)
				SELF:__StatusMessage(NULL_STRING, MessageControl)
				oDCCurrentControl := NULL_OBJECT
			ENDIF


		ELSE
			SELF:__DoValidate(oControl)
		ENDIF
	ENDIF


	RETURN NIL




/// <include file="Gui.xml" path="doc/DataWindow.Copy/*" />
METHOD Copy()




    if (sCurrentView == #FormView)
		if oDCCurrentControl is Edit var oEdit
            oEdit:Copy()
        elseif oDCCurrentControl is EditWindow var oEW
            oEW:Copy()
		elseif oDCCurrentControl is ControlWindow var oCW
			if oCW:Control != null_object .and. IsMethod(oCW:Control, #Copy)
				oCW:Control:Copy()
			ENDIF
		ENDIF
	ELSEIF (sCurrentView == #BrowseView)
		IF (oGBrowse != NULL_OBJECT) .AND. IsMethod(oGBrowse, #copy)
			Send(oGBrowse, #Copy)
		ENDIF
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.CurrentControl/*" />
ACCESS CurrentControl
RETURN oDCCurrentControl


/// <include file="Gui.xml" path="doc/DataWindow.CurrentControl/*" />
ASSIGN CurrentControl(uValue)
RETURN SELF:oDCCurrentControl:=uValue


/// <include file="Gui.xml" path="doc/DataWindow.CurrentView/*" />
ACCESS CurrentView
	RETURN SELF:sCurrentView

/// <include file="Gui.xml" path="doc/DataWindow.Cut/*" />
METHOD Cut()


    if (sCurrentView == #FormView)
		if oDCCurrentControl is Edit var oEdit
            oEdit:Cut()
        elseif oDCCurrentControl is EditWindow var oEW
            oEW:Cut()

		elseif oDCCurrentControl is ControlWindow var oCW
			if oCW:Control != null_object .and. IsMethod(oCW:Control, #Cut)
				oCW:Control:Cut()
			ENDIF
		ENDIF
	ELSEIF (sCurrentView == #BrowseView)
		IF (oGBrowse != NULL_OBJECT) .AND. IsMethod(oGBrowse, #cut)
			Send(oGBrowse, #Cut)
		ENDIF
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.DeActivate/*" />
METHOD DeActivate(oEvent)
	//RvdH 030825 Call to DeactivateAllOLEObjects moved to Window
	RETURN SUPER:Deactivate(oEvent)




/// <include file="Gui.xml" path="doc/DataWindow.DeactivateAllOLEObjects/*" />
METHOD DeactivateAllOLEObjects(oExcept)
	//RvdH 041123 Moved bulk of code to Window class. Only 'Adjust' code remains
	SUPER:DeactivateAllOLEObjects(oExcept)
	IF !lTopApp .AND. IsMethod(SELF:owner, #__AdjustClient)
		SELF:owner:__AdjustClient()
	ELSE
		SELF:__AdjustSurface()
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/DataWindow.DeferUse/*" />
ACCESS DeferUse

	RETURN lDeferUse


/// <include file="Gui.xml" path="doc/DataWindow.DeferUse/*" />
ASSIGN DeferUse(lNewVal)

	RETURN (lDeferUse := lNewVal)


/// <include file="Gui.xml" path="doc/DataWindow.Delete/*" />
METHOD Delete()
	LOCAL nRecno AS LONGINT
	LOCAL nLastRec AS LONGINT
	LOCAL fSQL AS LOGIC
	LOCAL fBrowse AS LOGIC
	LOCAL fRet AS LOGIC


	IF SELF:Server != NULL_OBJECT
		nRecno := SELF:SERVER:Recno
		nLastRec:= SELF:SERVER:LASTREC
		fBrowse := SELF:SCurrentView = #BROWSE
		fSQL := IsInstanceOf(self:SERVER, #SQLSELECT)


		// IF(fSQL)
		// IF(fBrowse)
		// SELF:oGBrowse:Suspendupdate()
		// ENDIF
		// ENDIF
	ENDIF


	fRet := SELF:__Delete()


	IF fSQL .AND. fRet


		#IFDEF __DEBUG
			OutputDebugString(PSZ(" Instance of SQLSelect"))
			OutputDebugString(PSZ(" EOF: " + AsString(SELF:SERVER:EOF)))
			OutputDebugString(PSZ(" BOF: " + AsString(SELF:SERVER:BOF)))
			OutputDebugString(PSZ(" RECNO: " + AsString(SELF:SERVER:Recno)))
			OutputDebugString(PSZ(" LASTREC: " + AsString(SELF:SERVER:LastRec)))
		#ENDIF


		IF nLastRec < nRecNo
			SELF:GoTop()
			SELF:GoBottom()
			// IF(fBrowse)
			// SELF:oGBrowse:Restoreupdate()
			// ENDIF
			RETURN fRet
		ENDIF


		IF SELF:SERVER:EOF .AND. SELF:SERVER:BOF
			SELF:GoTop()
		ELSE
			IF fBrowse
				SELF:Skip()
				// SELF:oGBrowse:Restoreupdate()
			ELSE
				IF nRecno = 1
					SELF:Goto(1)
				ELSE
					SELF:Goto(nRecno-1)
				ENDIF
			ENDIF
		ENDIF
	ENDIF


	RETURN(fRet)



/// <include file="Gui.xml" path="doc/DataWindow.DeleteValidated/*" />
METHOD DeleteValidated
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:= NULL_OBJECT // assume success
	IF oAttachedServer!=NULL_OBJECT .AND. SELF:__CheckRecordStatus()
		IF !(lRetCode:=SELF:__Delete())
			oHLStatus:=oAttachedServer:Status
			SELF:__UpdateStatus()
		ENDIF
	ENDIF
	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	LOCAL oSubForm AS Window


	IF oAttachedServer != NULL_OBJECT
		SELF:__Unlink()
	ENDIF


	IF lSubForm
	    IF oParent IS DataWindow VAR oDW
            oDW:__UnRegisterSubForm(SELF)
		ELSE
		    oParent:__UnRegisterSubForm(SELF)
		ENDIF
	ENDIF


	// If this window has subforms destroy them first
	DO WHILE ALen(aSubForms) > 0
        oSubForm := aSubForms[1]
        oSubForm:Close()
        oSubForm:Destroy()
	END DO
	aSubForms := NULL_ARRAY


	IF oGBrowse != NULL_OBJECT
		oGBrowse:__Unlink()
		oGBrowse:Destroy()
		oGBrowse:= NULL_OBJECT
	ENDIF


	IF oSurface != NULL_OBJECT
		oSurface:Destroy() //This destroys the controls too
		oSurface := NULL_OBJECT
	ENDIF
	aControls := NULL_ARRAY


	IF oFormFrame != NULL_OBJECT
		lKickingTheBucket := TRUE
		oFormFrame:Destroy()
		oFormFrame := NULL_OBJECT
	ENDIF


	oDCCurrentControl := NULL_OBJECT
	SELF:lValidFlag := FALSE


	SUPER:Destroy()


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.DisableConditionalControls/*" />
METHOD DisableConditionalControls()




	IF lControlsEnabled
		ASend(aConditionalControls,#Disable)
		lControlsEnabled := FALSE
	ENDIF
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Dispatch/*" />
METHOD Dispatch(oEvent)
	//RvdH 041123 Removed check for __UpdateActiveObject
	LOCAL oEvt := oEvent AS @@Event
	LOCAL uMsg := oEvt:uMsg AS DWORD
	LOCAL liRet AS LONGINT




	DO CASE
	CASE (uMsg == WM_DESTROY) .AND. lTopApp .AND. lQuitOnClose
		PostQuitMessage(0)
		RETURN (SELF:EventReturnValue := 0L)


	CASE (uMsg == WM_SIZE)  .AND. !(oParent IS ShellWindow)
		liRet := SUPER:Dispatch(oEvt)
		SELF:__UpdateActiveObject()
		RETURN liRet
		/*
		case (uMsg == WM_PARENTNOTIFY)
		if (LoWord(oEvt:wParam) == WM_RBUTTONDOWN) .and. (sCurrentView == #BrowseView)
		if oGBrowse:ContextMenu != NULL_OBJECT
		oGBrowse:ContextMenu:ShowAsPopup(oGBrowse)
		elseif (oContextMenu != NULL_OBJECT)
		oContextMenu:ShowAsPopup(self)
		endif
		endif
		*/
	CASE ((uMsg == WM_ACTIVATE) .AND. (LoWord(oEvt:wParam)!= WA_INACTIVE)) .OR. (uMsg == WM_CHILDACTIVATE)
		IF (oSurface != NULL_OBJECT)
			WCAppSetDialogWindow(oSurface:Handle())
		ENDIF
	ENDCASE


	RETURN SUPER:Dispatch(oEvt)




/// <include file="Gui.xml" path="doc/DataWindow.Draw/*" />
METHOD Draw(oDrawObject)




	IF oSurface != NULL_OBJECT
		oSurface:Draw(oDrawObject)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.EditChange/*" />
METHOD EditChange(oControlEvent)
	LOCAL oCurrentControl := NULL_OBJECT AS OBJECT
	LOCAL oCE := oControlEvent AS ControlEvent


	oCurrentControl := oCE:Control
	IF oCurrentControl IS ListBox .OR. oCurrentControl IS IPAddress
		oCurrentControl:Modified := TRUE // mark it as modified
	ENDIF


	IF (oDCCurrentControl == oCurrentControl)
		SELF:__StatusMessage("", MessageError)
	ENDIF
	SUPER:EditChange(oControlEvent)
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.EnableConditionalControls/*" />
METHOD EnableConditionalControls()




	IF !lControlsEnabled
		ASend(aConditionalControls,#Enable)
		lControlsEnabled := TRUE
	ENDIF
    RETURN SELF


/// <include file="Gui.xml" path="doc/DataWindow.EnableDragDropClient/*" />
METHOD EnableDragDropClient(lEnable, lSurfaceOnly)
   //SE-070501
   IF IsLogic(lSurfaceOnly) .AND. lSurfaceOnly
      IF oSurface != NULL_OBJECT
         oSurface:EnableDragDropClient(lEnable)
      ENDIF
      RETURN SELF
   ENDIF


   IF lSubForm
      oFormFrame:EnableDragDropClient(lEnable)
      RETURN SELF
   ENDIF


   RETURN SUPER:EnableDragDropClient(lEnable)


/// <include file="Gui.xml" path="doc/DataWindow.EnableStatusBar/*" />
METHOD EnableStatusBar(lEnable)
      SUPER:EnableStatusBar(lEnable)


   oFormFrame:__ResizeParent()


   RETURN SELF:StatusBar




/// <include file="Gui.xml" path="doc/DataWindow.EnableToolTips/*" />
METHOD EnableToolTips(lEnable)




	RETURN oSurface:EnableToolTips(lEnable)




/// <include file="Gui.xml" path="doc/DataWindow.Error/*" />
METHOD Error(oErrorObj)


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Expose/*" />
METHOD Expose(oExposeEvent)




	SUPER:Expose(oExposeEvent)


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.FIELDGET/*" />
METHOD FIELDGET(uFieldID)
	LOCAL oError AS USUAL
	LOCAL oFieldObject AS OBJECT
	LOCAL uValue AS USUAL




	BEGIN SEQUENCE


		IF (sCurrentView == #BrowseView) .AND. IsMethod(oGBrowse, #GetColumn)
			oFieldObject := Send(oGBrowse, #GetColumn, uFieldID)
		ELSE
			IF IsNumeric(uFieldID)
				oFieldObject := aControls[uFieldID]
			ELSEIF IsSymbol(uFieldID)
				oFieldObject := SELF:__FindControl(uFieldID)
			ELSEIF IsString(uFieldID)
				oFieldObject := SELF:__FindControl(String2Symbol(uFieldID))
			ENDIF
		ENDIF


		IF oFieldObject == NULL_OBJECT
			IF oAttachedServer != NULL_OBJECT
				RETURN oAttachedServer:FIELDGET(uFieldID)
			ELSE
				RETURN NIL
			ENDIF
		elseif oFieldObject is CheckBox var oCheck
			uValue := oCheck:Checked
		elseif oFieldObject is RadioButton var oRB
			uValue := oFieldObject:Pressed
		ELSE
			uValue := oFieldObject:Value
		ENDIF
		RETURN uValue


	RECOVER USING oError


		BREAK oError


	END SEQUENCE
	//RETURN NIL




/// <include file="Gui.xml" path="doc/DataWindow.FIELDPUT/*" />
METHOD FIELDPUT(uFieldId, uNewValue)
	// Retrieves the current value of the indicated string
	// uFieldPosition is numeric, symbol or string: the field position as numeric,
	// or the field name as a symbol or a string
//	LOCAL uField := uFieldId AS USUAL
	LOCAL oError AS USUAL
	LOCAL oFieldObject AS USUAL
	LOCAL dwFieldObject AS DWORD




	BEGIN SEQUENCE


		IF (sCurrentView == #BrowseView) .AND. IsMethod(oGBrowse, #GetColumn)
			oFieldObject := Send(oGBrowse, #GetColumn, uFieldId)
		ELSE
			IF IsNumeric(uFieldID)
				//SE-060526 this was not the same as in method FieldGet()
				//dwFieldObject := AScan(aControls, {|x| x:__GetDataFldPos == uField})
				oFieldObject := aControls[uFieldID]
			ELSEIF IsSymbol(uFieldID)
				oFieldObject := SELF:__FindControl(uFieldID)
			ELSEIF IsString(uFieldID)
				oFieldObject := SELF:__FindControl(String2Symbol(uFieldID))
			ENDIF


		ENDIF
		IF dwFieldObject > 0
			oFieldObject := aControls[dwFieldObject]
		ENDIF


		// Field object should contain control or column
		IF IsNil(oFieldObject)
			IF oAttachedServer != NULL_OBJECT
				RETURN oAttachedServer:FIELDPUT(uFieldId, uNewValue)
			ENDIF
			RETURN NIL
		ENDIF
		RETURN oFieldObject:Value := uNewValue


	RECOVER USING oError


		BREAK oError


	END SEQUENCE
	//RETURN NIL




/// <include file="Gui.xml" path="doc/DataWindow.FocusChange/*" />
METHOD FocusChange(oFocusChangeEvent)




	IF oFocusChangeEvent:GotFocus .AND. oFormFrame != NULL_OBJECT .AND. !lKickingTheBucket
		oFormFrame:SetFocusToForm()
	ENDIF


	SUPER:FocusChange(oFocusChangeEvent)


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Foreground/*" />
ASSIGN Foreground( oBrush )


	SUPER:Foreground := oBrush


	IF ( oFormFrame != NULL_OBJECT )
		oFormFrame:Foreground := oBrush
	ENDIF


	IF ( oSurface != NULL_OBJECT )
		oSurface:Foreground := oBrush
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.GetAllChildren/*" />
METHOD GetAllChildren()




	IF (oSurface != NULL_OBJECT)
		RETURN oSurface:GetAllChildren()
	ENDIF
	RETURN {}




/// <include file="Gui.xml" path="doc/DataWindow.GoBottom/*" />
METHOD GoBottom()
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:=NULL_OBJECT // assume success
	IF oAttachedServer!=NULL_OBJECT .AND. SELF:__CheckRecordStatus() // send data to server
		IF !(lRetCode:=oAttachedServer:GoBottom()) // if Skip is successful...
			oHLStatus:=oAttachedServer:Status // pick up server's reason code
			SELF:__UpdateStatus()
		ENDIF
	ENDIF
	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.GoTo/*" />
METHOD GoTo( nRecNo )
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:=NULL_OBJECT // assume success
	IF(oAttachedServer!=NULL_OBJECT .AND. SELF:lValidFlag)
		IF SELF:__CheckRecordStatus()
			IF !(lRetCode:=oAttachedServer:GoTo( nRecNo ))
				oHLStatus:=oAttachedServer:Status
				SELF:__UpdateStatus()
			ENDIF
		ENDIF
	ENDIF
	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.GoTop/*" />
METHOD GoTop()
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:=NULL_OBJECT // assume success
	IF oAttachedServer!=NULL_OBJECT .AND. SELF:__CheckRecordStatus() // send data to server
		IF !(lRetCode:=oAttachedServer:GoTop()) // if Skip is unsuccessful...
			oHLStatus:=oAttachedServer:Status // pick up server's reason code
			SELF:__UpdateStatus()
		ENDIF
	ENDIF
	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.HelpRequest/*" />
METHOD HelpRequest(oHelpRequestEvent)
	LOCAL cHelpContext AS STRING
	LOCAL oP AS OBJECT






	oP := SELF:owner
	WHILE IsInstanceOf(oP, #Window)
		oP:__EnableHelpCursor(FALSE)
		oP := oP:owner
	END
	IF (oHelpRequestEvent IS HelpRequestEvent) ;
			.AND. SELF:HelpDisplay != NULL_OBJECT ;
			.AND. SELF:CurrentView == #FormView ;
			.AND. (NULL_STRING != (cHelpContext := oHelpRequestEvent:HelpContext))
		SELF:HelpDisplay:Show(cHelpContext, oHelpRequestEvent:HelpInfo) //SE-060522
		SELF:EventReturnValue := 1l
	ELSE
		SUPER:HelpRequest(oHelpRequestEvent)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Hide/*" />
METHOD Hide()




	IF lSubForm
		oFormFrame:Hide()
	ELSE
		SUPER:Hide()
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.HorizontalScroll/*" />
METHOD HorizontalScroll(oScrollEvent)




	SELF:__HandleScrolling(oScrollEvent)
	RETURN SELF:Default(oScrollEvent)




/// <include file="Gui.xml" path="doc/DataWindow.HorizontalSlide/*" />
METHOD HorizontalSlide(oSlideEvent)




	SELF:__HandleScrolling(oSlideEvent)


	RETURN SELF:Default(oSlideEvent)




/// <include file="Gui.xml" path="doc/DataWindow.HorizontalSpin/*" />
METHOD HorizontalSpin(oSpinEvent)




	SELF:__HandleScrolling(oSpinEvent)


	RETURN SELF:Default(oSpinEvent)




/// <include file="Gui.xml" path="doc/DataWindow.ctor/*" />
CONSTRUCTOR(oOwner, oSource, nResourceID, nDialogStyle)
	LOCAL oResID AS ResourceID
	LOCAL oObject AS OBJECT


	DEFAULT(@oOwner, GetAppObject())
	//SE-120204
	IF IsLong(nDialogStyle)
	   dwDialogStyle := (DWORD) _OR(LONG(nDialogStyle), WS_DLGFRAME)
	ENDIF


	IF dwDialogStyle > 0
	   oParent := oOwner
	   CreateInstance(DefaultDDImpClassName, SELF, TRUE, dwDialogStyle)
	ENDIF


	IF IsObject(oOwner)
		oObject := oOwner
		IF oObject IS App
			SUPER(NIL, FALSE)
			lTopApp := TRUE
			// lQuitOnClose := true
		ELSEIF oObject IS ShellWindow
			SUPER(oOwner, TRUE)
		ELSEIF oObject IS DataWindow // .or. IsInstanceOf(oObject, #ToolBar)
			// Create sub form if wr're a regular DataWindow
			DEFAULT(@nResourceID, 0)
			lSubForm := ! SELF:IsDialog() //SE-120419 //!IsInstanceOf(SELF, #DataDialog)


			SUPER(oOwner, FALSE, FALSE)
		ELSEIF oObject IS ChildAppWindow
			SUPER(oOwner)
		ELSEIF oObject IS TopAppWindow
			SUPER(oOwner)
		ELSEIF oObject IS DialogWindow
			SUPER(oOwner, TRUE)
		ELSEIF oObject IS Window
			// <XXX> invalid owner - throw error
			WCError{#Init,#DataWindow,__WCSTypeError,oOwner,1}:Throw()
		ELSE
			SUPER(oOwner)
		ENDIF
	ELSE
		SUPER(oOwner)
	ENDIF


	IF IsObject(oOwner) .AND. IsMethod(oOwner, #HelpDisplay)
		SELF:oCurrentHelp := oOwner:Helpdisplay
	ENDIF
	sCurrentView := #FormView
	SELF:Caption := ResourceString{__WCSDWUntitled}:value
	aControls := {}
	aRadioGroups := {}
	aConditionalControls := {}
	nCCMode := CCOptimistic
	SELF:lValidFlag := TRUE
	lControlsEnabled := TRUE
	lAutoScroll := TRUE
	lAllowServerClose := TRUE
	aSubforms := {}
	symBrowserClass := gsymBrowserDef


	// Create Form frame object
	IF IsNil(oSource)
		oResID := ResourceID{ -1 }
	ELSEIF IsString(oSource) .OR. IsLong(oSource)
		oResID := ResourceID{oSource}
	ELSE
		oResID := oSource
	ENDIF
	IF lSubForm //IsInstanceOfusual(oOwner, #DataWindow) .and. !IsNil(nResourceID)
		//PP-030627
		oOwner:__FormWindow:GetDialogWindow():symFormDialog := SELF:symFormDialog
		oFormFrame:= oOwner:__FormWindow:CreateSubform(nResourceID, oResID)
		oFormFrame:DataWindow := SELF
		oOwner:__RegisterSubForm(SELF)
		hwnd := oFormFrame:Handle()
	ELSE
		oFormFrame := CreateInstance(DefaultFormFrameClassName,SELF, oResID, !IsInstanceOf(oImp, #__DDIMP))
    ENDIF
	oSurface := oFormFrame:GetDialogWindow()


	IF ( SELF:Background != NULL_OBJECT )
		oFormFrame:Background := SELF:Background
		oSurface:Background := SELF:Background
	ENDIF


	IF oContextMenu != NULL_OBJECT
		oFormFrame:ContextMenu := oContextMenu
		oSurface:ContextMenu   := oContextMenu
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.InsertObject/*" />
METHOD InsertObject()
	//RvdH 030825 Method moved from Ole Classes
	RETURN SELF:__GetOLEObject(#CreateFromInsertDialog)


/// <include file="Gui.xml" path="doc/DataWindow.IsDialog/*" />
METHOD IsDialog()
   //SE-070906
   RETURN dwDialogStyle > 0


/// <include file="Gui.xml" path="doc/DataWindow.LastFocus/*" />
ACCESS LastFocus
	//PP-040419 S.Ebert


	IF sCurrentView == #BrowseView
		RETURN oGBrowse
	ENDIF
	IF oSurface != NULL_OBJECT
		RETURN oSurface:LastFocus
	ENDIF
	RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/DataWindow.LastFocus/*" />
ASSIGN LastFocus (oControl)
	//PP-040419 S.Ebert


	IF lSubForm
		IF IsAssign(oParent, #LastFocus)
			IVarPut(oParent, #LastFocus, oControl)
		ENDIF
	ENDIF
	IF oSurface != NULL_OBJECT .AND. oControl != oGBrowse .AND. oControl != SELF:ToolBar
		oSurface:LastFocus := oControl
	ENDIF
	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.LineTo/*" />
METHOD LineTo(uPoint)




	IF (oSurface != NULL_OBJECT)
		oSurface:LineTo(uPoint)
	ENDIF
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.ListBoxClick/*" />
METHOD ListBoxClick(oControlEvent)
	LOCAL oListBox AS Control
	LOCAL cOldValue AS STRING
	LOCAL oCE := oControlEvent AS ControlEvent




	oListBox := oCE:Control
	IF oListBox IS ListBox
		oListBox:Modified := TRUE // assume its modified
		cOldValue := AsString(oListBox:Value)
		SELF:__DoValidate(oListBox)
		oListBox:ValueChanged := !(cOldValue == AsString(oListBox:Value))
	ENDIF
	SUPER:ListBoxClick(oCE)
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.ListBoxSelect/*" />
METHOD ListBoxSelect(oControlEvent)
	LOCAL oListBox AS BaseListBox
	LOCAL cOldValue AS STRING
	LOCAL oCE := oControlEvent AS ControlEvent






	oListBox := OBJECT(oCE:Control)
	IF oListBox IS ListBox VAR oLB
		oLB:Modified := TRUE // assume its modified
		oLB:__SetText(oLB:CurrentItem)
		cOldValue := AsString(oLB:Value)
		SELF:__DoValidate(oLB)
		oLB:ValueChanged := !(cOldValue == AsString(oLB:Value))
	ENDIF


	SUPER:ListBoxSelect(oCE)


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Menu/*" />
ASSIGN Menu(oNewMenu)




	SUPER:Menu := oNewMenu


	IF !IsInstanceOf(oParent, #SHELLWINDOW)
		oFormFrame:__ResizeParent()
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.MouseButtonDown/*" />
METHOD MouseButtonDown(oMouseEvent)
	//RvdH 030825 Method moved from Ole Classes


	SELF:DeactivateAllOLEObjects()


	RETURN SUPER:MouseButtonDown(oMouseEvent)




/// <include file="Gui.xml" path="doc/DataWindow.MoveTo/*" />
METHOD MoveTo(oPoint)




	IF (oSurface != NULL_OBJECT)
		oSurface:MoveTo(oPoint)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Notify/*" />
METHOD Notify(kNotification, uDescription)
	LOCAL oTB AS OBJECT
	LOCAL lThisRecDeleted AS LOGIC
	LOCAL oDF AS DataField


	SWITCH (INT) kNotification
	CASE NOTIFYCOMPLETION
		// Do nothing, __NotifyCompletion had no code in it
	    NOP
	CASE NOTIFYINTENTTOMOVE
		//RvdH MOved from OleDataWindow
		SELF:DeactivateAllOLEObjects()


		IF (oAttachedServer == NULL_OBJECT)
			RETURN TRUE
		ENDIF
		IF !SELF:CheckStatus()
			oTB:=TextBox{SELF, ResourceString{__WCSWarning}:value, ResourceString{__WCSChgDiscard}:value}
			oTB:Type:=BUTTONOKAYCANCEL+BOXICONHAND
			IF oTB:Show()!=BOXREPLYOKAY
				RETURN FALSE
			ELSE
				//Put original data back if moving to another record
				IF (sCurrentView == #BrowseView)
					SELF:__Scatter()
					Send(oGBrowse, #__NotifyChanges, GBNFY_FIELDCHANGE)
				ENDIF
			ENDIF
		ENDIF
		RETURN TRUE


	CASE NOTIFYFILECHANGE
		SELF:__Scatter()


	CASE NOTIFYFIELDCHANGE
		lRecordDirty := TRUE
		IF (sCurrentView == #FormView)
			FOREACH oControl as Control in aControls
                		oDF := oControl:__DataField
		                IF (oDF != NULL_OBJECT) .AND. (oDF:NameSym == uDescription)
                		    oControl:__Scatter()
		                ENDIF
			NEXT
			// RvdH 060529 This is done in the DataBrowser:Notify as well
			//ELSEIF (sCurrentView == #BrowseView)
			//	// Refresh current record and field from data server
			//	Send(oGBrowse, #__RefreshField, uDescription)
		ENDIF


	CASE NOTIFYCLOSE
		// Data Server has closed
		// if sCurrentView == #BrowseView
		//
		// Free up any internal buffers in the browse view and release any
		// memory used to maintain them.
		//
		// elseif sCurrentView == #FormView
		// self:__Unlink()
		// endif
		// Data Server has closed
		IF !SELF:CheckStatus()
			oTB:=TextBox{SELF, ResourceString{__WCSWarning}:value, ResourceString{__WCSChgDiscard}:value}
			oTB:Type:=BUTTONOKAY+BOXICONHAND
			oTB:Show()
		ENDIF
		SELF:__Unlink()


	CASE NOTIFYRECORDCHANGE
	CASE NOTIFYGOBOTTOM
	CASE NOTIFYGOTOP
	CASE NOTIFYDELETE
	CASE NOTIFYAPPEND
		// record position has changed
		lThisRecDeleted:=IVarGet(oAttachedServer,#Deleted)
		// Disable or enable controls depending on deletion state
		// this logic only applies if deleted records are included in view
		// Use SET DELETE to exclude / include deleted records in view.
		IF lThisRecDeleted
			// Do we need to disable controls
			SELF:__StatusMessage(ResourceString{__WCSDeletedRecord}:value, MESSAGEPERMANENT)
		ELSE
			SELF:__StatusMessage("", MESSAGEPERMANENT)
		ENDIF
		lDeleted := lThisRecDeleted
		//PP-040410 following line was incorrectly assigning NIL
		oHLStatus := NULL_OBJECT
		SELF:__Scatter()


		IF (kNotification == NOTIFYAPPEND)
	            //Set HLStatus for all controls
	            FOREACH oControl AS Control in aControls
	                oControl:PerformValidations()
	            NEXT
		ENDIF
	END SWITCH


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.OK/*" />
METHOD OK()


	IF SELF:Commit()
		SELF:EndWindow()
		RETURN TRUE
	ENDIF
	RETURN FALSE




/// <include file="Gui.xml" path="doc/DataWindow.OLEInPlaceActivate/*" />
METHOD OLEInPlaceActivate()
	//RvdH 030825 Method moved from Ole Classes
	LOCAL oTB AS ToolBar


	IF oParent IS ShellWindow
		oTB := oParent:ToolBar
	ELSE
		oTB := SELF:ToolBar
	ENDIF


	IF (oTB != NULL_OBJECT) .AND. oTB:IsVisible()
		oTB:Hide()
		lPendingToolBarShow := TRUE
	ENDIF


	RETURN SUPER:OLEInPlaceActivate()

/// <include file="Gui.xml" path="doc/DataWindow.OLEInPlaceDeactivate/*" />
METHOD OLEInPlaceDeactivate()
	//RvdH 030825 Method moved from Ole Classes
	LOCAL oTB AS ToolBar


	IF oParent IS ShellWindow
		oTB := oParent:ToolBar
	ELSE
		oTB := SELF:ToolBar
		oFormFrame:__ResizeParent()
	ENDIF


	IF (oTB != NULL_OBJECT) .AND. !oTB:IsVisible() .AND. lPendingToolBarShow
		oTB:Show()
	ENDIF


	lPendingToolBarShow := FALSE


	RETURN SUPER:OLEInPlaceDeactivate()




/// <include file="Gui.xml" path="doc/DataWindow.Origin/*" />
ACCESS Origin




	IF lSubForm
		RETURN oFormFrame:Origin
	ENDIF


	RETURN SUPER:Origin




/// <include file="Gui.xml" path="doc/DataWindow.Origin/*" />
ASSIGN Origin(oPoint)




	IF lSubForm
		RETURN oFormFrame:Origin := oPoint
	ENDIF


	RETURN SUPER:Origin:=oPoint




/// <include file="Gui.xml" path="doc/Window.OwnerAlignment/*" />
ASSIGN OwnerAlignment(iNewVal)
	//PP-040317 New method
	// LOCAL oFormDlg AS DialogWindow
	IF lSubForm
		//PP-040410 owner alignment improvement
		oFormFrame:OwnerAlignment := iNewVal
		// oFormDlg := oParent:__GetFormSurface()
		// oFormDlg:__AddAlign(SELF:__FormWindow, iNewVal)
	ENDIF
	RETURN


/// <include file="Gui.xml" path="doc/DataWindow.OwnerServer/*" />
ACCESS OwnerServer




	IF IsInstanceOf(SELF:Owner, #DataWindow)
		RETURN SELF:Owner:Server
	ENDIF
	RETURN NIL




/// <include file="Gui.xml" path="doc/DataWindow.PaintBoundingBox/*" />
METHOD PaintBoundingBox(oBB,kPM)


	IF oSurface != NULL_OBJECT
		oSurface:PaintBoundingBox(oBB,kPM)
	ENDIF
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Paste/*" />
METHOD Paste()




	IF sCurrentView == #FormView
		if oDCCurrentControl is Edit var oEdit
            oEdit:Paste()
        elseif oDCCurrentControl is EditWindow var oEW
            oEW:Paste()
		elseif oDCCurrentControl is ControlWindow var oCW
			if oCW:Control != null_object .and. IsMethod(oDCCurrentControl, #Paste)
				oCW:Control:Paste()
			ENDIF
		ENDIF
	ELSEIF sCurrentView == #BrowseView
		IF (oGBrowse != NULL_OBJECT) .AND. IsMethod(oGBrowse, #Paste)
			Send(oGBrowse, #Paste)
		ENDIF
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.PasteSpecial/*" />
METHOD PasteSpecial()
	//RvdH 030825 Method moved from Ole Classes
	RETURN SELF:__GetOLEObject(#CreateFromPasteDialog)




/// <include file="Gui.xml" path="doc/DataWindow.Pen/*" />
ASSIGN Pen(oPen)


	SUPER:Pen := oPen


	IF oFormFrame != NULL_OBJECT
		oFormFrame:Pen := oPen
	ENDIF


	IF oSurface != NULL_OBJECT
		oSurface:Pen := oPen
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.Pointer/*" />
ACCESS Pointer




	IF (sCurrentView == #FormView) .OR. !IsAccess(oGBrowse, #pointer)
		IF (oSurface != NULL_OBJECT)
			RETURN oSurface:Pointer
		ENDIF
	ELSE
		RETURN IVarGet(oGBrowse, #pointer)
	ENDIF


	RETURN NULL_OBJECT




/// <include file="Gui.xml" path="doc/DataWindow.Pointer/*" />
ASSIGN Pointer(oPointer)




	IF (oSurface != NULL_OBJECT)
		oSurface:Pointer:=oPointer
	ENDIF


	IF IsAssign(oGBrowse, #databrowser)
		IVarPut(oGBrowse, #pointer, oPointer)
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.PreValidate/*" />
METHOD PreValidate()


	//self:EnableConditionalControls()
	SELF:__CheckConditionalControls()
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.PreventAutoLayout/*" />
ACCESS PreventAutoLayout




	RETURN lPreventAutoLayout




/// <include file="Gui.xml" path="doc/DataWindow.PreventAutoLayout/*" />
ASSIGN PreventAutoLayout(lNewValue)




	lPreventAutoLayout := lNewValue


	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.QueryClose/*" />
METHOD QueryClose(oQCE)
	// If there are outstanding changes which have not
	// been written to the dataserver - ask the user.
	LOCAL oTB AS OBJECT






	IF oAttachedServer==NULL_OBJECT
		RETURN TRUE
	ENDIF


	SELF:SetFocus()
	IF !SELF:CheckStatus()
		oTB := TextBox{ SELF,;
			ResourceString{__WCSWarning}:value,;
			VO_Sprintf(__WCSDataWindow,SELF:Caption)+Chr(10)+ResourceString{__WCSChgDiscard}:value}
		oTB:Type := ButtonOkayCancel + BoxICONHand
		IF (oTB:Show() != BOXREPLYOkay)
			RETURN FALSE
		ENDIF
	ENDIF


	RETURN TRUE


/// <include file="Gui.xml" path="doc/DataWindow.RadioGroups/*" />
ACCESS RadioGroups
	// DHer: 18/12/2008
RETURN SELF:aRadioGroups




/// <include file="Gui.xml" path="doc/DataWindow.RegisterConditionalControls/*" />
METHOD RegisterConditionalControls(oCC)
	//SE-060526
	LOCAL dwI, dwCount AS DWORD






	dwCount := ALen(aConditionalControls)
	FOR dwI := 1 UPTO dwCount
		IF aConditionalControls[dwI] == oCC
			RETURN SELF
		ENDIF
	NEXT  // dwI


	AAdd(aConditionalControls,oCC)


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.RePaint/*" />
METHOD RePaint()




	IF oSurface != NULL_OBJECT
		oSurface:Repaint()
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.RepaintBoundingBox/*" />
METHOD RepaintBoundingBox(oBB)




	IF (oSurface != NULL_OBJECT)
		oSurface:RepaintBoundingBox(oBB)
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Resize/*" />
METHOD Resize(oResizeEvent)




	SUPER:Resize(oResizeEvent)
	SELF:__AdjustForm()


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Seek/*" />
METHOD Seek(uValue, lSoftSeek)
	LOCAL lRetCode AS LOGIC






	IF oAttachedServer!=NULL_OBJECT
		oHLStatus := NULL_OBJECT // assume success
		IF SELF:__CheckRecordStatus()
			IF !(lRetCode := oAttachedServer:Seek(uValue, lSoftSeek))
				oHLStatus := oAttachedServer:Status
				//oHLStatus := HyperLabel{#Seek, "Seek Failed, key not found"}
				SELF:__UpdateStatus()
			ENDIF
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.Server/*" />
ACCESS Server
	RETURN oAttachedServer




/// <include file="Gui.xml" path="doc/DataWindow.SetAlignStartSize/*" />
METHOD SetAlignStartSize(oSize)
	IF oSurface != NULL_OBJECT
		oSurface:SetAlignStartSize(oSize)
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/DataWindow.SetDialog/*" />
METHOD SetDialog(lResizable, lMaximizeBox, lMinimizeBox)
   //SE-120204
   //can be used in PreInit() method or befor super:init()
   IF hWnd == NULL_PTR


   	   dwDialogStyle := _OR(dwDialogStyle, WS_DLGFRAME)


	   IF IsLogic(lResizable) .AND. lResizable
	      dwDialogStyle := _OR(dwDialogStyle, WS_THICKFRAME)
	   ENDIF
	   IF IsLogic(lMaximizeBox) .AND. lMaximizeBox
	      dwDialogStyle := _OR(dwDialogStyle, WS_MAXIMIZEBOX, WS_SYSMENU)
	   ENDIF
	   IF IsLogic(lMinimizeBox) .AND. lMinimizeBox
	      dwDialogStyle := _OR(dwDialogStyle, WS_MINIMIZEBOX, WS_SYSMENU)
	   ENDIF


   ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.SetRelation/*" />
METHOD SetRelation( oDWChild, uRelation, cRelation )
	LOCAL lRetCode AS LOGIC


	//PP-040410 following line was incorrectly assigning NIL
	oHLStatus:=NULL_OBJECT // assume success
	IF oAttachedServer!=NULL_OBJECT .AND. SELF:lValidFlag
		IF SELF:__CheckRecordStatus()
			IF !(lRetCode:=Send(oAttachedServer,#SetRelation,oDWChild:Server, uRelation, cRelation ))
				oHLStatus:=oAttachedServer:Status
				SELF:__UpdateStatus()
			ENDIF
		ENDIF
	ENDIF
	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.SetSelectiveRelation/*" />
METHOD SetSelectiveRelation( oDWChild, uRelation, cRelation )
	LOCAL lRetCode AS LOGIC




	oHLStatus := NULL_OBJECT // assume success
	IF oAttachedServer != NULL_OBJECT .AND. SELF:lValidFlag
		IF SELF:__CheckRecordStatus()
			IF !(lRetCode:=Send(oAttachedServer,#SetSelectiveRelation,oDWChild:Server, uRelation, cRelation ))
				oHLStatus:=oAttachedServer:Status
				SELF:__UpdateStatus()
			ENDIF
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.Show/*" />
METHOD Show(nShowState)
	//PP-040419 Update from S.Ebert


	IF lDeferUse .AND. (oDeferUseServer != NULL_OBJECT)
		lDeferUse := FALSE
		SELF:Use(oDeferUseServer)
		oDeferUseServer := NULL_OBJECT
	ENDIF


	IF (SELF:ToolBar != NULL_OBJECT)
		SELF:ToolBar:Show()
	ENDIF


	IF SELF:lValidFlag
		IF !lSubForm
			// Show oFormFrame as SHOWNORMAL in case data window is shown iconized
            IF self:dwDialogStyle = 0   //SE-120204
				SUPER:Show(nShowState)
				oFormFrame:Show(SHOWNORMAL)
			ELSE // DataDialog
				oFormFrame:Show(SHOWNORMAL)
				SetFocus(oSurface:Handle())
				SUPER:Show(nShowState)
				RETURN SELF
			ENDIF
		ELSE
			oFormFrame:Show(nShowState)
		ENDIF
	ELSE
		SELF:Destroy()
	ENDIF


	IF (SELF:ToolBar != NULL_OBJECT)
		BringWindowToTop(SELF:ToolBar:Handle())
	ENDIF


	SELF:__AdjustForm()


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.Size/*" />
ACCESS Size




	IF lSubForm
		RETURN oFormFrame:Size
	ENDIF


	RETURN SUPER:Size




/// <include file="Gui.xml" path="doc/DataWindow.Size/*" />
ASSIGN Size(oDimension)




	IF lSubForm
		oFormFrame:ChangeFormSize(oFormFrame:Origin,oDimension)
		RETURN oDimension
	ENDIF


	RETURN SUPER:Size := oDimension




/// <include file="Gui.xml" path="doc/DataWindow.Skip/*" />
METHOD Skip(uRelativePosition)
	LOCAL lRetCode AS LOGIC




	oHLStatus := NULL_OBJECT // assume success
	IF oAttachedServer != NULL_OBJECT .AND. SELF:__CheckRecordStatus()
		IF !(lRetCode := oAttachedServer:Skip(uRelativePosition))
			oHLStatus := oAttachedServer:Status
			SELF:__UpdateStatus()
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.SkipNext/*" />
METHOD SkipNext()
	LOCAL lRetCode AS LOGIC




	lRetCode := SELF:Skip(1)


	IF lRetCode = TRUE .AND. oAttachedServer:EOF
		SELF:GoBottom()
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.SkipPrevious/*" />
METHOD SkipPrevious()


	RETURN SELF:Skip(-1)




/// <include file="Gui.xml" path="doc/DataWindow.Status/*" />
ACCESS Status
	return self:oHLStatus


/// <include file="Gui.xml" path="doc/DataWindow.Status/*" />
ASSIGN Status(oStatus)
	if !(oStatus is Hyperlabel)
		WCError{#Status,#DataWindow,__WCSTypeError,oStatus,1}:Throw()
	ENDIF


	RETURN oHLStatus := oStatus




/// <include file="Gui.xml" path="doc/DataWindow.StatusBar/*" />
ACCESS StatusBar
    //SE-120204
	 IF dwDialogStyle > 0
		 // Support of a StatusBar a DataDialog - Window
	    RETURN oStatusBar
	 ENDIF
	 RETURN SUPER:StatusBar


/// <include file="Gui.xml" path="doc/DataWindow.StatusBar/*" />
ASSIGN StatusBar(oNewStatusBar)
    //SE-120204
	 IF dwDialogStyle > 0
		 // Support of a StatusBar a DataDialog - Window
	    oStatusBar := oNewStatusBar
	    RETURN oStatusBar
	 ENDIF


	 RETURN (SUPER:StatusBar := oNewStatusBar)




/// <include file="Gui.xml" path="doc/DataWindow.StatusOK/*" />
METHOD StatusOK()




	oDCInvalidControl := NULL_OBJECT
	oDCInvalidColumn := NULL_OBJECT
	oHLStatus := NULL_OBJECT


	IF (oAttachedServer == NULL_OBJECT)
		oHLStatus := HyperLabel{#NoAttachedServer, #NoAttachedServer}
	ELSE
		SELF:__UpdateCurrent()
		IF !oAttachedServer:EOF //Don't validate the EOF record
			IF (sCurrentView == #FormView)
				FOREACH oControl as Control in aControls
                    			IF (oControl:Status != null_object)
						oDCInvalidControl := oControl
			                        exit
					ENDIF
				NEXT
				IF oDCInvalidControl != NULL_OBJECT
					IF (oHLStatus := oDCInvalidControl:Status) == NULL_OBJECT
						oHLStatus := HyperLabel{#InvalidControl, #RecInvalid}
					ENDIF
				ENDIF
			ELSEIF (sCurrentView == #BrowseView)
				IF (oDCInvalidColumn := Send(oGBrowse, #__StatusOK)) != NULL_OBJECT
					IF (oHLStatus := oDCInvalidColumn:Status) == NULL_OBJECT
						oHlStatus := HyperLabel{#InvalidColumn, #RecInvalid}
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF


	RETURN oHLStatus == NULL_OBJECT


/// <include file="Gui.xml" path="doc/DataWindow.SubForms/*" />
ACCESS SubForms
	// DHer: 18/12/2008
RETURN SELF:aSubForms


/// <include file="Gui.xml" path="doc/DataWindow.Surface/*" />
ACCESS Surface
	// DHer: 18/12/2008
RETURN SELF:oSurface




/// <include file="Gui.xml" path="doc/DataWindow.TextPrint/*" />
METHOD TextPrint(cText, oPoint)


	IF oSurface != NULL_OBJECT
		oSurface:TextPrint(cText, oPoint)
	ENDIF
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.ToolBar/*" />
ASSIGN ToolBar(oNewToolBar)




	SUPER:ToolBar := oNewToolBar


	oFormFrame:__ResizeParent()
	RETURN




/// <include file="Gui.xml" path="doc/DataWindow.ToolBarHeightChanged/*" />
METHOD ToolBarHeightChanged(oControlNotifyEvent)




	SUPER:ToolBarHeightChanged(oControlNotifyEvent)


	SELF:__AdjustForm()
	RETURN SELF






/// <include file="Gui.xml" path="doc/DataWindow.Undo/*" />
METHOD Undo()




	IF (sCurrentView == #FormView)
		IF oDCCurrentControl IS Edit VAR oEdit
			oEdit:Undo()
		ENDIF
	ELSEIF (sCurrentView == #BrowseView)
		IF (oGBrowse != NULL_OBJECT) .AND. IsMethod(oGBrowse, #Undo)
			Send(oGBrowse, #Undo)
		ENDIF
	ENDIF


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.UndoAll/*" />
METHOD UndoAll()




	IF oAttachedServer!=NULL_OBJECT
		Send(oAttachedServer,#Refresh)
	ENDIF


	RETURN NIL




/// <include file="Gui.xml" path="doc/DataWindow.UpdateActiveObject/*" />
METHOD UpdateActiveObject()
	//RvdH 041123 Replaced with call to __UpdateActiveObject()
	RETURN SELF:__UpdateActiveObject()




/// <include file="Gui.xml" path="doc/DataWindow.Use/*" />
METHOD Use(oDataServer)
	LOCAL lRetCode AS LOGIC






	if lDeferUse .and. oDataServer is DataServer
		oDeferUseServer := oDataServer
		RETURN TRUE
	ENDIF


	IF (oAttachedServer != oDataServer) .AND. (oAttachedServer != NULL_OBJECT)
		SELF:__Unlink()
		IF oFormFrame:AutoLayout
			IF (oSurface != NULL_OBJECT)
				oSurface:__DestroyChildren()
				oSurface:Size := Dimension{0, 0}
			ENDIF
			aControls := {}
			aRadioGroups := {}
			aConditionalControls := {}


			IF (oGBrowse != NULL_OBJECT)
				oGBrowse:Destroy()
				oGBrowse := NULL_OBJECT
			ENDIF
		ENDIF
	ENDIF


	IF (oDataServer != NULL_OBJECT)
		IF SELF:__VerifyDataServer(oDataServer)
			oDataServer:RegisterClient(SELF)


			lRetCode := SELF:__RegisterFieldLinks(oDataServer)
			IF (sCurrentView == #BrowseView)
				IF (oGBrowse != NULL_OBJECT)
					Send(oGBrowse, #Use, oDataServer)
					SELF:__Scatter() // new insert, since ViewAs already does a Scatter()
				ELSE
					sCurrentView := #ViewSwitch
					SELF:ViewAs(#BrowseView)
				ENDIF
			ELSEIF (sCurrentView == #FormView)
				sCurrentView := #ViewSwitch
				SELF:ViewAs(#FormView)
			ENDIF
			// self:__Scatter() // removed, see above
		ELSE
			IF (oHLStatus != NULL_OBJECT)
				ErrorBox{, oHLStatus}:Show()
			ELSE
				WCError{#oDataServer,#DataWindow,__WCSTypeError,oDataServer,1}:Throw()
			ENDIF
			SELF:lValidFlag := FALSE
		ENDIF
	ENDIF


	RETURN lRetCode




/// <include file="Gui.xml" path="doc/DataWindow.VerticalScroll/*" />
METHOD VerticalScroll(oScrollEvent)




	SELF:__HandleScrolling(oScrollEvent)
	RETURN SELF:Default(oScrollEvent)




/// <include file="Gui.xml" path="doc/DataWindow.VerticalSlide/*" />
METHOD VerticalSlide(oSlideEvent)




	SELF:__HandleScrolling(oSlideEvent)


	RETURN SELF:Default(oSlideEvent)




/// <include file="Gui.xml" path="doc/DataWindow.VerticalSpin/*" />
METHOD VerticalSpin(oSpinEvent)




	SELF:__HandleScrolling(oSpinEvent)


	RETURN SELF:Default(oSpinEvent)








/// <include file="Gui.xml" path="doc/DataWindow.ViewAs/*" />
METHOD ViewAs(symViewType)
	LOCAL oTextBox AS TextBox
#ifdef USE_OLEOBJECT
	LOCAL iLen AS INT
	LOCAL aObjects AS ARRAY
	LOCAL oOleObj AS OleObject
#endif
    LOCAL oControl AS Control
	//RvdH 041123 Added call to __GetMyOleObjects to retrieve the objects




	IF !IsSymbol(symViewType)
		WCError{#ViewAs,#DataWindow,__WCSTypeError,symViewType,1}:Throw()
	ENDIF


	IF (sCurrentView == symViewType)
		// No change in view -> do nothing
		RETURN SELF
	ENDIF
	// RvdH Copied from OleDataWindow
	SELF:DeactivateAllOLEObjects()


	// Save data in current view
	IF lLinked
		IF !SELF:__CheckRecordStatus() // check validation status
			// continuing now may lose changes
			oTextBox := TextBox{SELF, ResourceString{__WCSWarning}:value, ResourceString{__WCSChangingView}:value}
			oTextBox:Type := BUTTONOKAYCANCEL + BOXICONHAND
			IF oTextBox:Show() != BOXREPLYOKAY
				RETURN SELF
			ELSE
				//Put original data back
				IF (sCurrentView == #BrowseView)
					SELF:__Scatter()
					Send(oGBrowse, #__NotifyChanges, GBNFY_FIELDCHANGE)
				ENDIF
			ENDIF
		ENDIF
	ENDIF


	IF (symViewType == #BrowseView)
		// Show as browser
		// Hide form frame
		// Check if autocreating browser
		// flush changes to form so they are reflected in browser
		TRY
			sCurrentView := #ViewSwitch
#ifdef USE_OLEOBJECT
			aObjects := SELF:__GetMyOleObjects()
			iLen 		:= LONGINT(ALen(aObjects))
			LOCAL i AS INT
			FOR i:=1 TO iLen
				oOleObj := aObjects[i]
				IF oOleObj:Server != NULL_OBJECT


					oOleObj:DetachFromServer()
				ENDIF
			NEXT
#endif
			SELF:__AutoCreateBrowser()
			// show/hide is handled in FormFrame:ViewAs
			// oGBrowse:__AutoResize()
			// oGBrowse:Show()
			SELF:__FormWindow:SetGBrowse(oGBrowse)
			// oSurface:Hide()
        CATCH as Exception
            NOP
		END TRY
		Send(oGBrowse, #SuspendUpdate)
		oFormFrame:ViewAs(TRUE) // view as browse
		Send(oGBrowse, #RestoreUpdate)
		sCurrentView := #BrowseView
		IF oGBrowse != NULL_OBJECT
			Send(oGBrowse, #__NOTIFYChanges, GBNFY_VIEWASBROWSER)
		ENDIF
	ELSEIF (symViewType == #FormView)
		// Show as form
		//PP030808: Bug 12409
		// Check there are _no_ controls at all before doing autolayout
		IF ALen(SELF:GetAllChildren()) == 0
			SELF:__AutoLayout()


			// If there are some "normal" controls (not subforms) set focus, etc
		ELSEIF ALen(aControls) > 0
            oControl := aControls[1]
            oControl:SetFocus()
            IF oControl IS SingleLineEdit .AND. IsWindowEnabled(oControl:Handle())
                PostMessage(oControl:Handle(), EM_SETSEL, 0, -1)
            ENDIF
		ENDIF
		oFormFrame:ViewAs(FALSE) // view as form
		sCurrentView := #FormView
		IF (oGBrowse != NULL_OBJECT)
			Send(oGBrowse, #__NOTIFYChanges, GBNFY_VIEWASFORM)
		ENDIF
	ELSE
		WCError{#ViewAs,#DataWindow,__WCSTypeError,symViewType,1}:Throw()
	ENDIF


	SELF:__Scatter()


	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.ViewForm/*" />
METHOD ViewForm()


	SELF:ViewAs(#FormView)
	RETURN SELF




/// <include file="Gui.xml" path="doc/DataWindow.ViewTable/*" />
METHOD ViewTable()


	SELF:ViewAs(#BrowseView)
	RETURN SELF
END CLASS


STATIC GLOBAL glUseColonInAutoLayoutCaptions := TRUE AS LOGIC


/// <include file="Gui.xml" path="doc/UseColonInAutoLayoutCaptions/*" />
FUNCTION UseColonInAutoLayoutCaptions(lUse AS LOGIC) AS VOID
    glUseColonInAutoLayoutCaptions := lUse
RETURN


 /// <exclude />
FUNCTION __GetDFCaption (oDF AS DataField, arUsedKeys AS ARRAY)  AS STRING
	LOCAL cText 	AS STRING
	LOCAL cHotKey 	AS STRING
	LOCAL dwI, dwCount AS DWORD
	LOCAL i 			AS DWORD




	//	IF (IsInstanceOfUsual(oDF, #DataField) .AND. (!IsNil(oDF:HyperLabel)))
	IF !IsNil(oDF:HyperLabel)
		cText := oDF:HyperLabel:Caption
		// 	ELSEIF (IsInstanceOfUsual(oDF, #DataServer) .AND. (!IsNil(fldIndex)))
		// 		cText := LTrim(AsString(oDF:FieldName(fldIndex)))
	ELSE
		cText := ResourceString{__WCSUnknown}:value
	ENDIF


	cText := Proper(cText) //RTrim(Left(cText, 1) + Lower(SubStr(cText, 2)))


	//	IF !IsNil(arUsedKeys)
	IF (dwCount := ALen(arUsedKeys)) > 0
		FOR i := 1 TO SLen(cText)
			cHotKey := Lower(SubStr(cText, i ,1))
			//SE-060526
			FOR dwI := 1 UPTO dwCount
				IF arUsedKeys[dwI] = cHotKey
					EXIT
				ENDIF
			NEXT  // dwI
			IF dwI > dwCount
				AAdd(arUsedKeys, cHotKey)
				cText := Left(cText, i-1) + "&" + SubStr(cText, i)
				EXIT
			ENDIF
		NEXT
	ENDIF


    IF glUseColonInAutoLayoutCaptions
	    IF !IsBiDi()
		    IF !(Right(cText, 1) == ":")
			    cText := cText +":"
		    ENDIF
	    ELSE
		    IF !(Left(cText, 1) == ":")
			    cText := ":" + cText
		    ENDIF
	    ENDIF
    ENDIF


	RETURN cText


 /// <exclude />
FUNCTION __GetFSDefaultLength(uFS AS OBJECT) AS INT
    LOCAL liRetVal, liExtra AS LONGINT
    LOCAL uType AS USUAL
    LOCAL cFunction AS STRING


    IF uFS IS FieldSpec VAR oFs
        liRetVal := oFS:Length
        uType := oFS:UsualType


        DO CASE
        CASE (uType == LOGIC)
            liRetVal := Max(liRetVal, 3)
        CASE (uType == DATE)
            IF (SetCentury() == TRUE)
                liRetVal := 10
            ELSE
                liRetVal := Max(liRetVal, 8)
            ENDIF
        CASE (uType == STRING) .AND. oFS:ValType == "M"
            liRetVal := Max(liRetVal, 40)
            liRetVal := Min(liRetVal, 120)
        ENDCASE
        //RvdH 060608 optimized
        //IF (NULL_STRING != oFS:Picture) .AND. !Empty(oFS:Picture)
        IF SLen(oFS:Picture)> 0
            IF SubStr(oFS:Picture, 1, 1) != "@"
                //there is no associated function
                liRetVal := LONGINT(_CAST,SLen(oFS:Picture))
            ELSE
                //store the function character
                cFunction := SubStr(oFS:Picture, 2, 1)
                IF cFunction == "C" .OR. cFunction == "X"
                    //"@C" and "@X" functions require a " CR" or a " DB" respectively
                    //at the end of the string, so tag on three extra bytes
                    liExtra := 3
                ENDIF
                IF At(" ", oFS:Picture) != 0
                    //the actual picture will now start after the space between the function
                    //and the template, so calculate the length from this point
                    liRetVal := LONGINT(_CAST,SLen(SubStr(oFS:Picture, At(" ", oFS:Picture) + 1)))
                ENDIF
                liRetVal += liExtra
            ENDIF
        ENDIF
        IF (oFS:Length > liRetVal)
            RETURN oFS:Length
        ENDIF
    ELSE
        liRetVal := 10

    ENDIF
    RETURN liRetVal



