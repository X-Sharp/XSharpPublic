//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System.Windows.Forms.VisualStyles
using VOSDK := XSharp.VO.SDK
/// <include file="Gui.xml" path="doc/DataWindow/*" />
class DataWindow inherit ChildAppWindow implements ILastFocus
    protect sCurrentView as symbol
    protect nCCMode as int
    protect nLastLock as int

        //PROTECT lKickingTheBucket AS LOGIC
    protect lChanged as logic
    protect lLinked as logic
    protect lControlsEnabled as logic
    protect lSubForm as logic
    protect lTopApp as logic
    protect lAlreadyHasFlock as logic
    protect lRecordDirty as logic
    protect lDeleted as logic
    protect lValidFlag as logic
    protect lPreventAutoLayout as logic
    protect lDeferUse as logic
    protect oDeferUseServer as DataServer
    protect lAutoScroll as logic

    protect aSubForms as array
    protect aControls as array
    protect aConditionalControls as array
    protect aRadioGroups as array

        //PROTECT oFormFrame AS __FormFrame
    protect oGBrowse as IDataBrowser

    protect oHLStatus as HyperLabel
    protect oSurface as VOSurfacePanel
    protect oFrame	 as VOFramePanel
    protect oAttachedServer as DataServer
    protect oDCCurrentControl as object
    protect oDCInvalidControl as Control
    protect oDCInvalidColumn as DataColumn
    protect lPendingToolBarShow as logic
    protect lAllowServerClose as logic
    protect symBrowserClass as symbol
    protect symFormDialog as symbol
    protect dwDialogStyle  as dword //SE-070906
    protect oLastFocus as Control
    protect oResourceID 	as ResourceID


    internal lInAutoLayout as logic

    /// <exclude />
    method __CreateForm() as VOForm strict
        local oDw as VODataForm
        self:__ReadResource(oResourceID, oParent)
        if oShell != null_object
            oDw := GuiFactory.Instance:CreateDataForm(self, oShell:__Form, self:oResourceDialog)
        else
            oDw := GuiFactory.Instance:CreateDataForm(self, null_object, self:oResourceDialog)
        endif
        self:oSurface := oDw:Surface
        self:oFrame	  := oDw:Frame
        return oDw

    /// <exclude />
    property __DataForm as VODataForm get (VODataForm) self:__Form

    /// <exclude />
    property __Frame as VOFramePanel get self:oFrame

    /// <exclude />

    [Obsolete];
    method __AdjustForm() as void strict
        //Resizing happens automatically in the DataWinForm class
        return
    /// <exclude />

    [Obsolete];
    method __AdjustSurface() as logic strict
        //Resizing happens automatically in the DataWinForm class
        return true

    /// <exclude />
    property __aRadioGroups as array get aRadioGroups

    /// <exclude />
    method __AutoCreateBrowser() as DataWindow strict
        if oGBrowse == null_object
            if symBrowserClass == #DataBrowser
                oGBrowse := DataBrowser{self}
                self:__DataForm:DataBrowser := (System.Windows.Forms.Control) oGBrowse:__Control
                //ELSEIF symBrowserClass == #DataListView
                //	oGBrowse := DataListView{SELF}
            else
                oGBrowse := CreateInstance(symBrowserClass, self)
            endif
        endif
        do case
        case (sCurrentView == #ViewSwitch)
            oGBrowse:__NOTIFYChanges( GBNFY_VIEWSWITCH)
        case (sCurrentView == #BrowseView)
            oGBrowse:__NOTIFYChanges( GBNFY_VIEWASBROWSER)
        case (sCurrentView == #FormView)
            oGBrowse:__NOTIFYChanges( GBNFY_VIEWASFORM)
        end case

        if (lLinked .and. oAttachedServer != null_object)
            oGBrowse:Use(oAttachedServer)
        endif

        return self


    /// <exclude />
    method __AutoLayout() as DataWindow strict
        local cField as string
        local oDFField as DataField
        local liFieldLen as longint
        local liStart as longint
        local liFields, liField, liLines as longint
        local newControl as Control
        local oPoint as Point
        local oDimension as Dimension
        local oLabelDim as Dimension
        local maxLblSize as int
        local maxFldSize as int
        local maxEditSize as int
        local editHeight as int
        local editGap as int
        local cType as string
        local arUsedKeys as array
        local lOleAvail := true as logic
        local lMCAvail := true
        local lOleWarningShown := false as logic
        local lBidi := IsBiDi() as logic
        local iMaxWidth as int
        local iNewWidth as int
        local CoordinateSystem as logic
        local iFontWidth as int

        if lPreventAutoLayout
            return self
        endif
        lInAutoLayout := true
        CoordinateSystem := WC.CoordinateSystem
        WC.CoordinateSystem := WC.WindowsCoordinates
        self:oSurface:SuspendLayout()

        //RvdH 070415 limited iMMaxWidth to half the width of the desktop (and not two third)
        iMaxWidth := System.Windows.Forms.Screen.GetWorkingArea(__Form):Width /2

        __DataForm:AutoLayout := true
        __DataForm:ResetMinSize()

        arUsedKeys := {}

        // get Textmetric through Font
        // approx. - there is no width in font, use of VisualStyle Renderer Crashes in Windows Classic Scheme and getting the textmetrics through handle is not an option
        // since the window is shown when the handle is created
        iFontWidth := self:oSurface:Font:Height
        editHeight := iFontWidth + 4
        editGap := editHeight + 6

        // Find maximum field label size and number of lines
        liFields := (long) oAttachedServer:FCount
        for liField := 1 upto liFields
            //DebOut("lifield", lifield)
            oDFField := oAttachedServer:DataField(liField)
            if (oDFField == null_object)
                loop
            endif

            cType := oDFField:FieldSpec:ValType
            if (cType != "O") .or. lOleAvail
                liLines++
                cField 		:= __GetDFCaption(oDFField,{})
                oDimension := self:SizeText(cField)
                if (oDimension:Width > maxLblSize)
                    maxLblSize := oDimension:Width
                    oLabelDim := oDimension
                endif
                if (cType == "M")
                    liLines += 1
                elseif ((cType == "O") .and. lOleAvail) .or. (cType == "X")
                    liLines += 6
                else
                    maxFldSize := Max(maxFldSize, oDFField:FieldSpec:Length)
                endif
            elseif !lOLEWarningShown
                TextBox{self, ResourceString{__WCSWarning}:Value, ResourceString{__WCSNoOLESupport}:Value, BUTTONOKAY}:Show()
                lOleWarningShown := true
            endif
        next //liStart

    //// Add the fields
    liField := 0
    maxEditSize := Math.Min(iMaxWidth, (maxFldSize+1) * iFontWidth)

    oPoint := Point{iif(lBidi, (25 + maxEditSize), 20), 0}

    for liStart:=1 upto liLines
        // get next datafield
        oDFField := null_object
        while (oDFField == null_object) .and. (liField <= liFields)
            liField++
            oDFField := self:oAttachedServer:DataField(liField)
        enddo

        if liField>liFields
            exit
        endif

    // Create label
    cType := oDFField:FieldSpec:ValType

    if (cType != "O") .or. lOleAvail
        cField := __GetDFCaption(oDFField,arUsedKeys)
        oPoint:Y := editGap * (liStart-1)
        newControl := FixedText{self, (100 + liField), oPoint, oLabelDim, cField}
        newControl:Show()

        // Create Data control
        liFieldLen := __GetFSDefaultLength(oDFField:FieldSpec)

        // get the new width first (we need that for BiDi)
        switch cType
        case  "L"
            iNewWidth := 4*iFontWidth
        case "M"
            iNewWidth := (maxFldSize+1)*iFontWidth
        case "O"
        case "X"
            iNewWidth := 300
        case "D" when lMCAvail
            iNewWidth := 190
        otherwise
            if (cType == "C")
                iNewWidth := Max(liFieldLen, 2) * iFontWidth
            else
                iNewWidth := (liFieldLen +1 ) * iFontWidth
            endif
        end switch

    if (lBidi)
    // s.b. We have to add this otherwise the "C" type fields overlap the text captions.
        oPoint:X := 20 + maxEditSize - Math.Min(iMaxWidth, iNewWidth)
    else
        oPoint:X := 25 + maxLblSize
    endif

    oPoint:Y := editGap * (liStart-1)

    switch cType
    case  "L"
        newControl := CheckBox{self, 200+liField, oPoint, Dimension{iNewWidth, editHeight}, " ", BS_AUTOCHECKBOX}
    case "M"
        liStart += 1
        oPoint:Y := editGap * (liLines - liStart + 1)
        newControl := MultiLineEdit{self, 200+liField, oPoint, Dimension{iNewWidth, editHeight*2}, ES_AUTOVSCROLL}
#ifdef USE_OLEOBJECT
    case cType=="O"
    liStart += 6
    oPoint:Y := editGap * (liLines - liStart + 1)
    oOle := OleObject{ self, 200+liField, oPoint, Dimension{iNewWidth, editGap*6+editHeight}, true}
    newControl := oOle
    oOle:AutoSizeOnCreate := false
    oOle:AllowInPlace:= !IsInstanceOf(self, #DataDialog) /*.and. IsInstanceOf(oParent, #ShellWindow)*/
    oOle:ActivateOnDblClk:=  true
    oOle:AllowResize:=  true
#endif
    case "X"
        liStart += 6
        oPoint:Y := editGap * (liLines - liStart + 1)
        newControl := MultiMediaContainer{self, 200+liField, oPoint, Dimension{iNewWidth, editGap*6+editHeight}}
    case "D" when  lMCAvail
        newControl := DateTimePicker{self, 200+liField, oPoint, Dimension{iNewWidth, editHeight}, DTS_LONGDATEFORMAT}
    otherwise
        if (iNewWidth < iMaxWidth)
            newControl := SingleLineEdit{self, 200+liField, oPoint, Dimension{iNewWidth, editHeight}}
        else
            newControl := SingleLineEdit{self, 200+liField, oPoint, Dimension{iMaxWidth, editHeight}, ES_AUTOHSCROLL}
        endif
    end switch
    // Link the data editor to the Server
    newControl:LinkDF(oAttachedServer, liField)
    // Show it
    newControl:Show()

    oPoint:X := iif(lBidi, (25 + maxEditSize), 20)
    endif
    next
    self:oSurface:ResumeLayout()
    lInAutoLayout := false
    WC.CoordinateSystem := CoordinateSystem
    __DataForm:AdjustFrameSize()

    return self


    /// <exclude />
    method __CheckConditionalControls() as DataWindow strict
        local idx, iLen as dword
        local oControl as Control

        iLen := ALen(aControls)
        for idx :=1 to iLen
            oControl := aControls[idx]
            if (oControl:Status != null_object)
                exit
            endif
        next

        if !IsNil(oHLStatus) .or. (idx != (iLen+1))
            self:DisableConditionalControls()
        else
            self:EnableConditionalControls()
        endif
        return self


    /// <exclude />
    method __CheckRecordStatus() as logic strict
        local oOldStatus as HyperLabel
        local oTempStatus as HyperLabel

        oOldStatus := oHLStatus

        if (IsNil(oAttachedServer))
            oHLStatus := HyperLabel{#NoAttachedServer, #NoAttachedServer}
        else
            self:__UpdateCurrent()
            if !self:StatusOK()
                if (self:sCurrentView == #FormView)
                    if (oDCInvalidControl != null_object)
                        oTempStatus:=oHLStatus //Save status accross SetFocus
                        oDCInvalidControl:SetFocus()
                        oHLStatus := oTempStatus
                    endif
                elseif (self:sCurrentView == #BrowseView)
                    // Jump to error column for browse view
                    if (oDCInvalidColumn != null_object) .and. oGBrowse is DataBrowser var oDBr
                        oDBr:SetColumnFocus(oDCInvalidColumn)
                    endif
                endif
            else
                if !self:ValidateRecord()	//RH ValidateRecord is defined in our patches
                    if IsNil(oHLStatus)
                        oHLStatus := HyperLabel{#InvalidRecord, #RecInvalid}
                    endif
                endif
            endif
            // Prevalidate based on status change
            if (oHLStatus != oOldStatus)
                self:PreValidate()
            endif
        endif
        if (!IsNil(oHLStatus))
            self:__UpdateStatus()
            return false
        endif

        return true


    /// <exclude />
    method __Delete() as logic strict
        // DataWindow : Delete
        // This method deletes the current record.
        // Depending on the setting of SET DELETE, the record may or may not be still
        // available for viewing. If SET DELETE is set to ON, the record will not be shown
        // in subsequent browsing of the DataServer.

        local lRetCode := false as logic

        oHLStatus:= null_object // assume success
        if oAttachedserver!=null_object
            if self:__CheckRecordStatus()
                lRetCode := oAttachedServer:@@Delete()
                if lRetCode
                    if SetDeleted() .or. IsInstanceOf(oAttachedServer,#SQLSelect)
                        self:Skip(1)
                        oHLStatus:=oAttachedServer:Status
                        if oAttachedServer:EoF
                            self:Skip(-1)
                            oHLStatus:=oAttachedServer:Status
                        endif
                    endif
                else
                    oHLStatus:=oAttachedServer:Status
                    if IsNil(oHLStatus) // need default status info
                        oHLStatus := HyperLabel { #NoDelete, ResourceString{__WCSDeleteFailed}:Value, ResourceString{__WCSDeleteFailedMSG}:Value}
                    endif
                endif
            else
                oHLStatus:=oAttachedServer:Status
            endif
        endif
        return self:__UpdateStatus()



    /// <exclude />
    method __DoValidate(oControl as Control) as void strict
        //RH Check fore Server on control in stead of Server on window
        local oServer as DataServer
        if oControl is RadioButton
            foreach oRBG as RadioButtonGroup in aRadioGroups

                if oRBG:__IsElement( (RadioButton) oControl)
                    oControl:__Update()
                    oControl := oRBG
                    exit
                endif
            next //dwI
        endif

        if oControl:Modified .or. oControl:ValueChanged
            oControl:__Update()
            if oControl:ValueChanged
                if !oControl:PerformValidations()
                    oHLStatus := oControl:Status
                    self:__UpdateStatus()
                else
                    oHLStatus := oControl:Status
                    self:@@StatusMessage(null_string, MessageError)
                    oServer := oControl:Server
                    if oServer != null_object
                        if !oControl:__Gather()
                            oHLStatus := oServer:Status
                            self:__UpdateStatus()
                        endif
                    endif
                endif
                self:PreValidate()
            endif
        endif

        return


    /// <exclude />
    method __DoValidateColumn(oColumn as DataColumn) as void strict
        if oColumn:Modified
            oColumn:__Update()
            if oColumn:ValueChanged
                if !oColumn:PerformValidations()
                    oHLStatus := oColumn:Status
                    self:__UpdateStatus()
                else
                    oHLStatus := oColumn:Status
                    self:StatusMessage(null_string, MessageError)
                    if !IsNil(oAttachedServer)
                        oColumn:__Gather()
                    endif
                endif
                self:PreValidate()
                oColumn:ValueChanged := true
            endif
        endif

        return

    /// <exclude />
    method __EnableHelpCursor(lEnabled as logic) as Window strict
        return super:__EnableHelpCursor(lEnabled)


    /// <exclude />
    method __FindControl(symName as symbol) as Control strict
        foreach oControl  as Control in aControls
            if oControl:NameSym == symName
                return oControl
            endif
        next  // dwI

        return null_object


    /// <exclude />
    method __FindFieldSpec(SymName as symbol) as FieldSpec strict
        local oControl as Control

        if (oControl := self:__FindControl(SymName)) != null_object
            return oControl:FieldSpec
        endif

        return null_object


    /// <exclude />
    method __FindHyperLabel(SymName as symbol) as HyperLabel strict
        local oControl as Control

        if (oControl := self:__FindControl(SymName)) != null_object
            return oControl:HyperLabel
        endif

        return null_object


    /// <exclude />
    method __Gather() as logic strict

        foreach oControl as Control in aControls
            oControl:__Gather()
        next

        return IsNil(oHLStatus)


    /// <exclude />
    method __GetFormSurface() as VOSurfacePanel strict
        return oSurface


    /// <exclude />
    method __GetOLEObject(symMethod as symbol) as DataWindow strict
#ifdef USE_OLEOBJECT
        //RvdH 030825 Methods moved from Ole Classes
        local hFocus := GetFocus() as ptr
        local oOle as OleObject
        local dwI, dwCount as dword
        local oControl as Control
        local lRet as logic

        // neccessary???
        //SE-060526
        dwCount := ALen(aControls)
        for dwI := 1 upto dwCount
            oControl := aControls[dwI]
            if oControl:Handle() == hFocus
                oControl := aControls[dwI]
                exit
            endif
        next  // dwI

        if IsInstanceOf(oControl, #OleObject)
            oOle := object(_cast, oControl)
            oOle:DetachFromServer()
            lRet := Send(oOle, symMethod)
            if (oAttachedServer != null_object) .and. (oOle:__GetDataFldPos > 0)
                if (lRet)
                    oOle:ValueChanged :=true
                    oOle:Modified :=true
                    oAttachedServer:RLOCK()
                    self:FIELDPUT(oOle:__GetDataFldPos, oOle)
                    oOle:__Scatter() // ???!!! correct ??
                    oAttachedServer:Unlock()
                else
                    oOle:__Scatter() // ???!!! correct ??
                endif
            endif
            oOle:RePaint()
        else
            MessageBox(0, String2Psz(ResourceString{IDS_NOINSERT}:VALUE),;
            String2Psz(ResourceString{IDS_OLERUNTIME}:VALUE),;
            _or(MB_OK, MB_ICONHAND))
        endif
#endif
        return self


    /// <exclude />
    method __HandleScrolling(oEvent as Event) as DataWindow strict
        do case
        case oEvent is ScrollEvent var oScroll
            oScroll:ScrollBar:ThumbPosition := oScroll:Position
            self:__DoValidate(oScroll:ScrollBar)

        case oEvent is SpinnerEvent var oSpin
            oSpin:Spinner:ThumbPosition := oSpin:Value
            self:__DoValidate(oSpin:Spinner)

        case oEvent is SliderEvent var oSlide
            oSlide:Slider:ThumbPosition := oSlide:Position
            self:__DoValidate(oSlide:Slider)
        endcase

        return self


    /// <exclude />
    method __RegisterFieldLinks(oDataServer as DataServer) as logic strict
        local dwIndex, dwControls as dword
        local siDF as dword


        dwControls := ALen(aControls)
        if dwControls > 0
            for dwIndex := 1 upto dwControls
                if aControls[dwIndex] is Control var oDC
                    oDC := aControls[dwIndex]
                    siDF := oDataServer:FieldPos(oDC:NameSym)
                    if siDF > 0 .and. IsNil(oDC:Server) // Only one datafield per control
                        oDC:LinkDF(oDataServer, siDF) // Exit here, only one control per
                        lLinked := true
                    endif
                endif
            next
        else
            // Only autolayout if there are _no_ controls
            if ALen(self:GetAllChildren()) == 0
                self:__AutoLayout()
                lLinked := true
            endif
        endif

        if lLinked
            oDataServer:RegisterClient(self)
        endif

        return lLinked


    /// <exclude />
    method __RegisterSubform(oSubForm as DataWindow) as DataWindow strict
        AAdd(aSubForms, oSubForm)
        self:oSurface:AddControl(oSubForm:__Frame)
        // Set the parent of the __DataForm of the Subwindow to our parent
        oSubForm:__DataForm:Owner := self:__DataForm:Owner
        return self


    /// <exclude />
    method __Scatter() as DataWindow strict

        if (sCurrentView == #FormView)
            foreach oControl as Control in aControls
                oControl:__Scatter()
            next
            lRecordDirty := false
            self:PreValidate()
        elseif (oGBrowse != null_object)
            Send(oGBrowse, #__RefreshData)
            lRecordDirty := false
            self:PreValidate()
        endif

        return self



    /// <exclude />
    method __SetupDataControl(oDC as Control) as void
        // DebOut(__FUNCTION__)
        if oDC is RadioButtonGroup
            AAdd(aRadioGroups, oDC)
        endif

        if __DataForm:AutoLayout .and. !(oDC is FixedText)
            oDC:SetStyle(WS_TabStop)
        endif
        AAdd(aControls, oDC)

        return


    /// <exclude />
    method __SetupNonDataControl(oDC as Control) as void
        //DebOut(__FUNCTION__)
        if __DataForm:AutoLayout .and. !(oDC is FixedText)
            oDC:SetStyle(WS_TabStop)
        endif

        return


    /// <exclude />
    method __StatusMessage(uDescription as usual, nMode as longint) as DataWindow strict
        local uTemp as usual


        if uDescription is HyperLabel var oHL
            uTemp := oHL:Description
            if Empty(uTemp) .or. IsNil(uTemp)
                uTemp := null_string
            endif
        else
            uTemp := uDescription
        endif

        // sTmp is now a symbol or String

        if IsSymbol(uTemp)
            uTemp := Symbol2String(uTemp)
        elseif IsNil(uTemp)
            uTemp := null_string
        endif

        if !IsString(uTemp)
            uTemp := ResourceString{__WCSUnknownStatusMSG}:Value
        endif
        self:@@StatusMessage(uTemp, nMode)
        return self


    /// <exclude />
    property __SubForm as logic get lSubForm

    /// <exclude />

    property __HasSurface as logic get true
    /// <exclude />

    property __Surface as IVOPanel get oSurface

    /// <exclude />
    method __Unlink() as logic strict

        if oAttachedServer == null_object
            return false
        endif
        foreach oControl as Control in aControls
            oControl:__Unlink()
        next

        if oGBrowse != null_object
            oGBrowse:__Unlink()
        endif
        oAttachedServer:UnRegisterClient(self, lAllowServerClose)
        oAttachedServer := null_object
        lLinked := false

        return true


    /// <exclude />
    method __UnRegisterDataControl(oControl as Control) as DataWindow strict
        local dwI, dwCount as dword


        // test???
        //RETURN SELF

        dwCount := ALen(aControls)
        for dwI := 1 upto dwCount
            if aControls[dwI] = oControl
                ADel(aControls, dwI)
                ASize(aControls, dwCount-1)
                exit
            endif
        next  // dwI

        dwCount := ALen(aConditionalControls)
        for dwI := 1 upto dwCount
            if aConditionalControls[dwI] = oControl
                ADel(aConditionalControls, dwI)
                ASize(aConditionalControls, dwCount-1)
                exit
            endif
        next  // dwI

        dwCount := ALen(aRadioGroups)
        for dwI := 1 upto dwCount
            if aRadioGroups[dwI] = oControl
                ADel(aRadioGroups, dwI)
                ASize(aRadioGroups, dwCount-1)
                exit
            endif
        next  // dwI

        return self


    /// <exclude />
    method __UnRegisterSubform(oSubForm as object) as DataWindow strict
        local dwI, dwCount as dword



        dwCount := ALen(aSubForms)
        for dwI := 1 upto dwCount
            if aSubForms[dwI] == oSubForm
                ADel(aSubForms, dwI)
                ASize(aSubForms, dwCount - 1)
                exit
            endif
        next  // dwI

        return self


    /// <exclude />
    method __UpdateActiveObject() as DataWindow strict
        //         LOCAL oOle AS OBJECT
        //         LOCAL i AS DWORD
        //         LOCAL aObjects 	AS ARRAY
        //         aObjects := SELF:__GetMyOleObjects()
        //         FOR i:= 1 TO ALen(aObjects)
        //             oOle := aObjects[i]
        //             IF oOle:IsInPlaceActive
        //                 oOle:UpdateTools()
        //             ENDIF
        //         NEXT
        return self



    /// <exclude />
    method __UpdateCurrent() as DataWindow strict
        local oColumn as object


        if (sCurrentView == #FormView)
            if oDCCurrentControl is Control var oC  .and. oC:Modified
                self:__DoValidate(oC)
            endif
        elseif (sCurrentView == #BrowseView) .and. IsAccess(oGBrowse, #CurrentColumn)
            oColumn := IVarGet(oGBrowse, #CurrentColumn)
            if oColumn is DataColumn var oDC .and. oDC:Modified
                self:__DoValidateColumn(oDC)
            endif
        endif
        return self


    /// <exclude />
    method __UpdateStatus() as logic strict

        if oHLStatus != null_object
            if oHLStatus is HyperLabel
                self:__StatusMessage(ResourceString{__WCSError2}:Value + oHLStatus:Description, MESSAGEERROR)
            endif
            return false
        endif
        // No error
        self:__StatusMessage(null_string, MessageError)

        return true


    /// <exclude />
    method __VerifyDataServer(oDataServer as object) as logic strict

        if oDataServer is DataServer var oDS
            oAttachedServer := oDS
        elseif oDataServer is string var strServer
            oAttachedServer := CreateInstance(#DBServer, strServer)
            oAttachedServer:ConcurrencyControl := nCCMode
        elseif oDataServer is symbol var symServer
            oAttachedServer := CreateInstance(#DBServer, symServer)
            oAttachedServer:ConcurrencyControl := nCCMode
        elseif oDataServer is FileSpec var oFS
            oAttachedServer := CreateInstance(#DBServer, oFS)
            oAttachedServer:ConcurrencyControl := nCCMode
        else
            return false
        endif

        if IsInstanceOf(oAttachedServer, #DBServer)
            //Use the Used access and not the Status access because the last
            //operation issued before the Use() method may have failed
            if !IVarGet(oAttachedServer,#Used)
                oHLStatus := oAttachedServer:Status
                if oHLStatus == null_object
                    oHLStatus := HyperLabel{#Use, , VO_Sprintf(__WCSDSNotOpen, AsString(oAttachedServer), IVarGet(IVarGet(oAttachedServer,#FileSpec),#FullPath))}
                endif
                return false
            endif
        elseif IsInstanceOf(oAttachedServer, #SQLSelect) .and. ; //For SQL Server, check for error
                oAttachedServer:Status != null_object .and. ; //on the Init method
                IVarGet(IVarGet(oAttachedServer,#ErrInfo),#FuncSym) == #INIT
            oHLStatus := oAttachedServer:Status
            return false
        endif

        return true

    /// <include file="Gui.xml" path="doc/DataWindow.Activate/*" />
    method Activate (oEvent  as Event) as usual
        if (oFrame != null_object)
            WC.AppSetDialogWindow(oFrame)
        endif

        return self:Default(oEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.AllowServerClose/*" />

    property AllowServerClose as logic get lAllowServerClose set lAllowServerClose := value


    /// <include file="Gui.xml" path="doc/DataWindow.Append/*" />
    method Append() as logic clipper
        // Adds new record to DataWindow
        local lRetCode := false as logic

        oHLStatus := null_object // assume success
        if (oAttachedServer != null_object) .and. self:__CheckRecordStatus()
            if !(lRetCode := oAttachedServer:Append(true))
                oHLStatus := oAttachedServer:Status
                self:__UpdateStatus()
            else
                lRecordDirty := true
                // The notification procedure will set up the controls
            endif
        endif

        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.AutoScroll/*" />
    property AutoScroll  as logic get lAutoScroll set lAutoScroll := value


    /// <include file="Gui.xml" path="doc/DataWindow.Background/*" />
    property Background as Brush
        get
            //Only an optimization to avoid unneeded Window:PaintBackground() calls of
            //the DataWindow object itself or the __FormFrame.
            if oSurface != null_object
                return Brush{(Color)oSurface:BackColor}
            endif

            return null_object
        end get
        set
            //Only an optimization to avoid unneeded Window:PaintBackground() calls of
            //the DataWindow object itself or the __FormFrame.
            if oSurface != null_object .and. value != null_object
                oSurface:BackColor := value
            endif
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.Browser/*" />
    property Browser as DataBrowser
        get
            if oGBrowse is DataBrowser var oBrow
                return oBrow
            endif
            return null_object
        end get
        set
            oGBrowse := value
            __DataForm:DataBrowser := value:__DataGridView
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.BrowserClass/*" />
    property BrowserClass  as symbol get symBrowserClass set symBrowserClass := value


    /// <include file="Gui.xml" path="doc/DataWindow.ButtonClick/*" />
    method ButtonClick(oControlEvent as ControlEvent) as usual
        local oButton as Control
        local oWindow as Window
        local dwI, dwCount as dword
        local oRBG as RadioButtonGroup
        local aRadioGrps as array
        local lUnchanged as logic


        oButton := oControlEvent:Control

        if oButton:Owner is Window var oW
            oWindow := oW
        else
            oWindow := self
        endif
        if oButton is Button
            oButton:Modified := true // assume its modified
            if oButton is RadioButton var oRB
                //SE-060526
                aRadioGrps := IVarGet(oWindow, #__aRadioGroups)
                dwCount := ALen(aRadioGrps)
                for dwI := 1 upto dwCount
                    oRBG := aRadioGrps[dwI]
                    if oRBG:__IsElement(oRB)
                        lUnchanged := oRBG:__AlreadyHasFocus(oRB)
                        oRBG:__SetOn(oRB)
                        exit
                    endif
                next  // dwI
            elseif oButton is CheckBox
                lUnchanged := true
            endif
            ((DataWindow) oWindow):__DoValidate(oButton)
            oButton:ValueChanged := !lUnchanged
        endif
        return super:ButtonClick(oControlEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.Cancel/*" />
    method Cancel() as logic strict
        local lRetCode := false as logic
        lRetCode := true
        if IsMethod(oAttachedServer,#Refresh)
            lRetCode := Send(oAttachedServer,#Refresh)
        endif
        self:EndWindow()

        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.CanvasErase/*" />
    method CanvasErase() as void

        if oSurface != null_object
            oSurface:Invalidate()
        endif
        super:CanvasErase()


    /// <include file="Gui.xml" path="doc/DataWindow.Caption/*" />
    property Caption as string
        get
            return super:Caption
        end get
        set
            if !lTopApp .and. (lSubForm)
                return
            endif
            super:Caption := value
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.ChangeFont/*" />
    method ChangeFont(oFont as Font, lUpdate := false as logic)
        self:Font := oFont
        if oSurface != null_object
            oSurface:Font := oFont:__Font
        endif

        return oFont


    /// <include file="Gui.xml" path="doc/DataWindow.CheckStatus/*" />
    method CheckStatus() as logic
        local oOldStatus as object

        oOldStatus := oHLStatus
        if !self:StatusOK()
            if sCurrentView == #FormView
                if (oDCInvalidControl != null_object)
                    oDCInvalidControl:SetFocus()
                endif
            elseif (sCurrentView == #BrowseView)
                if (oDCInvalidColumn != null_object) .and. IsMethod(oGBrowse, #SetColumnFocus)
                    Send(oGBrowse, #SetColumnFocus, oDCInvalidColumn)
                endif
            endif
        endif

        // Prevalidate based on status change
        if (oHLStatus != oOldStatus)
            self:PreValidate()
        endif

        if (!IsNil(oHLStatus))
            self:__UpdateStatus()
            return false
        endif

        return true


    /// <include file="Gui.xml" path="doc/DataWindow.Clear/*" />
    method Clear() as void strict

        if sCurrentView == #FormView
            if oDCCurrentControl is SingleLineEdit var oSLE
                oSlE:__SetText(null_string)
            elseif oDCCurrentControl is MultiLineEdit var oMLE
                oMLE:__SetText(null_string)
            elseif oDCCurrentControl is EditWindow var oEW
                oEW:Clear()
            elseif oDCCurrentControl is ControlWindow var oCW
                if oCW:Control != null_object .and. IsMethod(oDCCurrentControl, #Clear)
                    Send(oCW:Control,#Clear)
                endif
            endif
        elseif sCurrentView == #BrowseView
            if (oGBrowse != null_object) .and. IsMethod(oGBrowse, #Clear)
                Send(oGBrowse, #Clear)
            endif
        endif

        return


    method ClearRelations

        if oAttachedServer!=null_object .and. IsMethod(oAttachedServer, #ClearRelation)
            return Send(oAttachedServer,#ClearRelation)
        endif
        return false


    /// <include file="Gui.xml" path="doc/DataWindow.ClipperKeys/*" />
    // todo: Implement ClipperKeys
    property ClipperKeys as logic get false set


    /// <include file="Gui.xml" path="doc/DataWindow.Close/*" />
    method Close(oEvent as event)  as usual
        if (oAttachedServer != null_object)
            if lRecordDirty .and. IsNil(oHLStatus )
                if IsMethod (oAttachedServer, #Commit) .and. oAttachedServer:Commit()
                    lRecordDirty := false
                endif
            endif
            self:__Unlink()
        endif
        // Workaround for TreeView Control Loosing focus bug
        //SetFocus(GuiWin32.GetParent(SELF:Handle()))
        return super:Close(oEvent)



    /// <include file="Gui.xml" path="doc/DataWindow.Commit/*" />
    method Commit() as logic
        local lRetCode := false as logic

        oHLStatus:= null_object // assume success
        if oAttachedServer!=null_object .and. self:__CheckRecordStatus()
            if !(lRetCode:=oAttachedServer:Commit())
                oHLStatus := oAttachedServer:Status
                self:__UpdateStatus()
            else
                lRecordDirty := false
            endif
        endif

        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.ConcurrencyControl/*" />
    property ConcurrencyControl as usual
        get
            if IsNil(oAttachedServer)
                return self: nCCMode
            endif

            return oAttachedServer:ConcurrencyControl
        end get
        set
            local newMode as int
            if IsString(value)
                value := String2Symbol(value)
            endif

            if IsSymbol(value)
                do case
                case value == #ccNone
                    newMode := ccNone
                case value == #ccOptimistic
                    newMode := ccOptimistic
                case value == #ccStable
                    newMode := ccStable
                case value == #ccRepeatable
                    newMode := ccRepeatable
                case value == #ccFile
                    newMode := ccFile
                otherwise
                    WCError{#ConcurrencyControl,#DataWindow,__WCSTypeError,value,1}:Throw()
                endcase
            elseif IsNumeric(value)
                newMode := value
            else
                WCError{#ConcurrencyControl,#DataWindow,__WCSTypeError,value,1}:Throw()
            endif

            self:nCCMode := newMode
            if oAttachedServer!=null_object
                oAttachedServer:ConcurrencyControl:=nCCMode
            endif
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.ContextMenu/*" />
    assign ContextMenu(oNewMenu as Menu)
        self:SetContextMenu(oNewMenu, #Both)
        return


    /// <include file="Gui.xml" path="doc/DataWindow.SetContextMenu/*" />
    method SetContextMenu(oNewMenu as Menu, symView as symbol) as void
        local lForm    as logic
        local lBrowser as logic

        if symView = #BrowseView
            lBrowser := true
        elseif symView = #FormView
            lForm := true
        else
            lForm := lBrowser := true
        endif

        if lForm
            //Todo ?
            //IF oSurface != NULL_OBJECT
            //	oSurface:ContextMenu := oNewMenu:__Menu
            //ENDIF
            super:ContextMenu := oNewMenu
        endif

        if lBrowser
            if oGBrowse != null_object
                oGBrowse:ContextMenu := oNewMenu
            endif
        endif

        return

    /// <include file="Gui.xml" path="doc/DataWindow.Controls/*" />
    property Controls as array get self:aControls

    /// <include file="Gui.xml" path="doc/DataWindow.ControlFocusChange/*" />
    method ControlFocusChange(oControlFocusChangeEvent as  ControlFocusChangeEvent) as usual
        local oControl as Control
        local cMessage as string
        local dwI, dwCount as dword
        local oRBG as RadioButtonGroup
        local oDCHyperLabel as HyperLabel
        local oCFCE := oControlFocusChangeEvent as  ControlFocusChangeEvent

        if oSurface != null_object

            oControl := oCFCE:Control

            if oCFCE:GotFocus

                if self:AutoScroll
                    oControl:__EnsureVisibity()
                endif
                self:LastFocus := oControl

                WC.AppSetDialogWindow(self:oFrame)
                if ! oControl == null_object
                    if oControl is RadioButton var oRB
                        //SE-060526
                        dwCount := ALen(aRadioGroups)
                        for dwI := 1 upto dwCount
                            oRBG := aRadioGroups[dwI]
                            if oRBG:__IsElement(oRB)
                                oControl := oRBG
                                exit
                            endif
                        next  // dwI
                    endif

                    // save active control
                    oDCCurrentControl := oControl

                    // if there is an outstanding error on the control - display it
                    if oControl:Status != null_object
                        if IsString(oControl:Status:Description)
                            cMessage := oControl:Status:Description
                        else
                            cMessage := ResourceString{__WCSUnknownStatus}:Value
                        endif
                    endif
                    self:__StatusMessage(cMessage, MessageError)

                    // Reset message for control
                    cMessage := null_string

                    // SetUp Prompt
                    oDCHyperLabel := oControl:HyperLabel
                    if oDCHyperLabel != null_object
                        cMessage := oDCHyperLabel:Description
                    endif
                    self:__StatusMessage(cMessage, MessageControl)
                else
                    self:__StatusMessage(null_string, MessageError)
                    self:__StatusMessage(null_string, MessageControl)
                    oDCCurrentControl := null_object
                endif

            else
                self:__DoValidate(oControl)
            endif
        endif

        return self


    /// <include file="Gui.xml" path="doc/DataWindow.Copy/*" />
    method Copy()   as void strict
        if (sCurrentView == #FormView)
            if oDCCurrentControl is Edit var oEdit
                oEdit:Copy()
            elseif oDCCurrentControl is EditWindow var oEditWindow
                oEditWindow:Copy()
            elseif oDCCurrentControl is ControlWindow var oCW
                if oCW:Control != null_object .and. IsMethod(oCW:Control, #Copy)
                    SEnd(oCW:Control,#Copy)
                endif
            endif
        elseif (sCurrentView == #BrowseView)
            if (oGBrowse != null_object) .and. IsMethod(oGBrowse, #copy)
                Send(oGBrowse, #Copy)
            endif
        endif

        return

    /// <include file="Gui.xml" path="doc/DataWindow.CurrentControl/*" />
    property CurrentControl  as object get oDCCurrentControl set oDCCurrentControl := value

    /// <include file="Gui.xml" path="doc/DataWindow.CurrentView/*" />
    property CurrentView as symbol get self:sCurrentView

    /// <include file="Gui.xml" path="doc/DataWindow.Cut/*" />
    method Cut()   as void strict
        if (sCurrentView == #FormView)
            if oDCCurrentControl is Edit var oEdit
                oEdit:Cut()
            elseif oDCCurrentControl is EditWindow var oEditWindow
                oEditWindow:Cut()
            elseif oDCCurrentControl is ControlWindow var oCW
                if oCW:Control != null_object .and. IsMethod(oCW:Control, #Cut)
                    Send(oCW:Control,#Cut)
                endif
            endif
        elseif (sCurrentView == #BrowseView)
            if (oGBrowse != null_object) .and. IsMethod(oGBrowse, #cut)
                Send(oGBrowse, #Cut)
            endif
        endif

        return


    /// <include file="Gui.xml" path="doc/DataWindow.DeActivate/*" />
    method DeActivate(oEvent as Event) as usual
        //RvdH 030825 Call to DeactivateAllOLEObjects moved to Window
        return super:DeActivate(oEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.DeactivateAllOLEObjects/*" />
    method DeactivateAllOLEObjects(oExcept)  as usual
        super:DeactivateAllOLEObjects(oExcept)
        if !lTopApp .and. IsMethod(self:Owner, #__AdjustClient)
            Send(self:Owner,#__AdjustClient)
            //ELSE
            //	SELF:__AdjustSurface()
        endif
        return self


    /// <include file="Gui.xml" path="doc/DataWindow.DeferUse/*" />
    property DeferUse as logic get lDeferUse set lDeferUse := value

    /// <include file="Gui.xml" path="doc/DataWindow.Delete/*" />
    method Delete() clipper
        local nRecno as longint
        local nLastRec as longint
        local fSQL as logic
        local fBrowse as logic
        local fRet as logic

        if oAttachedServer != null_object
            nRecno := oAttachedServer:Recno
            nLastRec:= oAttachedServer:LASTREC
            fBrowse := self:sCurrentView = #BROWSE
            fSQL := IsInstanceOfUsual( oAttachedServer, #SQLSELECT)

        endif

        fRet := self:__Delete()

        if fSQL .and. fRet


            if nLastRec < nRecno
                self:GoTop()
                self:GoBottom()
                return fRet
            endif

            if oAttachedServer:EOF .and. oAttachedServer:BOF
                self:GoTop()
            else
                if fBrowse
                    self:Skip()
                else
                    if nRecno = 1
                        self:GoTo(1)
                    else
                        self:GoTo(nRecno-1)
                    endif
                endif
            endif
        endif

        return(fRet)



    /// <include file="Gui.xml" path="doc/DataWindow.DeleteValidated/*" />
    method DeleteValidated
        local lRetCode := false as logic

        oHLStatus:= null_object // assume success
        if oAttachedServer!=null_object .and. self:__CheckRecordStatus()
            if !(lRetCode:=self:__Delete())
                oHLStatus:=oAttachedServer:Status
                self:__UpdateStatus()
            endif
        endif
        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.Destroy/*" />
    method Destroy() as usual
        local oSubForm as Window

        if oAttachedServer != null_object
            self:__Unlink()
        endif

        if lSubForm .and. oParent is DataWindow var oDW
            oDW:__UnRegisterSubForm(self)
        endif

        // If this window has subforms destroy them first
        do while ALen(aSubForms) > 0
            oSubForm := aSubForms[1]
            oSubForm:Close(Event{})
            oSubForm:Destroy()
        end do
        aSubForms := null_array

        if oGBrowse != null_object
            oGBrowse:__Unlink()
            oGBrowse:Destroy()
            oGBrowse:= null_object
        endif
        if WC.AppGetDialogWindow() == oFrame
            WC.AppSetDialogWindow(null_object)
        endif
        if oFrame != null_object
            oFrame:CleanUp()
            oFrame:Dispose()
            oFrame := null_object
        endif
        if oSurface != null_object
            oSurface:CleanUp()
            oSurface:Dispose()
            oSurface := null_object
        endif
        aControls := null_array

        oDCCurrentControl := null_object
        self:lValidFlag := false
        self:oLastFocus := null_object

        super:Destroy()

        return self


    /// <include file="Gui.xml" path="doc/DataWindow.DisableConditionalControls/*" />
    method DisableConditionalControls()
        if lControlsEnabled
            foreach oControl as Control in aConditionalControls
                oControl:Disable()
            next
            lControlsEnabled := false
        endif
        return self


    /// <include file="Gui.xml" path="doc/DataWindow.Draw/*" />
    method Draw(oDrawObject)
        //  Todo Draw
        if oSurface != null_object
            //oSurface:Draw(oDrawObject)
            nop
        endif

        return self

    /// <include file="Gui.xml" path="doc/DataWindow.EditChange/*" />
    method EditChange(oControlEvent as ControlEvent) as usual
        local oCurrentControl := null_object as object

        oCurrentControl := oControlEvent:Control
        if oCurrentControl is ListBox var oLB
            oLB:Modified := true
        elseif oCurrentControl is IPAddress var oIP
            oIP:Modified := true // mark it as modified
        endif

        if (oDCCurrentControl == oCurrentControl)
            self:__StatusMessage("", MessageError)
        endif
        return super:EditChange(oControlEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.EditFocusChange/*" />
    method EditFocusChange(oEditFocusChangeEvent as EditFocusChangeEvent) as usual
        var result := super:EditFocusChange(oEditFocusChangeEvent)

        if !oEditFocusChangeEvent:GotFocus
            if oEditFocusChangeEvent:Control != null_object
                oEditFocusChangeEvent:Control:__Update()
            endif
        endif

        return result

    /// <include file="Gui.xml" path="doc/DataWindow.EnableConditionalControls/*" />
    method EnableConditionalControls()
        if !lControlsEnabled
            foreach oControl as Control in aConditionalControls
                oControl:Enable()
            next
            lControlsEnabled := true
        endif
        return self

    /// <include file="Gui.xml" path="doc/DataWindow.EnableDragDropClient/*" />
    method EnableDragDropClient(lEnable := true as logic, lSurfaceOnly := true as logic) as void
        self:oSurface:AllowDrop := true

        super:EnableDragDropClient(lEnable)



    /// <include file="Gui.xml" path="doc/DataWindow.EnableStatusBar/*" />
    method EnableStatusBar(lEnable as logic) as StatusBar
        super:EnableStatusBar(lEnable)

        // No need to resize. __DataForm handles this
        return self:StatusBar


    /// <include file="Gui.xml" path="doc/DataWindow.EnableToolTips/*" />
    //METHOD EnableToolTips(lEnable)


    //	RETURN oSurface:EnableToolTips(lEnable)


    /// <include file="Gui.xml" path="doc/DataWindow.Error/*" />
    method Error(oErrorObj)
        return self


    /// <include file="Gui.xml" path="doc/DataWindow.Expose/*" />
    method Expose(oExposeEvent as ExposeEvent) as usual
        return super:Expose(oExposeEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.FIELDGET/*" />
    method FieldGet(uFieldID as usual)  as usual
        local oError as usual
        local oFieldObject as object
        local uRetVal := nil as usual

        begin sequence

            if (sCurrentView == #BrowseView) .and. IsMethod(oGBrowse, #GetColumn)
                oFieldObject := Send(oGBrowse, #GetColumn, uFieldID)
            else
                if IsNumeric(uFieldID)
                    oFieldObject := aControls[uFieldID]
                elseif IsSymbol(uFieldID)
                    oFieldObject := self:__FindControl(uFieldID)
                elseif IsString(uFieldID)
                    oFieldObject := self:__FindControl(String2Symbol(uFieldID))
                endif
            endif

            if oFieldObject == null_object
                if oAttachedServer != null_object
                    uRetVal := oAttachedServer:FIELDGET(uFieldID)
                else
                    uRetVal := nil
                endif
            elseif oFieldObject is CheckBox var cb
                uRetVal := cb:Checked
            elseif oFieldObject is RadioButton var rb
                uRetVal := rb:Pressed
            elseif oFieldObject is Control var oC
                uRetVal := oC:Value
            else
                uRetVal := IVarGet(oFieldObject,#Value)
            endif

        recover using oError

            break oError

        end sequence
        return uRetVal


    /// <include file="Gui.xml" path="doc/DataWindow.FIELDPUT/*" />
    method FieldPut(uFieldId as usual, uNewValue as usual) as usual
        // Retrieves the current value of the indicated string
        // uFieldPosition is numeric, symbol or string: the field position as numeric,
        // or the field name as a symbol or a string
        local oError as usual
        local oFieldObject as usual
        local dwFieldObject as dword
        local uRetVal := nil as usual

        begin sequence

            if (sCurrentView == #BrowseView) .and. IsMethod(oGBrowse, #GetColumn)
                oFieldObject := Send(oGBrowse, #GetColumn, uFieldId)
            else
                if IsNumeric(uFieldId)
                    //SE-060526 this was not the same as in method FieldGet()
                    //dwFieldObject := AScan(aControls, {|x| x:__GetDataFldPos == uField})
                    oFieldObject := aControls[uFieldId]
                elseif IsSymbol(uFieldId)
                    oFieldObject := self:__FindControl(uFieldId)
                elseif IsString(uFieldId)
                    oFieldObject := self:__FindControl(String2Symbol(uFieldId))
                endif

            endif
            if dwFieldObject > 0
                oFieldObject := aControls[dwFieldObject]
            endif

            // Field object should contain control or column
            if IsNil(oFieldObject)
                if oAttachedServer != null_object
                    uRetVal := oAttachedServer:FIELDPUT(uFieldId, uNewValue)
                else
                    uRetVal := nil
                endif
            else
                uRetVal := uNewValue
                IVarPut(oFieldObject,#Value, uNewValue )
            endif

        recover using oError

            break oError

        end sequence
        return uRetVal


    /// <include file="Gui.xml" path="doc/DataWindow.FocusChange/*" />
    method FocusChange(oFocusChangeEvent as FocusChangeEvent) as usual
        if oFocusChangeEvent:GotFocus  .and. __DataForm != null_object
            __DataForm:SetFocusToForm()
        endif
        return super:FocusChange(oFocusChangeEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.Foreground/*" />
    property Foreground as Brush
        get
            return super:Foreground
        end get
        set
            super:Foreground := value
            if ( oSurface != null_object )
                oSurface:ForeColor := value:Color
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/DataWindow.GoBottom/*" />
    method GoBottom() as logic strict
        local lRetCode := false as logic

        oHLStatus:=null_object // assume success
        if oAttachedServer!=null_object .and. self:__CheckRecordStatus() // send data to Server
            if !(lRetCode:=oAttachedServer:GoBottom()) // if Skip is successful...
                oHLStatus:=oAttachedServer:Status // pick up Server's reason code
                self:__UpdateStatus()
            endif
        endif
        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.GoTo/*" />
    method GoTo( nRecNo as long ) as logic
        local lRetCode := false as logic

        oHLStatus:=null_object // assume success
        if(oAttachedServer!=null_object .and. self:lValidFlag)
            if self:__CheckRecordStatus()
                if !(lRetCode:=oAttachedServer:GoTo( nRecNo ))
                    oHLStatus:=oAttachedServer:Status
                    self:__UpdateStatus()
                endif
            endif
        endif
        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.GoTop/*" />
    method GoTop() as logic strict
        local lRetCode := false as logic

        oHLStatus:=null_object // assume success
        if oAttachedServer!=null_object .and. self:__CheckRecordStatus() // send data to Server
            if !(lRetCode:=oAttachedServer:GoTop()) // if Skip is unsuccessful...
                oHLStatus:=oAttachedServer:Status // pick up Server's reason code
                self:__UpdateStatus()
            endif
        endif
        return lRetCode

    /// <include file="Gui.xml" path="doc/DataWindow.Hide/*" />
    method Hide() as void strict
        if lSubForm
            self:__DataForm:HideSubForm()
        else
            super:Hide()
        endif
        return

    property HyperLabel as HyperLabel
        get
            return super:HyperLabel
        end get
        set
            super:HyperLabel := value
            if value != null_object
                self:oSurface:Text := "Surface: "+value:Name
                self:__Frame:Text	:= "Frame: "+value:Name
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/DataWindow.HorizontalScroll/*" />
    method HorizontalScroll(oScrollEvent as ScrollEvent) as usual
        self:__HandleScrolling(oScrollEvent)
        return self:Default(oScrollEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.HorizontalSlide/*" />
    method HorizontalSlide(oSlideEvent as SliderEvent) as usual
        self:__HandleScrolling(oSlideEvent)
        return self:Default(oSlideEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.HorizontalSpin/*" />
    method HorizontalSpin(oSpinEvent as SpinnerEvent) as usual
        self:__HandleScrolling(oSpinEvent)
        return self:Default(oSpinEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.ctor/*" />
    constructor(oOwner, oSource, nResourceID, nDialogStyle)
        local oResID as ResourceID
        local oObject as object
        local oDwOwner as DataWindow

        DEFAULT( ref oOwner, GetAppObject())
        if IsLong(nDialogStyle)
            dwDialogStyle := nDialogStyle
        endif

        if dwDialogStyle > 0
            oParent := oOwner
            //__DDImp{SELF, TRUE, dwDialogStyle}
        endif

        if IsNil(oSource)
            oResID := ResourceID{ -1 }
        elseif IsString(oSource) .or. IsLong(oSource)
            oResID := ResourceID{oSource}
        else
            oResID := oSource
        endif
        self:oResourceID := oResID


        if IsObject(oOwner)
            oObject := oOwner
            switch oObject
            case App
                super(nil, false)
                lTopApp := true
                // lQuitOnClose := true
            case DialogWindow
                super(oOwner, true)
            case DataWindow
                // Create sub form if we're a regular DataWindow
                DEFAULT( ref nResourceID, 0)
                lSubForm := (dwDialogStyle = 0)
                oDwOwner := oOwner
                super(oOwner, false, false)
            case AppWindow
                super(oOwner)
            case Window
                // <XXX> invalid Owner - throw error
                WCError{#Init,#DataWindow,__WCSTypeError,oOwner,1}:Throw()
            otherwise
                super(oOwner)
            end switch
        else
            super(oOwner)
        endif

        if IsObject(oOwner) .and. oOwner is Window var oWin
            self:oCurrentHelp := oWin:Helpdisplay
        endif
        sCurrentView := #FormView
        self:Caption := ResourceString{__WCSDWUntitled}:Value
        aControls		:= {}
        aRadioGroups	:= {}
        aConditionalControls := {}
        nCCMode			:= CCOptimistic
        self:lValidFlag := true
        lControlsEnabled := true
        lAutoScroll		:= true
        lAllowServerClose := true
        aSubForms		:= {}
        symBrowserClass := gsymBrowserDef

        if lSubForm
            __DataForm:CreateSubForm(nResourceID,oDwOwner:ResourceDialog)
            oDwOwner:__RegisterSubForm(self)
        endif
//         if ( self:Background != null_object )
//             oSurface:BackColor := self:Background:Color
//         endif

        //IF oContextMenu != NULL_OBJECT
        //	oSurface:ContextMenu   := oContextMenu:__Menu
        //ENDIF

        return


    /// <include file="Gui.xml" path="doc/DataWindow.InsertObject/*" />
    method InsertObject() strict
        //RvdH 030825 Method moved from Ole Classes
        return self:__GetOLEObject(#CreateFromInsertDialog)

    /// <include file="Gui.xml" path="doc/DataWindow.IsDialog/*" />
    method IsDialog() as logic strict
        //SE-070906
        return dwDialogStyle > 0

    method IsVisible()  as logic strict
        if lSubForm
            return self:oSurface:Visible
        endif
        return super:IsVisible()

    /// <include file="Gui.xml" path="doc/DataWindow.LastFocus/*" />
    property LastFocus as Control
        get
            if sCurrentView == #BrowseView
                return (Control) oGBrowse
            endif
            return oLastFocus
        end get
        set
            if ! (value is DataBrowser)
                if lSubForm
                    if IsAssign(oParent, #LastFocus)
                        IVarPut(oParent, #LastFocus, value)
                    endif
                endif
                oLastFocus := value
            endif
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.LineTo/*" />
    method LineTo(oPoint as Point) as usual
        //Todo	LineTo

        if (oSurface != null_object)
            //oSurface:LineTo(uPoint)
            nop
        endif
        return oPoint


    /// <include file="Gui.xml" path="doc/DataWindow.ListBoxClick/*" />
    method ListBoxClick(oControlEvent as ControlEvent) as usual
        local oListBox as ListBox
        oListBox := (ListBox) oControlEvent:Control
        oListBox:Modified := true // assume its modified
        self:__DoValidate(oListBox)
        return super:ListBoxClick(oControlEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.ListBoxSelect/*" />
    method ListBoxSelect(oControlEvent as ControlEvent) as usual
        local oListBox as BaseListBox
        oListBox := (BaseListBox) oControlEvent:Control
        oListBox:Modified := true // assume its modified
        oListBox:__SetText(oListBox:CurrentItem)
        self:__DoValidate(oListBox)
        return super:ListBoxSelect(oControlEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.Menu/*" />
    property Menu as VOSDK.Menu
        get
            return super:Menu
        end get
        set
            super:Menu := value
            if oParent is ShellWindow
                // No need to resize. __DataForm handles this
                //__DataForm:ResizeParent()
                nop
            endif
        end set
    end property


    /// <include file="Gui.xml" path="doc/DataWindow.MouseButtonDown/*" />
    method MouseButtonDown(oMouseEvent as MouseEvent) as usual
        //RvdH 030825 Method moved from Ole Classes

        self:DeactivateAllOLEObjects()

        return super:MouseButtonDown(oMouseEvent)


    /// <include file="Gui.xml" path="doc/DataWindow.MoveTo/*" />
    method MoveTo(oPoint as Point)  as Point
        //Todo	 MoveTo

        if (oSurface != null_object)
            //oSurface:MoveTo(oPoint)
            nop
        endif

        return oPoint


    /// <include file="Gui.xml" path="doc/DataWindow.Notify/*" />
    method Notify(kNotification, uDescription)
        local oTB as TextBox
        local i, iLen as int
        local lThisRecDeleted as logic
        local oDF as DataField
        local oControl as Control


        switch (int) kNotification
        case NOTIFYCOMPLETION
            // Do nothing, __NotifyCompletion had no code in it

        case NOTIFYINTENTTOMOVE
            //RvdH MOved from OleDataWindow
            self:DeactivateAllOLEObjects()

            if (oAttachedServer == null_object)
                return true
            endif
            if !self:CheckStatus()
                oTB:=TextBox{self, ResourceString{__WCSWarning}:Value, ResourceString{__WCSChgDiscard}:Value}
                oTB:Type:=BUTTONOKAYCANCEL+BOXICONHAND
                if oTB:Show()!=BOXREPLYOKAY
                    return false
                else
                    //Put original data back if moving to another record
                    if (sCurrentView == #BrowseView)
                        self:__Scatter()
                        Send(oGBrowse, #__NotifyChanges, GBNFY_FIELDCHANGE)
                    endif
                endif
            endif
            return true

        case NOTIFYFILECHANGE
            self:__Scatter()

        case NOTIFYFIELDCHANGE
            lRecordDirty := true
            if (sCurrentView == #FormView)
                iLen := int(ALen(aControls))
                for i:= 1 to iLen
                    oControl := aControls[i]
                    oDF := oControl:__DataField
                    if (oDF != null_object) .and. (oDF:NameSym == uDescription)
                        oControl:__Scatter()
                    endif
                next
                // RvdH 060529 This is done in the DataBrowser:Notify as well
                //ELSEIF (sCurrentView == #BrowseView)
                //	// Refresh current record and field from data Server
                //	Send(oGBrowse, #__RefreshField, uDescription)
            endif

        case NOTIFYCLOSE
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
            if !self:CheckStatus()
                oTB:=TextBox{self, ResourceString{__WCSWarning}:Value, ResourceString{__WCSChgDiscard}:Value}
                oTB:Type:=BUTTONOKAY+BOXICONHAND
                oTB:Show()
            endif
            self:__Unlink()


        case NOTIFYRECORDCHANGE
        case NOTIFYGOBOTTOM
        case NOTIFYGOTOP
        case NOTIFYDELETE
        case NOTIFYAPPEND
            // record position has changed
            lThisRecDeleted:=IVarGet(oAttachedServer,#Deleted)
            // Disable or enable controls depending on deletion state
            // this logic only applies if deleted records are included in view
            // Use SET DELETE to exclude / include deleted records in view.
            if lThisRecDeleted
                // Do we need to disable controls
                self:__StatusMessage(ResourceString{__WCSDeletedRecord}:Value, MESSAGEPERMANENT)
            else
                self:__StatusMessage("", MESSAGEPERMANENT)
            endif
            lDeleted := lThisRecDeleted

            oHLStatus := null_object
            self:__Scatter()

            if (kNotification == NOTIFYAPPEND)
                //Set HLStatus for all controls
                foreach oC as Control in aControls
                    oC:PerformValidations()
                next
            endif
        end switch

        return self


    /// <include file="Gui.xml" path="doc/DataWindow.OK/*" />
    method OK() as logic strict

        if self:Commit()
            self:EndWindow()
            return true
        endif
        return false


    /// <include file="Gui.xml" path="doc/DataWindow.OLEInPlaceActivate/*" />
    method OLEInPlaceActivate()
        //RvdH 030825 Method moved from Ole Classes
        local oTB as ToolBar

        if oParent is ShellWindow var oShell
            oTB := oShell:ToolBar
        else
            oTB := self:ToolBar
        endif

        if (oTB != null_object) .and. oTB:IsVisible()
            oTB:Hide()
            lPendingToolBarShow := true
        endif

        return super:OLEInPlaceActivate()


    /// <include file="Gui.xml" path="doc/DataWindow.OLEInPlaceDeactivate/*" />
    method OLEInPlaceDeactivate()
        //RvdH 030825 Method moved from Ole Classes
        local oTB as ToolBar

        if oParent is ShellWindow var oShell
            oTB := oShell:ToolBar
        else
            oTB := self:ToolBar
            //__DataForm:ResizeParent()
        endif

        if (oTB != null_object) .and. !oTB:IsVisible() .and. lPendingToolBarShow
            oTB:Show()
        endif

        lPendingToolBarShow := false

        return super:OLEInPlaceDeactivate()


    /// <include file="Gui.xml" path="doc/DataWindow.Origin/*" />
    property Origin as Point
        get
            if self:lSubForm
                return self:__Frame:Location
            endif
            return super:Origin
        end get
        set
            if self:lSubForm
                self:__Frame:Location := value
            else
                super:Origin:=value
            endif
        end set
    end property
    /// <include file="Gui.xml" path="doc/Window.OwnerAlignment/*" />
    assign OwnerAlignment(iNewVal as usual)
        local lDone as logic
        if self:lSubForm
            lDone := Control.OwnerAlignmentHandledByWinForms(__Frame, iNewVal)
        endif
        if ! lDone
            ((IControlParent) oParent):__AddAlign(self, iNewVal)
        endif
        return

    /// <include file="Gui.xml" path="doc/DataWindow.OwnerServer/*" />
    property OwnerServer as object
        get
            if self:Owner is DataWindow var oDW
                return oDW:Server
            endif
            return nil
        end get
    end property


    /// <include file="Gui.xml" path="doc/DataWindow.PaintBoundingBox/*" />
    method PaintBoundingBox(oBB as BoundingBox,kPM as long) as void
        //Todo	 PaintBoundingBox
        if oSurface != null_object
            //oSurface:PaintBackground(oBB,kPM)
            nop
        endif
        return


    /// <include file="Gui.xml" path="doc/DataWindow.Paste/*" />
    method Paste()   as void strict
        if sCurrentView == #FormView
            if oDCCurrentControl is Edit var oEdit
                oEdit:Paste(ClipBoard{}:RetrieveString())
            elseif oDCCurrentControl is EditWindow var oEditWin
                oEditWin:Paste(ClipBoard{}:RetrieveString())
            elseif oDCCurrentControl is ControlWindow var oCW
                if oCW:Control != null_object .and. IsMethod(oCW:Control, #Paste)
                    Send(oCW:Control,#Paste, ClipBoard{}:RetrieveString())
                endif
            endif
        elseif sCurrentView == #BrowseView
            if (oGBrowse != null_object) .and. IsMethod(oGBrowse, #Paste)
                Send(oGBrowse, #Paste,ClipBoard{}:RetrieveString())
            endif
        endif

        return


    /// <include file="Gui.xml" path="doc/DataWindow.PasteSpecial/*" />
    method PasteSpecial()
        //RvdH 030825 Method moved from Ole Classes
        return self:__GetOLEObject(#CreateFromPasteDialog)


    /// <include file="Gui.xml" path="doc/DataWindow.Pen/*" />
    property Pen as Pen
        get
            return super:Pen
        end get
        set

            super:Pen := value
            if oSurface != null_object
                nop
            endif
            return
        end set
    end property


    /// <include file="Gui.xml" path="doc/DataWindow.Pointer/*" />
    property Pointer  as Pointer
        get
            if (sCurrentView == #FormView) .or. !IsAccess(oGBrowse, #pointer)
                if (oSurface != null_object)
                    return oSurface:Cursor
                endif
            else
                return IVarGet(oGBrowse, #pointer)
            endif

            return super:Pointer
        end get
        set
            if (oSurface != null_object)
                oSurface:Cursor:=value
            endif
            if IsAssign(oGBrowse, #databrowser)
                IVarPut(oGBrowse, #pointer, value)
            endif
            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.PreValidate/*" />
    method PreValidate()

        //self:EnableConditionalControls()
        self:__CheckConditionalControls()
        return self


    /// <include file="Gui.xml" path="doc/DataWindow.PreventAutoLayout/*" />
    property PreventAutoLayout as logic get lPreventAutoLayout set lPreventAutoLayout := value


    /// <include file="Gui.xml" path="doc/DataWindow.QueryClose/*" />
    method QueryClose(oQCE as Event) as logic
        // If there are outstanding changes which have not
        // been written to the dataServer - ask the user.
        local oTB as TextBox

        if oAttachedServer==null_object
            return true
        endif

        self:SetFocus()
        if !self:CheckStatus()
            oTB := TextBox{ self,;
                ResourceString{__WCSWarning}:Value,;
                VO_Sprintf(__WCSDataWindow,self:Caption)+CHR(10)+ResourceString{__WCSChgDiscard}:Value}
            oTB:Type := ButtonOkayCancel + BoxICONHand
            if (oTB:Show() != BOXREPLYOkay)
                return false
            endif
        endif

        return true

    /// <include file="Gui.xml" path="doc/DataWindow.RadioGroups/*" />
    property RadioGroups as array get self:aRadioGroups


    /// <include file="Gui.xml" path="doc/DataWindow.RegisterConditionalControls/*" />
    method RegisterConditionalControls(oCC)
        //SE-060526
        local dwI, dwCount as dword

        dwCount := ALen(aConditionalControls)
        for dwI := 1 upto dwCount
            if aConditionalControls[dwI] == oCC
                return self
            endif
        next
        AAdd(aConditionalControls,oCC)

        return self


    /// <include file="Gui.xml" path="doc/DataWindow.RePaint/*" />
    method RePaint() as void strict
        if oSurface != null_object
            oSurface:Invalidate()
        endif
        return


    /// <include file="Gui.xml" path="doc/DataWindow.RepaintBoundingBox/*" />
    method RepaintBoundingBox(oBB as BoundingBox) as void strict
        if (oSurface != null_object)
            oSurface:Invalidate((System.Drawing.Rectangle) oBB)
        endif
        return

    /// <include file="Gui.xml" path="doc/DataWindow.Seek/*" />
    method Seek(uValue, lSoftSeek, lLast)
        local lRetCode := false as logic
        if oAttachedServer!=null_object
            oHLStatus := null_object // assume success
            if self:__CheckRecordStatus()
                if !(lRetCode := Send(oAttachedServer,#Seek,uValue, lSoftSeek, lLast))
                    oHLStatus := oAttachedServer:Status
                    //oHLStatus := HyperLabel{#Seek, "Seek Failed, key not found"}
                    self:__UpdateStatus()
                endif
            endif
        endif

        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.Server/*" />
    property Server as DataServer get oAttachedServer

    /// <include file="Gui.xml" path="doc/DataWindow.SetAlignStartSize/*" />
    method SetAlignStartSize(oSize as Dimension)  as void
        super:SetAlignStartSize(oSize)
        return

    /// <include file="Gui.xml" path="doc/DataWindow.SetDialog/*" />
    method SetDialog(lResizable, lMaximizeBox, lMinimizeBox)
        //can be used in PreInit() method or befor super:init()
        if oWnd == null_object

            dwDialogStyle := _or(dwDialogStyle, WS_DLGFRAME)

            if IsLogic(lResizable) .and. lResizable
                dwDialogStyle := _or(dwDialogStyle, WS_THICKFRAME)
            endif
            if IsLogic(lMaximizeBox) .and. lMaximizeBox
                dwDialogStyle := _or(dwDialogStyle, WS_MAXIMIZEBOX, WS_SYSMENU)
            endif
            if IsLogic(lMinimizeBox) .and. lMinimizeBox
                dwDialogStyle := _or(dwDialogStyle, WS_MINIMIZEBOX, WS_SYSMENU)
            endif

        endif

        return self


    /// <include file="Gui.xml" path="doc/DataWindow.SetRelation/*" />
    method SetRelation( oDWChild as DataWindow, uRelation as usual, cRelation  as string)
        local lRetCode := false as logic

        oHLStatus:=null_object // assume success
        if oAttachedServer!=null_object .and. self:lValidFlag
            if self:__CheckRecordStatus()
                if !(lRetCode:=Send(oAttachedServer,#SetRelation,oDWChild:Server, uRelation, cRelation ))
                    oHLStatus:=oAttachedServer:Status
                    self:__UpdateStatus()
                endif
            endif
        endif
        return lRetCode


    /// <include file="Gui.xml" path="doc/DataWindow.SetSelectiveRelation/*" />
    method SetSelectiveRelation( oDWChild as DataWindow, uRelation as usual, cRelation as string)
        local lRetCode := false as logic

        oHLStatus := null_object // assume success
        if oAttachedServer != null_object .and. self:lValidFlag
            if self:__CheckRecordStatus()
                if !(lRetCode:=Send(oAttachedServer,#SetSelectiveRelation,oDWChild:Server, uRelation, cRelation ))
                    oHLStatus:=oAttachedServer:Status
                    self:__UpdateStatus()
                endif
            endif
        endif

        return lRetCode

    /// <include file="Gui.xml" path="doc/DataWindow.Show/*" />
    method Show(nShowState)  as void CLIPPER

        if lDeferUse .and. (oDeferUseServer != null_object)
            lDeferUse := false
            self:Use(oDeferUseServer)
            oDeferUseServer := null_object
        endif

        if (self:ToolBar != null_object)
            self:ToolBar:Show()
        endif

        if !lSubForm
            if self:lValidFlag
                super:Show(nShowState)
            else
                self:Destroy()
            endif
        else
            self:__DataForm:ShowSubForm()
        endif

        return

    /// <include file="Gui.xml" path="doc/DataWindow.Size/*" />
    property Size as Dimension
        get
            if self:lSubForm
                return self:__Frame:Size
            endif
            return super:Size
        end get
        set
            if self:lSubForm
                self:__Frame:Size := value
                self:__Frame:DefinedSize := self:__Frame:Size
                self:__DataForm:AdjustSizes()
            else
                super:Size := value
            endif
        end set
    end property
    /// <include file="Gui.xml" path="doc/DataWindow.Skip/*" />
    method Skip(uRelativePosition) as logic
        local lRetCode := false as logic

        oHLStatus := null_object // assume success
        if oAttachedServer != null_object .and. self:__CheckRecordStatus()
            if !(lRetCode := oAttachedServer:Skip(uRelativePosition))
                oHLStatus := oAttachedServer:Status
                self:__UpdateStatus()
            endif
        endif

        return lRetCode

    /// <include file="Gui.xml" path="doc/DataWindow.SkipNext/*" />
    method SkipNext() as logic strict
        local lRetCode := false as logic

        lRetCode := self:Skip(1)

        if lRetCode = true .and. oAttachedServer:EoF
            self:GoBottom()
        endif

        return lRetCode

    /// <include file="Gui.xml" path="doc/DataWindow.SkipPrevious/*" />
    method SkipPrevious() as logic strict

        return self:Skip(-1)


    /// <include file="Gui.xml" path="doc/DataWindow.Status/*" />
    property Status as HyperLabel get self:oHLStatus set oHLStatus := value


    /// <include file="Gui.xml" path="doc/DataWindow.StatusBar/*" />
    property StatusBar as StatusBar
        get
            if dwDialogStyle > 0
                // Support of a StatusBar a DataDialog - Window
                return oStatusBar
            endif
            return super:StatusBar
        end get
        set
            //SE-070906
            if dwDialogStyle > 0
                // Support of a StatusBar a DataDialog - Window
                oStatusBar := value
            endif
            super:StatusBar := value
            if value != null_object
                self:__DataForm:StatusBar := self:StatusBar:__StatusStrip
            else
                self:__DataForm:StatusBar := null_object
            endif

            return
        end set
    end property

    /// <include file="Gui.xml" path="doc/DataWindow.StatusOK/*" />
    method StatusOK() as logic strict
        local dwInvalidControl, iLen as dword


        oDCInvalidControl := null_object
        oDCInvalidColumn := null_object
        oHLStatus := null_object

        if (oAttachedServer == null_object)
            oHLStatus := HyperLabel{#NoAttachedServer, #NoAttachedServer}
        else
            self:__UpdateCurrent()
            if !oAttachedServer:EoF //Don't validate the EOF record
                if (sCurrentView == #FormView)
                    iLen := ALen(aControls)
                    for dwInvalidControl := 1 to iLen
                        var oControl := (Control) aControls[dwInvalidControl]
                        if (oControl:Status != null_object)
                            exit
                        endif
                    next
                    //dwInvalidControl := AScan(aControls, {|oControl| oControl:Status != NULL_OBJECT})
                    if (dwInvalidControl != (iLen+1))
                        oDCInvalidControl := aControls[dwInvalidControl]
                        if (oHLStatus := oDCInvalidControl:Status) == null_object
                            oHLStatus := HyperLabel{#InvalidControl, #RecInvalid}
                        endif
                    endif
                elseif (sCurrentView == #BrowseView)
                    if oGBrowse != null_object
                        if (oDCInvalidColumn := Send(oGBrowse, #__StatusOK)) != null_object
                            if (oHLStatus := oDCInvalidColumn:Status) == null_object
                                oHLStatus := HyperLabel{#InvalidColumn, #RecInvalid}
                            endif
                        endif
                    endif
                endif
            endif
        endif

        return oHLStatus == null_object

    /// <include file="Gui.xml" path="doc/DataWindow.SubForms/*" />
    property SubForms as array get self:aSubForms

    /// <include file="Gui.xml" path="doc/DataWindow.Surface/*" />
    property Surface as VOPanel get self:oSurface

    /// <include file="Gui.xml" path="doc/DataWindow.TextPrint/*" />
    method TextPrint(cText as string, oPoint as Point) as void
        // Todo	 TextPrint
        if oSurface != null_object
            //oSurface:TextPrint(cText, oPoint)
            nop
        endif
        return

    /// <include file="Gui.xml" path="doc/DataWindow.ToolBar/*" />
    property ToolBar as ToolBar
        get
            return super:ToolBar
        end get
        set

            super:ToolBar := value
            if value != null_object
                self:__DataForm:ToolBar := value:__ToolBar
                //ELSE
                //	SELF:__DataForm:ToolBar := NULL_OBJECT
            endif
            // No need to resize. __DataForm handles this
            return
        end set
    end property
    /// <include file="Gui.xml" path="doc/DataWindow.Undo/*" />
    method Undo() as void
        if (sCurrentView == #FormView)
            if oDCCurrentControl is Edit var oEdit
                oEdit:Undo()
            endif
        elseif (sCurrentView == #BrowseView)
            if (oGBrowse != null_object) .and. IsMethod(oGBrowse, #Undo)
                Send(oGBrowse, #Undo)
            endif
        endif
        return

    /// <include file="Gui.xml" path="doc/DataWindow.UndoAll/*" />
    method UndoAll() as void
        if oAttachedServer!=null_object
            Send(oAttachedServer,#Refresh)
        endif
        return

    /// <include file="Gui.xml" path="doc/DataWindow.UpdateActiveObject/*" />
    method UpdateActiveObject() as void
        self:__UpdateActiveObject()


    /// <include file="Gui.xml" path="doc/DataWindow.Use/*" />
    method Use(oDataServer as DataServer) as logic
        local lRetCode := false as logic

        if lDeferUse
            oDeferUseServer := oDataServer
            return true
        endif

        if (oAttachedServer != oDataServer) .and. (oAttachedServer != null_object)
            self:__Unlink()
            if __DataForm:AutoLayout
                if (oSurface != null_object)
                    oSurface:Controls:Clear()
                    oSurface:Size := Dimension{0, 0}
                endif
                aControls := {}
                aRadioGroups := {}
                aConditionalControls := {}

                if (oGBrowse != null_object)
                    oGBrowse:Destroy()
                    oGBrowse := null_object
                endif
            endif
        endif

        if (oDataServer != null_object)
            if self:__VerifyDataServer(oDataServer)
                oDataServer:RegisterClient(self)

                lRetCode := self:__RegisterFieldLinks(oDataServer)
                if (sCurrentView == #BrowseView)
                    if (oGBrowse != null_object)
                        Send(oGBrowse, #Use, oDataServer)
                        self:__Scatter() // new insert, since ViewAs already does a Scatter()
                    else
                        sCurrentView := #ViewSwitch
                        self:ViewAs(#BrowseView)
                    endif
                elseif (sCurrentView == #FormView)
                    sCurrentView := #ViewSwitch
                    self:ViewAs(#FormView)
                endif
                //self:__Scatter() // removed, see above
            else
                if (oHLStatus != null_object)
                    ErrorBox{, oHLStatus}:Show()
                else
                    WCError{#oDataServer,#DataWindow,__WCSTypeError,oDataServer,1}:Throw()
                endif
                self:lValidFlag := false
            endif
        endif

        return lRetCode

    method ValidateRecord() clipper
        return true

    /// <include file="Gui.xml" path="doc/DataWindow.VerticalScroll/*" />
    method VerticalScroll(oScrollEvent as ScrollEvent)  as usual
        self:__HandleScrolling(oScrollEvent)
        return self:Default(oScrollEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.VerticalSlide/*" />
    method VerticalSlide(oSlideEvent as SliderEvent)  as usual
        self:__HandleScrolling(oSlideEvent)
        return  self:Default(oSlideEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.VerticalSpin/*" />
    method VerticalSpin(oSpinEvent as SpinnerEvent)  as usual
        self:__HandleScrolling(oSpinEvent)
        return self:Default(oSpinEvent)

    /// <include file="Gui.xml" path="doc/DataWindow.ViewAs/*" />
    method ViewAs(symViewType as symbol)
        local oTextBox as TextBox
#ifdef USE_OLEOBJECT
        local oOleObj as OleObject
#endif
        local oControl as Control
        //RvdH 041123 Added call to __GetMyOleObjects to retrieve the objects

        if (sCurrentView == symViewType)
            // No change in view -> do nothing
            return self
        endif
        self:DeactivateAllOLEObjects()

        // Save data in current view
        if lLinked
            if !self:__CheckRecordStatus() // check validation status
                // continuing now may lose changes
                oTextBox := TextBox{self, ResourceString{__WCSWarning}:Value, ResourceString{__WCSChangingView}:Value}
                oTextBox:Type := BUTTONOKAYCANCEL + BOXICONHAND
                if oTextBox:Show() != BOXREPLYOKAY
                    return self
                else
                    //Put original data back
                    if (sCurrentView == #BrowseView)
                        self:__Scatter()
                        Send(oGBrowse, #__NotifyChanges, GBNFY_FIELDCHANGE)
                    endif
                endif
            endif
        endif

        if (symViewType == #BrowseView)
            // Show as browser
            // Hide form frame
            // Check if autocreating browser
            // flush changes to form so they are reflected in browser
            try
                sCurrentView := #ViewSwitch
#ifdef USE_OLEOBJECT
                for i:=1 to iLen
                    oOleObj := aObjects[i]
                    if oOleObj:Server != null_object

                        oOleObj:DetachFromServer()
                    endif
                next
#endif
                self:__AutoCreateBrowser()
                __DataForm:DataBrowser := (System.Windows.Forms.Control)oGBrowse:__Control
            catch
                nop
            end try
            sCurrentView := #BrowseView
            if oGBrowse != null_object
                oGBrowse:SuspendUpdate()
                __DataForm:ViewAs(true) // view as browse
                oGBrowse:RestoreUpdate()
                Send(oGBrowse, #__NOTIFYChanges, GBNFY_VIEWASBROWSER)
            endif
        else
            // Show as form
            if (oGBrowse != null_object)
                Send(oGBrowse, #__NOTIFYCHANGES, GBNFY_VIEWASFORM)
            endif
            if ALen(self:GetAllChildren()) == 0
                self:__AutoLayout()
            endif
            __DataForm:ViewAs(false) // view as form
            if ALen(aControls) > 0
                local nControl as long
                nControl := 1
                do while nControl <= aLen(aControls)
                    oControl := aControls[nControl]
                    if ! IsInstanceOf(oControl,#FixedText) .and. ! IsInstanceOf(oControl,#GroupBox) .and. !IsInstanceOf(oControl,#Window)
                        oControl:SetFocus()
                        if IsInstanceOf(oControl, #SingleLineEdit) .and. oControl:IsEnabled()
                            local oSle as SingleLineEdit
                            oSle := (SingleLineEdit) oControl
                            oSle:Selection := Selection{0,0}
                        endif
                        exit
                    endif
                    nControl += 1
                enddo
            endif

            sCurrentView := #FormView
        endif
        self:__Scatter()
        return self


    /// <include file="Gui.xml" path="doc/DataWindow.ViewForm/*" />
    method ViewForm() clipper
        self:ViewAs(#FormView)
        return self

    /// <include file="Gui.xml" path="doc/DataWindow.ViewTable/*" />
    method ViewTable() clipper
        self:ViewAs(#BrowseView)
        return self

    static internal glUseColonInAutoLayoutCaptions := true as logic
end class

/// <include file="Gui.xml" path="doc/UseColonInAutoLayoutCaptions/*" />
function UseColonInAutoLayoutCaptions(lUse as logic) as void
    DataWindow.glUseColonInAutoLayoutCaptions := lUse
    return

/// <exclude />
function __GetDFCaption (oDF as DataField, arUsedKeys as array)  as string
    local cText 	as string
    local cHotKey 	as string
    local dwI, dwCount as dword
    local i 			as dword

    //	IF (IsInstanceOfUsual(oDF, #DataField) .AND. (!IsNil(oDF:HyperLabel)))
    if !IsNil(oDF:HyperLabel)
        cText := oDF:HyperLabel:Caption
        // 	ELSEIF (IsInstanceOfUsual(oDF, #DataServer) .AND. (!IsNil(fldIndex)))
        // 		cText := LTrim(AsString(oDF:FieldName(fldIndex)))
    else
        cText := ResourceString{__WCSUnknown}:Value
    endif

    cText := Proper(cText) //RTrim(Left(cText, 1) + Lower(SubStr(cText, 2)))

    //	IF !IsNil(arUsedKeys)
    if (dwCount := ALen(arUsedKeys)) > 0
        for i := 1 to SLen(cText)
            cHotKey := Lower(SubStr(cText, i ,1))
            //SE-060526
            for dwI := 1 upto dwCount
                if arUsedKeys[dwI] = cHotKey
                    exit
                endif
            next  // dwI
            if dwI > dwCount
                AADD(arUsedKeys, cHotKey)
                cText := Left(cText, i-1) + "&" + SubStr(cText, i)
                exit
            endif
        next
    endif

    if DataWindow.glUseColonInAutoLayoutCaptions
        if !IsBiDi()
            if !(Right(cText, 1) == ":")
                cText := cText +":"
            endif
        else
            if !(Left(cText, 1) == ":")
                cText := ":" + cText
            endif
        endif
    endif

    return cText

/// <exclude />
function __GetFSDefaultLength(uFS as usual) as int
    local liRetVal, liExtra as int
    local uType as usual
    local cFunction as string


    if uFS is FieldSpec var oFs
        liRetVal := (int) oFS:Length
        uType := oFS:UsualType

        do case
        case (uType == logic)
            liRetVal := Max(liRetVal, 3)
        case (uType == date)
            if (SetCentury() == true)
                liRetVal := 10
            else
                liRetVal := Max(liRetVal, 8)
            endif
        case (uType == string) .and. oFS:ValType == "M"
            liRetVal := Max(liRetVal, 40)
            liRetVal := Min(liRetVal, 120)
        endcase
        //RvdH 060608 optimized
        //IF (NULL_STRING != oFS:Picture) .AND. !Empty(oFS:Picture)
        if SLen(oFS:Picture)> 0
            if SubStr(oFS:Picture, 1, 1) != "@"
                //there is no associated function
                liRetVal := longint(_cast,SLen(oFS:Picture))
            else
                //store the function character
                cFunction := SubStr(oFS:Picture, 2, 1)
                if cFunction == "C" .or. cFunction == "X"
                    //"@C" and "@X" functions require a " CR" or a " DB" respectively
                    //at the end of the string, so tag on three extra bytes
                    liExtra := 3
                endif
                if At(" ", oFS:Picture) != 0
                    //the actual picture will now start after the space between the function
                    //and the template, so calculate the length from this point
                    liRetVal := longint(_cast,SLen(SubStr(oFS:Picture, At(" ", oFS:Picture) + 1)))
                endif
                liRetVal += liExtra
            endif
        endif
        if (oFS:Length > liRetVal)
            return (int) oFS:Length
        endif
    else
        liRetVal := 10

    endif
    return liRetVal






