//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/ListBox/*" />
class ListBox inherit BaseListBox
    protect wSelectNum 		as long
    /// <exclude  />
    property ControlType as ControlType get ControlType.ListBox

    /// <exclude  />
    method OnHandleCreated(o as object, e as EventArgs) as void
        local nItem as long
        self:cSavedText := STRING.Empty
        nItem := self:__FindRetValue(self:uValue)
        super:OnHandleCreated(o,e)
        self:__CurrentItemNo := nItem


    protected method __BeginUpdate() as void
        self:__ListBox:BeginUpdate()
    protected method __EndUpdate() as void
        self:__ListBox:EndUpdate()
    /// <exclude />
    method __FindDisplayValue(cValue as string) as long strict
        // Returns 1-based index in collection
        local dwI as long
        foreach oItem as ListBoxItemValue in self:__Items
            dwI++
            if Alltrim(oItem:DisplayValue) == cValue
                return dwI
            endif
        next

        return 0


    /// <exclude />
    method __FindRetValue(uValue as usual) as long strict
        local dwI as long
        // Returns 1-based index in collection
        foreach oItem as ListBoxItemValue in self:__Items
            dwI++
            if valtype(oItem:Value) == valtype(uValue)
                if oItem:Value == uValue
                    return dwI
                endif
            else
                // What is this ?
                oItem:Value := oItem:Value
            endif
        next
        return 0

    /// <exclude  />
    method __SetText(cNewText as string) as string
        if cNewText != self:CurrentText
            return super:__SetText(cNewText)
        endif
        return cNewText

    /// <exclude />
    method __Update() as void strict
        local cOldValue as string
        local oItem as ListBoxItemValue
        local nIndex as long
        if self:Modified
            cOldValue := AsString(uValue)
            if (nIndex := self:__List:SelectedIndex) >= 0
                oItem := self:__Items[nIndex]
                uValue := oItem:Value
            else
                uValue := nil
            endif
            self:ValueChanged	:= !(cOldValue == AsString(uValue))
            self:Modified		:= false
        endif
        return

    /// <exclude />
    assign __Value(uNewVal as usual)  strict
        local cSelValue as string
        local nIndex as long
        local oValue as ListBoxItemValue
        //PP-030924 allow value of NIL to reset control
        if IsNil(uNewVal)
            self:__CurrentItemNo := 0
            super:TextValue := ""
            uValue := uNewVal
        else
            //PP-030924 clear and reload box, clears selection
            //SELF:__reset()
            cSelValue := AllTrim(AsString(uNewVal))
            nIndex := self:__FindRetValue(uNewVal)
            if nIndex > 0
                // select the corresponding display string
                self:CurrentItemNo := nIndex
                oValue := self:__Items[nIndex-1]
                self:__SetText(oValue:DisplayValue)
                uValue := oValue:Value
            else
                nIndex := self:FindItem(cSelValue, true)
                if nIndex == 0
                    nIndex := self:FindItem(cSelValue, false)
                    if nIndex > 0
                        oValue := self:__Items[nIndex-1]
                        if ! cSelValue == oValue:DisplayValue
                            nIndex := 0
                        endif
                    else
                        uValue := uNewVal
                    endif
                endif
                if nIndex > 0
                    oValue    := self:__Items[nIndex-1]
                    cSelValue := oValue:DisplayValue
                    uValue    := oValue:Value
                endif
                self:__CurrentItemNo := nIndex
                self:__SetText(cSelValue)
            endif
        endif

        return


    /// <include file="Gui.xml" path="doc/ListBox.AddItem/*" />
    method AddItem(cItem , nItemNumber , uRetValue ) as long
        // nItemNumber = 1-based index in collection
        // Returns 1-based index in collection

        local nIndex as long
        if IsNil(uRetValue)
            uRetValue := cItem
        endif
        nIndex := super:AddItem(cItem, nItemNumber, uRetValue)
        return nIndex

    /// <include file="Gui.xml" path="doc/ListBox.Caption/*" />
    property Caption as string get cCaption set cCaption := value

    /// <include file="Gui.xml" path="doc/ListBox.ChangeSelected/*" />
    method ChangeSelected(oRange as Range, lEnabled := true as logic)  as logic
        local nItem as long
        if !self:IsComboBox
            if self:ValidateControl() .and. ! self:IsComboBox
                for nItem := oRange:Min to oRange:Max
                    if lEnabled
                        self:__ListBox:SelectedIndices:Add(nItem-1)
                    else
                        if self:__ListBox:SelectedIndices:Contains(nItem-1)
                            self:__ListBox:SelectedIndices:Remove(nItem-1)
                        endif
                    endif
                next
                return true
            endif
        endif
        return false

    /// <include file="Gui.xml" path="doc/ListBox.Clear/*" />
    method Clear() as void
        if self:FieldSpec == null_object
            self:uValue := nil
        else
            self:uValue := EmptyUsual(self:FieldSpec:UsualType)
        endif

        super:Clear()
        return


    /// <include file="Gui.xml" path="doc/ListBox.ClearSelection/*" />
    method ClearSelection()
        if self:ValidateControl()
            if self:IsListBox
                self:__ListBox:SelectedIndices:Clear()
            else
                self:__ComboBox:SelectedIndex := -1
            endif
            self:__CurrentItemNo := 0
            return true
        endif

        return false

    /// <include file="Gui.xml" path="doc/ListBox.Create/*" />
    method Create() as System.Windows.Forms.Control strict
        if oCtrl ==  null_object  .and. !IsInstanceOf(self, #ComboBox)
            self:SetStyle(_or(LBS_Notify, LBS_NoIntegralHeight))
        endif
        return super:Create()

    /// <include file="Gui.xml" path="doc/ListBox.CurrentItem/*" />
    property CurrentItem  as string
        get
            if self:ValidateControl()
                return self:GetItem(0)
            endif
            return sSavedCurrentItem
        end get
        set
            self:CurrentItemNo := self:FindItem(value)
            return
        end set
    end property
    /// <include file="Gui.xml" path="doc/ListBox.CurrentItemNo/*" />
    property CurrentItemNo as long
        get
            if self:MultiSelection
                return self:FirstSelected()
            endif
            return super:CurrentItemNo
        end get
        set

            // nItemNo = 1-based index in collection
            local cSelValue as string
            local dwIndex as long
            local uOldValue as usual
            local oItem as ListBoxItemValue
            self:__CurrentItemNo := value

            if value > 0
                cSelValue := self:CurrentItem
            endif
            uOldValue := AsString(uValue)
            if ! IsNil(cSelValue) .and. (dwIndex := self:__FindDisplayValue(AllTrim(cSelValue))) > 0
                oItem := self:__Items[dwIndex-1]
                uValue := oItem:Value
            else
                uValue := cSelValue
            endif
            self:ValueChanged := !(AsString(uValue) == uOldValue)

        end set
    end property


    /// <include file="Gui.xml" path="doc/ListBox.CurrentText/*" />
    assign CurrentText(cNewText as string)
        self:__SetText(cNewText)
        return

    /// <include file="Gui.xml" path="doc/ListBox.DeleteItem/*" />
    method DeleteItem(nItem := 0 as long) as logic
        // nItem = 1-based index in collection
        local lReturnValue as logic
        lReturnValue := super:DeleteItem(nItem)
        return lReturnValue

    /// <include file="Gui.xml" path="doc/ListBox.DeselectItem/*" />
    method DeselectItem(nItem as long) as logic
        // nItem = 1-based index in collection
        if self:ValidateControl() .and. self:IsListBox
            if self:__ListBox:SelectedIndices:Contains(nItem-1)
                self:__ListBox:SelectedIndices:Remove(nItem-1)
                return true
            endif
        endif

        return false

    /// <include file="Gui.xml" path="doc/ListBox.EnableItemDrag/*" />
    method EnableItemDrag() as void
        //Todo EnableItemDrag
        //IF IsInstanceOf(oFormSurface, #DialogWindow)
        //	Send(oFormSurface, #__SubClassForDragList)
        //ENDIF
        //SELF:setstyle(LBS_SORT, FALSE)
        //RETURN MakeDragList(SELF:Handle())

    /// <include file="Gui.xml" path="doc/ListBox.FillUsing/*" />
    method FillUsingBySortedList(oList as System.Collections.Generic.SortedList<string,usual>) as void strict
        if self:IsListBox
            self:__BeginUpdate()
            self:Clear()
            if oList:Count > 0 // aus irgendeinem Grund geht er bei leerer Liste trotzdem in die SChleife
                foreach kvp as System.Collections.Generic.KeyValuePair<string,usual> in oList
                    super:AddItem(kvp:Key, ,kvp:Value)
                next
            endif
            self:__EndUpdate()
        endif

    /// <include file="Gui.xml" path="doc/ListBox.FillUsing/*" />
    method FillUsing(aContents, symField1, symField2)
        local wElemLen as dword
        local uDisplayValue as usual
        local cDisplayValue as string
        local uRetValue as usual

        if aContents is DataServer var oDS
            aContents := oDs:GetLookUpTable( Math.Min(0x7FFF, (long) IVarGet(aContents, #RecCount)), symField1, symField2)
        elseif IsArray(aContents)
            if !IsNil(symField1) .or. !IsNil(symField2)
                WCError{#FillUsing,#ListBox,__WCSTypeError,symField1,2}:Throw()
            endif
        else
            WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:Throw()
        endif

        self:Clear()
        self:IsBusy := true
        self:__BeginUpdate()
        if ALen(aContents) > 0
            foreach var uElement in aContents
                if IsArray(uElement)
                    wElemLen := ALen(uElement)
                    if wElemLen == 2
                        uDisplayValue   := uElement[1]
                        uRetValue       := uElement[2]
                    elseif wElemLen == 1
                        uDisplayValue := uElement[1]
                        uRetValue := uElement[1]
                    else
                        WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:Throw()
                    endif
                else
                    uDisplayValue := uElement
                    uRetValue := uElement
                endif
                if !IsString(uDisplayValue)
                    cDisplayValue := AsString(uDisplayValue)
                else
                    cDisplayValue := (string) uDisplayValue
                endif
                if IsInstanceOf(self, #COMBOBOXEX)
                    self:AddItem( cDisplayValue)
                else
                    super:AddItem(cDisplayValue, ,uRetValue)
                endif
            next
        endif
        self:__EndUpdate()
        self:IsBusy := false
        return self

    /// <include file="Gui.xml" path="doc/ListBox.FirstSelected/*" />
    method FirstSelected ( ) as long
        // nItem = 1-based index in collection
        local iResult as long
        try
            if self:__Items:Count > 0
                if self:IsComboBox
                    iResult :=  self:__ComboBox:SelectedIndex+1
                else
                    if __ListBox:SelectedIndex >= 0
                        wSelectNum := 1
                        if wSelectNum <= __ListBox:SelectedIndices:Count
                            iResult := (int) __ListBox:SelectedIndices[wSelectNum-1]+1
                        endif
                    endif
                endif
            endif
        catch  as Exception
            iResult := 0
        end try
        return iResult


    /// <include file="Gui.xml" path="doc/ListBox.ctor/*" />
    constructor(oOwner, xID, oPoint, oDimension, kStyle)
        super(oOwner, xID, oPoint, oDimension, kStyle, true)
        return

    /// <include file="Gui.xml" path="doc/ListBox.IsSelected/*" />
    method IsSelected(iIdx as long) as logic
        // nItem = 1-based index in collection
        local lResult as logic
        if self:ValidateControl()
            if ! self:MultiSelection
                lResult := (iIdx == self:CurrentItemNo)
            elseif self:IsListBox
                lResult := self:__ListBox:SelectedIndices:Contains(iIdx-1)
            endif
        endif

        return lResult

    /// <include file="Gui.xml" path="doc/ListBox.ItemCount/*" />
    access ItemCount as long
        return (long) self:__Items:Count


    /// <include file="Gui.xml" path="doc/ListBox.MultiSelection/*" />
    access MultiSelection as logic
        local lMulti as logic
        if self:ValidateControl() .and. self:IsListBox
            lMulti := __ListBox:SelectionMode == System.Windows.Forms.SelectionMode.MultiSimple .or. ;
                __ListBox:SelectionMode == System.Windows.Forms.SelectionMode.MultiExtended
        endif
        return lMulti

    /// <include file="Gui.xml" path="doc/ListBox.NextSelected/*" />
    method NextSelected() as long
        // RETURNS 1-based index in collection
        local iResult as long
        if self:__Items:Count > 0

            if self:IsComboBox
                iResult := 0
            else
                wSelectNum 	:= wSelectNum + 1
                if self:ValidateControl() .and. wSelectNum <= self:__ListBox:SelectedIndices:Count
                    iResult 	:= (int) self:__ListBox:SelectedIndices[wSelectNum-1] +1
                endif
            endif
        endif
        return iResult

    /// <include file="Gui.xml" path="doc/ListBox.SelectedCount/*" />
    access SelectedCount as long
        local liNumSelected := 0 as longint

        if self:ValidateControl()  .and. self:IsListBox
            liNumSelected := __ListBox:SelectedItems:Count
        endif

        return liNumSelected

    /// <include file="Gui.xml" path="doc/ListBox.SelectedFile/*" />
    access SelectedFile as string
        //Todo SelectedFile
        //LOCAL pPath AS PSZ
        //LOCAL sRet AS STRING
        //pPath := MemAlloc(261)
        //MemSet(pPath, 0, 261)
    ////call(fpSelectedFile, oParent:Handle(), pPath,	260, wID)
    //__DlgDirSelectEx(((Window) oParent):Handle(), pPath,	260, wID)
    //sRet := Psz2String(pPath)
    //MemFree(pPath)
    //RETURN sRet
    return null_string

    /// <include file="Gui.xml" path="doc/ListBox.SelectItem/*" />
    method SelectItem(nItemId as long) as logic
        // nItemID =  1-based index in collection
        if self:ValidateControl()
            if self:ItemCount >= nItemId .and. nItemId > 0
                if self:IsComboBox
                    self:__ComboBox:SelectedIndex := nItemId-1
                else
                    self:__ListBox:SelectedIndex := nItemId-1
                endif
                return true
            endif
        endif
        return false


        // Estimates optimal Width of the control by Measuring the Displayvalues
    method GetOptimalWidth() as dword
        local nMaxlen:= 0 as dword
        foreach oVal as ListBoxItemValue in self:__Items
            nMaxlen := 100 // Max( System.Windows.Forms.TextRenderer.MeasureText(oVal:DisplayValue,oCtrl:Font,System.Drawing.Size{0,0},SupportFunctions.lsbTextflags):Width,nMaxlen)
        next
        nMaxlen += 20 // Offset for Vertical Scrollbar
        return nMaxlen

        // not supported anymore
    method SetTabs(aTabs as array) as void
        local dwTabs as dword
        local dwI    as dword
        //PP-030319 From S Ebert
        if (dwTabs := ALen(aTabs)) > 0
            var pTabs := int[]{(int) dwTabs}
            for dwI := 1 upto dwTabs
                pTabs[dwI] := aTabs[dwI]
            next  // dwI
            GuiWin32.SendMessage(hWnd, LB_SETTABSTOPS, dwTabs, @pTabs)
        endif
        return


    /// <include file="Gui.xml" path="doc/ListBox.TextValue/*" />
    access TextValue as string
        local nItem as long
        local oItem as ListBoxItemValue
        if self:ValidateControl()

            if self:IsComboBox
                nItem := self:__ComboBox:SelectedIndex
                if nItem < 0
                    return self:__ComboBox:Text
                endif
            else
                nItem := self:__ListBox:SelectedIndex
            endif
            if nItem != -1
                oItem := (ListBoxItemValue) self:__Items[nItem]
                return oItem:DisplayValue
            endif
        endif
        return ""

    /// <include file="Gui.xml" path="doc/ListBox.TextValue/*" />
    assign TextValue(cNewText as string)
        local cSelValue as string
        local dwIndex as long
        local oItem as ListBoxItemValue
        if self:ValidateControl()

            cSelValue := cNewText
            if (dwIndex := self:FindItem(cSelValue, true, 1)) > 0
                oItem := self:__Items[dwIndex-1]
                uValue := oItem:Value
                self:__List:SelectedIndex := dwIndex-1
            else
                if (dwIndex := self:FindItem(cSelValue, false, 1)) > 0
                    oItem := self:__Items[dwIndex-1]
                    uValue := oItem:Value
                    self:__List:SelectedIndex := dwIndex-1
                else
                    self:__List:SelectedIndex := -1
                    uValue := nil
                endif
            endif
        endif
        return

#region Obsolete Methods
    /// <exclude  />
    [Obsolete];
    method __AddItem(cItem as string, uRetValue as usual, dwPosition as long) as void strict
        return

    /// <include file="Gui.xml" path="doc/ListBox.ListFiles/*" />
    [Obsolete];
    method ListFiles(sStartDir, oFixedText, FileTypes)
        //Todo ListFiles
        return self
        //LOCAL pPath AS PSZ
        //LOCAL i,iRet AS INT
        //LOCAL dwFileTypes AS DWORD
        //LOCAL w AS DWORD

        //DEFAULT( REF sStartDir, "*.*")

        //pPath := StringAlloc(sStartDir)
        //pPath := MemRealloc(pPath, 261)

        //IF !IsNil(FileTypes)
        //	dwFileTypes := FileTypes
        //ELSE
        //	dwFileTypes := _OR(DDL_DIRECTORY, DDL_DRIVES)
        //ENDIF

        //IF !IsNil(oFixedText)
        //	w := oFixedText:ControlID
        //ENDIF

////iRet := call(fpListFiles, oParent:Handle(), pPath, wID, w, dwFileTypes)
        //iRet := DlgDirList(oParent:Handle(), pPath, INT(wID), INT(w), dwFileTypes)

        //MemFree(pPath)
        //iRet := SUPER:ItemCount
////RvdH 050602, Bug [12898] Now fill aRetValues and aDisplayValues array with the file names
        //aRetValues 		:= ArrayNew(iRet)
        //aDisplayValues := ArrayNew(iRet)
        //FOR i := 1 TO iRet
        //	aRetValues[i] 		:= SELF:GetItem(i)
        //	aDisplayValues[i] := aRetValues[i]
        //NEXT

        //RETURN (iRet != 0)
#endregion


end class

//_DLL FUNCTION __DlgDirSelectEx( hDlg AS PTR, lpString AS PSZ, nCOunt AS INT, nIDListBox AS INT) AS LOGIC PASCAL:USER32.DlgDirSelectExA


