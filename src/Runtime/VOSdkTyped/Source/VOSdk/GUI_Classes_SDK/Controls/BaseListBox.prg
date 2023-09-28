//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections
using System.Collections.Generic
using System.Diagnostics

/// <summary>
/// Abstract (Empty) Class for compatibility with VO
/// </summary>
class BaseListBox inherit TextControl
    protect liSavedCurrentItemNo as long
    protect sSavedCurrentItem as string
    property  IsBusy         as logic auto

    property __List as IBaseListBox get (IBaseListBox ) oCtrl
    /// <summary>
    /// Is the control a ComboBox
    /// </summary>
    property IsComboBox as logic get oCtrl is VOComboBox
    /// <summary>
    /// Is the control a ListBox
    /// </summary>
    property IsListBox  as logic get oCtrl is VOListBox

    /// <exclude />
    access __Items as IList
        if oCtrl != null
            return __List:Items
        else
            return System.Collections.Generic.List<object>{}
        endif

    /// <exclude />
    access __ComboBox as VOComboBox
        if oCtrl is VOComboBox var oCombo
            return oCombo
        endif
        return null_object

    /// <exclude />
    access __ListBox as VOListBox
        if oCtrl is VOListBox var oLB
            return oLB
        endif
        return null_object

    /// <include file="Gui.xml" path="doc/BaseListBox.AddItem/*" />
    method AddItem(uItem , uIndex, uValue   ) as long
        // nIndex = 1-based index in collection
        local nItem as long
        local cItem as string
        local oValue as object
        local nIndex as long
        if !IsString(uItem)
            WCError{#AddItem,#BaseListBox,__WCSTypeError,uItem,1}:Throw()
        endif
        cItem := uItem
        if IsLong(uIndex)
            nIndex := uIndex
        endif
        oValue := uValue
        if nIndex <= 0
            self:__Items:Add(ListBoxItemValue{cItem, oValue})
            nItem := ((ICollection) self:__Items):Count
        else
            self:__Items:Insert(nIndex-1, ListBoxItemValue{cItem, oValue})
            nItem := nIndex
        endif
        return nItem
    /// <include file="Gui.xml" path="doc/BaseListBox.Clear/*" />

    method Clear() as void
        if self:ValidateControl()
            self:__Items:Clear()
        endif
        return
    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentItem/*" />
    access CurrentItem as string
        return null_string
    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentItemNo/*" />
    access CurrentItemNo as long
        // Returns 1-based index in collection
        local nResult as long
        if self:__IsValid
            nResult := __List:SelectedIndex+1
        else
            nResult :=liSavedCurrentItemNo
        endif
        return nResult

    /// <exclude />
    assign __CurrentItemNo(nItemNo as int)
        // Assignes 1-based index in collection
        if self:__IsValid
            self:IsBusy := true
            if self:__Items:Count >= nItemNo
                __List:SelectedIndex := nItemNo-1
            endif
            self:IsBusy := false
        endif

        return

    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentText/*" />
    access CurrentText as string
        if self is ComboBox
            return super:CurrentText
        endif
        return null_string

    /// <include file="Gui.xml" path="doc/BaseListBox.DeleteItem/*" />
    method DeleteItem(nItemNumber := 0 as long)  as logic
        // nItemNumber = 1-based index in collection
        local lOk as logic
        if nItemNumber == 0
            nItemNumber := self:CurrentItemNo
        endif
        if nItemNumber <= self:ItemCount .and. nItemNumber > 0
            self:__Items:RemoveAt(nItemNumber-1)
        endif
        return lOk

        //METHOD Destroy() AS USUAL STRICT
        //IF oCtrl != NULL_OBJECT
        //	liSavedCurrentItemNo := SELF:CurrentItemNo
        //	sSavedCurrentItem	 := SELF:CurrentItem
        //ENDIF
        //RETURN SUPER:Destroy()

    /// <include file="Gui.xml" path="doc/BaseListBox.FindItem/*" />
    method FindItem(cItem as string, lWholeItem := true as logic, nStart := 0 as long) as long
        // nStart = 1-based index in collection
        // returns 1-based index in collection
        local liIndex := LB_ERR as longint

        if self:__IsValid
            if lWholeItem
                liIndex := self:__List:FindStringExact(cItem, nStart-1)
            else
                if Len(cItem) == 0
                    liIndex := -1
                else
                    liIndex := self:__List:FindString(cItem, nStart-1)
                endif
            endif
        endif

        if (liIndex < 0)
            return 0
        endif

        return liIndex+1
    method __GetItem(nItemNumber as long) as ListBoxItemValue
        if nItemNumber > 0 .and. nItemNumber <= self:ItemCount
            return (ListBoxItemValue) __Items[nItemNumber-1]
        endif
        return null_object

    /// <include file="Gui.xml" path="doc/BaseListBox.GetItem/*" />
    method GetItem(nItemNumber as long, nLength := -1 as long) as string
        // nItemNumber = 1-based index in collection
        local cItem as string
        if nItemNumber == 0
            nItemNumber := self:CurrentItemNo
            // Wenn kein aktuelles Item gefunden wurde und es sich um eine editierbare Combobox handelt wird der Text zurückgegeben
            if nItemNumber == 0 .and. self:__ComboBox != null .and. self:__ComboBox:DropDownStyle == System.Windows.Forms.ComboBoxStyle.DropDown
                cItem := self:__ComboBox:Text
            endif
        endif
        var oItem := self:__GetItem(nItemNumber)
        if oItem != null_object
            cItem := oItem:DisplayValue
        endif
        if nLength >= 0
            cItem := cItem:Substring(0, nLength)
        endif
        return cItem

    /// <include file="Gui.xml" path="doc/BaseListBox.GetItemDisplayValue/*" />
    method GetItemDisplayValue(nItemNumber as long) as string
        // nItemNumber = 1-based index in collection
        local cResult as string
        if nItemNumber == 0
            nItemNumber := self:CurrentItemNo
        endif
        var oItem := self:__GetItem(nItemNumber)
        if oItem != null_object
            cResult := oItem:DisplayValue
        endif
        return cResult

    /// <include file="Gui.xml" path="doc/BaseListBox.GetItemValue/*" />
    method GetItemValue(nItemNumber as long) as usual
        // nItemNumber = 1-based index in collection
        local oResult as object
        if nItemNumber == 0
            nItemNumber := self:CurrentItemNo
        endif
        var oItem := self:__GetItem(nItemNumber)
        if oItem != null_object
            oResult := oItem:Value
        endif
        return oResult

    /// <include file="Gui.xml" path="doc/BaseListBox.SetItemValue/*" />
    method SetItemValue(nItemNumber as long, oValue as usual ) as logic
        // nItemNumber = 1-based index in collection
        if nItemNumber == 0
            nItemNumber := self:CurrentItemNo
        endif
        var oItem := self:__GetItem(nItemNumber)
        if oItem != null_object
            oItem:Value := oValue
            return true
        endif
        return false

    /// <include file="Gui.xml" path="doc/BaseListBox.ctor/*" />
    constructor( oOwner, xID, oPoint, oDimension, kStyle, lDataAware)
        local sClassName as string
        if self is ComboBox
            sClassName := "combobox"
        else
            sClassName := "listbox"
        endif
        super(oOwner, xID, oPoint, oDimension, sClassName, kStyle, lDataAware)
        self:SetStyle(_or(WS_VScroll,WS_Border))
        return

    /// <include file="Gui.xml" path="doc/BaseListBox.ItemCount/*" />
    access ItemCount as long
        return  self:__Items:Count

    /// <include file="Gui.xml" path="doc/BaseListBox.SetTop/*" />
    method SetTop(nItemNumber := 0 as long)  as void
        // nItemNumber = 1-based index in collection
        if self:IsListBox
            self:__ListBox:TopIndex := nItemNumber-1
        endif

end class

/// <exclude />
[DebuggerDisplay("{DisplayValue}")];
class ListBoxItemValue implements IComparable
    property DisplayValue	as string auto
    property Value			as usual auto
    /// <exclude />
    constructor(lcDisplayValue as string, luValue as usual)
        self:DisplayValue := Rtrim(lcDisplayValue)
        if IsString(luValue)
            self:Value        := Rtrim(luValue)
        else
            self:Value := luValue
        endif

    /// <exclude />
    public method CompareTo(obj as object) as int
        if obj is ListBoxItemValue
            local otherVal := (ListBoxItemValue) obj as ListBoxItemValue
            return self:DisplayValue:CompareTo((object) otherVal:DisplayValue)
        endif
        return -1

    /// <exclude />
    public method ToString() as string strict
        return strtran(self:DisplayValue,"&","&&") // escape ampersand
end class
