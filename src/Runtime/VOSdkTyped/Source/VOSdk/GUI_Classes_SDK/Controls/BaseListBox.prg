//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections
USING System.Collections.Generic
using System.Diagnostics

/// <summary>
/// Abstract (Empty) Class for compatibility with VO
/// </summary>
CLASS BaseListBox INHERIT TextControl
    PROTECT liSavedCurrentItemNo AS LONG
    PROTECT sSavedCurrentItem AS STRING
    PROTECT lIsComboBox		AS LOGIC
    PROPERTY  IsBusy         AS LOGIC AUTO

    PROPERTY __List AS IBaseListBox GET (IBaseListBox ) oCtrl

    /// <exclude />
    ACCESS __Items AS IList
        IF oCtrl != NULL
            RETURN __List:Items
        ELSE
            RETURN System.Collections.Generic.List<OBJECT>{}
        ENDIF

    /// <exclude />
    ACCESS __ComboBox AS VOComboBox
        IF SELF:lIsComboBox
            RETURN (VOComboBox) oCtrl
        ENDIF
        RETURN NULL_OBJECT

    /// <exclude />
    ACCESS __ListBox AS VOListBox
        IF SELF:lIsComboBox
            RETURN NULL_OBJECT
        ENDIF
        RETURN (VOListBox) oCtrl

    /// <include file="Gui.xml" path="doc/BaseListBox.AddItem/*" />
    METHOD AddItem(uItem , uIndex, uValue   ) AS LONG
        // nIndex = 1-based index in collection
        LOCAL nItem AS LONG
        LOCAL cItem AS STRING
        local oValue as object
        LOCAL nIndex AS LONG
        IF !IsString(uItem)
            WCError{#AddItem,#BaseListBox,__WCSTypeError,uItem,1}:Throw()
        ENDIF
        cItem := uItem
        IF IsLong(uIndex)
            nIndex := uIndex
        ENDIF
        oValue := uValue
        IF nIndex <= 0
            SELF:__Items:Add(ListBoxItemValue{cItem, oValue})
            nItem := ((ICollection) SELF:__Items):Count
        ELSE
            SELF:__Items:Insert(nIndex-1, ListBoxItemValue{cItem, oValue})
            nItem := nIndex
        ENDIF
        RETURN nItem
    /// <include file="Gui.xml" path="doc/BaseListBox.Clear/*" />

    METHOD Clear() AS VOID
        IF SELF:ValidateControl()
            SELF:__Items:Clear()
        ENDIF
        RETURN
    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentItem/*" />
    ACCESS CurrentItem AS STRING
        RETURN NULL_STRING
    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentItemNo/*" />
    ACCESS CurrentItemNo AS LONG
        // Returns 1-based index in collection
        LOCAL nResult AS LONG
        IF SELF:__IsValid
            nResult := __List:SelectedIndex+1
        ELSE
            nResult :=liSavedCurrentItemNo
        ENDIF
        RETURN nResult

    /// <exclude />
    ASSIGN __CurrentItemNo(nItemNo AS INT)
        // Assignes 1-based index in collection
        IF SELF:__IsValid
            SELF:IsBusy := TRUE
            IF SELF:__Items:Count >= nItemNo
                __List:SelectedIndex := nItemNo-1
            ENDIF
            SELF:IsBusy := FALSE
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/BaseListBox.CurrentText/*" />
    ACCESS CurrentText AS STRING
        IF SELF is ComboBox
            RETURN SUPER:CurrentText
        ENDIF
        RETURN NULL_STRING

    /// <include file="Gui.xml" path="doc/BaseListBox.DeleteItem/*" />
    METHOD DeleteItem(nItemNumber := 0 AS LONG)  AS LOGIC
        // nItemNumber = 1-based index in collection
        LOCAL lOk AS LOGIC
        IF nItemNumber == 0
            nItemNumber := SELF:CurrentItemNo
        ENDIF
        IF nItemNumber <= SELF:ItemCount .and. nItemNumber > 0
            SELF:__Items:RemoveAt(nItemNumber-1)
        ENDIF
        RETURN lOk

        //METHOD Destroy() AS USUAL STRICT
        //IF oCtrl != NULL_OBJECT
        //	liSavedCurrentItemNo := SELF:CurrentItemNo
        //	sSavedCurrentItem	 := SELF:CurrentItem
        //ENDIF
        //RETURN SUPER:Destroy()

    /// <include file="Gui.xml" path="doc/BaseListBox.FindItem/*" />
    METHOD FindItem(cItem AS STRING, lWholeItem := TRUE AS LOGIC, nStart := 0 AS LONG) AS LONG
        // nStart = 1-based index in collection
        // returns 1-based index in collection
        LOCAL liIndex := LB_ERR AS LONGINT

        IF SELF:__IsValid
            IF lWholeItem
                liIndex := SELF:__List:FindStringExact(cItem, nStart-1)
            ELSE
                IF Len(cItem) == 0
                    liIndex := -1
                ELSE
                    liIndex := SELF:__List:FindString(cItem, nStart-1)
                ENDIF
            ENDIF
        ENDIF

        IF (liIndex < 0)
            RETURN 0
        ENDIF

        RETURN liIndex+1
    /// <include file="Gui.xml" path="doc/BaseListBox.GetItem/*" />
    METHOD GetItem(nItemNumber AS LONG, nLength := -1 AS LONG) AS STRING
        // nItemNumber = 1-based index in collection
        LOCAL oItem AS ListBoxItemValue
        LOCAL cItem AS STRING
        IF nItemNumber == 0
            nItemNumber := SELF:CurrentItemNo
            // Wenn kein aktuelles Item gefunden wurde und es sich um eine editierbare Combobox handelt wird der Text zurückgegeben
            IF nItemNumber == 0 .AND. SELF:__ComboBox != NULL .AND. SELF:__ComboBox:DropDownStyle == System.Windows.Forms.ComboBoxStyle.DropDown
                cItem := SELF:__ComboBox:Text
            ENDIF
        ENDIF
        IF nItemNumber > 0 .and. nItemNumber <= SELF:ItemCount
            oItem := (ListBoxItemValue) __Items[nItemNumber-1]
            cItem := oItem:DisplayValue
        ENDIF
        IF nLength >= 0
            cItem := cItem:Substring(0, nLength)
        ENDIF
        RETURN cItem

    /// <include file="Gui.xml" path="doc/BaseListBox.GetItemDisplayValue/*" />
    METHOD GetItemDisplayValue(nItemNumber AS LONG) AS STRING
        // nItemNumber = 1-based index in collection
        LOCAL oItem AS ListBoxItemValue
        LOCAL cResult AS STRING
        IF nItemNumber == 0
            nItemNumber := SELF:CurrentItemNo
        ENDIF
        IF nItemNumber > 0 .and. nItemNumber <= SELF:ItemCount
            oItem := (ListBoxItemValue) __Items[nItemNumber-1]
            cResult := oItem:DisplayValue
        ENDIF
        RETURN cResult

    /// <include file="Gui.xml" path="doc/BaseListBox.GetItemValue/*" />
    METHOD GetItemValue(nItemNumber AS LONG) AS USUAL
        // nItemNumber = 1-based index in collection
        LOCAL oItem AS ListBoxItemValue
        LOCAL oResult AS OBJECT
        IF nItemNumber == 0
            nItemNumber := SELF:CurrentItemNo
        ENDIF
        IF nItemNumber > 0 .and. nItemNumber <= SELF:ItemCount
            oItem := (ListBoxItemValue) __Items[nItemNumber-1]
            oResult := oItem:Value
        ENDIF
        RETURN oResult

    /// <include file="Gui.xml" path="doc/BaseListBox.SetItemValue/*" />
    METHOD SetItemValue(nItemNumber AS LONG, oValue AS USUAL ) AS LOGIC
        // nItemNumber = 1-based index in collection
        LOCAL oItem AS ListBoxItemValue
        IF nItemNumber == 0
            nItemNumber := SELF:CurrentItemNo
        ENDIF
        IF nItemNumber > 0 .and. nItemNumber <= SELF:ItemCount
            oItem := (ListBoxItemValue) __Items[nItemNumber-1]
            oItem:Value := oValue
            RETURN TRUE
        ENDIF
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/BaseListBox.ctor/*" />
    CONSTRUCTOR( oOwner, xID, oPoint, oDimension, kStyle, lDataAware)
        LOCAL sClassName AS STRING
        IF SELF is ComboBox
            SELF:lIsComboBox := TRUE
            sClassName := "combobox"
        ELSE
            sClassName := "listbox"
        ENDIF
        SUPER(oOwner, xID, oPoint, oDimension, sClassName, kStyle, lDataAware)
        SELF:SetStyle(_OR(WS_VScroll,WS_Border))
        RETURN

    /// <include file="Gui.xml" path="doc/BaseListBox.ItemCount/*" />
    ACCESS ItemCount AS LONG
        RETURN  SELF:__Items:Count

    /// <include file="Gui.xml" path="doc/BaseListBox.SetTop/*" />
    METHOD SetTop(nItemNumber := 0 AS LONG)  AS VOID
        // nItemNumber = 1-based index in collection
        IF !SELF:lIsComboBox
            SELF:__ListBox:TopIndex := nItemNumber-1
        ENDIF

END CLASS

/// <exclude />
[DebuggerDisplay("{DisplayValue}")];
CLASS ListBoxItemValue IMPLEMENTS IComparable
    PROTECT cDisplayValue	AS STRING
    PROTECT uValue			AS USUAL
    PROPERTY DisplayValue	AS STRING GET cDisplayValue SET cDisplayValue := Value
    PROPERTY Value			AS USUAL GET uValue	SET uValue := Value
    /// <exclude />
    CONSTRUCTOR(lcDisplayValue AS STRING, luValue AS USUAL)
        cDisplayValue := Rtrim(lcDisplayValue)
        IF IsString(luValue)
            uValue        := Rtrim(luValue)
        ELSE
            uValue := luValue
        ENDIF

    /// <exclude />
    PUBLIC METHOD CompareTo(obj AS OBJECT) AS INT
        IF obj is ListBoxItemValue
            LOCAL otherVal := (ListBoxItemValue) obj AS ListBoxItemValue
            RETURN SELF:DisplayValue:CompareTo((OBJECT) otherVal:DisplayValue)
        ENDIF
        RETURN -1

    /// <exclude />
    PUBLIC METHOD ToString() AS STRING STRICT
        RETURN strtran(SELF:cDisplayValue,"&","&&") // escape ampersand
END CLASS
