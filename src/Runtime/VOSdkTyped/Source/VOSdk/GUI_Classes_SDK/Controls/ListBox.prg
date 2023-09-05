//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/ListBox/*" />
CLASS ListBox INHERIT BaseListBox
	PROTECT wSelectNum 		AS LONG
    /// <exclude  />
    PROPERTY ControlType AS ControlType GET ControlType.ListBox

    /// <exclude  />
	METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		LOCAL nItem AS LONG
		SELF:cSavedText := STRING.Empty
		nItem := SELF:__FindRetValue(SELF:uValue)
		SUPER:OnHandleCreated(o,e)
		SELF:__CurrentItemNo := nItem

    /// <exclude  />
	[Obsolete];
	METHOD __AddItem(cItem AS STRING, uRetValue AS USUAL, dwPosition AS LONG) AS VOID STRICT
		RETURN

 /// <exclude />
	METHOD __FindDisplayValue(cValue AS STRING) AS LONG STRICT
		// Returns 1-based index in collection
		LOCAL dwI AS LONG
		FOREACH oItem AS ListBoxItemValue IN SELF:__Items
			dwI++
			IF Alltrim(oItem:DisplayValue) == cValue
				RETURN dwI
			ENDIF
		NEXT

		RETURN 0


 /// <exclude />
	METHOD __FindRetValue(uValue AS USUAL) AS LONG STRICT
		LOCAL dwI as LONG
		// Returns 1-based index in collection
		FOREACH oItem AS ListBoxItemValue IN SELF:__Items
			dwI++
			IF valtype(oItem:Value) == valtype(uValue)
				IF oItem:Value == uValue
					RETURN dwI
				ENDIF
			ELSE
				// What is this ?
				oItem:Value := oItem:Value
			ENDIF
		NEXT
		RETURN 0

    /// <exclude  />
	METHOD __SetText(cNewText AS STRING) AS STRING
		IF cNewText != SELF:CurrentText
			RETURN SUPER:__SetText(cNewText)
		ENDIF
		RETURN cNewText

 /// <exclude />
	METHOD __Update() AS VOID STRICT
		LOCAL cOldValue AS STRING
		LOCAL oItem AS ListBoxItemValue
		LOCAL nIndex AS LONG
		IF SELF:Modified
			cOldValue := AsString(uValue)
			IF (nIndex := SELF:__List:SelectedIndex) >= 0
				oItem := SELF:__Items[nIndex]
				uValue := oItem:Value
			ELSE
				uValue := NIL
			ENDIF
			SELF:ValueChanged	:= !(cOldValue == AsString(uValue))
			SELF:Modified		:= FALSE
		ENDIF
		RETURN

 /// <exclude />
	ASSIGN __Value(uNewVal AS USUAL)  STRICT
		LOCAL cSelValue AS STRING
		LOCAL nIndex AS LONG
		LOCAL oValue AS ListBoxItemValue
		//PP-030924 allow value of NIL to reset control
		IF IsNil(uNewVal)
			SELF:__CurrentItemNo := 0
			SUPER:TextValue := ""
			uValue := uNewVal
		ELSE
			//PP-030924 clear and reload box, clears selection
			//SELF:__reset()
			cSelValue := AllTrim(AsString(uNewVal))
			nIndex := SELF:__FindRetValue(uNewVal)
			IF nIndex > 0
				// select the corresponding display string
				SELF:CurrentItemNo := nIndex
				oValue := SELF:__Items[nIndex-1]
				SELF:__SetText(oValue:DisplayValue)
				uValue := oValue:Value
			ELSE
				nIndex := SELF:FindItem(cSelValue, TRUE)
				IF nIndex == 0
					nIndex := SELF:FindItem(cSelValue, FALSE)
					IF nIndex > 0
						oValue := SELF:__Items[nIndex-1]
						IF ! cSelValue == oValue:DisplayValue
							nIndex := 0
						ENDIF
					ELSE
						uValue := uNewVal
					ENDIF
				ENDIF
				IF nIndex > 0
					oValue    := SELF:__Items[nIndex-1]
					cSelValue := oValue:DisplayValue
					uValue    := oValue:Value
				ENDIF
				SELF:__CurrentItemNo := nIndex
				SELF:__SetText(cSelValue)
			ENDIF
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/ListBox.AddItem/*" />
	METHOD AddItem(cItem , nItemNumber , uRetValue ) AS LONG
		// nItemNumber = 1-based index in collection
		// Returns 1-based index in collection

		LOCAL nIndex AS LONG
		IF IsNil(uRetValue)
			uRetValue := cItem
		ENDIF
		nIndex := SUPER:AddItem(cItem, nItemNumber, uRetValue)
		RETURN nIndex

/// <include file="Gui.xml" path="doc/ListBox.Caption/*" />
	PROPERTY Caption AS STRING GET cCaption SET cCaption := Value

/// <include file="Gui.xml" path="doc/ListBox.ChangeSelected/*" />
	METHOD ChangeSelected(oRange AS Range, lEnabled := TRUE AS LOGIC)  AS LOGIC
		LOCAL nItem AS LONG
		IF !SELF:lIsComboBox
			IF SELF:ValidateControl() .and. ! SELF:lIsComboBox
				FOR nItem := oRange:Min TO oRange:Max
					IF lEnabled
						SELF:__ListBox:SelectedIndices:Add(nItem-1)
					ELSE
						IF SELF:__ListBox:SelectedIndices:Contains(nItem-1)
							SELF:__ListBox:SelectedIndices:Remove(nItem-1)
						ENDIF
					ENDIF
				NEXT
				RETURN TRUE
			ENDIF
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListBox.Clear/*" />
	METHOD Clear() AS VOID
		IF SELF:FieldSpec == NULL_OBJECT
			SELF:uValue := NIL
		ELSE
			SELF:uValue := EmptyUsual(SELF:FieldSpec:UsualType)
		ENDIF

		SUPER:Clear()
		RETURN


/// <include file="Gui.xml" path="doc/ListBox.ClearSelection/*" />
	METHOD ClearSelection()
		IF SELF:ValidateControl()
			IF !lIsComboBox
				SELF:__ListBox:SelectedIndices:Clear()
			ENDIF
			SELF:__CurrentItemNo := 0
			RETURN TRUE
		ENDIF

		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListBox.Create/*" />
	METHOD Create() AS IVOControl STRICT
		IF oCtrl ==  NULL_OBJECT  .AND. !IsInstanceOf(SELF, #ComboBox)
			SELF:SetStyle(_OR(LBS_Notify, LBS_NoIntegralHeight))
		ENDIF
		RETURN SUPER:Create()

/// <include file="Gui.xml" path="doc/ListBox.CurrentItem/*" />
    PROPERTY CurrentItem  AS STRING
    GET
		IF SELF:ValidateControl()
			RETURN SELF:GetItem(0)
		ENDIF
        RETURN sSavedCurrentItem
    END GET
    SET
		SELF:CurrentItemNo := SELF:FindItem(value)
		RETURN
    END SET
    END PROPERTY
/// <include file="Gui.xml" path="doc/ListBox.CurrentItemNo/*" />
    PROPERTY CurrentItemNo AS LONG
    GET
		IF SELF:MultiSelection
			RETURN SELF:FirstSelected()
		ENDIF
		RETURN SUPER:CurrentItemNo
    END GET
    SET

		// nItemNo = 1-based index in collection
		LOCAL cSelValue AS STRING
		LOCAL dwIndex AS LONG
		LOCAL uOldValue AS USUAL
		LOCAL oItem AS ListBoxItemValue
		SELF:__CurrentItemNo := value

		IF value > 0
			cSelValue := SELF:CurrentItem
		ENDIF
		uOldValue := AsString(uValue)
		IF ! IsNil(cSelValue) .AND. (dwIndex := SELF:__FindDisplayValue(AllTrim(cSelValue))) > 0
			oItem := SELF:__Items[dwIndex-1]
			uValue := oItem:Value
		ELSE
			uValue := cSelValue
		ENDIF
		SELF:ValueChanged := !(AsString(uValue) == uOldValue)

    END SET
    END PROPERTY


/// <include file="Gui.xml" path="doc/ListBox.CurrentText/*" />
	ASSIGN CurrentText(cNewText AS STRING)
		SELF:__SetText(cNewText)
		RETURN

/// <include file="Gui.xml" path="doc/ListBox.DeleteItem/*" />
	METHOD DeleteItem(nItem := 0 AS LONG) AS LOGIC
		// nItem = 1-based index in collection
		LOCAL lReturnValue AS LOGIC
		lReturnValue := SUPER:DeleteItem(nItem)
		RETURN lReturnValue

/// <include file="Gui.xml" path="doc/ListBox.DeselectItem/*" />
	METHOD DeselectItem(nItem AS LONG) AS LOGIC
		// nItem = 1-based index in collection
		IF SELF:ValidateControl() .and. ! SELF:lIsComboBox
			IF SELF:__ListBox:SelectedIndices:Contains(nItem-1)
				SELF:__ListBox:SelectedIndices:Remove(nItem-1)
				RETURN TRUE
			ENDIF
		ENDIF

		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListBox.EnableItemDrag/*" />
	METHOD EnableItemDrag() AS VOID
		//Todo EnableItemDrag
		//IF IsInstanceOf(oFormSurface, #DialogWindow)
		//	Send(oFormSurface, #__SubClassForDragList)
		//ENDIF
		//SELF:setstyle(LBS_SORT, FALSE)
		//RETURN MakeDragList(SELF:Handle())

/// <include file="Gui.xml" path="doc/ListBox.FillUsing/*" />
	METHOD FillUsingBySortedList(oList AS System.Collections.Generic.SortedList<STRING,USUAL>) AS VOID STRICT
		IF !SELF:lIsComboBox
			SELF:__ListBox:BeginUpdate()
			SELF:Clear()
			IF oList:Count > 0 // aus irgendeinem Grund geht er bei leerer Liste trotzdem in die SChleife
				FOREACH kvp AS System.Collections.Generic.KeyValuePair<STRING,USUAL> IN oList
					SUPER:AddItem(kvp:Key, ,kvp:Value)
				NEXT
			ENDIF
			SELF:__ListBox:EndUpdate()
		ENDIF

/// <include file="Gui.xml" path="doc/ListBox.FillUsing/*" />
	METHOD FillUsing(aContents, symField1, symField2)
		LOCAL wArrLen AS DWORD
		LOCAL wElemLen AS DWORD
		LOCAL wIndex AS LONG
		LOCAL uElement AS USUAL
		LOCAL uDisplayValue AS USUAL
		LOCAL cDisplayValue AS STRING
		LOCAL uRetValue AS USUAL

		IF IsInstanceOfUsual(aContents, #DataServer) .AND. IsMethod(aContents, #GetLookUpTable)
			aContents := Send(aContents, #GetLookUpTable, Math.Min(0x7FFF, (LONG) IVarGet(aContents, #RecCount)), symField1, symField2)
		ELSEIF IsArray(aContents)
			IF !IsNil(symField1) .OR. !IsNil(symField2)
				WCError{#FillUsing,#ListBox,__WCSTypeError,symField1,2}:Throw()
			ENDIF
		ELSE
			WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:Throw()
		ENDIF

		SELF:Clear()
		SELF:IsBusy := TRUE
		IF SELF:lIsComboBox
			SELF:__ComboBox:BeginUpdate()
		ELSE
			SELF:__ListBox:BeginUpdate()
		ENDIF
		IF (wArrLen := ALen(aContents)) > 0
			FOR wIndex := 1 UPTO wArrLen
				uElement := aContents[wIndex]
				IF IsArray(uElement)
					wElemLen := ALen(uElement)
					IF wElemLen = 2
						uDisplayValue := uElement[1]
						uRetValue := uElement[2]
					ELSEIF wElemLen = 1
						uDisplayValue := uElement[1]
						uRetValue := uElement[1]
					ELSE
						WCError{#FillUsing,#ListBox,__WCSTypeError,aContents,1}:Throw()
					ENDIF
				ELSE
					uDisplayValue := uElement
					uRetValue := uElement
				ENDIF
				IF !IsString(uDisplayValue)
					cDisplayValue := AsString(uDisplayValue)
				ELSE
					cDisplayValue := (STRING) uDisplayValue
				ENDIF
				IF IsInstanceOf(SELF, #COMBOBOXEX)
					SELF:AddItem( cDisplayValue)
				ELSE
					SUPER:AddItem(cDisplayValue, ,uRetValue)
				ENDIF
			NEXT
		ENDIF
		IF SELF:lIsComboBox
			SELF:__ComboBox:EndUpdate()
		ELSE
			SELF:__ListBox:EndUpdate()
		ENDIF
		SELF:IsBusy := FALSE
		RETURN SELF

/// <include file="Gui.xml" path="doc/ListBox.FirstSelected/*" />
	METHOD FirstSelected ( ) AS LONG
		// nItem = 1-based index in collection
		LOCAL iResult AS LONG
		TRY
			IF SELF:__Items:Count > 0
				IF SELF:lIsComboBox
					iResult :=  SELF:__ComboBox:SelectedIndex+1
				ELSE
					IF __ListBox:SelectedIndex >= 0
						wSelectNum := 1
						IF wSelectNum <= __ListBox:SelectedIndices:Count
							iResult := (INT) __ListBox:SelectedIndices[wSelectNum-1]+1
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		CATCH  AS Exception
			iResult := 0
		END TRY
		RETURN iResult


/// <include file="Gui.xml" path="doc/ListBox.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		SUPER(oOwner, xID, oPoint, oDimension, kStyle, TRUE)
		RETURN

/// <include file="Gui.xml" path="doc/ListBox.IsSelected/*" />
	METHOD IsSelected(iIdx AS LONG)
		// nItem = 1-based index in collection
		LOCAL lResult AS LOGIC
		IF SELF:ValidateControl()
			IF ! SELF:MultiSelection
				lResult := (iIdx == SELF:CurrentItemNo)
			ELSEIF ! SELF:lIsComboBox
				lResult := SELF:__ListBox:SelectedIndices:Contains(iIdx-1)
			ENDIF
		ENDIF

		RETURN lResult

/// <include file="Gui.xml" path="doc/ListBox.ItemCount/*" />
	ACCESS ItemCount AS LONG
		RETURN (LONG) SELF:__Items:Count

/// <include file="Gui.xml" path="doc/ListBox.ListFiles/*" />
	METHOD ListFiles(sStartDir, oFixedText, FileTypes)
		//Todo ListFiles
		RETURN SELF
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

/// <include file="Gui.xml" path="doc/ListBox.MultiSelection/*" />
	ACCESS MultiSelection AS LOGIC
		LOCAL lMulti AS LOGIC
		IF SELF:ValidateControl() .and. ! SELF:lIsComboBox
			lMulti := __ListBox:SelectionMode == System.Windows.Forms.SelectionMode.MultiSimple .or. ;
					 __ListBox:SelectionMode == System.Windows.Forms.SelectionMode.MultiExtended
		ENDIF
		RETURN lMulti

/// <include file="Gui.xml" path="doc/ListBox.NextSelected/*" />
	METHOD NextSelected() AS LONG
		// RETURNS 1-based index in collection
		LOCAL iResult AS LONG
		IF SELF:__Items:Count > 0

			IF SELF:lIsComboBox
				iResult := 0
			ELSE
				wSelectNum 	:= wSelectNum + 1
				IF SELF:ValidateControl() .AND. wSelectNum <= SELF:__ListBox:SelectedIndices:Count
					iResult 	:= (INT) SELF:__ListBox:SelectedIndices[wSelectNum-1] +1
				ENDIF
			ENDIF
		ENDIF
		RETURN iResult

/// <include file="Gui.xml" path="doc/ListBox.SelectedCount/*" />
	ACCESS SelectedCount AS LONG
		LOCAL liNumSelected := 0 AS LONGINT

		IF SELF:ValidateControl()  .and. ! SELF:lIsComboBox
			liNumSelected := __ListBox:SelectedItems:Count

		ENDIF

		RETURN liNumSelected

/// <include file="Gui.xml" path="doc/ListBox.SelectedFile/*" />
	ACCESS SelectedFile AS STRING
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
		RETURN NULL_STRING

/// <include file="Gui.xml" path="doc/ListBox.SelectItem/*" />
	METHOD SelectItem(nItemId AS LONG) AS LOGIC
		// nItemID =  1-based index in collection
		IF SELF:ValidateControl()
			IF SELF:ItemCount >= nItemId .and. nItemId > 0
				IF SELF:lIsComboBox
					SELF:__ComboBox:SelectedIndex := nItemId-1
				ELSE
					SELF:__ListBox:SelectedIndex := nItemId-1
				ENDIF
				RETURN TRUE
			ENDIF
		ENDIF
		RETURN FALSE

	// not supported anymore
	[obsolete];
	METHOD SetTabs(aTabs AS ARRAY) AS VOID
		RETURN

	// Estimates optimal Width of the control by Measuring the Displayvalues
	METHOD GetOptimalWidth() AS DWORD
		LOCAL nMaxlen:= 0 AS DWORD
		FOREACH oVal AS ListBoxItemValue IN SELF:__Items
			nMaxlen := 100 // Max( System.Windows.Forms.TextRenderer.MeasureText(oVal:DisplayValue,oCtrl:Font,System.Drawing.Size{0,0},SupportFunctions.lsbTextflags):Width,nMaxlen)
		NEXT
		nMaxlen += 20 // Offset for Vertical Scrollbar
		RETURN nMaxlen

/// <include file="Gui.xml" path="doc/ListBox.TextValue/*" />
	ACCESS TextValue AS STRING
		LOCAL nItem AS LONG
		LOCAL oItem AS ListBoxItemValue
		IF SELF:ValidateControl()

			IF SELF:lIsComboBox
				nItem := SELF:__ComboBox:SelectedIndex
				IF nItem < 0
					RETURN SELF:__ComboBox:Text
				ENDIF
			ELSE
				nItem := SELF:__ListBox:SelectedIndex
			ENDIF
			IF nItem != -1
				oItem := (ListBoxItemValue) SELF:__Items[nItem]
				RETURN oItem:DisplayValue
			ENDIF
		ENDIF
		RETURN ""

/// <include file="Gui.xml" path="doc/ListBox.TextValue/*" />
	ASSIGN TextValue(cNewText AS STRING)
		LOCAL cSelValue AS STRING
		LOCAL dwIndex AS LONG
		LOCAL oItem AS ListBoxItemValue
		IF SELF:ValidateControl()

			cSelValue := cNewText
			IF (dwIndex := SELF:FindItem(cSelValue, TRUE, 1)) > 0
				oItem := SELF:__Items[dwIndex-1]
				uValue := oItem:Value
				SELF:__List:SelectedIndex := dwIndex-1
			ELSE
				IF (dwIndex := SELF:FindItem(cSelValue, FALSE, 1)) > 0
					oItem := SELF:__Items[dwIndex-1]
					uValue := oItem:Value
					SELF:__List:SelectedIndex := dwIndex-1
				ELSE
					SELF:__List:SelectedIndex := -1
					uValue := NIL
				ENDIF
			ENDIF
		ENDIF
		RETURN

END CLASS

//_DLL FUNCTION __DlgDirSelectEx( hDlg AS PTR, lpString AS PSZ, nCOunt AS INT, nIDListBox AS INT) AS LOGIC PASCAL:USER32.DlgDirSelectExA


