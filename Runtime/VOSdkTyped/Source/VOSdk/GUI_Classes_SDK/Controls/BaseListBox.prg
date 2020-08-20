


using System.Collections
using System.Diagnostics

/// <summary>
/// Abstract (Empty) Class for compatibility with VO 
/// </summary>
CLASS BaseListBox INHERIT TextControl
	PROTECT liSavedCurrentItemNo AS LONG
	PROTECT sSavedCurrentItem AS STRING	
	PROTECT lIsComboBox		AS LOGIC
	PROPERTY  IsBusy         AS LOGIC AUTO
	
	ACCESS __List AS System.Windows.Forms.ListControl
		RETURN (System.Windows.Forms.ListControl) oCtrl
	
	ACCESS __Items AS IList
		IF oCtrl != NULL
			IF SELF:lIsComboBox
				RETURN ((VOComboBox) oCtrl):Items
			ENDIF
			RETURN ((VOListBox) oCtrl):Items
		ELSE
			RETURN System.Collections.Generic.List<ListBoxItemValue>{}
		ENDIF
	
	ACCESS __ComboBox AS VOComboBox
		IF SELF:lIsComboBox
			RETURN (VOComboBox) oCtrl
		ENDIF
		RETURN NULL_OBJECT

	ACCESS __ListBox AS VOListBox
		IF SELF:lIsComboBox
			RETURN NULL_OBJECT			
		ENDIF
		RETURN (VOListBox) oCtrl
	
	ACCESS __ListControl AS System.Windows.Forms.ListControl
		RETURN (System.Windows.Forms.ListControl) oCtrl
	
	METHOD AddItem(uItem , uIndex, uValue   ) AS LONG
		// nIndex = 1-based index in collection
		LOCAL nItem AS LONG
		LOCAL cItem AS STRING
		LOCAL oValue AS OBJECT
		LOCAL nIndex AS LONG
		IF !IsString(uItem)
			WCError{#AddItem,#BaseListBox,__WCSTypeError,uItem,1}:@@Throw()
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
	
	METHOD Clear() AS VOID
		IF SELF:ValidateControl()
			SELF:__Items:Clear()
		ENDIF
		RETURN 

	ACCESS CurrentItem AS STRING
		RETURN NULL_STRING

	ACCESS CurrentItemNo AS LONG
		// Returns 1-based index in collection
		LOCAL nResult AS LONG
		IF SELF:__IsValid 
			nResult := SELF:__List:SelectedIndex+1
		ELSE
			nResult :=liSavedCurrentItemNo
		ENDIF
		RETURN nResult
	
	ASSIGN __CurrentItemNo(nItemNo AS INT)   
		// Assignes 1-based index in collection
		IF SELF:__IsValid 
			SELF:IsBusy := TRUE
			IF SELF:__Items:Count >= nItemNo
				SELF:__List:SelectedIndex := nItemNo-1
			ENDIF
			SELF:IsBusy := FALSE
		ENDIF

		RETURN 
	
	ACCESS CurrentText AS STRING
		IF SELF is ComboBox
			RETURN SUPER:CurrentText
		ENDIF
		RETURN NULL_STRING

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
	
	METHOD FindItem(cItem AS STRING, lWholeItem := TRUE AS LOGIC, nStart := 0 AS LONG) AS LONG
		// nStart = 1-based index in collection
		// returns 1-based index in collection		
		LOCAL liIndex := LB_ERR AS LONGINT
		
		IF SELF:__IsValid 
			IF SELF:lIsComboBox
				IF lWholeItem
					liIndex := SELF:__ComboBox:FindStringExact(cItem, nStart-1)
				ELSE
					IF Len(cItem) == 0
						liIndex := -1
					ELSE
						liIndex := SELF:__ComboBox:FindString(cItem, nStart-1)
					ENDIF
				ENDIF					
					
			ELSE
				IF lWholeItem
					liIndex := SELF:__ListBox:FindStringExact(cItem, nStart-1)
				ELSE
					IF Len(cItem) == 0
						liIndex := -1
					ELSE
						liIndex := SELF:__ListBox:FindString(cItem, nStart-1)
					ENDIF
				ENDIF					
			ENDIF
		ENDIF
		
		IF (liIndex < 0)
			RETURN 0
		ENDIF
		
		RETURN liIndex+1
		
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
	
	ACCESS ItemCount AS LONG
		RETURN  SELF:__Items:Count

	METHOD SetTop(nItemNumber := 0 AS LONG)  AS VOID
		// nItemNumber = 1-based index in collection
		IF !SELF:lIsComboBox
			SELF:__ListBox:TopIndex := nItemNumber-1
		ENDIF

END CLASS


[DebuggerDisplay("{DisplayValue}")];
CLASS ListBoxItemValue IMPLEMENTS IComparable
	PROTECT cDisplayValue	AS STRING
	PROTECT uValue			AS USUAL
	PROPERTY DisplayValue	AS STRING GET cDisplayValue SET cDisplayValue := Value
	PROPERTY Value			AS USUAL GET uValue	SET uValue := Value
	
	CONSTRUCTOR(lcDisplayValue AS STRING, luValue AS USUAL)
		cDisplayValue := Rtrim(lcDisplayValue)
		IF IsString(luValue)
			uValue        := Rtrim(luValue)
		ELSE
			uValue := luValue
		ENDIF
	
	PUBLIC METHOD CompareTo(obj AS OBJECT) AS INT
		IF obj is ListBoxItemValue
			LOCAL otherVal := (ListBoxItemValue) obj AS ListBoxItemValue
			IF strtran(SELF:cDisplayValue, chr(255),"") == "<nicht enthaltener Eintrag>"
				RETURN 1
			ELSEIF strtran(otherval:cDisplayValue, chr(255),"") == "<nicht enthaltener Eintrag>"
				RETURN -1
			ELSE
				RETURN SELF:DisplayValue:CompareTo((OBJECT) otherVal:DisplayValue)
			ENDIF
		ENDIF
		RETURN -1
	
	PUBLIC METHOD ToString() AS STRING STRICT
			LOCAL chilf := strtran(SELF:cDisplayValue, chr(255),"") AS STRING // remove specialchar (that was added to <nicht enthaltener Eintrag> to make it the last entry)
			RETURN strtran(chilf,"&","&&") // escape ampersand
END CLASS
