//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// ResourceMenu.prg
USING System.Collections.Generic
#define    M_GRAYED         0x0001   // 'GRAYED' keyword
#define    M_INACTIVE       0x0002   // 'INACTIVE' keyword
#define    M_BITMAP         0x0004   // 'BITMAP' keyword
#define    M_OWNERDRAW      0x0100   // 'OWNERDRAW' keyword
#define    M_CHECKED        0x0008   // 'CHECKED' keyword
#define    M_POPUP          0x0010   // Used internally
#define    M_MENUBARBREAK   0x0020   // 'MENUBARBREAK' keyword
#define    M_MENUBREAK      0x0040   // 'MENUBREAK' keyword
#define    M_ENDMENU        0x0080   // Used internally

USING System.Diagnostics


CLASS ResourceMenu INHERIT ResourceReader
	PROPERTY MenuItems  AS List<ResourceMenuItem>   AUTO
	PROPERTY IsValid	AS LOGIC    AUTO
	PROPERTY HelpID		AS DWORD	AUTO
	protect nLevel	    as dword
	METHOD __LoadFromResource(hDLL as IntPtr, hResInfo as IntPtr) as LOGIC
		LOCAL lpBuffer AS IntPtr
		LOCAL uiResSize AS DWORD
		LOCAL hResource AS IntPtr
		MenuItems :=List<ResourceMenuItem>{}
		SELF:IsValid := FALSE
		IF hResInfo != NULL
			uiResSize := GuiWin32.SizeOfResource(hDLL, hResInfo)
			IF uiResSize != 0
				hResource := GuiWin32.LoadResource(hDLL, hResInfo)
				IF hResource != NULL
					lpBuffer := GuiWin32.LockResource(hResource)
					SELF:ReadData(lpBuffer)
					SELF:IsValid := TRUE
					GuiWin32.FreeResource(hResource)
				ENDIF
			endif
		ENDIF
		RETURN SELF:IsValid

	CONSTRUCTOR(hDLL AS IntPtr, cName AS STRING)
		LOCAL hResInfo AS IntPtr
		SUPER()
		hResInfo := GuiWin32.FindResource(hDLL, cName, 4)
		SELF:__LoadFromResource(hDLL, hResInfo)
		return

	METHOD ReadData(lpBuffer AS IntPtr) AS VOID
        local pMenuEx	as MENUEX_TEMPLATE_HEADER
        local pMenu	    as MENU_TEMPLATE_HEADER

		pMenuEx	:= (MENUEX_TEMPLATE_HEADER PTR) lpBuffer
		if pMenuEx:wVersion == 01
			// this is indeed a DialogEx
			SELF:ReadMenuEx(pMenuEx)
        else
            pMenu	:= (MENU_TEMPLATE_HEADER ptr) lpBuffer
			self:ReadMenu(pMenu)
		ENDIF
		RETURN


	INTERNAL METHOD ReadMenuEx(pMenuEx as MENUEX_TEMPLATE_HEADER) as VOID
		local pWord     as word ptr
		SELF:HelpID := pMenuEx:dwHelpID
		pWord := (WORD PTR) pMenuEx
		pWord += pMenuEx:wOffSet/2
		nLevel := 1
		DO WHILE nLevel > 0
			//if ((Int) pWord % 4) != 0
			//	pWord += 1
			//ENDIF
			pWord:= SELF:ReadMenuItemEx(pWord)
		ENDDO
		RETURN


	INTERNAL METHOD ReadMenu(pMenu as MENU_TEMPLATE_HEADER) AS VOID
		local pWord     as word ptr
		SELF:HelpID := 0
		pWord := (WORD PTR) pMenu
		pWord += sizeof(MENU_TEMPLATE_HEADER) / sizeof(WORD)
		pWord += pMenu:wOffSet/2
		nLevel := 1
		DO WHILE nLevel > 0
			//if ((Int) pWord % 4) != 0
			//	pWord += 1
			//ENDIF
			pWord := SELF:ReadMenuItem(pWord)
		ENDDO
		return

	METHOD ReadMenuItem(pWord as WORD PTR) as WORD PTR
		LOCAL pItem as NormalMenuItem
		LOCAL oItem as ResourceMenuItem
		oItem := ResourceMenuItem{}

		pItem := (NormalMenuItem PTR) pWord
		oItem:Flags := pItem:fItemFlags
		IF oItem:IsPopup
			pWord += 1
			nLevel+=1
		ELSE
			oItem:ItemID := pItem:wMenuID
			pWord += 2
		ENDIF
		IF oItem:IsLast
			nLevel -=1
		ENDIF
		oItem:Caption := ReadText(pWord)
		pWord += oItem:Caption:Length+1
		SELF:MenuItems:Add(oItem)
		RETURN pWord

	METHOD ReadMenuItemEx(pWord as WORD PTR) as WORD PTR
		LOCAL pItem as MENUEX_TEMPLATE_ITEM
		LOCAL oItem as ResourceMenuItem
		oItem := ResourceMenuItem{}

		pItem := (MENUEX_TEMPLATE_ITEM PTR) pWord
		oItem:Type		:= pItem:dwType
		oItem:ItemID	:= pItem:uID
		oItem:Flags		:= pItem:bResInfo
		IF oItem:IsPopup
			nLevel+=1
		ENDIF
		IF oItem:IsLast
			nLevel -=1
		ENDIF
		pWord += 7
		oItem:Caption := ReadText(pWord)
		pWord += oItem:Caption:Length+1
		oItem:HelpID := pWord[1] << 16 + pWord[2]
		pWord += 2
		SELF:MenuItems:Add(oItem)
		RETURN pWord

	METHOD AsString() AS STRING STRICT
		LOCAL cResult as STRING
		LOCAL cPrefix as STRING
		FOREACH oItem as ResourceMenuItem in SELF:MenuItems
			IF String.IsNullOrEmpty(oItem:Caption)
				cResult += cPrefix+"-"+CRLF
			ELSE
				cResult += cPrefix+oItem:Caption
				IF oItem:ItemID != 0
					cResult += e"\t( "+NTrim(oItem:ItemID)+")"+CRLF
				ELSE
					cResult += CRLF
				ENDIF
			ENDIF
			if oItem:IsPopup
				cPrefix += "    "
			ENDIF
			IF oItem:IsLast
				cPrefix := Left(cPrefix, SLen(cPrefix)-4)
			ENDIF
		NEXT
		RETURN cResult

	METHOD AddItemsTo(oMenu as Menu) AS VOID
		LOCAL aStack as List<System.Windows.Forms.Menu>
		LOCAL oCurrent	as System.Windows.Forms.Menu
		LOCAL oNew as VOMenuItem
		aStack := List<System.Windows.Forms.Menu>{}
		oCurrent := oMenu:__Menu
		FOREACH oItem as ResourceMenuItem in MenuItems
			oNew := oMenu:__CreateMenuItem(oItem:Caption, oItem:ItemID)
			oCurrent:MenuItems:Add(oNew)
			oNew:Enabled	:= !oItem:IsDisabled
			oNew:Checked	:= oItem:IsChecked

			if oItem:IsPopup
				aStack:Add(oCurrent)
				oCurrent := oNew
			elseif oItem:IsLast
				if aStack:Count > 0
					oCurrent := aStack[aStack:Count-1]
					aStack:RemoveAt(aStack:Count-1)
				endif
			endif
		NEXT
		return

END CLASS

INTERNAL VOSTRUCT MENUEX_TEMPLATE_HEADER ALIGN 2
	MEMBER wVersion AS WORD
	MEMBER wOffSet as WORD
	MEMBER dwHelpID as DWORD

INTERNAL VOSTRUCT MENU_TEMPLATE_HEADER ALIGN 2
	MEMBER wVersion AS WORD
	MEMBER wOffSet as WORD

INTERNAL VOSTRUCT MENUEX_TEMPLATE_ITEM ALIGN 2
	MEMBER dwType as DWORD
	MEMBER dwState as DWORD
	MEMBER uID as LONG
	MEMBER bResInfo as WORD
	// MEMBER szText	as WCHAR[]
	// MEMBER dwHelpID AS DWORD
	// Offset of item = DWORD aligned


INTERNAL VOSTRUCT NormalMenuItem ALIGN 2
	MEMBER fItemFlags	as WORD
	MEMBER wMenuID		as WORD
	// MEMBER szItemText   as WCHAR[]
	// Seperator has fItemFlags = 0 and wMenuID = 0 and an empty string

//INTERNAL VOSTRUCT PopupMenuItem
//	MEMBER fItemFlags AS WORD
// MEMBER szItemText AS WCHAR[]

[DebuggerDisplay("ID: {ItemID}, Caption: {Caption}, Flags {Flags}")];
CLASS ResourceMenuItem INHERIT ResourceReader
	PROPERTY Caption	AS STRING AUTO
	PROPERTY HelpID	    AS DWORD AUTO
	PROPERTY ItemID	    AS LONG AUTO
	PROPERTY Flags	    AS LONG AUTO
	property Type		as dword auto
	PROPERTY IsPopup  as LOGIC GET _AND(Flags, M_POPUP) == M_POPUP
	PROPERTY IsLast   as LOGIC GET _AND(Flags, M_ENDMENU) == M_ENDMENU
	PROPERTY IsChecked as LOGIC GET _AND(Flags, M_CHECKED) == M_CHECKED
	PROPERTY IsDisabled as LOGIC GET _AND(Flags, M_GRAYED) == M_GRAYED
END CLASS


