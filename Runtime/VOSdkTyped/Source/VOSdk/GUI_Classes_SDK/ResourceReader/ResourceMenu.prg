
// ResourceMenu.prg
#using System.Collections.Generic
#define    M_GRAYED         0x0001   // 'GRAYED' keyword 
#define    M_INACTIVE       0x0002   // 'INACTIVE' keyword 
#define    M_BITMAP         0x0004   // 'BITMAP' keyword 
#define    M_OWNERDRAW      0x0100   // 'OWNERDRAW' keyword 
#define    M_CHECKED        0x0008   // 'CHECKED' keyword 
#define    M_POPUP          0x0010   // Used internally 
#define    M_MENUBARBREAK   0x0020   // 'MENUBARBREAK' keyword 
#define    M_MENUBREAK      0x0040   // 'MENUBREAK' keyword 
#define    M_ENDMENU        0x0080   // Used internally 

#USING System.Diagnostics


CLASS ResourceMenu INHERIT ResourceReader
	EXPORT MenuItems as List<ResourceMenuItem>
	EXPORT IsValid		AS LOGIC
	EXPORT HelpID		AS DWORD	
	PROTECT nLevel	    as DWORD	
	METHOD __LoadFromResource(hDLL as IntPtr, hResInfo as IntPtr) as LOGIC
		LOCAL lpBuffer AS IntPtr
		LOCAL uiResSize AS DWORD
		LOCAL hResource AS IntPtr
		MenuItems :=List<ResourceMenuItem>{}
		SELF:IsValid := FALSE
		IF hResInfo != NULL
			uiResSize := Win32.SizeOfResource(hDLL, hResInfo)
			IF uiResSize != 0
				hResource := Win32.LoadResource(hDLL, hResInfo)
				IF hResource != NULL
					lpBuffer := Win32.LockResource(hResource)
					SELF:ReadData(lpBuffer)
					SELF:IsValid := TRUE
					Win32.FreeResource(hResource)
				ENDIF
			ENDIF	
		ENDIF
		RETURN SELF:IsValid

	CONSTRUCTOR(hDLL AS IntPtr, cName AS STRING)
		LOCAL hResInfo AS IntPtr
		SUPER()
		hResInfo := Win32.FindResource(hDLL, cName, 4)
		SELF:__LoadFromResource(hDLL, hResInfo)
		RETURN 

	METHOD ReadData(lpBuffer AS IntPtr) AS VOID
		LOCAL pMenuEx	AS MENUEX_TEMPLATE_HEADER

		pMenuEx	:= (MENUEX_TEMPLATE_HEADER PTR) lpBuffer
		IF pMenuEx:wVersion == 01 
			// this is indeed a DialogEx
			SELF:ReadMenuEx(pMenuEx)
		ELSE
			SELF:ReadMenu( (PTR) lpBuffer)
		ENDIF
		RETURN
		
	
	INTERNAL METHOD ReadMenuEx(pMenuEx as MENUEX_TEMPLATE_HEADER) as VOID
		LOCAL pWord     as WORD PTR	
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
		LOCAL pWord     as WORD PTR	
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
		RETURN 

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
		RETURN 

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
	EXPORT Caption	as STRING
	EXPORT HelpID	as DWORD
	EXPORT ItemID	as LONG
	EXPORT Flags	as LONG
	EXPORT Type		as DWORD		
	PROPERTY IsPopup  as LOGIC GET _AND(Flags, M_POPUP) == M_POPUP
	PROPERTY IsLast   as LOGIC GET _AND(Flags, M_ENDMENU) == M_ENDMENU
	PROPERTY IsChecked as LOGIC GET _AND(Flags, M_CHECKED) == M_CHECKED
	PROPERTY IsDisabled as LOGIC GET _AND(Flags, M_GRAYED) == M_GRAYED
END CLASS


