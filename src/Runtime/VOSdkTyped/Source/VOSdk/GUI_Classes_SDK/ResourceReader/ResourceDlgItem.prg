//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// ResourceDlgItem.prg

// This class contains the code to read a native dialog item from a dialog resource
// There are 2 versionf of the DLGITEMTEMPLATE struct
// The structures are copied from the WIN32 API Lib


USING System.Runtime.InteropServices
USING System.Text
USING System.Diagnostics

[DebuggerDisplay("ID: {ControlID}, Caption: {Caption}, Class: {Class}, X,Y: {X},{Y} ")];
CLASS ResourceDialogItem  INHERIT ResourceReader
	PROPERTY Caption		AS STRING   AUTO
	PROPERTY ClassID		AS LONG     AUTO
	PROPERTY ClassName	    AS STRING   AUTO
	PROPERTY ControlID	    AS WORD     AUTO
	PROPERTY X              AS LONG     AUTO
	PROPERTY Y              AS LONG     AUTO
	PROPERTY CX             AS LONG     AUTO
	PROPERTY CY             AS LONG     AUTO
	PROPERTY Style          AS LONG     AUTO
	PROPERTY ExStyle        AS LONG     AUTO
	PROPERTY HelpID		    AS DWORD    AUTO
	PROPERTY TabIndex		AS LONG     AUTO

	// typedef struct {
	//		DWORD helpID;
	//		DWORD exStyle;
	//		DWORD style;
	//		short x;
	//		short y;
	//		short cx;
	//		short cy;
	//		WORD id;
	//		sz_Or_Ord windowClass; // name or ordinal of a window class
	//		sz_Or_Ord title; // title string or ordinal of a resource
	//		WORD extraCount; // bytes of following creation data
	// } DLGITEMTEMPLATEEX; 

	//typedef struct { 
	//		DWORD style;
	//		DWORD dwExtendedStyle;
	//		short x;
	//		short y;
	//		short cx;
	//		short cy;
	//		WORD id;
	//} DLGITEMTEMPLATE; 

	PROPERTY @@Class	AS STRING GET iif(String.IsNullOrEmpty(ClassName), ClassID:ToString("X4"), ClassName)

	PROPERTY WindowClassName AS STRING
		GET
			IF !String.IsNullOrEmpty(SELF:ClassName)
				RETURN SELF:ClassName
			ENDIF
			DO CASE
			CASE ClassID == 0x80 // Button
				RETURN "Button"
			CASE ClassID == 0x81 // Edit
				RETURN "Edit"
			CASE ClassID == 0x82 // Static
				RETURN "Static" 
			CASE ClassID == 0x83 // Listbox				
				RETURN "ListBox"
			CASE ClassID == 0x84 // Scrollbar			
				RETURN "ScrollBar"
			CASE ClassID == 0x85 // Combobox
				RETURN "ComboBox"
			OTHERWISE
				RETURN "Static" 
			ENDCASE
		END GET
	END PROPERTY

	PROPERTY IsComboBox AS LOGIC
		GET
			RETURN ClassID == 0x85 .or. String.Compare(SELF:ClassName, "COMBOBOX", TRUE) == 0
		END GET
	END PROPERTY
		
	ACCESS Origin as Point
		RETURN Point{SELF:X, SELF:Y}
		
	ACCESS Size as Dimension
		RETURN Dimension{SELF:CX, SELF:CY}
	
	METHOD ReadEx(pWord AS WORD PTR) AS WORD PTR
		LOCAL pItemEx AS winDLGITEMTEMPLATEEx
		// Align on DWORD boundary
		IF ((DWORD) pWord) % 4  != 0
			pWord += 1
		ENDIF
		pItemEx		:= ( winDLGITEMTEMPLATEEx PTR)  pWord
		SELF:HelpID := pItemEx:helpID
		SELF:X		:= pItemEx:x
		SELF:Y		:= pItemEx:y
		SELF:CX		:= pItemEx:cx
		SELF:CY		:= pItemEx:cy
		SELF:Style	:= pItemEx:style
		SELF:ExStyle:= pItemEx:dwExtendedStyle
		SELF:ControlID := pItemEx:id
		pWord += sizeof(winDLGITEMTEMPLATEEx) / SIZEOF(WORD)
		
		
		// Align on DWORD boundary
		IF ((DWORD) pWord) % 4  != 0
			pWord += 1
		ENDIF
		pWord := SELF:ReadExtraInfo(pWord)
		RETURN pWord

	METHOD Read(pWord AS WORD PTR) AS WORD PTR
		LOCAL pItem AS winDLGITEMTEMPLATE
		// Align on DWORD boundary
		IF ((DWORD) pWord) % 4  != 0
			pWord += 1
		ENDIF
		pItem := ( winDLGITEMTEMPLATE PTR) pWord
		SELF:X	:= pItem:x
		SELF:Y	:= pItem:y
		SELF:CX := pItem:cx
		SELF:CY := pItem:cy
		SELF:Style	 := pItem:style
		SELF:ExStyle := pItem:dwExtendedStyle
		SELF:ControlID := pItem:id
		pWord	+= sizeof(winDLGITEMTEMPLATE) / SIZEOF(WORD)
		pWord := SELF:ReadExtraInfo(pWord)
		RETURN pWord

	METHOD ReadExtraInfo(pWord as WORD PTR) AS WORD PTR
		// Class as Sz_Or_Ord:
		// If the first element is 0xFFFF, 
		// the array has one additional element that specifies the ordinal value of a predefined system class. 
		// If the first element of this array is any value other than 0xFFFF, the system treats the array as 
		// a null-terminated Unicode string that specifies the name of a registered window class
			LOCAL uiSize AS DWORD
	
		IF pWord[1] == 0XFFFF
			SELF:ClassID := pWord[2]
			pWord += 2
		ELSE
			SELF:ClassName := ReadText(pWord) 
			pWord += SELF:ClassName:Length+1
		ENDIF
		// Class as Sz_Or_Ord:
		// If the first element of this array is 0xFFFF, the array has one additional element that 
		// specifies the ordinal value of a resource, such as an icon, in an executable file. 
		// f the first element is any value other than 0xFFFF, the system treats the array as a 
		// null-terminated Unicode string that specifies the initial text.
		IF pWord[1] == 0
			SELF:Caption := ""
			pWord += 1
		ELSE
			SELF:Caption := ReadText(pWord)
			pWord  += SELF:Caption:Length+1
		ENDIF
		// ExtraCount
		uiSize := (DWORD) pWord[1] 
		pWord  += 1
		IF uiSize != 0
			pWord += (uiSize/2)
		ENDIF
		
		RETURN pWord
	
	
END CLASS

INTERNAL VOSTRUCT winDLGITEMTEMPLATEEx ALIGN 2 
	MEMBER helpID AS DWORD
	MEMBER  dwExtendedStyle AS LONG
	MEMBER  style AS LONG
	MEMBER  x AS SHORTINT
	MEMBER  y AS SHORTINT
	MEMBER  cx AS SHORTINT
	MEMBER  cy AS SHORTINT
MEMBER  id AS WORD

INTERNAL VOSTRUCT winDLGITEMTEMPLATE ALIGN 2 
	MEMBER  style AS LONG
	MEMBER  dwExtendedStyle AS LONG
	MEMBER  x AS SHORTINT
	MEMBER  y AS SHORTINT
	MEMBER  cx AS SHORTINT
	MEMBER  cy AS SHORTINT
MEMBER  id AS WORD


