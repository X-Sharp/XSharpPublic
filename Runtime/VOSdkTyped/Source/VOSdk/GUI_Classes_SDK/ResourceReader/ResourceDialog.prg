// ResourceDialog.prg
// This class contains the code to read a native dialog resource
// The dialog is also created and destroyed to read the 'real' positions of the controls
// The structures are copied from the WIN32 API Lib




#USING System.Runtime.InteropServices
#USING System.Text
#USING System.Diagnostics
#USING System.Windows.Forms.VisualStyles
#USING System.Windows.Forms
CLASS ResourceDialog INHERIT ResourceReader
	EXPORT Caption		AS STRING
	EXPORT FontName		AS STRING
	EXPORT FontPitch	AS WORD
	EXPORT FontWeight	AS WORD
	EXPORT FontItalic   AS BYTE
	EXPORT FontCharSet   AS BYTE
	EXPORT ClassName	AS STRING
	EXPORT MenuName		AS STRING
	EXPORT ClassID		AS WORD
	EXPORT MenuID		AS LONG
	PROTECT oOwner		 AS OBJECT
	PROTECT X            AS LONG		// Dialog box Units
	PROTECT Y            AS LONG
	PROTECT CX           AS LONG		// Dialog box Units
	PROTECT CY           AS LONG		// Dialog box Units
	EXPORT Style        AS INT
	EXPORT ExStyle      AS INT
	EXPORT ItemCnt		AS LONG
	EXPORT HelpID		AS DWORD	
	EXPORT Controls     AS System.Collections.Generic.List<ResourceDialogItem>
	EXPORT IsValid		AS LOGIC

#region Properties
	ACCESS Size as System.Drawing.Size
		RETURN System.Drawing.Size{CX,CY}
		
	ACCESS Location as System.Drawing.Point
		RETURN System.Drawing.Point{X,Y}
		
	ACCESS Font as System.Drawing.Font
		LOCAL oFont AS System.Drawing.Font
		LOCAL nStyle AS System.Drawing.FontStyle
		nStyle := System.Drawing.FontStyle.Regular
		IF SELF:FontItalic != 0
			nStyle := System.Drawing.FontStyle.Italic
		ENDIF
		IF SELF:FontWeight > 0
			nStyle += System.Drawing.FontStyle.Bold
		ENDIF
		oFont := System.Drawing.Font{SELF:FontName, SELF:FontPitch, nStyle}
		RETURN oFont
#endregion


	METHOD __AdjustSizes(hDLL AS IntPtr, sName AS STRING) AS VOID
		// Create the window "the old fashioned way" and adjust the sizes from the window
		LOCAL hWnd AS IntPtr
		LOCAL hOwner AS IntPtr
		IF SELF:IsValid
			IF oOwner IS Window VAR oWindow
				DO WHILE oWindow:Owner IS Window
					oWindow := oWindow:Owner
				ENDDO
				hOwner := oWindow:Handle()
				
			ELSE
				hOwner := IntPtr.Zero
			ENDIF
			hWnd := Win32.CreateDialogParam(hDLL, sName, hOwner, IntPtr.Zero, IntPtr.Zero)
			IF hWnd != IntPtr.Zero 
				LOCAL oRect := WINRECT{} AS WINRECT
				LOCAL oPoint := WINPOINT{} AS WINPOINT
				LOCAL sb AS StringBuilder
				LOCAL hFont AS IntPtr
				LOCAL oFont AS System.Drawing.Font
				LOCAL lOk AS LOGIC
				sb := StringBuilder{256}
				IF Win32.GetClassName(hWnd, sb, sb:Capacity) > 0
					SELF:ClassName := sb:ToString()
				ENDIF
				hFont := (IntPtr) Win32.SendMessage(hWnd, WM_GETFONT,0,0)
				TRY
					IF hFont != NULL
						oFont := System.Drawing.Font.FromHfont(hFont)
						SELF:FontName	 := oFont:Name
						SELF:FontItalic	 := iif(oFont:Italic,1,0)
						SELF:FontPitch   := (WORD) oFont:SizeInPoints
						SELF:FontCharSet := oFont:GdiCharSet
					ENDIF
				CATCH  AS Exception
				END TRY
				SELF:Style	 := Win32.GetWindowLong(hWnd, GWL_STYLE)
				SELF:ExStyle := Win32.GetWindowLong(hWnd, GWL_EXSTYLE)
				lOk := Win32.GetWindowRect(hWnd, REF oRect)
				IF lOk
					SELF:X := oRect:left
					SELF:Y := oRect:top
					SELF:CX := oRect:right-oRect:left
					SELF:CY := oRect:bottom-oRect:top
				ENDIF
				FOREACH IMPLIED oControl IN SELF:Controls
					LOCAL hItem AS IntPtr
					hItem	:= Win32.GetDlgItem(hWnd, oControl:ControlID)
					IF hItem != IntPtr.Zero
						lOk		           := Win32.GetWindowRect(hItem, REF oRect)
						IF lOk
							oControl:CX        := oRect:right - oRect:left
							IF !oControl:IsComboBox
								// Do not reset the size for combo boxes
								oControl:CY        := oRect:bottom - oRect:top
							ENDIF
							oPoint:x           := oRect:left
							oPoint:y           := oRect:top
							lOk                := Win32.ScreenToClient(hWnd, REF oPoint)
							IF lOk
								oControl:X         := oPoint:x
								oControl:Y         := oPoint:y
							ENDIF
						ENDIF
						oControl:Style	   := Win32.GetWindowLong(hItem, GWL_STYLE)
						oControl:ExStyle   := Win32.GetWindowLong(hItem, GWL_EXSTYLE)
						IF Win32.GetClassName(hItem, sb, sb:Capacity) > 0
							oControl:ClassName := sb:ToString()
						ENDIF
						//TRY
						//	hFont              := (IntPtr) Win32.SendMessage(hItem, WM_GETFONT,0,0)
						//	IF hFont != NULL
						//		oFont := System.Drawing.Font.FromHfont(hFont)
						//	ENDIF
						//END TRY
					ENDIF
				NEXT
				lOk  := Win32.DestroyWindow(hWnd)
			ENDIF
		ENDIF
		RETURN 

	METHOD __LoadFromResource(hDLL AS IntPtr, hResInfo AS IntPtr) AS LOGIC
		LOCAL lpBuffer AS IntPtr
		LOCAL uiResSize AS DWORD
		LOCAL hResource AS IntPtr
		Controls := System.Collections.Generic.List<ResourceDialogItem>{}
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
		
				
	CONSTRUCTOR(hDLL AS IntPtr, cName AS STRING, Owner AS OBJECT)
		LOCAL hResInfo AS IntPtr
		SUPER()
		oOwner   := Owner
		hResInfo := Win32.FindResource(hDLL, cName, 5)
		IF SELF:__LoadFromResource(hDLL, hResInfo)
			SELF:__AdjustSizes(hDLL, cName)
		ENDIF
		RETURN 



	METHOD GetDlgItem(nItem as LONG) AS ResourceDialogItem
		FOREACH IMPLIED oControl in SELF:Controls
			IF oControl:ControlID == nItem
				RETURN oControl
			ENDIF
		NEXT
		RETURN NULL_OBJECT
		
	METHOD CopyCreateParams(params AS System.Windows.Forms.CreateParams, lIsDialog AS LOGIC ) AS VOID
		IF Slen(SELF:ClassName) > 0
			params:ClassName := SELF:ClassName
		ENDIF
		params:Caption	:= SELF:Caption
		IF lIsDialog
			params:X		:= SELF:X 
			params:Y		:= SELF:Y
			params:Width	:= SELF:CX
			params:Height	:= SELF:CY
		ENDIF
		RETURN
		
		
		
#region Read from Native Resource	
	INTERNAL METHOD ReadData(lpBuffer AS IntPtr) AS VOID
		LOCAL pDialogEx	AS winDLGTEMPLATEEX

		pDialogEx	:= (winDLGTEMPLATEEX PTR) lpBuffer
		IF pDialogEx:dlgVer == 01 .and. pDialogEx:signature == 0XFFFF
			// this is indeed a DialogEx
			SELF:ReadDlgEx(pDialogEx)
		ELSE
			SELF:ReadDlg( (winDLGTEMPLATE PTR ) lpBuffer)
		ENDIF
		RETURN 

	INTERNAL METHOD ReadDlgEx(pDialogEx AS winDLGTEMPLATEEX) AS VOID
		LOCAL pWord AS WORD PTR
		LOCAL nItem AS DWORD
		SELF:Style		:= pDialogEx:style
		SELF:ExStyle	:= pDialogEx:exStyle
		SELF:HelpID		:= pDialogEx:helpID
		SELF:X			:= pDialogEx:x
		SELF:Y			:= pDialogEx:y
		SELF:CX			:= pDialogEx:cx
		SELF:CY			:= pDialogEx:cy
		SELF:ItemCnt    := pDialogEx:nItems	
		pWord			:= @pDialogEx:menu
		pWord := ReadExtraInfo(pWord,TRUE)
		
		//	sz_Or_Ord menu;			// name or ordinal of a menu resource
		//	sz_Or_Ord windowClass;	// name or ordinal of a window class
		//	WCHAR title[titleLen];	// title string of the dialog box
		//	short pointsize;		// only if DS_SETFONT flag is set
		//	short weight;			// only if DS_SETFONT flag is set 0-1000
		//	byte bItalic;			// only if DS_SETFONT flag is set 0 - 1
		//	byte bCharset;			// only if DS_SETFONT flag is set 
		//	WCHAR font[fontLen];	// typeface name, if DS_SETFONT is set 
		// } DLGTEMPLATEEX;

		// Read Menu
		FOR nItem := 1 TO SELF:ItemCnt
			LOCAL IMPLIED oItem := ResourceDialogItem{}
			SELF:Controls:Add(oItem)
			pWord := oItem:ReadEx(pWord)
			oItem:TabIndex := (LONG) nItem

		NEXT
		RETURN


	INTERNAL METHOD ReadDlg(pDialog AS winDLGTEMPLATE) AS VOID
		LOCAL nItem AS DWORD
		LOCAL pWord AS WORD PTR
		SELF:Style		:= pDialog:style
		SELF:ExStyle	:= pDialog:exStyle
		SELF:X			:= pDialog:x
		SELF:Y			:= pDialog:y
		SELF:CX			:= pDialog:cx
		SELF:CY			:= pDialog:cy
		SELF:ItemCnt    := pDialog:nItems	
		pWord			:= (WORD PTR) @pDialog:cy
		pWord			+= 1
		pWord := ReadExtraInfo(pWord,FALSE)
		FOR nItem := 1 TO SELF:ItemCnt
			LOCAL IMPLIED oItem := ResourceDialogItem{}
			SELF:Controls:Add(oItem)
			pWord := oItem:Read(pWord)
			oItem:TabIndex := (LONG) nItem
		NEXT
		RETURN 
		
	METHOD ReadExtraInfo(pWord as WORD PTR, lDialogEx as LOGIC) AS WORD PTR
		LOCAL pByte AS BYTE PTR
		LOCAL nId AS WORD, sName AS STRING
		pWord			:= ReadIdAndText(pWord, REF nId, REF sName)
		SELF:MenuID		:= nId
		SELF:MenuName	:= sName
		// Read Class
		pWord			:= ReadIdAndText(pWord, REF nId, REF sName)
		SELF:ClassID	:= nId
		SELF:ClassName	:= sName
		// Read Caption
		SELF:Caption := ReadText(pWord)
		pWord		 += SELF:Caption:Length+1
		IF _AND(SELF:Style, DS_SETFONT) == DS_SETFONT
			SELF:FontPitch	:= pWord[1]
			pWord += 1
			IF lDialogEx
				SELF:FontWeight := pWord[1]
				pWord += 1
				pByte := (BYTE PTR) pWord
				SELF:FontItalic := pByte[1]
				SELF:FontCharSet := pByte[2]
				pWord += 1
			ENDIF
			SELF:FontName := ReadText(pWord)
			pWord += SELF:FontName:Length+1
		ENDIF
		RETURN pWord
		
	METHOD ToString() AS STRING STRICT
		LOCAL cResult AS STRING
		cResult := SELF:Caption
		FOREACH IMPLIED item IN Controls
			IF !String.IsNullOrEmpty(item:ClassName)
				cResult += e"\n"+ item:ControlID:ToString()+" "+item:ClassName+" "+item:Caption
			ELSE
				cResult += e"\n"+ item:ControlID:ToString()+" "+item:ClassID:ToString("X")+" "+item:Caption
			ENDIF
		NEXT
		RETURN cResult
#endregion

END CLASS
	
		
	
INTERNAL VOSTRUCT winDLGTEMPLATE ALIGN 2
	MEMBER style AS INT
	MEMBER exStyle AS INT
	MEMBER nItems AS WORD
	MEMBER x AS SHORTINT
	MEMBER y AS SHORTINT
	MEMBER cx AS SHORTINT
	MEMBER cy AS SHORTINT
	//	sz_Or_Ord menu;			// name or ordinal of a menu resource
	//	sz_Or_Ord windowClass;	// name or ordinal of a window class
	//	WCHAR title[titleLen];	// title string of the dialog box
	//	short pointsize;		// only if DS_SETFONT flag is set
	//	WCHAR font[fontLen];	// typeface name, if DS_SETFONT is set 
	// } DLGTEMPLATEEX;
	

INTERNAL VOSTRUCT winDLGTEMPLATEEX ALIGN 2
	MEMBER dlgVer AS WORD
	MEMBER signature AS WORD
	MEMBER helpID AS DWORD
	MEMBER exStyle AS INT
	MEMBER style AS INT
	MEMBER nItems AS WORD
	MEMBER x AS SHORTINT
	MEMBER y AS SHORTINT
	MEMBER cx AS SHORTINT
	MEMBER cy AS SHORTINT
	// additional members start here but may have variable length
	MEMBER menu AS WORD
	//	sz_Or_Ord menu;			// name or ordinal of a menu resource
	//	sz_Or_Ord windowClass;	// name or ordinal of a window class
	//	WCHAR title[titleLen];	// title string of the dialog box
	//	short pointsize;		// only if DS_SETFONT flag is set
	//	short weight;			// only if DS_SETFONT flag is set 0-1000
	//	byte bItalic;			// only if DS_SETFONT flag is set 0 - 1
	//	byte bCharset;			// only if DS_SETFONT flag is set 
	//	WCHAR font[fontLen];	// typeface name, if DS_SETFONT is set 
	// } DLGTEMPLATEEX;
	


