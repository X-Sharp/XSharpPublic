VOSTRUCT __WCDropFiles
	// Liuho01@05/09/96: Undocumented data structure necessary for creating
	//							a win32 Drop File Source application
	//PP-031006 Bug 98. Changed wSize to DWORD as per Platform SDK
	MEMBER wSize AS DWORD				// Size of data structure
//	MEMBER wSize AS WORD				// Size of data structure
	MEMBER ptMousePos IS _winPoint		//Position of mouse in the
	
	MEMBER fInNonClientArea AS LOGIC	//window's non-client area
	MEMBER fUnicode AS LOGIC			// are the pathnames in Unicaode?

VOSTRUCT SelfPtr
	MEMBER ptrSelf AS PTR

	//structure __WCDCItem
	//member oObject as ptr
	//member ptrNext as __WCDCItem

VOSTRUCT strColor
	MEMBER s1 AS DWORD
	MEMBER s2 AS DWORD
	MEMBER s3 AS DWORD
	MEMBER s4 AS DWORD
	MEMBER s5 AS DWORD
	MEMBER s6 AS DWORD
	MEMBER s7 AS DWORD
	MEMBER s8 AS DWORD
	MEMBER s9 AS DWORD
	MEMBER s10 AS DWORD
	MEMBER s11 AS DWORD
	MEMBER s12 AS DWORD
	MEMBER s13 AS DWORD
	MEMBER s14 AS DWORD
	MEMBER s15 AS DWORD
	MEMBER s16 AS DWORD
VOSTRUCT WCColor
	MEMBER bBlue 	AS BYTE
	MEMBER bGreen 	AS BYTE
	MEMBER bRed 	AS BYTE
	MEMBER bNotUsed AS BYTE

VOSTRUCT __WCDialog_VARS
	//LIUHO01@01/05/96:		Used to Buffer Data which will be passed to __SetChildFontProc
	//						CallBack function
	MEMBER hFont AS PTR // handle to the new font
	MEMBER hDlg AS PTR // Window handle of dialog box
	MEMBER Xbase AS LONGINT // original base X
	MEMBER Ybase AS LONGINT // original base Y
	MEMBER fontX AS LONGINT // font width
	MEMBER fontY AS LONGINT // font height

/*
textblock STRUCTURE __WCToolBarUpdate
	// PRAAN02@12/05/95:	Used to buffer ToolBar changes if the ToolBar
	//						has not yet been created
	MEMBER symAction	 AS SYMBOL
	MEMBER nButtonID	 AS LONGINT
	MEMBER nMenuItemID AS LONGINT
	MEMBER nBeforeID	 AS LONGINT
	// RvdH 031015 Changed oBitMap to ptrBitMap
	//	MEMBER oBitmap		 AS OBJECT
	MEMBER ptrBitMap		 as PTR
	MEMBER nPosition	 AS DWORD
	MEMBER pszButtonText AS PSZ
	MEMBER nImageCount AS DWORD
	MEMBER bState		 AS BYTE
	MEMBER bStyle		 AS BYTE
	MEMBER symToolbar AS SYMBOL


ENDTEXT
*/