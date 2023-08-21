VOSTRUCT _winNMMOUSE ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER dwItemSpec AS DWORD
	MEMBER dwItemData AS DWORD
	MEMBER pt IS _winPOINT
	MEMBER dwHitInfo AS DWORD

VOSTRUCT _winNMKEY ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER nVKey AS DWORD
	MEMBER uFlags AS DWORD

VOSTRUCT _winNMCHAR  ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER ch AS DWORD
	MEMBER dwItemPrev AS DWORD
	MEMBER dwItemNext AS DWORD



VOSTRUCT _winNMTTCUSTOMDRAW ALIGN 1
	MEMBER nmcd IS _winNMCUSTOMDRAW
	MEMBER uDrawFlags AS DWORD


VOSTRUCT _winIMAGEINFO ALIGN 1
	MEMBER	hbmImage AS PTR
	MEMBER	hbmMask AS PTR
	MEMBER	Unused1 AS INT
	MEMBER	Unused2 AS INT
	MEMBER	rcImage IS _winRECT


VOSTRUCT _winHD_ITEM ALIGN 1
	MEMBER	 mask AS DWORD
	MEMBER	 cxy AS INT
	MEMBER	 pszText AS PSZ
	MEMBER	 hbm AS PTR
	MEMBER	 cchTextMax AS INT
	MEMBER   fmt AS INT
	MEMBER	 lParam AS LONGINT
	MEMBER   iImage AS INT
	MEMBER   iOrder AS INT
	MEMBER 	 type AS DWORD           // [in] filter type (defined what pvFilter is a pointer to)
	MEMBER   pvFilter AS PTR       // [in] fillter data see above



VOSTRUCT _winHD_LAYOUT ALIGN 1
	MEMBER prc   AS _winRECT
	MEMBER pwpos AS _winWINDOWPOS



VOSTRUCT _winHD_HITTESTINFO ALIGN 1
	MEMBER pt IS _winPOINT
	MEMBER flags AS DWORD
	MEMBER iItem AS INT

VOSTRUCT _winHD_NOTIFY ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER iItem AS INT
	MEMBER iButton AS INT
	MEMBER	pitem AS _winHD_ITEM


VOSTRUCT _winTBBUTTON  ALIGN 1
	MEMBER iBitmap AS INT
	MEMBER idCommand AS INT
	MEMBER fsState AS BYTE
	MEMBER fsStyle AS BYTE
	MEMBER DIM bReserved[2] AS BYTE
	MEMBER dwData AS DWORD
	MEMBER iString AS INT

VOSTRUCT _winCOLORMAP  ALIGN 1
	MEMBER FROM AS DWORD
	MEMBER _to AS DWORD


VOSTRUCT _winTBADDBITMAP ALIGN 1
	MEMBER	hInst AS PTR
	MEMBER	nID AS DWORD

VOSTRUCT _winTBSAVEPARAMS  ALIGN 1
	MEMBER hkr AS PTR
	MEMBER pszSubKey AS PSZ
	MEMBER pszValueName AS PSZ



VOSTRUCT _winTBREPLACEBITMAP ALIGN 1
	MEMBER	hInstOld AS PTR
	MEMBER nIDOld AS DWORD
	MEMBER hInstNew AS PTR
	MEMBER nIDNew AS DWORD
	MEMBER nButtons AS INT


VOSTRUCT _winTBNOTIFY ALIGN 1
	MEMBER	hdr IS _winNMHDR
	MEMBER	iItem AS INT
	MEMBER	tbButton IS _winTBBUTTON
	MEMBER	cchText AS INT
	MEMBER	pszText AS PSZ   
	//RvdH 070412
	MEMBER	rcButton IS _WINRECT

VOSTRUCT _winTT_HITTESTINFO ALIGN 1
	MEMBER hwnd AS PTR
	MEMBER pt IS _winPOINT
	MEMBER ti IS _winTOOLINFO



VOSTRUCT _winDRAGLISTINFO  ALIGN 1
	MEMBER uNotification AS DWORD
	MEMBER hWnd AS PTR
	MEMBER ptCursor IS _winPOINT

VOSTRUCT _winUDACCEL	ALIGN 1
	MEMBER nSec AS DWORD
	MEMBER nInc AS DWORD

VOSTRUCT _winNM_UPDOWN ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER iPos AS INT
	MEMBER iDelta AS INT

VOSTRUCT _winLV_FINDINFO ALIGN 1
	MEMBER	flags AS DWORD
	MEMBER	_psz AS PSZ
	MEMBER	lParam AS LONGINT
	MEMBER	pt IS _winPOINT
	MEMBER	vkDirection AS DWORD



VOSTRUCT _winLV_HITTESTINFO ALIGN 1
	MEMBER pt IS _winPOINT
	MEMBER flags AS DWORD
	MEMBER iItem AS INT
	MEMBER iSubItem AS INT // 4.70

VOSTRUCT _winLV_COLUMN ALIGN 1
	MEMBER mask AS DWORD
	MEMBER fmt AS INT
	MEMBER cx AS INT
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iSubItem AS INT
	MEMBER iImage AS INT
	MEMBER iOrder AS INT




VOSTRUCT _winNM_LISTVIEW ALIGN 1
	MEMBER   hdr IS _winNMHDR
	MEMBER   iItem AS INT
	MEMBER   iSubItem AS INT
	MEMBER   uNewState AS DWORD
	MEMBER   uOldState AS DWORD
	MEMBER	uChanged AS DWORD
	MEMBER	ptAction IS _winPOINT
	MEMBER	lParam AS LONGINT

VOSTRUCT _winNMLVCUSTOMDRAW ALIGN 1
	MEMBER nmcd IS _winNMCUSTOMDRAW
	MEMBER clrText AS DWORD
	MEMBER clrTextBk AS DWORD
	MEMBER iSubItem AS INT
	//RvdH 070412 Added
	MEMBER dwItemType AS DWORD
	// Item custom draw
	MEMBER clrFace AS DWORD
	MEMBER iIconEffect AS INT
	MEMBER iIconPhase AS INT
	MEMBER iPartId AS INT
	MEMBER iStateId AS INT

	// Group Custom Draw 
	MEMBER rcText IS _WINRECT
	MEMBER uAlign AS DWORD// Alignment. Use LVGA_HEADER_CENTER, LVGA_HEADER_RIGHT, LVGA_HEADER_LEFT
VOSTRUCT _winNMLVCACHEHINT ALIGN 1
	MEMBER  hdr IS _winNMHDR
	MEMBER  iFrom AS INT
	MEMBER  iTo AS INT

VOSTRUCT _winNMLVFINDITEM ALIGN 1
	MEMBER  hdr IS _winNMHDR
	MEMBER  iStart AS INT
	MEMBER  lvfi IS _winLV_FINDINFO

VOSTRUCT _winNMLVODSTATECHANGE ALIGN 1
	MEMBER  hdr IS _winNMHDR
	MEMBER  iFrom AS INT
	MEMBER  iTo   AS INT
	MEMBER  uNewState AS DWORD
	MEMBER  uOldState AS DWORD


VOSTRUCT _winLV_DISPINFO ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER item IS _winLV_ITEM



VOSTRUCT _winLV_KEYDOWN  ALIGN 1
	MEMBER hdr	 IS _winNMHDR 
	MEMBER wVKey AS WORD
	MEMBER flags AS DWORD  


VOSTRUCT _winTV_ITEM      ALIGN 1
	MEMBER	 mask AS DWORD
	MEMBER hItem AS PTR
	MEMBER state AS DWORD
	MEMBER stateMask AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER iSelectedImage AS INT
	MEMBER cChildren AS INT
	MEMBER lParam AS LONGINT



VOSTRUCT _winTV_INSERTSTRUCT      ALIGN 1
	MEMBER hParent AS PTR
	MEMBER hInsertAfter AS PTR
	MEMBER u IS _winTV_ITEMUNION

VOSTRUCT _winTV_HITTESTINFO  ALIGN 1
	MEMBER	 pt IS _winPOINT
	MEMBER	 flags AS DWORD
	MEMBER	 hItem AS PTR

VOSTRUCT _winTV_SORTCB ALIGN 1
	MEMBER	hParent AS PTR
	MEMBER	lpfnCompare AS PTR
	MEMBER	lParam AS LONGINT


VOSTRUCT _winNM_TREEVIEW  ALIGN 1
	MEMBER	hdr IS _winNMHDR
	MEMBER	action AS DWORD
	MEMBER	itemOld IS _winTV_ITEM
	MEMBER	itemNew IS _winTV_ITEM
	MEMBER	ptDrag IS _winPOINT



VOSTRUCT _winTV_DISPINFO  ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER item IS _winTV_ITEM




VOSTRUCT _winTV_KEYDOWN  ALIGN 1 
	MEMBER hdr 	IS _winNMHDR
	MEMBER wVKey AS WORD
	MEMBER flags AS DWORD


VOSTRUCT _winTC_ITEMHEADER ALIGN 1
	MEMBER mask AS DWORD
	MEMBER	lpReserved1 AS DWORD
	MEMBER	lpReserved2 AS DWORD
	MEMBER	pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT



VOSTRUCT _winTC_ITEM ALIGN 1
	MEMBER mask AS DWORD
	MEMBER lpReserved1 AS DWORD
	MEMBER lpReserved2 AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER lParam AS LONGINT





VOSTRUCT _winTC_HITTESTINFO ALIGN 1
	MEMBER pt IS _winPOINT
	MEMBER flags AS DWORD


VOSTRUCT _winTC_KEYDOWN ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER wVKey AS WORD
	MEMBER flags AS DWORD

VOSTRUCT _winINITCOMMONCONTROLSEX
	MEMBER dwSize AS DWORD
	MEMBER dwICC AS DWORD

VOSTRUCT _winMCHITTESTINFO  ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER pt IS _winPOINT
	MEMBER uHit AS DWORD
	MEMBER st IS _winSYSTEMTIME

VOSTRUCT _winNMSELCHANGE ALIGN 1
	MEMBER nmhdr IS _winNMHDR
	MEMBER stSelStart IS _winSYSTEMTIME
	MEMBER stSelEnd   IS _winSYSTEMTIME

VOSTRUCT _winNMDAYSTATE ALIGN 1
	MEMBER nmhdr IS _winNMHDR
	MEMBER stStart IS _winSYSTEMTIME
	MEMBER cDayState AS INT
	MEMBER prgDayState AS DWORD PTR

VOSTRUCT _winNMDATETIMECHANGE ALIGN 1
	MEMBER nmhdr 		IS _winNMHDR
	MEMBER dwFlags 	AS DWORD
	MEMBER st 			IS _winSYSTEMTIME

VOSTRUCT _winNMDATETIMEWMKEYDOWN ALIGN 1
	MEMBER nmhdr 		IS _winNMHDR
	MEMBER nVirtKey 	AS INT  // virtual key code of WM_KEYDOWN which MODIFIES an X field
	MEMBER pszFormat 	AS PSZ // format substring
	MEMBER st 			IS  _winSYSTEMTIME         // current systemtime, app should modify based on key

VOSTRUCT _winNMDATETIMEFORMAT ALIGN 1
	MEMBER nmhdr 		IS _winNMHDR
	MEMBER pszFormat 	AS PSZ   // format substring
	MEMBER st 			IS _winSYSTEMTIME     // current systemtime
	MEMBER pszDisplay	AS PSZ   // string to display
	MEMBER DIM szDisplay[64] AS BYTE  // buffer pszDisplay originally points at

VOSTRUCT _winNMDATETIMEFORMATQUERY ALIGN 1
	MEMBER nmhdr IS _winNMHDR
	MEMBER pszFormat AS PSZ   // format substring
	MEMBER szMax IS _winSIZE  // max bounding rectangle app will use for this format string

VOSTRUCT _winREBARINFO ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER fMask  AS DWORD
	MEMBER himl   AS PTR

VOSTRUCT _winNMIPADDRESS  ALIGN 1
	MEMBER hdr    IS _winNMHDR
	MEMBER iField AS INT
	MEMBER iValue AS INT

	////////////////////  ComboBoxEx ////////////////////////////////
VOSTRUCT _winNMCOMBOBOXEX  ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER ceItem IS  _winCOMBOBOXEXITEM

VOSTRUCT _winNMCBEDRAGBEGIN ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER iItemid AS INT
	MEMBER DIM szText[CBEMAXSTRLEN] AS BYTE
	// fChanged if the user actually did anything
	// iNewSelection gives what would be the new selection unless the notify is failed
	//                      iNewSelection may be CB_ERR if there's no match
VOSTRUCT _winNMCBEENDEDIT ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER fChanged AS LOGIC
	MEMBER iNewSelection AS INT
	MEMBER DIM szText[CBEMAXSTRLEN] AS BYTE
	MEMBER iWhy AS INT

	// new constants

VOSTRUCT _winLITEM //ALIGN 1 RvdH 070411 removed alignment
	MEMBER mask AS DWORD
	MEMBER iLink AS INT
	MEMBER state AS DWORD
	MEMBER stateMask AS DWORD
	MEMBER DIM szID[MAX_LINKID_TEXT] AS WORD
	MEMBER DIM szUrl[L_MAX_URL_LENGTH] AS WORD

VOSTRUCT _winLHITTESTINFO ALIGN 1
	MEMBER pt IS _winPOINT
	MEMBER item IS _winLITEM 

VOSTRUCT _winLVGROUP ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER mask AS DWORD
	MEMBER pHeader AS PTR
	MEMBER cchHeader AS INT
	MEMBER pFooter AS PTR
	MEMBER cchFooter AS INT
	MEMBER iGroupId AS INT
	MEMBER stateMask AS DWORD
	MEMBER state AS DWORD
	MEMBER uAlign AS DWORD

VOSTRUCT _winLVGROUPMETRICS ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER mask AS DWORD
	MEMBER Left AS DWORD
	MEMBER Top AS DWORD
	MEMBER Right AS DWORD
	MEMBER Bottom AS DWORD
	MEMBER crLeft AS DWORD
	MEMBER crTop AS DWORD
	MEMBER crRight AS DWORD
	MEMBER crBottom AS DWORD
	MEMBER crHeader AS DWORD
	MEMBER crFooter AS DWORD

VOSTRUCT _winLVTILEVIEWINFO
	MEMBER cbSize AS DWORD
	MEMBER dwMask AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER sizeTile IS _winSIZE
	MEMBER cLines AS INT
	MEMBER rcLabelMargin IS _winRECT

VOSTRUCT _winNMTBHOTITEM ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER idOld AS INT
	MEMBER idNew AS INT
	MEMBER dwFlags AS DWORD // HICF_*

	// Hot item change flags
VOSTRUCT _winNMTBDISPINFO ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER dwMask AS DWORD  // [in] Specifies the values requested .[out] Client ask the data to be set for future use
	MEMBER idCommand AS INT // [in] id of button we're requesting info for
	MEMBER lParam AS DWORD  // [in] lParam of button
	MEMBER iImage AS INT    // [out] image index
	MEMBER pszText AS PSZ   // [out] new text for item
	MEMBER cchText AS INT   // [in] size of buffer pointed to by pszText

	// Return codes for TBN_DROPDOWN
VOSTRUCT _winNMTOOLBAR ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER iItem AS INT
	MEMBER tbButton IS _winTBBUTTON
	MEMBER cchText AS INT
	MEMBER pszText AS PSZ 
	//RvdH 070412 Added
	MEMBER rcButton IS _WINRECT
	

	//*************************************        "ReBarWindow32"

VOSTRUCT _winNMREBARCHILDSIZE ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER uBand AS DWORD
	MEMBER wID AS DWORD
	MEMBER rcChild IS _winRECT
	MEMBER rcBand IS _winRECT

VOSTRUCT _winNMITEMACTIVATE ALIGN 1
	MEMBER   hdr IS _winNMHDR
	MEMBER   iItem AS INT
	MEMBER   iSubItem AS INT
	MEMBER   uNewState AS DWORD
	MEMBER   uOldState AS DWORD
	MEMBER	 uChanged AS DWORD
	MEMBER   ptAction IS _winPOINT
	MEMBER	 lParam AS LONGINT
	MEMBER	 uKeyFlags AS DWORD

	// key flags stored in uKeyFlags
VOSTRUCT _winTV_ITEMEX ALIGN 1
	MEMBER mask AS DWORD
	MEMBER hItem AS PTR
	MEMBER state AS DWORD
	MEMBER stateMask AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER iSelectedImage AS INT
	MEMBER cChildren AS INT
	MEMBER lParam AS LONGINT
	MEMBER iIntegral AS INT



VOSTRUCT _winNMTVCUSTOMDRAW ALIGN 1
	MEMBER nmcd IS _winNMCUSTOMDRAW
	MEMBER clrText AS DWORD
	MEMBER clrTextBk AS DWORD
	MEMBER iLevel AS INT

	//*************************************        "SysTabControl32"

VOSTRUCT _winPROPSHEETHEADER ALIGN 1
	MEMBER dwSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER hwndParent AS PTR
	MEMBER hInstance AS PTR
	MEMBER pszIcon AS PSZ
	MEMBER pszCaption AS PSZ
	MEMBER nPages AS DWORD
	MEMBER nStartPage AS DWORD
	MEMBER ppsp AS PTR
	MEMBER pfnCallback AS PTR
	MEMBER pszbmWatermark AS PSZ
	MEMBER hplWatermark AS PTR
	MEMBER pszbmHeader AS PSZ

VOSTRUCT _winTRACKMOUSEEVENT ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER hwndTrack AS PTR
	MEMBER dwHoverTime AS DWORD

VOSTRUCT _winNMCUSTOMDRAW ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER dwDrawStage AS DWORD
	MEMBER hdc AS PTR
	MEMBER rc IS _winRECT
	MEMBER dwItemSpec AS DWORD
	MEMBER uItemState AS DWORD
	MEMBER lItemlParam AS LONGINT

VOSTRUCT _winTBINSERTMARK ALIGN 1
	MEMBER iButton AS LONGINT
	MEMBER dwFlags AS DWORD


VOSTRUCT _winTBBUTTONINFO ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER dwMask AS DWORD
	MEMBER idCommand AS LONGINT
	MEMBER iImage	AS LONGINT
	MEMBER fsState AS BYTE
	MEMBER fsStyle AS BYTE
	MEMBER cx AS WORD
	MEMBER lParam AS DWORD PTR
	MEMBER pszText AS PSZ
	MEMBER cchText AS LONGINT

VOSTRUCT _winTBMETRICS ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER dwMask AS DWORD
	MEMBER cxPad  AS LONGINT        // PA
	MEMBER cyPad	AS LONGINT
	MEMBER cxBarPad AS LONGINT     // BARPAD
	MEMBER cyBarPad AS LONGINT
	MEMBER cxButtonSpacing AS LONGINT   // BUTTONSPACING
	MEMBER cyButtonSpacing AS LONGINT


VOSTRUCT _winTOOLINFO ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER uFlags AS DWORD
	MEMBER hwnd AS PTR
	MEMBER uId AS DWORD
	MEMBER rect IS _winRECT
	MEMBER hinst AS PTR
	MEMBER lpszText AS PSZ
	//PP-030909
	MEMBER lParam AS LONGINT     
	//RvdH 070412
	MEMBER lpReserved AS PTR

VOSTRUCT _winTOOLTIPTEXT ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER lpszText AS PSZ
	MEMBER DIM szText[80] AS BYTE
	MEMBER hinst AS PTR
	MEMBER uFlags AS DWORD
	//RvdH 070412 Added  
	MEMBER lParam AS LONGINT







VOSTRUCT _winLV_ITEM ALIGN 1
	MEMBER mask AS DWORD
	MEMBER iItem AS INT
	MEMBER iSubItem AS INT
	MEMBER state AS DWORD
	MEMBER stateMask AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER lParam AS LONGINT
	MEMBER iIndent AS INT
	//RvdH 070412 Added
	MEMBER iGroupId AS INT
	MEMBER cColumns AS DWORD // tile view columns
	MEMBER puColumns AS DWORD PTR 



VOSTRUCT _winREBARBANDINFO ALIGN 1
	MEMBER cbSize AS DWORD
	MEMBER fMask AS DWORD
	MEMBER fStyle AS DWORD
	MEMBER clrFore AS DWORD
	MEMBER clrBack AS DWORD
	MEMBER lpText AS PSZ
	MEMBER cch AS DWORD
	MEMBER iImage AS INT
	MEMBER hwndChild AS PTR
	MEMBER cxMinChild AS DWORD
	MEMBER cyMinChild AS DWORD
	MEMBER cx AS DWORD
	MEMBER hbmBack AS PTR
	MEMBER wID AS DWORD
	MEMBER cyChild AS DWORD
	MEMBER cyMaxChild AS DWORD
	MEMBER cyIntegral AS DWORD
	MEMBER cxIdeal AS DWORD
	MEMBER lParam AS LONGINT
	MEMBER cxHeader AS DWORD

VOSTRUCT _winCOMBOBOXEXITEM ALIGN 1
	MEMBER mask AS DWORD
	MEMBER iItem AS INT
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER iSelectedImage AS INT
	MEMBER iOverlay AS INT
	MEMBER iIndent AS INT
	MEMBER lParam AS LONGINT

VOSTRUCT _winNMLINK ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER item IS _winLITEM

	//  SysLink notifications
	//  NM_CLICK   // wParam: control ID, lParam: PNMLINK, ret: ignored.

	//  LinkWindow messages
VOSTRUCT	_winLVITEM6 ALIGN 1
	MEMBER mask AS DWORD
	MEMBER iItem AS INT
	MEMBER iSubItem AS INT
	MEMBER state AS DWORD
	MEMBER stateMask AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iImage AS INT
	MEMBER lParam AS LONGINT
	MEMBER iIndent AS INT
	MEMBER iGroupId AS INT
	MEMBER cColumns AS DWORD
	MEMBER puColumns AS PTR

VOSTRUCT _winHDITEM ALIGN 1
	MEMBER mask AS DWORD
	MEMBER cxy AS INT
	MEMBER pszText AS PSZ
	MEMBER hbm AS PTR
	MEMBER cchTextMax AS INT
	MEMBER fmt AS INT
	MEMBER lParam AS LONGINT
	MEMBER iImage AS INT
	MEMBER iOrder AS INT
	MEMBER type AS DWORD
	MEMBER pvFilter AS PTR

VOSTRUCT _winLVBKIMAGE
	MEMBER ulFlags AS LONGINT
	MEMBER hbm AS PTR
	MEMBER pszImage AS PSZ
	MEMBER cchImageMax AS LONGINT
	MEMBER xOffsetPercent AS INT
	MEMBER yOffsetPercent AS INT

VOSTRUCT _winNMTBGETINFOTIP ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iItem AS INT
	MEMBER lParam AS LONGINT

VOSTRUCT _winNMREBARCHEVRON ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER uBand AS DWORD
	MEMBER wID AS DWORD
	MEMBER lParam AS LONGINT
	MEMBER rc IS _winRECT
	MEMBER lParamNM AS LONGINT



VOSTRUCT _winNMLVGETINFOTIP ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER dwFlags AS DWORD
	MEMBER pszText AS PSZ
	MEMBER cchTextMax AS INT
	MEMBER iItem AS INT
	MEMBER iSubItem AS INT
	MEMBER lParam AS LONGINT

VOSTRUCT _winPROPSHEETPAGE ALIGN 1
	MEMBER dwSize AS DWORD
	MEMBER dwFlags AS DWORD
	MEMBER hInstance AS PTR
	MEMBER pszTemplate AS PSZ
	MEMBER pszIcon AS PSZ
	MEMBER pszTitle AS PSZ
	MEMBER pfnDlgProc AS PTR
	MEMBER lParam AS LONGINT
	MEMBER pfnCallback AS PTR
	MEMBER pcRefParent AS DWORD PTR
	MEMBER pszHeaderTitle AS PSZ
	MEMBER pszHeaderSubTitle AS PSZ
	//RvdH 070412 Added 
	MEMBER hActCtx	AS PTR

VOSTRUCT _winPSHNOTIFY ALIGN 1
	MEMBER hdr IS _winNMHDR
	MEMBER lParam AS LONGINT

_DLL FUNCTION InitCommonControls() AS VOID PASCAL:COMCTL32.InitCommonControls

_DLL FUNCTION ImageList_Create(cx AS INT, cy AS INT, flags AS DWORD, cInitial AS INT, cGrow AS INT);
		AS PTR PASCAL:COMCTL32.ImageList_Create


_DLL FUNCTION ImageList_Destroy( himl AS PTR) AS LOGIC PASCAL:COMCTL32.ImageList_Destroy


_DLL FUNCTION ImageList_GetImageCount(himi AS PTR) AS LONG PASCAL:COMCTL32.ImageList_GetImageCount


_DLL FUNCTION ImageList_Add(himi AS PTR, hbmImage AS PTR, hbmMask AS PTR);
		AS INT PASCAL:COMCTL32.ImageList_Add


_DLL FUNCTION ImageList_ReplaceIcon( himl AS PTR, I AS INT, hicon AS PTR);
		AS INT PASCAL:COMCTL32.ImageList_ReplaceIcon


_DLL FUNCTION ImageList_SetBkColor( himl AS PTR, clrBK AS DWORD);
		AS DWORD PASCAL:COMCTL32.ImageList_SetBkColor


_DLL FUNCTION ImageList_GetBkColor(himl AS PTR) AS DWORD PASCAL:COMCTL32.ImageList_GetBkColor


_DLL FUNCTION ImageList_SetOverlayImage(himl AS PTR,	iImage AS INT, iOverlay AS INT);
		AS LOGIC PASCAL:COMCTL32.ImageList_SetOverlayImage



FUNCTION ImageList_AddIcon(himl AS PTR, hicon AS PTR) AS INT STRICT
	RETURN ImageList_ReplaceIcon(himl, -1, hicon)

FUNCTION INDEXTOOVERLAYMASK(i AS WORD) AS WORD
	RETURN ((i) << 8)

_DLL FUNCTION ImageList_Draw(himl AS PTR, i AS INT, hdcDst AS PTR,;
		X AS INT, Y AS INT,	fStyle AS DWORD);
		AS LOGIC PASCAL:COMCTL32.ImageList_Draw



_DLL FUNCTION ImageList_Replace(himl AS PTR, i AS INT, hbmImage AS PTR, hbmMask AS PTR);
		AS LOGIC PASCAL:COMCTL32.ImageList_Replace


_DLL FUNCTION ImageList_AddMasked(himo AS PTR, hbmImage AS PTR, crMasg AS DWORD) AS INT PASCAL:COMCTL32.ImageList_AddMasked


_DLL FUNCTION ImageList_DrawEx(himl AS PTR, i AS INT, hdDst AS PTR, X AS INT, Y AS INT, dx AS INT, dy AS INT,;
		rgbBk AS DWORD, rgbFg AS DWORD, fStyle AS DWORD) AS LOGIC PASCAL:COMCTL32.ImageList_DrawEx


_DLL FUNCTION ImageList_Remove(himl AS PTR, i AS INT) AS LOGIC PASCAL:COMCTL32.ImageList_Remove


_DLL FUNCTION ImageList_GetIcon(himl AS PTR, i AS INT, flags AS DWORD) AS PTR PASCAL:COMCTL32.ImageList_GetIcon



_DLL FUNCTION ImageList_LoadImage(hi AS PTR, lpbmp AS PSZ, CX AS INT, cGrow AS INT, crMask AS DWORD,;
		uType AS DWORD, uFlags AS DWORD) AS PTR PASCAL:COMCTL32.ImageList_LoadImageA



_DLL FUNCTION ImageList_BeginDrag(himlTrack AS PTR, iTrack AS INT, dxHotspot AS INT, dyHotspot AS INT);
		AS LOGIC PASCAL:COMCTL32.ImageList_BeginDrag


_DLL FUNCTION ImageList_EndDrag() AS VOID PASCAL:COMCTL32.ImageList_EndDrag


_DLL FUNCTION ImageList_DragEnter(hwnLock AS PTR, x AS INT, Y AS INT) AS LOGIC PASCAL:COMCTL32.ImageList_DragEnter


_DLL FUNCTION ImageList_DragLeave(hwndLock AS PTR) AS LOGIC PASCAL:COMCTL32.ImageList_DragLeave


_DLL FUNCTION ImageList_DragMove(x AS INT, Y AS INT) AS LOGIC PASCAL:COMCTL32.ImageList_DragMove


_DLL FUNCTION ImageList_SetDragCursorImage( himlDrag AS PTR, iDrag AS INT, dxHotspot AS INT, dyHotspot AS INT);
		AS LOGIC PASCAL:COMCTL32.ImageList_SetDragCursorImage



_DLL FUNCTION  ImageList_DragShowNolock(fShow AS LOGIC) AS LOGIC PASCAL:COMCTL32.ImageList_DragShowNolock


_DLL FUNCTION ImageList_GetDragImage(ppt AS _winPOINT, pptHotspot AS _winPOINT) AS PTR PASCAL:COMCTL32.ImageList_GetDragImage


FUNCTION	ImageList_RemoveAll(himl AS PTR) AS LOGIC STRICT
	RETURN ImageList_Remove(himl, -1)

FUNCTION	ImageList_ExtractIcon(hi AS PTR, himl AS PTR, i AS INT) AS PTR STRICT
	RETURN	ImageList_GetIcon(himl, i, 0)

FUNCTION	ImageList_LoadBitmap(hi AS PTR, lpbmp AS PSZ, cx AS INT, cGrow AS INT, crMask AS DWORD) AS PTR STRICT
	RETURN ImageList_LoadImage(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, 0)


_DLL FUNCTION ImageList_Read(pstm AS PTR) AS PTR PASCAL:COMCTL32.ImageList_Read


_DLL FUNCTION ImageList_Write(himl AS PTR, pstm AS PTR) AS LOGIC PASCAL:COMCTL32.ImageList_Write

_DLL FUNCTION ImageList_GetIconSize( himl AS PTR, cx AS INT PTR, cy AS INT PTR) AS LOGIC PASCAL:COMCTL32.ImageList_GetIconSize


_DLL FUNCTION ImageList_SetIconSize(himl AS PTR, cx AS INT, cy AS INT) AS LOGIC PASCAL:COMCTL32.ImageList_SetIconSize


_DLL FUNCTION ImageList_GetImageInfo(himl AS PTR, i AS INT, pImageInfo AS _winIMAGEINFO) AS LOGIC PASCAL:COMCTL32.ImageList_GetImageInfo


_DLL FUNCTION ImageList_Merge(himl1 AS PTR, i1 AS INT, himl2 AS PTR, i2 AS INT, dx AS INT, dy AS INT);
		AS PTR PASCAL:COMCTL32.ImageList_Merge






FUNCTION Header_GetItemCount(hwndHD AS PTR) AS INT STRICT
	RETURN  SendMessage((hwndHD), HDM_GETITEMCOUNT, 0, 0L)


FUNCTION Header_InsertItem(hwndHD AS PTR, i AS INT, phdi AS _winHD_ITEM) AS INT STRICT
	LOCAL retVal	AS LONGINT
	retVal :=  SendMessage((hwndHD), HDM_INSERTITEM, DWORD(_CAST,i), LONGINT(_CAST,phdi))
	RETURN retVal




FUNCTION Header_DeleteItem(hwndHD AS PTR, i AS INT) AS LOGIC STRICT
	LOCAL retVal	AS LONGINT
	retVal :=  SendMessage((hwndHD), HDM_DELETEITEM, DWORD(_CAST,i) , 0L)
	RETURN (LOGIC(_CAST, retVal))




FUNCTION Header_GetItem(hwndHD AS PTR, i AS INT, phdi AS _winHD_ITEM) AS LOGIC STRICT
	LOCAL retVal	AS LONGINT
	retVal :=  SendMessage((hwndHD), HDM_GETITEM, DWORD(_CAST,i), LONGINT(_CAST, phdi))
	RETURN (LOGIC(_CAST, retVal))



_DLL FUNCTION CreateToolbarEx(hwnd AS PTR,	ws AS DWORD, wID AS DWORD, nBitmaps AS INT,;
		hBMInst AS PTR, wBMID AS DWORD, lpButtons AS _winTBBUTTON,;
		iNumButtons AS INT, dxButton AS INT,	dyButton AS INT, dxBitmap AS INT,;
		dyBitmap AS INT, uStructSize AS DWORD) AS PTR PASCAL:COMCTL32.CreateToolbarEx




_DLL FUNCTION CreateMappedBitmap(hInstance AS PTR, idBitmap AS INT, wFlags AS DWORD, lpColorMap AS _winCOLORMAP,;
		iNumMaps AS INT) AS PTR PASCAL:COMCTL32.CreateMappedBitmap

_DLL FUNCTION CreateStatusWindow(style AS LONGINT, lpszText AS PSZ, hwnParent AS PTR, wID AS DWORD);
		AS PTR PASCAL:COMCTL32.CreateStatusWindowA
_DLL FUNCTION DrawStatusText(hDc AS PTR, lprc AS _winRECT, pszText AS PSZ, uFlags AS DWORD) AS VOID PASCAL:COMCTL32.DrawStatusTextA



_DLL FUNCTION MenuHelp(uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT, hMainMenu AS PTR, hInst AS PTR,;
		hwndStatus AS PTR, lpwIDs AS DWORD PTR) AS VOID PASCAL:COMCTL32.MenuHelp


_DLL FUNCTION ShowHideMenuCtl(hWnd AS PTR, uFlags AS DWORD, lpInfo AS INT PTR) AS LOGIC PASCAL:COMCTL32.ShowHideMenuCtl


_DLL FUNCTION GetEffectiveClientRect(hWnd AS PTR, lprc AS _winRECT, lpInfo AS INT PTR) AS VOID PASCAL:COMCTL32.GetEffectiveClientRect

_DLL FUNCTION MakeDragList(hLB AS PTR) AS LOGIC PASCAL:COMCTL32.MakeDragList


_DLL FUNCTION DrawInsert(handParent AS PTR, hLB AS PTR, nItem AS INT) AS VOID PASCAL:COMCTL32.DrawInsert


_DLL FUNCTION LBItemFromPt(hLB AS PTR, pt AS _winPOINT, bAutoScroll AS LOGIC) AS INT PASCAL:comctl32.LBItemFromPt






_DLL FUNCTION CreateUpDownControl(dwStyle AS DWORD, X AS INT, Y AS INT, CX AS INT, CY AS INT, hParent AS PTR,;
		nID AS INT, hInst AS PTR, hBuddy AS PTR, nUpper AS INT, nLower AS INT, nPos AS INT);
		AS PTR PASCAL:COMCTL32.CreateUpDownControl
FUNCTION ListView_GetBkColor(hwnd AS PTR ) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), LVM_GETBKCOLOR, 0, 0L))))

FUNCTION ListView_GetImageList(hwnd AS PTR, iImageList AS INT) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), LVM_GETIMAGELIST, DWORD(_CAST,iImageList), 0))))

FUNCTION ListView_GetItemCount(hwnd AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), LVM_GETITEMCOUNT, 0, 0L))))


FUNCTION INDEXTOSTATEIMAGEMASK(i AS WORD) AS DWORD STRICT
	LOCAL retVal AS DWORD
	retval  := i  *	4096
	RETURN retVal


FUNCTION ListView_DeleteItem(hwnd AS PTR, i AS INT) AS LOGIC STRICT
	RETURN (LOGIC (_CAST, (SendMessage((hwnd), LVM_DELETEITEM, DWORD(_CAST,i), 0))))


FUNCTION ListView_DeleteAllItems(hwnd AS PTR) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_DELETEALLITEMS,  0,  0))))


FUNCTION ListView_SetCallbackMask(hwnd AS PTR, mask AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETCALLBACKMASK, (mask), 0))))


FUNCTION ListView_GetNextItem(hwnd AS PTR, i AS INT, flags AS WORD) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), LVM_GETNEXTITEM, DWORD(_CAST,i), MAKELPARAM((flags), 0)))))

FUNCTION ListView_FindItem(hwnd AS PTR, iStart AS INT, plvfi AS _winLV_FINDINFO) AS INT STRICT
	RETURN	(INT(_CAST, (SendMessage((hwnd), LVM_FINDITEM, DWORD(_CAST, iStart), LONGINT(_CAST, plvfi)))))

FUNCTION ListView_SetItemPosition(hwndLV AS PTR, i AS INT, x AS WORD, y AS WORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_SETITEMPOSITION, DWORD(_CAST, i), MAKELPARAM(x, y)))))

FUNCTION ListView_GetStringWidth(hwndLV AS PTR, _psz AS PSZ) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwndLV), LVM_GETSTRINGWIDTH, 0, LONGINT(_CAST, _psz)))))

FUNCTION ListView_EnsureVisible(hwndLV AS PTR, i AS INT, fPartialOK AS WORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_ENSUREVISIBLE, DWORD(_CAST,i), MAKELPARAM(fPartialOK , 0)))))

FUNCTION	ListView_Arrange(hwndLV AS PTR, _code AS DWORD)  AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_ARRANGE, _code, 0L))))

FUNCTION	ListView_EditLabel(hwndLV AS PTR, i AS INT) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwndLV), LVM_EDITLABEL, DWORD(_CAST,i), 0))))

FUNCTION	ListView_GetEditControl(hwndLV AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwndLV), LVM_GETEDITCONTROL, 0, 0))))

FUNCTION ListView_InsertColumn(hwnd AS PTR, iCol AS INT, pcol AS _winLV_COLUMN) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), LVM_INSERTCOLUMN, DWORD(_CAST, iCol), LONGINT(_CAST,pcol)))))

FUNCTION ListView_DeleteColumn(hwnd AS PTR, iCol AS INT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_DELETECOLUMN, DWORD(_CAST, iCol), 0))))

FUNCTION ListView_GetColumnWidth(hwnd AS PTR, iCol AS INT) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), LVM_GETCOLUMNWIDTH, DWORD(_CAST, iCol), 0))))

FUNCTION ListView_GetTextColor(hwnd AS PTR)  AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), LVM_GETTEXTCOLOR, 0, 0L))))

FUNCTION	ListView_GetTextBkColor(hwnd AS PTR) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), LVM_GETTEXTBKCOLOR, 0, 0L))))


FUNCTION ListView_SetTextBkColor(hwnd AS PTR, clrTextBk AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETTEXTBKCOLOR, 0, LONGINT(_CAST, clrTextBk)))))

FUNCTION	ListView_GetTopIndex(hwndLV AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwndLV), LVM_GETTOPINDEX, 0, 0))))

FUNCTION ListView_GetCountPerPage(hwndLV AS PTR) AS INT STRICT
	RETURN  (INT(_CAST, (SendMessage((hwndLV), LVM_GETCOUNTPERPAGE, 0, 0))))

FUNCTION ListView_Update(hwndLV AS PTR, i AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_UPDATE, i, 0L))))


FUNCTION ListView_GetItemState(hwndLV AS PTR, i AS DWORD, mask AS LONGINT) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwndLV), LVM_GETITEMSTATE, i, mask))))


FUNCTION ListView_SetItemCount(hwndLV AS PTR, cItems AS DWORD) AS VOID STRICT
	SendMessage((hwndLV), LVM_SETITEMCOUNT, cItems, 0)
	RETURN

FUNCTION ListView_GetSelectedCount(hwndLV AS PTR) AS DWORD STRICT
	RETURN DWORD(_CAST,SendMessage((hwndLV), LVM_GETSELECTEDCOUNT, 0, 0L))

FUNCTION ListView_GetItemSpacing(hwndLV AS PTR, fSmall AS DWORD) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwndLV), LVM_GETITEMSPACING, fSmall, 0L))))

FUNCTION ListView_GetISearchString(hwndLV AS PTR, lpsz AS PSZ) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_GETISEARCHSTRING, 0, LONGINT(_CAST ,lpsz)))))


UNION  _winTV_ITEMUNION
	MEMBER item IS _winTV_ITEM
	MEMBER itemex IS _winTV_ITEMEX



FUNCTION TreeView_InsertItem(hwnd AS PTR, lpis AS _winTV_INSERTSTRUCT) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_INSERTITEM, 0, LONGINT(_CAST, lpis)))))

FUNCTION TreeView_DeleteItem(hwnd AS PTR, hitem AS PTR) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_DELETEITEM, 0, LONGINT(_CAST,hitem)))))


FUNCTION TreeView_DeleteAllItems(hwnd AS PTR) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_DELETEITEM, 0, LONGINT(_CAST, TVI_ROOT)))))

FUNCTION TreeView_Expand(hwnd AS PTR, hitem AS PTR, _code AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_EXPAND, _code, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_GetCount(hwnd AS PTR) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), TVM_GETCOUNT, 0, 0))))

FUNCTION	TreeView_GetIndent(hwnd AS PTR) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), TVM_GETINDENT, 0, 0))))

FUNCTION TreeView_SetIndent(hwnd AS PTR, indent AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_SETINDENT, indent, 0))))


FUNCTION TreeView_GetImageList(hwnd AS PTR, iImage AS DWORD) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_GETIMAGELIST, iImage, 0))))

FUNCTION TreeView_SetImageList(hwnd AS PTR, himl AS PTR, iImage AS DWORD) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_SETIMAGELIST, iImage, LONGINT(_CAST, DWORD(_CAST, himl))))))

FUNCTION TreeView_GetNextItem(hwnd AS PTR, hitem AS PTR, _code AS DWORD) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_GETNEXTITEM, _code, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_GetChild(hwnd AS PTR, hitem AS PTR ) AS PTR STRICT
	RETURN 	TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD)

FUNCTION TreeView_GetNextSibling(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT)

FUNCTION	TreeView_GetPrevSibling(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUS)

FUNCTION	TreeView_GetParent(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_PARENT)


FUNCTION	TreeView_GetFirstVisible(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_FIRSTVISIBLE)

FUNCTION	TreeView_GetNextVisible(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_NEXTVISIBLE)

FUNCTION	TreeView_GetPrevVisible(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN  TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUSVISIBLE)

FUNCTION	TreeView_GetSelection(hwnd AS PTR) AS PTR STRICT
	RETURN	TreeView_GetNextItem(hwnd, NULL,	TVGN_CARET)

FUNCTION TreeView_GetDropHilight(hwnd AS PTR) AS PTR STRICT
	RETURN	TreeView_GetNextItem(hwnd, NULL,	TVGN_DROPHILITE)

FUNCTION	TreeView_GetRoot(hwnd AS PTR) AS PTR STRICT
	RETURN	 TreeView_GetNextItem(hwnd, NULL,  TVGN_ROOT)

FUNCTION TreeView_Select(hwnd AS PTR, hitem AS PTR, _code AS DWORD ) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_SELECTITEM, _code, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_SelectItem(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN TreeView_Select(hwnd, hitem, TVGN_CARET)

FUNCTION TreeView_SelectDropTarget(hwnd AS PTR, hitem AS PTR ) AS PTR STRICT
	RETURN	TreeView_Select(hwnd, hitem, TVGN_DROPHILITE)

FUNCTION	TreeView_SelectSetFirstVisible(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN TreeView_Select(hwnd, hitem, TVGN_FIRSTVISIBLE)


FUNCTION TreeView_GetEditControl(hwnd AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_GETEDITCONTROL, 0, 0))))

FUNCTION TreeView_GetVisibleCount(hwnd AS PTR) AS DWORD STRICT
	RETURN (DWORD(_CAST, (SendMessage((hwnd), TVM_GETVISIBLECOUNT, 0, 0))))

FUNCTION	TreeView_EndEditLabelNow(hwnd AS PTR, fCancel AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_ENDEDITLABELNOW, fCancel, 0))))


FUNCTION	TabCtrl_GetImageList(hwnd AS PTR)  AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TCM_GETIMAGELIST, 0, 0L))))

FUNCTION TabCtrl_GetItemCount(hwnd AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_GETITEMCOUNT, 0, 0L))))

FUNCTION	TabCtrl_DeleteItem(hwnd AS PTR, i AS INT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TCM_DELETEITEM, DWORD(_CAST, i), 0L))))

FUNCTION TabCtrl_DeleteAllItems(hwnd AS PTR)	AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TCM_DELETEALLITEMS, 0, 0L))))

FUNCTION TabCtrl_GetItemRect(hwnd AS PTR, i AS INT,  prc AS _winRECT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TCM_GETITEMRECT, DWORD(_CAST, i), LONGINT(_CAST, prc)))))

FUNCTION TabCtrl_GetCurSel(hwnd AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_GETCURSEL, 0, 0))))

FUNCTION TabCtrl_SetCurSel(hwnd AS PTR, i AS DWORD) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_SETCURSEL, i, 0))))

FUNCTION TabCtrl_SetItemExtra(hwndTC AS PTR, cb AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndTC), TCM_SETITEMEXTRA, cb, 0L))))

FUNCTION	TabCtrl_SetItemSize(hwnd AS PTR, x AS WORD, y AS WORD) AS DWORD
	RETURN (DWORD(_CAST, (SendMessage((hwnd), TCM_SETITEMSIZE, 0, MAKELPARAM(x,y)))))

FUNCTION TabCtrl_RemoveImage(hwnd AS PTR, i AS DWORD) AS VOID STRICT
	SendMessage((hwnd), TCM_REMOVEIMAGE, i, 0L)
	RETURN

FUNCTION TabCtrl_SetPadding(hwnd AS PTR,	cx AS WORD, cy AS WORD) AS VOID STRICT
	SendMessage((hwnd), TCM_SETPADDING, 0, MAKELPARAM(cx, cy))
	RETURN

FUNCTION	TabCtrl_GetRowCount(hwnd AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_GETROWCOUNT, 0, 0L))))


FUNCTION	TabCtrl_GetToolTips(hwnd AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TCM_GETTOOLTIPS, 0, 0L))))

FUNCTION TabCtrl_SetToolTips(hwnd AS PTR,  hwndTT AS DWORD) AS VOID STRICT
	SendMessage((hwnd), TCM_SETTOOLTIPS, hwndTT, 0L)
	RETURN

FUNCTION TabCtrl_GetCurFocus(hwnd AS PTR) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_GETCURFOCUS, 0, 0))))

FUNCTION TabCtrl_SetCurFocus(hwnd AS PTR, i AS DWORD) AS VOID STRICT
	SendMessage((hwnd),TCM_SETCURFOCUS, i, 0)
	RETURN

FUNCTION Animate_Create(hwndP  AS PTR, id AS PTR, dwStyle AS DWORD, hInstance AS PTR) AS PTR STRICT
	RETURN (CreateWindow(String2Psz(ANIMATE_CLASS), NULL, dwStyle, 0, 0, 0, 0, hwndP, id, hInstance, NULL))

FUNCTION	Animate_Stop(hwnd AS PTR) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage(hwnd, ACM_STOP, 0, 0))))

FUNCTION Animate_Close(hwnd AS PTR)  AS LOGIC STRICT
	RETURN  Animate_Open(hwnd, NULL)

FUNCTION	Animate_Seek(hwnd AS PTR, frame AS WORD ) AS LOGIC STRICT
	RETURN  Animate_Play(hwnd, frame, frame, 1)


	// new stuff from CommCtrl 1.2
_DLL FUNCTION ImageList_DrawIndirect(pimldp AS PTR) AS LOGIC STRICT:COMCTL32.ImageList_DrawIndirect
_DLL FUNCTION ImageList_Duplicate(himl AS PTR) AS PTR STRICT:COMCTL32.ImageList_Duplicate
_DLL FUNCTION ImageList_SetImageCount(himl AS PTR, uNewCount AS DWORD) AS LOGIC PASCAL:COMCTL32.ImageList_SetImageCount

	// new stuff from CommCtrl 1.2
_DLL FUNCTION InitCommonControlsEx(pICCEX AS _winINITCOMMONCONTROLSEX) AS LOGIC PASCAL:COMCTL32.InitCommonControlsEx

_DLL FUNCTION FlatSB_EnableScrollBar(hwnd AS PTR, wSBflags AS INT, wArrows AS DWORD) AS LOGIC PASCAL:COMCTL32.FlatSB_EnableScrollBar
_DLL FUNCTION FlatSB_GetScrollInfo(hwnd AS PTR, wnBar AS INT, lpsi AS _winSCROLLINFO) AS LOGIC PASCAL:COMCTL32.FlatSB_GetScrollInfo
_DLL FUNCTION FlatSB_GetScrollPos(hwnd AS PTR, _code AS INT) AS INT PASCAL:COMCTL32.FlatSB_GetScrollPos
_DLL FUNCTION FlatSB_GetScrollProp(hwnd AS PTR, index AS DWORD, pValue AS INT PTR) AS LOGIC PASCAL:COMCTL32.FlatSB_GetScrollProp
_DLL FUNCTION FlatSB_GetScrollRange(hwnd AS PTR, _code AS INT, lpMinPos AS INT PTR, lpMaxPos AS INT PTR) AS LOGIC PASCAL:COMCTL32.FlatSB_GetScrollRange
_DLL FUNCTION FlatSB_SetScrollInfo(hwnd AS PTR, wSBflags AS INT, wArrows AS DWORD) AS LOGIC PASCAL:COMCTL32.FlatSB_SetScrollInfo
_DLL FUNCTION FlatSB_SetScrollPos(hwnd AS PTR, _code AS INT, nPos AS INT, fRedraw AS LOGIC) AS INT PASCAL:COMCTL32.FlatSB_SetScrollPos
_DLL FUNCTION FlatSB_SetScrollProp(hwnd AS PTR, index AS DWORD, nPos AS INT, fRedraw AS LOGIC) AS LOGIC PASCAL:COMCTL32.FlatSB_SetScrollProp
_DLL FUNCTION FlatSB_SetScrollRange(hwnd AS PTR, _code AS INT, nMinPos AS INT, nMaxPos AS INT, fRedraw AS LOGIC) AS INT PASCAL:COMCTL32.FlatSB_SetScrollRange
_DLL FUNCTION FlatSB_ShowScrollBar(hwnd AS PTR, _code AS INT, fShow AS LOGIC) AS LOGIC PASCAL:COMCTL32.FlatSB_ShowScrollBar
_DLL FUNCTION InitializeFlatSB(hwnd AS PTR) AS LOGIC PASCAL:COMCTL32.InitializeFlatSB
_DLL FUNCTION UninitializeFlatSB(hwnd AS PTR) AS PTR PASCAL:COMCTL32.UninitializeFlatSB

_DLL FUNCTION PropertySheet(lppsph AS _winPROPSHEETHEADER) AS INT PASCAL:COMCTL32.PropertySheetA
	/*****************************************************************************\
	*                                                                             *
	* prsht.h - - Interface for the Windows Property Sheet Pages                  *
	*                                                                             *
	* Version 1.0                                                                 *
	*                                                                             *
	* Copyright 1991-1998, Microsoft Corp.      All rights reserved.              *
	*                                                                             *
	\*****************************************************************************/
_DLL FUNCTION CreatePropertySheetPage(lppsp AS _winPROPSHEETPAGE) AS PTR PASCAL:COMCTL32.CreatePropertySheetPageA
_DLL FUNCTION DestroyPropertySheetPage(hPSPage AS PTR) AS LOGIC PASCAL:COMCTL32.DestroyPropertySheetPage

_DLL FUNCTION _TrackMouseEvent(lpEventTrack AS _winTRACKMOUSEEVENT) AS LOGIC PASCAL:COMCTL32._TrackMouseEvent
FUNCTION Header_SetItem(hwndHD AS PTR, i AS INT, phdi AS _winHD_ITEM) AS LOGIC STRICT
	LOCAL retVal	AS LONGINT
	retVal :=  SendMessage((hwndHD), HDM_SETITEM, DWORD(_CAST,i), LONGINT(_CAST,phdi))
	RETURN (LOGIC(_CAST, retVal))


FUNCTION Header_Layout(hwndHD AS PTR, playout AS _winHD_LAYOUT) AS LOGIC STRICT
	LOCAL retVal	AS LONGINT

	retVal :=  SendMessage((hwndHD), HDM_LAYOUT, 0, LONGINT(_CAST, playout))
	RETURN (LOGIC(_CAST, retVal))

FUNCTION ListView_SetBkColor(hwnd AS PTR, clrBk AS DWORD)  AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETBKCOLOR, 0, LONGINT(_CAST,clrBk)))))


FUNCTION ListView_SetImageList(hwnd AS PTR, himl AS PTR , iImageList AS DWORD) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), LVM_SETIMAGELIST, (iImageList), LONGINT(_CAST, DWORD(_CAST, himl))))))

FUNCTION ListView_GetItem(hwnd AS PTR, pitem AS _winLV_ITEM) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_GETITEM, 0, LONGINT(_CAST,pitem)))))


FUNCTION ListView_SetItem(hwnd AS PTR, pitem AS _winLV_ITEM ) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETITEM, 0, LONGINT(_CAST, pitem)))))


FUNCTION ListView_InsertItem(hwnd AS PTR, pitem AS _winLV_ITEM) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), LVM_INSERTITEM, 0, LONGINT(_CAST, pitem)))))

FUNCTION ListView_GetCallbackMask(hwnd AS PTR) AS DWORD STRICT
	RETURN (DWORD(_CAST,(SendMessage((hwnd), LVM_GETCALLBACKMASK, DWORD (_CAST,0) , LONGINT(_CAST, 0)))))


FUNCTION ListView_GetItemPosition(hwndLV AS PTR, i AS INT, ppt AS _winPOINT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_GETITEMPOSITION, DWORD(_CAST,i), LONGINT(_CAST,ppt)))))

FUNCTION ListView_HitTest(hwndLV AS PTR, pinfo AS _winLV_HITTESTINFO) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwndLV), LVM_HITTEST, 0, LONGINT(_CAST, pinfo)))))


FUNCTION ListView_Scroll(hwndLV AS PTR, dx AS INT, dy AS INT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_SCROLL, DWORD(_CAST,dx), LONGINT(_CAST,dy)))))


FUNCTION ListView_RedrawItems(hwndLV AS PTR, iFirst AS INT, iLast AS INT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_REDRAWITEMS, DWORD(_CAST, iFirst), LONGINT(_CAST, iLast)))))

FUNCTION ListView_GetColumn(hwnd AS PTR, iCol AS INT, pcol AS _winLV_COLUMN) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_GETCOLUMN, DWORD(_CAST, iCol), LONGINT(_CAST, pcol)))))


FUNCTION ListView_SetColumn(hwnd AS PTR, iCol AS INT, pcol AS _winLV_COLUMN) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETCOLUMN, DWORD(_CAST, iCol), LONGINT(_CAST,  pcol)))))


FUNCTION ListView_SetColumnWidth(hwnd AS PTR, iCol AS INT, cx AS SHORTINT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETCOLUMNWIDTH, DWORD(_CAST, iCol), MAKELPARAM(WORD(_CAST, cx), 0)))))

FUNCTION ListView_CreateDragImage(hwnd AS PTR, i AS INT, lpptUpLeft AS _winPOINT) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), LVM_CREATEDRAGIMAGE, DWORD(_CAST, i),  LONGINT(_CAST, lpptUpLeft)))))

FUNCTION ListView_GetViewRect(hwnd AS PTR, prc AS _winRECT) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_GETVIEWRECT, 0, LONGINT(_CAST, prc)))))

FUNCTION ListView_SetTextColor(hwnd AS PTR, clrText AS DWORD) AS LOGIC STRICT
	//PP-030924 correct 51422
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), LVM_SETTEXTCOLOR, 0, LONGINT(_CAST,clrText)))))

FUNCTION ListView_GetOrigin(hwndLV AS PTR, ppt AS _winPOINT)	AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndLV), LVM_GETORIGIN, DWORD(_CAST, 0), LONGINT(_CAST,ppt)))))

FUNCTION ListView_SetItemState(hwndLV AS PTR, i AS DWORD, data AS DWORD , mask AS DWORD)	AS VOID STRICT
	LOCAL _ms_lvi IS _winLV_ITEM

	_ms_lvi:stateMask := mask
	_ms_lvi:state := data
	SendMessage((hwndLV), LVM_SETITEMSTATE, i, LONGINT(_CAST, @_ms_lvi))
	RETURN

FUNCTION ListView_GetItemText(hwndLV AS PTR, i AS DWORD, iSubItem_ AS INT , pszText_ AS PSZ, cchTextMax_ AS INT) AS VOID STRICT
	LOCAL _ms_lvi IS _winLV_ITEM

	_ms_lvi:iSubItem   := iSubItem_
	_ms_lvi:cchTextMax := cchTextMax_
	_ms_lvi:pszText    := pszText_
	SendMessage((hwndLV), LVM_GETITEMTEXT, i, LONGINT(_CAST, @_ms_lvi))
	RETURN

FUNCTION ListView_SetItemText(hwndLV AS PTR, i AS DWORD, iSubItem_ AS INT, pszText_ AS PSZ) AS VOID STRICT
	LOCAL _ms_lvi IS  _winLV_ITEM

	_ms_lvi:iSubItem := iSubItem_
	_ms_lvi:pszText	:= pszText_
	SendMessage((hwndLV), LVM_SETITEMTEXT, i, LONGINT(_CAST, @_ms_lvi))
	RETURN
	// these flags only apply to LVS_OWNERDATA listviews in report or list mode
FUNCTION ListView_SetItemPosition32(hwndLV AS PTR, i AS INT, x AS LONGINT, y AS LONGINT) AS VOID STRICT
	LOCAL ptNewPos IS _winPOINT
	ptNewPos:X := X
	ptNewPos:Y := Y
	SendMessage((hwndLV), LVM_SETITEMPOSITION32, DWORD(_CAST, i), LONGINT(_CAST, @ptNewPos))
	RETURN

FUNCTION TreeView_GetItem(hwnd AS PTR, pitem AS _winTV_ITEM) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_GETITEM, 0, LONGINT(_CAST, pitem)))))


FUNCTION	TreeView_SetItem(hwnd AS PTR, pitem AS _winTV_ITEM) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_SETITEM, 0, LONGINT(_CAST, pitem)))))

FUNCTION TreeView_EditLabel(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_EDITLABEL, 0, LONGINT(_CAST, hitem)))))


FUNCTION TreeView_HitTest(hwnd AS PTR, lpht AS _winTV_HITTESTINFO )AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_HITTEST, 0, LONGINT(_CAST, lpht)))))

FUNCTION	TreeView_CreateDragImage(hwnd AS PTR, hitem AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TVM_CREATEDRAGIMAGE, 0, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_SortChildren(hwnd AS PTR, hitem AS PTR, recurse AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_SORTCHILDREN, recurse, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_EnsureVisible(hwnd AS PTR, hitem AS PTR) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_ENSUREVISIBLE, 0, LONGINT(_CAST, hitem)))))

FUNCTION TreeView_SortChildrenCB(hwnd AS PTR, psort AS _winTV_SORTCB, recurse AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TVM_SORTCHILDRENCB, recurse, LONGINT(_CAST, psort)))))

FUNCTION TreeView_GetISearchString(hwndTV AS PTR, lpsz AS PSZ) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwndTV), TVM_GETISEARCHSTRING, 0, LONGINT(_CAST, lpsz)))))


FUNCTION TabCtrl_SetImageList(hwnd AS PTR, himl AS PTR) AS PTR STRICT
	RETURN (PTR(_CAST, (SendMessage((hwnd), TCM_SETIMAGELIST, 0, LONGINT(_CAST, DWORD(_CAST,himl))))))


FUNCTION	TabCtrl_GetItem(hwnd AS PTR, iItem AS INT, pitem AS _winTC_ITEM) AS LOGIC STRICT
	RETURN  (LOGIC(_CAST, (SendMessage((hwnd), TCM_GETITEM,	DWORD(_CAST, iItem), LONGINT(_CAST, pitem)))))



FUNCTION TabCtrl_SetItem(hwnd AS PTR, iItem AS INT, pitem AS _winTC_ITEM) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage((hwnd), TCM_SETITEM, DWORD(_CAST,iItem) , LONGINT(_CAST, pitem)))))


FUNCTION TabCtrl_InsertItem(hwnd AS PTR, iItem AS INT, pitem AS _winTC_ITEM) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwnd), TCM_INSERTITEM, DWORD(_CAST, iItem), LONGINT(_CAST, pitem)))))

FUNCTION TabCtrl_HitTest(hwndTC AS PTR, pinfo AS _winTC_HITTESTINFO) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage((hwndTC), TCM_HITTEST, 0, LONGINT(_CAST, pinfo)))))

FUNCTION TabCtrl_AdjustRect(hwnd AS PTR, bLarger AS LOGIC, prc AS _winRECT) AS INT STRICT
	RETURN (INT(_CAST, (SendMessage(hwnd, TCM_ADJUSTRECT, DWORD(_CAST, bLarger), LONGINT(_CAST, prc)))))

FUNCTION Animate_Open(hwnd AS PTR, szName AS PSZ) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage(hwnd, ACM_OPEN, 0,  LONGINT(_CAST, szName)))))

FUNCTION Animate_Play(hwnd AS PTR, wFrom AS WORD , wTo AS WORD, dwRep AS DWORD) AS LOGIC STRICT
	RETURN (LOGIC(_CAST, (SendMessage(hwnd, ACM_PLAY, dwRep, LONGINT(_CAST, MakeLong(wFrom, wTo))))))


#region defines
DEFINE ODT_HEADER 						 := 100
DEFINE ODT_TAB								 := 101
DEFINE ODT_LISTVIEW 					 := 102
DEFINE LVM_FIRST							 := 0x1000
DEFINE TV_FIRST 							 := 0x1100
DEFINE HDM_FIRST							 := 0x1200
DEFINE PGM_FIRST               := 0x1400
DEFINE CCM_FIRST               := 0x2000
DEFINE ECM_FIRST               := 0x1500      // Edit control messages
DEFINE BCM_FIRST               := 0x1600      // Button control messages
DEFINE CBM_FIRST               := 0x1700      // Combobox control messages
DEFINE CCM_SETBKCOLOR          := (CCM_FIRST + 1)
DEFINE CCM_SETCOLORSCHEME      := (CCM_FIRST + 2)
DEFINE CCM_GETCOLORSCHEME      := (CCM_FIRST + 3)
DEFINE CCM_GETDROPTARGET       := (CCM_FIRST + 4)
DEFINE CCM_SETUNICODEFORMAT    := (CCM_FIRST + 5)
DEFINE CCM_GETUNICODEFORMAT    := (CCM_FIRST + 6)
DEFINE CCM_SETVERSION          := (CCM_FIRST + 0x7)
DEFINE CCM_GETVERSION          := (CCM_FIRST + 0x8)
DEFINE CCM_SETNOTIFYWINDOW     := (CCM_FIRST + 0x9) // wParam == hwndParent.
DEFINE CCM_SETWINDOWTHEME      := (CCM_FIRST + 0xb)
DEFINE CCM_DPISCALE            := (CCM_FIRST + 0xc) // wParam == Awareness
DEFINE NM_FIRST 		:= (0U-	0U)
DEFINE NM_LAST			:= (0U- 99U)
DEFINE NM_OUTOFMEMORY 		:= (NM_FIRST-1)
DEFINE NM_CLICK 		:= (NM_FIRST-2)
DEFINE NM_DBLCLK		:= (NM_FIRST-3)
DEFINE NM_RETURN		:= (NM_FIRST-4)
DEFINE NM_RCLICK		:= (NM_FIRST-5)
DEFINE NM_RDBLCLK 		:= (NM_FIRST-6)
DEFINE NM_SETFOCUS		:= (NM_FIRST-7)
DEFINE NM_KILLFOCUS 		:= (NM_FIRST-8)
DEFINE NM_CUSTOMDRAW           := (NM_FIRST-12)
DEFINE NM_HOVER                := (NM_FIRST-13)
DEFINE NM_NCHITTEST            := (NM_FIRST-14)
DEFINE NM_KEYDOWN              := (NM_FIRST-15)
DEFINE NM_RELEASEDCAPTURE      := (NM_FIRST-16)
DEFINE NM_SETCURSOR            := (NM_FIRST-17)
DEFINE NM_CHAR                 := (NM_FIRST-18)
DEFINE NM_TOOLTIPSCREATED      := (NM_FIRST-19)   // notify of when the tooltips window is create
DEFINE NM_LDOWN                := (NM_FIRST-20)
DEFINE NM_RDOWN                := (NM_FIRST-21)
DEFINE NM_THEMECHANGED         := (NM_FIRST-22)
DEFINE INFOTIPSIZE := 1024
	//====== Generic WM_NOTIFY notification structures ============================
DEFINE LVN_FIRST							 := (0U-100U)
DEFINE LVN_LAST 							 := (0U-199U)
DEFINE HDN_FIRST							 := (0U-300U)
DEFINE HDN_LAST 							 := (0U-399U)
DEFINE TVN_FIRST							 := (0U-400U)
DEFINE TVN_LAST 							 := (0U-499U)
DEFINE TTN_FIRST							 := (0U-520U)
DEFINE TTN_LAST 							 := (0U-549U)
DEFINE TCN_FIRST							 := (0U-550U)
DEFINE TCN_LAST 							 := (0U-580U)
DEFINE CDN_FIRST    :=  (0U-601U)
DEFINE CDN_LAST     :=  (0U-699U)
DEFINE TBN_FIRST							:=	(0U-700U)
DEFINE TBN_LAST 							:=	(0U-720U)
DEFINE UDN_FIRST							:=	(0U-721)
DEFINE UDN_LAST 							:=	(0U-740)
DEFINE MSGF_COMMCTRL_BEGINDRAG		:= 0x4200
DEFINE MSGF_COMMCTRL_SIZEHEADER 		:=0x4201
DEFINE MSGF_COMMCTRL_DRAGSELECT 		:=0x4202
DEFINE MSGF_COMMCTRL_TOOLBARCUST		:=0x4203
	// custom draw return flags
	// values under 0x00010000 are reserved for global custom draw values.
	// above that are for specific controls
DEFINE CDRF_DODEFAULT          := 0x00000000
DEFINE CDRF_NEWFONT            := 0x00000002
DEFINE CDRF_SKIPDEFAULT        := 0x00000004
DEFINE CDRF_NOTIFYPOSTPAINT    := 0x00000010
DEFINE CDRF_NOTIFYITEMDRAW     := 0x00000020
DEFINE CDRF_NOTIFYSUBITEMDRAW  := 0x00000020
DEFINE CDRF_NOTIFYPOSTERASE    := 0x00000040
	// drawstage flags
	// values under 0x00010000 are reserved for global custom draw values.
	// above that are for specific controls
DEFINE CDDS_PREPAINT           := 0x00000001
DEFINE CDDS_POSTPAINT          := 0x00000002
DEFINE CDDS_PREERASE           := 0x00000003
DEFINE CDDS_POSTERASE          := 0x00000004
	// the 0x000010000 bit means it's individual item specific
DEFINE CDDS_ITEM              :=  0x00010000
DEFINE CDDS_ITEMPREPAINT      :=  0x00010001
DEFINE CDDS_ITEMPOSTPAINT     :=  0x00010002
DEFINE CDDS_ITEMPREERASE      :=  0x00010003
DEFINE CDDS_ITEMPOSTERASE     :=  0x00010004
DEFINE CDDS_SUBITEM           :=  0x00020000
	// itemState flags
DEFINE CDIS_SELECTED       := 0x0001
DEFINE CDIS_GRAYED         := 0x0002
DEFINE CDIS_DISABLED       := 0x0004
DEFINE CDIS_CHECKED        := 0x0008
DEFINE CDIS_FOCUS          := 0x0010
DEFINE CDIS_DEFAULT        := 0x0020
DEFINE CDIS_HOT            := 0x0040
DEFINE CDIS_MARKED         := 0x0080
DEFINE CDIS_INDETERMINATE  := 0x0100
DEFINE CLR_NONE 							 := 0xFFFFFFFFL
DEFINE CLR_DEFAULT						 := 0xFF000000L
DEFINE ILC_MASK 							 := 0x0001
DEFINE ILC_COLOR							 := 0x0000
DEFINE ILC_COLORDDB 					 := 0x00FE
DEFINE ILC_COLOR4 						 := 0x0004
DEFINE ILC_COLOR8 						 := 0x0008
DEFINE ILC_COLOR16						 := 0x0010
DEFINE ILC_COLOR24						 := 0x0018
DEFINE ILC_COLOR32						 := 0x0020
DEFINE ILC_PALETTE						 := 0x0800
DEFINE ILD_NORMAL 						 := 0x0000
DEFINE ILD_TRANSPARENT				 := 0x0001
DEFINE ILD_MASK 							 := 0x0010
DEFINE ILD_IMAGE							 := 0x0020
DEFINE ILD_BLEND25						 := 0x0002
DEFINE ILD_BLEND50						 := 0x0004
DEFINE ILD_OVERLAYMASK				 := 0x0F00
DEFINE ILD_SELECTED 					 := ILD_BLEND50
DEFINE ILD_FOCUS							 := ILD_BLEND25
DEFINE ILD_BLEND							 := ILD_BLEND50
DEFINE CLR_HILIGHT						 := CLR_DEFAULT
DEFINE WC_HEADER		:= "SysHeader32"
DEFINE HDS_HORZ 		 := 0x00000000
DEFINE HDS_BUTTONS		 := 0x00000002
DEFINE HDS_HOTTRACK              := 0x00000004
DEFINE HDS_HIDDEN 		 := 0x00000008
DEFINE HDS_DRAGDROP              := 0x00000040
DEFINE HDS_FULLDRAG              := 0x00000080
DEFINE HDS_FILTERBAR             := 0x00000100
DEFINE HDS_FLAT                  := 0x00000200
DEFINE HDI_WIDTH		:= 0x0001
DEFINE HDI_HEIGHT 		:= HDI_WIDTH
DEFINE HDI_TEXT 		:= 0x0002
DEFINE HDI_FORMAT 		:= 0x0004
DEFINE HDI_LPARAM 		:= 0x0008
DEFINE HDI_BITMAP 		:= 0x0010
DEFINE HDI_IMAGE                := 0x0020
DEFINE HDI_DI_SETITEM           := 0x0040
DEFINE HDI_ORDER                := 0x0080
DEFINE HDI_FILTER               := 0x0100
DEFINE HDF_LEFT 		:= 0
DEFINE HDF_RIGHT		:= 1
DEFINE HDF_CENTER 		:= 2
DEFINE HDF_JUSTIFYMASK		:= 0x0003
DEFINE HDF_RTLREADING 		:= 4
DEFINE HDF_OWNERDRAW		:= 0x8000
DEFINE HDF_STRING 		:= 0x4000
DEFINE HDF_BITMAP 		:= 0x2000
DEFINE HDF_BITMAP_ON_RIGHT      := 0x1000
DEFINE HDF_IMAGE                := 0x0800
DEFINE HDF_SORTUP               := 0x0400
DEFINE HDF_SORTDOWN             := 0x0200
DEFINE HDM_GETITEMCOUNT         := (HDM_FIRST + 0)
DEFINE HDM_INSERTITEMA          := (HDM_FIRST + 1)
DEFINE HDM_INSERTITEMW          := (HDM_FIRST + 10)
DEFINE HDM_INSERTITEM           := HDM_INSERTITEMA
DEFINE HDM_DELETEITEM 			  := (HDM_FIRST + 2)
DEFINE HDM_GETITEMA     			:= (HDM_FIRST + 3)
DEFINE HDM_SETITEMA            := (HDM_FIRST + 4)
DEFINE HDM_LAYOUT 						 := (HDM_FIRST + 5)
DEFINE HDM_HITTEST				:= (HDM_FIRST + 6)
DEFINE HDM_GETITEMRECT					 := (HDM_FIRST + 7)
DEFINE HDM_SETIMAGELIST					 := (HDM_FIRST + 8)
DEFINE HDM_GETIMAGELIST					 := (HDM_FIRST + 9)
DEFINE HDM_GETITEMW     			:= (HDM_FIRST + 11)
DEFINE HDM_SETITEMW            := (HDM_FIRST + 12)
DEFINE HDM_ORDERTOINDEX					 := (HDM_FIRST + 15)
DEFINE HDM_CREATEDRAGIMAGE			 	:= (HDM_FIRST + 16)
DEFINE HDM_GETORDERARRAY				 := (HDM_FIRST + 17)
DEFINE HDM_SETORDERARRAY				 := (HDM_FIRST + 18)
DEFINE HDM_SETHOTDIVIDER				 := (HDM_FIRST + 19)
DEFINE HDM_GETITEM		:= HDM_GETITEMA
DEFINE HDM_SETITEM	       := HDM_SETITEMA
DEFINE HHT_NOWHERE		:= 0x0001
DEFINE HHT_ONHEADER 		:= 0x0002
DEFINE HHT_ONDIVIDER		:= 0x0004
DEFINE HHT_ONDIVOPEN		:= 0x0008
DEFINE HHT_ONFILTER             := 0x0010
DEFINE HHT_ONFILTERBUTTON       := 0x0020
DEFINE HHT_ABOVE		:= 0x0100
DEFINE HHT_BELOW		:= 0x0200
DEFINE HHT_TORIGHT		:= 0x0400
DEFINE HHT_TOLEFT 		:= 0x0800
DEFINE HDN_ITEMCHANGINGA	:= (HDN_FIRST-0)
DEFINE HDN_ITEMCHANGINGW	:= (HDN_FIRST-20)
DEFINE HDN_ITEMCHANGEDA 	:= (HDN_FIRST-1)
DEFINE HDN_ITEMCHANGEDW 	:= (HDN_FIRST-21)
DEFINE HDN_ITEMCLICKA 		:= (HDN_FIRST-2)
DEFINE HDN_ITEMCLICKW 		:= (HDN_FIRST-22)
DEFINE HDN_ITEMDBLCLICKA	:= (HDN_FIRST-3)
DEFINE HDN_ITEMDBLCLICKW	:= (HDN_FIRST-23)
DEFINE HDN_DIVIDERDBLCLICKA 	:= (HDN_FIRST-5)
DEFINE HDN_DIVIDERDBLCLICKW 	:= (HDN_FIRST-25)
DEFINE HDN_BEGINTRACKA		:= (HDN_FIRST-6)
DEFINE HDN_BEGINTRACKW		:= (HDN_FIRST-26)
DEFINE HDN_ENDTRACKA		:= (HDN_FIRST-7)
DEFINE HDN_ENDTRACKW		:= (HDN_FIRST-27)
DEFINE HDN_TRACKA 		:= (HDN_FIRST-8)
DEFINE HDN_TRACKW 		:= (HDN_FIRST-28)
DEFINE HDN_GETDISPINFOA         := (HDN_FIRST-9)
DEFINE HDN_GETDISPINFOW         := (HDN_FIRST-29)
DEFINE HDN_BEGINDRAG            := (HDN_FIRST-10)
DEFINE HDN_ENDDRAG              := (HDN_FIRST-11)
DEFINE HDN_FILTERCHANGE         := (HDN_FIRST-12)
DEFINE HDN_FILTERBTNCLICK       := (HDN_FIRST-13)
DEFINE HDN_ITEMCHANGING 	:= HDN_ITEMCHANGINGA
DEFINE HDN_ITEMCHANGED		:= HDN_ITEMCHANGEDA
DEFINE HDN_ITEMCLICK		:= HDN_ITEMCLICKA
DEFINE HDN_ITEMDBLCLICK 	:= HDN_ITEMDBLCLICKA
DEFINE HDN_DIVIDERDBLCLICK	:= HDN_DIVIDERDBLCLICKA
DEFINE HDN_BEGINTRACK 		:= HDN_BEGINTRACKA
DEFINE HDN_ENDTRACK 		:= HDN_ENDTRACKA
DEFINE HDN_TRACK		:= HDN_TRACKA
DEFINE TOOLBARCLASSNAME 			 :=  "ToolbarWindow32"
DEFINE CMB_MASKED 						 := 0x02
DEFINE TBSTATE_CHECKED		 := 0x01
DEFINE TBSTATE_PRESSED		 := 0x02
DEFINE TBSTATE_ENABLED		 := 0x04
DEFINE TBSTATE_HIDDEN 		 := 0x08
DEFINE TBSTATE_INDETERMINATE	 := 0x10
DEFINE TBSTATE_WRAP 		 := 0x20
DEFINE TBSTYLE_BUTTON 		:= 0x00
DEFINE TBSTYLE_SEP		:= 0x01
DEFINE TBSTYLE_CHECK		:= 0x02
DEFINE TBSTYLE_GROUP		:= 0x04
DEFINE TBSTYLE_CHECKGROUP 	:= 0x06
DEFINE TBSTYLE_TOOLTIPS 	:= 0x0100
DEFINE TBSTYLE_WRAPABLE 	:= 0x0200
DEFINE TBSTYLE_ALTDRAG		:= 0x0400
DEFINE TB_ENABLEBUTTON		:= (WM_USER + 1)
DEFINE TB_CHECKBUTTON 		:= (WM_USER + 2)
DEFINE TB_PRESSBUTTON 		:= (WM_USER + 3)
DEFINE TB_HIDEBUTTON		:= (WM_USER + 4)
DEFINE TB_INDETERMINATE 	:= (WM_USER + 5)
DEFINE TB_ISBUTTONENABLED 	:= (WM_USER + 9)
DEFINE TB_ISBUTTONCHECKED 	:= (WM_USER + 10)
DEFINE TB_ISBUTTONPRESSED 	:= (WM_USER + 11)
DEFINE TB_ISBUTTONHIDDEN	:= (WM_USER + 12)
DEFINE TB_ISBUTTONINDETERMINATE := (WM_USER + 13)
DEFINE TB_SETSTATE		:= (WM_USER + 17)
DEFINE TB_GETSTATE		:= (WM_USER + 18)
DEFINE TB_ADDBITMAP 		:= (WM_USER + 19)
DEFINE HINST_COMMCTRL 		:= PTR(_CAST, 0xFFFFFFFF)
DEFINE IDB_STD_SMALL_COLOR	:= 0
DEFINE IDB_STD_LARGE_COLOR	:= 1
DEFINE IDB_VIEW_SMALL_COLOR 	:= 4
DEFINE IDB_VIEW_LARGE_COLOR 	:= 5
DEFINE IDB_HIST_SMALL_COLOR     := 8
DEFINE IDB_HIST_LARGE_COLOR     := 9
DEFINE STD_CUT			:= 0
DEFINE STD_COPY 		:= 1
DEFINE STD_PASTE		:= 2
DEFINE STD_UNDO 		:= 3
DEFINE STD_REDOW		:= 4
DEFINE STD_DELETE 		:= 5
DEFINE STD_FILENEW		:= 6
DEFINE STD_FILEOPEN 		:= 7
DEFINE STD_FILESAVE 		:= 8
DEFINE STD_PRINTPRE 		:= 9
DEFINE STD_PROPERTIES 		:= 10
DEFINE STD_HELP 		:= 11
DEFINE STD_FIND 		:= 12
DEFINE STD_REPLACE		:= 13
DEFINE STD_PRINT		:= 14
DEFINE VIEW_LARGEICONS		:= 0
DEFINE VIEW_SMALLICONS		:= 1
DEFINE VIEW_LIST		:= 2
DEFINE VIEW_DETAILS 		:= 3
DEFINE VIEW_SORTNAME		:= 4
DEFINE VIEW_SORTSIZE		:= 5
DEFINE VIEW_SORTDATE		:= 6
DEFINE VIEW_SORTTYPE		:= 7
DEFINE VIEW_PARENTFOLDER	:= 8
DEFINE VIEW_NETCONNECT		:= 9
DEFINE VIEW_NETDISCONNECT 	:= 10
DEFINE VIEW_NEWFOLDER 		:= 11
DEFINE VIEW_VIEWMENU           := 12
DEFINE HIST_BACK               := 0
DEFINE HIST_FORWARD            := 1
DEFINE HIST_FAVORITES          := 2
DEFINE HIST_ADDTOFAVORITES     := 3
DEFINE HIST_VIEWTREE           := 4
DEFINE TB_ADDBUTTONSA		:= (WM_USER + 20)
DEFINE TB_INSERTBUTTONA		:= (WM_USER + 21)
DEFINE TB_DELETEBUTTON		:= (WM_USER + 22)
DEFINE TB_GETBUTTON 		:= (WM_USER + 23)
DEFINE TB_BUTTONCOUNT 		:= (WM_USER + 24)
DEFINE TB_COMMANDTOINDEX	:= (WM_USER + 25)
DEFINE TB_SAVERESTOREA		:= (WM_USER + 26)
DEFINE TB_SAVERESTOREW		:= (WM_USER + 76)
DEFINE TB_CUSTOMIZE 		:= (WM_USER + 27)
DEFINE TB_ADDSTRINGA		:= (WM_USER + 28)
DEFINE TB_ADDSTRINGW		:= (WM_USER + 77)
DEFINE TB_GETITEMRECT 		:= (WM_USER + 29)
DEFINE TB_BUTTONSTRUCTSIZE	:= (WM_USER + 30)
DEFINE TB_SETBUTTONSIZE 	:= (WM_USER + 31)
DEFINE TB_SETBITMAPSIZE 	:= (WM_USER + 32)
DEFINE TB_AUTOSIZE		:= (WM_USER + 33)
DEFINE TB_GETTOOLTIPS 		:= (WM_USER + 35)
DEFINE TB_SETTOOLTIPS 		:= (WM_USER + 36)
DEFINE TB_SETPARENT 		:= (WM_USER + 37)
DEFINE TB_SETROWS 		:= (WM_USER + 39)
DEFINE TB_GETROWS 		:= (WM_USER + 40)
DEFINE TB_GETBITMAPFLAGS	:= (WM_USER + 41)
DEFINE TB_SETCMDID		:= (WM_USER + 42)
DEFINE TB_CHANGEBITMAP		:= (WM_USER + 43)
DEFINE TB_GETBITMAP 		:= (WM_USER + 44)
DEFINE TB_GETBUTTONTEXTA	:= (WM_USER + 45)
DEFINE TB_GETBUTTONTEXTW	:= (WM_USER + 75)
DEFINE TB_REPLACEBITMAP 	:= (WM_USER + 46)
	// some new TB messages
DEFINE TB_SETINDENT            := (WM_USER + 47)
DEFINE TB_SETIMAGELIST         := (WM_USER + 48)
DEFINE TB_GETIMAGELIST         := (WM_USER + 49)
DEFINE TB_LOADIMAGES           := (WM_USER + 50)
DEFINE TB_GETRECT              := (WM_USER + 51) // wParam is the Cmd instead of index
DEFINE TB_SETHOTIMAGELIST      := (WM_USER + 52)
DEFINE TB_GETHOTIMAGELIST      := (WM_USER + 53)
DEFINE TB_SETDISABLEDIMAGELIST := (WM_USER + 54)
DEFINE TB_GETDISABLEDIMAGELIST := (WM_USER + 55)
DEFINE TB_SETSTYLE             := (WM_USER + 56)
DEFINE TB_GETSTYLE             := (WM_USER + 57)
DEFINE TB_GETBUTTONSIZE        := (WM_USER + 58)
DEFINE TB_SETBUTTONWIDTH       := (WM_USER + 59)
DEFINE TB_SETMAXTEXTROWS       := (WM_USER + 60)
DEFINE TB_GETTEXTROWS          := (WM_USER + 61)
DEFINE TB_GETBUTTONTEXT		:= TB_GETBUTTONTEXTA
DEFINE TB_SAVERESTORE		:= TB_SAVERESTOREA
DEFINE TB_ADDSTRING 				:= TB_ADDSTRINGA
DEFINE TB_GETOBJECT             := (WM_USER + 62)  // wParam == IID, lParam void **ppv
DEFINE TB_GETHOTITEM            := (WM_USER + 71)
DEFINE TB_SETHOTITEM            := (WM_USER + 72)  // wParam == iHotItem
DEFINE TB_SETANCHORHIGHLIGHT    := (WM_USER + 73)  // wParam == TRUE/FALSE
DEFINE TB_GETANCHORHIGHLIGHT    := (WM_USER + 74)
DEFINE TB_MAPACCELERATORA       := (WM_USER + 78)  // wParam == ch, lParam int * pidBtn
DEFINE TBIMHT_AFTER      	:= 0x00000001 // TRUE = insert After iButton, otherwise before
DEFINE TBIMHT_BACKGROUND 	:= 0x00000002 // TRUE iff missed buttons completely
DEFINE TB_GETINSERTMARK        := (WM_USER + 79)  // lParam == LPTBINSERTMARK
DEFINE TB_SETINSERTMARK        := (WM_USER + 80)  // lParam == LPTBINSERTMARK
DEFINE TB_INSERTMARKHITTEST    := (WM_USER + 81)  // wParam == LPPOINT lParam == LPTBINSERTMARK
DEFINE TB_MOVEBUTTON           := (WM_USER + 82)
DEFINE TB_GETMAXSIZE           := (WM_USER + 83)  // lParam == LPSIZE
DEFINE TB_SETEXTENDEDSTYLE     := (WM_USER + 84)  // For TBSTYLE_EX_*
DEFINE TB_GETEXTENDEDSTYLE     := (WM_USER + 85)  // For TBSTYLE_EX_*
DEFINE TB_GETPADDING           := (WM_USER + 86)
DEFINE TB_SETPADDING           := (WM_USER + 87)
DEFINE TB_SETINSERTMARKCOLOR   := (WM_USER + 88)
DEFINE TB_GETINSERTMARKCOLOR   := (WM_USER + 89)
DEFINE TB_SETCOLORSCHEME       := CCM_SETCOLORSCHEME  // lParam is color scheme
DEFINE TB_GETCOLORSCHEME       := CCM_GETCOLORSCHEME      // fills in COLORSCHEME pointed to by lParam
DEFINE TB_SETUNICODEFORMAT     := CCM_SETUNICODEFORMAT
DEFINE TB_GETUNICODEFORMAT     := CCM_GETUNICODEFORMAT
DEFINE TB_MAPACCELERATORW      := (WM_USER + 90)  // wParam == ch, lParam int * pidBtn
DEFINE TB_MAPACCELERATOR       := TB_MAPACCELERATORA
DEFINE TBBF_LARGE 		 := 0x0001
DEFINE TBIF_IMAGE              := 0x00000001
DEFINE TBIF_TEXT               := 0x00000002
DEFINE TBIF_STATE              := 0x00000004
DEFINE TBIF_STYLE              := 0x00000008
DEFINE TBIF_LPARAM             := 0x00000010
DEFINE TBIF_COMMAND            := 0x00000020
DEFINE TBIF_SIZE               := 0x00000040
DEFINE TBIF_BYINDEX            := 0x80000000 // this specifies that the wparam in Get/SetButtonInfo is an index, not id
DEFINE TB_GETBUTTONINFOW        := (WM_USER + 63)
DEFINE TB_SETBUTTONINFOW        := (WM_USER + 64)
DEFINE TB_GETBUTTONINFOA        := (WM_USER + 65)
DEFINE TB_SETBUTTONINFOA        := (WM_USER + 66)
DEFINE TB_GETBUTTONINFO         := TB_GETBUTTONINFOA
DEFINE TB_SETBUTTONINFO         := TB_SETBUTTONINFOA
DEFINE TB_INSERTBUTTONW        := (WM_USER + 67)
DEFINE TB_ADDBUTTONSW          := (WM_USER + 68)
DEFINE TB_HITTEST              := (WM_USER + 69)
DEFINE TB_INSERTBUTTON         := TB_INSERTBUTTONA
DEFINE TB_ADDBUTTONS           := TB_ADDBUTTONSA
DEFINE TB_SETDRAWTEXTFLAGS     := (WM_USER + 70)  // wParam == mask lParam == bit values
DEFINE TB_GETSTRINGW           := (WM_USER + 91)
DEFINE TB_GETSTRINGA           := (WM_USER + 92)
DEFINE TB_GETSTRING            := TB_GETSTRINGA
DEFINE TBMF_PAD                := 0x00000001
DEFINE TBMF_BARPAD             := 0x00000002
DEFINE TBMF_BUTTONSPACING      := 0x00000004
DEFINE TB_GETMETRICS           := (WM_USER + 101)
DEFINE TB_SETMETRICS           := (WM_USER + 102)
DEFINE TB_SETWINDOWTHEME       := CCM_SETWINDOWTHEME
DEFINE TBN_GETBUTTONINFOA  	:= (TBN_FIRST-0)
DEFINE TBN_GETBUTTONINFOW  	:= (TBN_FIRST-20)
DEFINE TBN_BEGINDRAG	 	:= (TBN_FIRST-1)
DEFINE TBN_ENDDRAG		:= (TBN_FIRST-2)
DEFINE TBN_BEGINADJUST		:= (TBN_FIRST-3)
DEFINE TBN_ENDADJUST		:= (TBN_FIRST-4)
DEFINE TBN_RESET		:= (TBN_FIRST-5)
DEFINE TBN_QUERYINSERT		:= (TBN_FIRST-6)
DEFINE TBN_QUERYDELETE		:= (TBN_FIRST-7)
DEFINE TBN_TOOLBARCHANGE	:= (TBN_FIRST-8)
DEFINE TBN_CUSTHELP 		:= (TBN_FIRST-9)
DEFINE TBN_DROPDOWN             := (TBN_FIRST - 10)
DEFINE TBN_GETOBJECT            := (TBN_FIRST - 12)
DEFINE TBN_GETBUTTONINFO	:= TBN_GETBUTTONINFOA
DEFINE TOOLTIPS_CLASS 				:= "tooltips_class32"
DEFINE TTS_ALWAYSTIP					 := 0x01
DEFINE TTS_NOPREFIX 					 := 0x02
DEFINE TTF_IDISHWND 					 := 0x01
DEFINE TTF_CENTERTIP					 := 0x02
DEFINE TTF_RTLREADING 				 := 0x04
DEFINE TTF_SUBCLASS 					 := 0x10
DEFINE TTDT_AUTOMATIC 				 := 0
DEFINE TTDT_RESHOW						 := 1
DEFINE TTDT_AUTOPOP 					 := 2
DEFINE TTDT_INITIAL 					 := 3
DEFINE TTM_ACTIVATE 					 := (WM_USER + 1)
DEFINE TTM_SETDELAYTIME 			 := (WM_USER + 3)
DEFINE TTM_ADDTOOL						:= (WM_USER + 4)
DEFINE TTM_ADDTOOLW 					 := (WM_USER + 50)
DEFINE TTM_DELTOOL						:= (WM_USER + 5)
DEFINE TTM_DELTOOLW 					 := (WM_USER + 51)
DEFINE TTM_NEWTOOLRECT				:= (WM_USER + 6)
DEFINE TTM_NEWTOOLRECTW 			 := (WM_USER + 52)
DEFINE TTM_RELAYEVENT 				 := (WM_USER + 7)
DEFINE TTM_GETTOOLINFO				:= (WM_USER + 8)
DEFINE TTM_GETTOOLINFOW 			 := (WM_USER + 53)
DEFINE TTM_SETTOOLINFO				:= (WM_USER + 9)
DEFINE TTM_SETTOOLINFOW 			 := (WM_USER + 54)
DEFINE TTM_HITTEST						:= (WM_USER +10)
DEFINE TTM_HITTESTW 					 := (WM_USER +55)
DEFINE TTM_GETTEXT						:= (WM_USER +11)
DEFINE TTM_GETTEXTW 					 := (WM_USER +56)
DEFINE TTM_UPDATETIPTEXT			:= (WM_USER +12)
DEFINE TTM_UPDATETIPTEXTW 		 := (WM_USER +57)
DEFINE TTM_GETTOOLCOUNT 			 := (WM_USER +13)
DEFINE TTM_ENUMTOOLS					:= (WM_USER +14)
DEFINE TTM_ENUMTOOLSW 				 := (WM_USER +58)
DEFINE TTM_GETCURRENTTOOL 		:= (WM_USER + 15)
DEFINE TTM_GETCURRENTTOOLW		 := (WM_USER + 59)
DEFINE TTM_winDOWFROMPOINT		 := (WM_USER + 16)
DEFINE TTN_NEEDTEXT 					:= (TTN_FIRST - 0)
DEFINE TTN_NEEDTEXTW					 := (TTN_FIRST - 10)
DEFINE TTN_SHOW 							 := (TTN_FIRST - 1)
DEFINE TTN_POP								 := (TTN_FIRST - 2)
DEFINE SBARS_SIZEGRIP 				 := 0x0100
DEFINE STATUSCLASSNAME		 := 	 "msctls_statusbar32"
DEFINE SB_SETTEXT 						:= (WM_USER+1)
DEFINE SB_SETTEXTW						:= (WM_USER+11)
DEFINE SB_GETTEXT 						:= (WM_USER+2)
DEFINE SB_GETTEXTW						:= (WM_USER+13)
DEFINE SB_GETTEXTLENGTH 			:= (WM_USER+3)
DEFINE SB_GETTEXTLENGTHW			:= (WM_USER+12)
DEFINE SB_SETPARTS						 := (WM_USER+4)
DEFINE SB_GETPARTS						 := (WM_USER+6)
DEFINE SB_GETBORDERS					 := (WM_USER+7)
DEFINE SB_SETMINHEIGHT				 := (WM_USER+8)
DEFINE SB_SIMPLE							 := (WM_USER+9)
DEFINE SB_GETRECT 						 := (WM_USER+10)
DEFINE SBT_OWNERDRAW					 :=  0x1000
DEFINE SBT_NOBORDERS					 :=  0x0100
DEFINE SBT_POPOUT 						 :=  0x0200
DEFINE SBT_RTLREADING 				 :=  0x0400
DEFINE MINSYSCOMMAND	 := SC_SIZE
DEFINE TRACKBAR_CLASS 				 :=  "msctls_trackbar32"
DEFINE TBS_AUTOTICKS					 := 0x0001
DEFINE TBS_VERT 							 := 0x0002
DEFINE TBS_HORZ 							 := 0x0000
DEFINE TBS_TOP								 := 0x0004
DEFINE TBS_BOTTOM 						 := 0x0000
DEFINE TBS_LEFT 							 := 0x0004
DEFINE TBS_RIGHT							 := 0x0000
DEFINE TBS_BOTH 							 := 0x0008
DEFINE TBS_NOTICKS						 := 0x0010
DEFINE TBS_ENABLESELRANGE 		 := 0x0020
DEFINE TBS_FIXEDLENGTH				 := 0x0040
DEFINE TBS_NOTHUMB						 := 0x0080
DEFINE TBS_TOOLTIPS            := 0x0100
DEFINE TBM_GETPOS 						 := (WM_USER)
DEFINE TBM_GETRANGEMIN				 := (WM_USER+1)
DEFINE TBM_GETRANGEMAX				 := (WM_USER+2)
DEFINE TBM_GETTIC 						 := (WM_USER+3)
DEFINE TBM_SETTIC 						 := (WM_USER+4)
DEFINE TBM_SETPOS 						 := (WM_USER+5)
DEFINE TBM_SETRANGE 					 := (WM_USER+6)
DEFINE TBM_SETRANGEMIN				 := (WM_USER+7)
DEFINE TBM_SETRANGEMAX				 := (WM_USER+8)
DEFINE TBM_CLEARTICS					 := (WM_USER+9)
DEFINE TBM_SETSEL 						 := (WM_USER+10)
DEFINE TBM_SETSELSTART				 := (WM_USER+11)
DEFINE TBM_SETSELEND					 := (WM_USER+12)
DEFINE TBM_GETPTICS 					 := (WM_USER+14)
DEFINE TBM_GETTICPOS					 := (WM_USER+15)
DEFINE TBM_GETNUMTICS 				 := (WM_USER+16)
DEFINE TBM_GETSELSTART				 := (WM_USER+17)
DEFINE TBM_GETSELEND					 := (WM_USER+18)
DEFINE TBM_CLEARSEL 					 := (WM_USER+19)
DEFINE TBM_SETTICFREQ 				 := (WM_USER+20)
DEFINE TBM_SETPAGESIZE				 := (WM_USER+21)
DEFINE TBM_GETPAGESIZE				 := (WM_USER+22)
DEFINE TBM_SETLINESIZE				 := (WM_USER+23)
DEFINE TBM_GETLINESIZE				 := (WM_USER+24)
DEFINE TBM_GETTHUMBRECT 			 := (WM_USER+25)
DEFINE TBM_GETCHANNELRECT 		 := (WM_USER+26)
DEFINE TBM_SETTHUMBLENGTH 		 := (WM_USER+27)
DEFINE TBM_GETTHUMBLENGTH 		 := (WM_USER+28)
DEFINE TB_LINEUP							 := 0
DEFINE TB_LINEDOWN						 := 1
DEFINE TB_PAGEUP							 := 2
DEFINE TB_PAGEDOWN						 := 3
DEFINE TB_THUMBPOSITION 			 := 4
DEFINE TB_THUMBTRACK					 := 5
DEFINE TB_TOP 								 := 6
DEFINE TB_BOTTOM							 := 7
DEFINE TB_ENDTRACK						 := 8
DEFINE DL_BEGINDRAG 					 := (WM_USER+133)
DEFINE DL_DRAGGING						 := (WM_USER+134)
DEFINE DL_DROPPED 						 := (WM_USER+135)
DEFINE DL_CANCELDRAG					 := (WM_USER+136)
DEFINE DL_CURSORSET 					 := 0
DEFINE DL_STOPCURSOR					 := 1
DEFINE DL_COPYCURSOR					 := 2
DEFINE DL_MOVECURSOR					 := 3
DEFINE DRAGLISTMSGSTRING			 := "commctrl_DragListMsg"
DEFINE UPDOWN_CLASS 			:=				"msctls_updown32"
DEFINE UD_MAXVAL							 := 0x7fff
DEFINE UD_MINVAL							 := (-UD_MAXVAL)
DEFINE UDS_WRAP 							 := 0x0001
DEFINE UDS_SETBUDDYINT				 := 0x0002
DEFINE UDS_ALIGNRIGHT 				 := 0x0004
DEFINE UDS_ALIGNLEFT					 := 0x0008
DEFINE UDS_AUTOBUDDY					 := 0x0010
DEFINE UDS_ARROWKEYS					 := 0x0020
DEFINE UDS_HORZ 							 := 0x0040
DEFINE UDS_NOTHOUSANDS				 := 0x0080
DEFINE UDM_SETRANGE 					 := (WM_USER+101)
DEFINE UDM_GETRANGE 					 := (WM_USER+102)
DEFINE UDM_SETPOS 						 := (WM_USER+103)
DEFINE UDM_GETPOS 						 := (WM_USER+104)
DEFINE UDM_SETBUDDY 					 := (WM_USER+105)
DEFINE UDM_GETBUDDY 					 := (WM_USER+106)
DEFINE UDM_SETACCEL 					 := (WM_USER+107)
DEFINE UDM_GETACCEL 					 := (WM_USER+108)
DEFINE UDM_SETBASE						 := (WM_USER+109)
DEFINE UDM_GETBASE						 := (WM_USER+110)
DEFINE UDN_DELTAPOS := (UDN_FIRST - 1)
DEFINE PROGRESS_CLASS 			:=	 "msctls_progress32"
DEFINE PBS_SMOOTH              := 0x01
DEFINE PBS_VERTICAL            := 0x04
DEFINE PBM_SETRANGE 					 := (WM_USER+1)
DEFINE PBM_SETPOS 						 := (WM_USER+2)
DEFINE PBM_DELTAPOS 					 := (WM_USER+3)
DEFINE PBM_SETSTEP						 := (WM_USER+4)
DEFINE PBM_STEPIT 						 := (WM_USER+5)
DEFINE PBM_SETRANGE32          := (WM_USER+6)
DEFINE PBM_GETRANGE            := (WM_USER+7)
DEFINE PBM_GETPOS              := (WM_USER+8)
DEFINE PBM_SETBARCOLOR         := (WM_USER+9)
DEFINE PBM_SETBKCOLOR          := CCM_SETBKCOLOR
DEFINE HOTKEYF_SHIFT					 := 0x01
DEFINE HOTKEYF_CONTROL				 := 0x02
DEFINE HOTKEYF_ALT						 := 0x04
DEFINE HOTKEYF_EXT						 := 0x08
DEFINE HKCOMB_NONE						 := 0x0001
DEFINE HKCOMB_S 							 := 0x0002
DEFINE HKCOMB_C 							 := 0x0004
DEFINE HKCOMB_A 							 := 0x0008
DEFINE HKCOMB_SC							 := 0x0010
DEFINE HKCOMB_SA							 := 0x0020
DEFINE HKCOMB_CA							 := 0x0040
DEFINE HKCOMB_SCA 						 := 0x0080
DEFINE HKM_SETHOTKEY					 := (WM_USER+1)
DEFINE HKM_GETHOTKEY					 := (WM_USER+2)
DEFINE HKM_SETRULES 					 := (WM_USER+3)
DEFINE HOTKEY_CLASS 		 := 		 "msctls_hotkey32"
DEFINE CCS_TOP								:=	0x00000001L
DEFINE CCS_NOMOVEY						:=	0x00000002L
DEFINE CCS_BOTTOM 						:=	0x00000003L
DEFINE CCS_NORESIZE 					:=	0x00000004L
DEFINE CCS_NOPARENTALIGN			:=	0x00000008L
DEFINE CCS_ADJUSTABLE 				:=	0x00000020L
DEFINE CCS_NODIVIDER					:=	0x00000040L
DEFINE WC_LISTVIEWA 					:=	"SysListView32"
DEFINE LVS_ICON 							 := 0x0000
DEFINE LVS_REPORT 						 := 0x0001
DEFINE LVS_SMALLICON					 := 0x0002
DEFINE LVS_LIST 							 := 0x0003
DEFINE LVS_TYPEMASK 					 := 0x0003
DEFINE LVS_SINGLESEL					 := 0x0004
DEFINE LVS_SHOWSELALWAYS			 := 0x0008
DEFINE LVS_SORTASCENDING			 := 0x0010
DEFINE LVS_SORTDESCENDING 		 := 0x0020
DEFINE LVS_SHAREIMAGELISTS		 := 0x0040
DEFINE LVS_NOLABELWRAP				 := 0x0080
DEFINE LVS_AUTOARRANGE				 := 0x0100
DEFINE LVS_EDITLABELS 				 := 0x0200
DEFINE LVS_OWNERDATA           := 0x1000
DEFINE LVS_NOSCROLL 					 := 0x2000
DEFINE LVS_TYPESTYLEMASK			 := 0xfc00
DEFINE LVS_ALIGNTOP 					 := 0x0000
DEFINE LVS_ALIGNLEFT					 := 0x0800
DEFINE LVS_ALIGNMASK					 := 0x0c00
DEFINE LVS_OWNERDRAWFIXED 		 := 0x0400
DEFINE LVS_NOCOLUMNHEADER 		 := 0x4000
DEFINE LVS_NOSORTHEADER 			 := 0x8000
DEFINE LVM_GETBKCOLOR 				 := (LVM_FIRST + 0)
DEFINE LVM_SETBKCOLOR 				 := (LVM_FIRST + 1)
DEFINE LVM_GETIMAGELIST 			:=	(LVM_FIRST + 2)
DEFINE LVSIL_NORMAL 					:=	0
DEFINE LVSIL_SMALL						:=	1
DEFINE LVSIL_STATE						:=	2
DEFINE LVM_SETIMAGELIST 			:=	(LVM_FIRST + 3)
DEFINE LVM_GETITEMCOUNT 			 := (LVM_FIRST + 4)
DEFINE LVIF_TEXT							 := 0x0001
DEFINE LVIF_IMAGE 						 := 0x0002
DEFINE LVIF_PARAM 						 := 0x0004
DEFINE LVIF_STATE 						 := 0x0008
DEFINE LVIF_INDENT             := 0x0010
DEFINE LVIF_NORECOMPUTE        := 0x0800
DEFINE LVIS_FOCUSED 					 := 0x0001
DEFINE LVIS_SELECTED					 := 0x0002
DEFINE LVIS_CUT 							 := 0x0004
DEFINE LVIS_DROPHILITED 			 := 0x0008
DEFINE LVIS_OVERLAYMASK 			 := 0x0F00
DEFINE LVIS_STATEIMAGEMASK		 := 0xF000
DEFINE LPSTR_TEXTCALLBACK 		 :=PSZ(_CAST, 0xFFFFFFFF)
DEFINE I_IMAGECALLBACK				 := (-1)
DEFINE LVM_GETITEM						:=		(LVM_FIRST + 5)
DEFINE LVM_SETITEM			 := 		 (LVM_FIRST + 6)
DEFINE LVM_INSERTITEM 			:=		(LVM_FIRST + 7)
DEFINE LVM_DELETEITEM 				 := (LVM_FIRST + 8)
DEFINE LVM_DELETEALLITEMS 		 := (LVM_FIRST + 9)
DEFINE LVM_GETCALLBACKMASK		 := (LVM_FIRST + 10)
DEFINE LVM_SETCALLBACKMASK		 := (LVM_FIRST + 11)
DEFINE LVNI_ALL 							 := 0x0000
DEFINE LVNI_FOCUSED 					 := 0x0001
DEFINE LVNI_SELECTED					 := 0x0002
DEFINE LVNI_CUT 							 := 0x0004
DEFINE LVNI_DROPHILITED 			 := 0x0008
DEFINE LVNI_ABOVE 						 := 0x0100
DEFINE LVNI_BELOW 						 := 0x0200
DEFINE LVNI_TOLEFT						 := 0x0400
DEFINE LVNI_TORIGHT 					 := 0x0800
DEFINE LVM_GETNEXTITEM				 := (LVM_FIRST + 12)
DEFINE LVFI_PARAM 						 := 0x0001
DEFINE LVFI_STRING						 := 0x0002
DEFINE LVFI_PARTIAL 					 := 0x0008
DEFINE LVFI_WRAP							 := 0x0020
DEFINE LVFI_NEARESTXY 				 := 0x0040
DEFINE LVM_FINDITEM 			:=			(LVM_FIRST + 13)
DEFINE LVIR_BOUNDS						 := 0
DEFINE LVIR_ICON							 := 1
DEFINE LVIR_LABEL 						 := 2
DEFINE LVIR_SELECTBOUNDS			 := 3
DEFINE LVM_GETITEMRECT				 := (LVM_FIRST + 14)
DEFINE LVM_SETITEMPOSITION		 := (LVM_FIRST + 15)
DEFINE LVM_GETITEMPOSITION		 := (LVM_FIRST + 16)
DEFINE LVM_GETSTRINGWIDTH 		:=	(LVM_FIRST + 17)
DEFINE LVHT_NOWHERE 					 := 0x0001
DEFINE LVHT_ONITEMICON				 := 0x0002
DEFINE LVHT_ONITEMLABEL 			 := 0x0004
DEFINE LVHT_ONITEMSTATEICON 	 := 0x0008
DEFINE LVHT_ONITEM 					 :=  0x000E
DEFINE LVHT_ABOVE 						 := 0x0008
DEFINE LVHT_BELOW 						 := 0x0010
DEFINE LVHT_TORIGHT 					 := 0x0020
DEFINE LVHT_TOLEFT						 := 0x0040
DEFINE LVM_HITTEST						 := (LVM_FIRST + 18)
DEFINE LVM_ENSUREVISIBLE			 := (LVM_FIRST + 19)
DEFINE LVM_SCROLL 						 := (LVM_FIRST + 20)
DEFINE LVM_REDRAWITEMS				 := (LVM_FIRST + 21)
DEFINE LVA_DEFAULT						 := 0x0000
DEFINE LVA_ALIGNLEFT					 := 0x0001
DEFINE LVA_ALIGNTOP 					 := 0x0002
DEFINE LVA_SNAPTOGRID 				 := 0x0005
DEFINE LVM_ARRANGE						 := (LVM_FIRST + 22)
DEFINE LVM_EDITLABEL					:= (LVM_FIRST + 23)
DEFINE LVM_GETEDITCONTROL 		 := (LVM_FIRST + 24)
DEFINE LVCF_FMT 							 := 0x0001
DEFINE LVCF_WIDTH 						 := 0x0002
DEFINE LVCF_TEXT							 := 0x0004
DEFINE LVCF_SUBITEM 					 := 0x0008
DEFINE LVCFMT_LEFT						 := 0x0000
DEFINE LVCFMT_RIGHT 					 := 0x0001
DEFINE LVCFMT_CENTER					 := 0x0002
DEFINE LVCFMT_JUSTIFYMASK 		 := 0x0003
DEFINE LVM_GETCOLUMN		 := 		 (LVM_FIRST + 25)
DEFINE LVM_SETCOLUMN			:=		(LVM_FIRST + 26)
DEFINE LVM_INSERTCOLUMN  := 		 (LVM_FIRST + 27)
DEFINE LVM_DELETECOLUMN 			 := (LVM_FIRST + 28)
DEFINE LVM_GETCOLUMNWIDTH 		 := (LVM_FIRST + 29)
DEFINE LVSCW_AUTOSIZE 						 := -1
DEFINE LVSCW_AUTOSIZE_USEHEADER 	 := -2
DEFINE LVM_SETCOLUMNWIDTH 				 := (LVM_FIRST + 30)
DEFINE LVM_CREATEDRAGIMAGE				 := (LVM_FIRST + 33)
DEFINE LVM_GETVIEWRECT						:= (LVM_FIRST + 34)
DEFINE LVM_GETTEXTCOLOR 				 := (LVM_FIRST + 35)
DEFINE LVM_SETTEXTCOLOR 			 := (LVM_FIRST + 36)
DEFINE LVM_GETTEXTBKCOLOR 		 := (LVM_FIRST + 37)
DEFINE LVM_SETTEXTBKCOLOR 		 := (LVM_FIRST + 38)
DEFINE LVM_GETTOPINDEX				 := (LVM_FIRST + 39)
DEFINE LVM_GETCOUNTPERPAGE		:= (LVM_FIRST + 40)
DEFINE LVM_GETORIGIN					:=	(LVM_FIRST + 41)
DEFINE LVM_UPDATE 						:=	(LVM_FIRST + 42)
DEFINE LVM_SETITEMSTATE 			 :=(LVM_FIRST + 43)
DEFINE LVM_GETITEMSTATE 			 := (LVM_FIRST + 44)
DEFINE LVM_GETITEMTEXT				:= (LVM_FIRST + 45)
DEFINE LVM_SETITEMTEXT			 := (LVM_FIRST + 46)
DEFINE LVSICF_NOINVALIDATEALL  := 0x00000001
DEFINE LVSICF_NOSCROLL         := 0x00000002
DEFINE LVM_SETITEMCOUNT 			 := (LVM_FIRST + 47)
DEFINE LVM_SORTITEMS					 := (LVM_FIRST + 48)
DEFINE LVM_SETITEMPOSITION32	 := (LVM_FIRST + 49)
DEFINE LVM_GETSELECTEDCOUNT 	 := (LVM_FIRST + 50)
DEFINE LVM_GETITEMSPACING 		 := (LVM_FIRST + 51)
DEFINE LVM_GETISEARCHSTRINGW	 := (LVM_FIRST + 117)
DEFINE LVM_GETISEARCHSTRING 	:= (LVM_FIRST + 52)
DEFINE LVCDI_ITEM      := 0x00000000
DEFINE LVCDI_GROUP     := 0x00000001
	// ListView custom draw return values
DEFINE LVCDRF_NOSELECT             := 0x00010000
DEFINE LVCDRF_NOGROUPFRAME         := 0x00020000  
DEFINE LVN_ITEMCHANGING 		 := 	(LVN_FIRST-0)
DEFINE LVN_ITEMCHANGED				:=	(LVN_FIRST-1)
DEFINE LVN_INSERTITEM 					:=(LVN_FIRST-2)
DEFINE LVN_DELETEITEM 					:=(LVN_FIRST-3)
DEFINE LVN_DELETEALLITEMS 		:=	(LVN_FIRST-4)
DEFINE LVN_BEGINLABELEDIT 		 :=(LVN_FIRST-5)
DEFINE LVN_BEGINLABELEDITW		 := (LVN_FIRST-75)
DEFINE LVN_ENDLABELEDIT 			:=(LVN_FIRST-6)
DEFINE LVN_ENDLABELEDITW			 := (LVN_FIRST-76)
DEFINE LVN_COLUMNCLICK					:=(LVN_FIRST-8)
DEFINE LVN_BEGINDRAG						:=(LVN_FIRST-9)
DEFINE LVN_BEGINRDRAG 					:=(LVN_FIRST-11)
DEFINE LVN_ODCACHEHINT         := (LVN_FIRST-13)
DEFINE LVN_ODFINDITEM         := (LVN_FIRST-52)
DEFINE LVN_ITEMACTIVATE        := (LVN_FIRST-14)
DEFINE LVN_ODSTATECHANGED      := (LVN_FIRST-15)
DEFINE LVN_GETDISPINFO				 :=(LVN_FIRST-50)
DEFINE LVN_GETDISPINFOW 				:=(LVN_FIRST-77)
DEFINE LVN_SETDISPINFO				 :=(LVN_FIRST-51)
DEFINE LVN_SETDISPINFOW 				:=(LVN_FIRST-78)
DEFINE LVIF_DI_SETITEM					:= 0x1000
DEFINE LVN_KEYDOWN				:=  (LVN_FIRST-55)
DEFINE TVS_HASBUTTONS 				 := 0x0001
DEFINE TVS_HASLINES 					 := 0x0002
DEFINE TVS_LINESATROOT				 := 0x0004
DEFINE TVS_EDITLABELS 				 := 0x0008
DEFINE TVS_DISABLEDRAGDROP		 := 0x0010
DEFINE TVS_SHOWSELALWAYS			 := 0x0020
DEFINE TVIF_TEXT							 := 0x0001
DEFINE TVIF_IMAGE 						 := 0x0002
DEFINE TVIF_PARAM 						 := 0x0004
DEFINE TVIF_STATE 						 := 0x0008
DEFINE TVIF_HANDLE						 := 0x0010
DEFINE TVIF_SELECTEDIMAGE 		 := 0x0020
DEFINE TVIF_CHILDREN					 := 0x0040
DEFINE TVIS_FOCUSED 					 := 0x0001
DEFINE TVIS_SELECTED					 := 0x0002
DEFINE TVIS_CUT 							 := 0x0004
DEFINE TVIS_DROPHILITED 			 := 0x0008
DEFINE TVIS_BOLD							 := 0x0010
DEFINE TVIS_EXPANDED					 := 0x0020
DEFINE TVIS_EXPANDEDONCE			 := 0x0040
DEFINE TVIS_OVERLAYMASK 			 := 0x0F00
DEFINE TVIS_STATEIMAGEMASK		 := 0xF000
DEFINE TVIS_USERMASK					 := 0xF000
DEFINE I_CHILDRENCALLBACK 		:= (-1)
DEFINE TVI_ROOT 							 := PTR(_CAST, 0xFFFF0000)
DEFINE TVI_FIRST							 := PTR(_CAST, 0xFFFF0001)
DEFINE TVI_LAST 							 := PTR(_CAST, 0xFFFF0002)
DEFINE TVI_SORT 							 := PTR(_CAST, 0xFFFF0003)
DEFINE TVM_INSERTITEM 			 :=  (TV_FIRST + 0)
DEFINE TVM_DELETEITEM 			 := 	(TV_FIRST + 1)
DEFINE TVM_EXPAND 						 := (TV_FIRST + 2)
DEFINE TVE_COLLAPSE 					 := 0x0001
DEFINE TVE_EXPAND 						 := 0x0002
DEFINE TVE_TOGGLE 						 := 0x0003
DEFINE TVE_COLLAPSERESET			 := 0x8000
DEFINE TVM_GETITEMRECT				 := (TV_FIRST + 4)
DEFINE TVM_GETCOUNT 					 := (TV_FIRST + 5)
DEFINE TVM_GETINDENT					 := (TV_FIRST + 6)
DEFINE TVM_SETINDENT					 := (TV_FIRST + 7)
DEFINE TVM_GETIMAGELIST 			 := (TV_FIRST + 8)
DEFINE TVSIL_NORMAL 					:= 0
DEFINE TVSIL_STATE						:= 2
DEFINE TVM_SETIMAGELIST 			 := (TV_FIRST + 9)
DEFINE TVM_GETNEXTITEM				 := (TV_FIRST + 10)
DEFINE TVGN_ROOT							 := 0x0000
DEFINE TVGN_NEXT							 := 0x0001
DEFINE TVGN_PREVIOUS					 := 0x0002
DEFINE TVGN_PARENT						 := 0x0003
DEFINE TVGN_CHILD 						 := 0x0004
DEFINE TVGN_FIRSTVISIBLE			 := 0x0005
DEFINE TVGN_NEXTVISIBLE 			 := 0x0006
DEFINE TVGN_PREVIOUSVISIBLE 	 := 0x0007
DEFINE TVGN_DROPHILITE				 := 0x0008
DEFINE TVGN_CARET 						 := 0x0009
DEFINE TVM_SELECTITEM 				:=	(TV_FIRST + 11)
DEFINE TVM_GETITEM						:= (TV_FIRST + 12)
DEFINE TVM_SETITEM				 := 	 (TV_FIRST + 13)
DEFINE TVM_EDITLABEL			:=			 (TV_FIRST + 14)
DEFINE TVM_GETEDITCONTROL 	:=	 (TV_FIRST + 15)
DEFINE TVM_GETVISIBLECOUNT	 := 	(TV_FIRST + 16)
DEFINE TVM_HITTEST					 := 	(TV_FIRST + 17)
DEFINE TVHT_NOWHERE 					 := 0x0001
DEFINE TVHT_ONITEMICON				 := 0x0002
DEFINE TVHT_ONITEMLABEL 			 := 0x0004
DEFINE TVHT_ONITEM						 := 0x0046
DEFINE TVHT_ONITEMINDENT			 := 0x0008
DEFINE TVHT_ONITEMBUTTON			 := 0x0010
DEFINE TVHT_ONITEMRIGHT 			 := 0x0020
DEFINE TVHT_ONITEMSTATEICON 	 := 0x0040
DEFINE TVHT_ABOVE 						 := 0x0100
DEFINE TVHT_BELOW 						 := 0x0200
DEFINE TVHT_TORIGHT 					 := 0x0400
DEFINE TVHT_TOLEFT						 := 0x0800
DEFINE TVM_CREATEDRAGIMAGE		 := (TV_FIRST + 18)
DEFINE TVM_SORTCHILDREN 			 := (TV_FIRST + 19)
DEFINE TVM_ENSUREVISIBLE			 := (TV_FIRST + 20)
DEFINE TVM_SORTCHILDRENCB 		:=	(TV_FIRST + 21)
DEFINE TVM_ENDEDITLABELNOW		 := (TV_FIRST + 22)
DEFINE TVM_GETISEARCHSTRING 	:= (TV_FIRST + 23)
DEFINE TVN_SELCHANGINGA 			 := (TVN_FIRST-1)
DEFINE TVN_SELCHANGINGW 			 := (TVN_FIRST-50)
DEFINE TVN_SELCHANGEDA				 := (TVN_FIRST-2)
DEFINE TVN_SELCHANGEDW				 := (TVN_FIRST-51)
DEFINE TVC_UNKNOWN						:=	0x0000
DEFINE TVC_BYMOUSE						:=	0x0001
DEFINE TVC_BYKEYBOARD 				:=	0x0002
DEFINE TVN_GETDISPINFOA 			:=	(TVN_FIRST-3)
DEFINE TVN_GETDISPINFOW 			:=	(TVN_FIRST-52)
DEFINE TVN_SETDISPINFOA 			:=	(TVN_FIRST-4)
DEFINE TVN_SETDISPINFOW 			:=	(TVN_FIRST-53)
DEFINE TVIF_DI_SETITEM				:=	0x1000
DEFINE TVN_ITEMEXPANDING	 := 	 (TVN_FIRST-5)
DEFINE TVN_ITEMEXPANDED 	 := 	 (TVN_FIRST-6)
DEFINE TVN_BEGINDRAG			 := 	 (TVN_FIRST-7)
DEFINE TVN_BEGINRDRAG 		 := 	 (TVN_FIRST-8)
DEFINE TVN_DELETEITEM 		 := 	 (TVN_FIRST-9)
DEFINE TVN_BEGINLABELEDIT  := 	 (TVN_FIRST-10)
DEFINE TVN_ENDLABELEDIT 	 := 	 (TVN_FIRST-11)
DEFINE TVN_KEYDOWN				 := 	 (TVN_FIRST-12)
DEFINE WC_TABCONTROL	 := 			 "SysTabControl32"
DEFINE TCS_SCROLLOPPOSITE      := 0x0001   // assumes multiline tab
DEFINE TCS_BOTTOM              := 0x0002
DEFINE TCS_RIGHT               := 0x0002
DEFINE TCS_MULTISELECT         := 0x0004  // allow multi-select in button mode
DEFINE TCS_FORCEICONLEFT       := 0x0010
DEFINE TCS_FORCELABELLEFT      := 0x0020
DEFINE TCS_HOTTRACK            := 0x0040
DEFINE TCS_VERTICAL            := 0x0080
DEFINE TCS_TABS                := 0x0000
DEFINE TCS_BUTTONS             := 0x0100
DEFINE TCS_SINGLELINE          := 0x0000
DEFINE TCS_MULTILINE           := 0x0200
DEFINE TCS_RIGHTJUSTIFY        := 0x0000
DEFINE TCS_FIXEDWIDTH          := 0x0400
DEFINE TCS_RAGGEDRIGHT         := 0x0800
DEFINE TCS_FOCUSONBUTTONDOWN   := 0x1000
DEFINE TCS_OWNERDRAWFIXED      := 0x2000
DEFINE TCS_TOOLTIPS            := 0x4000
DEFINE TCS_FOCUSNEVER          := 0x8000
DEFINE TCM_FIRST							 := 0x1300
DEFINE TCM_GETIMAGELIST 			 := (TCM_FIRST + 2)
DEFINE TCM_SETIMAGELIST 			 := (TCM_FIRST + 3)
DEFINE TCM_GETITEMCOUNT 			 := (TCM_FIRST + 4)
DEFINE TCIF_TEXT							 := 0x0001
DEFINE TCIF_IMAGE 						 := 0x0002
DEFINE TCIF_RTLREADING				 := 0x0004
DEFINE TCIF_PARAM 						 := 0x0008
DEFINE TCM_GETITEM					 := (TCM_FIRST + 5)
DEFINE TCM_SETITEM			:=			(TCM_FIRST + 6)
DEFINE TCM_INSERTITEM 	 := 		 (TCM_FIRST + 7)
DEFINE TCM_DELETEITEM 	 := 		 (TCM_FIRST + 8)
DEFINE TCM_DELETEALLITEMS 		 := (TCM_FIRST + 9)
DEFINE TCM_GETITEMRECT				 := (TCM_FIRST + 10)
DEFINE TCM_GETCURSEL					 := (TCM_FIRST + 11)
DEFINE TCM_SETCURSEL					 := (TCM_FIRST + 12)
DEFINE TCHT_NOWHERE 					 := 0x0001
DEFINE TCHT_ONITEMICON				 := 0x0002
DEFINE TCHT_ONITEMLABEL 			 := 0x0004
DEFINE TCHT_ONITEM						:= 0x0006
DEFINE TCM_HITTEST						 := (TCM_FIRST + 13)
DEFINE TCM_SETITEMEXTRA 			 := (TCM_FIRST + 14)
DEFINE TCM_ADJUSTRECT 				 := (TCM_FIRST + 40)
DEFINE TCM_SETITEMSIZE				 := (TCM_FIRST + 41)
DEFINE TCM_REMOVEIMAGE				 := (TCM_FIRST + 42)
DEFINE TCM_SETPADDING 				 := (TCM_FIRST + 43)
DEFINE TCM_GETROWCOUNT				 := (TCM_FIRST + 44)
DEFINE TCM_GETTOOLTIPS				 := (TCM_FIRST + 45)
DEFINE TCM_SETTOOLTIPS				 := (TCM_FIRST + 46)
DEFINE TCM_GETCURFOCUS				 := (TCM_FIRST + 47)
DEFINE TCM_SETCURFOCUS				 := (TCM_FIRST + 48)
DEFINE TCN_KEYDOWN             := (TCN_FIRST - 0)
DEFINE TCN_SELCHANGE					 := (TCN_FIRST - 1)
DEFINE TCN_SELCHANGING				 := (TCN_FIRST - 2)
DEFINE ANIMATE_CLASS 	:=				"SysAnimate32"
DEFINE ACS_CENTER 						 := 0x0001
DEFINE ACS_TRANSPARENT				 := 0x0002
DEFINE ACS_AUTOPLAY 					 := 0x0004
DEFINE ACM_OPENW							 := (WM_USER+103)
DEFINE ACM_OPEN 							:= (WM_USER+100)
DEFINE ACM_PLAY 							:=	(WM_USER+101)
DEFINE ACM_STOP 							:=	(WM_USER+102)
DEFINE ACN_START							:=	1
DEFINE ACN_STOP 							:=	2
DEFINE ICC_LISTVIEW_CLASSES := 0x00000001 // listview, header
DEFINE ICC_TREEVIEW_CLASSES := 0x00000002 // treeview, tooltips
DEFINE ICC_BAR_CLASSES      := 0x00000004 // toolbar, statusbar, trackbar, tooltips
DEFINE ICC_TAB_CLASSES      := 0x00000008 // tab, tooltips
DEFINE ICC_UPDOWN_CLASS     := 0x00000010 // updown
DEFINE ICC_PROGRESS_CLASS   := 0x00000020 // progress
DEFINE ICC_HOTKEY_CLASS     := 0x00000040 // hotkey
DEFINE ICC_ANIMATE_CLASS    := 0x00000080 // animate
DEFINE ICC_win95_CLASSES    := 0x000000FF
DEFINE ICC_DATE_CLASSES     := 0x00000100 // month picker, date picker, time picker, updown
DEFINE ICC_USEREX_CLASSES   := 0x00000200 // comboex
DEFINE ICC_COOL_CLASSES     := 0x00000400 // rebar (coolbar) control
DEFINE ICC_INTERNET_CLASSES := 0x00000800
DEFINE ICC_PAGESCROLLER_CLASS := 0x00001000   // page scroller
DEFINE ICC_NATIVEFNTCTL_CLASS := 0x00002000   // native font control
DEFINE ICC_STANDARD_CLASSES   := 0x00004000
DEFINE ICC_LINK_CLASS         := 0x00008000
DEFINE TBSTYLE_FLAT        := 0x0800
DEFINE TTM_SETMAXTIPWIDTH  := (WM_USER + 24)
DEFINE TTM_GETMAXTIPWIDTH  := (WM_USER + 25)
	// MonthCalendar Control
DEFINE MCN_FIRST  :=     (0U-750U)       // monthcal
DEFINE MCN_LAST   :=     (0U-759U)
DEFINE MCM_FIRST  := 0x1000
	// BOOL MonthCal_GetCurSel(HWND hmc, LPSYSTEMTIME pst)
	//   returns FALSE if MCS_MULTISELECT
	//   returns TRUE and sets *pst to the currently selected date otherwise
DEFINE MCM_GETCURSEL       := (MCM_FIRST + 1)
	// BOOL MonthCal_SetCurSel(HWND hmc, LPSYSTEMTIME pst)
	//   returns FALSE if MCS_MULTISELECT
	//   returns TURE and sets the currently selected date to *pst otherwise
DEFINE MCM_SETCURSEL       := (MCM_FIRST + 2)
	// DWORD MonthCal_GetMaxSelCount(HWND hmc)
	//   returns the maximum number of selectable days allowed
DEFINE MCM_GETMAXSELCOUNT  := (MCM_FIRST + 3)
	// BOOL MonthCal_SetMaxSelCount(HWND hmc, UINT n)
	//   sets the max number days that can be selected iff MCS_MULTISELECT
DEFINE MCM_SETMAXSELCOUNT  := (MCM_FIRST + 4)
	// BOOL MonthCal_GetSelRange(HWND hmc, LPSYSTEMTIME rgst)
	//   sets rgst[0] to the first day of the selection range
	//   sets rgst[1] to the last day of the selection range
DEFINE MCM_GETSELRANGE     := (MCM_FIRST + 5)
	// BOOL MonthCal_SetSelRange(HWND hmc, LPSYSTEMTIME rgst)
	//   selects the range of days from rgst[0] to rgst[1]
DEFINE MCM_SETSELRANGE     := (MCM_FIRST + 6)
	// DWORD MonthCal_GetMonthRange(HWND hmc, DWORD gmr, LPSYSTEMTIME rgst)
	//   if rgst specified, sets rgst[0] to the starting date and
	//      and rgst[1] to the ending date of the the selectable (non-grayed)
	//      days if GMR_VISIBLE or all the displayed days (including grayed)
	//      if GMR_DAYSTATE.
	//   returns the number of months spanned by the above range.
DEFINE MCM_GETMONTHRANGE   := (MCM_FIRST + 7)
	// BOOL MonthCal_SetDayState(HWND hmc, int cbds, DAYSTATE *rgds)
	//   cbds is the count of DAYSTATE items in rgds and it must be equal
	//   to the value returned from MonthCal_GetMonthRange(hmc, GMR_DAYSTATE, NULL)
	//   This sets the DAYSTATE bits for each month (grayed and non-grayed
	//   days) displayed in the calendar. The first bit in a month's DAYSTATE
	//   corresponts to bolding day 1, the second bit affects day 2, etc.
DEFINE MCM_SETDAYSTATE     := (MCM_FIRST + 8)
	// BOOL MonthCal_GetMinReqRect(HWND hmc, LPRECT prc)
	//   sets *prc the minimal size needed to display one month
DEFINE MCM_GETMINREQRECT   := (MCM_FIRST + 9)
	// set what day is "today"   send NULL to revert back to real date
DEFINE MCM_SETTODAY    := (MCM_FIRST + 12)
	// get what day is "today"
	// returns BOOL for success/failure
DEFINE MCM_GETTODAY    := (MCM_FIRST + 13)
	// determine what pinfo->pt is over
DEFINE MCM_HITTEST          := (MCM_FIRST + 14)
DEFINE MCHT_TITLE                      := 0x00010000
DEFINE MCHT_CALENDAR                   := 0x00020000
DEFINE MCHT_TODAYLINK                  := 0x00030000
DEFINE MCHT_NEXT                       := 0x01000000   // these indicate that hitting
DEFINE MCHT_PREV                       := 0x02000000  // here will go to the next/prev month
DEFINE MCHT_NOWHERE                    := 0x00000000
DEFINE MCHT_TITLEBK                    := 0x00010000
DEFINE MCHT_TITLEMONTH                 := 0x00010001
DEFINE MCHT_TITLEYEAR                  := 0x00010002
DEFINE MCHT_TITLEBTNNEXT               := 0x01010003
DEFINE MCHT_TITLEBTNPREV               := 0x02010003
DEFINE MCHT_CALENDARBK                 := 0x00020000
DEFINE MCHT_CALENDARDATE               := 0x00020001
DEFINE MCHT_CALENDARDATENEXT           := 0x01020001
DEFINE MCHT_CALENDARDATEPREV           := 0x02020001
DEFINE MCHT_CALENDARDAY                := 0x00020002
DEFINE MCHT_CALENDARWEEKNUM            := 0x00020003
	// set colors to draw control with -- see MCSC_ bits below
DEFINE MCM_SETCOLOR            := (MCM_FIRST + 10)
DEFINE MCM_GETCOLOR            := (MCM_FIRST + 11)
DEFINE MCSC_BACKGROUND   := 0   // the background color (between months)
DEFINE MCSC_TEXT         := 1   // the dates
DEFINE MCSC_TITLEBK      := 2   // background of the title
DEFINE MCSC_TITLETEXT    := 3
DEFINE MCSC_MONTHBK      := 4   // background within the month cal
DEFINE MCSC_TRAILINGTEXT := 5   // the text color of header & trailing days
	// set first day of week to iDay:
	// 0 for Monday, 1 for Tuesday, ..., 6 for Sunday
	// -1 for means use locale info
DEFINE MCM_SETFIRSTDAYOFWEEK := (MCM_FIRST + 15)
	// DWORD result...  low word has the day.  high word is bool if this is app set
	// or not (FALSE == using locale info)
DEFINE MCM_GETFIRSTDAYOFWEEK := (MCM_FIRST + 16)
	// DWORD MonthCal_GetRange(HWND hmc, LPSYSTEMTIME rgst)
	//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
	//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
	//   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit
DEFINE MCM_GETRANGE := (MCM_FIRST + 17)
	// BOOL MonthCal_SetRange(HWND hmc, DWORD gdtr, LPSYSTEMTIME rgst)
	//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
	//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
	//   returns TRUE on success, FALSE on error (such as invalid parameters)
DEFINE MCM_SETRANGE := (MCM_FIRST + 18)
	// int MonthCal_GetMonthDelta(HWND hmc)
	//   returns the number of months one click on a next/prev button moves by
DEFINE MCM_GETMONTHDELTA := (MCM_FIRST + 19)
	// int MonthCal_SetMonthDelta(HWND hmc, int n)
	//   sets the month delta to n. n==0 reverts to moving by a page of months
	//   returns the previous value of n.
DEFINE MCM_SETMONTHDELTA := (MCM_FIRST + 20)
	// MCN_SELCHANGE is sent whenever the currently displayed date changes
	// via month change, year change, keyboard navigation, prev/next button
	//
DEFINE MCN_SELCHANGE       := (MCN_FIRST + 1)
	// MCN_GETDAYSTATE is sent for MCS_DAYSTATE controls whenever new daystate
	// information is needed (month or year scroll) to draw bolding information.
	// The app must fill in cDayState months worth of information starting from
	// stStart date. The app may fill in the array at prgDayState or change
	// prgDayState to point to a different array out of which the information
	// will be copied. (similar to tooltips)
	//
DEFINE MCN_GETDAYSTATE     := (MCN_FIRST + 3)
	// MCN_SELECT is sent whenever a selection has occured (via mouse or keyboard)
	//
	//typedef NMSELCHANGE NMSELECT, FAR * LPNMSELECT;
DEFINE MCN_SELECT  := (MCN_FIRST + 4)
DEFINE MCS_DAYSTATE        := 0x0001
DEFINE MCS_MULTISELECT     := 0x0002
DEFINE MCS_WEEKNUMBERS     := 0x0004
DEFINE MCS_NOTODAYCIRCLE   := 0x0008
DEFINE MCS_NOTODAY         := 0x0010
DEFINE GMR_VISIBLE     := 0       // visible portion of display
DEFINE GMR_DAYSTATE    := 1       // above plus the grayed out parts of
	// partially displayed months
	///////////////////////////////////////////////////////////////////////
	// Date Time Picker Control
	///////////////////////////////////////////////////////////////////////
DEFINE DTN_FIRST               := (0U-760U)       // datetimepick
DEFINE DTN_LAST                := (0U-799U)
DEFINE DTM_FIRST        := 0x1000
	// DWORD DateTimePick_GetSystemtime(HWND hdp, LPSYSTEMTIME pst)
	//   returns GDT_NONE if "none" is selected (DTS_SHOWNONE only)
	//   returns GDT_VALID and modifies *pst to be the currently selected value
DEFINE DTM_GETSYSTEMTIME   := (DTM_FIRST + 1)
	//define DateTime_GetSystemtime(hdp, pst)    (DWORD)SNDMSG(hdp, DTM_GETSYSTEMTIME, 0, (LPARAM)(pst))
	// BOOL DateTime_SetSystemtime(HWND hdp, DWORD gd, LPSYSTEMTIME pst)
	//   if gd==GDT_NONE, sets datetimepick to None (DTS_SHOWNONE only)
	//   if gd==GDT_VALID, sets datetimepick to *pst
	//   returns TRUE on success, FALSE on error (such as bad params)
DEFINE DTM_SETSYSTEMTIME   := (DTM_FIRST + 2)
	//define DateTime_SetSystemtime(hdp, gd, pst)    (BOOL)SNDMSG(hdp, DTM_SETSYSTEMTIME, (LPARAM)(gd), (LPARAM)(pst))
	// DWORD DateTime_GetRange(HWND hdp, LPSYSTEMTIME rgst)
	//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
	//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
	//   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit
DEFINE DTM_GETRANGE := (DTM_FIRST + 3)
	//define DateTime_GetRange(hdp, rgst)  (DWORD)SNDMSG(hdp, DTM_GETRANGE, 0, (LPARAM)(rgst))
	// BOOL DateTime_SetRange(HWND hdp, DWORD gdtr, LPSYSTEMTIME rgst)
	//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
	//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
	//   returns TRUE on success, FALSE on error (such as invalid parameters)
DEFINE DTM_SETRANGE := (DTM_FIRST + 4)
	//define DateTime_SetRange(hdp, gd, rgst)  (BOOL)SNDMSG(hdp, DTM_SETRANGE, (WPARAM)(gd), (LPARAM)(rgst))
	// BOOL DateTime_SetFormat(HWND hdp, LPCTSTR sz)
	//   sets the display formatting string to sz (see GetDateFormat and GetTimeFormat for valid formatting chars)
	//   NOTE: 'X' is a valid formatting character which indicates that the application
	//   will determine how to display information. Such apps must support DTN_WMKEYDOWN,
	//   DTN_FORMAT, and DTN_FORMATQUERY.
DEFINE DTM_SETFORMAT  := (DTM_FIRST + 5)
	//define DateTime_SetFormat(hdp, sz)  (BOOL)SNDMSG(hdp, DTM_SETFORMAT, 0, (LPARAM)(sz))
DEFINE DTM_SETMCCOLOR    := (DTM_FIRST + 6)
	//define DateTime_SetMonthCalColor(hdp, iColor, clr) SNDMSG(hdp, DTM_SETMCCOLOR, iColor, clr)
DEFINE DTM_GETMCCOLOR    := (DTM_FIRST + 7)
	//define DateTime_GetMonthCalColor(hdp, iColor) SNDMSG(hdp, DTM_GETMCCOLOR, iColor, 0)
	// HWND DateTime_GetMonthCal(HWND hdp)
	//   returns the HWND of the MonthCal popup window. Only valid
	// between DTN_DROPDOWN and DTN_CLOSEUP notifications.
DEFINE DTM_GETMONTHCAL   := (DTM_FIRST + 8)
	//define DateTime_GetMonthCal(hdp) (HWND)SNDMSG(hdp, DTM_GETMONTHCAL, 0, 0)
DEFINE DTM_SETMCFONT     := (DTM_FIRST + 9)
	//define DateTime_SetMonthCalFont(hdp, hfont, fRedraw) SNDMSG(hdp, DTM_SETMCFONT, (WPARAM)hfont, (LPARAM)fRedraw)
DEFINE DTM_GETMCFONT     := (DTM_FIRST + 10)
	//define DateTime_GetMonthCalFont(hdp) SNDMSG(hdp, DTM_GETMCFONT, 0, 0)
DEFINE DTS_UPDOWN          := 0x0001 // use UPDOWN instead of MONTHCAL
DEFINE DTS_SHOWNONE        := 0x0002 // allow a NONE selection
DEFINE DTS_SHORTDATEFORMAT := 0x0000 // use the short date format (app must forward WM_winINICHANGE messages)
DEFINE DTS_LONGDATEFORMAT  := 0x0004 // use the long date format (app must forward WM_winINICHANGE messages)
DEFINE DTS_TIMEFORMAT      := 0x0009 // use the time format (app must forward WM_winINICHANGE messages)
DEFINE DTS_APPCANPARSE     := 0x0010 // allow user entered strings (app MUST respond to DTN_USERSTRING)
DEFINE DTS_RIGHTALIGN      := 0x0020 // right-align popup instead of left-align it
DEFINE DTN_DATETIMECHANGE  := (DTN_FIRST + 1) // the systemtime has changed
DEFINE DTN_USERSTRING  := (DTN_FIRST + 2) // the user has entered a string
DEFINE DTN_WMKEYDOWN  := (DTN_FIRST + 3) // modify keydown on app format field (X)
DEFINE DTN_FORMAT  := (DTN_FIRST + 4) // query display for app format field (X)
DEFINE DTN_FORMATQUERY  := (DTN_FIRST + 5) // query formatting info for app format field (X)
DEFINE DTN_DROPDOWN    := (DTN_FIRST + 6) // MonthCal has dropped down
DEFINE DTN_CLOSEUP     := (DTN_FIRST + 7) // MonthCal is popping up
DEFINE GDTR_MIN     := 0x0001
DEFINE GDTR_MAX     := 0x0002
DEFINE GDT_ERROR    := -1
DEFINE GDT_VALID    := 0
DEFINE GDT_NONE     := 1
	// Extended ListViewStyles
DEFINE LVM_SETEXTENDEDLISTVIEWSTYLE := (LVM_FIRST + 54)
DEFINE LVM_GETEXTENDEDLISTVIEWSTYLE := (LVM_FIRST + 55)
DEFINE LVS_EX_GRIDLINES        := 0x00000001
DEFINE LVS_EX_SUBITEMIMAGES    := 0x00000002
DEFINE LVS_EX_CHECKBOXES       := 0x00000004
DEFINE LVS_EX_TRACKSELECT      := 0x00000008
DEFINE LVS_EX_HEADERDRAGDROP   := 0x00000010
DEFINE LVS_EX_FULLROWSELECT    := 0x00000020 // applies to report mode only
DEFINE LVS_EX_ONECLICKACTIVATE := 0x00000040
DEFINE LVS_EX_TWOCLICKACTIVATE := 0x00000080
DEFINE LVM_GETSUBITEMRECT      := (LVM_FIRST + 56)
DEFINE LVM_SUBITEMHITTEST      := (LVM_FIRST + 57)
DEFINE LVM_SETCOLUMNORDERARRAY := (LVM_FIRST + 58)
DEFINE LVM_GETCOLUMNORDERARRAY := (LVM_FIRST + 59)
DEFINE LVM_SETHOTITEM          := (LVM_FIRST + 60)
DEFINE LVM_GETHOTITEM          := (LVM_FIRST + 61)
DEFINE LVM_SETHOTCURSOR        := (LVM_FIRST + 62)
DEFINE LVM_GETHOTCURSOR        := (LVM_FIRST + 63)
DEFINE LVM_APPROXIMATEVIEWRECT := (LVM_FIRST + 64)
DEFINE LVM_SETWORKAREA         := (LVM_FIRST + 65)
	// Rebar stuff
DEFINE RBIM_IMAGELIST     := 0x00000001
DEFINE RBS_TOOLTIPS        := 0x0100
DEFINE RBS_VARHEIGHT       := 0x0200
DEFINE RBS_BANDBORDERS     := 0x0400
DEFINE RBS_FIXEDORDER      := 0x0800
DEFINE RBS_REGISTERDROP    := 0x1000
DEFINE RBS_AUTOSIZE        := 0x2000
DEFINE RBS_VERTICALGRIPPER := 0x4000  // this always has the vertical gripper (default for horizontal mode)
DEFINE RBS_DBLCLKTOGGLE    := 0x8000
DEFINE RBBS_BREAK          := 0x00000001  // break to new line
DEFINE RBBS_FIXEDSIZE      := 0x00000002  // band can't be sized
DEFINE RBBS_CHILDEDGE      := 0x00000004  // edge around top & bottom of child window
DEFINE RBBS_HIDDEN         := 0x00000008  // don't show
DEFINE RBBS_NOVERT         := 0x00000010  // don't show when vertical
DEFINE RBBS_FIXEDBMP       := 0x00000020  // bitmap doesn't move during band resize
DEFINE RBBS_VARIABLEHEIGHT := 0x00000040  // allow autosizing of this child vertically
DEFINE RBBS_GRIPPERALWAYS  := 0x00000080  // always show the gripper
DEFINE RBBS_NOGRIPPER      := 0x00000100  // never show the gripper
DEFINE RBBIM_STYLE     := 0x00000001
DEFINE RBBIM_COLORS    := 0x00000002
DEFINE RBBIM_TEXT      := 0x00000004
DEFINE RBBIM_IMAGE     := 0x00000008
DEFINE RBBIM_CHILD     := 0x00000010
DEFINE RBBIM_CHILDSIZE := 0x00000020
DEFINE RBBIM_SIZE      := 0x00000040
DEFINE RBBIM_BACKGROUND:= 0x00000080
DEFINE RBBIM_ID        := 0x00000100
DEFINE RBBIM_IDEALSIZE := 0x00000200
DEFINE RBBIM_LPARAM    := 0x00000400
DEFINE RBBIM_HEADERSIZE:= 0x00000800
DEFINE RB_INSERTBAND   := (WM_USER +  1)
DEFINE RB_DELETEBAND   := (WM_USER +  2)
DEFINE RB_GETBARINFO   := (WM_USER +  3)
DEFINE RB_SETBARINFO   := (WM_USER +  4)
DEFINE RB_GETBANDINFO98  := (WM_USER +  5)
DEFINE RB_SETBANDINFO  := (WM_USER +  6)
DEFINE RB_SETPARENT    := (WM_USER +  7)
DEFINE RB_GETBANDCOUNT := (WM_USER +  12)
DEFINE RB_GETROWCOUNT  := (WM_USER +  13)
DEFINE RB_GETROWHEIGHT := (WM_USER +  14)
DEFINE RB_HITTEST      := (WM_USER +  8)
DEFINE RB_GETRECT      := (WM_USER +  9)
DEFINE RB_IDTOINDEX    := (WM_USER +  16)
DEFINE RB_GETTOOLTIPS  := (WM_USER +  17)
DEFINE RB_SETTOOLTIPS  := (WM_USER +  18)
DEFINE RB_SETBKCOLOR   := (WM_USER +  19)
DEFINE RB_GETBKCOLOR   := (WM_USER +  20)
DEFINE RB_SETTEXTCOLOR := (WM_USER +  21)
DEFINE RB_GETTEXTCOLOR := (WM_USER +  22)
DEFINE RB_SIZETORECT   := (WM_USER +  23)
DEFINE RB_BEGINDRAG    := (WM_USER + 24)
DEFINE RB_ENDDRAG      := (WM_USER + 25)
DEFINE RB_DRAGMOVE     := (WM_USER + 26)
DEFINE RB_GETBARHEIGHT := (WM_USER + 27)
DEFINE RB_GETBANDINFOW := (WM_USER + 28)
DEFINE RB_GETBANDINFOA := (WM_USER + 29)
DEFINE RB_GETBANDINFO  := RB_GETBANDINFOA
DEFINE RB_MINIMIZEBAND := (WM_USER + 30)
DEFINE RB_MAXIMIZEBAND := (WM_USER + 31)
	//DEFINE RB_GETDROPTARGET:= (WM_USER + 32)
DEFINE RB_GETDROPTARGET := (CCM_GETDROPTARGET)
DEFINE RB_GETBANDBORDERS:=(WM_USER + 34)  // returns in lparam = lprc the amount of edges added to band wparam
DEFINE RB_SHOWBAND     := (WM_USER + 35)      // show/hide band
DEFINE RB_SETPALETTE   := (WM_USER + 37)
DEFINE RB_GETPALETTE   := (WM_USER + 38)
DEFINE RB_MOVEBAND     := (WM_USER + 39)
DEFINE RB_SETUNICODEFORMAT     := CCM_SETUNICODEFORMAT
DEFINE RB_GETUNICODEFORMAT     := CCM_GETUNICODEFORMAT
DEFINE RB_GETBANDMARGINS  := (WM_USER + 40)
DEFINE RB_SETWINDOWTHEME  := CCM_SETWINDOWTHEME
DEFINE RB_PUSHCHEVRON  := (WM_USER + 43)
DEFINE RBN_FIRST :=             (0U-831U)
DEFINE RBN_LAST  :=             (0U-859U)
DEFINE RBN_HEIGHTCHANGE  :=   (RBN_FIRST - 0)
DEFINE RBN_GETOBJECT     :=   (RBN_FIRST - 1)
DEFINE RBN_LAYOUTCHANGED :=   (RBN_FIRST - 2)
DEFINE RBN_AUTOSIZE      :=   (RBN_FIRST - 3)
DEFINE RBN_BEGINDRAG     :=   (RBN_FIRST - 4)
DEFINE RBN_ENDDRAG       :=   (RBN_FIRST - 5)
DEFINE RBN_DELETINGBAND  :=   (RBN_FIRST - 6)	// Uses NMREBAR
DEFINE RBN_DELETEDBAND   :=   (RBN_FIRST - 7)	// Uses NMREBAR
DEFINE RBN_CHILDSIZE     :=   (RBN_FIRST - 8)
DEFINE RBN_CHEVRONPUSHED :=   (RBN_FIRST - 10)
DEFINE RBN_MINMAX        :=   (RBN_FIRST - 21)
DEFINE RBN_AUTOBREAK     :=   (RBN_FIRST - 22)
	//IPAddress Control
DEFINE IPM_CLEARADDRESS := (WM_USER+100) // no parameters
DEFINE IPM_SETADDRESS   := (WM_USER+101) // lparam = TCP/IP address
DEFINE IPM_GETADDRESS   := (WM_USER+102) // lresult = # of non black fields.  lparam = LPDWORD for TCP/IP address
DEFINE IPM_SETRANGE     := (WM_USER+103) // wparam = field, lparam = range
DEFINE IPM_SETFOCUS     := (WM_USER+104) // wparam = field
DEFINE IPM_ISBLANK      := (WM_USER+105) // no parameters
DEFINE IPN_FIRST        := (0U-860U)       // internet address
DEFINE IPN_LAST         := (0U-879U)       // internet address
DEFINE IPN_FIELDCHANGED := (IPN_FIRST - 0)
DEFINE CBEIF_TEXT              := 0x00000001
DEFINE CBEIF_IMAGE             := 0x00000002
DEFINE CBEIF_SELECTEDIMAGE     := 0x00000004
DEFINE CBEIF_OVERLAY           := 0x00000008
DEFINE CBEIF_INDENT            := 0x00000010
DEFINE CBEIF_LPARAM            := 0x00000020
DEFINE CBEIF_DI_SETITEM        := 0x10000000
DEFINE CBEM_INSERTITEM        := (WM_USER + 1)
DEFINE CBEM_SETIMAGELIST      := (WM_USER + 2)
DEFINE CBEM_GETIMAGELIST      := (WM_USER + 3)
DEFINE CBEM_GETITEM           := (WM_USER + 4)
DEFINE CBEM_SETITEM           := (WM_USER + 5)
DEFINE CBEM_DELETEITEM        := CB_DELETESTRING
DEFINE CBEM_GETCOMBOCONTROL   := (WM_USER + 6)
DEFINE CBEM_GETEDITCONTROL    := (WM_USER + 7)
DEFINE CBEM_SETEXSTYLE        := (WM_USER + 8)  // use  SETEXTENDEDSTYLE instead
DEFINE CBEM_SETEXTENDEDSTYLE  := (WM_USER + 14)   // lparam == new style, wParam (optional) == mask
DEFINE CBEM_GETEXSTYLE        := (WM_USER + 9) // use GETEXTENDEDSTYLE instead
DEFINE CBEM_HASEDITCHANGED    := (WM_USER + 10)
DEFINE CBEM_INSERTITEMW       := (WM_USER + 11)
DEFINE CBEM_SETITEMW          := (WM_USER + 12)
DEFINE CBEM_GETITEMW          := (WM_USER + 13)
DEFINE CBES_EX_NOEDITIMAGE          := 0x00000001
DEFINE CBES_EX_NOEDITIMAGEINDENT    := 0x00000002
DEFINE CBES_EX_PATHWORDBREAKPROC    := 0x00000004
DEFINE CBES_EX_NOSIZELIMIT          := 0x00000008
DEFINE CBES_EX_CASESENSITIVE        := 0x00000010
DEFINE CBEN_FIRST               := (0U-800U)       // combo box ex
DEFINE CBEN_LAST                := (0U-830U)
DEFINE CBEN_GETDISPINFO         := (CBEN_FIRST - 0)
DEFINE CBEN_INSERTITEM          := (CBEN_FIRST - 1)
DEFINE CBEN_DELETEITEM          := (CBEN_FIRST - 2)
DEFINE CBEN_BEGINEDIT           := (CBEN_FIRST - 4)
DEFINE CBEN_ENDEDIT             := (CBEN_FIRST - 5)
DEFINE CBEN_DRAGBEGIN           := (CBEN_FIRST - 8)
	// lParam specifies why the endedit is happening
DEFINE CBENF_KILLFOCUS        := 1
DEFINE CBENF_RETURN           := 2
DEFINE CBENF_ESCAPE           := 3
DEFINE CBENF_DROPDOWN         := 4
DEFINE CBEMAXSTRLEN           := 260
	// CBEN_DRAGBEGIN sends this information ...
DEFINE BTNS_BUTTON          := TBSTYLE_BUTTON      // 0x0000
DEFINE BTNS_SEP           := TBSTYLE_SEP         // 0x0001
DEFINE BTNS_CHECK         := TBSTYLE_CHECK       // 0x0002
DEFINE BTNS_GROUP         := TBSTYLE_GROUP       // 0x0004
DEFINE BTNS_CHECKGROUP        := TBSTYLE_CHECKGROUP  // (TBSTYLE_GROUP | TBSTYLE_CHECK)
DEFINE BTNS_DROPDOWN        := TBSTYLE_DROPDOWN    // 0x0008
DEFINE BTNS_AUTOSIZE        := TBSTYLE_AUTOSIZE    // 0x0010 automatically calculate the cx of the button
DEFINE BTNS_NOPREFIX        := TBSTYLE_NOPREFIX    // 0x0020 this button should not have accel prefix
DEFINE BTNS_SHOWTEXT          := 0x0040              // ignored unless TBSTYLE_EX_MIXEDBUTTONS is set
DEFINE BTNS_WHOLEDROPDOWN     := 0x0080           // draw drop-down arrow, but without split arrow section
DEFINE TBSTYLE_EX_MIXEDBUTTONS    := 0x00000008
DEFINE TBSTYLE_EX_HIDECLIPPEDBUTTONS:= 0x00000010  // don't show partially obscured buttons
DEFINE RBBS_USECHEVRON        := 0x00000200  // display drop-down button for this band if it's sized smaller than ideal width
DEFINE RBBS_HIDETITLE       := 0x00000400  // keep band title hidden
DEFINE WMN_FIRST          := (0U-1000U)
DEFINE WMN_LAST           := (0U-1200U)
DEFINE RBHT_CHEVRON         := 0x0008
DEFINE TTS_NOANIMATE        := 0x10
DEFINE TTS_NOFADE         := 0x20
DEFINE TTS_BALLOON          := 0x40
DEFINE TTM_TRACKACTIVATE      := (WM_USER + 17)  // wParam = TRUE/FALSE start end  lparam = LPTOOLINFO
DEFINE TTM_TRACKPOSITION      := (WM_USER + 18)  // lParam = dwPos
DEFINE TTM_SETTIPBKCOLOR      := (WM_USER + 19)
DEFINE TTM_SETTIPTEXTCOLOR      := (WM_USER + 20)
DEFINE TTM_GETDELAYTIME       := (WM_USER + 21)
DEFINE TTM_GETTIPBKCOLOR      := (WM_USER + 22)
DEFINE TTM_GETTIPTEXTCOLOR      := (WM_USER + 23)
DEFINE TTM_SETMARGIN        := (WM_USER + 26)  // lParam = lprc
DEFINE TTM_GETMARGIN        := (WM_USER + 27)  // lParam = lprc
DEFINE TTM_POP            := (WM_USER + 28)
DEFINE TTM_UPDATE         := (WM_USER + 29)
DEFINE TTM_GETBUBBLESIZE      := (WM_USER + 30)
DEFINE TTM_ADJUSTRECT       := (WM_USER + 31)
DEFINE TTM_SETTITLEA        := (WM_USER + 32)  // wParam = TTI_*, lParam = char* szTitle
DEFINE TTM_SETTITLEW        := (WM_USER + 33)  // wParam = TTI_*, lParam = wchar* szTitle
DEFINE SBARS_TOOLTIPS       := 0x0800
DEFINE UDM_SETRANGE32       := (WM_USER+111)
DEFINE UDM_GETRANGE32       := (WM_USER+112) // wParam & lParam are LPINT
DEFINE UDM_SETUNICODEFORMAT     := CCM_SETUNICODEFORMAT
DEFINE UDM_GETUNICODEFORMAT     := CCM_GETUNICODEFORMAT
DEFINE UDM_SETPOS32         := (WM_USER+113)
DEFINE UDM_GETPOS32         := (WM_USER+114)
DEFINE LVS_EX_LABELTIP        := 0x00004000 // listview unfolds partly hidden labels if it does not have infotip text
DEFINE TCN_GETOBJECT        := (TCN_FIRST - 3)
DEFINE TCN_FOCUSCHANGE        := (TCN_FIRST - 4)
DEFINE TBSTYLE_DROPDOWN       := 0x0008  // obsolete use BTNS_DROPDOWN instead
DEFINE TBSTYLE_AUTOSIZE       := 0x0010  // obsolete use BTNS_AUTOSIZE instead
DEFINE TBSTYLE_NOPREFIX       := 0x0020  // obsolete use BTNS_NOPREFIX instead
DEFINE TBSTYLE_LIST         := 0x1000
DEFINE TBSTYLE_CUSTOMERASE      := 0x2000
DEFINE TBSTYLE_REGISTERDROP     := 0x4000
DEFINE TBSTYLE_TRANSPARENT      := 0x8000
DEFINE TBSTYLE_EX_DRAWDDARROWS    := 0x00000001
DEFINE TBSTATE_ELLIPSES       := 0x40
DEFINE TBSTATE_MARKED       := 0x80
DEFINE SBN_FIRST          := (0U-880U)       // status bar
DEFINE SBN_LAST           := (0U-899U)
DEFINE PGN_FIRST          := (0U-900U)       // Pager Control
DEFINE PGN_LAST           := (0U-950U)
DEFINE TB_MARKBUTTON        := (WM_USER + 6)
DEFINE TB_ISBUTTONHIGHLIGHTED   := (WM_USER + 14)
	//SysLink Support
DEFINE MAX_LINKID_TEXT  := 48
DEFINE L_MAX_URL_LENGTH := 2083
DEFINE INVALID_LINK_INDEX  := (-1)
DEFINE LWS_TRANSPARENT    := 0x0001
DEFINE LWS_IGNORERETURN   := 0x0002
DEFINE LIF_ITEMINDEX    := 0x00000001
DEFINE LIF_STATE        := 0x00000002
DEFINE LIF_ITEMID       := 0x00000004
DEFINE LIF_URL          := 0x00000008
DEFINE LIS_FOCUSED      := 0x00000001
DEFINE LIS_ENABLED      := 0x00000002
DEFINE LIS_VISITED      := 0x00000004
DEFINE LM_HITTEST        := (WM_USER+0x300)  // wParam: n/a, lparam: PLHITTESTINF
DEFINE LM_GETIDEALHEIGHT := (WM_USER+0x301)  // wParam: n/a, lparam: n/a, ret: cy
DEFINE LM_SETITEM        := (WM_USER+0x302)  // wParam: n/a, lparam: LITEM*, ret:
DEFINE LM_GETITEM        := (WM_USER+0x303)  // wParam: n/a, lparam: LITEM*, ret:
DEFINE LVGA_FOOTER_CENTER  := 0x00000010
DEFINE LVGA_FOOTER_LEFT    := 0x00000008
DEFINE LVGA_FOOTER_RIGHT   := 0x00000020  // Don't forget to validate exclusivity
DEFINE LVGA_HEADER_CENTER  := 0x00000002
DEFINE LVGA_HEADER_LEFT    := 0x00000001
DEFINE LVGA_HEADER_RIGHT   := 0x00000004  // Don't forget to validate exclusivity
DEFINE LVGF_ALIGN          := 0x00000008
DEFINE LVGF_FOOTER         := 0x00000002
DEFINE LVGF_GROUPID        := 0x00000010
DEFINE LVGF_HEADER         := 0x00000001
DEFINE LVGF_NONE           := 0x00000000
DEFINE LVGF_STATE          := 0x00000004
DEFINE LVGS_COLLAPSED      := 0x00000001
DEFINE LVGS_HIDDEN         := 0x00000002
DEFINE LVGS_NORMAL         := 0x00000000
DEFINE LVIF_COLUMNS := 0x0200
DEFINE LVIF_GROUPID := 0x0100
DEFINE LVM_ENABLEGROUPVIEW := (LVM_FIRST + 157)
DEFINE LVM_GETGROUPINFO := (LVM_FIRST + 148)
DEFINE LVM_HASGROUP := (LVM_FIRST + 161)
DEFINE LVM_INSERTGROUP := (LVM_FIRST + 145)
DEFINE LVM_MOVEGROUP := (LVM_FIRST + 151)
DEFINE LVM_MOVEITEMTOGROUP := (LVM_FIRST + 154)
DEFINE LVM_REMOVEGROUP := (LVM_FIRST + 150)
DEFINE LVM_REMOVEALLGROUPS := (LVM_FIRST + 160)
DEFINE LVM_SETVIEW := (LVM_FIRST + 142)
DEFINE LV_VIEW_TILE := 0x0004
DEFINE LVM_ISGROUPVIEWENABLED := (LVM_FIRST + 175)
DEFINE LVM_SETGROUPINFO := (LVM_FIRST + 147)
DEFINE LVGMF_BORDERCOLOR := 0x00000002
DEFINE LVGMF_BORDERSIZE := 0x00000001
DEFINE LVGMF_NONE := 0x00000000
DEFINE LVGMF_TEXTCOLOR := 0x00000004
DEFINE LVM_SETGROUPMETRICS := (LVM_FIRST + 155)
DEFINE LVM_GETGROUPMETRICS := (LVM_FIRST + 156)
DEFINE LVBKIF_FLAG_TILEOFFSET := 0x00000100
DEFINE LVBKIF_SOURCE_HBITMAP := 0x00000001
DEFINE LVBKIF_SOURCE_MASK := 0x00000003
DEFINE LVBKIF_SOURCE_NONE := 0x00000000
DEFINE LVBKIF_SOURCE_URL := 0x00000002
DEFINE LVBKIF_STYLE_MASK := 0x00000010
DEFINE LVBKIF_STYLE_NORMAL := 0x00000000
DEFINE LVBKIF_STYLE_TILE := 0x00000010
DEFINE LVBKIF_TYPE_WATERMARK := 0x10000000
DEFINE LVM_GETBKIMAGEA := (LVM_FIRST + 69)
DEFINE LVM_GETBKIMAGEW := (LVM_FIRST + 139)
DEFINE LVM_GETHEADER := (LVM_FIRST + 31)
DEFINE LVM_GETSELECTEDCOLUMN := (LVM_FIRST + 174)
DEFINE LVM_SETBKIMAGE := LVM_SETBKIMAGEA
DEFINE LVM_SETBKIMAGEA := (LVM_FIRST + 68)
DEFINE LVM_SETBKIMAGEW := (LVM_FIRST + 138)
DEFINE LVM_SETSELECTEDCOLUMN := (LVM_FIRST + 140)
DEFINE LVS_EX_BORDERSELECT := 0x00008000 // border selection style instead of highlight
DEFINE LVS_EX_DOUBLEBUFFER := 0x00010000
DEFINE LVS_EX_HIDELABELS := 0x00020000
DEFINE LVS_EX_SIMPLESELECT := 0x00100000
DEFINE LVS_EX_SINGLEROW := 0x00040000
DEFINE LVS_EX_SNAPTOGRID := 0x00080000  // Icons automatically snap to grid.
DEFINE LV_VIEW_ICON := 0x0000
DEFINE LV_VIEW_DETAILS := 0x0001
DEFINE LV_VIEW_SMALLICON := 0x0002
DEFINE LV_VIEW_LIST := 0x0003
DEFINE HICF_OTHER          := 0x00000000
DEFINE HICF_MOUSE          := 0x00000001          // Triggered by mouse
DEFINE HICF_ARROWKEYS      := 0x00000002          // Triggered by arrow keys
DEFINE HICF_ACCELERATOR    := 0x00000004          // Triggered by accelerator
DEFINE HICF_DUPACCEL       := 0x00000008          // This accelerator is not unique
DEFINE HICF_ENTERING       := 0x00000010          // idOld is invalid
DEFINE HICF_LEAVING        := 0x00000020          // idNew is invalid
DEFINE HICF_RESELECT       := 0x00000040          // hot item reselected
DEFINE TBN_HOTITEMCHANGE    :=   (TBN_FIRST - 13)
DEFINE TBN_DRAGOUT          :=   (TBN_FIRST - 14) // this is sent when the user clicks down on a button then drags off the button
DEFINE TBN_DELETINGBUTTON   :=   (TBN_FIRST - 15) // uses TBNOTIFY
DEFINE TBN_GETDISPINFOA     :=   (TBN_FIRST - 16) // This is sent when the  toolbar needs  some display information
DEFINE TBN_GETDISPINFOW     :=   (TBN_FIRST - 17) // This is sent when the  toolbar needs  some display information
DEFINE TBN_GETINFOTIPA      :=   (TBN_FIRST - 18)
DEFINE TBN_GETINFOTIPW      :=   (TBN_FIRST - 19)
DEFINE TBN_GETDISPINFO      :=   TBN_GETDISPINFOA
DEFINE TBN_GETINFOTIP       :=   TBN_GETINFOTIPA
DEFINE TBNF_IMAGE             := 0x00000001
DEFINE TBNF_TEXT              := 0x00000002
DEFINE TBNF_DI_SETITEM        := 0x10000000
DEFINE TBDDRET_DEFAULT        := 0
DEFINE TBDDRET_NODEFAULT      := 1
DEFINE TBDDRET_TREATPRESSED   := 2       // Treat as a standard press button
DEFINE REBARCLASSNAME         := "ReBarWindow32"
DEFINE TTF_TRACK             :=  0x0020
DEFINE TTF_ABSOLUTE          :=  0x0080
DEFINE TTF_TRANSPARENT       :=  0x0100
DEFINE TTF_DI_SETITEM        :=  0x8000       // valid only on the TTN_NEEDTEXT callback
	//*************************************        "msctls_statusbar32"
DEFINE SB_ISSIMPLE            := (WM_USER+14)
DEFINE SB_SETICON             := (WM_USER+15)
DEFINE SB_SETTIPTEXTA         := (WM_USER+16)
DEFINE SB_SETTIPTEXTW         := (WM_USER+17)
DEFINE SB_GETTIPTEXTA         := (WM_USER+18)
DEFINE SB_GETTIPTEXTW         := (WM_USER+19)
DEFINE SB_GETICON             := (WM_USER+20)
DEFINE SB_SETTIPTEXT          := SB_SETTIPTEXTA
DEFINE SB_GETTIPTEXT          := SB_GETTIPTEXTA
DEFINE SBT_TOOLTIPS           := 0x0800
DEFINE SBN_SIMPLEMODECHANGE   := (SBN_FIRST - 0)
	//*************************************        "msctls_trackbar32"
DEFINE TBM_SETTOOLTIPS        := (WM_USER+29)
DEFINE TBM_GETTOOLTIPS        := (WM_USER+30)
DEFINE TBM_SETTIPSIDE         := (WM_USER+31)
	// TrackBar Tip Side flags
DEFINE TBTS_TOP              := 0
DEFINE TBTS_LEFT             := 1
DEFINE TBTS_BOTTOM           := 2
DEFINE TBTS_RIGHT            := 3
DEFINE TBCD_TICS    := 0x0001
DEFINE TBCD_THUMB   := 0x0002
DEFINE TBCD_CHANNEL := 0x0003
	//*************************************        "msctls_updown32"
DEFINE UDS_HOTTRACK           := 0x0100
	//====== COMMON CONTROL STYLES ================================================
DEFINE CCS_VERT              :=  0x00000080L
DEFINE CCS_LEFT              :=  0x00000081L
DEFINE CCS_RIGHT             :=  0x00000083L
DEFINE CCS_NOMOVEX           :=  0x00000082L
	//*************************************        "SysListView32"
DEFINE TBM_GETBUDDY           := (WM_USER+33) // wparam = BOOL fLeft; (or right)
DEFINE LVCF_IMAGE             := 0x0010
DEFINE LVCF_ORDER             := 0x0020
DEFINE LVCFMT_IMAGE           := 0x0800
DEFINE LVCFMT_BITMAP_ON_RIGHT := 0x1000
DEFINE LVCFMT_COL_HAS_IMAGES  := 0x8000
DEFINE LVM_SETICONSPACING           := (LVM_FIRST + 53)
DEFINE LV_MAX_WORKAREAS  :=  16
DEFINE LVM_SETWORKAREAS         := (LVM_FIRST + 65)
DEFINE LVM_GETWORKAREAS         := (LVM_FIRST + 70)
DEFINE LVM_GETNUMBEROFWORKAREAS := (LVM_FIRST + 73)
DEFINE LVM_GETSELECTIONMARK     := (LVM_FIRST + 66)
DEFINE LVM_SETSELECTIONMARK     := (LVM_FIRST + 67)
DEFINE LVM_SETHOVERTIME         := (LVM_FIRST + 71)
DEFINE LVM_GETHOVERTIME         := (LVM_FIRST + 72)
DEFINE LVM_SETTOOLTIPS          := (LVM_FIRST + 74)
DEFINE LVM_GETTOOLTIPS          := (LVM_FIRST + 78)
DEFINE LVM_GETBKIMAGE         := LVM_GETBKIMAGEA
	// NMITEMACTIVATE is used instead of NMLISTVIEW in IE >= 0x400
	// therefore all the fields are the same except for extra uKeyFlags
	// they are used to store key flags at the time of the single click with
	// delayed activation - because by the time the timer goes off a user may
	// not hold the keys (shift, ctrl) any more
DEFINE LVKF_ALT      := 0x0001
DEFINE LVKF_CONTROL  := 0x0002
DEFINE LVKF_SHIFT    := 0x0004
DEFINE LVN_HOTTRACK            := (LVN_FIRST-21)
DEFINE LVN_ODFINDITEMA         := (LVN_FIRST-52)
DEFINE LVN_ODFINDITEMW         := (LVN_FIRST-79)
DEFINE LVN_GETDISPINFOA        := (LVN_FIRST-50)
DEFINE LVN_SETDISPINFOA        := (LVN_FIRST-51)
DEFINE LVN_MARQUEEBEGIN        := (LVN_FIRST-56)
DEFINE LVGIT_UNFOLDED  := 0x0001
DEFINE LVN_GETINFOTIPA         := (LVN_FIRST-57)
DEFINE LVN_GETINFOTIPW         := (LVN_FIRST-58)
DEFINE LVN_GETINFOTIP          := LVN_GETINFOTIPA
	//*************************************        "SysTreeView32"
DEFINE TVS_RTLREADING         := 0x0040
DEFINE TVS_NOTOOLTIPS         := 0x0080
DEFINE TVS_CHECKBOXES         := 0x0100
DEFINE TVS_TRACKSELECT        := 0x0200
DEFINE TVS_SINGLEEXPAND       := 0x0400
DEFINE TVS_INFOTIP            := 0x0800
DEFINE TVS_FULLROWSELECT      := 0x1000
DEFINE TVS_NOSCROLL           := 0x2000
DEFINE TVS_NONEVENHEIGHT      := 0x4000
DEFINE TVS_NOHSCROLL          := 0x8000 // 5.80
DEFINE TVIF_INTEGRAL          := 0x0080
DEFINE TVIS_EXPANDPARTIAL     := 0x0080
DEFINE TVE_EXPANDPARTIAL      := 0x4000
DEFINE TVGN_LASTVISIBLE       := 0x000A
DEFINE TVM_SETTOOLTIPS        := (TV_FIRST + 24)
DEFINE TVM_GETTOOLTIPS        := (TV_FIRST + 25)
DEFINE TVM_SETINSERTMARK      := (TV_FIRST + 26)
DEFINE TVM_SETITEMHEIGHT      := (TV_FIRST + 27)
DEFINE TVM_GETITEMHEIGHT      := (TV_FIRST + 28)
DEFINE TVM_SETBKCOLOR         := (TV_FIRST + 29)
DEFINE TVM_SETTEXTCOLOR       := (TV_FIRST + 30)
DEFINE TVM_GETBKCOLOR         := (TV_FIRST + 31)
DEFINE TVM_GETTEXTCOLOR       := (TV_FIRST + 32)
DEFINE TVM_SETSCROLLTIME      := (TV_FIRST + 33)
DEFINE TVM_GETSCROLLTIME      := (TV_FIRST + 34)
DEFINE TVM_SETINSERTMARKCOLOR := (TV_FIRST + 37)
DEFINE TVM_GETINSERTMARKCOLOR := (TV_FIRST + 38)
DEFINE TVM_SETLINECOLOR       := (TV_FIRST + 40) // 5.80
DEFINE TVN_SELCHANGED        := TVN_SELCHANGEDA
DEFINE TVN_GETDISPINFO       := TVN_GETDISPINFOA
DEFINE TVN_GETINFOTIPA       := (TVN_FIRST-13)
DEFINE TVN_GETINFOTIPW       := (TVN_FIRST-14)
DEFINE TVN_SINGLEEXPAND      := (TVN_FIRST-15)
DEFINE TVN_GETINFOTIP        := TVN_GETINFOTIPA
DEFINE TVN_SELCHANGING       := TVN_SELCHANGINGA
DEFINE TCS_EX_FLATSEPARATORS  := 0x00000001
DEFINE TCS_EX_REGISTERDROP    := 0x00000002
DEFINE TCIF_STATE             := 0x0010
DEFINE TCIS_BUTTONPRESSED     := 0x0001
DEFINE TCIS_HIGHLIGHTED       := 0x0002
DEFINE TCM_HIGHLIGHTITEM      := (TCM_FIRST + 51)
DEFINE TCM_SETEXTENDEDSTYLE   := (TCM_FIRST + 52)  // optional wParam == mask
DEFINE TCM_GETEXTENDEDSTYLE   := (TCM_FIRST + 53)
	//*************************************        "SysAnimate32"
DEFINE ACS_TIMER              := 0x0008  // don't use threads... use timers
	//*************************************        Rebar stuff
DEFINE RB_INSERTBANDA  := (WM_USER +  1)
DEFINE RB_INSERTBANDW  := (WM_USER +  10)
DEFINE RB_SETBANDINFOW := (WM_USER +  11)
	// for manual drag control
	// lparam == cursor pos
	// -1 means do it yourself.
	// -2 means use what you had saved before
DEFINE ILD_ROP							 := 0x0040
DEFINE FSB_REGULAR_MODE    := 0
DEFINE FSB_ENCARTA_MODE    := 1
DEFINE FSB_FLAT_MODE       := 2
DEFINE MAXPROPPAGES           := 100
DEFINE PSP_DEFAULT               := 0x00000000
DEFINE PSP_DLGINDIRECT           := 0x00000001
DEFINE PSP_USEHICON              := 0x00000002
DEFINE PSP_USEICONID             := 0x00000004
DEFINE PSP_USETITLE              := 0x00000008
DEFINE PSP_RTLREADING            := 0x00000010
DEFINE PSP_HASHELP               := 0x00000020
DEFINE PSP_USEREFPARENT          := 0x00000040
DEFINE PSP_USECALLBACK           := 0x00000080
DEFINE PSP_PREMATURE             := 0x00000400
	//----- New flags for wizard97 --------------
DEFINE PSP_HIDEHEADER            := 0x00000800
DEFINE PSP_USEHEADERTITLE        := 0x00001000
DEFINE PSP_USEHEADERSUBTITLE     := 0x00002000
	//-------------------------------------------
DEFINE PSPCB_RELEASE          := 1
DEFINE PSPCB_CREATE           := 2
DEFINE PSH_DEFAULT            := 0x00000000
DEFINE PSH_PROPTITLE          := 0x00000001
DEFINE PSH_USEHICON           := 0x00000002
DEFINE PSH_USEICONID          := 0x00000004
DEFINE PSH_PROPSHEETPAGE      := 0x00000008
DEFINE PSH_WIZARDHASFINISH    := 0x00000010
DEFINE PSH_WIZARD             := 0x00000020
DEFINE PSH_USEPSTARTPAGE      := 0x00000040
DEFINE PSH_NOAPPLYNOW         := 0x00000080
DEFINE PSH_USECALLBACK        := 0x00000100
DEFINE PSH_HASHELP            := 0x00000200
DEFINE PSH_MODELESS           := 0x00000400
DEFINE PSH_RTLREADING         := 0x00000800
DEFINE PSH_WIZARDCONTEXTHELP  := 0x00001000
	//----- New flags for wizard97 -----------
DEFINE PSH_WIZARD97           := 0x00002000
	// 0x00004000 was not used by any previous release
DEFINE PSH_WATERMARK          := 0x00008000
DEFINE PSH_USEHBMWATERMARK    := 0x00010000  // user pass in a hbmWatermark instead of pszbmWatermark
DEFINE PSH_USEHPLWATERMARK    := 0x00020000  //
DEFINE PSH_STRETCHWATERMARK   := 0x00040000  // stretchwatermark also applies for the header
DEFINE PSH_HEADER             := 0x00080000
DEFINE PSH_USEHBMHEADER       := 0x00100000
DEFINE PSH_USEPAGELANG        := 0x00200000  // use frame dialog template matched to page
	//----------------------------------------
DEFINE PSCB_INITIALIZED  := 1
DEFINE PSCB_PRECREATE    := 2
DEFINE PSN_FIRST              := (0U-200U)
DEFINE PSN_LAST               := (0U-299U)
DEFINE PSN_SETACTIVE          := (PSN_FIRST-0)
DEFINE PSN_KILLACTIVE         := (PSN_FIRST-1)
DEFINE PSN_VALIDATE           := (PSN_FIRST-1)
DEFINE PSN_APPLY              := (PSN_FIRST-2)
DEFINE PSN_RESET              := (PSN_FIRST-3)
DEFINE PSN_CANCEL             := (PSN_FIRST-3)
DEFINE PSN_HELP               := (PSN_FIRST-5)
DEFINE PSN_WIZBACK            := (PSN_FIRST-6)
DEFINE PSN_WIZNEXT            := (PSN_FIRST-7)
DEFINE PSN_WIZFINISH          := (PSN_FIRST-8)
DEFINE PSN_QUERYCANCEL        := (PSN_FIRST-9)
DEFINE PSN_GETOBJECT          := (PSN_FIRST-10)
DEFINE PSNRET_NOERROR              := 0
DEFINE PSNRET_INVALID              := 1
DEFINE PSNRET_INVALID_NOCHANGEPAGE := 2
DEFINE PSM_SETCURSEL          := (WM_USER + 101)
DEFINE PSM_REMOVEPAGE         := (WM_USER + 102)
DEFINE PSM_ADDPAGE            := (WM_USER + 103)
DEFINE PSM_CHANGED            := (WM_USER + 104)
DEFINE PSM_RESTARTWINDOWS     := (WM_USER + 105)
DEFINE PSM_REBOOTSYSTEM       := (WM_USER + 106)
DEFINE PSM_CANCELTOCLOSE      := (WM_USER + 107)
DEFINE PSM_QUERYSIBLINGS      := (WM_USER + 108)
DEFINE PSM_UNCHANGED          := (WM_USER + 109)
DEFINE PSM_APPLY              := (WM_USER + 110)
DEFINE PSM_SETTITLEA          := (WM_USER + 111)
DEFINE PSM_SETTITLEW          := (WM_USER + 120)
DEFINE PSM_SETTITLE           := PSM_SETTITLEA
DEFINE PSM_SETWIZBUTTONS      := (WM_USER + 112)
DEFINE PSWIZB_BACK            := 0x00000001
DEFINE PSWIZB_NEXT            := 0x00000002
DEFINE PSWIZB_FINISH          := 0x00000004
DEFINE PSWIZB_DISABLEDFINISH  := 0x00000008
DEFINE PSM_PRESSBUTTON        := (WM_USER + 113)
DEFINE PSBTN_BACK             := 0
DEFINE PSBTN_NEXT             := 1
DEFINE PSBTN_FINISH           := 2
DEFINE PSBTN_OK               := 3
DEFINE PSBTN_APPLYNOW         := 4
DEFINE PSBTN_CANCEL           := 5
DEFINE PSBTN_HELP             := 6
DEFINE PSBTN_MAX              := 6
DEFINE PSM_SETCURSELID        := (WM_USER + 114)
DEFINE PSM_SETFINISHTEXTA     := (WM_USER + 115)
DEFINE PSM_SETFINISHTEXTW     := (WM_USER + 121)
DEFINE PSM_SETFINISHTEXT      := PSM_SETFINISHTEXTA
DEFINE PSM_GETTABCONTROL      := (WM_USER + 116)
DEFINE PSM_ISDIALOGMESSAGE    := (WM_USER + 117)
DEFINE PSM_GETCURRENTPAGEHWND := (WM_USER + 118)
DEFINE ID_PSRESTARTWINDOWS    := 0x2
DEFINE ID_PSREBOOTSYSTEM      := 0x3
DEFINE PROP_SM_CXDLG          := 212
DEFINE PROP_SM_CYDLG          := 188
DEFINE PROP_MED_CXDLG         := 227
DEFINE PROP_MED_CYDLG         := 215
DEFINE PROP_LG_CXDLG          := 252
DEFINE PROP_LG_CYDLG          := 218
DEFINE WIZ_CXDLG              := 276
DEFINE WIZ_CYDLG              := 140
DEFINE WIZ_CXBMP              := 80
DEFINE WIZ_BODYX              := 92
DEFINE WIZ_BODYCX             := 184
DEFINE WSB_PROP_CYVSCROLL  := 0x00000001L
DEFINE WSB_PROP_CXHSCROLL  := 0x00000002L
DEFINE WSB_PROP_CYHSCROLL  := 0x00000004L
DEFINE WSB_PROP_CXVSCROLL  := 0x00000008L
DEFINE WSB_PROP_CXHTHUMB   := 0x00000010L
DEFINE WSB_PROP_CYVTHUMB   := 0x00000020L
DEFINE WSB_PROP_VBKGCOLOR  := 0x00000040L
DEFINE WSB_PROP_HBKGCOLOR  := 0x00000080L
DEFINE WSB_PROP_VSTYLE     := 0x00000100L
DEFINE WSB_PROP_HSTYLE     := 0x00000200L
DEFINE WSB_PROP_winSTYLE   := 0x00000400L
DEFINE WSB_PROP_PALETTE    := 0x00000800L
DEFINE WSB_PROP_MASK       := 0x00000FFFL
DEFINE TME_HOVER       := 0x00000001
DEFINE TME_LEAVE       := 0x00000002
DEFINE TME_QUERY       := 0x40000000
DEFINE TME_CANCEL      := 0x80000000
DEFINE HOVER_DEFAULT   := 0xFFFFFFFF
DEFINE LVS_EX_FLATSB           := 0x00000100
DEFINE LVS_EX_REGIONAL         := 0x00000200
DEFINE LVS_EX_INFOTIP          := 0x00000400 // listview does InfoTips for you
DEFINE LVS_EX_UNDERLINEHOT     := 0x00000800
DEFINE LVS_EX_UNDERLINECOLD    := 0x00001000
#endregion
