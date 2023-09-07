//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/MultiMediaContainer/*" />
CLASS MultiMediaContainer INHERIT Control
	PROTECT pBitmap AS IntPtr
	PROTECT sMajorType AS STRING
	PROTECT sMinorType AS STRING
	PROTECT sFileName AS STRING
	PROTECT wBasicType AS INT
	PROTECT hwndMCI AS IntPtr

    /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.Panel


	METHOD __ResizeMCIWnd() AS MultiMediaContainer STRICT
		//Todo Implement
		//LOCAL rc IS _winRECT

		//GetClientRect(SELF:handle(), @rc)
		//SetWindowPos(hwndMCI, NULL_PTR, 0, 0, rc:right, rc:bottom, _OR(SWP_NOMOVE, SWP_NOZORDER))
		RETURN SELF

	ASSIGN __Value(uNewVal AS USUAL)  STRICT
		//Todo Implement
		/*	LOCAL rc IS _winRECT
		LOCAL ic1, ic2 AS DWORD

		IF IsString(uNewVal)
		// this sets uValue
		ic1 := At2(",", uNewVal)
		IF (ic1 != 0)
		sMajorType := Lower(AllTrim(Left(uNewVal, ic1-1)))
		ic2 :=  At3(",", uNewVal, ic1+1)
		sMinorType := Lower(AllTrim(SubStr(uNewVal, ic1+1, ic2-ic1-1)))
		SELF:FileName := AllTrim(SubStr(uNewVal, ic2+1))
		ELSE
		SELF:FileName := uNewVal
		ENDIF
		ELSE
		IF (pBitmap != NULL_PTR)
		DIBDelete(pBitmap)
		pBitmap := NULL_PTR
		ENDIF
		IF (hwndMCI != NULL_PTR)
		MCIWndDestroy(hwndMCI)
		hwndMCI := NULL_PTR
		ENDIF
		uValue := NULL_STRING
		GetWindowRect(SELF:Handle(), @rc)
		MapWindowPoints(NULL_PTR, GetParent(SELF:Handle()), (_winPOINT PTR) @rc, 2)
		InvalidateRect(GetParent(SELF:Handle()), @rc, TRUE)
		ENDIF*/
		RETURN


	//METHOD Destroy() AS USUAL STRICT
		//Todo Implement

		//IF (pBitmap != NULL_PTR)
		//	DIBDelete(pBitmap)
		//	pBitmap := NULL_PTR
		//ENDIF

		//IF IsWindow(hwndMCI)
		//	MCIWndDestroy(hwndMCI)
		//	hwndMCI := NULL_PTR
		//ENDIF

		//RETURN SUPER:Destroy()

	METHOD Dispatch(oEvent AS @@Event)
		//Todo Implement
		//LOCAL ps IS _winPAINTSTRUCT
		LOCAL oEvt := oEvent AS @@Event
		////local pBmiH as _WINBITMAPINFO
		////local nHeight as int
		////local nWidth as int
		////local oPOint as POINT
		//LOCAL oSize AS DIMENSION
		////local r8Zoom as real8

		////uRet := super:Dispatch(oEvt)

		//IF (oEvt:Message == WM_PAINT) .AND. (pBitmap != NULL_PTR)
		//	// oPoint := self:Origin
		//	// pBmiH := DIBGetInfo(self:pBitMap)
		//	// nHeight := pBmiH.bmiHeader.biHeight
		//	// nWidth := pBmiH.bmiHeader.biWidth

		//	oSize := SELF:Size
		//	// r8Zoom := oSize:Width/nWidth
		//	BeginPaint(hWnd, @ps)
		//	EndPaint(hWnd, @ps)
		//	DIBStretch(pBitmap, SELF:handle(), DWORD(oSize:Width), DWORD(oSize:Height), 1.0)

		//	RETURN (SELF:EventReturnValue := 0L)

		//	/*
		//	 self:Size := DIMension{nWidth, nHeight}
		//	 self:Origin := oPoint
		//	 DIBShow(self:pBitMap, self:Handle())
		//	*/
		//ENDIF

		RETURN SUPER:Dispatch(oEvt)


	ASSIGN FileName(sNewVal)
		//Todo Implement
		//	LOCAL rc IS _winRECT
		//	LOCAL sof IS _winOFSTRUCT
		//	LOCAL sMime AS STRING

		//	IF (pBitmap != NULL_PTR)
		//		DIBDelete(pBitmap)
		//		pBitmap := NULL_PTR
		//		uValue := NULL_STRING
		//	ENDIF

		//	IF IsString(sNewVal)
		//		IF (OpenFile(String2Psz(sNewVal), @sof, OF_EXIST) != HFILE_ERROR)
		//			sMime := Upper(GetMimeType(sNewVal))
		//			IF !Empty(sMime)
		//				sMajorType := Lower(Left(sMime, (At2("/", sMime) - 1)))
		//				sMinorType := Lower(SubStr(sMime, (At2("/", sMime) + 1)))
		//			ENDIF
		//			IF (sMajorType == "image")
		//				uValue := sNewVal
		//				pBitmap := DIBCreateFromFile(sNewVal)
		//			ELSEIF (sMajorType == "video") .OR. (sMajorType == "audio")
		//				IF IsWindow(hwndMCI)
		//					//MCIWndDestroy(hwndMCI)
		//					SendMessage(hwndMCI, MCIWNDM_OPEN, 0, LONGINT(_CAST, sNewVal))
		//				ELSE
		//					hwndMCI := PCALL(gpfnMCWWndCreate, SELF:Handle(), _GetInst(), MCIWNDF_SHOWALL, sNewVal)
		//				ENDIF
		//				SELF:__ResizeMCIWnd()
		//			ENDIF
		//		ELSE
		//			MessageBox(0, String2Psz("MultiMedia File "+sNewVal+" does not exist!"), PSZ(_CAST, "Warning"), 0)
		//		ENDIF
		//	ENDIF

		//	GetWindowRect(SELF:Handle(), @rc)
		//	MapWindowPoints(NULL_PTR, GetParent(SELF:Handle()), (_winPOINT PTR) @rc, 2)
		//	InvalidateRect(GetParent(SELF:Handle()), @rc, TRUE)

		RETURN

/// <include file="Gui.xml" path="doc/MultiMediaContainer.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
		LOCAL LoadError AS WCError

		//__LoadMSVFWDll()

		SUPER(oOwner, xID, oPoint, oDimension, __WCMMContWindowClass, WS_BORDER, TRUE)

		IF TRUE // (!CAPaintIsLoaded())
			LoadError := WCError{#Init,#MultiMediaContainer,__WCSCAPaintLoadFailed}
			LoadError:CanDefault := TRUE
			LoadError:Throw()
		ENDIF

		uValue := NULL_STRING

		RETURN

/// <include file="Gui.xml" path="doc/MultiMediaContainer.MajorType/*" />
	ACCESS MajorType  AS STRING
		RETURN sMajorType

/// <include file="Gui.xml" path="doc/MultiMediaContainer.MajorType/*" />
	ASSIGN MajorType(sNewType AS STRING )
		sMajorType := Lower(sNewType)

/// <include file="Gui.xml" path="doc/MultiMediaContainer.MCISendMessage/*" />
	METHOD MCISendMessage(dwMsg, wParam, lParam)
		//Todo Implement
		//LOCAL uMsg, wPar AS DWORD
		//LOCAL lPar, lRet AS LONGINT

		//IF !IsWindow(hwndMCI)
		//	RETURN 0L
		//ENDIF

		//DEFAULT( REF wParam, 0)
		//DEFAULT( REF lParam, 0)

		//uMsg := dwMsg
		//wPar := wParam
		//lPar := lParam

		//lRet := SendMessage(hwndMCI, uMsg, wPar, lPar)

		//IF (uMSG == MCIWNDM_CHANGESTYLES)
		//	SELF:__ResizeMCIWnd()
		//ENDIF

		//RETURN lRet
		RETURN 0

/// <include file="Gui.xml" path="doc/MultiMediaContainer.MinorType/*" />
	ACCESS MinorType AS STRING
		RETURN sMinorType

/// <include file="Gui.xml" path="doc/MultiMediaContainer.MinorType/*" />
	ASSIGN MinorType(sNewType AS STRING)
		sMinorType := Lower(sNewType)

	/*ASSIGN Size(oNewDim)
	LOCAL uRet AS USUAL

	uRet := SUPER:Size := oNewDim

	SELF:__ResizeMCIWnd()

	RETURN */
END CLASS

//Todo Implement
//FUNCTION __LoadMSVFWDll()
//	LOCAL hDll AS PTR
//	LOCAL rsFormat AS ResourceString

//	IF glMSVFWDllLoaded
//		RETURN TRUE
//	ENDIF

//	hDll := LoadLibrary(String2Psz("MSVFW32.DLL"))
//	IF (hDll == NULL_PTR)
//		rsFormat := ResourceString{__WCSLoadLibraryError}
//		WCError{#LoadSplitWindowDLL, #SplitWindow, VO_SPrintF(rsFormat:value, "MSVFW32.DLL"),,,FALSE}:Throw()
//		RETURN FALSE
//	ENDIF

//	gpfnMCWWndCreate := GetProcAddress(hDll, String2Psz( "MCIWndCreate"))

//	RETURN (glMSVFWDllLoaded := TRUE)

/*ROCEDURE __WCRegisterMMContWindow _INIT3
LOCAL wc IS _WINWNDclass

wc:style := _OR(CS_DBLCLKS, CS_GLOBALCLASS)
#ifdef __VULCAN__
LOCAL hDll := LoadLibraryW( "user32.dll" ) AS PTR
wc:lpfnWndProc 	:= GetProcAddress( hDll, String2Psz( "DefWindowProcA" ) )
FreeLibrary( hDll )
#else
wc:lpfnWndProc := PTR(_CAST, @DefWindowProc())
#endif
wc:hInstance := _GetInst() // 0x400000
wc:hIcon := NULL_PTR // LoadIcon(0, IDI_APPLICATION)
wc:hCursor := LoadCursor(0, IDC_Arrow)
wc:hbrBackground := (COLOR_WINDOW + 1)
wc:lpszClassName := String2Psz( __WCMMContWindowClass)
wc:cbWndExtra := 0
wc:cbClsExtra := 0

RegisterClass(@wc)
RETURN*/

//STATIC GLOBAL glMSVFWDllLoaded := FALSE AS LOGIC

//GLOBAL gpfnMCWWndCreate AS TMCIWndCreate PTR

//FUNCTION TMCIWndCreate(hwndParent AS PTR, hInstance AS PTR, dwStyle AS DWORD, szFile AS PSZ) AS PTR STRICT
//	//SYSTEM
//	RETURN NULL_PTR

#region defines
DEFINE __WCMMContWindowClass := "_VOMMContainer"
DEFINE MM_AVI := 2
DEFINE MM_BMP := 1
DEFINE MM_UNKNOWN := 0
DEFINE MM_WAV := 3
#endregion
