FUNCTION DIBCreateFromFile(pszFName AS PSZ) AS PTR STRICT
	IF lCAPaintInitialized
		RETURN PCALL(pfnDIBCreateFromFile, pszFName)
	ENDIF
	RETURN NULL_PTR

FUNCTION DIBCreateFromPTR(pbImage AS BYTE PTR, nSize AS INT) AS PTR STRICT
	IF lCAPaintInitialized
		RETURN PCALL(pfnDIBCreateFromPTR, pbImage, nSize)
	ENDIF
	RETURN NULL_PTR

FUNCTION DIBShow(pWinBmp AS PTR, hWnd AS PTR) AS VOID STRICT
	IF lCAPaintInitialized
		PCALL(pfnDIBShow, pWinBmp, hWnd)
	ENDIF

	RETURN

FUNCTION DIBStretch(pWinBmp AS PTR, hWnd AS PTR, nCx AS DWORD, nCy AS DWORD, r8Factor AS REAL8) AS VOID STRICT
	IF lCAPaintInitialized
		PCALL(pfnDIBStretch, pWinBmp, hWnd, nCx, nCy, r8Factor)
	ENDIF

	RETURN

FUNCTION DIBSaveAs(pWinBmp AS PTR, pszFName AS PSZ) AS VOID STRICT
	IF lCAPaintInitialized
		PCALL(pfnDIBSaveAs, pWinBmp, pszFName)
	ENDIF

	RETURN


FUNCTION DIBGetInfo(pWinBmp AS PTR) AS _WINBITMAPINFO STRICT
	LOCAL sbmi AS _WINBITMAPINFO
	IF lCAPaintInitialized
		sbmi := PCALL(pfnDIBGetInfo, pWinBmp)
	ENDIF

	RETURN sbmi

FUNCTION DIBDelete(pWinBmp AS PTR) AS VOID
	IF lCAPaintInitialized
		PCALL(pfnDIBDelete, pWinBmp)
	ENDIF

	RETURN

FUNCTION CAPaintLastErrorMsg() AS PSZ STRICT
	IF lCAPaintInitialized
		RETURN PCALL(pfnCAPaintLastErrorMsg)
	ENDIF
	RETURN NULL_PSZ

FUNCTION CAPaintLastError() AS INT STRICT
	IF lCAPaintInitialized
		RETURN PCALL(pfnCAPaintLastError)
	ENDIF
	RETURN 0

FUNCTION CAPaintShowErrors(l AS LOGIC) AS VOID STRICT
	IF lCAPaintInitialized
		PCALL(pfnCAPaintShowErrors, l)
	ENDIF

	RETURN

FUNCTION CAPaintIsLoaded() AS LOGIC STRICT
	IF !lCAPaintInitialized
		InitializeCAPaint()
	ENDIF
	RETURN lCAPaintInitialized


STATIC GLOBAL lCAPaintInitialized  AS LOGIC
STATIC GLOBAL hCAPaint 				AS PTR
STATIC GLOBAL pfnDIBCreateFromFile   AS DIBCreateFromFile PTR
STATIC GLOBAL pfnDIBCreateFromPTR  AS DIBCreateFromPTR PTR
STATIC GLOBAL pfnDIBDelete       AS DIBDelete PTR
STATIC GLOBAL pfnDIBStretch      AS DIBStretch PTR
STATIC GLOBAL pfnDIBShow       AS DIBShow PTR
STATIC GLOBAL pfnDIBGetInfo      AS DIBGetInfo PTR
STATIC GLOBAL pfnDIBSaveAs       AS DIBSaveAs PTR
STATIC GLOBAL pfnCAPaintLastErrorMsg AS CAPaintLastErrorMsg PTR
STATIC GLOBAL pfnCAPaintLastError  AS CAPaintLastError PTR
STATIC GLOBAL pfnCAPaintShowErrors   AS CAPaintShowErrors PTR


FUNCTION InitializeCAPaint() AS LOGIC STRICT

	hCAPaint := LoadLibrary(String2Psz("CAPAINT.DLL"))
	IF (hCAPaint != NULL_PTR)
		pfnDIBCreateFromFile   := GetProcAddress(hCAPaint, String2Psz("DIBCreateFromFile"))
		pfnDIBCreateFromPTR    := GetProcAddress(hCAPaint, String2Psz("DIBCreateFromPTR"))
		pfnDIBDelete           := GetProcAddress(hCAPaint, String2Psz("DIBDelete"))
		pfnDIBStretch          := GetProcAddress(hCAPaint, String2Psz("DIBStretch"))
		pfnDIBShow             := GetProcAddress(hCAPaint, String2Psz("DIBShow"))
		pfnDIBGetInfo          := GetProcAddress(hCAPaint, String2Psz("DIBGetInfo"))
		pfnDIBSaveAs           := GetProcAddress(hCAPaint, String2Psz("DIBSaveAs"))
		pfnCAPaintLastErrorMsg := GetProcAddress(hCAPaint, String2Psz("CAPaintLastErrorMsg"))
		pfnCAPaintLastError    := GetProcAddress(hCAPaint, String2Psz("CAPaintLastError"))
		pfnCAPaintShowErrors   := GetProcAddress(hCAPaint, String2Psz("CAPaintShowErrors"))

		lCAPaintInitialized  := LOGIC(_CAST, pfnDIBCreateFromFile) .and. LOGIC(_CAST, pfnDIBDelete) .and.;
			LOGIC(_CAST, pfnDIBStretch) .and. LOGIC(_CAST, pfnDIBShow) .and.;
			LOGIC(_CAST, pfnDIBGetInfo) .and. LOGIC(_CAST, pfnDIBSaveAs) .and.;
			LOGIC(_CAST, pfnCAPaintLastErrorMsg) .and. LOGIC(_CAST, pfnCAPaintLastErrorMsg) .and.;
			LOGIC(_CAST, pfnCAPaintShowErrors)
	ENDIF

	RETURN (hCAPaint != NULL_PTR)

FUNCTION FreeCAPaint        ()  AS LOGIC STRICT
	LOCAL lRet	AS LOGIC

	IF hCAPaint = NULL_PTR
		lCAPaintInitialized := .F. 
		lRet := .F. 
	ELSE
		FreeLibrary(hCAPaint)
		hCAPaint := NULL_PTR
		lCAPaintInitialized := .F. 
		lRet := .T. 
	ENDIF

	RETURN lRet
