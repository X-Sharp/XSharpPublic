#ifdef __VULCAN__
   #using System.Runtime.InteropServices
#endif

/*   // dcaton 070739 this is not used anymore, obsolete as per MSDN
CLASS __IMalloc
	METHOD AddRef() AS DWORD STRICT 
RETURN DWORD(_CAST,E_NOTIMPL)

METHOD Alloc(cb AS DWORD) AS PTR STRICT 
RETURN NULL_PTR

METHOD DidAlloc(pv AS PTR) AS INT STRICT 
RETURN E_NOTIMPL

METHOD Free(pv AS PTR) AS VOID STRICT 
RETURN

METHOD GetSize(pv AS PTR) AS DWORD STRICT 
RETURN DWORD(_CAST,E_NOTIMPL)

METHOD HeapMinimize() AS VOID STRICT 
RETURN

CONSTRUCTOR() 
    
    
RETURN 

METHOD QueryInterface(riid AS PTR, ppvObject AS PTR) AS LONGINT STRICT 
RETURN E_NOTIMPL

METHOD Realloc(pv AS PTR, cb AS DWORD) AS PTR STRICT 
RETURN DWORD(_CAST,E_NOTIMPL)

METHOD Release() AS DWORD STRICT 
RETURN DWORD(_CAST,E_NOTIMPL)
END CLASS
*/

CLASS OpenDialog INHERIT StandardFileDialog

CONSTRUCTOR(oOwnWnd, cInitPath, dwFlag) 
	

	SUPER(oOwnWnd,cInitPath,dwFlag)
	IsOpen := TRUE

	IF (LoWord(GetVersion()) < 4)
		Flags := _OR(OFN_SHOWHELP, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLESIZING)
	ELSE
		//Flags := _OR(OFN_EXPLORER, OFN_SHOWHELP, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLESIZING)
		Flags := _OR(OFN_EXPLORER, OFN_SHOWHELP, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLEHOOK)
	ENDIF

	RETURN 

END CLASS

CLASS PaletteDialog INHERIT StandardColorDialog

CONSTRUCTOR(uOwner,oColor) 
	

	IF !IsNil(uOwner)
		IF !IsInstanceOfUsual(uOwner,#Window)
			WCError{#Init,#PaletteDialog,__WCSTypeError,uOwner,1}:@@Throw()
		ENDIF
	ENDIF

	IF (uOwner != NULL_OBJECT)
		_hWnd := uOwner:Handle(4)
	ELSE
		_hWnd := NULL_PTR
	ENDIF

	SUPER(oColor)

	liFlags := CC_RGBINIT

	RETURN 

END CLASS

CLASS SaveAsDialog INHERIT StandardFileDialog

CONSTRUCTOR(oOwnWnd, cInitPath, dwFlag) 
	

	SUPER(oOwnWnd,cInitPath, dwFlag)

	IF (LoWord(GetVersion()) < 4)
		Flags := _OR(OFN_SHOWHELP, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING)
	ELSE
		Flags := _OR(OFN_EXPLORER, OFN_SHOWHELP, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING, OFN_ENABLEHOOK)
	ENDIF

	RETURN 

END CLASS

CLASS SelectDialog INHERIT StandardColorDialog

CONSTRUCTOR(uOwner,oColor) 
	

	IF !IsNil(uOwner)
		IF !IsInstanceOfUsual(uOwner,#Window)
			WCError{#Init,#SelectDialog,__WCSTypeError,uOwner,1}:@@Throw()
		ENDIF
	ENDIF

	IF (uOwner != NULL_OBJECT)
		_hWnd := uOwner:Handle(4)
	ELSE
		_hWnd := NULL_PTR
	ENDIF

	SUPER(oColor)
	liFlags := _OR(CC_PREVENTFULLOPEN, CC_RGBINIT)

	RETURN 

END CLASS

CLASS StandardColorDialog INHERIT StandardDialog
	PROTECT liFlags AS LONGINT
	PROTECT dwDefColor AS DWORD
	PROTECT pCustClr AS PTR
	PROTECT _hWnd AS PTR

METHOD Color() 
	//PP-040425 Changed to use new capability of the Color class
	RETURN Color{dwDefColor, -1}

METHOD Destroy() 
	

	IF (pCustClr != NULL_PTR)
		MemFree(pCustClr)
	ENDIF

	IF !InCollect()
		pCustClr := NULL_PTR
		UnregisterAxit(SELF)
	ENDIF
   RETURN SELF

CONSTRUCTOR(oColor) 
	LOCAL hDC AS PTR
	LOCAL aCustClrs AS strColor
	LOCAL oDefColor AS Color
	

	__LoadComDlgDLL()

	SUPER()
	oDefColor := Color{COLORBLACK}

	IF !IsNil(oColor)
		IF !IsInstanceOfUsual(oColor,#Color)
			WCError{#Init,#StandardColorDialog,__WCSTypeError,oColor,1}:@@Throw()
		ENDIF
	ENDIF

	IF oColor != NULL_OBJECT
		dwDefColor := oColor : ColorRef
	ELSE
		dwDefColor := odefColor : ColorRef
	ENDIF


	// Memory allocation for the structure of 16 DWORDs
	aCustClrs := MemAlloc(_SIZEOF(strColor))
	pCustClr := aCustClrs

	hDC := GetDC(NULL_PTR)
	aCustClrs:s2 := GetBkColor(hDC)
	ReleaseDC(NULL_PTR, hDC)

	aCustClrs:s1 := dwDefColor
	aCustClrs:s3 := aCustClrs:s4 := aCustClrs:s5 := aCustClrs:s6 := ;
		aCustClrs:s7 := aCustClrs:s8 := aCustClrs:s9 := aCustClrs:s10 := ;
		aCustClrs:s11 := aCustClrs:s12 := aCustClrs:s13 := aCustClrs:s14 := ;
		aCustClrs:s15 := aCustClrs:s16 := aCustClrs:s2

	RETURN 

METHOD Show() 
	LOCAL lRet AS LOGIC
	LOCAL iSize := _SIZEOF(_winCHOOSECOLOR) AS DWORD
	LOCAL cc AS _WINCHOOSECOLOR

	cc := MemAlloc(iSize)

	cc:lStructSize := iSize
	cc:hwndOwner := _hWnd
	cc:hInstance := 0
	cc:rgbResult := dwDefColor
	cc:lpCustColors := pCustClr
	cc:Flags := DWORD(_CAST,liFlags)
	//cc.Flags :=CC_FULLOPEN
	cc:lCustData := 0
	cc:lpfnHook := NULL_PTR
	cc:lpTemplateName := NULL_PSZ

	lRet := PCALL(gpfnChooseColor, cc)
	IF lRet
		dwDefColor := cc:rgbResult
	ENDIF

	MemFree(cc)
	RETURN lRet

END CLASS

CLASS StandardDialog INHERIT VObject

CONSTRUCTOR() 
    
    SUPER()


RETURN 
END CLASS

CLASS StandardFileDialog INHERIT StandardDialog
	PROTECT hWnd AS PTR
	PROTECT cDefExt AS STRING
	PROTECT Result AS USUAL
	PROTECT InitDir, Title AS STRING
	PROTECT pszFilters AS PSZ
	PROTECT iFilterLen AS INT
	PROTECT FltIndex, Size AS INT
	PROTECT Flags AS DWORD
	PROTECT FlagsEx AS DWORD
	PROTECT IsOpen AS LOGIC
	PROTECT pOpenFileName AS PTR
	PROTECT oOwner AS OBJECT
	PROTECT strucSelf AS SelfPtr

	//PP-030828 Strong typing
	METHOD __AddFilter(sFilter AS STRING, sFilterDesc AS STRING) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL pszNew AS PSZ
	LOCAL pCurPos AS BYTE PTR
	LOCAL iOldLen AS INT

	iOldLen := iFilterLen

	IF iFilterLen = 0
		iFilterLen := 1
	ENDIF
	iFilterLen += INT(_CAST, SLen(sFilter) + SLen(sFilterDesc) + 2)

	pszNew := MemAlloc(DWORD(iFilterLen))
	MemSet(pszNew, 0, DWORD(iFilterLen))
	pCurPos := pszNew

	IF (PTR(_CAST, pszFilters) != NULL_PTR)
		MemCopy(pCurPos, pszFilters, DWORD(iOldLen-1))
		MemFree(pszFilters)
		pCurPos += iOldLen-1
	ENDIF

	MemCopyString(pCurPos, sFilterDesc, Len(sFilterDesc))
	pCurPos += Len(sFilterDesc)+1

	MemCopyString(pCurPos, sFilter, Len(sFilter))

	pszFilters := pszNew
   RETURN

METHOD __ClearFilters() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF (PTR(_CAST, pszFilters) != NULL_PTR)
		MemFree(pszFilters)
		pszFilters := NULL_PSZ
		iFilterLen := 0
	ENDIF
   RETURN

ACCESS __Flags AS DWORD STRICT 
	//PP-030828 Strong typing
	

	RETURN Flags

ACCESS Caption 
	

	RETURN Title

ASSIGN Caption(cNewCaption) 
	

	Title := AsString(cNewCaption)

	RETURN 

ACCESS DefExt 
	//PP-040101
	RETURN SELF:cDefExt

ASSIGN DefExt( cNew ) 
	//PP-040101
	RETURN ( SELF:cDefExt := cNew )

METHOD Destroy() 
	

	IF (pOpenFileName != NULL_PTR)
		MemFree(pOpenFileName)
	ENDIF

	IF (PTR(_CAST, pszFilters) != NULL_PTR)
		MemFree(pszFilters)
	ENDIF

	IF !InCollect()
		pOpenFileName := NULL_PTR
		UnregisterAxit(SELF)
	ENDIF
   RETURN SELF

METHOD Dispatch(oEvt, hDlg) 

	RETURN 0L

ASSIGN DlgStyle(flag) 

	IF LoWord(GetVersion()) >= 4
		IF flag
			Flags := _OR(Flags, DWORD(_CAST, OFN_EXPLORER))
		ELSE
			Flags := _AND(Flags, DWORD(_CAST, _NOT(OFN_EXPLORER)))
		ENDIF
	ENDIF

	RETURN 

ACCESS FileName 
	

	IF (Len(Result) == 0)
		RETURN NULL_STRING
	ENDIF
	RETURN Result

ACCESS FilterIndex 
	//PP-030910
	RETURN fltindex

METHOD help() 
	

	RETURN NIL

ASSIGN HideReadOnly(flag) 
	

	IF flag
		Flags := _OR(Flags, OFN_HIDEREADONLY)
	ELSE
		Flags := _AND(Flags, _NOT(OFN_HIDEREADONLY))
	ENDIF

	RETURN 

CONSTRUCTOR(uOwner, cInitPath) 
	LOCAL iPos/*, iRest*/ AS INT
	LOCAL cTest, cRest, cAllFiles AS STRING
	LOCAL lHasWildCard := FALSE AS LOGIC
	LOCAL sOFN AS _WINOPENFILENAME
	LOCAL sFilters AS STRING
	LOCAL pVersionInfo IS _WINOSVERSIONINFO

	__LoadComDlgDLL()

	SUPER()
	IF !IsNil(uOwner)
		IF !IsInstanceOfUsual(uOwner,#Window)
			WCError{#Init,#TextBox,__WCSTypeError,uOwner,1}:@@Throw()
		ELSE
			oOwner:=uOwner
		ENDIF
	ENDIF

	IF oOwner != NULL_OBJECT
		hWnd := oOwner:HANDLE()
	ELSE
		hWnd := NULL_PTR
	ENDIF

	pVersionInfo:dwOSVersionInfoSize := _SIZEOF(_WINOSVERSIONINFO)
	GetVersionEx(@pVersionInfo)

	IF pVersionInfo:dwMajorVersion >= 5
		SELF:Size := _SIZEOF(_WINOPENFILENAME)
	ELSE
		SELF:size := OPENFILENAME_SIZE_VERSION_400
	ENDIF

	cDefExt := NULL_STRING
	InitDir := NULL_STRING
	Title := NULL_STRING
	pszFilters := NULL_PSZ
	FltIndex := 1
	IsOpen := FALSE
	// needed for changing dialog's style (by OFN_EXPLORER)
	Flags := OFN_ALLOWMULTISELECT
	
	sOFN := MemAlloc(DWORD(Size))
	MemClear(sOFN, DWORD(Size))
	pOpenFileName := sOFN

	cAllFiles := ResourceString{__WCSAllFiles}:value
	Result := IIF(IsNil(cInitPath), "*.*", cInitPath)

	// Find default extension
	iPos := INT(_CAST, RAt2(".", Result))
	cTest := CharPos(Result, 2)

	IF (iPos # 0 .AND. cTest # NULL_STRING)
		cRest := SubStr(Result, ++iPos) // skip '.'
//		iRest := INT(_CAST, SLen(cRest))

		IF InStr("*", cRest) .OR. Instr("?", cRest)
			lHasWildCard := FALSE
		ENDIF

		// Set as default extension if there are no wildcards
		IF !lHasWildCard
			cDefExt := cRest
		ENDIF

		// Set up filter string
		IF !(cRest == "*") // its not the standard wildcard
			cTest := "*." + cRest
			sFilters := cTest + "|" + cTest + "|" + cAllFiles
		ELSE
			sFilters := cAllFiles
		ENDIF
	ELSE // Set up standard filter string
		sFilters := cAllFiles
	ENDIF


	IF NULL_STRING != sFilters
		iFilterLen := INT(_CAST, SLen(sFilters)+1)
		cTest := SubStr(sFilters, -1) // Grab the string separator '|'
		WHILE (TRUE)
			iPos := INT(_CAST, RAt2(cTest, sFilters))
			IF iPos == 0
				EXIT
			ENDIF
			// Replace '|' for '\0'
			sFilters := Stuff(sFilters, DWORD(iPos), 1, _CHR(0))
		END
		pszFilters := MemAlloc(DWORD(iFilterLen))
		MemSet(pszFilters, 0, DWORD(iFilterLen))
		MemCopy(pszFilters, String2Psz(sFilters), DWORD(iFilterLen-1) )
	ENDIF

   strucSelf :=__WCSelfPtrAlloc(SELF)

	RETURN 

ACCESS InitialDirectory 
	

	RETURN InitDir

ASSIGN InitialDirectory(cNewDir) 
	

	InitDir := AsString(cNewDir)

	RETURN 

ACCESS NoPlacesBar 
	

	RETURN LOGIC(_CAST, _AND(FlagsEx, OFN_EX_NOPLACESBAR))

ASSIGN NoPlacesBar(flag) 
	

	IF flag
		FlagsEx := _OR(FlagsEx, OFN_EX_NOPLACESBAR)
	ELSE
		FlagsEx := _AND(FlagsEx, _NOT(OFN_EX_NOPLACESBAR))
	ENDIF

	RETURN 

ACCESS ReadOnly 
	

	RETURN LOGIC(_CAST, _AND(Flags, OFN_READONLY))

ASSIGN ReadOnly(flag) 
	

	IF flag
		Flags := _OR(Flags, OFN_READONLY)
	ELSE
		Flags := _AND(Flags, _NOT(OFN_READONLY))
	ENDIF

	RETURN 

METHOD SetFilter(uFilter, uFilterDesc, nIndex) 
	LOCAL i AS INT

	

	IF IsLong(nIndex)
		FltIndex := nIndex
	ENDIF

	IF IsString(uFilter) .AND. IsString(uFilterDesc)
		SELF:__ClearFilters()
		SELF:__AddFilter(uFilter, uFilterDesc)
	ELSEIF IsArray(uFilter) .AND. IsArray(uFilterDesc)
		SELF:__ClearFilters()
		FOR i:=1 TO Min(ALen(uFilter), ALen(uFilterDesc))
			SELF:__AddFilter(AsString(uFilter[i]), AsString(uFilterDesc[i]))
		NEXT
	ENDIF

	RETURN NIL

METHOD SetStyle(kStyle, lOnOff) 
	

	DEFAULT(@lOnOff, TRUE)

	IF (lOnOff)
		Flags := _OR(Flags, DWORD(kStyle))
	ELSE
		Flags := _AND(Flags, _NOT(DWORD(kStyle)))
	ENDIF

	RETURN NIL

METHOD SetStyleEx(kStyle, lOnOff) 
	

	DEFAULT(@lOnOff, TRUE)

	IF (lOnOff)
		FlagsEx := _OR(Flags, DWORD(kStyle))
	ELSE
		FlagsEx := _AND(Flags, _NOT(DWORD(kStyle)))
	ENDIF

	RETURN NIL

METHOD Show() 
	LOCAL lRet := TRUE AS LOGIC
	LOCAL sOFN AS _winOPENFILENAME
	LOCAL pszRes AS PSZ
	LOCAL cPathName AS STRING
	LOCAL cDirName AS STRING
	LOCAL pb AS BYTE PTR
	LOCAL iPos, iNxPos AS INT
	LOCAL aResults AS ARRAY

	

	sOFN := pOpenFileName

	pszRes := MemAlloc(MAX_LEN)
	MemCopyString(pszRes, Result, MAX_LEN)

	// Filling in ofn members:
	sOFN:lStructSize 	:= DWORD(SELF:Size)
	sOFN:hwndOwner 	:= hWnd
	sOFN:lpstrFile 	:= pszRes
	sOFN:nMaxFile 	:= MAX_LEN
	sOFN:lpstrFileTitle := NULL_PSZ
	sOFN:nMaxFileTitle := 0
	sOFN:lpstrDefExt 	:= String2Psz(cDefExt)
	sOFN:lpstrInitialDir:= String2Psz(InitDir)
	sOFN:lpstrTitle 	:= String2Psz(Title)
	sOFN:Flags 			:= Flags
	sOFN:FlagsEx 		:= FlagsEx
#ifdef __VULCAN__
   LOCAL StdFileHookDelegate AS __StdFileHookDelegate
   StdFileHookDelegate := __StdFileHookDelegate{ NULL, @__StdFileHook() }
	sOFN:lpfnHook 		:= System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) StdFileHookDelegate )
#else	
	sOFN:lpfnHook 		:= @__StdFileHook()
#endif	
	sOFN:lCustData 	:= LONGINT(_CAST, strucSelf)
	sOFN:lpstrFilter 	:= pszFilters
	sOFN:nFilterIndex := DWORD(FltIndex)

	IF (sOFN:lpfnHook != NULL_PTR)
		sOFN:Flags := _OR (sOFN:Flags, OFN_ENABLEHOOK)
	ELSE
		sOFN:Flags := _AND(sOFN:Flags, _NOT(OFN_ENABLEHOOK))
	ENDIF

	// IF (IsBiDi())
	// sOFN.Flags := _OR(sOFN.Flags, OFN_BIDIDIALOG)
	// ENDIF

	IF IsOpen
		IF PCALL(gpfnGetOpenFileName, sOFN)
			IF (_AND(sOFN:Flags, OFN_ALLOWMULTISELECT) != 0)

				aResults := {}

				IF (_AND(sOFN:Flags, DWORD(_CAST, OFN_EXPLORER)) != 0)
					// Exlorer Style
					cPathName := Psz2String(sOFN:lpstrFile)
					pb := sOFN:lpstrFile
					pb += PszLen(pb) + 1
					IF (PszLen(pb) > 0)
						IF (Right(cPathName, 1) == "\")
							cPathName := Left(cPathName, SLen(cPathName) - 1)
						ENDIF

						WHILE(PszLen(pb) > 0)
							AAdd(aResults, cPathName + "\" + Psz2String(pb))
							pb += PszLen(pb) + 1
						END
						Result := aResults
					ELSE
						Result := cPathName
					ENDIF
				ELSE
					cPathName := Psz2String(sOFN:lpstrFile)
					// Win3 Style
					iPos := LONGINT( At2(" ", cPathName))
					IF (iPos == 0)
						Result := cPathName
					ELSE
						cDirName := Left(cPathName, DWORD(iPos-1))
						IF (Right(cDirName, 1) == "\")
							cDirName := Left(cDirName, SLen(cDirName) - 1)
						ENDIF

						iNxPos := LONGINT( At3(" ", cPathName, DWORD(iPos+1)))
						WHILE (iNxPos != 0)
							AAdd(aResults, cDirName + "\" + SubStr(cPathName, iPos+1, iNxPos-iPos))
							iPos := iNxPos
							iNxPos := LONGINT( At3(" ", cPathName, DWORD(iPos+1)))
						END
						AAdd(aResults, cDirName + "\" + SubStr2(cPathName, DWORD(iPos+1)))
						Result := aResults
					ENDIF
				ENDIF
			ELSE
				Result := Psz2String(sOFN:lpstrFile)
			ENDIF
		ELSE
			Result := NULL_STRING
			lRet := FALSE
		ENDIF
	ELSE
		IF PCALL(gpfnGetSaveFileName, sOFN) .AND. PCALL(gpfnCommDlgExtendedError) = 0
			Result := Psz2String(sOFN:lpstrFile)
		ELSE
			PCALL(gpfnCommDlgExtendedError)
			Result := NULL_STRING
			lRet := FALSE
		ENDIF
	ENDIF

#ifdef __VULCAN__
      // This is needed to prevent the GC from prematurely collecting the delegate
      // while the native call to gpfnGetOpen/SaveFileName is active
      GC.KeepAlive( StdFileHookDelegate )
#endif

	FltIndex := INT(_CAST, sOFN:nFilterIndex)
	Flags := sOFN:Flags

	IF (PTR(_CAST, pszRes) != NULL_PTR)
		MemFree(pszRes)
	ENDIF

	RETURN lRet

END CLASS

CLASS StandardFolderDialog INHERIT StandardDialog
	PROTECT hwndParent AS PTR
	PROTECT sTitle AS STRING
	PROTECT sStart AS STRING
	PROTECT dwType AS DWORD
	PROTECT sResult AS STRING

	//PP-030828 Strong typing
	ACCESS __StartFolder AS STRING STRICT 
	//PP-030828 Strong typing
	RETURN sStart

METHOD DialogCallBack(hWnd, uMsg, lParam, lpData) 
	//PP-030910 suggestion from S Ebert
	IF uMsg = BFFM_INITIALIZED
		IF ! Empty(sStart)
			SendMessage(hWnd, BFFM_SETSELECTION, 1, LONGINT(String2Psz(sStart)))
		ENDIF
	ENDIF
	RETURN SELF

ACCESS FolderName 
	RETURN sResult

CONSTRUCTOR(oOwner, sCaption, sStartFolder, kType) 
   SUPER()
	DEFAULT(@kType, BIF_RETURNONLYFSDIRS)
	DEFAULT(@sCaption, "Browser Folder")
	DEFAULT(@sStartFolder, "")

	__LoadShellDll()

	IF IsMethod(oOwner, #handle)
		hwndParent := oOwner:Handle()
	ENDIF

	sTitle := sCaption
	sStart := sStartFolder
	IF IsNumeric(dwType)
		dwType := kType
	ENDIF
	IF WinDLLVersion{"COMCTL32"}:MajorVersion >= 5
		dwType := _OR(dwType, 0x0040 )	// BIF_NEWDIALOGSTYLE
	ENDIF

   RETURN 

ACCESS Result 
	RETURN sResult

METHOD Show() 
	LOCAL bi IS _winBROWSEINFO
	LOCAL pidl AS PTR
	LOCAL DIM aName[MAX_PATH] AS BYTE
	LOCAL lRet AS LOGIC
	LOCAL p AS SelfPtr

		bi:hwndOwner := hWndParent
		bi:pidlRoot := NULL_PTR
		bi:pszDisplayName := @(aName[1])
		IF Empty(sTitle)
			bi:lpszTitle := NULL_PTR
		ELSE
			bi:lpszTitle := String2Psz(sTitle)  
		ENDIF
		bi:ulFlags := dwType
#ifdef __VULCAN__
      LOCAL d AS FolderDialogCallBackDelegate
      d := FolderDialogCallBackDelegate{ NULL, @FolderDialogCallBack() }
		bi:lpfn := Marshal.GetFunctionPointerForDelegate( (System.Delegate) d )
#else
		bi:lpfn := @FolderDialogCallBack()
#endif

      p :=__WCSelfPtrAlloc(SELF)
        //RvdH 100216 Changed. The original code does not work at all....
        //bi:lParam := LONGINT(_CAST,@p)
		bi:lParam := LONGINT(_CAST,p)
        
		bi:iImage := 0

		pidl := PCALL(gpfnSHBrowseForFolder, @bi)
		IF (pidl != NULL_PTR)
			sResult := Psz2String(bi:pszDisplayName)
			lRet := PCALL(gpfnSHGetPathFromIDList, pidl, bi:pszDisplayName)
			IF lRet
				sResult := Psz2String(bi:pszDisplayName)
			ENDIF
			CoTaskMemFree( pidl )
		ENDIF
      __WCSelfPtrFree(p)

#ifdef __VULCAN__
      // This is needed to prevent the GC from prematurely collecting the delegate
      // while the native call to gpfnSHBrowseForFolder is active
      GC.KeepAlive( d )
#endif

	RETURN lRet

END CLASS

CLASS StandardFontDialog INHERIT StandardDialog
	PROTECT lFlags AS LONGINT
	PROTECT lFixPitchFlag AS LONGINT
	PROTECT lTTYFlag AS LONGINT
	PROTECT lEffectFlag AS LONGINT
	PROTECT lANSIFlag AS LONGINT
	PROTECT lOldFlags AS LONGINT
	PROTECT oColor AS Color
	PROTECT oFont AS Font
	PROTECT iFamily AS INT
	PROTECT hPtr AS PTR
	PROTECT oPrinter AS printer

METHOD EnableANSI(bOnOff) 
	

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		// lANSIFlag := CF_ANSIONLY // obsolete (see Win32 SDK Help file)
		lANSIFlag := CF_SCRIPTSONLY		// tk new
	ELSE
		lANSIFlag := 0
	ENDIF
   RETURN SELF

METHOD EnableEffects(bOnOff) 
	

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lEffectFlag := CF_EFFECTS
	ELSE
		lEffectFlag := 0
	ENDIF

	RETURN SELF

METHOD EnableFixedPitch(bOnOff) 

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lFixPitchFlag := CF_FIXEDPITCHONLY
	ELSE
		lFixPitchFlag := 0
	ENDIF
	RETURN SELF

METHOD EnableTrueType(bOnOff) 
	

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lTTYFlag := CF_TTONLY
	ELSE
		lTTYFlag := 0
	ENDIF
	RETURN SELF

ASSIGN Flags(lInt) 
	

	lFlags := _OR(lFlags, LONGINT(_CAST, lInt))
	RETURN 


ACCESS Font 
	

	RETURN oFont

ASSIGN Font(oInitFont) 
	

	RETURN (oFont := oInitFont)

ACCESS FontColor 
	

	RETURN oColor

ASSIGN FontColor(oNewCol) 
	

	RETURN (oColor := oNewCol)

CONSTRUCTOR(uOwner) 
	

	__LoadComDlgDLL()

	IF !IsNil(uOwner)
		IF !IsInstanceOfUsual(uOwner,#Window) .AND. IsInstanceOfUsual( uOwner,#Printer)
			WCError{#Init,#StandardFontDialog,__WCSTypeError,uOwner,1}:@@Throw()
		ENDIF
	ENDIF

	SUPER()

	oColor := Color{COLORBLACK}

	IF IsInstanceOfUsual(uOwner, #Printer)
		lFlags := CF_PRINTERFONTS
		oPrinter := uOwner
	ELSE
		lFlags := CF_SCREENFONTS
		IF (uOwner != NULL_OBJECT)
			hPtr := uOwner:Handle(4)
		ENDIF
	ENDIF

	SELF:EnableEffects(TRUE)

	RETURN 

METHOD Show() 
	LOCAL bRet AS LOGIC
	LOCAL iFam AS INT
	LOCAL oDim AS Dimension
	LOCAL sChooseFont IS _WINCHOOSEFONT
	LOCAL sLogFont IS _WINLOGFONT
	LOCAL nRed AS DWORD
	LOCAL nBlue AS DWORD
	LOCAL nGreen AS DWORD

	IF (oFont != NULL_OBJECT)
		lFlags := _OR(lFlags, CF_INITTOLOGFONTSTRUCT)

		IF oFont:Bold
			sLogFont:lfWeight := FW_BOLD
		ENDIF

		sLogFont:lfCharSet		    := oFont:__FontCharSet
		sLogFont:lfPitchAndFamily	:= oFont:__FontPitchAndFamily
		IF (oFont:__FontHeight > 0)
			sLogFont:lfHeight		:= oFont:__FontHeight
		ELSE
			sLogFont:lfHeight := oFont:ConvPntToDim(oFont:__PointSize):Height
		ENDIF
		sLogFont:lfItalic 			:= oFont:Italic
		sLogFont:lfStrikeOut		:= oFont:Strikethru
		sLogFont:lfUnderline		:= oFont:underline

		lstrcpy(@sLogFont:lfFaceName, String2Psz(oFont:__FontFaceName))
	ENDIF

	sChooseFont:lStructSize := _SIZEOF(_winCHOOSEFONT)
	sChooseFont:hwndOwner := hPtr

	IF (oPrinter != NULL_OBJECT)
		sChooseFont:hDC := oPrinter:__GetDC()
	ENDIF

	sChooseFont:lpLogFont := @sLogFont
	sChooseFont:Flags := _OR(lFlags, lTTyFlag, lEffectFlag, lAnsiFlag, lFixPitchFlag)


	sChooseFont:rgbColors := oColor:ColorRef
	// pChooseFont.nFontType := SCREEN_FONTTYPE

	bRet := PCALL(gpfnChooseFont, @sChooseFont)

	IF bRet
		iFam := _AND(sLogFont:lfPitchAndFamily, 0B11110000) // Different from CV code!
		DO CASE
		CASE iFam == FF_ROMAN
			iFamily := FONTFAMILY_ROMAN

		CASE iFam == FF_SWISS
			iFamily := FONTFAMILY_SWISS

		CASE iFam == FF_MODERN
			iFamily := FONTFAMILY_MODERN

		CASE iFam == FF_SCRIPT
			iFamily := FONTFAMILY_SCRIPT

		CASE iFam == FF_DECORATIVE
			iFamily := FONTFAMILY_DECORAT

		OTHERWISE
			iFamily := FONTFAMILY_ANY
		ENDCASE
	ENDIF

	oDim := Dimension{sLogFont:lfWidth, sLogFont:lfHeight}
	oFont := Font{iFamily, oDim, Psz2String(@(sLogFont:lfFaceName))}

	oFont:__PointSize := sChooseFont:iPointSize / 10

	nRed := _AND(sChooseFont:rgbColors, 255) // uk
	nGreen := _AND((sChooseFont:rgbColors >> 8), 255) // uk
	nBlue := _AND((sChooseFont:rgbColors >> 16), 255) // uk
	oColor := Color{nRed, nGreen, nBlue} // uk

	sLogFont:lfPitchAndFamily := _AND(sLogFont:lfPitchAndFamily, 0B11)

	DO CASE
	CASE sLogFont:lfPitchAndFamily == FIXED_PITCH
		oFont : @@PitchFixed := TRUE

	CASE sLogFont:lfPitchAndFamily == VARIABLE_PITCH
		oFont : @@PitchVariable := TRUE

		// CASE pLogFont.lfPitchAndFamily == DEFAULT_PITCH
		// oDefFont:Normal := TRUE

	OTHERWISE
		oFont:Normal := TRUE
	ENDCASE

	IF sLogFont:lfItalic != 0
		oFont:Italic := TRUE
	ENDIF

	IF sLogFont:lfUnderline != 0
		oFont:Underline := TRUE
	ENDIF

	IF sLogFont:lfStrikeOut != 0
		oFont:Strikethru := TRUE
	ENDIF

	DO CASE
	CASE sLogFont:lfWeight == FW_THIN
		oFont:Light := TRUE

	CASE sLogFont:lfWeight == FW_EXTRALIGHT
		oFont:Light := TRUE

	CASE sLogFont:lfWeight == FW_LIGHT
		oFont:Light := TRUE

	CASE sLogFont:lfWeight == FW_NORMAL
		oFont:Normal := TRUE

	CASE sLogFont:lfWeight == FW_MEDIUM
		oFont:Normal := TRUE

	CASE sLogFont:lfWeight == FW_SEMIBOLD
		oFont:Normal := TRUE

	CASE sLogFont:lfWeight == FW_BOLD
		oFont:Bold := TRUE

	CASE sLogFont:lfWeight == FW_ULTRABOLD
		oFont:Bold := TRUE

	CASE sLogFont:lfWeight == FW_HEAVY
		oFont:Bold := TRUE

	OTHERWISE
		oFont:Normal := TRUE
	ENDCASE

	RETURN bRet

END CLASS

VOSTRUCT _winBROWSEINFO
	MEMBER hwndOwner AS PTR
	MEMBER pidlRoot AS PTR
	MEMBER pszDisplayName AS PSZ
	MEMBER lpszTitle AS PSZ
	MEMBER ulFlags AS DWORD
	MEMBER lpfn AS PTR
	MEMBER lPARAM AS LONGINT
	MEMBER iImage AS INT

FUNCTION __LoadComDlgDLL()
	LOCAL hDll AS PTR
	LOCAL rsFormat AS ResourceString

	IF glComDlgDllLoaded
		RETURN TRUE
	ENDIF

    hDll := LoadLibrary(String2Psz( "COMDLG32.DLL"))
	IF (hDll == NULL_PTR)
		rsFormat := ResourceString{__WCSLoadLibraryError}
		WCError{#__LoadComDlgDLL, #StandardDialog, VO_Sprintf(rsFormat:value, "COMDLG32.DLL"),,,FALSE}:@@Throw()
		RETURN FALSE
	ENDIF

    gpfnPrintDlg                := GetProcAddress(hDll, String2Psz( "PrintDlgA"))
    gpfnChooseColor             := GetProcAddress(hDll, String2Psz( "ChooseColorA"))
    gpfnGetOpenFileName         := GetProcAddress(hDll, String2Psz( "GetOpenFileNameA"))
    gpfnGetSaveFileName         := GetProcAddress(hDll, String2Psz( "GetSaveFileNameA"))
    gpfnCommDlgExtendedError    := GetProcAddress(hDll, String2Psz( "CommDlgExtendedError"))
    gpfnChooseFont              := GetProcAddress(hDll, String2Psz( "ChooseFontA"))
  	glComDlgDllLoaded := TRUE
	RETURN TRUE


STATIC GLOBAL glComDlgDllLoaded := FALSE AS LOGIC

//function declaration
STATIC GLOBAL gpfnChooseColor AS TChooseColor PTR
STATIC GLOBAL gpfnChooseFont AS TChooseFont PTR

STATIC GLOBAL gpfnCommDlgExtendedError AS TCommDlgExtendedError PTR
STATIC GLOBAL gpfnGetOpenFileName AS TGetOpenFileName PTR
STATIC GLOBAL gpfnGetSaveFileName AS TGetSaveFileName PTR
GLOBAL gpfnPrintDlg AS TPrintDlg PTR

#ifdef __VULCAN__
   DELEGATE __StdFileHookDelegate( hWnd AS PTR, msg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LOGIC
#endif

FUNCTION __StdFileHook(hWnd AS PTR, msg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LOGIC /* WINCALL */
	LOCAL oStdFileDlg AS OBJECT
	LOCAL sOFN AS _winOPENFILENAME
	LOCAL liRet AS LONGINT
	LOCAL sNMHDR AS _winNMHDR
	LOCAL hDlg AS PTR
	LOCAL lFlags AS LONGINT
	LOCAL sSelf AS SelfPtr

	oStdFileDlg := NULL_OBJECT

   //RvdH 080215 This now uses the new functions for Properties
	IF (msg == WM_INITDIALOG)
		sOFN  := PTR(_CAST, lParam)
		sSelf := PTR(_CAST, sOFN:lCustData)
		// The Self Structure was allocated in the constructor
		__WCRegisterProperty(hwnd, sSelf)
	ELSEIF (msg == WM_NOTIFY)
		sNMHDR := PTR(_CAST, lParam)
	   oStdFileDlg := __WCGetObjectByProperty(hwnd)
		IF (sNMHDR:_code == CDN_HELP) .AND. IsInstanceOf(oStdFileDlg, #StandardFileDialog)
			oStdFileDlg:help()
			RETURN TRUE
		ENDIF
	ELSEIF (msg == WM_DESTROY)
	   __WCUnRegisterProperty(hWnd)  // This also frees the memory of the SelfPtr structure
	ENDIF

	oStdFileDlg := __WCGetObjectByProperty(hwnd)
	IF IsInstanceOf(oStdFileDlg, #StandardFileDialog)
		hDlg := hWnd
		lFlags := oStdFileDlg:__Flags
		IF (_AND(lFlags, OFN_EXPLORER) != 0)
			hDlg := GetParent(hDlg)
		ENDIF
		liRet := Send(oStdFileDlg, #Dispatch, @@Event{ hWnd, msg, wParam, lParam}, hDlg)
		RETURN (liRet != 0)
	ENDIF

	RETURN FALSE

#ifdef __VULCAN__
   DELEGATE FolderDialogCallBackDelegate( hWnd AS PTR, uMsg AS DWORD, lParam AS LONGINT, lpData AS LONGINT ) AS INT
#endif

FUNCTION FolderDialogCallBack(hWnd AS PTR, uMsg AS DWORD, lParam AS LONGINT, lpData AS LONGINT) AS INT /* CALLBACK */
	//PP-030910 suggestion from S Ebert
	//PP-040418 Issue 12715 removed STATIC from LOCAL declaration
	//PP-040421 Issue 12801 GC was moving object
	LOCAL oDlg AS StandardFolderDialog
	LOCAL p AS selfptr

	p := PTR(_CAST,lpData)

   oDlg := __WCSelfPtr2Object(p)
	IF oDlg != NULL_OBJECT
		oDlg:DialogCallBack(hWnd, uMsg, lParam,  lpData)
	ENDIF

	RETURN 0

STATIC FUNCTION TChooseColor(lpcc AS _winCHOOSECOLOR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TChooseFont(lpcf AS _winCHOOSEFONT) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCommDlgExtendedError() AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TGetOpenFileName(lpofn AS _winOPENFILENAME) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TGetSaveFileName(lpofn AS _winOPENFILENAME) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

FUNCTION TPrintDlg(lppd AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE



#region defines
DEFINE BFFM_ENABLEOK := (WM_USER + 101)
DEFINE BFFM_INITIALIZED := 1
DEFINE BFFM_SELCHANGED := 2
DEFINE BFFM_SETSELECTION := (WM_USER + 102)
DEFINE BFFM_SETSTATUSTEXT := (WM_USER + 100)
DEFINE BIF_BROWSEFORCOMPUTER := 0x1000
DEFINE BIF_BROWSEFORPRINTER := 0x2000
DEFINE BIF_BROWSEINCLUDEFILES := 0x4000
DEFINE BIF_DONTGOBELOWDOMAIN := 0x0002
DEFINE BIF_RETURNFSANCESTORS := 0x0008
DEFINE BIF_RETURNONLYFSDIRS := 0x0001
DEFINE BIF_STATUSTEXT := 0x0004
#endregion
