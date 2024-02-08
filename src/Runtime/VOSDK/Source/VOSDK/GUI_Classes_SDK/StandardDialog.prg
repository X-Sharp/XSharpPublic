#ifdef __VULCAN__
   USING System.Runtime.InteropServices
#endif




/// <include file="Gui.xml" path="doc/OpenDialog/*" />
CLASS OpenDialog INHERIT StandardFileDialog


/// <include file="Gui.xml" path="doc/OpenDialog.ctor/*" />
CONSTRUCTOR(oOwnWnd, cInitPath, dwFlag)




	SUPER(oOwnWnd,cInitPath,dwFlag)
	IsOpen := TRUE


	IF (LoWord(GetVersion()) < 4)
		Flags := _OR(OFN_SHOWHELP, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLESIZING)
	ELSE
		Flags := _OR(OFN_EXPLORER, OFN_SHOWHELP, OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST,OFN_ENABLEHOOK,OFN_ENABLESIZING)
	ENDIF


	RETURN


END CLASS


/// <include file="Gui.xml" path="doc/PaletteDialog/*" />
CLASS PaletteDialog INHERIT StandardColorDialog


/// <include file="Gui.xml" path="doc/PaletteDialog.ctor/*" />
CONSTRUCTOR(uOwner,oColor)




	IF !IsNil(uOwner)
		IF !(uOwner IS Window)
			WCError{#Init,#PaletteDialog,__WCSTypeError,uOwner,1}:Throw()
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


/// <include file="Gui.xml" path="doc/SaveAsDialog/*" />
CLASS SaveAsDialog INHERIT StandardFileDialog


/// <include file="Gui.xml" path="doc/SaveAsDialog.ctor/*" />
CONSTRUCTOR(oOwnWnd, cInitPath, dwFlag)




	SUPER(oOwnWnd,cInitPath, dwFlag)


	IF (LoWord(GetVersion()) < 4)
		Flags := _OR(OFN_SHOWHELP, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING)
	ELSE
		Flags := _OR(OFN_EXPLORER, OFN_SHOWHELP, OFN_PATHMUSTEXIST, OFN_HIDEREADONLY, OFN_ENABLESIZING, OFN_ENABLEHOOK)
	ENDIF


	RETURN


END CLASS


/// <include file="Gui.xml" path="doc/SelectDialog/*" />
CLASS SelectDialog INHERIT StandardColorDialog


/// <include file="Gui.xml" path="doc/SelectDialog.ctor/*" />
CONSTRUCTOR(uOwner,oColor)




	IF !IsNil(uOwner)
		IF !(uOwner IS Window)
			WCError{#Init,#SelectDialog,__WCSTypeError,uOwner,1}:Throw()
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


/// <include file="Gui.xml" path="doc/StandardColorDialog/*" />
CLASS StandardColorDialog INHERIT StandardDialog
	PROTECT liFlags AS LONGINT
	PROTECT dwDefColor AS DWORD
	PROTECT pCustClr AS PTR
	PROTECT _hWnd AS PTR


/// <include file="Gui.xml" path="doc/StandardColorDialog.Color/*" />
METHOD Color()
	//PP-040425 Changed to use new capability of the Color class
	RETURN Color{dwDefColor, -1}


/// <include file="Gui.xml" path="doc/StandardColorDialog.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF (pCustClr != NULL_PTR)
		MemFree(pCustClr)
	ENDIF


	IF !InCollect()
		pCustClr := NULL_PTR
		UnregisterAxit(SELF)
	ENDIF
   RETURN SELF


/// <include file="Gui.xml" path="doc/StandardColorDialog.ctor/*" />
CONSTRUCTOR(oColor)
	LOCAL hDC AS PTR
	LOCAL aCustClrs AS strColor
	LOCAL oDefColor AS Color




	//__LoadComDlgDLL()


	SUPER()
	oDefColor := Color{COLORBLACK}


	IF !IsNil(oColor)
		IF !(oColor IS Color)
			WCError{#Init,#StandardColorDialog,__WCSTypeError,oColor,1}:Throw()
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


/// <include file="Gui.xml" path="doc/StandardColorDialog.Show/*" />
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


	lRet := ChooseColor( cc)
	IF lRet
		dwDefColor := cc:rgbResult
	ENDIF


	MemFree(cc)
	RETURN lRet


END CLASS


/// <include file="Gui.xml" path="doc/StandardDialog/*" />
CLASS StandardDialog INHERIT VObject


/// <include file="Gui.xml" path="doc/StandardDialog.ctor/*" />
CONSTRUCTOR()


    SUPER()




RETURN
END CLASS


/// <include file="Gui.xml" path="doc/StandardFileDialog/*" />
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
 /// <exclude />
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


	IF  pszFilters != NULL_PSZ
		MemCopy(pCurPos, pszFilters, DWORD(iOldLen-1))
		MemFree(pszFilters)
		pCurPos += iOldLen-1
	ENDIF


	MemCopyString(pCurPos, sFilterDesc, Len(sFilterDesc))
	pCurPos += Len(sFilterDesc)+1


	MemCopyString(pCurPos, sFilter, Len(sFilter))


	pszFilters := pszNew
   RETURN


 /// <exclude />
METHOD __ClearFilters() AS VOID STRICT
	//PP-030828 Strong typing




	IF pszFilters != NULL_PSZ
		MemFree(pszFilters)
		pszFilters := NULL_PSZ
		iFilterLen := 0
	ENDIF
   RETURN


 /// <exclude />
ACCESS __Flags AS DWORD STRICT
	//PP-030828 Strong typing




	RETURN Flags


/// <include file="Gui.xml" path="doc/StandardFileDialog.Caption/*" />
ACCESS Caption




	RETURN Title


/// <include file="Gui.xml" path="doc/StandardFileDialog.Caption/*" />
ASSIGN Caption(cNewCaption)




	Title := AsString(cNewCaption)


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.DefExt/*" />
ACCESS DefExt
	//PP-040101
	RETURN SELF:cDefExt


/// <include file="Gui.xml" path="doc/StandardFileDialog.DefExt/*" />
ASSIGN DefExt( cNew )
	//PP-040101
	RETURN ( SELF:cDefExt := cNew )


/// <include file="Gui.xml" path="doc/StandardFileDialog.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF (pOpenFileName != NULL_PTR)
		MemFree(pOpenFileName)
        pOpenFileName := NULL_PTR
	ENDIF


	IF pszFilters != NULL_PSZ
		MemFree(pszFilters)
        pszFilters := NULL_PSZ
	ENDIF


	IF !InCollect()
		pOpenFileName := NULL_PTR
		UnregisterAxit(SELF)
	ENDIF
   RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFileDialog.Dispatch/*" />
METHOD Dispatch(oEvt, hDlg)


	RETURN 0L


/// <include file="Gui.xml" path="doc/StandardFileDialog.DlgStyle/*" />
ASSIGN DlgStyle(flag)


	IF LoWord(GetVersion()) >= 4
		IF flag
			Flags := _OR(Flags, DWORD(_CAST, OFN_EXPLORER))
		ELSE
			Flags := _AND(Flags, DWORD(_CAST, _NOT(OFN_EXPLORER)))
		ENDIF
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.FileName/*" />
ACCESS FileName




	IF (Len(Result) == 0)
		RETURN NULL_STRING
	ENDIF
	RETURN Result


/// <include file="Gui.xml" path="doc/StandardFileDialog.FilterIndex/*" />
ACCESS FilterIndex
	//PP-030910
	RETURN fltindex


/// <include file="Gui.xml" path="doc/StandardFileDialog.help/*" />
METHOD help()




	RETURN NIL


/// <include file="Gui.xml" path="doc/StandardFileDialog.HideReadOnly/*" />
ASSIGN HideReadOnly(flag)




	IF flag
		Flags := _OR(Flags, OFN_HIDEREADONLY)
	ELSE
		Flags := _AND(Flags, _NOT(OFN_HIDEREADONLY))
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.ctor/*" />
CONSTRUCTOR(uOwner, cInitPath)
	LOCAL iPos/*, iRest*/ AS INT
	LOCAL cTest, cRest, cAllFiles AS STRING
	LOCAL lHasWildCard := FALSE AS LOGIC
	LOCAL sOFN AS _WINOPENFILENAME
	LOCAL sFilters AS STRING
	LOCAL pVersionInfo IS _WINOSVERSIONINFO


	//__LoadComDlgDLL()


	SUPER()
	IF !IsNil(uOwner)
		IF !(uOwner IS Window)
			WCError{#Init,#TextBox,__WCSTypeError,uOwner,1}:Throw()
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


/// <include file="Gui.xml" path="doc/StandardFileDialog.InitialDirectory/*" />
ACCESS InitialDirectory




	RETURN InitDir


/// <include file="Gui.xml" path="doc/StandardFileDialog.InitialDirectory/*" />
ASSIGN InitialDirectory(cNewDir)




	InitDir := AsString(cNewDir)


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.NoPlacesBar/*" />
ACCESS NoPlacesBar




	RETURN LOGIC(_CAST, _AND(FlagsEx, OFN_EX_NOPLACESBAR))


/// <include file="Gui.xml" path="doc/StandardFileDialog.NoPlacesBar/*" />
ASSIGN NoPlacesBar(flag)




	IF flag
		FlagsEx := _OR(FlagsEx, OFN_EX_NOPLACESBAR)
	ELSE
		FlagsEx := _AND(FlagsEx, _NOT(OFN_EX_NOPLACESBAR))
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.ReadOnly/*" />
ACCESS ReadOnly




	RETURN LOGIC(_CAST, _AND(Flags, OFN_READONLY))


/// <include file="Gui.xml" path="doc/StandardFileDialog.ReadOnly/*" />
ASSIGN ReadOnly(flag)




	IF flag
		Flags := _OR(Flags, OFN_READONLY)
	ELSE
		Flags := _AND(Flags, _NOT(OFN_READONLY))
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/StandardFileDialog.SetFilter/*" />
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


/// <include file="Gui.xml" path="doc/StandardFileDialog.SetStyle/*" />
METHOD SetStyle(kStyle, lOnOff)

	DEFAULT(@lOnOff, TRUE)

	IF (lOnOff)
		Flags := _OR(Flags, DWORD(kStyle))
	ELSE
		Flags := _AND(Flags, _NOT(DWORD(kStyle)))
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/StandardFileDialog.SetStyleEx/*" />
METHOD SetStyleEx(kStyle, lOnOff)

	DEFAULT(@lOnOff, TRUE)


	IF (lOnOff)
		FlagsEx := _OR(Flags, DWORD(kStyle))
	ELSE
		FlagsEx := _AND(Flags, _NOT(DWORD(kStyle)))
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/StandardFileDialog.Show/*" />
METHOD Show()
	LOCAL lRet := TRUE AS LOGIC
	LOCAL sOFN AS _winOPENFILENAME
	LOCAL pszRes AS PSZ
	LOCAL cPathName AS STRING
	LOCAL cDirName AS STRING
	LOCAL pb AS BYTE PTR
	LOCAL iPos, iNxPos AS INT
	LOCAL aResults AS ARRAY


    TRY



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
	sOFN:lpstrDefExt 	:= StringAlloc(cDefExt)
	sOFN:lpstrInitialDir:= StringAlloc(InitDir)
	sOFN:lpstrTitle 	:= StringAlloc(Title)
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
		IF GetOpenFileName( sOFN)
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
		IF GetSaveFileName( sOFN) .AND. CommDlgExtendedError() = 0
			Result := Psz2String(sOFN:lpstrFile)
		ELSE
			CommDlgExtendedError()
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


    FINALLY
	    IF pszRes != NULL_PTR
		    MemFree(pszRes)
	    ENDIF

        IF sOFN != NULL
            sOFN:lpstrFile := NULL_PSZ
            MemFree(sOFN:lpstrDefExt 	)
            sOFN:lpstrDefExt := NULL_PSZ
            MemFree(sOFN:lpstrInitialDir)
            sOFN:lpstrInitialDir := NULL_PSZ
            MemFree(sOFN:lpstrTitle 	)
            sOFN:lpstrTitle := NULL_PSZ
        ENDIF
    END TRY


	RETURN lRet

END CLASS


/// <include file="Gui.xml" path="doc/StandardFolderDialog/*" />
CLASS StandardFolderDialog INHERIT StandardDialog
	PROTECT hwndParent AS PTR
	PROTECT sTitle AS STRING
	PROTECT sStart AS STRING
	PROTECT dwType AS DWORD
	PROTECT sResult AS STRING


	//PP-030828 Strong typing
 /// <exclude />
	ACCESS __StartFolder AS STRING STRICT
	//PP-030828 Strong typing
	RETURN sStart


/// <include file="Gui.xml" path="doc/StandardFolderDialog.DialogCallBack/*" />
METHOD DialogCallBack(hWnd, uMsg, lParam, lpData)
	//PP-030910 suggestion from S Ebert
	IF uMsg = BFFM_INITIALIZED
		IF ! Empty(sStart)
			SendMessage(hWnd, BFFM_SETSELECTION, 1, LONGINT(String2Psz(sStart)))
		ENDIF
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFolderDialog.FolderName/*" />
ACCESS FolderName
	RETURN sResult


/// <include file="Gui.xml" path="doc/StandardFolderDialog.ctor/*" />
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


/// <include file="Gui.xml" path="doc/StandardFolderDialog.Result/*" />
ACCESS Result
	RETURN sResult


/// <include file="Gui.xml" path="doc/StandardFolderDialog.Show/*" />
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
      LOCAL d AS __FolderDialogCallBackDelegate
      d := __FolderDialogCallBackDelegate{ NULL, @__FolderDialogCallBack() }
		bi:lpfn := Marshal.GetFunctionPointerForDelegate( (System.Delegate) d )
#else
		bi:lpfn := @__FolderDialogCallBack()
#endif


      p :=__WCSelfPtrAlloc(SELF)
        //RvdH 100216 Changed. The original code does not work at all....
        //bi:lParam := LONGINT(_CAST,@p)
		bi:lParam := LONGINT(_CAST,p)


		bi:iImage := 0


		pidl := SHBrowseForFolder( @bi)
		IF (pidl != NULL_PTR)
			sResult := Psz2String(bi:pszDisplayName)
			lRet := SHGetPathFromIDList( pidl, bi:pszDisplayName)
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


/// <include file="Gui.xml" path="doc/StandardFontDialog/*" />
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


/// <include file="Gui.xml" path="doc/StandardFontDialog.EnableANSI/*" />
METHOD EnableANSI(bOnOff)

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		// lANSIFlag := CF_ANSIONLY // obsolete (see Win32 SDK Help file)
		lANSIFlag := CF_SCRIPTSONLY		// tk new
	ELSE
		lANSIFlag := 0
	ENDIF
   RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFontDialog.EnableEffects/*" />
METHOD EnableEffects(bOnOff)




	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lEffectFlag := CF_EFFECTS
	ELSE
		lEffectFlag := 0
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFontDialog.EnableFixedPitch/*" />
METHOD EnableFixedPitch(bOnOff)


	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lFixPitchFlag := CF_FIXEDPITCHONLY
	ELSE
		lFixPitchFlag := 0
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFontDialog.EnableTrueType/*" />
METHOD EnableTrueType(bOnOff)

	DEFAULT(@bOnOff, TRUE)
	IF bOnOff
		lTTYFlag := CF_TTONLY
	ELSE
		lTTYFlag := 0
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/StandardFontDialog.Flags/*" />
ASSIGN Flags(lInt)

	lFlags := _OR(lFlags, LONGINT(_CAST, lInt))
	RETURN




/// <include file="Gui.xml" path="doc/StandardFontDialog.Font/*" />
ACCESS Font




	RETURN oFont


/// <include file="Gui.xml" path="doc/StandardFontDialog.Font/*" />
ASSIGN Font(oInitFont)




	RETURN (oFont := oInitFont)


/// <include file="Gui.xml" path="doc/StandardFontDialog.FontColor/*" />
ACCESS FontColor




	RETURN oColor


/// <include file="Gui.xml" path="doc/StandardFontDialog.FontColor/*" />
ASSIGN FontColor(oNewCol)




	RETURN (oColor := oNewCol)


/// <include file="Gui.xml" path="doc/StandardFontDialog.ctor/*" />
CONSTRUCTOR(uOwner)




	//__LoadComDlgDLL()


	IF !IsNil(uOwner)
		IF !(uOwner IS Window) .AND. ( uOwner IS Printer)
			WCError{#Init,#StandardFontDialog,__WCSTypeError,uOwner,1}:Throw()
		ENDIF
	ENDIF


	SUPER()


	oColor := Color{COLORBLACK}


	IF (uOwner IS Printer)
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


/// <include file="Gui.xml" path="doc/StandardFontDialog.Show/*" />
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
	sChooseFont:Flags := (DWORD) _OR(lFlags, lTTyFlag, lEffectFlag, lAnsiFlag, lFixPitchFlag)




	sChooseFont:rgbColors := oColor:ColorRef
	// pChooseFont.nFontType := SCREEN_FONTTYPE


	bRet := ChooseFont( @sChooseFont)


	IF bRet
		iFam := _AND(sLogFont:lfPitchAndFamily, 0B11110000) // Different from CV code!
		SWITCH iFam
		CASE FF_ROMAN
			iFamily := FONTFAMILY_ROMAN


		CASE FF_SWISS
			iFamily := FONTFAMILY_SWISS


		CASE FF_MODERN
			iFamily := FONTFAMILY_MODERN


		CASE FF_SCRIPT
			iFamily := FONTFAMILY_SCRIPT


		CASE FF_DECORATIVE
			iFamily := FONTFAMILY_DECORAT


		OTHERWISE
			iFamily := FONTFAMILY_ANY
		END SWITCH
	ENDIF


	oDim := Dimension{sLogFont:lfWidth, sLogFont:lfHeight}
	oFont := Font{iFamily, oDim, Psz2String(@(sLogFont:lfFaceName))}


	oFont:__PointSize := sChooseFont:iPointSize / 10


	nRed := _AND(sChooseFont:rgbColors, 255) // uk
	nGreen := _AND((sChooseFont:rgbColors >> 8), 255) // uk
	nBlue := _AND((sChooseFont:rgbColors >> 16), 255) // uk
	oColor := Color{nRed, nGreen, nBlue} // uk


	sLogFont:lfPitchAndFamily := (BYTE) _AND(sLogFont:lfPitchAndFamily, 0B11)


	DO CASE
	CASE sLogFont:lfPitchAndFamily == FIXED_PITCH
		oFont:PitchFixed := TRUE


	CASE sLogFont:lfPitchAndFamily == VARIABLE_PITCH
		oFont:PitchVariable := TRUE


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


	SWITCH sLogFont:lfWeight
	CASE FW_THIN
		oFont:Light := TRUE


	CASE FW_EXTRALIGHT
		oFont:Light := TRUE


	CASE FW_LIGHT
		oFont:Light := TRUE


	CASE FW_NORMAL
		oFont:Normal := TRUE


	CASE FW_MEDIUM
		oFont:Normal := TRUE


	CASE FW_SEMIBOLD
		oFont:Normal := TRUE


	CASE FW_BOLD
		oFont:Bold := TRUE


	CASE FW_ULTRABOLD
		oFont:Bold := TRUE


	CASE FW_HEAVY
		oFont:Bold := TRUE


	OTHERWISE
		oFont:Normal := TRUE
	END SWITCH


	RETURN bRet


END CLASS


/// <exclude/>
VOSTRUCT _winBROWSEINFO
	MEMBER hwndOwner AS PTR
	MEMBER pidlRoot AS PTR
	MEMBER pszDisplayName AS PSZ
	MEMBER lpszTitle AS PSZ
	MEMBER ulFlags AS DWORD
	MEMBER lpfn AS PTR
	MEMBER lPARAM AS LONGINT
	MEMBER iImage AS INT


 /// <exclude />
FUNCTION __LoadComDlgDLL()
    // no longer needed in .Net
	RETURN TRUE






#ifdef __VULCAN__
/// <exclude/>
   DELEGATE __StdFileHookDelegate( hWnd AS PTR, msg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LOGIC
#endif


 /// <exclude />
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
	IF oStdFileDlg IS StandardFileDialog VAR stdFileDlg
		hDlg := hWnd
		lFlags := oStdFileDlg:__Flags
		IF (_AND(lFlags, OFN_EXPLORER) != 0)
			hDlg := GetParent(hDlg)
		ENDIF
		liRet := stdFileDlg:Dispatch( @@Event{ hWnd, msg, wParam, lParam}, hDlg)
		RETURN (liRet != 0)
	ENDIF


	RETURN FALSE


#ifdef __VULCAN__
/// <exclude/>
   DELEGATE __FolderDialogCallBackDelegate( hWnd AS PTR, uMsg AS DWORD, lParam AS LONGINT, lpData AS LONGINT ) AS INT
#endif


 /// <exclude />
FUNCTION __FolderDialogCallBack(hWnd AS PTR, uMsg AS DWORD, lParam AS LONGINT, lpData AS LONGINT) AS INT /* CALLBACK */
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
