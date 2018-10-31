PARTIAL CLASS HelpDisplay INHERIT VObject
	PROTECT cFileName AS STRING
	PROTECT wError AS LONGINT				        //RvdH 070205 changed from WORD to LONG
	PROTECT oTopApp AS TopAppWindow
	PROTECT hWnd AS PTR
	PROTECT pfnHTMLHelp AS THTMLHelp PTR
	PROTECT lHTMLHelp AS LOGIC
	PROTECT cPopUpTopic AS STRING //SE-060519

	//PP-030929 Show default page when HelpContents?
	EXPORT DefaultPageOnContents := TRUE AS LOGIC

	//SE-060521
	EXPORT Win32Processing := FALSE AS LOGIC

	//PP-030828 Strong typing
	METHOD __CallHelp(uCommand AS DWORD, dwData AS DWORD, cTopic := "" AS STRING) AS LOGIC STRICT 
   //SE-060519
   LOCAL cFName AS STRING

   

   IF lHTMLHelp
      cFName := cFileName
      //RvdH 060608 optimized
      //IF ! Empty(cTopic)
      IF SLen(cTopic) > 0
    	   cFName += "::/"+cTopic
      ENDIF
      RETURN LOGIC(_CAST, PCALL(pfnHTMLHelp, hWnd, String2Psz(cFName), uCommand, dwData))
   ENDIF

   RETURN WinHelp(hWnd, String2Psz(cFileName), uCommand, dwData)

METHOD __VerifyHelp() AS LOGIC STRICT
	//PP-030828 Strong typing
	

	IF (hWnd == NULL_PTR) .AND. !lHTMLHelp // Create dummy window for use with help system
		// oTopApp := TopAppWindow{}
		// hWnd := oTopApp:Handle()
		IF (oApp != NULL_OBJECT)
			hWnd:= oApp:__HelpWndHandle
		ENDIF
		IF (hWnd == NULL_PTR)
			wError := HdUnknown
			RETURN FALSE
		ENDIF
	ENDIF

	RETURN TRUE

METHOD Destroy() 
	

	IF (hWnd != NULL_PTR)
		IF !lHTMLHelp
			WinHelp(hWnd, String2Psz(cFileName), HELP_QUIT, 0)
		ELSE
			PCALL(pfnHTMLHelp, NULL_PTR, NULL_PSZ, HH_CLOSE_ALL, 0)
		ENDIF
		//DestroyWindow(hWnd)
	ENDIF

	IF !InCollect()
		UnregisterAxit(SELF)
		hWnd := NULL_PTR
		cfileName := NULL_STRING
		IF (oTopApp != NULL_OBJECT)
			oTopApp:Destroy()
			oTopApp := NULL_OBJECT
		ENDIF
	ENDIF

	RETURN NIL

METHOD EnableHTMLHelp(lEnable, cPopUpTopic) 
   //SE-060519
   

	Default(@lEnable, TRUE)

	lHTMLHelp := FALSE

	IF lEnable
		IF (pfnHTMLHelp := GetProcAddress(LoadLibrary(String2Psz("HHCTRL.OCX")), String2Psz("HtmlHelpA"))) == NULL_PTR
			RETURN FALSE
		ENDIF

		lHTMLHelp := TRUE

	   IF IsString(cPopUpTopic)
       	SELF:cPopUpTopic := cPopUpTopic
      ELSE
   		SELF:cPopUpTopic := "PopUps.Txt"
   	ENDIF
	ENDIF

   RETURN TRUE

METHOD HelpError() 
	

	RETURN wError

CONSTRUCTOR(cFileName, oOwnerWindow, lWin32Processing) 
	//SE-060519
	

	IF !IsString(cFileName)
		WCError{#Init,#HelpDisplay,__WCSTypeError,cFileName,1}:@@Throw()
	ENDIF

	IF !IsNil(oOwnerWindow)
		IF !IsInstanceOfUsual(oOwnerWindow, #Window)
			WCError{#Init,#HelpDisplay,__WCSTypeError,oOwnerWindow,2}:@@Throw()
		ELSE
			hwnd := oOwnerWindow:Handle()
		ENDIF
	ENDIF

	SUPER()
	

	SELF:cFileName := cFileName

	IF IsLogic(lWin32Processing)
		SELF:Win32Processing := lWin32Processing
	ENDIF

	RETURN 

METHOD Show(cKeyword, symLookupType) 
   //SE-060519
	LOCAL cKey AS STRING
	LOCAL wLen AS DWORD
	LOCAL strucMKH AS _WinMultiKeyHelp
	LOCAL wMK AS DWORD
	LOCAL cFirst AS STRING
	LOCAL lRetVal AS LOGIC
	LOCAL hhLink IS _winHH_AKLINK
	LOCAL dwCommand AS DWORD
	LOCAL DIM dwHH[2,2] AS DWORD
	LOCAL sHelpInfo AS _winHelpInfo
	LOCAL pszKey	AS PSZ
	

	//PP-030929
	Default(@symLookupType, #KEYWORD)

	IF !IsString(cKeyword)
		WCError{#Show,#HelpDisplay,__WCSTypeError,cKeyword,1}:@@Throw()
		wError := HdInvalidKey
		RETURN FALSE
	ENDIF

	cKey := cKeyword

	IF (cKey == NULL_STRING)
		wError := HdInvalidKey
		RETURN FALSE
	ENDIF

	IF !SELF:__VerifyHelp()
		RETURN FALSE
	ENDIF

   IF ! IsSymbol(symLookupType)
	   IF IsPtr(symLookupType)
	      sHelpInfo := symLookupType
	      IF sHelpInfo != NULL_PTR
	         hWnd := sHelpInfo:hItemHandle
	      ENDIF
	   ENDIF
      symLookupType := #KEYWORD
   ENDIF

	wLen   := SLen(cKey)
	cFirst := Left(cKey,1)

	DO CASE
	CASE cFirst>="0" .AND. cFirst<="9"
		// When numeric, take the value of the key
	   lRetVal := SELF:__CallHelp(IIF(lHTMLHelp, HH_HELP_CONTEXT, HELP_CONTEXT), Val(cKey))
	   
	CASE ! lHTMLHelp .AND. cFirst="[" .AND. wLen>3 .AND. SubStr3(cKey,3,1)="]"
		// When [Letter] store the Letter in the keylist and use the rest of the key
		wMK :=  _SIZEOF(_winMULTIKEYHELP) + wLen
		strucMKH := MemAlloc(wMK)
		strucMKH:mkSize := wMK
		strucMKH:mkKeyList := Asc(SubStr3(cKey,2,1))
		pszKey := StringAlloc(SubStr3(cKey,3, wLen-3))
		MemCopy(@strucMKH:szKeyPhrase,pszKey,wLen-3)
		MemFree(pszKey)
		lRetVal := SELF:__CallHelp(HELP_MULTIKEY, DWORD(_CAST,strucMKH))
		MemFree(strucMKH)
	CASE cFirst="#" .AND. sHelpInfo != NULL_PTR  //SE-060519
		// When # use the rest as Topic Number
   	IF lHTMLHelp
	      dwHH[1,1] := DWORD(sHelpInfo:iCtrlId)
	      dwHH[1,2] := Val(SubStr2(cKey,2))
	      dwHH[2,1] := 0
	      dwHH[2,2] := 0
			lRetVal := SELF:__CallHelp(HH_TP_HELP_WM_HELP, DWORD(_CAST,@dwHH), cPopUpTopic)
		ELSE
			//SELF:__CallHelp(HELP_SETPOPUP_POS, DWORD(MakeLong(WORD(sHelpInfo.MousePos.x), WORD(sHelpInfo.MousePos.y))))
			// Assume it is a number
			lRetVal := SELF:__CallHelp(HELP_CONTEXTPOPUP, DWORD(Val(SubStr2(cKey,2))))
	   ENDIF
	CASE cKey=="HelpOnHelp"
      IF ! lHTMLHelp
		   lRetVal := SELF:__CallHelp(HELP_HELPONHELP, 0)
		ENDIF
	CASE cKey=="HelpIndex"
	   lRetVal := SELF:__CallHelp(IIF(lHTMLHelp, HH_DISPLAY_INDEX, HELP_FINDER), 0)
	CASE cKey=="HelpSearch"
		IF lHTMLHelp
			//PP-030929
			lRetVal := SELF:__CallHelp(HH_DISPLAY_SEARCH, 0)
		ENDIF
	CASE cKey=="HelpContents"
		//PP-030929
		IF ! lHTMLHelp             
			pszKey 	:= StringAlloc(cKey)
			lRetVal := SELF:__CallHelp(HELP_CONTENTS , DWORD(_CAST, pszKey))
			MemFree(pszKey)
		ELSE
 			lRetVal := SELF:__CallHelp(HH_DISPLAY_TOC, 0)
 			IF lRetVal
 			   IF SELF:DefaultPageOnContents
				   lRetVal := SELF:__CallHelp(HH_DISPLAY_TOPIC, 0)
			   ENDIF
			ENDIF
		ENDIF

	OTHERWISE
		pszKey 	:= StringAlloc(cKey)         
		IF ! lHTMLHelp
			//RvdH 070628 Changd HELP_CONTENTS to HELP_KEY in the next line. 
			lRetVal := SELF:__CallHelp(HELP_KEY, DWORD(_CAST, pszKey))
			IF ! lRetVal
				lRetVal := SELF:__CallHelp(HELP_FINDER, 0)
			ENDIF
		ELSE
			lRetVal := SELF:__CallHelp(HH_DISPLAY_TOPIC, 0)
			IF lRetVal
				IF symLookupType == #ALINK //PP-030929
   				dwCommand := HH_ALINK_LOOKUP
   			ELSE
				   IF cFirst = "~"
	   			   cKey := SubStr2(cKey, 2)
	   				dwCommand := HH_ALINK_LOOKUP
	   			ELSE
	   				dwCommand := HH_KEYWORD_LOOKUP
	   			ENDIF
	   		ENDIF

   			hhLink:cbStruct     := _SIZEOF(_winHH_AKLINK)
   			hhLink:fIndexOnFail := TRUE
   			hhLink:pszKeywords  := pszKey
			   lRetVal := SELF:__CallHelp(dwCommand, DWORD(_CAST, @hhLink))
			ENDIF
			MemFree(pszKey)
		ENDIF
	ENDCASE

	IF !lRetVal
		wError := HDInvalidKey
	ELSE
		wError := HDOk
	ENDIF

	RETURN lRetVal

END CLASS

FUNCTION THTMLHelp(hwndCaller AS PTR, pszFile AS PSZ, uCommand AS DWORD, dwData AS DWORD) AS LONGINT STRICT
	//SE-060519 Update S.Ebert
	//SYSTEM
	RETURN 0



#region defines
DEFINE HDCANTOPENFILE := 1
DEFINE HDINVALIDKEY := 3
DEFINE HDOK := 0
DEFINE HDOUTOFMEMORY := 2
DEFINE HDUNKNOWN := 4
#endregion
