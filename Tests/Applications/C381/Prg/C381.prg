	#include "VOGUIClasses.vh"
#include "VOSystemLibrary.vh"
#include "VOWin32APILibrary.vh"
PARTIAL CLASS HelpDisplay INHERIT VObject
	PROTECT cFileName AS STRING
	PROTECT wError AS LONGINT				        //RvdH 070205 changed from WORD to LONG
	PROTECT oTopApp AS OBJECT
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
    RETURN TRUE


CONSTRUCTOR(cFileName, oOwnerWindow, lWin32Processing) 
	//SE-060519
	


	IF !IsNil(oOwnerWindow)
		IF IsInstanceOfUsual(oOwnerWindow, #Window)
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
		RETURN FALSE
	ENDIF

	cKey := cKeyword

	IF (cKey == NULL_STRING)
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
		ENDIF
		MemFree(pszKey)
	ENDCASE



	RETURN lRetVal

END CLASS


