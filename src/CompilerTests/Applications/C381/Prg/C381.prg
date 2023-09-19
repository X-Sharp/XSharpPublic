#pragma warnings(165, off) // uniassigned local
#region Help defines
#define HH_DISPLAY_TOPIC 0x0000
#define HH_HELP_FINDER 0x0000
#define HH_DISPLAY_TOC 0x0001
#define HH_DISPLAY_INDEX 0x0002
#define HH_DISPLAY_SEARCH 0x0003
#define HH_SET_WIN_TYPE 0x0004
#define HH_GET_WIN_TYPE 0x0005
#define HH_GET_WIN_HANDLE 0x0006
#define HH_ENUM_INFO_TYPE 0x0007
#define HH_SET_INFO_TYPE 0x0008
#define HH_SYNC 0x0009
#define HH_RESERVED1 0x000A
#define HH_RESERVED2 0x000B
#define HH_RESERVED3 0x000C
#define HH_KEYWORD_LOOKUP 0x000D
#define HH_DISPLAY_TEXT_POPUP 0x000E
#define HH_HELP_CONTEXT 0x000F
#define HH_TP_HELP_CONTEXTMENU 0x0010
#define HH_TP_HELP_WM_HELP 0x0011
#define HH_CLOSE_ALL 0x0012
#define HH_ALINK_LOOKUP 0x0013
#define HH_GET_LAST_ERROR 0x0014
#define HH_ENUM_CATEGORY 0x0015
#define HH_ENUM_CATEGORY_IT 0x0016
#define HH_RESET_IT_FILTER 0x0017
#define HH_SET_INCLUSIVE_FILTER 0x0018
#define HH_SET_EXCLUSIVE_FILTER 0x0019
#define HH_INITIALIZE 0x001C
#define HH_UNINITIALIZE 0x001D
#define HH_PRETRANSLATEMESSAGE 0x00fd
#define HH_SET_GLOBAL_PROPERTY 0x00fc
#define HELP_CONTEXT 0x0001L
#define HELP_QUIT 0x0002L
#define HELP_INDEX 0x0003L
#define HELP_CONTENTS 0x0003L
#define HELP_HELPONHELP 0x0004L
#define HELP_SETINDEX 0x0005L
#define HELP_SETCONTENTS 0x0005L
#define HELP_CONTEXTPOPUP 0x0008L
#define HELP_FORCEFILE 0x0009L
#define HELP_KEY 0x0101L
#define HELP_COMMAND 0x0102L
#define HELP_PARTIALKEY 0x0105L
#define HELP_MULTIKEY 0x0201L
#define HELP_SETWINPOS 0x0203L
#define HELP_CONTEXTMENU 0x000a
#define HELP_FINDER 0x000b
#define HELP_WM_HELP 0x000c
#define HELP_SETPOPUP_POS 0x000d
#define HELP_TCARD 0x8000
#define HELP_TCARD_DATA 0x0010
#define HELP_TCARD_OTHER_CALLER 0x0011
#endregion
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
		#pragma warnings(9020, off) // narrowing
		strucMKH:mkKeyList := Asc(SubStr3(cKey,2,1))
    	#pragma warnings(9020, default) // narrowing
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

FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length
