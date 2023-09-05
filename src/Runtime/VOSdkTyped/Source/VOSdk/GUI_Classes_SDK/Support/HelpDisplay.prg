//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/HelpDisplay/*" />


CLASS HelpDisplay INHERIT VObject
	PROTECT cFileName AS STRING
	PROTECT wError AS LONGINT				        //RvdH 070205 changed from WORD to LONG
	PROTECT oTopApp AS TopAppWindow
	PROTECT oWnd AS VOForm
	PROTECT lHTMLHelp AS LOGIC
	PROTECT cPopUpTopic AS STRING //SE-060519

	//PP-030929 Show default page when HelpContents?
	EXPORT DefaultPageOnContents := TRUE AS LOGIC

	//SE-060521
	EXPORT Win32Processing := FALSE AS LOGIC

	//PP-030828 Strong typing
 /// <exclude />
	METHOD __CallHelp(uCommand AS DWORD, dwData AS DWORD, cTopic := "" AS STRING) AS LOGIC STRICT
		//SE-060519
		LOCAL cFName AS STRING
		LOCAL hWnd AS IntPtr
		IF oWnd != NULL_OBJECT
			hWnd := oWnd:Handle
		ENDIF
		IF lHTMLHelp
			cFName := cFileName
			//RvdH 060608 optimized
			//IF ! Empty(cTopic)
			IF SLen(cTopic) > 0
				cFName += "::/"+cTopic
			ENDIF
			RETURN GuiWin32.HtmlHelp( hWnd, cFName, uCommand, dwData) == S_OK
		ELSE
			RETURN GuiWin32.WinHelp(hWnd, cFileName, uCommand, dwData)
		ENDIF


 /// <exclude />
	METHOD __VerifyHelp() AS LOGIC STRICT
		//PP-030828 Strong typing


		IF (oWnd == NULL_OBJECT) .AND. !lHTMLHelp // Create dummy window for use with help system
			IF (oApp != NULL_OBJECT)
				oWnd:= oApp:__HelpWndHandle
			ENDIF
			IF (oWnd == NULL_OBJECT)
				wError := HdUnknown
				RETURN FALSE
			ENDIF
		ENDIF

		RETURN TRUE

/// <include file="Gui.xml" path="doc/HelpDisplay.Destroy/*" />
	METHOD Destroy() AS USUAL clipper


		IF (oWnd != NULL_OBJECT)
			IF !lHTMLHelp
				GuiWin32.WinHelp(oWnd:Handle, cFileName, HELP_QUIT, 0)
			ELSE
				GuiWin32.HtmlHelp(IntPtr.Zero, NULL_STRING, HH_CLOSE_ALL, 0)
			ENDIF
		ENDIF

		oWnd := NULL_OBJECT
		cFileName := NULL_STRING
		IF (oTopApp != NULL_OBJECT)
			oTopApp:Destroy()
			oTopApp := NULL_OBJECT
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/HelpDisplay.EnableHTMLHelp/*" />
	METHOD EnableHTMLHelp(lEnable := TRUE AS LOGIC, cPopUpTopic AS USUAL) AS LOGIC

		lHTMLHelp := FALSE

		IF lEnable

			lHTMLHelp := TRUE

			IF IsString(cPopUpTopic)
				SELF:cPopUpTopic := cPopUpTopic
			ELSE
				SELF:cPopUpTopic := "PopUps.Txt"
			ENDIF
		ENDIF

		RETURN TRUE

/// <include file="Gui.xml" path="doc/HelpDisplay.HelpError/*" />
	METHOD HelpError() AS LONG STRICT
		RETURN wError

/// <include file="Gui.xml" path="doc/HelpDisplay.ctor/*" />
	CONSTRUCTOR(cFileName, oOwnerWindow, lWin32Processing)
		//SE-060519


		IF !IsString(cFileName)
			WCError{#Init,#HelpDisplay,__WCSTypeError,cFileName,1}:Throw()
		ENDIF

		IF !IsNil(oOwnerWindow)
			IF !(oOwnerWindow IS Window)
				WCError{#Init,#HelpDisplay,__WCSTypeError,oOwnerWindow,2}:Throw()
			ELSE
				SELF:oWnd := oOwnerWindow
			ENDIF
		ENDIF

		SUPER()


		SELF:cFileName := cFileName

		IF IsLogic(lWin32Processing)
			SELF:Win32Processing := lWin32Processing
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/HelpDisplay.Show/*" />
	METHOD Show(cKeyword, symLookupType)
		//SE-060519
		LOCAL cKey AS STRING
		LOCAL wLen AS DWORD
		LOCAL strucMKH AS _winMULTIKEYHELP
		LOCAL wMK AS DWORD
		LOCAL cFirst AS STRING
		LOCAL lRetVal AS LOGIC
		LOCAL hhLink IS _winHH_AKLINK
		LOCAL dwCommand AS DWORD
		LOCAL DIM dwHH[2,2] AS DWORD
		LOCAL sHelpInfo AS _winHELPINFO
		LOCAL pszKey	AS PSZ


		DEFAULT (REF symLookupType,#KEYWORD)

		IF !IsString(cKeyword)
			WCError{#Show,#HelpDisplay,__WCSTypeError,cKeyword,1}:Throw()
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
		/*	IF IsPtr(symLookupType)
				sHelpInfo := symLookupType
				IF sHelpInfo != NULL_PTR
					oWnd := sHelpInfo:hItemHandle
				ENDIF
			ENDIF*/
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
			strucMKH:mkKeylist := (BYTE) Asc(SubStr3(cKey,2,1))
			pszKey := StringAlloc(SubStr3(cKey,3, wLen-3))
			MemCopy(@strucMKH:szKeyphrase,pszKey,wLen-3)
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

					hhLink:cbStruct     := (LONG) _SIZEOF(_winHH_AKLINK)
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



#region defines
DEFINE HDCANTOPENFILE := 1
DEFINE HDINVALIDKEY := 3
DEFINE HDOK := 0
DEFINE HDOUTOFMEMORY := 2
DEFINE HDUNKNOWN := 4
#endregion
