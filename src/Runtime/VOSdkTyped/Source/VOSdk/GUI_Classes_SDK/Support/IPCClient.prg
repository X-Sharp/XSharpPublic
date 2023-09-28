//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



#ifdef __VULCAN__
   USING System.Runtime.InteropServices
#endif

/// <include file="Gui.xml" path="doc/IpcClient/*" />
CLASS IpcClient INHERIT EventContext
	PROTECT dwIdInst AS DWORD
	PROTECT hServHstr AS PTR
	PROTECT hConv AS PTR
	PROTECT oIpcDataUpdateEvent AS IpcDataUpdateEvent
	PROTECT oIpcClientErrorEvent AS IpcClientErrorEvent

	#ifdef __VULCAN__
	   //HIDDEN cbDelegate AS DDECallbackDelegate
	#endif
	//PP-030828 Strong typing
 /// <exclude />
	METHOD __UpdateClient(hData AS PTR, hTopicHstr AS PTR, hItemHstr AS PTR) AS VOID STRICT
	//PP-030828 Strong typing


	oIpcDataUpdateEvent := IpcDataUpdateEvent{NULL_Ptr,0,NULL_Ptr, hTopicHstr, hItemHstr, hData, 0, 0, SELF}
	SELF:DataUpdate(oIpcDataUpdateEvent)
	RETURN


/// <include file="Gui.xml" path="doc/IpcClient.ChangeData/*" />
METHOD ChangeData(cTopic, cItem, cStringData)
	/*
	LOCAL hItemHstr AS PTR
	LOCAL dwLen AS DWORD
	LOCAL pszTemp AS PSZ
	LOCAL hConvTemp AS PTR
	LOCAL hTopicHstr AS PTR


	IF !IsString(cItem)
		WCError{#ChangData,#IpcClient,__WCSTypeError,cItem,2}:Throw()
	ENDIF
	IF !IsString(cStringData)
		WCError{#ChangData,#IpcClient,__WCSTypeError,cStringData,3}:Throw()
	ENDIF

	IF !Empty(cTopic)
		hTopicHstr := DdeCreateStringHandle(dwIdInst, String2Psz( cTopic), CP_WINANSI)
		hConvTemp := DdeConnect(dwIdInst, hServHstr, hTopicHstr, NULL_PTR)
	ELSE
		hConvTemp := hConv
	ENDIF

	pszTemp := StringAlloc(cStringData) // Copying the string to static memory
	dwLen := Len(cStringData) +1
	hItemHstr := DdeCreateStringHandle(dwIdInst, String2Psz(cItem), CP_WINANSI)
	DdeClientTransaction(pszTemp, dwLen, hConvTemp, hItemHstr, CF_TEXT,;
		XTYP_POKE, MAX_WAIT, NULL_PTR)
	MemFree(pszTemp)
	IF !Empty(cTopic)
		DdeDisconnect(hConvTemp)
	ENDIF
	*/
	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcClient.ClientError/*" />
METHOD ClientError(oIpcClientErrorEvent as IPCClientErrorEvent)
	LOCAL selected AS DWORD
	LOCAL __WCSIpcError AS DWORD


	selected := oIpcClientErrorEvent:ErrorType
	DO CASE
	CASE (selected == IPCSERVERNOTFOUND)
		__WCSIpcError := __WCSIpcServerNotFound

	CASE (selected == IPCOutOfMemory)
		__WCSIpcError := __WCSIpcOutOfMemory

	CASE (selected == IPCTopicNotFound)
		__WCSIpcError := __WCSIpcTopicNotFound

	CASE (selected == IPCItemNotFound)
		__WCSIpcError := __WCSIpcItemNotFound

	ENDCASE
	WCError{#ClientError,#IpcClient,__WCSIpcError}:Throw()
	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcClient.DataUpdate/*" />
METHOD DataUpdate(oIpcDataUpdateEvent)

	RETURN SELF


/// <include file="Gui.xml" path="doc/IpcClient.Destroy/*" />
METHOD Destroy() AS USUAL clipper
	/*
	LOCAL i, dwLen AS DWORD
	LOCAL hConv AS PTR


	DdeFreeStringHandle(dwIdInst, hServHstr)
	DdeUninitialize(dwIdInst)
	dwLen := ALen(aDdeConv)
	FOR i := 1 UPTO dwLen
		hConv := __WCGetHConvFromConv(SELF)
		IF hConv !=0
			DdeDisconnect(hConv)
			__WCDelIpcObjectFromConv(hConv)
		ENDIF
	NEXT

	IF !InCollect()
		dwIdInst := 0
		hServHstr := 0
		oIpcDataUpdateEvent := NULL_Object
		oIpcClientErrorEvent := NULL_Object
		UnRegisterAxit(SELF)
	ENDIF
	*/
	SUPER:Destroy()

	RETURN SELF
/// <inheritdoc />
	METHOD Dispatch(oEvent)
		/*
	LOCAL oE AS IpcEvent
	LOCAL dwType AS DWORD
	LOCAL hConv AS PTR



	oE := oEvent //faster execution
	dwType := oE:dwType
	hConv := oE:hConv

	DO CASE
	CASE dwType == XTYP_DISConnect
		__WCDelIpcObjectFromConv(hConv)
		RETURN 0L
	CASE dwType == XTYP_ADVDATA
		oIpcDataUpdateEvent := IpcDataUpdateEvent{dwType, oE:dwFmt, hConv, oE:hsz1, oE:hsz2,;
			oE:hData, oE:dwData1, oE:dwData2,SELF}
		SELF:DataUpdate(oIpcDataUpdateEvent)
		RETURN 1L
	ENDCASE
	*/
	RETURN 0L

/// <include file="Gui.xml" path="doc/IpcClient.Execute/*" />
	METHOD Execute(cTopic, cItem,cCommand)
		/*
	LOCAL dwLen AS DWORD
	LOCAL pszTemp AS PSZ
	LOCAL hTopicHstr AS PTR
	LOCAL hConvTemp AS PTR



	IF !IsString(cTopic)
		WCError{#ChangData,#IpcClient,__WCSTypeError,cTopic,1}:Throw()
	ENDIF
	IF !IsString(cItem)
		WCError{#ChangData,#IpcClient,__WCSTypeError,cItem,2}:Throw()
	ENDIF
	IF !IsString(cCommand)
		WCError{#ChangData,#IpcClient,__WCSTypeError,cCommand,3}:Throw()
	ENDIF

	IF !Empty(cTopic)
		hTopicHstr := DdeCreateStringHandle(dwIdInst, String2Psz(cTopic), CP_WINANSI)
		hConvTemp := DdeConnect(dwIdInst, hServHstr, hTopicHstr, NULL_PTR)
	ELSE
		hConvTemp := hConv
	ENDIF

	pszTemp := StringAlloc(cCommand) // Copying the string to static memory
	dwLen := Len(cCommand) +1
	DdeClientTransaction(pszTemp, dwLen, hConvTemp, 0, CF_TEXT, XTYP_EXECUTE,;
		MAX_WAIT, NULL_PTR)

	MemFree(pszTemp)
	IF !Empty(cTopic)
		DdeDisconnect(hConvTemp)
	ENDIF
	*/
	RETURN SELF

/// <include file="Gui.xml" path="doc/IpcClient.IdInst/*" />
ACCESS IdInst

	RETURN dwIdInst

/// <include file="Gui.xml" path="doc/IpcClient.ctor/*" />
CONSTRUCTOR(cServerName)


	SUPER()
	IF !IsString(cServerName)
		WCError{#Init, #IpcClient,__WCSTypeError,cServerName,1}:Throw()
	ENDIF
/*
#ifdef __VULCAN__
   cbDelegate := DDECallbackDelegate{ NULL, @__DdecallbackProc() }
	DdeInitialize(ptr(_cast, @dwIdInst), Marshal.GetFunctionPointerForDelegate((System.Delegate) cbDelegate ), 0, 0)
#else
	DdeInitialize(PTR(_CAST, @dwIdInst), @__DdecallbackProc(), 0, 0)
#endif
	hServHstr := DdeCreateStringHandle(dwIdInst, String2Psz(cServerName), CP_WINANSI)
	*/
	RETURN

/// <include file="Gui.xml" path="doc/IpcClient.RequestData/*" />
METHOD RequestData(oIpcTopic, continuous)
	/*
	LOCAL iItemListLen AS INT
	LOCAL liLoop AS LONGINT
	LOCAL hTopicHstr AS PTR
	LOCAL hItemHstr AS PTR
	LOCAL cTopicName AS STRING
	LOCAL cItemName AS STRING
	LOCAL hData AS PTR
	LOCAL dwErrorCode AS DWORD



	cTopicName := oIpcTopic:cTopicName
	hTopicHstr := DdeCreateStringHandle(dwIdInst, String2Psz(cTopicName), CP_WINANSI)
	hConv := DdeConnect(dwIdInst, hServHstr, hTopicHstr,NULL_PTR)
	IF (hConv != NULL_PTR)
		__WCAddIpcObjectToConv(hConv, SELF) // add to Client-Server table
		iItemListLen := INT(_CAST, ALen(oIpcTopic:aItemList))
		FOR liLoop:= 1 TO iItemListLen
			cItemName := oIpcTopic:aItemList[liLoop]
			hItemHstr := DdeCreateStringHandle(dwIdInst, String2Psz(cItemName), CP_WINANSI)
			hData := DdeClientTransaction(NULL_PTR,0,hConv,hItemHstr, CF_TEXT, XTYP_REQUEST, MAX_WAIT, NULL_PTR)
			IF hData != 0 //no err
				SELF:__UpdateClient(hData, hTopicHstr, hItemHstr) // call requstor's data update event handler
				DdeFreeDataHandle(hData)
			ELSE
				dwErrorCode := DdeGetLastError(dwIdInst)
				oIpcClientErrorEvent := IpcClientErrorEvent{IPCITEMNOTFOUND}// Item not found
				SELF:ClientError(oIpcClientErrorEvent)
			ENDIF

			IF continuous
				hData := DdeClientTransaction(NULL_PTR, 0,hConv, hItemHstr, CF_TEXT, DWORD(_CAST,_or(XTYP_ADVSTART, XTYPF_ACKREQ)), MAX_WAIT, NULL_PTR)
				IF (hData = NULL_PTR)
					dwErrorCode := DdeGetLastError(dwIdInst)
					oIpcClientErrorEvent := IpcClientErrorEvent{IPCITEMNOTFOUND} // Item not found
					SELF:ClientError(oIpcClientErrorEvent)
				ENDIF // end hData
			ENDIF // end continuous
		NEXT
	ELSE // server not found
		dwErrorCode := DdeGetLastError(dwIdInst)
		oIpcClientErrorEvent := IpcClientErrorEvent{IPCSERVERNOTFOUND}
		SELF:ClientError(oIpcClientErrorEvent)
	ENDIF
	*/
	RETURN SELF

END CLASS
