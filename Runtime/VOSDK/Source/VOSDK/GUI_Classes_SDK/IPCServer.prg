#ifdef __VULCAN__

   #using System.Runtime.InteropServices
   INTERNAL DELEGATE DDECallbackDelegate( wType AS WORD, wFmt AS WORD, hConv AS PTR, hsz1 AS PTR, hsz2 AS PTR, hData AS PTR, dwData1 AS DWORD, dwData2 AS DWORD ) AS LONGINT

#endif

PARTIAL CLASS IpcServer INHERIT @@EventContext
	PROTECT dwIdInst AS DWORD
	PROTECT hInst AS PTR
	PROTECT hStr AS PTR
	PROTECT aTopicList AS ARRAY
	PROTECT oIpcDataRequestEvent AS IpcDataRequestEvent
	PROTECT oIpcTopicData AS IpcTopicData
	PROTECT oIpcExecuteRequestEvent AS IpcExecuteRequestEvent
	PROTECT oIpcDataUpdateEvent AS IpcDataUpdateEvent
	
	#ifdef __VULCAN__
	   HIDDEN cbDelegate AS DDECallbackDelegate
	#endif

	//PP-030828 Strong typing
	METHOD __FindTopic(hsz1 AS PTR) AS LOGIC STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL dwI AS DWORD
	LOCAL hHsz AS PTR
	LOCAL pszString AS PSZ
	LOCAL dwLen AS DWORD
	LOCAL cTopic AS STRING

	

	hHsz  := DWORD (_CAST, hsz1)
	dwLen := DdeQueryString(dwidInst, hHsz, NULL_PSZ,20,0)

	pszString := MemAlloc(dwLen+1)
	IF (PTR(_CAST, pszString) != NULL_PTR)
		DdeQueryString(dwidInst, hHsz, pszString, dwLen+1, 0)
		cTopic := Upper(Psz2String(pszString))
		MemFree(pszString)
	ENDIF

	dwLen := ALen(aTopicList)
	FOR dwI := 1 UPTO dwLen
	   IF aTopicList[dwI, 1] == cTopic
	   	RETURN TRUE
	   ENDIF
	NEXT  // dwI

	RETURN FALSE

METHOD __WCAddTopicToList(cTopic AS STRING, oIpcTopic AS IPCTopic) AS IPCServer STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aTopicList)
	FOR dwI := 1 UPTO dwCount
	   IF aTopicList[dwI, 1] == cTopic
	   	aTopicList[dwI, 2] := oIpcTopic
	   	RETURN SELF
	   ENDIF
	NEXT  // dwI

	AAdd(aTopicList,{cTopic, oIpcTopic})

	RETURN SELF

METHOD __WCFindItem(hsz1 AS PTR, hsz2 AS PTR) AS LOGIC STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL hHsz1 AS PTR
	LOCAL hHsz2 AS PTR
	LOCAL pszTopic AS PSZ
	LOCAL pszItem AS PSZ
	LOCAL dwI, dwLen AS DWORD
	LOCAL cTempTopic AS STRING
	LOCAL cTempItem AS STRING
	LOCAL oIpcTopic AS IpcTopic
	LOCAL aItemList AS ARRAY

	hHsz1 := DWORD (_CAST,hsz1)
	dwLen := DdeQueryString(dwidInst, hHsz1, NULL_PSZ, 20, 0)

	pszTopic := MemAlloc(dwLen+1)
	IF (PTR(_CAST, pszTopic) != NULL_PTR)
		DdeQueryString(dwidInst, hHsz1, pszTopic, dwLen+1, 0)
		cTempTopic := Upper(Psz2String(pszTopic))
      MemFree(pszTopic)
   ENDIF

	oIpcTopic := SELF:__WCGetTopicFromList(cTempTopic)
	hHsz2 := DWORD(_CAST, hsz2)
	dwLen := DdeQueryString(dwidInst, hHsz2, NULL_PSZ, 20, 0)

	pszItem := MemAlloc(dwLen+1)
	IF (PTR(_CAST, pszItem) != NULL_PTR)
		DdeQueryString(dwidInst,hHsz2,pszItem, dwLen+1,0)
		cTempItem := Upper(Psz2String(pszItem))
		MemFree(pszItem)
	ENDIF

	aItemList := AClone(oIpcTopic:aItemList)
	dwLen := ALen(aItemList)
	FOR dwI := 1 UPTO dwLen
		IF aItemList[dwI] == cTempItem
			RETURN TRUE
		ENDIF
	NEXT  // dwI

	RETURN FALSE

METHOD __WCGetTopicFromList(cTopic AS STRING) AS OBJECT STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
	LOCAL cTempTopic   AS STRING


	cTempTopic := Upper(cTopic)
	dwCount := ALen(aTopicList)
	FOR dwI := 1 UPTO dwCount
	   IF aTopicList[dwI, 1] == cTempTopic
	   	RETURN aTopicList[dwI, 2]
	   ENDIF
	NEXT  // dwI

	RETURN NULL_OBJECT

METHOD AddTopic(oIpcTopic) 
	//PP-030828 Strong typing
	LOCAL cTopicString AS STRING

	

	IF !IsNil(oIpcTopic)
		IF !IsInstanceOfUsual(oIpcTopic, #IpcTopic)
			WCError{#AddItem,#IpcServer,__WCSTypeError,oIpcTopic,1}:@@Throw()
		ENDIF

		cTopicString :=oIpcTopic:cTopicName
		SELF:__WCAddTopicToList(cTopicString, oIpcTopic)
	ENDIF
	RETURN SELF

METHOD DataRequest(oIpcDataRequestEvent) 
	LOCAL oIpcTopicData AS IpcTopicData

	

	oIpcTopicData := IpcTopicData{"ok", 3}

	RETURN oIpcTopicData

METHOD DataUpdate(oIpcDataUpdateEvent) 
	
	RETURN SELF

METHOD Destroy() 
	LOCAL i, dwLen AS DWORD
	LOCAL hConv AS PTR
	LOCAL hHsz AS PTR

	

	DdeFreeStringHandle(dwIdInst, hStr)
	IF dwIdInst != 0
		DdeUninitialize(dwIdInst)
	ENDIF

	dwLen := ALen(aDdeConv)
	FOR i := 1 UPTO dwLen
		hConv := __WCGetHConvFromConv(SELF)
		IF hConv !=0
			DdeDisconnect(hConv)
			__WCDelIpcObjectFromConv(hConv)
		ENDIF

	NEXT

	dwLen := ALen(aDdeServer)
	IF dwLen >0
		FOR i := 1 UPTO dwLen
			hHsz := __WCGetHszFromHsz(SELF)
			IF hHsz !=0
				__WCDelServerFromHsz(hHsz)
			ENDIF

		NEXT
	ENDIF

	IF !InCollect()
		dwIdInst := 0
		hStr := 0
		aTopicList := {}
		oIpcDataRequestEvent := NULL_OBJECT
		oIpcTopicData := NULL_OBJECT
		oIpcExecuteRequestEvent := NULL_OBJECT
		oIpcDataUpdateEvent := NULL_OBJECT

		UnregisterAxit(SELF)
	ENDIF
	SUPER:Destroy()

	RETURN SELF

METHOD Dispatch(oEvent) 
	LOCAL oE AS IpcEvent
	LOCAL dwType AS DWORD
	LOCAL hConv AS PTR
	LOCAL dwFmt AS DWORD
	LOCAL hHsz1 AS PTR
	LOCAL hHsz2 AS PTR
	LOCAL hData AS PTR
	LOCAL dwData1 AS DWORD
	LOCAL dwData2 AS DWORD
	LOCAL ptrData AS PTR
	LOCAL dwLen AS DWORD

	

	oE:=oEvent //faster execution
	dwType :=oE:dwType
	dwFmt := oE:dwFmt
	hConv := oE:hConv
	hHsz1 := oE:hsz1
	hHsz2 := oE:hsz2
	hData := oE:hData
	dwData1 := oE:dwData1
	dwData2 := oE:dwData2

	DO CASE
	CASE dwType == XTYP_CONNECT
		IF SELF:__FindTopic(hHsz1)
			RETURN 1L
		ELSE
			RETURN	0L
		ENDIF

	CASE dwType == DWORD(_CAST, XTYP_CONNECT_CONFIRM)
		__WCAddIpcObjectToConv(hConv, SELF)
		RETURN 0L

	CASE dwType == DWORD(_CAST,XTYP_DISCONNECT)
		__WCDelIpcObjectFromConv(hConv)
		RETURN 0L

	CASE dwType == XTYP_ADVREQ .OR. dwType == XTYP_REQUEST .OR. dwType == XTYP_ADVSTART
		IF !SELF:__WCFindItem(hHsz1, hHsz2)
			RETURN 0L // no item found
		ELSE
			oIpcDataRequestEvent := IpcdataRequestEvent{oEvent}
			oIpcTopicData := SELF:DataRequest(oIpcDataRequestEvent)
			ptrData := oIpcTopicData:Data
			dwLen := oIpcTopicData:Length+1
			RETURN LONGINT(_CAST, DDeCreateDataHandle(dwIdInst, ptrData, dwLen, 0, hHsz2, CF_TEXT,0))
		ENDIF

	CASE dwType == xtyp_EXECUTE
		oIpcExecuteRequestEvent := IpcExecuteRequestEvent{dwType, dwFmt, hConv, hHsz1, hHsz2,;
			hData, dwdata1, dwData2, SELF}
		SELF:ExecuteRequest(oIpcExecuteRequestEvent)
		RETURN (LONGINT(_CAST, DDE_FACK))

	CASE dwType == XTYP_POKE
		oIpcDataUpdateEvent := IpcDataUpdateEvent{dwType, dwFmt, hConv, hHsz1, hHsz2,;
			hData, dwData1, dwData2,SELF}
		SELF:DataUpdate(oIpcDataUpdateEvent)
		RETURN (LONGINT(_CAST, DDE_FACK))
	OTHERWISE
		RETURN 0L
	ENDCASE


METHOD ExecuteRequest(oIpcExecuteRequestEvent) 
	
	RETURN SELF

ACCESS idInst 
	
	RETURN dwIdInst

CONSTRUCTOR(cServName) 
	LOCAL c AS STRING

	

	aTopicList := {}
	SUPER()
	IF !IsString(cServName)
		WCError{#Init,#IpcServer,__WCSTypeError,cServName,1}:@@Throw()
	ENDIF

#ifdef __VULCAN__
   cbDelegate := DDECallbackDelegate{ NULL, @__DdecallbackProc() }
	DdeInitialize(PTR(_CAST, @dwIdInst), Marshal.GetFunctionPointerForDelegate( (System.Delegate) cbDelegate ), 0, 0)
#else
	DdeInitialize(PTR(_CAST, @dwIdInst), @__DdecallbackProc(), 0, 0)
#endif
	c := cServName
	hStr := DdeCreateStringHandle(dwIdInst, String2Psz(c), CP_WINANSI)
	__WCAddServerToHsz(hStr, SELF )
	DdeNameService(dwIdInst, hStr, 0, DNS_REGISTER)

	RETURN 

METHOD UpdateTopic(cTopic, cItem) 
	LOCAL oIpcTopic AS IpcTopic
	LOCAL aItemLIst AS ARRAY
	LOCAL dwI, dwCount AS DWORD
	LOCAL cTopicName AS STRING
	LOCAL cItemName AS STRING
	LOCAL cTempItem AS STRING
	LOCAL hTopic AS PTR
	LOCAL hItem AS PTR

	

	IF !IsString(cTopic)
		WCError{#UpdateTopic,#IpcServer,__WCSTypeError,cTopic,1}:@@Throw()
	ENDIF

	IF !IsString(cItem)
		WCError{#UPdateTopic,#IpcServer,__WCSTypeError,cItem,2}:@@Throw()
	ENDIF

	cTempItem := Upper(cItem)
	oIpcTopic := SELF:__WCGetTopicFromList(cTopic)

	IF !IsNil(oIpcTopic)
		cTopicName := oIpcTopic:cTopicName
		hTopic := DdeCreateStringHandle(dwIdInst, String2Psz(cTopicName), CP_WINANSI)
		aItemList := AClone(oIpcTopic:aItemList)
		dwCount := ALen(aItemList)
		FOR dwI := 1 UPTO dwCount
		   IF aItemList[dwI] = cTempItem
		   	cItemName := aItemList[dwI]
		   	hItem := DdeCreateStringHandle(dwIdInst, String2Psz(cItemName), CP_WINANSI)
			   DdePostAdvise(dwIdInst, hTopic, hItem)
			   EXIT
		   ENDIF
		NEXT  // dwI
	ENDIF

	RETURN SELF

END CLASS

FUNCTION __DdecallbackProc(wType AS WORD, wFmt AS WORD, hConv AS PTR, hsz1 AS PTR, hsz2 AS PTR, hData AS PTR, dwData1 AS DWORD, dwData2 AS DWORD) AS LONGINT /* WINCALL */
	LOCAL oIpcEvent AS IpcEvent
	LOCAL oIPCObject AS OBJECT
	LOCAL liRetVal AS LONGINT


	oIpcEvent := IpcEvent{wType, wFmt, hConv, hsz1, hsz2, hData, dwData1, dwData2}
	oIpcObject := oIpcEvent:oIpcObject
	IF oIpcObject != NULL_OBJECT
		//u:= oIpcObject:Dispatch(oIpcEvent)
		liRetVal := Send(oIpcObject, #Dispatch, oIpcEvent)
		RETURN liRetVal
	ENDIF

	RETURN 0L

