CLASS IpcClientErrorEvent INHERIT @@Event

ACCESS ErrorType 
	

	RETURN wParam

CONSTRUCTOR(nErrorType) 
	

	SUPER(NULL_PTR,0,nErrorType,0, NULL_OBJECT)

	RETURN 
END CLASS

CLASS IpcDataRequestEvent INHERIT IpcEvent

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
    
    SUPER(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)


RETURN 
END CLASS

CLASS IpcDataUpdateEvent INHERIT IpcEvent
	PROTECT cData AS STRING

ACCESS AsString 
	

	RETURN cData

METHOD GetData() 
	
	RETURN cData

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	LOCAL dwLen AS DWORD

	SUPER(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)

	cData := Psz2String(DdeAccessData(hdata,@dwLen))
	DdeUnaccessData(hdata)

	RETURN 

END CLASS

CLASS IpcEvent INHERIT VObject
	EXPORT dwType AS DWORD
	EXPORT dwFmt AS DWORD
	EXPORT hConv AS PTR
	EXPORT hsz1 AS PTR
	EXPORT hsz2 AS PTR
	EXPORT hData AS PTR
	EXPORT dwData1 AS DWORD
	EXPORT dwData2 AS DWORD
	EXPORT oIPCObject AS OBJECT

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	LOCAL oIpcEvent AS IpcEvent

	SUPER()

	IF !IsObject(wT)
		IF IsPtr(wT)
			dwType := 0
		ELSE
			dwType :=wT
		ENDIF

		dwFmt := wF
		hConv :=hC
		hsz1 :=h1
		hsz2 :=h2
		hData :=hD
		dwData1 := dwD1
		dwData2 := dwD2

		IF !IsNil(oIpc)
			oIpcObject := oIpc
		ELSE
			DO CASE

			CASE dwType == DWORD(_CAST, XTYP_CONNECT_CONFIRM)
				oIPCObject:=__WCGetServerFromHsz(hsz2)

			CASE dwType==XTYP_CONNECT
				oIPCObject:=__WCGetServerFromHsz(hsz2)
			CASE dwType==XTYP_WILDCONNECT
				IF (hsz2 != 0)
					oIPCObject:=__WCGetServerFromHsz(hsz2)
				ENDIF
			CASE dwType==XTYP_ADVREQ .OR.;
				dwType==XTYP_REQUEST .OR.;
				dwType==XTYP_ADVSTART .OR.;
				dwType==XTYP_EXECUTE .OR.;
				dwType==XTYP_POKE .OR.;
				dwType==XTYP_ADVSTOP
				oIPCObject:= __WCGetIpcObjectFromConv(hConv)
			CASE dwType==XTYP_ADVDATA .OR.;
				dwType==XTYP_XACT_COMPLETE .OR.;
				dwType==XTYP_DISCONNECT
				oIPCObject:=__WCGetIpcObjectFromConv(hConv)
			ENDCASE
		ENDIF
	ELSE
		oIpcEvent:=wT
		dwType:=oIpcEvent:dwType
		dwFmt:=oIpcEvent:dwFmt
		hConv:=oIpcEvent:hConv
		hsz1:=oIpcEvent:hsz1
		hsz2:=oIpcEvent:hsz2
		hData:=oIpcEvent:hData
		dwData1:=oIpcEvent:dwData1
		dwData2:=oIpcEvent:dwData2
		oIPCObject:=oIpcEvent:oIPCObject
	ENDIF

	RETURN 

ACCESS Item 
	LOCAL cBuf AS STRING
	LOCAL dwLen AS DWORD
	LOCAL pszBuf := NULL AS PSZ

	

	IF (oIPCObject != NULL_OBJECT)
		dwLen := DdeQueryString(oIPCObject:idInst, hsz2, NULL_PSZ, 0, CP_WINANSI) +1
		IF (PTR(_CAST, pszBuf) != NULL_PTR)
			MemFree(pszBuf)
		ENDIF
		pszBuf := MemAlloc(dwLen)
		DdeQueryString(oIPCObject:idInst, hsz2, pszBuf, dwlen, CP_WINANSI)
		cBuf := Psz2String(pszBuf)
		MemFree(pszBuf)
	ENDIF

	RETURN cBuf

ACCESS Topic 
	LOCAL cBuf AS STRING
	LOCAL dwLen AS DWORD
	LOCAL pszBuf := NULL AS PSZ

	

	IF (oIPCObject != NULL_OBJECT)
		dwLen := DdeQueryString(oIPCObject:idInst, hsz1, NULL_PSZ, 0, CP_WINANSI) +1
		IF (PTR(_CAST, pszBuf) != NULL_PTR)
			MemFree(pszBuf)
		ENDIF
		pszBuf := MemAlloc(dwLen)
		DdeQueryString(oIPCObject:IdInst, hsz1, pszBuf, dwlen, CP_WINANSI)
		cBuf := Psz2String(pszBuf)
		MemFree(pszBuf)
	ENDIF
	RETURN cBuf

END CLASS

CLASS IpcExecuteRequestEvent INHERIT IpcEvent
	PROTECT cCommand AS STRING

ACCESS Command 
	

	RETURN cCommand

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	LOCAL dwLen AS DWORD

	

	SUPER(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)
	cCommand := Psz2String(DdeAccessData(hdata, @dwLen))
	DdeUnaccessData(hdata)

	RETURN 

END CLASS

GLOBAL aDdeConv:={} AS ARRAY //Table of Conversation handles and IPC objects (Clients or servers)

GLOBAL aDdeServer:={} AS ARRAY //Table of Servers objects and Server handles

FUNCTION __WCAddIpcObjectToConv(hConv AS PTR, oIpc AS OBJECT) AS VOID
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI, 1] == hConv
	   	//aDdeServer[dwI, 2] := oIpc //SE-060526 IMHO this is wrong
	   	aDdeConv[dwI, 2] := oIpc
	   	RETURN
	   ENDIF
	NEXT  // dwI

	AAdd(aDdeConv, {hConv, oIpc})

	RETURN
 FUNCTION __WCAddServerToHsz(dwHsz AS PTR, oServer AS IpcServer) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI, 1] == dwHsz
	   	aDdeServer[dwI, 2] := oServer
	   	RETURN
	   ENDIF
	NEXT  // dwI

	AAdd(aDdeServer, {dwHsz, oServer})

	RETURN

FUNCTION __WCDelIpcObjectFromConv(hConv AS PTR) AS VOID
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI, 1] == hConv
	   	ADel(aDdeConv, dwI)
	      ASize(aDdeConv, dwCount-1)
	   	RETURN
	   ENDIF
	NEXT  // dwI

	RETURN

FUNCTION __WCDelServerFromHsz(dwHsz AS PTR) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI, 1] == dwHsz
	   	ADel(aDdeServer, dwI)
	      ASize(aDdeServer, dwCount-1)
	   	RETURN
	   ENDIF
	NEXT  // dwI

	RETURN

FUNCTION __WCGetHConvFromConv(oIpc AS OBJECT) AS PTR
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI, 2] == oIpc
	   	RETURN PTR(_CAST, aDdeConv[dwI, 1])
	   ENDIF
	NEXT  // dwI

	RETURN NULL_PTR


FUNCTION __WCGetHszFromHsz(oIpc AS OBJECT) AS PTR
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI, 2] == oIpc
	   	RETURN PTR(_CAST, aDdeServer[dwI, 1])
	   ENDIF
	NEXT  // dwI

	RETURN NULL_PTR

FUNCTION __WCGetIpcObjectFromConv(hConv AS PTR) AS OBJECT
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI, 1] == hConv
	   	RETURN aDdeConv[dwI, 2]
	   ENDIF
	NEXT  // dwI

	RETURN NULL_OBJECT

FUNCTION __WCGetServerFromHsz(dwHsz AS PTR) AS OBJECT
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI, 1] == dwHsz
	   	RETURN aDdeServer[dwI, 2]
	   ENDIF
	NEXT  // dwI

	RETURN NULL_OBJECT

