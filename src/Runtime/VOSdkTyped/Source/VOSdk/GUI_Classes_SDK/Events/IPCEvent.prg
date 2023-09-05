//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


CLASS IpcClientErrorEvent INHERIT @@Event

ACCESS ErrorType 
	

	RETURN wParam

CONSTRUCTOR(nErrorType) 
	

	SUPER(NULL_PTR,0,nErrorType,0, NULL_OBJECT)

	RETURN 
END CLASS

CLASS IpcDataRequestEvent inherit IpcEvent

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	
	SUPER(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)


RETURN 
END CLASS

CLASS IpcDataUpdateEvent inherit IpcEvent
	protect cData as string

access AsString 
	

	return cData

method GetData() 
	
	return cData

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	//local dwLen as dword

	super(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)

	//cData := Psz2String(DdeAccessData(hdata,@dwLen))
	//DdeUnaccessData(hdata)

	return 

END CLASS

CLASS IpcEvent INHERIT VObject
	EXPORT dwType AS DWORD
	EXPORT dwFmt AS DWORD
	EXPORT hConv AS IntPtr
	EXPORT hsz1 AS IntPtr
	EXPORT hsz2 AS IntPtr
	EXPORT hData AS IntPtr
	EXPORT dwData1 AS DWORD
	EXPORT dwData2 AS DWORD
	EXPORT oIPCObject AS OBJECT

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	local oIpcEvent as IpcEvent

	super()

	if !IsObject(wT)
		if IsPtr(wT)
			dwType := 0
		else
			dwType :=wT
		endif

		dwFmt := wF
		hConv :=hC
		hsz1 :=h1
		hsz2 :=h2
		hData :=hD
		dwData1 := dwD1
		dwData2 := dwD2

		if !IsNil(oIpc)
			oIpcObject := oIpc
		else
			do case

			case dwType == dword(_cast, XTYP_CONNECT_CONFIRM)
				oIPCObject:=__WCGetServerFromHsz(hsz2)

			case dwType==XTYP_CONNECT
				oIPCObject:=__WCGetServerFromHsz(hsz2)
			case dwType==XTYP_WILDCONNECT
				if (hsz2 != NULL)
					oIPCObject:=__WCGetServerFromHsz(hsz2)
				endif
			case dwType==XTYP_ADVREQ .or.;
				dwType==XTYP_REQUEST .or.;
				dwType==XTYP_ADVSTART .or.;
				dwType==XTYP_EXECUTE .or.;
				dwType==XTYP_POKE .or.;
				dwType==XTYP_ADVSTOP
				oIPCObject:= __WCGetIpcObjectFromConv(hConv)
			case dwType==XTYP_ADVDATA .or.;
				dwType==XTYP_XACT_COMPLETE .or.;
				dwType==XTYP_DISCONNECT
				oIPCObject:=__WCGetIpcObjectFromConv(hConv)
			endcase
		endif
	else
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
	//LOCAL dwLen AS DWORD
	//LOCAL pszBuf := NULL AS PSZ

	

	//IF (oIPCObject != NULL_OBJECT)
	//	dwLen := DdeQueryString(oIPCObject:idInst, hsz2, NULL_PSZ, 0, CP_WINANSI) +1
	//	IF (PTR(_CAST, pszBuf) != NULL_PTR)
	//		MemFree(pszBuf)
	//	ENDIF
	//	pszBuf := MemAlloc(dwLen)
	//	DdeQueryString(oIPCObject:idInst, hsz2, pszBuf, dwlen, CP_WINANSI)
	//	cBuf := Psz2String(pszBuf)
	//	MemFree(pszBuf)
	//ENDIF

	RETURN cBuf

ACCESS Topic 
	LOCAL cBuf AS STRING
	//LOCAL dwLen AS DWORD
	//LOCAL pszBuf := NULL AS PSZ

	

	//IF (oIPCObject != NULL_OBJECT)
	//	dwLen := DdeQueryString(oIPCObject:idInst, hsz1, NULL_PSZ, 0, CP_WINANSI) +1
	//	IF (PTR(_CAST, pszBuf) != NULL_PTR)
	//		MemFree(pszBuf)
	//	ENDIF
	//	pszBuf := MemAlloc(dwLen)
	//	DdeQueryString(oIPCObject:IdInst, hsz1, pszBuf, dwlen, CP_WINANSI)
	//	cBuf := Psz2String(pszBuf)
	//	MemFree(pszBuf)
	//ENDIF
	RETURN cBuf

END CLASS

CLASS IpcExecuteRequestEvent INHERIT IpcEvent
	PROTECT cCommand AS STRING

ACCESS Command 
	

	RETURN cCommand

CONSTRUCTOR(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc) 
	//LOCAL dwLen AS DWORD

	

	SUPER(wT, wF, hC, h1, h2, hD, dwD1, dwD2, oIpc)
	//cCommand := Psz2String(DdeAccessData(hdata, @dwLen))
	//DdeUnaccessData(hdata)

	RETURN 

END CLASS

GLOBAL aDdeConv:={} AS ARRAY //Table of Conversation handles and IPC objects (Clients or servers)

GLOBAL aDdeServer:={} AS ARRAY //Table of Servers objects and Server handles

FUNCTION __WCAddIpcObjectToConv(hConv AS IntPtr, oIpc AS OBJECT) AS VOID
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI][ 1] == hConv
		//aDdeServer[dwI, 2] := oIpc //SE-060526 IMHO this is wrong
		aDdeConv[dwI][ 2] := oIpc
		RETURN
	   ENDIF
	NEXT  // dwI

	AAdd(aDdeConv, {hConv, oIpc})

	RETURN
 FUNCTION __WCAddServerToHsz(dwHsz AS IntPtr, oServer AS IpcServer) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI][ 1] == dwHsz
		aDdeServer[dwI][ 2] := oServer
		RETURN
	   ENDIF
	NEXT  // dwI

	AAdd(aDdeServer, {dwHsz, oServer})

	RETURN

FUNCTION __WCDelIpcObjectFromConv(hConv AS IntPtr) AS VOID
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI][ 1] == hConv
		ADel(aDdeConv, dwI)
		  ASize(aDdeConv, dwCount-1)
		RETURN
	   ENDIF
	NEXT  // dwI

	RETURN

FUNCTION __WCDelServerFromHsz(dwHsz AS IntPtr) AS VOID
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI][ 1] == dwHsz
		ADel(aDdeServer, dwI)
		  ASize(aDdeServer, dwCount-1)
		RETURN
	   ENDIF
	NEXT  // dwI

	RETURN

FUNCTION __WCGetHConvFromConv(oIpc AS OBJECT) AS IntPtr
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI][ 2] == oIpc
		RETURN PTR(_CAST, aDdeConv[dwI][ 1])
	   ENDIF
	NEXT  // dwI

	RETURN Null_Ptr


FUNCTION __WCGetHszFromHsz(oIpc AS OBJECT) AS IntPtr
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI][ 2] == oIpc
		RETURN PTR(_CAST, aDdeServer[dwI][ 1])
	   ENDIF
	NEXT  // dwI

	RETURN Null_Ptr

FUNCTION __WCGetIpcObjectFromConv(hConv AS IntPtr) AS OBJECT
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeConv)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeConv[dwI][ 1] == hConv
		RETURN aDdeConv[dwI][ 2]
	   ENDIF
	NEXT  // dwI

	RETURN Null_OBJECT

FUNCTION __WCGetServerFromHsz(dwHsz AS IntPtr) AS OBJECT
   //SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aDdeServer)
	FOR dwI := 1 UPTO dwCount
	   IF aDdeServer[dwI][ 1] == dwHsz
		RETURN aDdeServer[dwI][ 2]
	   ENDIF
	NEXT  // dwI

	RETURN Null_OBJECT

