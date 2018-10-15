PARTIAL CLASS CSocket
	// UH 10/06/1998
	// PROTECT nSocket AS INT
	PROTECT nSocket AS DWORD

	PROTECT nSocketType AS INT
	PROTECT nCurrentStatus AS INT
	PROTECT lCleanupRequired AS LOGIC
	PROTECT ServerAddress AS _WinSockAddr_IN
	//PROTECT pHostBuffer AS BYTE PTR
	PROTECT cErrorString AS STRING
	//PROTECT cAddress AS STRING
	//PROTECT cReceiveLine AS STRING
	//PROTECT cSendLine AS STRING
	//PROTECT pRawSendData AS BYTE PTR
	//PROTECT nRawSendDataLength AS INT

	PROTECT nTimeoutReTries AS INT
	PROTECT nTimeout AS INT
	PROTECT nLastError AS DWORD
	//PROTECT hWnd AS PTR

	// UH 05/29/2000
	PROTECT nRcvBuf  AS INT
	PROTECT nSndBuf  AS INT
	PROTECT pRcvBuf  AS PTR
	PROTECT cRcvBuf  AS STRING  //SE
	PROTECT pSndBuf  AS PTR
	#ifndef __VULCAN__
   ~"ONLYEARLY+"
   DECLARE METHOD __Start
   DECLARE METHOD __ReceiveBuf
   DECLARE METHOD __SendRaw
   DECLARE METHOD connect
   DECLARE METHOD Close
   DECLARE METHOD DisConnect
   DECLARE METHOD SendLine
   DECLARE METHOD SendLineTo
   DECLARE METHOD SendRaw
   DECLARE METHOD SendRawText
   DECLARE METHOD GetRaw
   DECLARE METHOD GetRawText
   //DECLARE METHOD GetMultilineResponse
   DECLARE METHOD GetLine
   DECLARE METHOD GetSockName
   DECLARE METHOD GetPeerName
   DECLARE METHOD Bind
   DECLARE METHOD Listen
   DECLARE METHOD accept
   DECLARE METHOD __Linger
   DECLARE METHOD GetLineFrom
   DECLARE METHOD GetLines
   DECLARE METHOD __SetErrorVars
   DECLARE METHOD DisplayError
   DECLARE METHOD __ConnectThread
   DECLARE METHOD __SetOptions
   DECLARE ACCESS Error
   DECLARE ASSIGN Error
   DECLARE ACCESS ErrorMsg
   DECLARE ACCESS TimeOut
   DECLARE ASSIGN TimeOut
   DECLARE ACCESS TimeOutRetries
   DECLARE ASSIGN TimeOutRetries
   DECLARE ACCESS Status
   DECLARE ACCESS RcvBufSize
   DECLARE ACCESS SndBufSize
   ~"ONLYEARLY-"
	#endif

METHOD __ConnectThread() AS LOGIC STRICT
	#ifndef  __VULCAN__
	LOCAL dwID AS DWORD
	#endif
	LOCAL cMsg AS STRING
#ifdef __VULCAN__
   LOCAL phT AS System.Threading.Thread
#else
	LOCAL phT AS PTR
#endif
	LOCAL nWait AS DWORD
	LOCAL condata IS _THREAD_DATA

	SELF:nLastError := 0

	condata:nRC := SOCKET_ERROR
	condata:nSocket := SELF:nSocket
	condata:nSize := _SIZEOF(_WinSockAddr_IN)
	MemCopy(@condata:ServerAddress:sin_family, @ServerAddress:sin_family, _SIZEOF(_WinSockAddr_IN))

	nWait := DWORD(SELF:nTimeout)
#ifdef __VULCAN__
   phT := System.Threading.Thread{ System.Threading.ParameterizedThreadStart{ NULL, @__ConnectFunc() } }
   phT:Start( (IntPtr) @condata )
   phT:Join( nWait )
#else
	phT := CreateVOThread(0, 0, @__ConnectFunc(), @condata, THREAD_TERMINATE, @dwID)
	WaitForSingleObject(phT, nWait)
#endif

	IF condata:nRC = SOCKET_ERROR
		SELF:Error := DWORD(_CAST,WSAGetLastError())
		IF SELF:Error = 0
			SELF:Error := WSA_WAIT_TIMEOUT
		ENDIF
		SELF:nCurrentStatus := SSTAT_DISCONNECTED
		cMsg := "Connection failed "
#ifndef __VULCAN__
		CloseHandle(phT)
#endif
		SELF:InternetStatus(0, SSTAT_DISCONNECTED, cMsg, SLen(cMsg))

		/*	original code wcm 2003-11-19
		SuspendThread(phT)

		// UH 07/24/2000
		// TerminateThread(phT, 0xFFFFFFFF)
		TerminateVOThread(phT, 0xFFFFFFFF)

		CloseHandle(phT)
		SELF:Error := ERR_WSA_WAIT_TIMEOUT
		SELF:nCurrentStatus := SSTAT_DISCONNECTED

		// UH 01/11/2000
		// cMsg := "Connection failed "
		// CloseHandle(phT)
		// SELF:InternetStatus(0, SSTAT_DISCONNECTED, cMsg, SLen(cMsg))
		*/
		RETURN .F.
	ELSE
		cMsg := "Connected to Server "
#ifndef __VULCAN__
		CloseHandle(phT)
#endif
		SELF:InternetStatus(0, INTERNET_STATUS_CONNECTED_TO_SERVER, cMsg, SLen(cMsg))
	ENDIF

	SELF:nCurrentStatus := SSTAT_CONNECTED

	RETURN TRUE

METHOD __Linger() AS VOID STRICT
	LOCAL nWhat AS INT
	LOCAL sL IS _WINLINGER

	sL:l_onoff := 1
	sL:l_linger := 30

	nWhat := setsockopt(SELF:nSocket, SOL_SOCKET, SO_LINGER, @sL, _SIZEOF(_WINLINGER))

	IF nWhat == SOCKET_ERROR
		SELF:__SetErrorVars(ProcName(), ProcLine())
	ENDIF

	RETURN


METHOD __ReceiveBuf() AS LOGIC STRICT
	LOCAL nLoops 	 AS INT
	LOCAL nNumChars AS INT
	LOCAL uRet      AS USUAL

   uRet := SELF:InternetStatus(0, INTERNET_STATUS_RECEIVING_RESPONSE, NULL_PTR, 0)

	IF IsLogic(uRet) .AND. ! uRet
	   cRcvBuf := NULL_STRING
	   SELF:nLastError := ERROR_INTERNET_OPERATION_CANCELLED
	   RETURN FALSE
	ENDIF

   nLoops := SELF:nTimeOutRetries

	SELF:nLastError := 0

	DO WHILE TRUE

		MemSet(SELF:pRcvBuf, 0, DWORD(SELF:nRcvBuf))

		nNumChars := recv(SELF:nSocket, SELF:pRcvBuf,SELF:nRcvBuf,	0)

    	#IFDEF __DEBUG__       // wcm 2003-11-19
	    	DebOut32("__ReceiveBuf() characters received: " + NTrim(nNumChars))
	   #ENDIF

		DO CASE
   		CASE nNumChars = SOCKET_ERROR
   			  SELF:nLastError := DWORD(_CAST,WSAGetLastError())
   			  IF SELF:nLastError == WSAETIMEDOUT
   			     SELF:InternetStatus(0, WSAETIMEDOUT, "socket timeout", 0)
   			     IF nLoops <= 0
   			        cRcvBuf := NULL_STRING
   			        EXIT
                 ENDIF
   				  nLoops--
   				  SELF:nLastError := 0
   			  ELSE
   				  SELF:InternetStatus(0, SELF:nLastError, "socket error", 0)
   				  cRcvBuf := NULL_STRING
   				  EXIT
   			  ENDIF

   		CASE nNumChars = 0
   		     cRcvBuf := NULL_STRING
   			  EXIT

   		OTHERWISE
              cRcvBuf := Mem2String(SELF:pRcvBuf, DWORD(nNumChars))
              #IFDEF __DEBUG__       // wcm 2003-11-19
         	     DebOut32("__ReceiveBuf(): " + Left(cRcvBuf,120))
         	  #ENDIF
              EXIT

		ENDCASE
	ENDDO

	RETURN nNumChars != SOCKET_ERROR

METHOD __SendRaw(cData AS STRING, dwSize REF DWORD) AS LOGIC STRICT
   //SE-040702
	LOCAL nRet         AS INT
	LOCAL nLoops       AS INT
	LOCAL dwPos        AS DWORD
    LOCAL dwBufferSize AS DWORD
	LOCAL pData        AS BYTE PTR
	LOCAL cTemp        AS STRING
	LOCAL dwSent       AS DWORD
	LOCAL uRet         AS USUAL
	LOCAL nTries       AS LONGINT

	SELF:nLastError := 0

	IF dwSize = 0
	   dwSize := SLen(cData)
	   IF dwSize = 0
         RETURN TRUE
      ENDIF
   ENDIF

	dwBufferSize := DWORD(SELF:nSndBuf)
	nLoops       := SELF:nTimeOutRetries
	dwPos        := 0
	dwSent       := 0
	pData        := String2Psz(cData) //SE-070611

	#IFDEF __DEBUG__
		DebOut32("__SendRaw() dwSize = " + NTrim(dwSize))
	#ENDIF

	DO WHILE dwPos < dwSize

      IF dwSize - dwPos < dwBufferSize
   		dwBufferSize := dwSize - dwPos // set the size of the send buffer to be the max or what's left to get
   	ENDIF

      uRet := SELF:InternetStatus(0, INTERNET_STATUS_SENDING_REQUEST, NULL_PTR, 0)

      IF IsLogic(uRet) .AND. ! uRet
         nRet := -1
         SELF:nLastError := ERROR_INTERNET_OPERATION_CANCELLED
         EXIT
      ENDIF
		nTries := 0
		DO WHILE TRUE
			nTries += 1
			WSASetLastError(0)
			nRet   := WSockSend(DWORD(SELF:nSocket), pData + dwPos, LONGINT(dwBufferSize), 0)

			#IFDEF __DEBUG__
				DebOut32("__SendRaw() nRet = " + NTrim(nRet) + " #"+SubStr3(cData, dwPos+1, 40) + " ntries "+NTrim(nTries))
			#ENDIF

			IF nRet == SOCKET_ERROR
				SELF:nLastError := DWORD(_CAST,WSAGetLastError())
				IF SELF:nLastError == WSAETIMEDOUT
				   cTemp := "socket timeout"
				   nLoops--
				ELSE
				   nLoops := 0
               IF SELF:nLastError < 10000
                  cTemp := "server OS code"
               ELSE
                  cTemp := "socket error"
               ENDIF
            ENDIF
            SELF:InternetStatus(0, SELF:nLastError, cTemp, 0)
            IF nLoops <= 0
               dwSize := dwSent
               RETURN FALSE
            ENDIF
			ELSE
			   dwSent += DWORD(nRet)
				EXIT
			ENDIF

			#IFDEF __DEBUG__
				DebOut32("__SendRaw() Error Loop" )
			#ENDIF
		ENDDO

	   cTemp := NTrim(nRet)
      SELF:InternetStatus(0, INTERNET_STATUS_REQUEST_SENT, cTemp, SLen(cTemp))

		dwPos += dwBufferSize
	ENDDO

	#IFDEF __DEBUG__
		DebOut32("__SendRaw() Return " + IIF(nRet < 0,"Error", "Sucess" ))
	#ENDIF

	dwSize := dwSent

	RETURN nRet >= 0


METHOD __SetErrorVars(cFile AS STRING, nLine AS DWORD) AS DWORD STRICT
	LOCAL nErr AS DWORD

	nErr := WSAGetLastError()
	SELF:nLastError := nErr

	SELF:cErrorString := "Error: " + NTrim(nErr) + " -- "
	SELF:cErrorString += cFile + " Line #" + NTrim(nLine) + CRLF
	SELF:cErrorString += SELF:ErrorMsg

	SELF:nCurrentStatus := SSTAT_ERRORSTATE

	SELF:DisplayError()

	RETURN nErr

METHOD __SetOptions () AS VOID STRICT
	LOCAL nTemp AS INT
	LOCAL nSize AS INT

	nSize := _SIZEOF(INT)
	IF getsockopt(SELF:nSocket, SOL_SOCKET, SO_RCVBUF, @nTemp, @nSize) = 0
		SELF:nRcvBuf := nTemp
	ELSE
		SELF:nRcvBuf := MAX_SOCKBUFF
	ENDIF

	nSize := _SIZEOF(INT)
	IF getsockopt(SELF:nSocket, SOL_SOCKET, SO_SNDBUF, @nTemp, @nSize) = 0
		SELF:nSndBuf := nTemp
	ELSE
		SELF:nSndBuf := MAX_SOCKBUFF
	ENDIF
	SELF:pRcvBuf := MemAlloc(DWORD(SELF:nRcvBuf))
	SELF:pSndBuf := MemAlloc(DWORD(SELF:nSndBuf))

	#IFDEF __DEBUG
		DebOut32("Socket Number  Set: " + NTrim(SELF:nSocket))
		DebOut32("Socket Recv Buffer: " + NTrim(SELF:nRcvBuf))
		DebOut32("Socket Send Buffer: " + NTrim(SELF:nSndBuf))
	#ENDIF


//	SELF:nTimeOut := Max(SELF:nTimeout, 1000)		//wcm 2003-11-19

	nTemp := SELF:nTimeout

	// UH 05/30/2000 Moved from sendline()
	setsockopt( SELF:nSocket, SOL_SOCKET, SO_SNDTIMEO, @nTemp, _SIZEOF(INT) )
	setsockopt( SELF:nSocket, SOL_SOCKET, SO_RCVTIMEO, @nTemp, _SIZEOF(INT) )

	RETURN

METHOD __Start(nSock AS DWORD) AS LOGIC STRICT

	IF nSock = 0
		nSock := INVALID_SOCKET
	ENDIF

	SELF:lCleanupRequired := .T.

	IF nSock = INVALID_SOCKET
		SELF:nSocket := socket(PF_INET, SELF:nSocketType, 0)

		IF SELF:nSocket == INVALID_SOCKET
			SELF:cErrorString := "Unable to open a new socket."
			SELF:DisplayError()
			RETURN .F.
		ENDIF
	ELSE
		SELF:nSocket := nSock
	ENDIF

	RETURN .T.

METHOD accept() AS CSocket STRICT
	LOCAL oSocket AS CSocket
	LOCAL nSock AS DWORD
	LOCAL nErr AS DWORD

	//
	// do not get the address of remote end.
	//
	nSock := accept(SELF:nSocket, NULL, NULL)

	IF (nSock != INVALID_SOCKET)
		//
		// the new socket will only have AsyncSelect of FD_ACCEPT on
		// a call to SetReceiveTarget will reset SELF
		//
		oSocket := CSocket{SELF:nSocketType, nSock, SSTAT_CONNECTED}

		// UH 07/06/2000
		oSocket:__SetOptions()
	ELSE
		nErr := WSAGetLastError()
		IF nErr != WSAEWOULDBLOCK
			SELF:__SetErrorVars(ProcName(), ProcLine())
		ENDIF
	ENDIF

	RETURN oSocket


DESTRUCTOR()
	//RvdH 070407 Moved code from Axit() to Destroy()
	Destroy()
	RETURN

METHOD bind(nPort AS WORD, cIP AS STRING, nFamily AS SHORTINT) AS LOGIC STRICT
	LOCAL nSize AS INT
	LOCAL lTemp AS DWORD
	LOCAL sin IS _WinSockAddr_IN

	nSize := _SIZEOF(_WinSockAddr_IN)
	MemSet(@sin, 0, DWORD(nSize))

	sin:sin_family := nFamily //AF_INET

	IF SLen(cIP) = 0
		sin:sin_addr:s_un:s_addr := htonl(INADDR_ANY)
	ELSE
		lTemp := inet_addr(String2Psz(cIP))
		IF lTemp == INADDR_NONE
			SELF:__SetErrorVars(ProcName(), ProcLine())
			RETURN FALSE
		ENDIF
		sin:sin_addr:s_un:s_addr := lTemp
	ENDIF

	sin:sin_port := htons(nPort)

	IF bind(SELF:nSocket, @sin, nSize) == SOCKET_ERROR
		SELF:__SetErrorVars(ProcName(), ProcLine())
		RETURN FALSE
	ENDIF

	RETURN TRUE

METHOD Close() AS LOGIC STRICT
	LOCAL lRet AS LOGIC

	IF SELF:nCurrentStatus != SSTAT_UNINITIALIZED
		// UH 09/29/2000
		SELF:Disconnect()
		IF closesocket(SELF:nSocket) = SOCKET_ERROR
			SELF:Error := WSAGetLastError()
		ELSE
			lRet := .T.
			SELF:nCurrentStatus := SSTAT_UNINITIALIZED
		ENDIF
	ENDIF

	RETURN lRet

METHOD connect(cIP AS STRING, nPort AS WORD) AS LOGIC STRICT
	LOCAL cMsg	AS STRING
	LOCAL lRet AS LOGIC

	IF SELF:nCurrentStatus != SSTAT_DISCONNECTED
		RETURN FALSE
	ENDIF

	cMsg := "Connecting to Server " + cIP + ", port " + NTrim(nPort)
	SELF:InternetStatus(0, INTERNET_STATUS_CONNECTING_TO_SERVER, cMsg, SLen(cMsg))

	SELF:nCurrentStatus := SSTAT_CONNECTING

	SELF:ServerAddress:sin_family := AF_INET
	SELF:ServerAddress:sin_addr:s_un:s_addr := inet_addr(String2Psz(cIP))
	SELF:ServerAddress:sin_port := htons(nPort)

	IF SELF:ServerAddress:sin_addr:s_un:s_addr == INADDR_NONE
		//
		// treat this as a host name and get it resolved.
		//
		cIP := GetIPAddress(cIP)

		IF SLen(cIP) = 0
			SELF:__SetErrorVars(ProcName(), ProcLine())
			RETURN .F.
		ELSE
			SELF:ServerAddress:sin_addr:s_un:s_addr := inet_addr(String2Psz(cIP))
		ENDIF

		IF ServerAddress:sin_addr:s_un:s_addr == INADDR_NONE
			SELF:__SetErrorVars(ProcName(), ProcLine())
			RETURN .F.
		ENDIF
	ENDIF

	lRet := SELF:__ConnectThread()

	IF lRet
		cMsg := "Connected to " + cIP + ", port " + NTrim(nPort)
		SELF:InternetStatus(0, INTERNET_STATUS_CONNECTED_TO_SERVER, cMsg, SLen(cMsg))

		SELF:__SetOptions()
	ELSE
		SELF:DisplayError()
	ENDIF

	RETURN lRet

METHOD Destroy()
	//RvdH 070407 Moved code from Axit() to Destroy()
	LOCAL cMsg AS STRING
	IF SELF:lCleanupRequired
		IF SELF:nSocket != INVALID_SOCKET
			// UH 11/12/2000
			//self:Disconnect()
			SELF:Close()

			MemFree(SELF:ServerAddress)
			//MemFree(SELF:pHostBuffer)

			IF SELF:pRcvBuf != NULL_PTR
				MemFree(SELF:pRcvBuf)
				SELF:pRcvBuf := NULL_PTR
			ENDIF
			IF SELF:pSndBuf != NULL_PTR
				MemFree(SELF:pSndBuf)
				SELF:pSndBuf := NULL_PTR
			ENDIF

			IF !InCollect()
				cMsg := "Disconnecting and cleanup socket " + NTrim(SELF:nSocket)
				SELF:InternetStatus(0, INTERNET_STATUS_CLOSING_CONNECTION, cMsg, SLen(cMsg))
			ENDIF

			SELF:nSocket := INVALID_SOCKET
		ENDIF

		SELF:lCleanupRequired := .F.
	ENDIF
	IF ! InCollect()
		UnregisterAxit(SELF)
	ENDIF
	RETURN NIL

METHOD DisConnect() AS LOGIC STRICT
	LOCAL lRet AS LOGIC

	IF SELF:nCurrentStatus != SSTAT_UNINITIALIZED
		IF SELF:nCurrentStatus != SSTAT_DISCONNECTED
			// UH 09/29/2000
			IF shutdown(SELF:nSocket, SD_BOTH) = SOCKET_ERROR
				SELF:Error := WSAGetLastError()
			ELSE
				lRet := .T.
			ENDIF

			#IFDEF __DEBUG__
				DebOut32( "Disconnecting socket " + NTrim(SELF:nSocket) + " -> " + AsString(lRet) )
			#ENDIF
			SELF:nCurrentStatus := SSTAT_DISCONNECTED
		ENDIF
	ENDIF

	RETURN lRet

METHOD DisplayError() AS VOID STRICT
	LOCAL cErr AS STRING

	cErr := SELF:ErrorMsg

	SELF:InternetStatus(0, SELF:nLastError, cErr, SLen(cErr))

	RETURN

ACCESS Error AS DWORD STRICT
	RETURN SELF:nLastError

ASSIGN Error(n AS DWORD)  STRICT
	RETURN SELF:nLastError := n

ACCESS ErrorMsg AS STRING STRICT
	RETURN SystemErrorString(SELF:nLastError, "Socket Error " + NTrim(SELF:nLastError))

METHOD GetLine() AS STRING STRICT
   //SE-040707
	LOCAL cRet AS STRING
	LOCAL cMsg AS STRING

	IF SELF:__ReceiveBuf()
      cRet    := cRcvBuf
      cRcvBuf := NULL_STRING
	ENDIF

	cMsg := NTrim(SLen(cRet))
  	SELF:InternetStatus(0, INTERNET_STATUS_RESPONSE_RECEIVED, cMsg, SLen(cMsg))

	RETURN cRet

METHOD GetLineFrom(cIP REF STRING, nRemPort REF DWORD) AS STRING STRICT
   LOCAL nRet AS LONGINT
   LOCAL nSize AS INT
   LOCAL cRet AS STRING
   LOCAL sin IS _WinSockAddr_IN
   LOCAL DIM abBuffer[1024] AS BYTE
   LOCAL iTimeOut AS INT

	nSize := _SIZEOF(_WinSockAddr_IN)

	MemSet(@sin, 0, DWORD(nSize))

   iTimeOut := nTimeOut
	setsockopt(SELF:nSocket, SOL_SOCKET, SO_SNDTIMEO, @iTimeOut, _SIZEOF(INT))
	setsockopt(SELF:nSocket, SOL_SOCKET, SO_RCVTIMEO, @iTimeOut, _SIZEOF(INT))

	nRet := recvfrom(SELF:nSocket, @abBuffer[1], 1024, 0, @sin, @nSize)

	IF nRet == SOCKET_ERROR
		#IFDEF __DEBUG__
			DebOut32("CSocket:GetLineFrom() -> Socket Error")
		#ENDIF

		SELF:__SetErrorVars(ProcName(), ProcLine())
	ELSE
		cRet     := Mem2String(@abBuffer[1], DWORD(nRet))
		nRemPort := ntohs(sin:sin_port)
		cIP      := Psz2String(inet_ntoa(sin:sin_addr))
	ENDIF

	RETURN cRet

METHOD GetLines(aSearch AS ARRAY) AS ARRAY STRICT
	LOCAL aRet        AS ARRAY
	LOCAL lEOL        AS LOGIC
	LOCAL cLine       AS STRING
	LOCAL dwSearch    AS DWORD
	LOCAL dwI         AS DWORD
	LOCAL cStatusInfo AS STRING

	dwSearch := ALen(aSearch)
	aRet     := {}
	cRcvBuf  := NULL_STRING

	DO WHILE ! lEol
	   cLine := SELF:GetRawText(TRUE, TRUE)
	   IF cLine == NULL_STRING
			//	End has been found
			lEOL := TRUE
		ELSE
			IF dwSearch > 0
			   FOR dwI := 1 TO dwSearch
			       IF AtC(aSearch[dwI], cLine) > 0
					    AAdd(aRet, cLine)
						 EXIT
					 ENDIF
				NEXT  // dwI
			ELSE
			   AAdd(aRet, cLine)
			ENDIF
		ENDIF
	ENDDO

    cStatusInfo := "Lines received " + NTrim(ALen(aRet))
	SELF:InternetStatus(0, INTERNET_STATUS_RESPONSE_RECEIVED, cStatusInfo, SLen(cStatusInfo))

	RETURN aRet


METHOD getpeername(cName REF STRING, nPort REF INT) AS LOGIC STRICT
	LOCAL lRet AS LOGIC
	LOCAL nSize AS INT
	LOCAL sin IS _WinSockAddr_IN

	nSize := _SIZEOF(_WinSockAddr_IN)
	MemSet(@sin, 0, DWORD(nSize))

	IF getpeername(SELF:nSocket, @sin, @nSize) = 0
		nPort := ntohs(sin:sin_port)
		cName := Psz2String(inet_ntoa(sin:sin_addr))
		lRet := .T.
	ELSE
		SELF:nLastError := WSAGetLastError()
	ENDIF

	RETURN lRet

METHOD GetRaw() AS STRING STRICT
	LOCAL cRet 				AS STRING
	LOCAL nLoops 			AS INT
	LOCAL nNumChars 		AS LONGINT
	LOCAL cMsg 				AS STRING
	LOCAL lProcessedPackets	AS LOGIC

	nLoops 				:= 0
	cRet				:= ""
	lProcessedPackets	:= FALSE

	#IFDEF __DEBUG__
		DebOut32("GetSocketRaw() ... nTimeOut = " + NTrim(nTimeout))
	#ENDIF

	DO WHILE nLoops < SELF:nTimeoutReTries
		MemSet(SELF:pRcvBuf, 0, DWORD(SELF:nRcvBuf))

		nNumChars := recv( SELF:nSocket, SELF:pRcvBuf,SELF:nRcvBuf,	0 )
		SELF:nLastError := WSAGetLastError()

		#IFDEF __DEBUG__       // wcm 2003-11-19
			DebOut32("             characters received: " + NTrim(nNumChars))
		#ENDIF

		DO CASE
		CASE nNumChars = SOCKET_ERROR
			IF lProcessedPackets
				// wcm 2004-04-22
				// this isnt an error, just no more data to read
				SELF:nLastError := 0
				EXIT
			ELSE
				// this process added back as necessary
				IF SELF:nLastError == WSAETIMEDOUT
					nLoops ++
					SELF:InternetStatus(0, WSAETIMEDOUT, "socket timeout", 0)
				ELSE
					SELF:InternetStatus(0, SELF:nLastError, "socket error", 0)
					EXIT
				ENDIF
			ENDIF

		CASE nNumChars = 0
			SELF:nLastError := 0
			EXIT

		OTHERWISE
			cMsg := Mem2String(SELF:pRcvBuf, DWORD(nNumChars))
			//PP-040512 Issue 12946 Line below was in prior to 2.7 but missing in 2.7 - used to report status
			SELF:InternetStatus(0, INTERNET_STATUS_RECEIVING_RESPONSE, cMsg, nNumChars)
			cRet += cMsg
			lProcessedPackets := TRUE
		ENDCASE
	ENDDO

	cMsg := NTrim(SLen(cRet)) + " bytes received"
	SELF:InternetStatus(0, INTERNET_STATUS_RESPONSE_RECEIVED, cMsg, SLen(cMsg))

	RETURN cRet

METHOD GetRawText(lLineMode := FALSE AS LOGIC, lNext := FALSE AS LOGIC, dwByteCount := 0 AS DWORD) AS STRING STRICT
	LOCAL cRet 			AS STRING
	LOCAL dwReceived 	AS DWORD
	LOCAL dwLen       AS DWORD
	LOCAL lExit       AS LOGIC
	LOCAL cMsg        AS STRING

	IF lNext
      cRet := cRcvBuf
   ENDIF
   cRcvBuf := NULL_STRING

	DO WHILE ! lExit

	   IF lLineMode
	      IF (dwLen := At2(CRLF, cRet)) > 0
	         cRcvBuf := SubStr2(cRet, dwLen+2)
	         // RFC 1939 - If any line of the multi-line response
            // begins with the termination octet ('.'), the line is "byte-stuffed" by
            // pre-pending the termination octet to that line of the response.
	         IF cRet = "."
               IF cRet = "." + CRLF
                  cRet := cRcvBuf := NULL_STRING
               ELSEIF cRet = ".."
                  cRet := SubStr3(cRet, 2, dwLen)
               ELSE
                  cRet := SubStr3(cRet, 1, dwLen+1)
               ENDIF
            ELSE
               cRet := SubStr3(cRet, 1, dwLen+1)
            ENDIF
            lExit   := TRUE
         ENDIF
	   ELSE

         dwReceived := SLen(cRcvBuf)
         IF dwByteCount > 0
   	      dwLen := SLen(cRet)
   	      IF dwLen + dwReceived > dwByteCount
   	         dwReceived := dwByteCount - dwLen
   	         cRet    += Left(cRcvBuf, dwReceived)
   	         cRcvBuf := SubStr2(cRcvBuf, dwReceived + 1)
   	         lExit := TRUE
            ELSE
               cRet    += cRcvBuf
               cRcvBuf := NULL_STRING
   	      ENDIF
   	   ELSE
   	      cRet    += cRcvBuf
            cRcvBuf := NULL_STRING
         ENDIF
         IF ! lExit .AND. Right(cRet, SLen(DEFAULT_STOPDATA)) == DEFAULT_STOPDATA
            lExit := TRUE
         ENDIF
   	ENDIF

	   IF ! lExit
         IF ! SELF:__ReceiveBuf()
            EXIT
   	   ENDIF

   	   IF lLineMode
	         cRet += cRcvBuf
         ENDIF
   	ENDIF
	ENDDO

	cMsg := NTrim(SLen(cRet))
  	SELF:InternetStatus(0, INTERNET_STATUS_RESPONSE_RECEIVED, cMsg, SLen(cMsg))

	RETURN cRet

METHOD getsockname(cName REF STRING, nPort REF INT) AS LOGIC STRICT
	LOCAL lRet AS LOGIC
	LOCAL nSize AS LONGINT
	LOCAL sin IS _WinSockAddr_IN

	nSize := _SIZEOF(_WinSockAddr_IN)
	MemSet(@sin, 0, DWORD(nSize))

	IF getsockname(SELF:nSocket, @sin, @nSize) = 0
		nPort := ntohs(sin:sin_port)
		cName := Psz2String(inet_ntoa(sin:sin_addr))
		lRet := .T.
	ELSE
		SELF:nLastError := WSAGetLastError()
	ENDIF

	RETURN lRet


CONSTRUCTOR(nType, xSocket, xSocketStatus)
	LOCAL nSock AS DWORD
	LOCAL nStat AS INT
	LOCAL cMsg AS STRING

	IF IsNumeric(nType) .AND. nType <= SOCK_SEQPACKET
		SELF:nSocketType := nType
	ELSE
		SELF:nSocketType := SOCK_STREAM
	ENDIF

	IF IsNumeric(xSocket) .AND. IsNumeric(xSocketStatus)
		nSock := xSocket
		nStat := xSocketStatus
	ELSE
		nSock := INVALID_SOCKET
		nStat := SSTAT_DISCONNECTED
	ENDIF

	SELF:nTimeout := 500  // 1000
	SELF:nTimeOutRetries := 10
	SELF:nCurrentStatus := SSTAT_UNINITIALIZED
	SELF:lCleanupRequired := .F.
	//SELF:pRawSendData := NULL_PTR
	//SELF:nRawSendDataLength := 0
	//SELF:cSendLine := ""
	//SELF:cReceiveLine := ""

	//SELF:hWnd := GetActiveWindow()

	SELF:ServerAddress := MemAlloc( _SIZEOF(_WinSockAddr_IN) )
	MemSet(SELF:ServerAddress, 0, _SIZEOF(_WinSockAddr_IN) )

	//SELF:pHostBuffer := MemAlloc(MAXGETHOSTSTRUCT)

	IF SELF:__Start(nSock)
		SELF:nCurrentStatus := nStat
		cMsg := "Socket " + NTrim(SELF:nSocket) + " started"
		SELF:InternetStatus(0, nStat, cMsg, SLen(cMsg))
	ENDIF



	RETURN

METHOD InternetStatus( nContext, nStatus, xStatus, nStatusLength )

	#IFDEF __DEBUG__
	   LOCAL cMsg AS STRING
		cMsg := StrZero(nStatus, 5, 0) + ": " + SubStr3( AsString(xStatus), 1, 64)
		DebOut32("Socket Status-"+cMsg)
	#ENDIF

	RETURN TRUE

METHOD listen(nBackLog AS INT) AS LOGIC STRICT

	IF nBackLog == 0
		nBackLog := 5
	ENDIF

	IF listen(SELF:nSocket, nBackLog) == SOCKET_ERROR
		SELF:__SetErrorVars(ProcName(), ProcLine())
		RETURN FALSE
	ENDIF

	SELF:nCurrentStatus := SSTAT_LISTENING

	RETURN TRUE

ACCESS RcvBufSize() AS INT STRICT
	RETURN SELF:nRcvBuf

METHOD SendLine(cData AS STRING) AS INT STRICT
    //SE-040702
    LOCAL dwSent AS DWORD

    dwSent := 0
    IF SELF:__SendRaw(cData, @dwSent)
       RETURN INT(dwSent)
    ENDIF

    RETURN -1

METHOD SendLineTo(cData AS STRING, cDest AS STRING, nRemPort AS WORD) AS INT STRICT
   //SE-040625
	LOCAL nSize AS DWORD
	LOCAL nSinSize AS DWORD
	LOCAL nRet AS INT
	//LOCAL nWait AS DWORD
	//LOCAL pFunc AS PTR
	//LOCAL pHT AS PTR
	//LOCAL dwID AS DWORD
	LOCAL cIP AS STRING
	LOCAL sin IS _WinSockAddr_IN
	LOCAL DIM abBuffer[1024] AS BYTE

	IF SLen(cData) = 0
		RETURN 0
	ENDIF

	nSinSize := _SIZEOF(_WinSockAddr_IN)
	MemSet(@sin, 0, nSinSize)

	SELF:nLastError := 0

	nSize := SLen(cData)
	IF nSize>=1024
      nSize := 1023
   ENDIF

	MemCopy(@abBuffer[1], String2Psz(cData), nSize + 1)

	IF Occurs(".", cDest) = 3
		cIP := cDest
	ELSE
		cIP := GetIPAddress(cDest)
	ENDIF

	IF SLen(cIP) = 0
		sin:sin_addr:s_un:s_addr := htonl(INADDR_BROADCAST)
	ELSE
		sin:sin_family := AF_INET
		sin:sin_port := htons(nRemPort)
		sin:sin_addr:s_un:s_addr := inet_addr(String2Psz(cIP))
	ENDIF

	IF sin:sin_addr:s_un:s_addr = INADDR_NONE
		WSASetLastError(WSAEINVAL)
		RETURN 0
	ENDIF

	IF SELF:nCurrentStatus != SSTAT_DISCONNECTED
		RETURN 0
	ENDIF

	SELF:nCurrentStatus := SSTAT_CONNECTING

	nRet := sendto( SELF:nSocket, @abBuffer[1], INT(nSize), 0, @sin, INT(nSinSize)) // UH 11/02/2000 nSize

	IF nRet = SOCKET_ERROR
		SELF:__SetErrorVars(ProcName(), ProcLine())
		nRet := 0
	ELSE
		nRet := INT(nSize)
	ENDIF

	SELF:nCurrentStatus := SSTAT_DISCONNECTED

	RETURN nRet

METHOD SendRaw(pData AS PTR, nSize AS DWORD) AS LOGIC STRICT

	LOCAL lRet AS LOGIC
	LOCAL nRet, nLoops, nRemaining, nBuffer AS INT
	LOCAL nTemp AS DWORD
	LOCAL pTemp AS BYTE PTR

	nBuffer := SELF:nSndBuf
	nRemaining := INT(_CAST, nSize)		// allows it to go negative
	nTemp := 0
	nLoops := 0

	#IFDEF __DEBUG__
		DebOut32("SendRaw() ... nSize = " + NTrim(nSize) )
	#ENDIF

	DO WHILE nRemaining > 0
		IF nRemaining < nBuffer
			nBuffer := nRemaining		// set the size of the send buffer to be the max or what's left to get
		ENDIF

		pTemp := pData
		pTemp += nTemp

		DO WHILE nLoops < SELF:nTimeoutReTries
			nRet := WSockSend( SELF:nSocket, pTemp, nBuffer, 0 )
			#IFDEF __DEBUG__
				DebOut32("SendRaw() ... nRet = " + NTrim(nRet) )
			#ENDIF

			IF nRet == SOCKET_ERROR
				SELF:nLastError := WSAGetLastError()
				IF SELF:nLastError == WSAETIMEDOUT
					SELF:InternetStatus(0, WSAETIMEDOUT, "socket timeout", 0)
				ELSEIF SELF:nLastError < 10000
					SELF:InternetStatus(0, SELF:nLastError, "server OS code", 0)
					EXIT
				ELSE
					SELF:InternetStatus(0, SELF:nLastError, "socket error", 0)
					EXIT
				ENDIF
			ELSE
				EXIT
			ENDIF
			#IFDEF __DEBUG__
				DebOut32("SendRaw() ... Error Loop" )
			#ENDIF
		ENDDO
		nTemp += DWORD(nBuffer)
		nRemaining -= nBuffer
	ENDDO

	IF nRemaining > 0
		#IFDEF __DEBUG__
			DebOut32("SendRaw() ... Return Error" )
		#ENDIF

		lRet := .F.
	ELSE
		#IFDEF __DEBUG__
			DebOut32("SendRaw() ... Return Sucess" )
		#ENDIF

		lRet := .T.
	ENDIF

	RETURN lRet


METHOD SendRawText(cData AS STRING) AS LOGIC STRICT
   //SE-040625
   LOCAL dwSize AS DWORD

   RETURN SELF:__SendRaw(cData, @dwSize)

ACCESS SndBufSize() AS INT STRICT
	RETURN SELF:nSndBuf

ACCESS Status() AS INT STRICT
	RETURN SELF:nCurrentStatus

ACCESS TimeOut() AS INT STRICT
	RETURN SELF:nTimeOut

ASSIGN TimeOut(nNew AS INT)  STRICT
	SELF:nTimeout := nNew
	IF SELF:nSocket != INVALID_SOCKET
		setsockopt( SELF:nSocket, ;
				SOL_SOCKET, ;
				SO_SNDTIMEO, ;
				@nNew, ;
				_SIZEOF(INT) )

		setsockopt( SELF:nSocket, ;
				SOL_SOCKET, ;
				SO_RCVTIMEO, ;
			@nNew, ;
			_SIZEOF(INT) )
	ENDIF

	RETURN SELF:nTimeOut

ACCESS TimeOutRetries() AS INT STRICT
	RETURN SELF:nTimeOutRetries

ASSIGN TimeOutRetries(nNew AS INT)  STRICT
	SELF:nTimeoutRetries := nNew
	RETURN SELF:nTimeOutRetries

END CLASS

PARTIAL CLASS StdSocket INHERIT CSocket
	PROTECT oOwner AS OBJECT

CONSTRUCTOR(xOwner, nType, xSocket, xSocketStatus)
	IF IsObject(xOwner)
		oOwner := xOwner
	ENDIF

	SUPER( nType, xSocket, xSocketStatus )

	RETURN

METHOD InternetStatus( nContext, nStatus, xStatus, nStatusLength )
	IF IsMethod(SELF:oOwner, #InternetStatus)
		RETURN SELF:oOwner:InternetStatus(nContext, nStatus, xStatus, nStatusLength)
	ENDIF
	RETURN TRUE

END CLASS

INTERNAL VOSTRUCT _THREAD_DATA
	MEMBER nRC AS INT
	MEMBER nSize AS INT
	MEMBER nSocket AS DWORD
	MEMBER ServerAddress IS _WinSockAddr_IN
	MEMBER pSin AS _WinSockAddr_IN
	MEMBER DIM abBuffer[1024] AS BYTE

_DLL FUNCTION WSAGetLastError() AS DWORD PASCAL:WSOCK32.WSAGetLastError

#ifdef __VULCAN__

STATIC FUNCTION __ConnectFunc( oData AS OBJECT ) AS VOID
   LOCAL pData AS _THREAD_DATA
   pData := (_THREAD_DATA PTR) (IntPtr) oData
	pData:nRC := connect(pData:nSocket, (_WINsockaddr PTR) @pData:ServerAddress, pData:nSize)
	RETURN

#else

STATIC FUNCTION __ConnectFunc( pData AS _THREAD_DATA) AS VOID STRICT

	pData:nRC := connect(pData:nSocket, @pData:ServerAddress, pData:nSize)

	ExitVOThread(0)
	RETURN

#endif




#region defines
DEFINE FIOASYNC := 0x8004667D
DEFINE FIONBIO := 0x8004667E
DEFINE FIONREAD := 0x4004667F
DEFINE MIN_SOCKETS_REQUIRED := 10
DEFINE SCMD_NEWSOCKETACCEPTED := -101
DEFINE SCMD_SOCKETSTATUSCHANGED := -100
/*
#warning DEFINES also defined in WinAPI
DEFINE SD_BOTH := 2
DEFINE SD_RECEIVE := 0
DEFINE SD_SEND := 1*/

DEFINE SOCK_BLOCK_SIZE := 520
DEFINE SSTAT_CONNECTED := 5
DEFINE SSTAT_CONNECTING := 2
DEFINE SSTAT_DISCONNECTED := 1
DEFINE SSTAT_DISCONNECTING := 4
DEFINE SSTAT_ERRORSTATE := 6
DEFINE SSTAT_LISTENING := 3
DEFINE SSTAT_TIMEDOUT := 7
DEFINE SSTAT_UNINITIALIZED := 0
DEFINE WS_VERSION_MAJOR := 1
DEFINE WS_VERSION_MINOR := 1
DEFINE WS_VERSION_REQUIRED := 0x0101
#endregion
