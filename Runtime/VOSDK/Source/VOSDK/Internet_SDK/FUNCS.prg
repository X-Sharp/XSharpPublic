FUNCTION __CheckUUEncode	(c AS STRING)		AS DWORD STRICT
	LOCAL nRet		AS DWORD
	LOCAL cTemp		AS STRING
	LOCAL cBefore	AS STRING

	nRet := At2(UUE_START_NEWS, c)

	IF nRet > 0
		IF nRet == 1
			cBefore := CRLF
		ELSE
			cBefore := SubStr(c, nRet-2, 2)
		ENDIF
		IF cBefore == CRLF
			cTemp   := SubStr(c, nRet + SLen(UUE_START_NEWS), 5)
			cTemp := AllTrim(cTemp)
			IF SLen(cTemp) == 3 .AND. Val(cTemp) > 599
			ELSE
				nRet := 0
			ENDIF
		ELSE
			nRet := 0
		ENDIF
	ENDIF

	RETURN nRet

FUNCTION __DecodeB64(cPath AS STRING, cFile AS STRING, cMail AS STRING) AS DWORD STRICT

	LOCAL nRet      AS DWORD
	LOCAL hfOut     AS PTR
	LOCAL cFName    AS STRING
	LOCAL i         AS DWORD
	LOCAL nPos      AS DWORD
	//    LOCAL nStop     AS DWORD
	LOCAL cTemp     AS STRING
	//    LOCAL nSize     AS DWORD

	// this method strips out preceeding and affixed double CRs
	// it assumes there are NOT extra ones. The decode will fail, otherwise.
	// the 'correctly" formed attachment segment to decode will have the following
	// structure:  XXXXXXXCRLFXXXXXXXXCRLFXXXXXXCRLF etc. No double CRLFs

	cFName := __GetFullPath(cPath, cFile)
	cTemp := cMail

	// strip all preceeding CRLFs from the front of the string
	i := 1
	DO WHILE SubStr(cTemp, i, 2) = CRLF
		i += 2
	ENDDO
	IF i > 1
		cTemp := SubStr2(cTemp, i)
	ENDIF

	// Now see if there is another double CRLF...
	nPos := At2(TEMP_DECODE_BOUND, cTemp)
	IF nPos > 0
		cTemp := SubStr(cTemp, 1, nPos-1)
		// but the string MUST end in a CRLF or decoding won't work properly
		IF !(Right(cTemp,2) == CRLF)
			cTemp += CRLF
		ENDIF
	ENDIF

	hfOut := FCreate(cFName)

	IF hfOut != F_ERROR
		nRet := DWORD(DecodeBase64(cTemp, hfOut))
		FClose(hfOut)
	ENDIF

	RETURN nRet


FUNCTION __DecodeQP (cMailPart AS STRING, hfOut AS PTR)  AS DWORD    STRICT
	LOCAL nRet			AS DWORD
	LOCAL cPart			AS STRING
	LOCAL cDecode		AS STRING
	LOCAL nFound		AS DWORD
	LOCAL nOldPos		AS DWORD
	LOCAL lEOF			AS LOGIC
	LOCAL lDecode		AS LOGIC

	nOldPos	:= 1
	lEof 	:= .F.

	DO WHILE !lEOF
		nFound := At3("=", cMailPart, nOldPos-1)

		IF nFound > 0
			cDecode := SubStr(cMailPart, nFound+1, 2)
			IF cDecode == CRLF
				lDecode := FALSE
			ELSE
				lDecode := TRUE
			ENDIF

			cPart := SubStr(cMailPart, nOldPos, nFound-nOldPos )

			IF lDecode
				cPart += Chr(Val("0x"+cDecode))
			ENDIF
		ELSE
			cPart := SubStr(cMailPart, nOldPos)
			lEOF := TRUE
		ENDIF
		nRet += FWrite(hfOut, cPart )
		nOldPos := nFound + 3
	ENDDO

	RETURN nRet

FUNCTION __DecodeQPrintable(cPath AS STRING, cFile AS STRING, cMail AS STRING) AS DWORD STRICT

	LOCAL nRet      AS DWORD
	LOCAL nSize     AS DWORD
	LOCAL nPos      AS DWORD
	LOCAL hfOut     AS PTR

	IF SLen(cMail) = 0
		RETURN 0
	ENDIF

	IF SLen(cFile) = 0
		RETURN 0
	ENDIF

	cFile := __GetFullPath(cPath, cFile)
	hfOut := FCreate(cFile)

	IF hfOut != F_ERROR
		nPos := At2(TEMP_DECODE_BOUND, cMail)

		IF nPos > 0
			nSize := SLen(TEMP_DECODE_BOUND)
			cMail := SubStr2(cMail, nPos + nSize)
		ENDIF

		nRet := __DecodeQP(cMail, hfOut)

		FClose(hfOut)
	ENDIF

	RETURN nRet


FUNCTION __DecodeUU(cText AS STRING, hfOut AS PTR)    AS DWORD STRICT
	LOCAL cLine     AS STRING
	LOCAL nRet	    AS DWORD
	LOCAL nPos 		AS DWORD
	LOCAL nNewPos	AS DWORD
	LOCAL cbErr     AS CODEBLOCK

	cbErr := ErrorBlock( {|e|_Break(e)} )

	BEGIN SEQUENCE

		nPos    := 1
		nNewPos := At3(CRLF, cText, nPos)

		DO WHILE nNewPos > nPos
			cLine := SubStr3(cText, nPos, nNewPos - nPos + 2)
			nPos := nNewPos + 2
			IF cLine = UUE_END
				EXIT
			ENDIF
			nRet += UUDecodeLine(cLine, hfOut)
			nNewPos := At3(CRLF, cText, nPos)
		ENDDO
	RECOVER
	END SEQUENCE

	ErrorBlock(cbErr)

	RETURN nRet


STATIC FUNCTION __FixDate   (pft AS _WINFILETIME) AS VOID STRICT
	LOCAL stRemote IS _WINSYSTEMTIME
	LOCAL stLocal  IS _WINSYSTEMTIME
	LOCAL nDiff    AS DWORD

	FileTimeToSystemTime(pft, @stRemote)
	GetSystemTime(@stLocal)

	IF stRemote:wYear != stLocal:wYear - 1
		RETURN
	ENDIF

	#IFDEF __DEBUG__
//		AltD()
	#ENDIF

	IF stRemote:wMonth <  stLocal:wMonth
		RETURN
	ENDIF

	nDiff := stRemote:wMonth - stLocal:wMonth

	DO CASE
	CASE nDiff == 0
		//  Same month
		IF stRemote:wDay != stLocal:wDay
			RETURN
		ENDIF

	CASE nDiff == 1
		//  New month at FTP Server
		IF stRemote:wDay != 1
			RETURN
		ENDIF

	OTHERWISE
		RETURN
	ENDCASE

	IF IFXGetTimeDiff(@stRemote, @stLocal) > 0
		stRemote:wYear := stLocal:wYear
		SystemTimeToFileTime(@stRemote, pft)
	ENDIF

	RETURN


FUNCTION __GetDate(pTime AS _WINFILETIME) AS DATE STRICT
	LOCAL dRet    AS DATE
	LOCAL ftLocal IS _WINFILETIME
	LOCAL sysTime IS _WINSYSTEMTIME

	IF FileTimeToLocalFileTime(pTime, @ftLocal)
		pTime := @ftLocal
	ENDIF

	IF FileTimeToSystemTime(pTime, @sysTime)
		dRet := ConDate(sysTime:wYear, sysTime:wMonth, sysTime:wDay)
	ENDIF

	RETURN dRet

FUNCTION __GetFileData(pData AS _WINWIN32_FIND_DATA) AS ARRAY STRICT

   LOCAL aTemp AS ARRAY
   LOCAL cTemp AS STRING
   //LOCAL nPos  AS DWORD

   aTemp := ArrayCreate(F_ATTR)
   cTemp := Psz2String(@pData:cFileName)

   //  UH 03/08/2002
   //RvdH 070423 File Names can have embedded spaces
   //     nPos := RAt(" ", cTemp)
   //     IF (nPos > 0)
   //         cTemp := SubStr2(cTemp, nPos + 1)
   //     ENDIF

   aTemp[F_NAME] := cTemp
   IF pData:nFileSizeHigh > 0
      aTemp[F_SIZE] := pData:nFileSizeLow + (pData:nFileSizeHigh * 1.0 * MAXDWORD)
   ELSE
      aTemp[F_SIZE] := pData:nFileSizeLow
   ENDIF
   aTemp[F_DATE] := __GetDate(@pData:ftLastWriteTime)
   aTemp[F_TIME] := __GetTime(@pData:ftLastWriteTime)

   // UH 08/13/1998
   IF pData:dwFileAttributes = 0
      IF At2(".", cTemp) = 0
         pData:dwFileAttributes := FA_DIRECTORY
      ENDIF
   ENDIF
   aTemp[F_ATTR] := FAttr2String(pData:dwFileAttributes)

   RETURN aTemp




FUNCTION __GetFileName(cFile AS STRING) AS STRING STRICT
	LOCAL dwPos AS DWORD

	IF (dwPos := RAt2("\", cFile)) = 0
		IF (dwPos := RAt2(":", cFile)) = 0
			RETURN cFile
		ENDIF
	ENDIF

	RETURN SubStr2(cFile, dwPos + 1)

FUNCTION __GetFullPath(cPath AS STRING, cFile AS STRING)	AS STRING STRICT
	//SE-040622
	LOCAL dwPos	AS DWORD

	IF SLen(cPath) > 0
		//
		//	File and path name specified, check, if
		//	file name contains path
		//
		IF (dwPos := RAt("\", cFile)) > 0
			cFile := SubStr2(cFile, dwPos + 1)
		ENDIF
		IF Right(cPath, 1) != "\"
			RETURN cPath + "\" + cFile
		ENDIF
		RETURN cPath + cFile
	ENDIF

	RETURN cFile
FUNCTION __GetTime(pTime AS _WINFILETIME) AS STRING STRICT

   LOCAL cRet    AS STRING
   //LOCAL ftLocal IS _WINFILETIME
   //LOCAL sysTime IS _WINSYSTEMTIME
   LOCAL wDate, wTime AS WORD
   LOCAL nHours, nMins, nSecs AS DWORD
   //RvdH 071203 Use DosTime mechanism  because that produces the correct results !
//    IF FileTimeToLocalFileTime(pTime, @ftLocal)
//       pTime := @ftLocal
//    ENDIF

//    IF FileTimeToSystemTime(pTime, @sysTime)
//       cRet := ConTime(sysTime.wHour, sysTime.wMinute, sysTime.wSecond)
//    ENDIF
   // Dos Time Structure is 16 bits:
   // hhhhhmmmmmmsssss
   // 1111110000000000
   // 5432109876543210
   IF FileTimeToDosDateTime(pTime, @wDate, @wTime)
      nSecs   := _AND(wTime , 0b11111)
      nMins   := _AND(wTime >> 5,0b111111)
      nHours  := _AND(wTime >> 11, 0b11111)
      cRet    := ConTime(nHours, nMins, nSecs)
   ENDIF
   RETURN cRet

FUNCTION __GetTimeZoneDiff() AS STRING STRICT

	LOCAL tzi		IS _WINTIME_ZONE_INFORMATION
	LOCAL dwZone	AS DWORD
	LOCAL nDiff		AS INT
	LOCAL cRet		AS STRING

	dwZone := GetTimeZoneInformation(@tzi)

	DO CASE
	CASE dwZone == TIME_ZONE_ID_STANDARD
		nDiff := tzi:Bias/60

	CASE dwZone == TIME_ZONE_ID_DAYLIGHT
		//  UH 08/03/2002
		//  nDiff := tzi.DaylightBias/60
		nDiff := (tzi:Bias/60) + (tzi:DaylightBias/60)

	OTHERWISE
		nDiff := 0
	ENDCASE

	IF nDiff > 0
		nDiff *= 100
		cRet := " -" + StrZero(nDiff, 4, 0)
	ELSE
		nDiff *= -100
		cRet := " +" + StrZero(nDiff, 4, 0)
	ENDIF

	RETURN cRet

FUNCTION __GetToken(cText AS STRING, cStart AS STRING, cStop AS STRING, lStopNotRequired := FALSE AS LOGIC) AS STRING STRICT
	//SE-040627
	LOCAL cRet		AS STRING
	LOCAL dwPos		AS DWORD
	LOCAL dwStop	AS DWORD

	// this function returns all the text between the cStart and cStop strings
	// mostly used to decode HTML or email tags:  <something>, or "something"

	IF cStart == NULL_STRING
		dwPos := 1
	ELSE
		dwPos := AtC(cStart, cText)
	ENDIF

	IF dwPos > 0
		IF (dwStop := At3(cStop, cText, dwPos)) > 0
			dwPos += SLen(cStart)
			cRet  := SubStr3(cText, dwPos, dwStop-dwPos)
		ELSEIF lStopNotRequired
			// return until CRLF or end of text
			dwPos += SLen(cStart)
			IF (dwStop := At3(CRLF, cText, dwPos)) > 0
				cRet  := SubStr3(cText, dwPos, dwStop-dwPos)
			ELSE
				cRet := SubStr2(cText, dwPos)
			ENDIF
		ENDIF
	ENDIF

	RETURN cRet

FUNCTION __GoodFileName(cAttachName AS STRING) AS STRING STRICT
    LOCAL pName AS BYTE PTR
    LOCAL x, len AS INT
    LOCAL b AS BYTE
    // The following characters are not allowed in a filename: \ / ? : * " > < |
    // Replace them with                                       [ ] $ = + ^ ) ( !
    len     := SLen(cAttachName)
	#ifndef VULCAN
    pName   := StringAlloc(cAttachName)
	#endif
    FOR x := 1 TO len
		#ifdef VULCAN
			b := cAttachName[x]
		#else
	        b := pName[x]
		#endif
        DO CASE
        CASE b ==  47   // "/" => "["  // 47 => 91
            b := 91
        CASE b ==  92   // "\" => "]"  // 92 => 93
            b := 93
        CASE b ==  63   // "?" => "$"  // 63 => 36
            b := 36
        CASE b ==  58   // ":" => "="  // 58 => 61
            b := 61
        CASE b ==  42   // "*" => "+"  // 42 => 43
            b := 43
        CASE b ==  34   // '"' => '^'  // 34 => 94
            b := 94
        CASE b ==  62  //  ">" => ")"  // 62 => 41
            b := 41
        CASE b ==  60  //  "<" => "("  // 60 => 40
            b := 40
        CASE b ==  124 //  "|" => "!"  // 124 => 33
            b := 33
        ENDCASE
		#ifdef VULCAN
			cAttachName[x] := b
		#else
	        pName[x] := b
		#endif
    NEXT
	#ifndef VULCAN
    	cAttachName := Mem2String(pName, Len)
	    MemFree(pName)
	#endif
	RETURN cAttachName

FUNCTION __SaveAs(cPath AS STRING, cFile AS STRING, cMail AS STRING) AS DWORD STRICT

	LOCAL nRet      AS DWORD
	LOCAL cFName    AS STRING
	LOCAL nPos      AS DWORD
	LOCAL cTemp     AS STRING
	LOCAL nSize     AS DWORD

	cFName := cPath + cFile

	cTemp := cMail

	nPos := At2(CRLF + CRLF, cTemp)

	IF nPos > 0
		nSize := 4
		cTemp := SubStr2(cTemp, nPos + nSize)
	ENDIF

	IF MemoWrit(cFName, cTemp)
		nRet := SLen(cTemp)
	ENDIF

	RETURN nRet

FUNCTION __StrList2Array(cList AS STRING, cDelimiter := ", " AS STRING) AS ARRAY STRICT
	//SE-040628
	LOCAL dwPos	  AS DWORD
	LOCAL dwStart AS DWORD
	LOCAL dwLen   AS DWORD
	LOCAL aRet	  AS ARRAY

	aRet  := {}

	dwLen := SLen(cDelimiter)

	IF SLen(cList) == 0 .OR. dwLen = 0
		RETURN aRet
	ENDIF

	dwStart := 1
	dwPos   := 0
	DO WHILE TRUE
		IF (dwPos := At3(cDelimiter, cList, dwPos)) > 0
			AAdd(aRet, AllTrim(SubStr3(cList, dwStart, dwPos-dwStart)))
			dwStart := dwPos + dwLen
		ELSE
			AAdd(aRet, AllTrim(SubStr2(cList, dwStart)))
			EXIT
		ENDIF
	ENDDO

	RETURN aRet

FUNCTION __UUDecodeMail  (cPath AS STRING, cFile AS STRING, cMail AS STRING) AS DWORD STRICT
	LOCAL nPos      AS DWORD
	LOCAL nStop     AS DWORD
	LOCAL nRet      AS DWORD
	LOCAL hfOut     AS PTR

	IF SLen(cMail) = 0
		RETURN 0
	ENDIF

	nPos := __CheckUUEncode(cMail)

	IF nPos = 0
		RETURN 0
	ENDIF

	nPos += SLen(UUE_START_EMAIL)
	nStop := At3(CRLF, cMail, nPos)

	IF nStop = 0
		RETURN 0
	ENDIF

	IF SLen(cFile) ==  0
		cFile := SubStr3(cMail, nPos, nStop - nPos)
	ENDIF

	//  UH 15/12/1999
	cFile := __GoodFileName(cFile)
	cFile := __GetFullPath(cPath, cFile)

	hfOut := FCreate(cFile)

	nPos := nStop + 2
	cMail := SubStr2(cMail, nPos)

	IF hfOut != F_ERROR
		nRet := __DecodeUU(cMail, hfOut)
		FClose(hfOut)
	ENDIF

	RETURN nRet

FUNCTION CharSetFromContentType(cContent	AS	 STRING)	AS	 STRING
	LOCAL cChars      AS STRING

	cChars := __GetToken(cContent, TEMP_CHARSET, ";", TRUE)
    cChars := StrTran (cChars, e"\"")	// added this line KB 20-5-2010 because sometimes the charset is between quotes
	IF cChars == NULL_STRING
		cChars := CHARSET_ISO1
	ENDIF

	RETURN cChars

FUNCTION CheckHostIP(cHost AS STRING) AS STRING STRICT
	//
	//  Takes a IP or address string and returns IP string
	//
	// SE - 060704
	LOCAL dwPos AS DWORD
	LOCAL cRet  AS STRING

	dwPos := At2(".", cHost)

	IF dwPos > 0 .AND. Val( Left(cHost, dwPos-1) ) > 0
		cRet := cHost
	ELSE
		cRet := GetIPAddress(cHost)
	ENDIF

	RETURN cRet


//GLOBAL csWSA IS _WINRTL_CRITICAL_SECTION
FUNCTION DecodeMailTimeStamp(cDate AS STRING, dDate REF DATE, cTime REF STRING) AS LOGIC STRICT
	//SE-040702
	LOCAL aMonth  AS ARRAY
	LOCAL cTemp	  AS STRING
	LOCAL dwPos	  AS DWORD
	LOCAL dwDay	  AS DWORD
	LOCAL dwMonth AS DWORD
	LOCAL dwYear  AS DWORD

	IF SLen(cDate) = 0
		RETURN FALSE
	ENDIF

	aMonth := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}

	//	UH 09/07/1999
	IF (dwPos := At2(", ", cDate)) = 0
		dwPos := 1
	ELSE
		dwPos += 1
		cDate := SubStr2(cDate, dwPos)
	ENDIF

	cDate := AllTrim(cDate)

	IF(dwPos := At2(" ", cDate)) = 0
		RETURN FALSE
	ENDIF

	cTemp := SubStr3(cDate, 1, dwPos - 1)
	cDate := SubStr2(cDate, dwPos + 1)
	dwDay := Val(cTemp)

	IF (dwPos := At2(" ", cDate)) = 0
		RETURN FALSE
	ENDIF

	cTemp := SubStr3(cDate, 1, dwPos - 1)
	cDate := SubStr2(cDate, dwPos + 1)

	cTemp := StrTran(cTemp, " ", "")

	IF (dwMonth := AScan(aMonth, cTemp)) = 0
		RETURN FALSE
	ENDIF

	IF (dwPos := At2(" ", cDate)) = 0
		RETURN FALSE
	ENDIF
	cTemp := SubStr3(cDate, 1, dwPos - 1)
	cDate := SubStr2(cDate, dwPos + 1)

	dwYear := Val(cTemp)
	dDate  := ConDate(dwYear, dwMonth, dwDay)

	IF (dwPos := At2(" ", cDate)) = 0
		RETURN FALSE
	ENDIF

	cTemp := SubStr3(cDate, 1, dwPos - 1)
	cTime := StrTran(cTemp, " ", "")

	RETURN SLen(cTime) = 8


FUNCTION GetHostByIP(cAddr AS STRING) AS STRING STRICT
	//
	//  Takes a IP string and returns a address string
	//
	LOCAL IPAddr        AS STRING
	LOCAL i,i1,j        AS DWORD
	LOCAL cRet          AS STRING
	LOCAL DIM bAddr[4]  AS BYTE
	LOCAL pHostEnt      AS _WInHostEnt

	IPAddr := AllTrim(cAddr) + "."

	i:= At2(".", IPAddr)
	IF (i != 0)
		bAddr[1] := Val(Left(IPAddr, i-1))
		FOR j := 2 TO 4
			i1 := At3(".", IPAddr, i+1)
			IF (i == 0)
				RETURN("")
			ENDIF
			bAddr[j] := Val(SubStr(IPAddr, i+1, i1-i-1))
			i := i1
		NEXT
	ELSE
		RETURN("")
	ENDIF

	pHostEnt := gethostbyaddr(@bAddr[1], 4, PF_INET)
	IF (pHostEnt != NULL_PTR)
		cRet := Psz2String(pHostEnt:h_name)
	ELSE
		cRet := ""
	ENDIF

	RETURN(cRet)


FUNCTION GetIPAddress(cHost AS STRING) AS STRING STRICT
	LOCAL cRet      AS STRING
	LOCAL pHostEnt  AS _WInHostEnt
	LOCAL inaddr    IS _winin_addr
	LOCAL pDword    AS DWORD PTR
	IF SLen(cHost) > 0
		IF (pHostEnt := gethostbyname(String2Psz(cHost))) != NULL_PTR
			// h_addr_list is a CHAR PTR PTR and must be deferenced twice
			// to get the 4 bytes of the IP Address
			pDword := pHostEnt:h_addr_list
			pDword := PTR(_CAST, pDword[1])
			inaddr:s_un:s_addr := pDword[1]
			cRet := Psz2String(inet_ntoa(Inaddr))
		ENDIF
	ENDIF
	RETURN cRet


FUNCTION	GetMailTimeStamp(lForceUK AS LOGIC)	AS	STRING STRICT
	LOCAL cRet      AS STRING
	LOCAL d         AS DATE
	LOCAL cTime		AS STRING
	LOCAL nTmp		AS DWORD
	//RvdH 070709 Changed to use hardcoded DOW and MOY strings
	//            in stead of switching the Nation DLLs
	// 	LOCAL lDict     AS LOGIC
	// 	LOCAL lIntl     AS LOGIC
	// 	LOCAL nCountry  AS DWORD
	// 	LOCAL cNation	AS STRING

	// modified by W.Riedman to allow the date format to be forced to
	// English for the transmission of Sent Date.  19/04/2004

	// 	nCountry := SetDateCountry(BRITISH)
	// 	cNation  := GetNatDLL()

	// 	lDict := _SetDict(.F.)
	// 	lIntl := _SetIntl(.F.)
	// 	IF lForceUK
	// 		SetNatDLL("")		// clear the loaded library
	// 	ENDIF

	d := Today()

	IF lForceUK
		nTmp := DoW(d)
		cRet := SubStr("SunMonTueWenThuFriSat",(3*nTmp)-2,3)
	ELSE
		cRet := SubStr3( CDoW(d), 1, 3)
	ENDIF
	cRet += ", "

	cRet += StrZero(Day(d), 2, 0)

	cRet += " "
	IF lForceUk
		nTmp := Month(d)
		cRet += SubStr("JanFebMarAprMayJunJulAugSepOctNovDec",(3*nTmp)-2,3)
	ELSE
		cRet += AllTrim(SubStr(CMonth(d),1,3))
	ENDIF
	cRet += " "
	cRet += AllTrim(Str(Year(d)))

	cTime := Time()

	//  	_SetDict(lDict)
	// 	_SetIntl(lIntl)

	// reset nation dll and date format
	// 	SetDateCountry(nCountry)
	// 	SetNatDLL(cNation)

	cRet := cRet + " " + cTime + __GetTimeZoneDiff()			//" +0100"

	RETURN cRet


FUNCTION GetUseNetMsgID(cUnique AS STRING, cDomain AS STRING)   AS STRING STRICT
	LOCAL cRet      AS STRING
	LOCAL cHost     AS STRING
	LOCAL d         AS DATE
	LOCAL nTemp,i,n AS DWORD
	LOCAL pHostEnt  AS _WInHostEnt

	cHost := HostName()
	IF SLen(cHost) > 0
		pHostEnt := gethostbyname(String2Psz(cHost))
		IF pHostEnt != NULL_PTR
			cHost := Psz2String(pHostEnt:h_name)
		ENDIF
	ENDIF

	IF SLen(cUnique) = 0
		d := Today()
		cUnique := NTrim(Month(d))
		cUnique += NTrim(Day(d))
		cUnique += NTrim(GetTickCount())
	ENDIF

	n := SLen(cDomain)
	IF n = 0
		cDomain := cHost
	ELSE
		FOR i := 1 TO n
			nTemp += Asc(SubStr(cDomain, i, 1))
		NEXT
		cUnique := NTrim(nTemp) + cUnique
	ENDIF

	cRet := "<" + cUnique  + "@" + cDomain + ">"

	RETURN cRet


FUNCTION HostName() AS STRING STRICT
	//SE-040706
	LOCAL cRet    AS STRING
	LOCAL pBuffer AS PSZ

	pBuffer := MemAlloc(260)
	IF pBuffer != NULL_PSZ
		IF gethostname(pBuffer, 260) = 0
			cRet := Psz2String(pBuffer)
		ENDIF
		MemFree(pBuffer)
	ENDIF

	RETURN cRet

FUNCTION IFXFtpFindFirstFile(hConnect AS PTR,;
		lpszFile AS PSZ,;
		lpfd AS _WINWIN32_FIND_DATA, ;
		dwFlags AS DWORD, ;
		dwContext AS DWORD ) AS PTR STRICT
	LOCAL hFind AS PTR

	hFind := FtpFindFirstFile (hConnect, lpszFile, lpfd, dwFlags, dwContext)

	IF hFind != NULL_PTR
		__FixDate(@lpfd:ftLastWriteTime)
	ENDIF

	RETURN hFind


STATIC FUNCTION IFXGetTimeDiff(pstRemote AS _WINSYSTEMTIME, pstLocal AS _WINSYSTEMTIME) AS INT STRICT
	LOCAL nMinute   AS INT
	LOCAL nHours    AS INT
	LOCAL nDays     AS INT
	LOCAL nRet      AS INT

	IF pstRemote:wDay == pstLocal:wDay
		nHours  := INT( pstRemote:wHour)  - INT(pstLocal:wHour  )
		nMinute := INT(pstRemote:wMinute) - INT(pstLocal:wMinute)

		nRet := nHours * 60 + nMinute
	ELSE
		IF pstRemote:wDay > pstLocal:wDay
			IF pstLocal:wDay == 1
				//
				//  Changed to next month
				//
				nDays := -1
			ELSE
				nDays := 1
			ENDIF
		ELSE
			IF pstRemote:wDay == 1
				//
				//  Changed to next month
				//
				nDays := 1
			ELSE
				nDays := -1
			ENDIF
		ENDIF

		nHours  := INT(pstRemote:wHour)   - INT(pstLocal:wHour)
		nMinute := INT(pstRemote:wMinute) - INT(pstLocal:wMinute)
		nRet    := (nDays * 60 * 24) + nHours * 60 + nMinute
	ENDIF

	RETURN nRet


FUNCTION IFXInternetFindNextFile(hFind AS PTR, lpfd AS _WINWIN32_FIND_DATA) AS LOGIC STRICT
	//SE-040706
	IF InternetFindNextFile(hFind, lpfd)
		__FixDate(@lpfd:ftLastWriteTime)
		RETURN TRUE
	ENDIF

	RETURN FALSE

FUNCTION WinSockExit() AS INT STRICT
	LOCAL nRet  AS INT

	IF WSACleanup() != 0
		nRet := INT(WSAGetLastError())

		IF nRet == WSAEINPROGRESS
			WSACancelBlockingCall()
			IF WSACleanup() = 0
				nRet := 0
			ELSE
				nRet := SSTAT_ERRORSTATE
			ENDIF
		ENDIF
	ENDIF

	//  UH 02/11/2000
	//DeleteCriticalSection(@csWSA) //SE - was deleted by comment //

	RETURN nRet



FUNCTION WinSockInit()
	LOCAL cErr  AS STRING
	LOCAL nErr  AS INT

	nErr := WSAStartup( WS_VERSION_REQUIRED, @wsaData)

	IF nErr != 0
		DO CASE
		CASE nErr = WSASYSNOTREADY
			cErr := "System not ready."

		CASE nErr = WSAVERNOTSUPPORTED
			cErr := "Version not supported."

		CASE nErr = WSAEINVAL
			cErr := "Invalid version."
		OTHERWISE
			cErr := "Unreconized error."
		ENDCASE
		MessageBox(NULL_PTR, String2Psz(cErr), String2Psz("Socket ERROR"), MB_OK)
		RETURN FALSE
	ENDIF

	IF wsaData:wVersion != WS_VERSION_REQUIRED
		cErr := "Unable to initialize winsock DLL. Wrong version, "
		cErr += "wanted " + NTrim(WS_VERSION_MAJOR)
		cErr += "." + NTrim(WS_VERSION_MINOR) + ", "
		cErr += "current "
		cErr += NTrim(HiByte(wsaData:wVersion)) + "."
		cErr += NTrim(LoByte(wsaData:wVersion))

		MessageBox(NULL_PTR, String2Psz(cErr), String2Psz("Socket ERROR"), MB_OK)

		RETURN FALSE
	ENDIF

	IF wsaData:iMaxSockets < MIN_SOCKETS_REQUIRED
		cErr := "Not enough available sockets. Wanted "
		cErr += NTrim(MIN_SOCKETS_REQUIRED)
		cErr += ", only " + NTrim(wsaData:iMaxSockets)
		cErr += " available."

		MessageBox(NULL_PTR, String2Psz(cErr), String2Psz("Socket ERROR"), MB_OK)

		RETURN FALSE
	ENDIF

	RETURN TRUE



FUNCTION __Array2StrList(aList AS ARRAY, cDelimiter := ", " AS STRING) AS STRING STRICT
	//SE-040628
	LOCAL cRet    AS STRING
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD

	cRet    := ""
	dwCount := ALen(aList)
	FOR dwI := 1 UPTO dwCount
		cRet += aList[dwI]
		IF dwI < dwCount
			cRet += cDelimiter
		ENDIF
	NEXT  // dwI

	RETURN cRet


PROCEDURE LibInit() _INIT3
	IF WinSockInit()
#ifdef __VULCAN__
       AppDomain.CurrentDomain:ProcessExit += System.EventHandler{ NULL, @WinSockExitHandler() }
#else
        _RegisterExit(@WinSockExit())
#endif
		//  UH 02/11/2000
		//InitializeCriticalSection(@csWSA)
	ENDIF
	RETURN

#ifdef __VULCAN__
STATIC FUNCTION WinSockExitHandler( o AS OBJECT, args AS EventArgs ) AS VOID
   WinSockExit()
   RETURN
#endif


FUNCTION POPGetMails(cServerIP AS STRING, cUser AS STRING, cPassW AS STRING, lDelete AS LOGIC) AS ARRAY STRICT
	LOCAL oPop    AS CPop
	LOCAL aRet    AS ARRAY
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD

	oPop := CPop{cServerIP}

	IF  oPop:Logon(cUser, cPassW)
		oPop:GetStatus()

		dwCount := oPop:MailCount
		aRet    := oPop:ListMail()

		IF lDelete
			// Deleting all messages
			FOR dwI := 1 UPTO dwCount
				oPop:DeleteMail(dwI)
			NEXT  // dwI
		ENDIF
	ENDIF

	oPop:Disconnect()

	RETURN aRet
FUNCTION SMTPSendMail       (cServerIP      AS STRING,;
		cMailSubject   AS STRING,;
		xDestUser      AS USUAL,;
		xCCUser        AS USUAL,;
		cBody          AS STRING,;
		cFromAddress   AS STRING,;
		xAttachFile    AS USUAL,;
		cFromName      AS STRING,;
		xBCCUser       AS USUAL,;
		cCargo         AS STRING)   AS LOGIC STRICT

	LOCAL oSmtp 	AS CSMTP
	LOCAL lRet  	AS LOGIC
	LOCAL oEmail	AS CEmail

	oEMail := CEmail{}

	oEmail:FromAddress   := cFromAddress
	oEmail:FromName      := cFromName
	oEmail:Cargo         := cCargo
	oEmail:Subject       := cMailSubject
	oEmail:DestList      := xDestUser
	oEmail:CCList        := xCCUser
	oEmail:BCCList       := xBCCUser
	oEmail:MailBody	     := cBody

	IF !IsNil(xAttachFile)
		oEmail:AttachmentFileList := xAttachFile
	ENDIF

	oSmtp := CSmtp{oEmail}

	oSMtp:RemoteHost  := cServerIP

	oSmtp:TimeOut := 5000

	lRet := oSmtp:SendMail()

	#IFDEF __DEBUG__
		IF !lRet
			DebOut32(NTrim(oSmtp:Error) + " " + oSmtp:ErrorMsg)
		ENDIF
	#ENDIF

	RETURN lRet

GLOBAL wsaData  	IS _WinWSAData
FUNCTION __AdJustPath(cPath AS USUAL) AS STRING
    // Updated to better handle UNC paths
    IF IsString(cPath)
       IF Right(cPath,  1) != "\"
          cPath += "\"
       ENDIF
    ELSE
       cPath := CurDir() + "\"
       IF ! Left(cPath,2) == "\\"
          cPath := CurDrive() + ":\" + cPath
       ENDIF
    ENDIF
RETURN cPath

#ifdef __VULCAN__
  // Copied from System Library

FUNCTION ConvertFromCodePageToCodePage(cString AS STRING, dwFrom AS DWORD, dwTo AS DWORD) AS STRING
   LOCAL pUStr                  AS PTR
   LOCAL pBuffer        AS PTR
   LOCAL nLen,nULen     AS LONGINT
   // Convert VO string from dynamic memory to fixed memory
   nLen     := LONGINT(SLen(cString))
   pBuffer  := MemAlloc(nLen)
   MemCopyString(pBuffer,cString,nLen)

   // Determine length of Unicode string
   // And allocate enough space to hold it
   nULen    := MultiByteToWideChar(CP_ACP,0,pBuffer,nLen,NULL_PTR, 0)
   pUStr    := SysAllocStringLen(NULL_PTR,nULen)
   // Convert Fixed memory Ansi string to Fixed memory Unicode string
   MultiByteToWideChar(dwFrom,0,pBuffer,nLen,pUStr,nULen)

   // Now determine size needed for ANSI string
   nLen :=      WideCharToMultiByte(dwTo,0,pUStr,nULen,NULL_PTR,0, NULL,NULL)

   // Allocate Fixed memory buffer to hold the UTF8 string
   pBuffer  := MemRealloc(pBuffer, nLen+1)
   // Convert Unicode to Ansi
   nLen  := WideCharToMultiByte(dwTo,0,pUStr,nULen,pBuffer,nLen ,NULL,NULL)

   // Convert fixed memory buffer to dynamic memory string
   cString  := Mem2String(pBuffer,nLen)
   // Release fixed memory buffer
   MemFree(pBuffer)
   // Release the Unicode String
   SysFreeString(pUStr)

   RETURN cString

#endif

