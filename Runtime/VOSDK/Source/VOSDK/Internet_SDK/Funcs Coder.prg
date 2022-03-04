INTERNAL DEFINE MAXLINELENGTH := 74U // without ending CRLF


INTERNAL FUNCTION __B64DecChar(bByte AS BYTE) AS BYTE STRICT
   //CODE:     0-25  26-51  52-61  62  63
   //Zeichen:   A-Z   a-z    0-9    +   /


   IF bByte >= 48    // '0'
      IF bByte <= 57 // '9'
         RETURN (BYTE)(bByte + 4)
      ELSEIF bByte >= 65 // 'A'
         IF bByte <= 90  // 'Z'
            RETURN (BYTE)(bByte - 65)
         ELSEIF bByte >= 97 // 'a'
            IF bByte <= 122 // 'z'
               RETURN (BYTE) (bByte - 71)
            ENDIF
         ENDIF
      ENDIF
   ELSEIF bByte = 43  // '+'
      RETURN 62
   ELSEIF bByte = 47  // '/'
      RETURN 63
   ENDIF


   //BREAK


   RETURN 0x80
INTERNAL FUNCTION __B64Enc6Bits(dwCode AS DWORD) AS BYTE STRICT
   //CODE:     0-25  26-51  52-61  62  63
   //Zeichen:   A-Z   a-z    0-9    +   /


   dwCode := _AND(dwCode, 63u)

   IF dwCode < 26
      RETURN (BYTE)(dwCode + 65) //A
   ELSEIF dwCode < 52
      RETURN (BYTE)(dwCode + 71) //a
   ELSEIF dwCode < 62
      RETURN (BYTE)(dwCode - 4)  //0
   ELSEIF dwCode = 62
      RETURN 43 //+
   ENDIF
   RETURN 47 // /
INTERNAL FUNCTION __CheckQPEncodeWord(cText AS STRING, dwMaxChars REF DWORD, dwPos REF DWORD) AS LOGIC STRICT
   LOCAL bChar   AS BYTE
   LOCAL pChar   AS BYTE PTR
   LOCAL dwEnd   AS DWORD
   LOCAL dwLineLen AS DWORD
   LOCAL dwInc   AS DWORD
   LOCAL lEncoded AS LOGIC


	dwEnd     := SLen(cText)
   dwPos     := 1
   dwLineLen := 0
	lEncoded  := FALSE
	pChar     := String2Psz(cText)


	// Length calculation
	DO WHILE dwPos <= dwEnd
      bChar := pChar[dwPos]
      IF bChar = 61 .OR. bChar = 63 .OR. bChar > 126 .OR. bChar < 32 .OR. bChar = 95
         dwInc := 3
         lEncoded := TRUE
      ELSE
         dwInc := 1
         IF bChar = 32
            lEncoded := TRUE
         ENDIF
      ENDIF


      IF dwLineLen + dwInc > dwMaxChars
         EXIT
      ENDIF


      dwLineLen += dwInc
      dwPos++
	ENDDO


	dwMaxChars := dwLineLen


	dwPos--


	RETURN lEncoded
 /// <exclude />
FUNCTION __DecodeAtom(cValue AS STRING, lDecoded REF LOGIC) AS STRING STRICT
   LOCAL dwPos    AS DWORD
   LOCAL cCharSet AS STRING


   cCharSet := Lower(Left(cValue, 15))
   lDecoded := FALSE
   IF (dwPos := At2("?q?", cCharSet)) > 0
      cValue := QPDecode(SubStr2(cValue, dwPos+3), TRUE)
      lDecoded := TRUE
   ELSEIF (dwPos := At2("?b?", cCharSet)) > 0
      cValue := B64DecodeString(SubStr2(cValue, dwPos+3))
      lDecoded := TRUE
   ENDIF


   IF cCharSet = CHARSET_ISO .OR. cCharSet = CHARSET_USASCII
      RETURN cValue
   ELSEIF cCharSet = CHARSET_UTF8
      RETURN ConvertFromCodePageToCodePage(cValue, CP_UTF8, CP_ACP)
   ENDIF


   IF lDecoded
      RETURN "<" + SubStr3(cValue, 1, dwPos-1) + "> " + cValue
   ENDIF


   RETURN cValue


 /// <exclude />
FUNCTION __DecodeField(cValue AS STRING, lFoldedEncode REF LOGIC) AS STRING STRICT
   //see RFC 2047
   LOCAL dwPos    AS DWORD
   LOCAL dwStart  AS DWORD
   LOCAL dwEnd    AS DWORD
   LOCAL cRet     AS STRING
   LOCAL cTemp    AS STRING
   LOCAL lDecoded AS LOGIC


   dwStart := 1
   dwPos   := 0
   dwEnd   := SLen(cValue)


   DO WHILE dwStart <= dwEnd // changed from < to <= because of wrong decoding of 1 byte long fields.
      IF (dwPos := At3("=?", cValue, dwStart-1)) > 0
         IF dwPos > dwStart
            cRet += SubStr3(cValue, dwStart, dwPos - dwStart)
         ENDIF
         dwStart := dwPos + 2
         IF (dwPos := At3("?=", cValue, dwPos+1)) > 0
            IF lFoldedEncode .AND. (cRet == " " .OR. cRet == TAB)
               cRet := NULL_STRING //delete leading space for encoded folded line
            ENDIF
            cTemp := __DecodeAtom(SubStr3(cValue, dwStart, dwPos - dwStart), @lDecoded)
            IF lDecoded
               cRet += cTemp
            ELSE
               cRet += "=?" + cTemp + "?="
            ENDIF
            cTemp := NULL_STRING
            dwStart := dwPos + 2
            lFoldedEncode := (dwStart >= dwEnd)
         ELSE
            cRet := "=?" + cRet
            lFoldedEncode := FALSE
         ENDIF
      ELSE
         cRet += SubStr2(cValue, dwStart)
         lFoldedEncode := FALSE
         EXIT
      ENDIF
   ENDDO


   RETURN cRet
 /// <exclude />
FUNCTION __EncodeField(cValue AS STRING, dwCurrentLineLen AS DWORD) AS STRING STRICT
   //see RFC 2047
   LOCAL dwMaxlength    AS DWORD
   LOCAL dwLen          AS DWORD
   LOCAL dwLength       AS DWORD
   LOCAL dwDelimiterLen AS DWORD
   LOCAL cText          AS STRING
   LOCAL cResult        AS STRING
   LOCAL cCharset       AS STRING
   LOCAL lEncodeIt      AS LOGIC


   dwMaxlength := 1000


   lEncodeIt := __CheckQPEncodeWord(cValue, @dwMaxlength, @dwLen)


   dwMaxlength := MAXLINELENGTH - dwCurrentLineLen


   IF ! lEncodeIt .AND. dwMaxlength >= SLen(cValue)
      RETURN cValue
   ENDIF


   cCharset       := "=?"+CHARSET_ISO+"1?Q?"
   dwDelimiterLen := SLen(cCharset) + 2 // + 2 is for the ending "?="


   IF MAXLINELENGTH <= dwDelimiterLen + dwCurrentLineLen
      RETURN CRLF + " " + __EncodeField(cValue, 1)
   ENDIF


   dwMaxlength := MAXLINELENGTH - dwCurrentLineLen - dwDelimiterLen


   cResult := NULL_STRING
   DO WHILE TRUE
      dwLength  := SLen(cValue)
      lEncodeIt := __CheckQPEncodeWord(cValue, @dwMaxlength, @dwLen)
      IF dwLen = dwLength
         cText := cValue
      ELSE
         cText := SubStr3(cValue, 1, dwLen)
      ENDIF
      IF lEncodeIt
         cText := __QPEncodeWord(cText, dwMaxlength)
      ENDIF
      cResult += cCharset + cText + "?="
      IF dwLen = dwLength
         EXIT
      ENDIF
      cResult += CRLF + " "
      dwMaxlength := MAXLINELENGTH - dwDelimiterLen - 1
      cValue := SubStr2(cValue, dwLen+1)
   ENDDO


   RETURN cResult




 /// <exclude />
FUNCTION __FormatAddress(cAddress AS STRING, cName AS STRING) AS STRING STRICT
   cAddress := "<" + cAddress + ">"
   IF ! Empty(cName)
      cAddress := e"\"" + cName + e"\" " + cAddress
   ENDIF
   RETURN  cAddress
 /// <exclude />
FUNCTION __GetAddressList(cList AS STRING) AS ARRAY STRICT
   LOCAL dwPos    AS DWORD
   LOCAL aList    AS ARRAY
   LOCAL cAddress AS STRING


   dwPos := 1
   aList := NULL_ARRAY


   DO WHILE dwPos > 0
      cAddress := __GetNextAddress(cList, @dwPos)
      IF ! Empty(cAddress)
         cAddress := __ParseAddress(cAddress, OUT VAR cName)
         IF aList = NULL_ARRAY
            aList := {}
         ENDIF
         AAdd(aList, __FormatAddress(cAddress, cName))
      ENDIF
   ENDDO


   RETURN aList




 /// <exclude />
FUNCTION __GetMailInfo(cSection AS STRING, cToken AS STRING, lRemoveSpace AS LOGIC)  AS STRING STRICT
   // cSection should begin with the first charactor of a line, otherwise a correct token search can't be granted.
   // cToken must be found a the beginning of a line.
   // returns the string after the token until the last CRLF ended line, which last character
   // is no ';'.
   // lRemoveSpace removes all spaces and tabs and replaces all CRLF with a single space.
   // the result string has no leading or trailing spaces.
	LOCAL cRet	 AS STRING
	LOCAL dwPos	 AS DWORD
	LOCAL dwStop AS DWORD
	LOCAL dwEnd  AS DWORD
	LOCAL bChar  AS BYTE
	LOCAL pChar  AS BYTE PTR
	LOCAL lFoldedEncode AS LOGIC


	IF Lower(Left(cSection, SLen(cToken))) == Lower(cToken)
      dwPos := 1
   ELSE
      cToken := CRLF+cToken
      dwPos  := AtC2(cToken, cSection)
   ENDIF


	IF dwPos > 0
	   dwEnd := SLen(cSection)
	   dwPos += SLen(cToken)
	   // skip leading spaces
	   pChar := String2Psz( cSection)
	   DO WHILE dwPos <= dwEnd
	      bChar := pChar[dwPos]
	      IF bChar = 9 .OR. bChar = 32
                  dwPos++
	      ELSE
	         EXIT
	      ENDIF
	   ENDDO
	   // unfold lines
	   lFoldedEncode := FALSE
	   DO WHILE TRUE
         IF (dwStop := At3(CRLF, cSection, dwPos-1)) > 0
            cRet  += __DecodeField(SubStr3(cSection, dwPos, dwStop - dwPos), @lFoldedEncode)
            dwPos := dwStop + 2
            pChar := String2Psz( cSection)
            DO WHILE dwPos <= dwEnd
      	      bChar := pChar[dwPos]
      	      IF bChar = 9 .OR. bChar = 32
                  dwPos++
      	      ELSE
      	         EXIT
      	      ENDIF
            ENDDO
            IF dwPos > dwStop + 2 //This is a folded line
               dwPos-- // one space back
            ELSE
               EXIT
            ENDIF
         ELSE
            cRet += __DecodeField(SubStr2(cSection, dwPos), @lFoldedEncode)
            EXIT
         ENDIF
      ENDDO


		IF lRemoveSpace
		   cRet := StrTran(cRet, " ", "")
			cRet := StrTran(cRet, TAB, "")
			//cRet := StrTran(cRet, CRLF, " ")
		ENDIF
	ENDIF


	RETURN cRet //AllTrim(cRet)


 /// <exclude />
FUNCTION __GetNextAddress(cBuffer AS STRING, dwPosition REF DWORD) AS STRING STRICT
    LOCAL lEscaped       AS LOGIC
    LOCAL lQuoted        AS LOGIC
    LOCAL lQuotedRFC2047 AS LOGIC
    LOCAL lInRouteAddr   AS LOGIC
    LOCAL lIsGroup       AS LOGIC
    LOCAL dwPos          AS DWORD
    LOCAL dwStart        AS DWORD
    LOCAL dwEnd          AS DWORD
    LOCAL pBuffer        AS BYTE PTR
    LOCAL bChar          AS BYTE


	dwPos   := dwPosition
	dwEnd   := SLen(cBuffer)
	pBuffer := String2Psz(cBuffer)


	DO WHILE dwPos <= dwEnd .AND. IsSpace(PSZ(_CAST, pBuffer + dwPos - 1))
		++dwPos
	ENDDO


	dwStart := dwPos


	DO WHILE dwPos <= dwEnd
	    LOCAL done := FALSE AS LOGIC
		IF lEscaped
			lEscaped := FALSE
		ELSE
		   bChar := pBuffer[dwPos]
		   SWITCH bChar
   			CASE 92 // '\'
   				  lEscaped := TRUE
   			CASE 34 // '"'
   				  lQuoted := ! lQuoted
   			CASE 60 // '<'
   				  lInRouteAddr := TRUE
   			CASE 62 // '>'
   				  lInRouteAddr := FALSE
   			CASE 61 // '='
      			  IF dwPos < dwEnd .AND. pBuffer[dwPos + 1] = 63 // '?'
      					++dwPos
      					lQuotedRFC2047 := TRUE
      			  ENDIF
   			CASE 63 // '?'
                 IF lQuotedRFC2047 .AND. dwPos < dwEnd .AND. pBuffer[dwPos + 1] = 61 // '='
   					  ++dwPos
   					  lQuotedRFC2047 := FALSE
                 ENDIF
			   OTHERWISE
   				  IF ! lQuoted .AND. ! lQuotedRFC2047 .AND. ! lInRouteAddr
   				        SWITCH bChar
                        CASE 59 // ';'
                            IF lIsGroup
                                IF dwPos < dwEnd .AND. pBuffer[dwPos + 1] = 44 // ','
                                    ++dwPos
                                ENDIF
                            ENDIF
                            done := TRUE
                        CASE 58 // ':'
                            lIsGroup := TRUE
                        CASE 44 // ','
                            IF ! lIsGroup
                                done := TRUE
                            ENDIF
      				  END SWITCH
                 ENDIF
			END SWITCH
		ENDIF
        IF done
            EXIT
        ENDIF
		++dwPos
   ENDDO


   IF dwPos >= dwEnd
		dwPosition := 0
	ELSE
		dwPosition := dwPos + 1  // ',' or ';'
	ENDIF


	// Parse extracted address (mailbox or group)
	IF dwPos > dwStart
	   RETURN SubStr3(cBuffer, dwStart, dwPos - dwStart)
   ENDIF


	RETURN NULL_STRING


 /// <exclude />
FUNCTION __ParseAddress(cBuffer AS STRING, cName OUT STRING) AS STRING STRICT
   //  RFC 1036: " ... three permissible forms are:
	//                  From: mark@cbosgd.ATT.COM
	//                  From: mark@cbosgd.ATT.COM (Mark Horton)
	//                  From: Mark Horton <mark@cbosgd.ATT.COM> ..."
	//
	//  There are more than this because many mail clients do not conform to RFC 1036
	//  IE6 can interpret them so we must too...
	//  "'Mark Horton'"
	//  "Mark Horton"
	//  'Mark Horton' ....etc is common. If you find other formats, add them here...
	LOCAL lEscaped       AS LOGIC
	LOCAL lQuoted        AS LOGIC
	LOCAL bQuoteEnd      AS BYTE
	LOCAL dwPos          AS DWORD
   LOCAL dwStart        AS DWORD
   LOCAL dwEnd          AS DWORD
   LOCAL pBuffer        AS BYTE PTR
   LOCAL bChar          AS BYTE
   LOCAL cAtom          AS STRING
   LOCAL lAddress       AS LOGIC
   LOCAL cAddress       AS STRING
   LOCAL dwLast         AS DWORD
   LOCAL lDecoded       AS LOGIC


   pBuffer := String2Psz(cBuffer)
	dwPos   := 1
	dwEnd   := SLen(cBuffer)
	cName   := NULL_STRING


	dwStart := dwPos


	DO WHILE dwPos <= dwEnd


		IF lEscaped
			lEscaped := FALSE
			dwLast := dwPos
		ELSE
		   bChar := pBuffer[dwPos]
		   DO CASE
   			CASE bChar = 92 // '\'
   				  lEscaped := TRUE
   			CASE bChar = 64 // '@'
                 lAddress := TRUE
   			CASE lQuoted
	              IF bChar = bQuoteEnd
	                 dwLast := dwPos
	                 IF bQuoteEnd = 63
                       IF dwPos < dwEnd .AND. pBuffer[dwPos + 1] = 61 // '='
                          lQuoted := FALSE
                          ++dwPos
                       ENDIF
                    ELSE
                       lQuoted := FALSE
                    ENDIF
                    IF ! lQuoted
                       cAtom := SubStr3(cBuffer, dwStart, dwLast - dwStart)
                       dwStart := dwPos + 1
                    ENDIF
                 ENDIF
	        CASE bChar = 40 // '('
	              dwStart   := dwPos + 1
	              lQuoted   := TRUE
                 bQuoteEnd := 41
   			CASE bChar = 34 .OR. bChar = 39 // '"' .or. "'"
   			     dwStart   := dwPos + 1
   			     lQuoted   := TRUE
                 bQuoteEnd := bChar
   			CASE bChar = 60 // '<'
   			     dwStart   := dwPos + 1
   			     lQuoted   := TRUE
                 bQuoteEnd := 62 // '>'
   			CASE bChar = 61 // '='
      			IF dwPos < dwEnd .AND. pBuffer[dwPos + 1] = 63 // '?'
      				++dwPos
      				dwStart   := dwPos + 1
      				lQuoted   := TRUE
      				bQuoteEnd := 63 // '?'
      			ENDIF
			OTHERWISE
			    dwLast := dwPos
			    DO WHILE dwPos <= dwEnd .AND. IsSpace(PSZ(_CAST, pBuffer + dwPos - 1))
               		++dwPos
                ENDDO
	            IF dwLast < dwPos
                    IF dwStart < dwLast
                        cAtom := SubStr3(cBuffer, dwStart, dwLast - dwStart)
                    ENDIF
                    dwStart := dwPos
                    --dwPos
                ENDIF
			ENDCASE


      ENDIF


      IF dwPos >= dwEnd .AND. ! lQuoted
         dwLast := dwEnd + 1
         IF dwStart < dwLast
            cAtom := SubStr3(cBuffer, dwStart, dwLast - dwStart)
         ENDIF
      ENDIF


      IF ! cAtom == NULL_STRING
	      dwStart := dwPos + 1
	      IF lAddress
	         lAddress := FALSE
	         IF Empty(cAddress)
               cAddress := cAtom
            ENDIF
         ELSE
            IF cName == NULL_STRING
               cName := __DecodeAtom(cAtom, @lDecoded)
            ELSE
               cName += " " + __DecodeAtom(cAtom, @lDecoded)
            ENDIF
         ENDIF
         cAtom := NULL_STRING
      ENDIF


		++dwPos
   ENDDO


	RETURN cAddress
INTERNAL FUNCTION __QPEncodeWord(cText AS STRING, dwResultlen AS DWORD) AS STRING STRICT
   //RFC-2047 page 6
   STATIC LOCAL cTab := "0123456789ABCDEF" AS STRING
   LOCAL cResult AS STRING
   LOCAL pResult AS BYTE PTR
   LOCAL pTab    AS BYTE PTR
   LOCAL pChar   AS BYTE PTR
   LOCAL bChar   AS BYTE
   LOCAL dwChar  AS DWORD
	LOCAL dwPos   AS DWORD
   LOCAL dwRPos  AS DWORD
   LOCAL dwEnd   AS DWORD


	IF (dwEnd := SLen(cText)) = 0
      RETURN cText
	ENDIF


	IF (pResult := MemAlloc(dwResultlen)) != NULL_PTR


      pTab  := String2Psz( cTab)
      pChar := String2Psz( cText)


      dwRPos := dwPos := 1
   	DO WHILE dwPos <= dwEnd
         bChar := pChar[dwPos]
         IF bChar = 61 .OR. bChar = 63 .OR. bChar > 126 .OR. bChar < 32 .OR. bChar = 95 // '_'
            pResult[dwRPos]   := 0x3D // '='
            dwChar := bChar
            pResult[dwRPos+2] := pTab[_AND(dwChar, 0xF)+1]
            dwChar := dwChar >> 4
            pResult[dwRPos+1] := pTab[dwChar+1]
            dwRPos += 3
         ELSE
            IF bChar = 32
               bChar := 95 // '_'
            ENDIF
            pResult[dwRPos] := bChar
            dwRPos += 1
         ENDIF


         dwPos++
   	ENDDO


   	cResult := Mem2String(pResult, dwResultlen)
   	MemFree(pResult)


	ENDIF


	RETURN cResult
/// <include file="Internet.xml" path="doc/B64Decode/*" />
FUNCTION B64Decode(pSrc AS PTR, pTarget AS PTR, dwLength AS DWORD, dwCharCount REF DWORD) AS DWORD STRICT
	LOCAL pS      AS BYTE PTR
   LOCAL pT      AS BYTE PTR
   LOCAL pByte   AS BYTE PTR
   LOCAL dwEnd   AS DWORD
   LOCAL dwChunk AS DWORD
   LOCAL dwJ     AS DWORD
   LOCAL bByte   AS BYTE


   pS    := pSrc
   pT    := pTarget


   pByte := (BYTE PTR) @dwChunk


   dwEnd := DWORD(_CAST, pSrc) + dwLength - 1


   BEGIN SEQUENCE


   DO WHILE DWORD(_CAST, pS) <= dwEnd
       dwChunk  := 0
       dwJ      := 0
       DO WHILE dwJ < 4
          IF DWORD(_CAST, pS) > dwEnd
             IF dwJ > 0
                BREAK
             ENDIF
             EXIT
          ENDIF
          bByte := BYTE(pS++)
          IF bByte = 13 .OR. bByte = 10 .OR. bByte = 61 // '='
             IF bByte = 61 // '='
                IF dwJ < 2
                   BREAK
                ELSE
                   IF dwJ = 2
                      bByte := 13
                      DO WHILE DWORD(_CAST, pS) <= dwEnd .AND. (bByte = 13 .OR. bByte = 10)
                         bByte := BYTE(pS++)
                      ENDDO
                      IF bByte != 61
                         BREAK
                      ENDIF
                      dwChunk := dwChunk << 12
                   ELSEIF dwJ > 2
                      dwChunk := dwChunk << 6
                   ENDIF
                ENDIF
                EXIT
             ELSE
                LOOP
             ENDIF
          ENDIF
          IF (bByte := __B64DecChar(bByte)) = 0x80
             LOOP
          ENDIF
          dwChunk  := _OR(dwChunk << 6, bByte)
          dwJ+=1
       ENDDO


       IF dwJ>1
          BYTE(pT++) := pByte[3]
          IF dwJ > 2
             BYTE(pT++) := pByte[2]
             IF dwJ > 3
                BYTE(pT++) := pByte[1]
             ENDIF
          ENDIF
       ENDIF
       ENDDO
   RECOVER


    dwLength := 0


   END


   dwLength    := DWORD(_CAST, pS) - DWORD(_CAST, pSrc)
   dwCharCount += DWORD(_CAST, pT) - DWORD(_CAST, pTarget)


   RETURN dwLength


/// <include file="Internet.xml" path="doc/B64DecodeString/*" />
FUNCTION B64DecodeString(cValue AS STRING) AS STRING STRICT
    LOCAL dwSize      AS DWORD
    LOCAL dwCharCount AS DWORD
    LOCAL pBuffer     AS PTR
    LOCAL cRet        AS STRING


    IF (dwSize := SLen(cValue)) > 0


       pBuffer := MemAlloc((dwSize / 4) * 3)


       IF pBuffer != NULL_PTR
          dwCharCount := 0
          IF B64Decode(String2Psz(cValue), pBuffer, dwSize, @dwCharCount) > 0
             cRet := Mem2String(pBuffer, dwCharCount)
          ENDIF
          MemFree(pBuffer)
       ENDIF


    ENDIF


    RETURN cRet


/// <include file="Internet.xml" path="doc/B64Encode/*" />
FUNCTION B64Encode(pSrc AS PTR, pTarget AS PTR, dwLength AS DWORD, dwCharCount REF DWORD) AS VOID STRICT
	LOCAL pS AS BYTE PTR
   LOCAL pT AS BYTE PTR
   LOCAL pByte   AS BYTE PTR
   LOCAL dwChunk AS DWORD
   LOCAL dwCount AS DWORD
   LOCAL dwI     AS DWORD
   LOCAL dwJ     AS DWORD
   LOCAL DIM aChar[4] AS BYTE


   dwCount := dwLength / 3


   pS      := pSrc
   pT      := pTarget
   pByte := (BYTE PTR) @dwChunk


   dwChunk := 0


   FOR dwI := 1 UPTO dwCount
       pByte[3] := BYTE(pS++)
       pByte[2] := BYTE(pS++)
       pByte[1] := BYTE(pS++)
       aChar[4] := __B64Enc6Bits(dwChunk)
       aChar[3] := __B64Enc6Bits(dwChunk >> 6)
       aChar[2] := __B64Enc6Bits(dwChunk >> 12)
       aChar[1] := __B64Enc6Bits(dwChunk >> 18)


       FOR dwJ := 1 UPTO 4
           BYTE(pT++) := aChar[dwJ]
           IF ++dwCharCount = 76
              BYTE(pT++)  := 13 //CR
              BYTE(pT++)  := 10 //LF
              dwCharCount := 0
           ENDIF
       NEXT  // dwJ
   NEXT  // dwI


   dwCount := dwLength % 3


   IF dwCount > 0
      dwChunk  := 0
      aChar[4] := 61 // '='
      IF dwCount = 1
         pByte[3] := BYTE(pS++)
         aChar[3] := 61 // '='
      ELSE
         pByte[3] := BYTE(pS++)
         pByte[2] := BYTE(pS++)
         aChar[3] := __B64Enc6Bits(dwChunk >> 6)
      ENDIF
      aChar[2] := __B64Enc6Bits(dwChunk >> 12)
      aChar[1] := __B64Enc6Bits(dwChunk >> 18)


      FOR dwJ := 1 UPTO 4
          BYTE(pT++) := aChar[dwJ]
          IF ++dwCharCount = 76
             BYTE(pT++)  := 13 //CR
             BYTE(pT++)  := 10 //LF
             dwCharCount := 0
          ENDIF
       NEXT  // dwJ
   ENDIF


	RETURN




/// <include file="Internet.xml" path="doc/B64EncodeStream/*" />
FUNCTION B64EncodeStream(cValue AS STRING, dwCharCount REF DWORD) AS STRING STRICT
    LOCAL dwSize AS DWORD
    LOCAL dwBufSize AS DWORD
    LOCAL pBuffer AS PTR
    LOCAL cRet AS STRING


    dwSize    := SLen(cValue)
    IF dwSize > 0


       dwBufSize := ((dwSize + 2) / 3) * 4
       dwBufSize := dwBufSize + ((dwCharCount + dwBufSize) / 76) * 2


       IF (pBuffer := MemAlloc(dwBufSize)) != NULL_PTR


          B64Encode(String2Psz(cValue), pBuffer, dwSize, @dwCharCount)


          cRet := Mem2String(pBuffer, dwBufSize)


          MemFree(pBuffer)
       ENDIF
    ENDIF


    RETURN cRet


/// <include file="Internet.xml" path="doc/B64EncodeString/*" />
FUNCTION B64EncodeString(cValue AS STRING) AS STRING STRICT
    LOCAL dwSize AS DWORD
    LOCAL dwBufSize AS DWORD
    LOCAL dwCharCount AS DWORD
    LOCAL pBuffer AS PTR
    LOCAL cRet AS STRING


    dwSize    := SLen(cValue)
    IF dwSize > 0
       dwBufSize := ((dwSize + 2) / 3) * 4
       dwBufSize := dwBufSize + (dwBufSize / 76) * 2


       IF (pBuffer := MemAlloc(dwBufSize)) != NULL_PTR


          B64Encode(String2Psz(cValue), pBuffer, dwSize, @dwCharCount)


          cRet := Mem2String(pBuffer, dwBufSize)


          MemFree(pBuffer)
       ENDIF
    ENDIF


    RETURN cRet


/// <include file="Internet.xml" path="doc/QPDecode/*" />
FUNCTION QPDecode(cText AS STRING, lDecodeSpace := FALSE AS LOGIC) AS STRING STRICT
   LOCAL pText  AS BYTE PTR
   LOCAL bChar  AS BYTE
   LOCAL bChar2 AS BYTE
	LOCAL dwPos  AS DWORD
   LOCAL dwRPos AS DWORD
   LOCAL dwEnd  AS DWORD


	pText := String2Psz(cText)
   dwEnd := SLen(cText)
   dwPos := 1
   dwRPos := 0


	DO WHILE dwPos <= dwEnd
	   bChar := pText[dwPos]
	   IF bChar = 0x3D // '='
         bChar  := pText[dwPos+1]
         bChar2 := pText[dwPos+2]
         dwPos  += 3
         IF ! (bChar = 0x0D .AND. bChar2 = 0x0A) //CRLF
            IF bChar >= 0x40 // 'A'
         		bChar -= 55
         	ELSE
         		bChar -= 48
         	ENDIF
            IF bChar < 16
               bChar := bChar << 4
               IF bChar2 >= 0x40 // 'A'
            		bChar2 -= 55
            	ELSE
            		bChar2 -= 48
            	ENDIF
               IF bChar2 < 16
                  bChar += bChar2
                  dwRPos++
                  pText[dwRPos] := bChar
               ENDIF
            ENDIF
         ENDIF
      ELSE
         dwRPos++
         IF lDecodeSpace .AND. bChar = 95 //'_'
            pText[dwRPos] := 32 //' '
         ELSEIF dwPos > dwRPos
            pText[dwRPos] := bChar
         ENDIF
         dwPos++
      ENDIF
	ENDDO


   IF dwRPos > dwEnd
      dwRPos := dwEnd
   ENDIF


   cText := Mem2String(pText, dwRPos)


	RETURN cText


/// <include file="Internet.xml" path="doc/QPEncode/*" />
FUNCTION QPEncode(cText AS STRING, lEncodeSpace := FALSE AS LOGIC) AS STRING STRICT
    STATIC LOCAL cTab := "0123456789ABCDEF" AS STRING
    LOCAL cResult AS STRING
    LOCAL pChar   AS BYTE PTR
    LOCAL pResult AS BYTE PTR
    LOCAL pTab    AS BYTE PTR
    LOCAL bChar   AS BYTE
    LOCAL dwChar  AS DWORD
    LOCAL dwPos   AS DWORD
    LOCAL dwRPos  AS DWORD
    LOCAL dwEnd   AS DWORD
    LOCAL dwLineLen AS DWORD
    LOCAL dwInc   AS DWORD


    IF (dwEnd := SLen(cText)) = 0
        RETURN cText
    ENDIF


    cResult   := NULL_STRING
    dwRPos    := 0
    dwPos     := 1
    dwLineLen := 0
    pChar     := Cast2Psz( cText)


    // Length calculation
    DO WHILE dwPos <= dwEnd
        bChar := pChar[dwPos]
        dwInc := 1
        IF bChar = 9 .OR. bChar = 32 // TAB or Space  (White space)
            // IF start of line or end of string or last character before CRLF
            IF dwLineLen = 0 .OR. (dwPos = dwEnd .AND. dwPos + 1 < dwEnd .AND. pChar[dwPos+1] = 13 .AND. pChar[dwPos+2] = 10)
                dwInc := 3
            ENDIF
        ELSEIF bChar = 13 .AND. dwPos < dwEnd .AND. pChar[dwPos+1] = 10
            dwLineLen := 0
            dwPos     += 2
            dwRPos    += 2
            LOOP
        ELSEIF bChar = 61 .OR. bChar > 126 .OR. bChar < 33 .OR. (lEncodeSpace .AND. bChar = 95)
            dwInc := 3
        ENDIF


        IF dwLineLen + dwInc > 75
            dwLineLen := 0
            dwRPos    += 3
        ENDIF


        dwRPos    += dwInc
        dwLineLen += dwInc
        dwPos++
    ENDDO


    IF dwRPos = dwEnd //no encoding needed
        RETURN cText
    ENDIF


    IF (pResult := MemAlloc(dwRPos)) != NULL_PTR


        pTab      := Cast2Psz(cTab)


        dwRPos    := 1 //have to be decremented at the end
        dwLineLen := 0
        dwPos     := 1


        DO WHILE dwPos <= dwEnd
            bChar := pChar[dwPos]
            dwInc := 1
            IF bChar = 9 .OR. bChar = 32  // TAB or Space
                // If start of line or end of string or last character befor CRLF
                IF dwLineLen = 0 .OR. (dwPos = dwEnd .AND. dwPos + 1 < dwEnd .AND. pChar[dwPos+1] = 13 .AND. pChar[dwPos+2] = 10)
                    dwInc := 3
                ENDIF
            ELSEIF bChar = 13 .AND. dwPos < dwEnd .AND. pChar[dwPos+1] = 10
                pResult[dwRPos]   := 0x0D // CR
                pResult[dwRPos+1] := 0x0A // LF
                dwLineLen := 0
                dwPos     += 2
                dwRPos    += 2
                LOOP
            ELSEIF bChar = 61 .OR. bChar > 126 .OR. bChar < 33 .OR. (lEncodeSpace .AND. bChar = 95)
                dwInc := 3
            ENDIF


            IF dwLineLen + dwInc > 75
                pResult[dwRPos]   := 0x3D // '='
                pResult[dwRPos+1] := 0x0D // CR
                pResult[dwRPos+2] := 0x0A // LF
                dwLineLen := 0
                dwRPos    += 3
            ENDIF


            IF dwInc = 3
                pResult[dwRPos]   := 0x3D // '='
                dwChar := bChar
                pResult[dwRPos+2] := pTab[_AND(dwChar, 0xF)+1]
                dwChar := dwChar >> 4
                pResult[dwRPos+1] := pTab[dwChar+1]
            ELSE
                IF lEncodeSpace .AND. bChar = 32
                    bChar := 95 // '_'
                ENDIF
                pResult[dwRPos] := bChar
            ENDIF


            dwRPos    += dwInc
            dwLineLen += dwInc
            dwPos++
        ENDDO


        cResult := Mem2String(pResult, dwRPos-1)
        MemFree(pResult)


    ENDIF


    RETURN cResult


 /// <exclude />
FUNCTION __CreateAddressList(cSection AS STRING, aList AS ARRAY, lSectionAlways := FALSE AS LOGIC) AS STRING STRICT
   LOCAL dwI       AS DWORD
   LOCAL dwCount   AS DWORD
   LOCAL cList     AS STRING
   LOCAL cAddress  AS STRING
   LOCAL dwLineNen AS DWORD


   dwLineNen := SLen(cSection) + 1


   dwCount := ALen(aList)
   FOR dwI := 1 UPTO dwCount
       cAddress := __ParseAddress(aList[dwI],  OUT VAR cName)
       IF ! cAddress == NULL_STRING
          cAddress := "<" + cAddress + ">"
          dwLineNen += SLen(cAddress) + 1
          cName := __EncodeField(cName, dwLineNen)
          IF ! cName == NULL_STRING
             cAddress := cName + " " + cAddress
          ENDIF
          IF dwI < dwCount
             cList += cAddress + "," + CRLF + TAB // fold addresslist
             dwLineNen := 1
          ELSE
             cList += cAddress + CRLF
          ENDIF
       ENDIF
   NEXT  // dwI


   IF SLen(cList) > 0
      RETURN cSection + " " + cList
   ENDIF


   IF lSectionAlways
      RETURN cSection
   ENDIF


   RETURN cList


