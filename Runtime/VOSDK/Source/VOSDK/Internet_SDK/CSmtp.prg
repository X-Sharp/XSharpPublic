PARTIAL CLASS CSmtp INHERIT CMailAbstract

	// the class to send an email

	PROTECT nEmailFormat	AS DWORD		// encoding or format type of email
	PROTECT oEmail			AS CEmail	// email message to be sent

	PROTECT cMailApplication	AS STRING

	//S.Ebert
	PROTECT lSecureSMTP   	AS LOGIC

METHOD __StartData () 

	IF ! SELF:SendRemote("DATA" + CRLF)
		RETURN .F. 
	ENDIF

	IF !SELF:RecvRemote()
		RETURN .F. 
	ENDIF

	IF !SELF:CheckReply()
		SELF:nError := 214
		RETURN .F. 
	ENDIF

	RETURN .T. 

METHOD __StopSending (lSuccess) 

	LOCAL cTemp AS STRING
	LOCAL nTemp AS DWORD

	nTemp := SELF:nError
	cTemp := SELF:cReply

	SELF:nCurState := CLOSING_CONNECTION
	SELF:Disconnect()
	SELF:nCurState := WAITING_FOR_ACTION
	IF !lSuccess
		//  UH 05/29/2000
		SELF:nError := nTemp
		SELF:cReply := cTemp
	ENDIF
	RETURN lSuccess


METHOD CheckReply()
   //SE-040707
	LOCAL cChar AS STRING

	cChar := SubStr3(SELF:cReply, 1, 1)

	SELF:nReply := Val(SubStr3(SELF:cReply, 1, 4) )
	
	IF cChar == "4" .OR. cChar == "5"
		SELF:nError := DWORD(SELF:nReply)
		RETURN FALSE
	ENDIF

	IF cChar == "1" .OR. cChar == "2" .OR. cChar == "3"
		SELF:nError := 0
	ENDIF

	RETURN TRUE

METHOD connect(cIP, nPort) 
   //SE-040707
	LOCAL cBuffer AS STRING
	LOCAL wPort   AS WORD

	IF IsNumeric(nPort)
		wPort := nPort
	ELSE
		wPort := SELF:RemotePort
	ENDIF

	// UH 08/03/2000
	IF ! SELF:Open()
		RETURN FALSE
	ENDIF

	IF ! SELF:oSocket:connect(cIP, wPort)
		SELF:nError := SELF:osocket:Error
		RETURN FALSE
	ENDIF

	IF ! SELF:RecvRemote()
		RETURN FALSE
	ENDIF

	//S.Ebert - EHLO for secured servers
	cBuffer := IIF(SELF:SecureSMTP, "EHLO ","HELO " )+ SELF:DomainName + CRLF

	IF ! SELF:SendRemote(cBuffer)
		RETURN FALSE
	ENDIF

	IF ! SELF:RecvRemote()
		RETURN FALSE
	ENDIF

	IF ! SELF:CheckReply()
		SELF:nError := 213
		RETURN FALSE
	ENDIF

	//S.Ebert - For multiline 250- responses   05/22/2005
	DO WHILE SELF:nReply = 250 .AND. RAt2("250 ", SELF:cReply) = 0
		IF ! SELF:RecvRemote()
			RETURN FALSE
		ENDIF
		IF ! SELF:CheckReply()
			SELF:nError := 213
			RETURN FALSE
		ENDIF
	ENDDO

	//S.Ebert
	IF SELF:SecureSMTP		// needs to authenticate SMTP login
		RETURN SELF:Logon()
	ENDIF

	RETURN TRUE

METHOD Disconnect () 
   //SE-040707
	LOCAL cBuffer AS STRING
	LOCAL lRet AS LOGIC

	lRet := TRUE

	IF SELF:lSocketOpen
		
		cBuffer := DEFAULT_STOPDATA
		IF (lRet := SELF:SendRemote(cBuffer))
			lRet := SELF:RecvRemote()
		ENDIF
		
		IF lRet
			IF ! (lRet := SELF:CheckReply())
				SELF:nError := 215
			ENDIF
		ENDIF

		cBuffer := "QUIT" + CRLF
		IF lRet
			lRet := SELF:SendRemote(cBuffer)
		ENDIF
		
		IF lRet
			lRet := SELF:RecvRemote()
		ENDIF
		
		IF lRet
			IF ! (lRet := SELF:CheckReply())
				SELF:nError := 216
			ENDIF
		ENDIF

		SUPER:Close()
	ENDIF

	RETURN lRet

ACCESS Email 
	RETURN SELF:oEmail

ASSIGN Email(oMail) 

	IF IsInstanceOf(oMail, #CEmail)
		SELF:oEmail := oMail
	ENDIF

	RETURN SELF:oEmail

ACCESS EmailFormat 
	RETURN SELF:nEmailFormat

ASSIGN EmailFormat(nValue) 
    //SE-040707
	DO CASE
	CASE nValue == EMAIL_FORMAT_MIME

	CASE nValue == EMAIL_FORMAT_UUENCODE
		IF SELF:oEmail != NULL_OBJECT
			SELF:oEmail:TransferEncoding := CODING_TYPE_NONE
		ENDIF

	OTHERWISE
		nValue := SELF:nEmailFormat
	ENDCASE

	RETURN SELF:nEmailFormat := nValue


CONSTRUCTOR(oMail, cServer, nPort) 
	
	// the class to send an email - the email is expected to be fully formatted
	// the owner window is used to find and report transmission status
    Default(@nPort, IPPORT_SMTP)

	SUPER(nPort, cServer)

	SELF:oEmail           := oMail						// mail must already exist and be completed
	SELF:RemoteHost       := cServer					// this does a string name conversion to an IP string
	SELF:nEmailFormat     := EMAIL_FORMAT_MIME		// this is default. Could be UUENCODE but not recommmended
	SELF:TextEncoding     := CODING_TYPE_PRINTABLE
#ifdef __VULCAN__
	 SELF:cMailApplication := "SMTP Mailer V1.6 - Powered by build "+__VERSION__
#else	 
	SELF:cMailApplication := "SMTP Mailer V1.6 - Powered by Visual Objects 2.8 build "+__VERSION__
#endif	 
	
	RETURN 

METHOD Logon() 
   //SE-040706
	//S.Ebert - October 2003
	LOCAL cBuffer  AS STRING
	LOCAL cUserID 	AS STRING
	LOCAL cPassW 	AS STRING
	LOCAL dwPos    AS DWORD

	cUserID := B64EncodeString(SELF:UserName)
	cPassW  := B64EncodeString(SELF:PassWord)
	
	IF ! SELF:SendRemote("AUTH LOGIN " + cUserID + CRLF)
		RETURN FALSE	
	ENDIF

	IF ! SELF:RecvRemote()
		RETURN FALSE	
	ENDIF

	cBuffer := SubStr2(SELF:cReply, 5)
	
	IF (dwPos := At2(CRLF, cBuffer)) > 0
		cBuffer := Left(cBuffer, dwPos+1)
		//extract Bass64 coded string and being sure that no other characters
		//are included because DecodeBase64() forces a GPF otherwise.

		IF Upper(Left(B64DecodeString(cBuffer),8)) == "PASSWORD"
			//
			IF ! SELF:CheckReply()
				SELF:nError := 213
				RETURN FALSE	
			ENDIF
			//
			IF ! SELF:SendRemote(cPassW + CRLF)
				RETURN FALSE
			ENDIF

			IF ! SELF:RecvRemote()
				RETURN FALSE	
			ENDIF

			IF ! SELF:CheckReply()
				SELF:nError := 213
				RETURN FALSE	
			ENDIF
		ENDIF
	ENDIF

	RETURN TRUE

ACCESS MailApplication 
   RETURN SELF:cMailApplication

ASSIGN MailApplication(cValue) 
   RETURN SELF:cMailApplication := cValue

METHOD RecvRemote() 
   //SE-040707
	LOCAL lRet   AS LOGIC
	LOCAL dwSize AS DWORD
	LOCAL cRet	 AS STRING

	SELF:cReply := SELF:oSocket:GetLine()

	dwSize := SLen(SELF:cReply)

	IF dwSize = 0
		lRet := TRUE
	ELSE
		cRet := SELF:cReply
		SELF:nReply := Val(SubStr3(cRet, 1, 4))
		IF SELF:nReply > 0
			lRet        := TRUE
			SELF:nError := 0
		ELSE
			//	UH 03/16/1998
			cRet := AllTrim(Upper(cRet))
			IF cRet = "(EST)"
				SELF:nReply := 250
				lRet        := TRUE
				SELF:nError := 0
			ENDIF
		ENDIF
	ENDIF

	RETURN lRet



ACCESS SecureSMTP 
	RETURN lSecureSMTP

ASSIGN SecureSMTP(lValue) 
	RETURN SELF:lSecureSMTP := lValue

METHOD SendHeaderInfo() 
   //SE-040709    
	//RvdH 070325 Removed parameters. Get everything from the oEmail
	//     instance variable. 
	LOCAL lRet        AS LOGIC
	LOCAL dwI, dwJ    AS DWORD
	LOCAL dwCount     AS DWORD
	LOCAL aAddressees AS ARRAY
	LOCAL aList       AS ARRAY
	LOCAL aRCPT       AS ARRAY
	LOCAL cAddress    AS STRING
	LOCAL cName       AS STRING
	LOCAL cFromAdr    AS STRING
	LOCAL dwRCPTs     AS DWORD  
	LOCAL cFrom			AS STRING

	// First we will build an array of potential recipients - this strips out potential duplications
	// when the same person is in CC's, BCC's or in the TO list
	
	IF IsString(cFrom)
		cFromAdr := __ParseAddress(SELF:oEmail:@@From, @cName)
	ENDIF

	aRCPT := {}

	AAdd(aRCPT, SELF:oEmail:DestList)
	AAdd(aRCPT, SELF:oEmail:CCList)
	AAdd(aRCPT, SELF:oEmail:BCCList)

	aAddressees := {}
	dwRCPTs     := ALen(aRCPT)

	FOR dwJ := 1 UPTO dwRCPTs
		aList := aRCPT[dwJ]
		dwCount := ALen(aList)
		FOR dwI := 1 UPTO dwCount
			cAddress    := __ParseAddress(aList[dwI], @cName)
			IF AScan(aAddressees, {|cTag|At(cAddress, cTag)>0}) = 0
				AAdd(aAddressees, cAddress)
			ENDIF
		NEXT  // dwI
	NEXT  // dwJ

	// Start the real transmission now
	IF (dwCount := ALen(aAddressees)) = 0
		// nobody to send to - it was all internal
		RETURN FALSE
	ENDIF

	cFromAdr := "MAIL FROM:<" + cFromAdr + ">" + CRLF

	IF ! SELF:SendRemote(cFromAdr)
		RETURN FALSE
	ENDIF

	IF !SELF:RecvRemote()
		RETURN FALSE
	ENDIF

	IF !SELF:CheckReply()
		SELF:nError := 211
		RETURN FALSE
	ENDIF

	// Now send all elements of the array

	BEGIN SEQUENCE
		FOR dwI := 1 UPTO dwCount
			cAddress := "RCPT TO:<" + aAddressees[dwI] + ">" + CRLF
			IF ! SELF:SendRemote(cAddress)
				BREAK
			ENDIF
			IF ! SELF:RecvRemote()
				BREAK
			ENDIF
			IF ! SELF:CheckReply()
				SELF:nError := 212
				BREAK
			ENDIF
		NEXT  // dwI
		lRet := TRUE

	RECOVER
		lRet := FALSE

	END SEQUENCE

	RETURN lRet

METHOD SendMail() 
    //SE-070419   
    //SE-071018 Updated
    LOCAL lRet   AS LOGIC
    LOCAL cData  AS STRING
    
    SELF:nCurState := CONNECTING
    SELF:nError := 0
    
    IF ! SELF:connect(SELF:cHostAddress)
        SELF:Close()
        RETURN FALSE
    ENDIF

    SELF:nCurState := ESTABLISHING_SESSION
    //RvdH 070325 Added BCCList..
    IF ! SELF:SendHeaderInfo()
        RETURN SELF:__StopSending(FALSE)
    ENDIF

    IF !SELF:__StartData()
        RETURN SELF:__StopSending(FALSE)
    ENDIF

    IF __GetMailInfo(SELF:oEmail:Cargo, TEMP_MAILER, FALSE) == NULL_STRING
        SELF:oEmail:Cargo += TEMP_MAILER + " " + __EncodeField(SELF:cMailApplication, SLen(TEMP_MAILER)+1) + CRLF
    ENDIF
    
    SmtpTransparency(@cData)
    lRet  := TRUE
    SELF:oEmail:StreamStart()
    DO WHILE lRet
        cData := oEmail:StreamOut()  
        IF cData == NULL_STRING
            EXIT
        ENDIF   
        
        //SE-071018 see RFC-2821 chapter 4.5.2 Transparency 
        SmtpTransparency(@cData)
        
        lRet  := SELF:oSocket:SendRawText(cData)
    ENDDO


    IF ! lRet
        SELF:nError := SELF:oSocket:Error
    ENDIF

    SELF:__StopSending(lRet)
    
    cData := NULL_STRING
    //RvdH 070417 CollectForced should not be needed. Let VO handle it.
    // CollectForced()

    RETURN lRet

	

ACCESS TextEncoding 

	IF SELF:oEmail == NULL_OBJECT
		RETURN CODING_TYPE_NONE
	ENDIF

	RETURN SELF:oEmail:TransferEncoding

ASSIGN TextEncoding(n) 

	IF SELF:oEmail == NULL_OBJECT
		RETURN CODING_TYPE_NONE
	ENDIF

	RETURN SELF:oEmail:TransferEncoding := n

END CLASS

STATIC FUNCTION SmtpTransparency(cData REF STRING) AS VOID PASCAL
    /* SE-071017 see RFC-2821 chapter 4.5.2 Transparency
    Before sending a line of mail text, the SMTP client checks the
    first character of the line. If it is a period, one additional
    period is inserted at the beginning of the line.
    
    Optimized for a minimum of String memory usage
    */   
    STATIC bLast AS BYTE
    LOCAL  dwLen AS DWORD
    #ifndef  __VULCAN__
    LOCAL  pChar AS BYTE PTR  
    #endif
    
    IF At2(CRLF+".", cData) > 0
        cData := StrTran(cData, CRLF+".", CRLF+"..")
    ENDIF
    
    IF bLast = 0x0A
        IF Asc(cData) = 46
            cData := "." + cData
        ENDIF   
    ELSEIF bLast = 0x0D       
        IF cData = _CHR(0x0A)+"." 
            cData := Stuff(cData, 2, 0, ".")
        ENDIF   
    ENDIF
    
    IF (dwLen := SLen(cData)) > 0
#ifdef  __VULCAN__
        bLast := cData[dwLen - 1]
#else
        pChar := PTR(_CAST, cData)  
        bLast := pChar[dwLen]
#endif        
        IF bLast = 0x0A
            IF dwLen > 1 
#ifdef  __VULCAN__
               IF cData[dwLen - 2] == 0x0D
#else
                pChar := PTR(_CAST, cData)
                IF pChar[dwLen - 1] = 0x0D
#endif
                    RETURN
                ENDIF    
            ENDIF     
        ELSEIF bLast = 0x0D
            RETURN
        ENDIF   
    ENDIF
    
    bLast := 0
    RETURN
