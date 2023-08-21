FUNCTION FileSizeString(uBytes, lTrim)

	LOCAL cResult AS STRING
	LOCAL nBytes AS INT
	// returns bytes, KB or MB but returns valid strings
	
	Default(@lTrim, FALSE)
	
	DO CASE
	CASE IsString(uBytes)
		cResult := uBytes	// return this anyway - might be "<Dir>" etc
	CASE uBytes < 1024
		cResult := Str(uBytes,7,0)
	CASE uBytes < 10485760
		nBytes := INT(uBytes)/1024
		cResult := Str(nBytes,5,0) + "KB"
	OTHERWISE
		// for 10MB and up
		nBytes := INT(uBytes)/1048576
		cResult := Str(nBytes,5,0) + "MB"
	ENDCASE

	IF lTrim
		cResult := AllTrim(cResult)
	ENDIF

	RETURN cResult


FUNCTION FullDate (dInDate AS DATE) AS STRING STRICT

	RETURN Left(DToC(dInDate),2)+" "+CMonth(dIndate)+" "+Left(DToS(dInDate),4)

FUNCTION GetAssociatedIcon( cFile AS STRING , lLarge := TRUE AS LOGIC ) AS Icon PASCAL

	LOCAL strucSHFileInfo IS _winSHFILEINFO
	LOCAL hImage AS PTR
	LOCAL oIcon AS Icon
	LOCAL dwStyle AS DWORD

	// Code thanks to Karl-Heinz Rauscher 14/01/02
	// Other info parameters possible, of course
	dwStyle := _or ( SHGFI_ICON , SHGFI_USEFILEATTRIBUTES )

	IF lLarge
		dwStyle := _or ( dwStyle , SHGFI_LARGEICON )
   	ELSE
		dwStyle := _or ( dwStyle , SHGFI_SMALLICON )
	ENDIF

	hImage := SHGetFileInfo( Cast2Psz ( cFile ) ,;
		0,;
		@strucSHFileInfo,;
		_sizeof( _winSHFILEINFO),;
		dwStyle )

	IF hImage != NULL_PTR
		oIcon := Icon{strucSHFileInfo.hIcon}
	ENDIF

	RETURN oIcon


FUNCTION GetTempFilePath() AS STRING PASCAL
	LOCAL ptrName AS PTR
	LOCAL cResult AS STRING

	ptrName := MemAlloc(250)
	GetTempPath(250,ptrName)
	cResult := Psz2String(ptrName)
	MemFree(ptrName)

	RETURN cResult

FUNCTION SetUserInfo() AS VOID PASCAL

	LOCAL oReg AS RegSetup

	aMailInfo := ArrayCreate(14)
 	oReg := RegSetup{}	

	aMailInfo[DEF_ADDRESS]    := oReg:QueryString("Email_Address")
	aMailInfo[DEF_SMTPSERVER] := oReg:QueryString("Email_SmtpServer")
	aMailInfo[DEF_POPSERVER]  := oReg:QueryString("Email_PopServer")	
	aMailInfo[DEF_FULLNAME]   := oReg:QueryString("Email_Fullname")
	aMailInfo[DEF_ACCOUNT]    := oReg:QueryString("Email_Account")
	aMailInfo[DEF_PASSWORD]   := Crypt(oReg:QueryString("Email_Password"),"VO SO GOOD")
	aMailInfo[DEF_ATTACHPATH] := oReg:QueryString("Email_Directory")
	aMailInfo[DEF_HEADERS]    := (oReg:QueryInt("Email_InspectPopHeaders") = 1)
	aMailInfo[DEF_DELETEMAIL] := (oReg:QueryInt("Email_DeleteDownloads") = 1)
	aMailInfo[DEF_DELAY]      := oReg:QueryInt("Email_CheckDelay")
	aMailInfo[DEF_SMTPAUTH]   := (oReg:QueryInt("Email_SmtpAuthentication") = 1)
	aMailInfo[DEF_SMTPNAME]   := oReg:QueryString("Email_SmtpUserName")
	aMailInfo[DEF_SMTPPASSWD] := Crypt(oReg:QueryString("Email_SmtpPassword"),"VO SO GOOD")
	aMailInfo[DEF_STARTINBOX] := oReg:QueryInt("Email_StartupInbox") = 1

	RETURN	

FUNCTION TempFilename (cExt AS STRING) AS STRING PASCAL
   LOCAL cTemp, cPath AS STRING
   
   cPath := GetTempFilePath()

   DO WHILE .T.
      
      cTemp := cPath+"VO_"+SubStr2(StrZero(Rand(0)*99999.,6,0),2)+"."+cExt
      IF ! File(cTemp)
         EXIT
      ENDIF
   ENDDO
	
	RETURN cTemp


