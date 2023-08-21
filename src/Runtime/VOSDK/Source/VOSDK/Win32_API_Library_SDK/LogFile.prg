PARTIAL CLASS LogFile
	PROTECT cFName      AS STRING
	PROTECT cBackupName AS STRING
	PROTECT cDLLPath    AS STRING
	PROTECT lFileLog    AS LOGIC
	PROTECT lAppend     AS LOGIC

ACCESS Append()   AS LOGIC

	RETURN SELF:lAppend

ASSIGN Append(lNew AS LOGIC) 

	IF IsLogic(lNew)
		SELF:lAppend := lNew
	ENDIF

	RETURN SELF:lAppend

ACCESS BackupName() AS STRING
	RETURN SELF:cBackupName

ASSIGN BackupName(cNew AS STRING) 

	IF IsString(cNew)
		SELF:cBackupName := cNew
	ENDIF

	RETURN SELF:cBackupName

METHOD DebugMsg(cLine  AS STRING) 
	LOCAL hf    AS PTR
	LOCAL n     AS DWORD

	IF SELF:lFileLog
		hf := __NetOpen(SELF:cFName, FO_READWRITE)

		IF hf != F_ERROR
			FSeek(hf, 0, FS_END)
			n := FWriteLine(hf, cLine)
			FClose(hf)
		ENDIF
	ELSE
		_DebOut32(cLine)
		n := SLen(cLine)
	ENDIF

	RETURN n > 0

METHOD DumpError( oErr  ) 
	LOCAL hf			AS PTR
	LOCAL cStack	AS STRING
	LOCAL cCaption	AS STRING
	LOCAL nLeft		AS DWORD
	LOCAL nRight	AS DWORD
    LOCAL oError   := oErr as Error

	hf := __NetOpen(SELF:cFName, FO_READWRITE)

	IF hf != F_ERROR
		FSeek(hf, 0, FS_END)

		FPuts(hf, "***********************ERROR********************************")
		FPuts(hf, Version() )
		FPuts(hf, DToC(Today()) + " " + Time() )
		FPuts(hf, " " )
		FPuts(hf, "Error Object created:" )
		FPuts(hf, "--------------------" )
		FPuts(hf, "SubSystem       :" + AsString(oError:SubSystem) )
		FPuts(hf, "SubCode         :" + AsString(oError:SubCode  ) )
		FPuts(hf, "GenCode         :" + ErrString(oError:GenCode ) )

		//FdW//20060916-Begin
		cStack:=AsString(oError:StackTrace)
		IF !Empty(cStack)
			cCaption:="Stack           :"
			nLeft:=1
			nRight:=At2(CRLF,cStack)
			IF nRight==0
				nRight:=SLen(cStack)+1
			ENDIF
			DO WHILE nRight>nLeft
				FPuts(hf,cCaption+"'"+SubStr3(cStack,nLeft,nRight-nLeft+1)+"'")
				//FPutS(hf,cCaption+"'"+SubStr3(cStack,nLeft,nLeft-nRight+1)+"'")
				nLeft:=nRight+2
				nRight:=At3(cStack,CRLF,nLeft)
				cCaption:="                :"
				IF nRight==0
					nRight:=SLen(cStack)+1
				ENDIF
			ENDDO
		ENDIF
		//FdW//20060916-End


		IF oError:SubCode = E_EXCEPTION
			FPuts(hf, "ExceptionCode   :" + AsHexString(oError:OsCode) )
			FPuts(hf, "ExceptionFlags  :" + AsHexString(oError:ArgType) )
			FPuts(hf, "ExceptionAddress:" + AsHexString(oError:FuncPtr) )
			FPuts(hf, "ParamNumber     :" + AsString(oError:ArgNum   ) )
			FPuts(hf, "ExceptionInfo   :" + AsHexString(oError:FuncSym) )
		ELSE
			FPuts(hf, "OsCode          :" + AsString(oError:OsCode   ) )
			FPuts(hf, "ArgType         :" + TypeString(oError:ArgType    ) )
			FPuts(hf, "FuncPtr         :" + AsString(oError:FuncPtr  ) )
			FPuts(hf, "ArgNum          :" + AsString(oError:ArgNum   ) )
			FPuts(hf, "FuncSym         :" + AsString(oError:FuncSym  ) )

		ENDIF

		FPuts(hf, "Severity        :" + AsString(oError:Severity ) )
		FPuts(hf, "CanDefault      :" + AsString(oError:CanDefault))
		FPuts(hf, "CanRetry        :" + AsString(oError:CanRetry ) )
		FPuts(hf, "CanSubstitute   :" + AsString(oError:CanSubstitute))
		FPuts(hf, "Operation       :" + AsString(oError:Operation) )
		FPuts(hf, "Description     :" + AsString(oError:Description))
		FPuts(hf, "FileName        :" + AsString(oError:FileName ) )
		FPuts(hf, "Tries           :" + AsString(oError:Tries    ) )
		FPuts(hf, "FileHandle      :" + AsString(oError:FileHandle))
		FPuts(hf, "SubCodeText     :" + AsString(oError:SubCodeText))
		FPuts(hf, "Arg             :" + AsString(oError:Arg) )
		FPuts(hf, "ArgTypeReq      :" + TypeString(oError:ArgTypeReq) )
		FPuts(hf, "MaxSize         :" + AsString(oError:MaxSize      ) )
		FPuts(hf, "SubstituteType  :" + TypeString(oError:SubstituteType))
		FPuts(hf, "CallFuncSym     :" + AsString(oError:CallFuncSym ) )
		FPuts(hf, "--------------------" )

		FClose(hf)
	ENDIF

	RETURN SELF

ACCESS FName()  AS STRING
	RETURN SELF:cFName

ASSIGN FName(cNew AS STRING) 

	IF IsString(cNew)
		SELF:cFName := cNew
	ENDIF

	RETURN SELF:cFName

CONSTRUCTOR( cName, lWinLogPath, lApp) 
	LOCAL       nPos        AS DWORD
	LOCAL       nStart      AS DWORD
	LOCAL       cPath       AS STRING
	LOCAL       cTemp       AS STRING
	LOCAL       hf          AS PTR

	cPath := TruePath( ExecName( .T. ) )

	SELF:cDllPath := cPath

	IF !IsLogic(lWinLogPath)
		lWinLogPath := .F. 
	ENDIF

	IF IsString(cName) .AND. SLen(cName) > 0
		//  Full path given ?
		nPos := RAt("\", cName)
		IF nPos > 0
			cPath := cName
		ELSE
			cPath += cName
		ENDIF
	ELSE
		//  UH 08/21/2000
		cPath := ExecName( .T. )
	ENDIF

	nPos  := RAt(".", cPath)
	IF lWinLogPath
		nStart  := RAt("\", cPath)
		//RvdH 070312 Bug report Gerd Hoerneman
		IF nPos > 0 
			SELF:cFName := GetLogPath() + SubStr3(cPath, nStart, nPos - nStart) + ".log"
		ELSE                                                                        
			SELF:cFName := GetLogPath() + SubStr2(cPath, nStart) + ".log"
		ENDIF                                                                       
		
	ELSE
		IF nPos > 0
			SELF:cFName := SubStr(cPath, 1, nPos) + "log"
		ELSE
			SELF:cFName := cPath + ".log"
		ENDIF
	ENDIF

	IF IsLogic(lApp)
		SELF:lAppend := lApp
	ELSE
		SELF:lAppend := .T. 
	ENDIF

	IF File(SELF:cFName)
		IF SELF:lAppend
			hf := __NetOpen(SELF:cFName, FO_READWRITE)
		ELSE
			IF File(SELF:cFName)
				IF SLen(SELF:cBackupName) = 0
					nPos := RAt(".", SELF:cFName)
					IF nPos > 0
						cTemp := SubStr3(SELF:cFName, 1, nPos) + "bak"
					ELSE
						cTemp := SELF:cFname + ".bak"
					ENDIF

					SELF:cBackUpName := cTemp
				ENDIF
				FCopy(SELF:cFName, SELF:cBackUpName )
			ENDIF
			hf := FCreate(SELF:cFName)
		ENDIF
	ELSE
		hf := FCreate(SELF:cFName)
	ENDIF

	IF hf == F_ERROR
		IF NetErr()
			SELF:lFileLog := .T. 
		ENDIF
	ELSE
		SELF:lFileLog := .T. 
		FClose(hf)
	ENDIF

	RETURN 

ACCESS Log2File()  AS LOGIC

	RETURN SELF:lFileLog

ASSIGN Log2File(lNew AS LOGIC) 

	IF IsLogic(lNew)
		SELF:lFileLog := lNew
	ENDIF

	RETURN SELF:lFileLog

ACCESS LogPath() AS STRING
	LOCAL cRet      AS STRING
	LOCAL nPos      AS DWORD

	cRet := SELF:cFName
	nPos := RAt("\", cRet)
	IF nPos > 0
		cRet := SubStr(cRet, 1, nPos - 1)
	ENDIF

	RETURN cRet

ACCESS ServerPath()  AS STRING
	RETURN SELF:cDLLPath

END CLASS

FUNCTION GetLogPath() AS STRING STRICT

	LOCAL       hKey                AS PTR
	LOCAL       nResult             AS DWORD
	LOCAL DIM   abTemp[_MAX_PATH]   AS BYTE
	LOCAL DIM   abRoot[_MAX_PATH]   AS BYTE
	LOCAL       nTemp               AS DWORD
	LOCAL       dwType              AS DWORD
	LOCAL       dwRegLength         AS DWORD
	LOCAL       cRet                AS STRING

	dwRegLength := _MAX_PATH

	IF RegCreateKeyEx(  HKEY_LOCAL_MACHINE, ;
		String2Psz( "SYSTEM\CurrentControlSet\Services\W3SVC\Parameters"),;
		0,;
		NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL,;
		@hKey,;
		@nResult) == ERROR_SUCCESS

		abTemp[1] := 0

		//RvdH 030318: Changed @abTemp[0] to @abTemp[1]
		RegQueryValueEx(hKey, String2Psz( "LogFileDirectory"), NULL, @dwType, @abTemp[1], @dwRegLength)
		RegCloseKey(hKey)

		cRet := Psz2String(@abTemp[1])

		IF SLen(cRet) = 0
			dwRegLength := GetSystemDirectory(@abTemp[1], _MAX_PATH)
			IF dwRegLength > 0
				cRet := Psz2String(@abTemp[1])
				cRet += "\LogFiles"
			ENDIF
		ELSE
			nTemp := AtC("%SystemRoot%", cRet)

			IF nTemp > 0
				cRet := SubStr2(cRet, nTemp + 12)

				nTemp := GetEnvironmentVariable(String2Psz("SystemRoot"), @abRoot[1], _MAX_PATH)
				IF nTemp > 0
					cRet := Psz2String(@abRoot[1]) + cRet
				ELSE
					cRet := "c:\\winnt" + cRet
				ENDIF
			ENDIF
		ENDIF

	ENDIF

	RETURN cRet

STATIC FUNCTION __NetOpen   (cName AS STRING, nMode AS DWORD)   AS PTR STRICT
	LOCAL hf    AS PTR
	LOCAL n     AS DWORD

	hf := F_ERROR
	n  := 10

	DO WHILE hf == F_ERROR
		hf := FOpen(cName, nMode)
		IF hf == F_ERROR
			IF NetErr()
				n--
				Sleep(1)
			ELSE
				n := 0
			ENDIF
		ELSE
			EXIT
		ENDIF
		IF n == 0
			EXIT
		ENDIF
	ENDDO

	RETURN hf
