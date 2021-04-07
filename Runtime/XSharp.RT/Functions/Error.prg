//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

PROCEDURE ErrorSys _INIT1
   RuntimeState.GetInstance():Settings[Set.ErrorBlock] :=  {|oError| DefError(oError)}
   SetErrorLog(TRUE)
   RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/break/*" />
FUNCTION _Break(uValue AS USUAL) AS USUAL
	BREAK uValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorblock/*" />
FUNCTION ErrorBlock() AS USUAL STRICT
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	IF cbOld == NULL_CODEBLOCK
		cbOld := {|e| DefError(  e ) }
		XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbOld)
	ENDIF
	RETURN cbOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorblock/*" />
FUNCTION ErrorBlock(cbNewSetting AS CODEBLOCK) AS USUAL
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbNewSetting)
	RETURN cbOld



/// <exclude />
FUNCTION VO_Sprintf( format AS USUAL,  args PARAMS OBJECT[] ) AS STRING
    IF format:IsString
	    RETURN _VO_Sprintf( (STRING) format, args)
    ENDIF
    RETURN _VO_Sprintf( (DWORD) format, args)



FUNCTION DefError(oErr AS OBJECT) AS OBJECT PASCAL

	LOCAL dwChoice			AS INT
	LOCAL cMessage			AS STRING
	LOCAL cTitle			AS STRING
	LOCAL hf				AS IntPtr
	STATIC LOCAL dwDefError 		AS DWORD

	IF !(oErr IS Error)
		VAR err := Error{}
		err:Gencode   := EG_WRONGCLASS
		err:Severity  := ES_ERROR
		err:FuncSym    := "DefError"
		err:Arg        := oErr:ToString()
		err:ArgNum    :=  1
		err:CanDefault := .T.
		THROW err

	ENDIF
	LOCAL oError := (Error) oErr AS Error
	IF oError:CanDefault
		// network open error?
		IF (oError:Gencode = EG_OPEN)
			IF (oError:OSCode  = 32) .OR.;					// Sharing Violation
				(oError:OSCode  = 33)    .OR.;				// Lock Violation
				(oError:Gencode = EG_APPENDLOCK)
				NetErr(.T.)
				RETURN E_DEFAULT                // continue with default behavior
			ENDIF
		ENDIF
	ENDIF

	IF oError:Severity = ES_WARNING
		cTitle := "WARNING"
	ELSE
		cTitle := "ERROR"
	ENDIF

	IF oError:SubstituteType = PTR
		oError:Severity := ES_CATASTROPHIC
	ENDIF

	cMessage := oError:ToString()


	dwChoice := 3


	IF SetErrorLog()
		hf := __OpenErrorLog()

		IF hf != F_ERROR
			FSeek3(hf, 0, FS_END)
			__WriteErrorLog(hf, cMessage, oError)
			FClose(hf)
		ENDIF
	ENDIF
    dwChoice := MessageBoxW(IntPtr.Zero, cMessage, cTitle, MB_ABORTRETRYIGNORE)

	DO CASE
	CASE dwChoice = IDIGNORE
		IF oError:CanSubstitute
			RETURN NULL
		ELSE
			RETURN E_DEFAULT
		ENDIF

	CASE dwChoice = IDRETRY
		RETURN E_RETRY
	ENDCASE



	IF CanBreak()
		THROW XSharp.Internal.WrappedException{oError}
	ENDIF

	ErrorLevel(oError:Gencode)

	dwDefError := 0

	CoreDb.CloseAll()

	_Quit()

	RETURN NIL

STATIC DEFINE FILE_ERRORLOG := "VOERROR.LOG"
STATIC FUNCTION __OpenErrorLog() AS IntPtr PASCAL

	LOCAL cFile                     AS STRING
	LOCAL cBuffer                   AS STRING
	LOCAL hfRet                     AS IntPtr

	cFile := WorkDir() + FILE_ERRORLOG

	hfRet := FOpen2(cFile, FO_WRITE)

	IF hfRet != F_ERROR
		FSeek3( hfRet, 0, FS_END)
		FPutS3( hfRet, "", 0)
	ELSE
		hfRet := FCreate2(cFile, FC_ARCHIVED)

		IF FError() = 3                          // path not found

			cFile :=""

			IF !GetDefaultDir() ==""
				cBuffer :="SetDefaultDir: " +GetDefaultDir()
				SetDefaultDir(NULL_PSZ)
			ELSE
				cBuffer :="SetDefault: " +GetDefault()
				SetDefault("")
			ENDIF
			
			//_IError(EG_ARG, .F., String2Psz(cBuffer))
			? cBuffer
		ENDIF
	ENDIF

	RETURN hfRet



STATIC FUNCTION __WriteErrorLog (hf AS IntPtr, cMsg AS STRING, oError AS Error) AS VOID PASCAL
   LOCAL cExe		AS STRING
   
	FPutS(hf, "***********************ERROR********************************")
	FPutS(hf, __VERSION__ )
	FPutS(hf, DateTime.Now:ToString())
	cExe	:= _ExecName() 
	FPutS(hf, "Application: " + cExe )
	FPutS(hf, " " )
	FPutS(hf, "Error message:" )
	FPutS(hf, "--------------" )
	FPutS(hf, cMsg)

	FPutS(hf, "Error Object created:" )
	FPutS(hf, "--------------------" )
	FPutS(hf, "SubSystem       :" + AsString(oError:SubSystem) )
	FPutS(hf, "SubCode         :" + AsString(oError:SubCode)   )
	FPutS(hf, "GenCode         :" + ErrString(oError:Gencode ) )

	IF oError:SubCode = E_EXCEPTION
		FPutS(hf, "ExceptionCode   :" + oError:OSCode:ToString("X") )
		FPutS(hf, "ExceptionFlags  :" + oError:ArgType:ToString("X") )
		FPutS(hf, "ExceptionAddress:" + oError:FuncPtr:ToString("X") )
		FPutS(hf, "ParamNumber     :" + AsString(oError:ArgNum)    )
		FPutS(hf, "ExceptionInfo   :" + AsString(oError:FuncSym) )

	ELSE

		FPutS(hf, "OsCode          :" + oError:OSCode:ToString() )
		FPutS(hf, "ArgType         :" + TypeString(oError:ArgType    ) )
		FPutS(hf, "FuncPtr         :" + oError:FuncPtr:ToString()   )
		FPutS(hf, "ArgNum          :" + oError:ArgNum:ToString()    )
		FPutS(hf, "FuncSym         :" + oError:FuncSym:ToString()   )

	ENDIF

	FPutS(hf, "Severity        :" + AsString(oError:Severity ) )
	FPutS(hf, "CanDefault      :" + AsString(oError:CanDefault))
	FPutS(hf, "CanRetry        :" + AsString(oError:CanRetry ) )
	FPutS(hf, "CanSubstitute   :" + AsString(oError:CanSubstitute))
	FPutS(hf, "Operation       :" + AsString(oError:Operation) )
	FPutS(hf, "Description     :" + AsString(oError:Description))
	FPutS(hf, "FileName        :" + AsString(oError:FileName ) )
	FPutS(hf, "Tries           :" + AsString(oError:Tries    ) )
	FPutS(hf, "FileHandle      :" + AsString(oError:FileHandle))
	FPutS(hf, "SubCodeText     :" + AsString(oError:SubCodeText))
	FPutS(hf, "Arg             :" + AsString(oError:Arg) )
	FPutS(hf, "ArgTypeReq      :" + TypeString(oError:ArgTypeReq) )
	FPutS(hf, "MaxSize         :" + AsString(oError:MaxSize      ) )
	FPutS(hf, "SubstituteType  :" + TypeString(oError:SubstituteType))
	FPutS(hf, "CallFuncSym     :" + AsString(oError:CallFuncSym ) )
	FPutS(hf, "--------------------" )

	RETURN
_DLL FUNCTION MessageBoxW(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD)	AS INT PASCAL:USER32.MessageBoxW UNICODE

STATIC DEFINE MB_ABORTRETRYIGNORE    := 0x00000002U

STATIC DEFINE IDOK            := 1
STATIC DEFINE IDCANCEL        := 2
STATIC DEFINE IDABORT         := 3
STATIC DEFINE IDRETRY         := 4
STATIC DEFINE IDIGNORE        := 5
STATIC DEFINE IDYES           := 6
STATIC DEFINE IDNO            := 7
STATIC DEFINE IDCLOSE         := 8
STATIC DEFINE IDHELP          :=9
STATIC DEFINE IDTRYAGAIN      := 10
STATIC DEFINE IDCONTINUE      := 11
