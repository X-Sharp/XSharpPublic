//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

DELEGATE ShowErrorDialog_Delegate(oError AS Error) AS INT
GLOBAL ShowErrorDialog_Handler AS ShowErrorDialog_Delegate

PROCEDURE ErrorSys _INIT1
   ErrorBlock( {|oError| DefError(oError)} )
   SetErrorLog( TRUE )
   IF XSharp.RuntimeState.Dialect:IsVoLike()
        SetErrorLogFile( "VOERROR.LOG" )
   ELSE
        SetErrorLogFile( "ERROR.LOG" )
   ENDIF
   RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/break/*" />
FUNCTION _Break(uValue AS USUAL) AS USUAL
	THROW XSharp.Internal.WrappedException{uValue}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorblock/*" />
FUNCTION ErrorBlock() AS USUAL STRICT
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	IF cbOld == NULL_CODEBLOCK
		cbOld := {|e| DefError(  e ) }
		XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbOld)
	ENDIF
	RETURN cbOld



FUNCTION ErrorExec(pErrInfo AS Exception) AS USUAL
    local cbError as CODEBLOCK
    local oError  as XSharp.Error
    cbError := ErrorBlock()
	oError := ErrorBuild(XSharp.Error{pErrInfo})
    RETURN cbError:Eval(oError)



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



INTERNAL FUNCTION DefError(oErr AS OBJECT) AS OBJECT PASCAL

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
	IF ShowErrorDialog_Handler != NULL
		dwChoice := ShowErrorDialog_Handler(oError)
	ELSE
		dwChoice := MessageBoxW(IntPtr.Zero, cMessage, cTitle, MB_ABORTRETRYIGNORE)
	ENDIF

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


INTERNAL FUNCTION __OpenErrorLog() AS IntPtr PASCAL

	LOCAL cFile                     AS STRING
	LOCAL cBuffer                   AS STRING
	LOCAL hfRet                     AS IntPtr
    LOCAL cFolder                   AS STRING
    cFolder := System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly():Location)
	cFile := System.IO.Path.Combine(cFolder, SetErrorLogFile())

	hfRet := FOpen2(cFile, FO_WRITE)

	IF hfRet != F_ERROR
		FSeek3( hfRet, 0, FS_END)
		FPutS( hfRet, "", 0)
	ELSE
		hfRet := FCreate2(cFile, FC_ARCHIVED)

		IF FError() = 3                          // path not found

			cFile :=""

			IF !GetDefaultDir() ==""
				cBuffer :="SetDefaultDir: " +GetDefaultDir()
				SetDefaultDir(NULL)
			ELSE
				cBuffer :="SetDefault: " +GetDefault()
				SetDefault("")
			ENDIF

			//_IError(EG_ARG, .F., String2Psz(cBuffer))
			? cBuffer
		ENDIF
	ENDIF

	RETURN hfRet



INTERNAL FUNCTION __WriteErrorLog (hf AS IntPtr, cMsg AS STRING, oError AS Error) AS VOID PASCAL
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
INTERNAL _DLL FUNCTION MessageBoxW(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD)	AS INT PASCAL:USER32.MessageBoxW UNICODE

INTERNAL  DEFINE MB_ABORTRETRYIGNORE    := 0x00000002U

INTERNAL DEFINE IDOK            := 1
INTERNAL DEFINE IDCANCEL        := 2
INTERNAL DEFINE IDABORT         := 3
INTERNAL DEFINE IDRETRY         := 4
INTERNAL DEFINE IDIGNORE        := 5
INTERNAL DEFINE IDYES           := 6
INTERNAL DEFINE IDNO            := 7
INTERNAL DEFINE IDCLOSE         := 8
INTERNAL DEFINE IDHELP          :=9
INTERNAL DEFINE IDTRYAGAIN      := 10
INTERNAL DEFINE IDCONTINUE      := 11




/// <summary>
/// This function is automatically inserted by the compiler in a compiler generated
/// RECOVER USING block when you have a BEGIN SEQUENCE .. END SEQUENCE in your code
/// without RECOVER USING clause
/// </summary>
/// <param name="u">The parameter that was passed in the BREAK statement or the call to the _Break function</param>
/// <remarks>If a REAL exception occurs then this function is NOT called. The function is only called when
/// the (generated) RECOVER USING block is called with a value from a BREAK statement. <br />
/// The default implementation of this function (in the XSharp.RT assembly) does nothing.
/// You can override this function in your own code if you want.
/// The function should then have the following prototype
/// <code language="X#">
/// FUNCTION _SequenceRecover(u as USUAL) AS VOID
/// </code>
/// </remarks>
/// <seealso cref='O:XSharp.RT.Functions._Break'>Break Function</seealso>
FUNCTION _SequenceRecover(u as USUAL) AS VOID
    RETURN


/// <summary>
/// This function is automatically inserted by the compiler in a RECOVER USING block and gets called when the
/// RECOVER USING block is reached because of an exception.
/// </summary>
/// <param name="e">The exception that triggered the jump into the RECOVER USING block</param>
/// <remarks>
/// The default implementation of this function (in the XSharp.RT assembly) called the installed error handler
/// that is installed with ErrorBlock()
/// The function should then have the following prototype
/// <code language="X#">
/// FUNCTION _SequenceError(e as Exception) AS VOID
/// </code>
/// </remarks>
/// <returns>The result of the call to the error handler installed in the ErrorBlock</returns>
/// <seealso cref='O:XSharp.RT.Functions._Break'>Break Function</seealso>
/// <seealso cref='O:XSharp.RT.Functions.ErrorBlock'>Break Function</seealso>
FUNCTION _SequenceError(e as Exception) AS USUAL
    LOCAL error as XSharp.Error
    IF e IS XSharp.Error VAR err
        error := err
    ELSE
        error := Error{e}
    ENDIF
    RETURN Eval(ErrorBlock(), error)






