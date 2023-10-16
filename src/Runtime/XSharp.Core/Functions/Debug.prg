//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Debug Output and Stack trace info etc.
USING System.Reflection
USING System.Diagnostics
USING System.Runtime.InteropServices

define EMPTY_ERRORSTACK := "*EmptyCallStack*"

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/debout32/*" />
FUNCTION DebOut32( pszText AS STRING ) AS VOID
    Win32.OutputDebugString(pszText+e"\r\n")
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_debout32/*" />
FUNCTION _DebOut32( pszText AS STRING ) AS VOID
   Win32.OutputDebugString(pszText+e"\r\n")
   RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procfile/*" />
FUNCTION ProcFile(wActivation AS DWORD) AS STRING
	RETURN ProcFile((INT) wActivation + 1)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procfile/*" />
FUNCTION ProcFile() AS STRING
	RETURN ProcFile( (INT) 1)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procfile/*" />
FUNCTION ProcFile(wActivation AS INT) AS STRING
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL file := "" AS STRING

   IF ( wActivation + 1 < st:FrameCount .AND. wActivation >= 0)
	  // Note: add 1 so this function isn't included in the stack trace
      file :=  st:GetFrame( (INT) wActivation + 1 ):GetFileName()
   ENDIF

   RETURN file

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procline/*" />
FUNCTION ProcLine(dwActivation AS DWORD) AS DWORD
	RETURN ProcLine((INT) dwActivation + 1)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procline/*" />
FUNCTION ProcLine() AS DWORD
	RETURN ProcLine( (INT) 1)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procline/*" />
FUNCTION ProcLine(dwActivation AS INT) AS DWORD
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL line := 0 AS DWORD

   IF ( dwActivation + 1 < st:FrameCount .AND. dwActivation >= 0)
	  // Note: add 1 so this function isn't included in the stack trace
      line := (DWORD) st:GetFrame( (INT) dwActivation + 1 ):GetFileLineNumber()
   ENDIF

   RETURN line


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procname/*" />
FUNCTION ProcName() AS STRING
	RETURN ProcName( 1)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/procname/*" />
FUNCTION ProcName(wActivation AS INT) AS STRING
    RETURN ProcName(wActivation , FALSE)
FUNCTION ProcName(wActivation AS INT, lShowSignature AS LOGIC) AS STRING
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL name := "" AS STRING

    IF ( wActivation + 1 < st:FrameCount .AND. wActivation >= 0)
		VAR mi := st:GetFrame( wActivation + 1 ):GetMethod()
        if lShowSignature
            var stringBuilder := System.Text.StringBuilder{}
            if mi:DeclaringType != null
                stringBuilder:Append(mi:DeclaringType:FullName:Replace('+', '.'))
                stringBuilder:Append(".")
            endif
            stringBuilder:Append(mi:Name)
            var first := true
            if mi is MethodInfo var m .and. m:IsGenericMethod
                var genericArguments := m:GetGenericArguments()
                stringBuilder:Append("<")

                foreach var arg in genericArguments
                    if (!first)
                        stringBuilder:Append(",")
                    else
                        first := false
                    endif
                    stringBuilder:Append(arg:Name)
                next
                stringBuilder:Append(">")
            endif
            stringBuilder:Append("(")
            first := true
            foreach var param in mi:GetParameters()
                if (!first)
                    stringBuilder:Append(",")
                else
                    first := false
                endif
                var str := "<UnknownType>"
                if param.ParameterType != null
                    str := param.ParameterType.Name
                endif
                var dir := " as "
                if param:IsIn
                    if param:IsOut
                        dir := " ref "
                    else
                        dir := " as  "
                    endif
                elseif param:IsOut
                    dir := " out "
                endif
                stringBuilder.Append(param.Name+dir+str)
            next
            stringBuilder:Append(")")

            name := stringBuilder:ToString()
        ELSE
		    VAR t  := mi:DeclaringType
		    IF t == NULL
			    name := mi:Name:ToUpperInvariant()
            ELSE
    	        name := String.Concat( mi:DeclaringType:Name, iif (mi:IsStatic, ".", ":"), mi:Name ):ToUpperInvariant()

            ENDIF
        ENDIF
    ENDIF
   RETURN name

/// <summary>Return the error stack as a string.</summary>
/// <param name="wActivation">Starting level. Defaults to 1.</param>
/// <returns>The error stack with line numbers. In the VO and Vulcan dialect the stack is in "VO Format"</returns>
FUNCTION ErrorStack(wActivation := 1 AS DWORD) AS STRING
	LOCAL oStackTrace AS System.Diagnostics.StackTrace
	oStackTrace := System.Diagnostics.StackTrace{TRUE}
    RETURN ErrorStack(oStackTrace, wActivation)

INTERNAL STATIC CLASS XSharp.ErrorStackSettings
    STATIC PROPERTY ErrorStackVOFormat AS LOGIC AUTO
    STATIC CONSTRUCTOR
        SWITCH XSharp.RuntimeState.Dialect
        CASE XSharp.XSharpDialect.VO
        CASE XSharp.XSharpDialect.Vulcan
            ErrorStackVOFormat := TRUE
        OTHERWISE
            ErrorStackVOFormat := FALSE
        END SWITCH
END CLASS


/// <summary>Return the error stack as a string.</summary>
/// <param name="oStackTrace">StackTrace object to convert to an error stack string</param>
/// <param name="wActivation">Starting level. Defaults to 1.</param>
/// <returns>The error stack with line numbers. In the VO and Vulcan dialect the stack is in "VO Format"</returns>
/// <seealso cref='SetErrorStackVOFormat' />
FUNCTION ErrorStack(oStackTrace AS System.Diagnostics.StackTrace, wActivation := 1 AS DWORD) AS STRING
	LOCAL cResult := "" AS STRING
    LOCAL wStart  AS DWORD
    IF wActivation == UInt32.MaxValue
        wStart := 0
    ELSE
        wStart := wActivation + 1
    ENDIF

    IF ErrorStackSettings.ErrorStackVOFormat
	    IF wStart <= oStackTrace:FrameCount - 1
		    for var i := wStart upto (oStackTrace:FrameCount - 1)
			    VAR oFrame := oStackTrace:GetFrame((INT)i)
			    VAR oMethod := oFrame:GetMethod()
			    VAR cStackLine := oMethod:Name:ToUpper()
			    VAR oInnerType := oMethod:DeclaringType
			    DO WHILE ( oInnerType != NULL )
				    IF !( oInnerType:Name == "Functions" )
					    cStackLine := oInnerType:Name:ToUpper() + ":" + cStackLine
				    ENDIF
				    oInnerType := oInnerType:DeclaringType
			    ENDDO
			    cStackLine += " (Line: " + oFrame:GetFileLineNumber():ToString() + ")" + CRLF
    	        cResult += " "+cStackLine
		    NEXT
	    else
		    cResult := EMPTY_ERRORSTACK + CRLF
	    ENDIF
    ELSE
        cResult := oStackTrace:ToString()
        VAR aLines := cResult:Split(<CHAR>{'\r','\n'},StringSplitOptions.RemoveEmptyEntries)
        IF aLines:Length >= wStart
            cResult := ""
            FOR VAR i := wStart UPTO aLines:Length -1
                cResult += aLines[i]+e"\r\n"
            NEXT
        ENDIF
    ENDIF
	RETURN cResult

/// <summary>This function allows you to enable or disable the VO compatible Errorstack format.</summary>
/// <param name="lNew">Specify TRUE to enable the new format.</param>
/// <returns>The current setting for the ErrorStack format.</returns>
/// <remarks>The default setting for the format is based on the dialect of the main application.
/// When the main dialect is VO or Vulcan then the VO format is used. Otherwise the normal .Net format is used.
/// </remarks>
/// <seealso cref='ErrorStack' />
FUNCTION SetErrorStackVOFormat(lNew AS LOGIC) AS LOGIC
    LOCAL lPrevious := ErrorStackSettings.ErrorStackVOFormat AS LOGIC
    ErrorStackSettings.ErrorStackVOFormat := lNew
    RETURN lPrevious


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/altd/*" />
/// <remarks>This function is inlined by the compiler, but is included so it can be used in Macros as well.</remarks>
FUNCTION AltD() AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/altd/*" />
/// <param name="nMode">This parameter is ignored in X#</param>
/// <remarks>This function is inlined by the compiler, but is included so it can be used in Macros as well.</remarks>
FUNCTION AltD(nMode AS INT) AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN
