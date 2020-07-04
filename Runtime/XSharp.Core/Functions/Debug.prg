//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Debug Output and Stack trace info etc.
USING System.Reflection
USING System.Diagnostics
USING System.Runtime.InteropServices


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
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL name := "" AS STRING
   
   IF ( wActivation + 1 < st:FrameCount .AND. wActivation >= 0)
		VAR mi := st:GetFrame( wActivation + 1 ):GetMethod()
		VAR t  := mi:DeclaringType
		IF t == NULL 
			name := mi:Name:ToUpperInvariant()
		ELSE
			name := String.Concat( mi:DeclaringType:Name, ":", mi:Name ):ToUpperInvariant()
		ENDIF
   ENDIF
   
   RETURN name

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorstack/*" />
FUNCTION ErrorStack(wActivation := 1 AS DWORD) AS STRING
	LOCAL oStackTrace AS System.Diagnostics.StackTrace
	oStackTrace := System.Diagnostics.StackTrace{TRUE}
    RETURN ErrorStack(oStackTrace, wActivation)

INTERNAL STATIC CLASS XSharp.ErrorStackSettings
    STATIC PROPERTY ErrorStackVOFormat AS LOGIC AUTO
    STATIC CONSTRUCTOR
        IF XSharp.RuntimeState.Dialect == XSharp.XSharpDialect.VO .OR. ;
            XSharp.RuntimeState.Dialect == XSharp.XSharpDialect.Vulcan
            ErrorStackVOFormat := TRUE
        ENDIF
END CLASS


INTERNAL GLOBAL ErrorStackVOFormat := FALSE AS LOGIC

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorstack/*" />
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
		    FOR VAR i := wStart UPTO (oStackTrace:FrameCount - 1)
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
			    cResult += cStackLine
		    NEXT
	    ELSE
		    cResult := "*EmptyCallStack*" + CRLF
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

/// <summary>This function allows you to enable or disable the VO compatible Errorstack format.<summary/>
/// <param name="lNew">Specify TRUE to enable the new format.</param>
/// <returns>The current setting for the ErrorStack format.</returns>
/// <remarks>The default setting for the format is based on the dialect of the main application.
/// When the main dialect is VO or Vulcan then the VO format is used. Otherwise the normal .Net format is used.
/// </remarks>
FUNCTION SetErrorStackVOFormat(lNew AS LOGIC) AS LOGIC
    LOCAL lPrevious := ErrorStackSettings.ErrorStackVOFormat AS LOGIC
    ErrorStackSettings.ErrorStackVOFormat := lNew
    RETURN lPrevious


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/altd/*" />
/// <param name="nMode">This parameter is ignored in X#</param>
/// <remarks>This function is inlined by the compiler </remarks>
FUNCTION AltD() AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN  


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/altd/*" />
/// <param name="nMode">This parameter is ignored in X#</param>
/// <remarks>This function is inlined by the compiler </remarks>
FUNCTION AltD(nMode AS INT) AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN  
