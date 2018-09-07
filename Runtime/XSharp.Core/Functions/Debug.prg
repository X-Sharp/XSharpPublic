//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Debug Output and Stack trace info etc.
USING System.Reflection
USING System.Diagnostics


/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION DebOut32( s AS STRING ) AS VOID
   System.Diagnostics.Debug.WriteLine(s)
   RETURN

/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION _DebOut32( s AS STRING ) AS VOID
   System.Diagnostics.Debug.WriteLine(s)
   RETURN


/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>

FUNCTION ProcFile(dwActivation AS DWORD) AS STRING
	RETURN ProcFile((INT) dwActivation + 1) 

/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <returns>
/// </returns>

FUNCTION ProcFile() AS STRING
	RETURN ProcFile( (INT) 1)

/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
FUNCTION ProcFile(activation AS INT) AS STRING
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL file := "" AS STRING
   
   IF ( activation + 1 < st:FrameCount .AND. activation >= 0)
	  // Note: add 1 so this function isn't included in the stack trace
      file :=  st:GetFrame( (INT) activation + 1 ):GetFileName()  
   ENDIF
   
   RETURN file

/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>

FUNCTION ProcLine(dwActivation AS DWORD) AS DWORD
	RETURN ProcLine((INT) dwActivation + 1) 

/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ProcLine() AS DWORD
	RETURN ProcLine( (INT) 1)


/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
FUNCTION ProcLine(activation AS INT) AS DWORD
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL line := 0 AS DWORD
   
   IF ( activation + 1 < st:FrameCount .AND. activation >= 0)
	  // Note: add 1 so this function isn't included in the stack trace
      line := (DWORD) st:GetFrame( (INT) activation + 1 ):GetFileLineNumber()  
   ENDIF
   
   RETURN line   	


/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ProcName() AS STRING 
	RETURN ProcName( 1)

/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
FUNCTION ProcName(activation AS INT) AS STRING
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL name := "" AS STRING
   
   IF ( activation + 1 < st:FrameCount .AND. activation >= 0)
		VAR mi := st:GetFrame( activation + 1 ):GetMethod()
		VAR t  := mi:DeclaringType
		IF t == NULL 
			name := mi:Name:ToUpperInvariant()
		ELSE
			name := String.Concat( mi:DeclaringType:Name, ":", mi:Name ):ToUpperInvariant()
		ENDIF
   ENDIF
   
   RETURN name


/// <summary>
/// Either determine whether the Debugger can be invoked manually or programmatically define a breakpoint in an application.
/// </summary>
/// <param name="nMode">This parameter is ignored in X#</param>
/// <returns>
/// </returns>
/// <remarks>This function is inlined by the compiler </remarks>
FUNCTION AltD() AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN  

/// <summary>
/// Either determine whether the Debugger can be invoked manually or programmatically define a breakpoint in an application.
/// </summary>
/// <param name="nMode">This parameter is ignored in X#</param>
/// <returns>
/// </returns>
/// <remarks>This function is inlined by the compiler </remarks>
FUNCTION AltD(nMode AS INT) AS VOID
	IF System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	ENDIF
	RETURN  
