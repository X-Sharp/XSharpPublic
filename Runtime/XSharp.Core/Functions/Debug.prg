//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Debug Output and Stack trace info etc.
using System.Reflection
using System.Diagnostics


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
   return


/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>

function ProcFile(dwActivation as DWORD) as string
	return ProcFile((int) dwActivation + 1) 

/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <returns>
/// </returns>

function ProcFile() as string
	return ProcFile( (int) 1)

/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcFile(activation as int) as string
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL file := "" AS STRING
   
   if ( activation + 1 < st:FrameCount .and. activation >= 0)
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

function ProcLine(dwActivation as DWORD) as dword
	return ProcLine((int) dwActivation + 1) 

/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <returns>
/// </returns>
function ProcLine() as dword
	return ProcLine( (int) 1)


/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcLine(activation as INT) as dword
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL line := 0 AS DWORD
   
   if ( activation + 1 < st:FrameCount .and. activation >= 0)
	  // Note: add 1 so this function isn't included in the stack trace
      line := (DWORD) st:GetFrame( (INT) activation + 1 ):GetFileLineNumber()  
   ENDIF
   
   RETURN line   	


/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <returns>
/// </returns>
function ProcName() as string 
	return ProcName( 1)

/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcName(activation as int) as string
   LOCAL st := StackTrace{ TRUE } AS StackTrace
   LOCAL name := "" AS STRING
   
   if ( activation + 1 < st:FrameCount .and. activation >= 0)
		var mi := st:GetFrame( activation + 1 ):GetMethod()
		var t  := mi:DeclaringType
		IF t == NULL 
			name := mi:Name:ToUpperInvariant()
		ELSE
			name := String.Concat( mi:DeclaringType:Name, ":", mi:Name ):ToUpperInvariant()
		ENDIF
   ENDIF
   
   RETURN name
