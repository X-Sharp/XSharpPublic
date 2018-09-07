//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#include "GetSet.xh"
USING XSharp


/// <summary>
/// Return and the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetAnsi() AS LOGIC
	RETURN RuntimeState.Ansi
	
	
	/// <summary>
	/// </summary>
	/// <param name="pBuffer"></param>
	/// <param name="nSize"></param>
	/// <returns>
	/// </returns>
	//FUNCTION GetCallStack(pBuffer AS Psz,nSize AS INT) AS LOGIC
	/// THROW NotImplementedException{}
	//RETURN FALSE   
	
	/// <summary>
	/// </summary>
	/// <param name="b1"></param>
	/// <param name="b2"></param>
	/// <param name="b3"></param>
	/// <param name="nPad"></param>
	/// <returns>
	/// </returns>
FUNCTION GetChunkBase64(b1 AS BYTE,b2 AS BYTE,b3 AS BYTE,nPad AS INT) AS STRING
	/// THROW NotImplementedException{}
	RETURN String.Empty   
	
	/// <summary>
	/// Get the current X# search path for opening file.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetCurPath() AS STRING
	getstate STRING Set.Path
	
	
	/// <summary>
	/// Return the X# default drive and directory.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetDefault() AS STRING
	getstate STRING Set.Default 
	
	/// <summary>
	/// Return the current SetDefaultDir() setting.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetDefaultDir() AS STRING
	getstate STRING Set.Default 
	
	

FUNCTION GetMimType(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   
	
	/// <summary>
	/// Get the current DLL for nation-dependent operations and messages.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetNatDLL() AS STRING
	getstate STRING Set.NatDLL 
	
	
	/// <summary>
	/// Returns a string representing the evening extension for time strings in 12-hour format.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetPMExt() AS STRING
	getstate STRING Set.PmExt
	
	

	
	
	/// <summary>
	/// Return the current separation character used in time strings.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetTimeSep() AS DWORD
	getstate DWORD Set.TimeSep
	
	/// <summary>
	/// Returns TimeZone difference for current timezone in Hours
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetTimeZoneDiff() AS INT
	RETURN TimeZoneInfo.Local.BaseUtcOffSet:Hours

FUNCTION GetYield AS LOGIC
	RETURN SetYield()