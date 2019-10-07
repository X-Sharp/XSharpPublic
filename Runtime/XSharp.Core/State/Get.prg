//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#include "GetSet.xh"
USING XSharp

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getamext/*" />
FUNCTION GetAMExt() AS STRING
	GETSTATE STRING Set.AmExt 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setampm/*" />
FUNCTION GetAmPm() AS LOGIC
	GETSTATE LOGIC Set.AmPm


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setansi/*" />
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
	
	/// <summary>This function is not implemented yet</summary>
	/// <param name="b1"></param>
	/// <param name="b2"></param>
	/// <param name="b3"></param>
	/// <param name="nPad"></param>
	/// <returns>
	/// </returns>
FUNCTION GetChunkBase64(b1 AS BYTE,b2 AS BYTE,b3 AS BYTE,nPad AS INT) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getcurpath/*" />
FUNCTION GetCurPath() AS STRING
	GETSTATE STRING Set.Path
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getdefault/*" />
FUNCTION GetDefault() AS STRING
	GETSTATE STRING Set.Default 
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getdefaultdir/*" />
FUNCTION GetDefaultDir() AS STRING
	GETSTATE STRING Set.Default 
	
	
/// <summary>This function is not implemented yet</summary>
FUNCTION GetMimType(c AS STRING) AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty   
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getnatdll/*" />
FUNCTION GetNatDLL() AS STRING
	GETSTATE STRING Set.NatDLL 
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getpmext/*" />
FUNCTION GetPMExt() AS STRING
	GETSTATE STRING Set.PmExt
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/gettimesep/*" />
FUNCTION GetTimeSep() AS DWORD
	GETSTATE DWORD Set.TimeSep
	
	/// <summary>
	/// Returns TimeZone difference for current timezone in Hours
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetTimeZoneDiff() AS INT
	RETURN TimeZoneInfo.Local.BaseUtcOffSet:Hours

FUNCTION GetYield AS LOGIC
	RETURN SetYield()
