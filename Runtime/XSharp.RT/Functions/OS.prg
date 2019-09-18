//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirchange/*" />
FUNCTION DirChange(pszDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirChange(Psz2String(pszDir))
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirmake/*" />
FUNCTION DirMake(pszNewDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirMake(Psz2String(pszNewDir))
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirremove/*" />
FUNCTION DirRemove(pszDirName AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirRemove(Psz2String(pszDirName))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskchange/*" />
FUNCTION DiskChange(pszDrive AS PSZ) AS LOGIC
	RETURN XSharp.Core.Functions.DiskChange(Psz2String(pszDrive))
 
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetRTFullPath() AS STRING
	RETURN Assembly.GetAssembly(TYPEOF(XSharp.__Usual)):Location

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetThreadCount() AS DWORD
	RETURN (DWORD) System.Diagnostics.Process.GetCurrentProcess():Threads:Count


	/// <summary>
	/// Get the number of 1/10000 seconds that have elapsed since Windows was started.
	/// </summary>
	/// <returns>
	/// </returns>
FUNCTION GetTickCountLow() AS DWORD
	RETURN (DWORD)Environment.TickCount*10	
