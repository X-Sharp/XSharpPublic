//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection


/// <summary>
/// Change the current Windows directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirChange(pszDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirChange(Psz2String(pszDir))
	
/// <summary>
/// Create a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirMake(pszDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirMake(Psz2String(pszDir))
	
/// <summary>
/// Remove a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirRemove(pszDir AS PSZ) AS INT
	RETURN XSharp.Core.Functions.DirRemove(Psz2String(pszDir))

	/// <summary>
	/// Change the current disk drive.
	/// </summary>
	/// <param name="pszDisk"></param>
	/// <returns>
	/// </returns>
FUNCTION DiskChange(pszDisk AS PSZ) AS LOGIC
	RETURN XSharp.Core.Functions.DiskChange(Psz2String(pszDisk))
 
	

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
