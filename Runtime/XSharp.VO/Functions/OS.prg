//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
using System.Reflection


/// <summary>
/// Change the current Windows directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
function DirChange(pszDir as Psz) as int
	return global::Functions.DirChange(Psz2String(pszDir))
	
/// <summary>
/// Create a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
function DirMake(pszDir as Psz) as int
	return global::Functions.DirMake(Psz2String(pszDir))
	
/// <summary>
/// Remove a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
function DirRemove(pszDir as Psz) as int
	return global::Functions.DirRemove(Psz2String(pszDir))

	/// <summary>
	/// Change the current disk drive.
	/// </summary>
	/// <param name="pszDisk"></param>
	/// <returns>
	/// </returns>
function DiskChange(pszDisk as Psz) as logic
	return global::Functions.DiskChange(Psz2String(pszDisk))
 
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function GetRTFullPath() as string
	return Assembly.GetAssembly(typeof(XSharp.__Usual)):Location

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
function GetThreadCount() as dword
	return (DWORD) System.Diagnostics.Process.GetCurrentProcess():Threads:Count


	/// <summary>
	/// Get the number of 1/10000 seconds that have elapsed since Windows was started.
	/// </summary>
	/// <returns>
	/// </returns>
function GetTickCountLow() as dword
	RETURN (DWORD)Environment.TickCount*10	
