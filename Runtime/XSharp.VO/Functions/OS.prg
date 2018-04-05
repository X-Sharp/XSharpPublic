//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp



/// Return the operating system name.
/// </summary>
/// <param name="lExtended"></param>
/// <returns>
/// </returns>
function OS(lExtended as Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   



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
 
	