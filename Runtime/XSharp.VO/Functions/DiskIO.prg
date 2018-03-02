//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



/// <summary>
/// Fill a series of __Arrays with directory information.
/// </summary>
/// <param name="cPath"></param>
/// <param name="aFNAME"></param>
/// <param name="aFSIZE"></param>
/// <param name="aFDATE"></param>
/// <param name="aFTIME"></param>
/// <param name="aFATTR"></param>
/// <returns>
/// </returns>
function ADir(cPath as __Usual,aFNAME as __Usual,aFSIZE as __Usual,aFDATE as __Usual,aFTIME as __Usual,aFATTR as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   


/// <summary>
/// Return the current Windows directory.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
function CurDir(cDisk as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DefaultDirInit() as void
	/// THROW NotImplementedException{}
	return  

/// <summary>
/// Create an __Array of directory and file information.
/// </summary>
/// <param name="cPath"></param>
/// <param name="xAttr"></param>
/// <returns>
/// </returns>
function Directory(cPath as __Usual,xAttr as __Usual) as __Array
	/// THROW NotImplementedException{}
	return null_array   



/// <summary>
/// Return the space available on the current disk drive.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
function DiskFree() as __Usual
	return DiskFree(CurDrive())

/// <summary>
/// Return the space available on a specified disk.
/// </summary>
/// <param name="cDisk">The drivename to get the free space from.</param>
/// <returns>
/// The free space on the specified disk drive.
/// </returns>	   
function DiskFree(cDisk as __Usual) as __Usual
	return System.IO.DriveInfo{cDisk}:TotalFreeSpace

/// <summary>
/// Return the current Windows drive.
/// </summary>
/// <returns>
/// The current windows drive.
/// </returns>
function DiskName() as string
	return CurDrive()

/// <summary>
/// Return the capacity of the current disk.
/// </summary>
/// <returns>
/// The capacity of the current disk.
/// </returns>
function DiskSpace() as __Usual
	return DiskSpace(CurDrive())



/// <summary>
/// Return the capacity of the specified disk.
/// </summary>
/// <param name="nDisk"></param>
/// <returns>
/// </returns>
function DiskSpace(nDisk as __Usual) as __Usual
	return System.IO.DriveInfo{nDisk}:TotalSize




