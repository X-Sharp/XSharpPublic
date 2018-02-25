//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


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
	FUNCTION DiskSpace(nDisk AS __Usual) AS __Usual
	RETURN System.IO.DriveInfo{nDisk}:TotalSize

	/// <summary>
	/// Return the last DOS error code associated with an activation of the runtime error block.
	/// </summary>
	/// <param name="nSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION DosError(nSet AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

