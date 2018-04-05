//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>
/// Return the last DOS error code  (Exit code) and set a new code.
/// </summary>
/// <param name="nSet">New value for the DOS eror code </param>
/// <returns>
/// </returns>
function DosError(nSet as dword) as dword
	local nOld as int
	nOld := System.Environment.ExitCode
	System.Environment.ExitCode := unchecked((int) nSet)
	return UNCHECKED((DWORD) nOld)

/// <summary>
/// Return the last DOS error code  (Exit code). use GetDosError() to fetch the error from the Last Win32 call.
/// </summary>
/// <param name="nSet"></param>
/// <returns>
/// </returns>
function DosError() as dword
	local nOld as int
	nOld := System.Environment.ExitCode
	return UNCHECKED((DWORD) nOld)



/// <summary>
/// Return the DOS error code from any application.
/// </summary>
/// <returns>
/// </returns>
function GetDosError() as dword
	return unchecked((dword) System.Runtime.InteropServices.Marshal.GetLastWin32Error())
	
/// <summary>
/// Retrieve the contents of a DOS environment variable.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function GetEnv(cVar as string) as string
	return System.Environment.GetEnvironmentVariable(cVar)

/// <summary>
/// Identify the current workstation.
/// </summary>
/// <returns>The workstation ID as a string.</returns>
function NetName() as string
	return System.Environment.MachineName

/// <summary>
/// Return the current Windows directory.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
function CurDir() as string
	local cDir as string
	local index as int
	cDir := System.Environment.CurrentDirectory
	index := cDir:Indexof(System.IO.Path.VolumeSeparatorChar)
	if index > 0
		cDir := cDir:Substring(index+1)
	endif
	if cDir[0] == System.IO.Path.DirectorySeparatorChar
		cDir := cDir:Substring(1)
	endif
	if cDir[cDir:Length-1]  ==System.IO.Path.DirectorySeparatorChar
		cDir := cDir:Substring(0, cDir:Length-1)
	endif
	return cDir


/// <summary>
/// Return the currently selected working directory.
/// </summary>
/// <returns>
/// </returns>
function WorkDir() as string
	local cPath as string
	local asm   as System.Reflection.Assembly
	asm := System.Reflection.Assembly.GetCallingAssembly()
	cPath := asm:ManifestModule:FullyQualifiedName
	cPath := System.IO.Path.GetDirectoryName(cPath)
	if cPath[cPath:Length-1] !=  System.IO.Path.DirectorySeparatorChar
		cPath += System.IO.Path.DirectorySeparatorChar:ToString()
	endif
	return cPath

/// <summary>
/// Return the space available on the current disk drive.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
function DiskFree() as Int64
	return DiskFree(CurDrive())


internal function DiskNo2DiskName(nDisk as INT) as string
	return ('A'+ (nDisk-1)):ToString()

/// <summary>
/// Return the space available on a specified disk.
/// </summary>
/// <param name="cDrive">The drivename to get the free space from.</param>
/// <returns>
/// The free space on the specified disk drive.
/// </returns>	   
function DiskFree(cDrive as STRING) as INT64
	return System.IO.DriveInfo{cDrive}:TotalFreeSpace


/// <summary>
/// Return the space available on a specified disk.
/// </summary>
/// <param name="nDrive">The drive number (1 = A, 2 = B etc)
/// <returns>
/// The free space on the specified disk drive.
/// </returns>	   
function DiskFree(nDrive as INT) as int64
	local cDrive as string
	cDrive := DiskNo2DiskName(nDrive)
	return DiskFree(cDrive)


/// <summary>
/// Return the capacity of the current disk.
/// </summary>
/// <returns>
/// The capacity of the current disk.
/// </returns>
function DiskSpace() as int64
	return DiskSpace(CurDrive())


/// <summary>
/// Return the capacity of the specified disk.
/// </summary>
/// <param name="nDisk"></param>
/// <returns>
/// </returns>
function DiskSpace(nDisk as INT) as int64
	local cDisk as string
	cDisk := DiskNo2DiskName(nDisk)
	return DiskSpace(cDisk)

/// <summary>
/// Return the capacity of the specified disk.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
function DiskSpace(cDisk as STRING) as int64
	return System.IO.DriveInfo{cDisk}:TotalSize

/// <summary>
/// Detect a concurrency conflict.
/// </summary>
/// <returns>
/// </returns>
FUNCTION NetErr() AS LOGIC STRICT
    RETURN RuntimeState.NetErr
    
/// <summary>
/// Detect a concurrency conflict.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION NetErr( lValue AS LOGIC ) AS LOGIC
    LOCAL curvalue := RuntimeState.NetErr AS LOGIC
    RuntimeState.NetErr := lValue
    RETURN curvalue




/// <summary>
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function LockTries() as int
	return RuntimeState.LockTries

/// <summary>
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
function LockTries(nValue as int) as int
	local nResult as int
	nResult := RuntimeState.LockTries
	RuntimeState.LockTries := nValue
	return nValue