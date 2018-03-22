//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// File and Disk IO Functions

using System
using System.Collections
using System.IO
using System.Linq
using System.Runtime.InteropServices
using System.Security
using Microsoft.Win32.SafeHandles
using System.Runtime
using System.Runtime.ConstrainedExecution

/// <summary>
/// Return the current Windows drive.
/// </summary>
/// <returns>
/// Return the letter of the current drive without colon
/// </returns>
function CurDrive() as string
	local currentDirectory := System.IO.Directory.GetCurrentDirectory() as string
	local drive := "" as string
	local position as int
	
	position := currentDirectory:IndexOf(System.IO.Path.VolumeSeparatorChar)
	if position > 0
		drive := currentDirectory:Substring(0,position)
	endif
	return drive



/// <summary>
/// Return the currently selected working directory.
/// </summary>
/// <returns>
/// </returns>
function WorkDir() as string
	return System.Environment.CurrentDirectory


/// <summary>
/// Change the name of a file.
/// </summary>
/// <param name="cOldFile">The original file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.</param>
/// <param name="cNewFile">The new file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.  If the source directory is different from the target directory, the file moves to the target directory.  If <cNewFile> exists or is currently open, FRename() fails and returns FALSE.</param>
/// <returns>TRUE if the operation succeeds; otherwise, FALSE.  In the case of a failure, FError() can be used to determine the specific error.</returns>
function FRename( cOldFile as string , cNewFile as string) as logic
	local renamed := true as logic
	try
		System.IO.File.Move(cOldFile, cNewFile)
	catch e as Exception
		System.Diagnostics.Trace.WriteLine(e:Message)
		FError((dword)Marshal.GetLastWin32Error())
		renamed := false
	end try
	return renamed


/// <summary>
/// Delete a file from disk.
/// </summary>
/// <param name="cFile">The file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.</param>
/// <returns>TRUE if the operation succeeds; otherwise, FALSE.  In the case of a failure, FError() can be used to determine the specific error.</returns>
function FErase(fileName as string) as logic
	local isDeleted := false as logic
	try
		if System.IO.File.Exists(fileName)
			System.IO.File.Delete(fileName)
			isDeleted := true
		else
			isDeleted := false
		endif
	catch 
		FError((dword)Marshal.GetLastWin32Error())
		isDeleted := false
	end try
	return isDeleted

/// <summary>Copy a file to a new file or to a device.</summary>
/// <param name="cSourceFile">The name of the source file to copy, including an optional drive, directory, and extension.</param>
/// <param name="cTargetFile">The name of the target file, including an optional drive, directory, and extension.</param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <remarks>
/// FCopy() is the functional form of the COPY FILE command.
/// If <cSourceFile> does not exist, a runtime error is raised.  
/// If <cTargetFile> does not exist, it is created.  
///</remarks>
function FCopy(cSourceFile as string,cTargetFile as string) as logic
	local IsCopied := true as logic
	try
		System.IO.File.Copy(cSourceFile,cTargetFile,true)
	catch 
		FError((dword)Marshal.GetLastWin32Error())
		IsCopied := false
	end try
	return IsCopied
