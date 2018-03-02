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
/// THROW NotImplementedException{}
return String.Empty   