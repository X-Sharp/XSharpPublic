//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// File and Disk IO Functions

USING System
USING System.Collections
USING System.IO
USING System.Linq
USING System.Runtime.InteropServices
USING System.Security
USING Microsoft.Win32.SafeHandles
USING System.Runtime
USING System.Runtime.ConstrainedExecution


	
	/// <summary>
	/// Change the name of a file.
	/// </summary>
	/// <param name="cOldFile">The original file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.</param>
	/// <param name="cNewFile">The new file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.  If the source directory is different from the target directory, the file moves to the target directory.  If cNewFile exists or is currently open, FRename() fails and returns FALSE.</param>
	/// <returns>TRUE if the operation succeeds; otherwise, FALSE.  In the case of a failure, FError() can be used to determine the specific error.</returns>
FUNCTION FRename( cOldFile AS STRING , cNewFile AS STRING) AS LOGIC
	LOCAL renamed := FALSE AS LOGIC
	TRY
		System.IO.File.Move(cOldFile, cNewFile)
		renamed := TRUE
		CATCH 
		FError((DWORD)Marshal.GetLastWin32Error())
	END TRY
	RETURN renamed
	
	
	/// <summary>
	/// Delete a file from disk.
	/// </summary>
	/// <param name="cFile">The file name, including an optional drive, directory, and extension.  SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory as part of the file name.  No extension is assumed.</param>
	/// <returns>TRUE if the operation succeeds; otherwise, FALSE.  In the case of a failure, FError() can be used to determine the specific error.</returns>
FUNCTION FErase(fileName AS STRING) AS LOGIC
	LOCAL isDeleted := FALSE AS LOGIC
	TRY
		System.IO.File.Delete(fileName)
		isDeleted := TRUE
		CATCH 
		FError((DWORD)Marshal.GetLastWin32Error())
		isDeleted := FALSE
	END TRY
	RETURN isDeleted
	
	/// <summary>Copy a file to a new file or to a device.</summary>
	/// <param name="cSourceFile">The name of the source file to copy, including an optional drive, directory, and extension.</param>
	/// <param name="cTargetFile">The name of the target file, including an optional drive, directory, and extension.</param>
	/// <returns>TRUE if successful; otherwise, FALSE.</returns>
	/// <remarks>
	/// FCopy() is the functional form of the COPY FILE command.
	/// If cSourceFile does not exist, a runtime error is raised.  
	/// If cTargetFile does not exist, it is created.  
	///</remarks>
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING) AS LOGIC
	RETURN FCopy(cSourceFile, cTargetFile, TRUE)
	
	/// <summary>Copy a file to a new file or to a device.</summary>
	/// <param name="cSourceFile">The name of the source file to copy, including an optional drive, directory, and extension.</param>
	/// <param name="cTargetFile">The name of the target file, including an optional drive, directory, and extension.</param>
	/// <param name="lOverWrite">Should the target file be overwritten.</param>
	/// <returns>TRUE if successful; otherwise, FALSE.</returns>
	/// <remarks>
	/// FCopy() is the functional form of the COPY FILE command.
	/// If cSourceFile does not exist, a runtime error is raised.  
	/// If cTargetFile does not exist, it is created.  If it exists it is only overwritten if lOverWrite = TRUE
	///</remarks>
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING, lOverWrite AS LOGIC) AS LOGIC
	LOCAL IsCopied := TRUE AS LOGIC
	TRY
		System.IO.File.Copy(cSourceFile,cTargetFile,lOverWrite)
		CATCH 
		FError((DWORD)Marshal.GetLastWin32Error())
		IsCopied := FALSE
	END TRY
	RETURN IsCopied
	
	
	
	/// <summary>
	/// Break a path name into its components.
	/// </summary>
	/// <param name="cPath"></param>
	/// <param name="cDrive"></param>
	/// <param name="cDir"></param>
	/// <param name="cName"></param>
	/// <param name="cExt"></param>
	/// <returns>
	/// </returns>
FUNCTION _SplitPath(cPath AS STRING, cDrive OUT STRING,cDir OUT STRING,cName OUT STRING,cExt OUT STRING) AS VOID
	LOCAL nPos AS LONG
	LOCAL cSep AS STRING
	cDrive	:= ""
	cDir	:= ""
	cName	:= ""
	cExt	:= ""
	IF String.IsNullOrEmpty(cPath)
		RETURN
	ENDIF
	cSep := Path.DirectorySeparatorChar:ToString()
	nPos := cPath:IndexOf(Path.VolumeSeparatorChar)
	IF nPos > 0
		cDrive := cPath:Substring(0, nPos+1)
		cPath  := cPath:SubString(nPos + 1)
	ENDIF
	
	IF cPath:Trim() != ""
		cDir := Path.GetDirectoryName(cPath)
	ENDIF
	
	IF String.IsNullOrEmpty( cDir )
		IF cPath:StartsWith(cSep)
			cDir := cSep
		ELSE
			cDir := ""
		ENDIF
	ELSEIF ! cDir:EndsWith(cSep)
		cDir += cSep
	ENDIF
	
	cName := Path.GetFileNameWithoutExtension(cPath)
	cExt  := Path.GetExtension(cPath)
	
	RETURN
	
