//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.IO
/// <summary>
/// Return the last DOS error code  (Exit code) and set a new code.
/// </summary>
/// <param name="nSet">New value for the DOS eror code </param>
/// <returns>
/// </returns>
FUNCTION DosError(nSet AS DWORD) AS DWORD
	LOCAL nOld AS INT
	nOld := System.Environment.ExitCode
	System.Environment.ExitCode := UNCHECKED((INT) nSet)
	RETURN UNCHECKED((DWORD) nOld)

/// <summary>
/// Return the last DOS error code  (Exit code). use GetDosError() to fetch the error from the Last Win32 call.
/// </summary>
/// <param name="nSet"></param>
/// <returns>
/// </returns>
FUNCTION DosError() AS DWORD
	LOCAL nOld AS INT
	nOld := System.Environment.ExitCode
	RETURN UNCHECKED((DWORD) nOld)



/// <summary>
/// Return the DOS error code from any application.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetDosError() AS DWORD
	RETURN UNCHECKED((DWORD) System.Runtime.InteropServices.Marshal.GetLastWin32Error())
	
/// <summary>
/// Retrieve the contents of a DOS environment variable.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION GetEnv(cVar AS STRING) AS STRING
	RETURN System.Environment.GetEnvironmentVariable(cVar)


	/// <summary>
/// Update or replace the contents of a DOS environment variable.
/// </summary>
/// <param name="cVar"></param>
/// <param name="cValue"></param>
/// <returns>
/// </returns>
FUNCTION SetEnv(cVar AS STRING,cValue AS STRING) AS LOGIC
	RETURN SetEnv(cVar, cValue, FALSE)
/// <summary>
/// Update or replace the contents of a DOS environment variable.
/// </summary>
/// <param name="cVar"></param>
/// <param name="cValue"></param>
/// <param name="lAppend"></param>
/// <returns>
/// </returns>
FUNCTION SetEnv(cVar AS STRING,cValue AS STRING,lAppend AS LOGIC) AS LOGIC
	LOCAL result AS LOGIC
	TRY
        XSharp.IO.File.clearErrorState()

		IF lAppend
			LOCAL cOldValue AS STRING
			cOldValue := System.Environment.GetEnvironmentVariable(cVar)
			IF ! String.IsNullOrEmpty( cOldValue )
				cOldValue += ";"
			ENDIF
			cValue := cOldValue + cValue
		ENDIF
		System.Environment.SetEnvironmentVariable(cVar, cValue)
		result := System.Environment.GetEnvironmentVariable(cVar) == cValue
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
		result := FALSE
	END TRY
	RETURN result   



/// <summary>
/// Identify the current workstation.
/// </summary>
/// <returns>The workstation ID as a string.</returns>
FUNCTION NetName() AS STRING
	RETURN System.Environment.MachineName


/// <summary>
/// Return the current Windows directory.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
FUNCTION CurDir (cDisk AS STRING) AS STRING
	RETURN CurDir()

/// <summary>
/// Return the current Windows directory.
/// </summary>
/// <returns>
/// </returns>
FUNCTION CurDir() AS STRING
	LOCAL cDir AS STRING
	LOCAL index AS INT
	cDir := System.Environment.CurrentDirectory
	index := cDir:Indexof(Path.VolumeSeparatorChar)
	IF index > 0
		cDir := cDir:Substring(index+1)
	ENDIF
	IF cDir:Length > 0 .AND. cDir[0] == Path.DirectorySeparatorChar
		cDir := cDir:Substring(1)
	ENDIF
	IF cDir:Length > 0 .AND. cDir[cDir:Length-1]  == Path.DirectorySeparatorChar
		cDir := cDir:Substring(0, cDir:Length-1)
	ENDIF
	RETURN cDir

/// <summary>
/// Return the current Windows drive.
/// </summary>
/// <returns>
/// Return the letter of the current drive without colon
/// </returns>
FUNCTION CurDrive() AS STRING
	LOCAL currentDirectory := System.IO.Directory.GetCurrentDirectory() AS STRING
	LOCAL drive := "" AS STRING
	LOCAL position AS INT
	
	position := currentDirectory:IndexOf(System.IO.Path.VolumeSeparatorChar)
	IF position > 0
		drive := currentDirectory:Substring(0,position)
	ENDIF
	RETURN drive
	

/// <summary>
/// Return the currently selected working directory.
/// </summary>
/// <returns>
/// </returns>
FUNCTION WorkDir() AS STRING
	LOCAL cPath AS STRING
	LOCAL asm   AS System.Reflection.Assembly
	asm := System.Reflection.Assembly.GetCallingAssembly()
	cPath := asm:ManifestModule:FullyQualifiedName
	cPath := Path.GetDirectoryName(cPath)
	IF cPath[cPath:Length-1] !=  Path.DirectorySeparatorChar
		cPath += Path.DirectorySeparatorChar:ToString()
	ENDIF
	RETURN cPath

/// <summary>
/// Return the space available on the current disk drive.
/// </summary>
/// <param name="cDisk"></param>
/// <returns>
/// </returns>
FUNCTION DiskFree() AS INT64
	RETURN DiskFree(CurDrive())


INTERNAL FUNCTION DiskNo2DiskName(nDisk AS INT) AS STRING
    nDisk := _AND(nDisk, 0xFF)
    IF nDisk >= 0 .AND. nDisk <= 25
        LOCAL charDrive AS CHAR
        charDrive := (CHAR) ( 65 + (nDisk-1))
	    RETURN charDrive:ToString()
    ELSE
        RETURN CurDrive()
    ENDIF

/// <summary>
/// Return the space available on a specified disk.
/// </summary>
/// <param name="cDrive">The drivename to get the free space from.</param>
/// <returns>
/// The free space on the specified disk drive.
/// </returns>	   
FUNCTION DiskFree(cDrive AS STRING) AS INT64
    LOCAL result AS INT64
    IF String.IsNullOrEmpty(cDrive) 
        cDrive := CurDrive()
    ELSEIF cDrive:Length > 1
        cDrive := cDrive:Substring(0,1)+":"
    ENDIF
    TRY
        XSharp.IO.File.clearErrorState()
	    result :=  DriveInfo{cDrive}:TotalFreeSpace
    CATCH e AS Exception
        XSharp.IO.File.SetErrorState(e)
        result := 0
    END TRY
    RETURN result

/// <summary>
/// Return the space available on a specified disk.
/// </summary>
/// <param name="nDrive">The drive number (1 = A, 2 = B etc)</param>
/// <returns>
/// The free space on the specified disk drive.
/// </returns>	   
FUNCTION DiskFree(nDrive AS INT) AS INT64
	LOCAL cDrive AS STRING
	cDrive := DiskNo2DiskName(nDrive)
	RETURN DiskFree(cDrive)


/// <summary>
/// Return the current Windows drive.
/// </summary>
/// <returns>
/// Return the letter of the current drive without colon
/// </returns>
FUNCTION DiskName() AS STRING
	RETURN CurDrive()


/// <summary>
/// Return the capacity of the current disk.
/// </summary>
/// <returns>
/// The capacity of the current disk.
/// </returns>
FUNCTION DiskSpace() AS INT64
	RETURN DiskSpace(CurDrive())


/// <summary>
/// Return the capacity of the specified disk.
/// </summary>
/// <param name="nDisk"></param>
/// <returns>
/// </returns>
FUNCTION DiskSpace(nDisk AS INT) AS INT64
	LOCAL cDisk AS STRING
	cDisk := DiskNo2DiskName(nDisk)
	RETURN DiskSpace(cDisk)

/// <summary>
/// Return the capacity of the specified disk.
/// </summary>
/// <param name="cDrive"></param>
/// <returns>
/// </returns>
FUNCTION DiskSpace(cDrive AS STRING) AS INT64
    LOCAL result AS INT64
    IF String.IsNullOrEmpty(cDrive) 
        cDrive := CurDrive() 
    ELSEIF cDrive:Length > 1 
        cDrive := cDrive:Substring(0,1)+":"
    ENDIF
    TRY
        XSharp.IO.File.ClearErrorState()
	    result :=  DriveInfo{cDrive}:TotalSize
    CATCH e AS Exception
        XSharp.IO.File.SetErrorState(e)
        result := 0
    END TRY
    RETURN result

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
FUNCTION LockTries() AS DWORD
	RETURN RuntimeState.LockTries

/// <summary>
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION LockTries(nValue AS DWORD) AS DWORD
	LOCAL nResult AS DWORD
	nResult := RuntimeState.LockTries
	RuntimeState.LockTries := nValue
	RETURN nResult



/// <summary>
/// Change the current Windows directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirChange(cDir AS STRING) AS INT
	LOCAL result AS INT
	TRY
       XSharp.IO.File.clearErrorState()
		IF Directory.Exists(cDir)
			Directory.SetCurrentDirectory(cDir)
			result := 0
		ELSE
			result := 3 // Path not found
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
        result := (INT) XSharp.IO.File.errorCode
	END TRY
	RETURN result
	
/// <summary>
/// Create a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirMake(cDir AS STRING) AS INT
	LOCAL result AS INT
	TRY
        XSharp.IO.File.clearErrorState()
		IF !Directory.Exists(cDir)
			Directory.CreateDirectory(cDir)
			result := 0
		ELSE
			result := 183 // ERROR_ALREADY_EXISTS 
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
        result := (INT) XSharp.IO.File.errorCode
	END TRY
	RETURN result
	
/// <summary>
/// Remove a directory.
/// </summary>
/// <param name="pszDir"></param>
/// <returns>
/// </returns>
FUNCTION DirRemove(cDir AS STRING) AS INT
	LOCAL result AS INT
	TRY
        XSharp.IO.File.clearErrorState()
		IF Directory.Exists(cDir)
			Directory.Delete(cDir,FALSE)
			result := 0
		ELSE
			result := 2 // Cannot find file 
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
        result := (INT) XSharp.IO.File.errorCode
	END TRY
	RETURN result


/// <summary>
/// Change the current disk drive.
/// </summary>
/// <param name="pszDisk"></param>
/// <returns>
/// </returns>

FUNCTION DiskChange(c AS STRING) AS LOGIC
	IF String.IsNullOrEmpty(c)
		RETURN FALSE
	ENDIF
	c := c:Substring(0,1)+Path.VolumeSeparatorChar:ToString()+Path.DirectorySeparatorChar:ToString()
	RETURN DirChange(c) == 0


/// <summary>Return the operating system name.</summary>
/// <returns>
/// </returns>
FUNCTION OS() AS STRING
	RETURN OS(FALSE)

/// <summary>Return the operating system name.</summary>
/// <param name="lExtended"></param>
/// <returns>
/// </returns>
FUNCTION OS(lExtended AS LOGIC) AS STRING
    LOCAL cOs AS STRING
    VAR o := Environment.OSVersion
	cOs := o:ToString()
	IF !lExtended
		SWITCH o:Platform
		CASE PlatformID.Win32NT
			cOs := "Windows NT"
		CASE PlatformID.Win32S
			cOs := "Win32s on Windows 3.1"
		CASE PlatformID.Win32Windows
			IF o:Version:Major == 4
				IF o:Version:Minor == 0
					cOs := "Windows 95"
				ELSEIF o:Version:Minor == 10
					cOs := "Windows 98"
					IF o:Version:Build == 2222
						cOs := "Windows 98 SE"
					ENDIF
				ELSEIF o:Version:Minor == 90
					cOs := "Windows ME"
				ENDIF
			ENDIF
		CASE PlatformID.WinCE
			cOs := "Windows CE"
		OTHERWISE
			cOs := o:ToString()
		END SWITCH
	ENDIF
    RETURN cOs
 



FUNCTION TruePath( cFile AS STRING ) AS STRING PASCAL
    LOCAL           nPos        AS DWORD
    LOCAL           cPath       AS STRING

    nPos  := RAt("\", cFile)
    IF nPos = 0
        cPath := cFile
    ELSE
        cPath := cFile:SubString(0, (INT) nPos)
    ENDIF

    RETURN cPath

/// <exclude/>	
FUNCTION _ExecName() AS STRING
    RETURN System.Reflection.Assembly.GetExecutingAssembly():Location

FUNCTION ExecName( lFull AS LOGIC ) AS STRING
    LOCAL   nPos        AS DWORD
    LOCAL   cPath       AS STRING

    cPath := _ExecName()

    IF ! lFull
        nPos  := RAt( "\", cPath )

        IF nPos != 0
            cPath := SubStr2( cPath, nPos + 1 )
        ENDIF
    ENDIF

    RETURN cPath

FUNCTION ModuleName( lFull AS LOGIC ) AS STRING 
    RETURN ExecName( lFull )


FUNCTION GetMimeType(sFileName AS STRING) AS STRING
    LOCAL sExt AS STRING
	sExt := System.IO.Path.GetExtension(sFileName)
    RETURN Microsoft.Win32.Registry.GetValue("HKEY_CLASSES_ROOT\"+sExt,"Content Type",""):ToString()


/// <summary>
/// Returns the command line used to invoke the application.
/// </summary>
/// <returns>
/// </returns>
FUNCTION _GetCmdLine() AS STRING
RETURN System.Environment.CommandLine




/// <summary>Terminate application processing, close all open files, and return control to the operating system.</summary>
/// <remarks>This function can be used from anywhere in an application.
/// A RETURN executed from the Start() function can also be used to QUIT an application.</remarks>
FUNCTION _Quit() AS VOID
    System.Diagnostics.Process.GetCurrentProcess():CloseMainWindow()
    RETURN
