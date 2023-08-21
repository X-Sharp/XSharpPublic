//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
USING System.IO

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/doserror/*" />
FUNCTION DosError(nNewDosCode AS DWORD) AS DWORD
	LOCAL nOld AS INT
	nOld := System.Environment.ExitCode
	System.Environment.ExitCode := UNCHECKED((INT) nNewDosCode)
	RETURN UNCHECKED((DWORD) nOld)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/doserror/*" />
FUNCTION DosError() AS DWORD
	LOCAL nOld AS INT
	nOld := System.Environment.ExitCode
	RETURN UNCHECKED((DWORD) nOld)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getdoserror/*" />
FUNCTION GetDosError() AS DWORD
	RETURN UNCHECKED((DWORD) System.Runtime.InteropServices.Marshal.GetLastWin32Error())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getenv/*" />
FUNCTION GetEnv(cEnvVariable AS STRING) AS STRING
	RETURN System.Environment.GetEnvironmentVariable(cEnvVariable)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setenv/*" />

FUNCTION SetEnv(cVar AS STRING,cValue AS STRING) AS LOGIC
	RETURN SetEnv(cVar, cValue, FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setenv/*" />
FUNCTION SetEnv(cVar AS STRING,cValue AS STRING,lAppend AS LOGIC) AS LOGIC
	LOCAL result AS LOGIC
	TRY
        XSharp.IO.File.ClearErrorState()

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
		XSharp.IO.File.SetErrorState(e)
		result := FALSE
	END TRY
	RETURN result



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/netname/*" />

FUNCTION NetName() AS STRING
	RETURN System.Environment.MachineName


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/curdir/*" />
FUNCTION CurDir (cDrive AS STRING) AS STRING
	RETURN CurDir()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/curdir/*" />
FUNCTION CurDir() AS STRING
	LOCAL cDir AS STRING
	LOCAL index AS INT
	cDir := System.Environment.CurrentDirectory
	index := cDir:IndexOf(Path.VolumeSeparatorChar)
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/curdrive/*" />
FUNCTION CurDrive() AS STRING
	LOCAL currentDirectory := System.IO.Directory.GetCurrentDirectory() AS STRING
	LOCAL drive := "" AS STRING
	LOCAL position AS INT

	position := currentDirectory:IndexOf(System.IO.Path.VolumeSeparatorChar)
	IF position > 0
		drive := currentDirectory:Substring(0,position)
	ENDIF
	RETURN drive


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/workdir/*" />
FUNCTION WorkDir() AS STRING
	LOCAL cPath AS STRING
	LOCAL asm   AS System.Reflection.Assembly
	asm := System.Reflection.Assembly.GetEntryAssembly()
    if asm == null // during unit tests
        asm := System.Reflection.Assembly.GetExecutingAssembly()
    endif
	cPath := asm:Location
	cPath := Path.GetDirectoryName(cPath)
	IF cPath[cPath:Length-1] !=  Path.DirectorySeparatorChar
		cPath += Path.DirectorySeparatorChar:ToString()
	ENDIF
	RETURN cPath

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskfree/*" />
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskfree/*" />
FUNCTION DiskFree(cDrive AS STRING) AS INT64
    LOCAL result AS INT64
    IF String.IsNullOrEmpty(cDrive)
        cDrive := CurDrive()
    ELSEIF cDrive:Length > 1
        cDrive := cDrive:Substring(0,1)+":"
    ENDIF
    TRY
        XSharp.IO.File.ClearErrorState()
	    result :=  DriveInfo{cDrive}:TotalFreeSpace
    CATCH e AS Exception
        XSharp.IO.File.SetErrorState(e)
        result := 0
    END TRY
    RETURN result

/// <inheritdoc cref="DiskFree" />
/// <param name="nDrive">The number of the disk drive to query, where 1 is drive A, 2 is B, 3 is C, and so on. </param>
FUNCTION DiskFree(nDrive AS INT) AS INT64
	RETURN DiskFree(DiskNo2DiskName(nDrive))


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskname/*" />
FUNCTION DiskName() AS STRING
	RETURN CurDrive()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskspace/*" />
FUNCTION DiskSpace() AS INT64
	RETURN DiskSpace(CurDrive())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskspace/*" />
FUNCTION DiskSpace(nDrive AS INT) AS INT64
	LOCAL cDisk AS STRING
	cDisk := DiskNo2DiskName(nDrive)
	RETURN DiskSpace(cDisk)

/// <inheritdoc cref="DiskSpace" />
/// <param name="cDrive">The name of the drive as a string, for example "C:", "A:". If you do not specify a drive, the Windows default is used.</param>
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/neterr/*" />
FUNCTION NetErr() AS LOGIC STRICT
    RETURN RuntimeState.NetErr

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/neterr/*" />
FUNCTION NetErr( lNewError AS LOGIC ) AS LOGIC
    LOCAL curvalue := RuntimeState.NetErr AS LOGIC
    RuntimeState.NetErr := lNewError
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


INTERNAL FUNCTION BadFileParam(cFunction AS STRING, cParam AS STRING, nParam AS DWORD) AS VOID
    RuntimeState.FileError      := FERROR_PARAM
    RuntimeState.FileException  := Error.ArgumentError(cFunction,cParam, nParam,"Empty file or directoryname is not allowed")
    RuntimeState.NetErr         := FALSE
RETURN




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirchange/*" />
FUNCTION DirChange(pszDir AS STRING) AS INT
	LOCAL result AS INT
	TRY
		XSharp.IO.File.ClearErrorState()
		IF String.IsNullOrEmpty(pszDir)
		   BadFileParam(__FUNCTION__, nameof(pszDir), 1)
		   RETURN -1
		ENDIF
		IF Directory.Exists(pszDir)
			Directory.SetCurrentDirectory(pszDir)
			result := 0
		ELSE
			result := 3 // Path not found
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
        result := (INT) RuntimeState.FileError
	END TRY
	RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirmake/*" />
FUNCTION DirMake(pszNewDir AS STRING) AS INT
	LOCAL result AS INT
	TRY
        XSharp.IO.File.ClearErrorState()
		XSharp.IO.File.ClearErrorState()
		IF String.IsNullOrEmpty(pszNewDir)
		   BadFileParam(__FUNCTION__, nameof(pszNewDir), 1)
		   RETURN -1
		ENDIF
		IF !Directory.Exists(pszNewDir)
			Directory.CreateDirectory(pszNewDir)
			result := 0
		ELSE
			result := 183 // ERROR_ALREADY_EXISTS
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
        result := (INT) RuntimeState.FileError
	END TRY
	RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dirremove/*" />
FUNCTION DirRemove(pszDirName AS STRING) AS INT
	LOCAL result AS INT
	TRY
		XSharp.IO.File.ClearErrorState()
		IF String.IsNullOrEmpty(pszDirName)
		   BadFileParam(__FUNCTION__, nameof(pszDirName), 1)
		   RETURN -1
		ENDIF
		IF Directory.Exists(pszDirName)
			Directory.Delete(pszDirName,FALSE)
			result := 0
		ELSE
			result := 2 // Cannot find file
		ENDIF
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
        result := (INT) RuntimeState.FileError
	END TRY
	RETURN result


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/diskchange/*" />
FUNCTION DiskChange(pszDrive AS STRING) AS LOGIC
	IF String.IsNullOrEmpty(pszDrive)
		RETURN FALSE
	ENDIF
	pszDrive := pszDrive:Substring(0,1)+Path.VolumeSeparatorChar:ToString()+Path.DirectorySeparatorChar:ToString()
	RETURN DirChange(pszDrive) == 0


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/os/*" />
FUNCTION OS() AS STRING
	RETURN OS(FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/os/*" />
FUNCTION OS(lExtended AS LOGIC) AS STRING
    IF RuntimeState.RunningOnWindows
        RETURN ReadOsVersionFromRegistry(lExtended)
    ENDIF
    VAR o   := Environment.OSVersion
    VAR cOs := o:ToString()
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



FUNCTION ReadOsVersionFromRegistry(lExtended AS LOGIC) AS STRING

    VAR registryKey := Microsoft.Win32.Registry.LocalMachine:OpenSubKey("Software\\Microsoft\\Windows NT\\CurrentVersion")
	VAR cOS := (STRING)registryKey:GetValue("productName")
    IF lExtended
        VAR os := Environment.OSVersion
	    VAR pa := Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE")
	    VAR isx86 := pa:ToLower():Contains("x86")
	    cOS += " (" + IIF(isx86, "x86","x64") + i") ( Version {os.Version.Major}.{os.Version.Minor}, Build {os.Version.Build} )"
    ENDIF
    RETURN cOS

FUNCTION IsRunningOnWindows() AS LOGIC
    RETURN RuntimeState.RunningOnWindows


FUNCTION TruePath( cFile AS STRING ) AS STRING PASCAL
    LOCAL           nPos        AS DWORD
    LOCAL           cPath       AS STRING

    nPos  := RAt("\", cFile)
    IF nPos = 0
        cPath := cFile
    ELSE
        cPath := cFile:Substring(0, (INT) nPos)
    ENDIF

    RETURN cPath

/// <exclude/>
FUNCTION _ExecName() AS STRING
    RETURN System.Reflection.Assembly.GetEntryAssembly():Location

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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_getcmdline/*" />
FUNCTION _GetCmdLine() AS STRING
RETURN System.Environment.CommandLine



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_getinst/*" />
FUNCTION _GetInst() AS IntPtr
    LOCAL oType AS System.Type
    oType := Typeof(Functions)
    RETURN System.Runtime.InteropServices.Marshal.GetHINSTANCE(oType:Module)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_getcmdshow/*" />
[Obsolete];
FUNCTION _GetCmdShow() AS LONG
    RETURN 1

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/_getprevinst/*" />
[Obsolete];
FUNCTION _GetPrevInst AS IntPtr
    RETURN IntPtr.Zero


/// <summary>Terminate application processing, close all open files, and return control to the operating system.</summary>
/// <remarks>This function can be used from anywhere in an application.
/// A RETURN executed from the Start() function can also be used to QUIT an application.</remarks>
FUNCTION _Quit() AS VOID
    TRY
        CoreDb.CloseAll()
    CATCH
        NOP // Ignore errors here, after all we are quitting
    END TRY
    System.Diagnostics.Process.GetCurrentProcess():Kill()
    RETURN
