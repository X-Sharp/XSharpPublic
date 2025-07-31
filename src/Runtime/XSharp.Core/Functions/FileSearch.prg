//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.IO
USING System.Linq
/// <summary>This delegate is used when you want to implement your own function for the logic behind the File() function. You can register your own FileSearcher by calling the RegisterFileSearch() function.</summary>
/// <seealso cref='RegisterFileSearch' >RegisterFileSearch() Function</seealso>

PUBLIC DELEGATE XSharp.FileSearcher(cIn AS STRING) AS STRING
INTERNAL STATIC CLASS XSharp.FileSearch
    STATIC PRIVATE foundEntries	:= List<OBJECT>{} AS List<OBJECT>
    STATIC PRIVATE enumerator   := NULL AS IEnumerator<OBJECT>
    STATIC PRIVATE currentItem	:= NULL AS OBJECT
    STATIC PRIVATE isAtEnd		:= TRUE AS LOGIC
    STATIC Worker := FileSearchWorker AS FileSearcher

    CONST timeFormat := "HH:MM:ss" AS STRING

    INTERNAL STATIC METHOD FFCount( filespec AS STRING , attributes AS DWORD ) AS DWORD
        FindFirst(filespec,attributes)
        RETURN (DWORD)foundEntries:Count

    INTERNAL STATIC METHOD FindFirst( filespec AS STRING , attributes AS DWORD ) AS LOGIC
        LOCAL lRet := FALSE AS LOGIC

        TRY

            XSharp.IO.File.ClearErrorState()

            // Split filespec in path and mask
            // when path is empty then path is current directory
            // make sure that we only search in the given path
            LOCAL cPath AS STRING
            LOCAL cMask AS STRING
            cPath := Path.GetDirectoryName(filespec)
            cMask := Path.GetFileName(filespec)
            IF String.IsNullOrEmpty(cPath)
                cPath := System.Environment.CurrentDirectory
            ENDIF
            foundEntries:Clear()
            IF attributes == FA_VOLUME
                LOCAL allDrives := DriveInfo.GetDrives() AS DriveInfo[]
                FOREACH drive AS DriveInfo IN allDrives
                    foundEntries:Add(drive)
                NEXT
            ELSE
                LOCAL oDirInfo AS DirectoryInfo
                oDirInfo := DirectoryInfo{cPath}

                LOCAL files := oDirInfo:GetFiles(cMask) AS FileInfo[]
                //VAR selectedFiles := FROM FileInfo IN files WHERE ( FileInfo:Attributes & (FileAttributes) (attributes + FA_NORMAL)) != 0 SELECT FileInfo
                FOREACH file AS FileInfo IN files
                    // extra check because sometimes short file names are matched where the long file name does not match
                    IF Like(cMask,file:Name)
                        foundEntries:Add(file)
                    ENDIF
                NEXT

                IF (attributes & FA_DIRECTORY) == (DWORD) FA_DIRECTORY
                    LOCAL directories := oDirInfo:GetDirectories(cMask) AS FileSystemInfo[]
                    VAR selectedDirs := FROM DirectoryInfo IN directories WHERE (DirectoryInfo:Attributes & (FileAttributes) (attributes + FA_NORMAL) ) != 0 SELECT DirectoryInfo
                    FOREACH directory AS DirectoryInfo IN selectedDirs
                        IF Like(cMask,directory:Name)
                            foundEntries:Add(directory)
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            enumerator := foundEntries:GetEnumerator()
            enumerator:Reset()
            isAtEnd := !enumerator:MoveNext()
            IF  !isAtEnd
                currentItem := enumerator:Current
            ENDIF
            lRet := foundEntries:Count > 0

        CATCH oEx AS Exception

            XSharp.IO.File.SetErrorState(oEx)
            lRet := FALSE

        END TRY

    RETURN lRet

    INTERNAL STATIC METHOD FindNext() AS LOGIC
        IF !isAtEnd
            isAtEnd := !enumerator:MoveNext()
            IF !isAtEnd
                currentItem := enumerator:Current
            ENDIF
        ENDIF
        RETURN !isAtEnd

    INTERNAL STATIC METHOD FName() AS STRING
        LOCAL name := "" AS STRING
        IF !isAtEnd
            IF (currentItem IS DriveInfo)
                name := ((DriveInfo)currentItem):Name
            ELSEIF (currentItem IS FileInfo)
                name := ((FileInfo)currentItem):Name
            ELSEIF (currentItem IS DirectoryInfo)
                name := ((DirectoryInfo) currentItem):Name
            ENDIF
        ENDIF
        RETURN name

    INTERNAL STATIC METHOD FSize() AS DWORD
        LOCAL size := 0 AS INT
        IF !isAtEnd
            IF currentItem IS DriveInfo
                IF ((DriveInfo) currentItem):IsReady
                    size := (INT)((DriveInfo) currentItem):TotalSize
                ENDIF
            ELSEIF (currentItem IS FileInfo)
                size := (INT)((FileInfo)currentItem):Length
            ELSEIF currentItem IS DirectoryInfo
                size := (INT)((DirectoryInfo) currentItem):GetFileSystemInfos().LongLength
            ENDIF
        ENDIF
        RETURN (DWORD) size

    INTERNAL STATIC METHOD FTime() AS STRING
        LOCAL time := "00:00:00" AS STRING
        IF !isAtEnd
            IF (currentItem IS FileInfo)
                time := ((FileInfo)currentItem):LastWriteTime.ToString(timeFormat)
            ELSEIF currentItem IS DirectoryInfo
                time := ((DirectoryInfo) currentItem):LastWriteTime.ToString(timeFormat)
            ENDIF
        ENDIF
        RETURN  time

    INTERNAL STATIC METHOD FDate() AS DateTime
        LOCAL time := DateTime.MinValue AS DateTime
        IF !isAtEnd
            IF (currentItem IS FileInfo)
                time := ((FileInfo)currentItem):LastWriteTime
            ELSEIF currentItem IS DirectoryInfo
                time := ((DirectoryInfo) currentItem):LastWriteTime
            ENDIF
        ENDIF
        RETURN  time


    INTERNAL STATIC METHOD FAttrib() AS DWORD
        LOCAL attributes := 0x00000008 AS INT
        IF !isAtEnd
            IF (currentItem IS FileInfo)
                attributes := (INT)((FileInfo)currentItem):Attributes
            ELSEIF currentItem IS DirectoryInfo
                attributes := (INT)((DirectoryInfo) currentItem):Attributes
            ENDIF
        ENDIF
        RETURN  (DWORD)attributes

        END CLASS

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ffcount/*" />
FUNCTION FFCount(pszFileSpec AS STRING,dwAttributes AS DWORD) AS DWORD
    RETURN XSharp.FileSearch.FFCount(pszFileSpec,dwAttributes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ffirst/*" />
FUNCTION FFirst(pszFileSpec AS STRING,kAttributes AS DWORD) AS LOGIC
    RETURN XSharp.FileSearch.FindFirst(pszFileSpec,kAttributes)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fattrib/*" />
FUNCTION FAttrib() AS DWORD
    RETURN XSharp.FileSearch.FAttrib()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fdate/*" />
FUNCTION FDate() AS DateTime
    RETURN XSharp.FileSearch.FDate()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fname/*" />
FUNCTION FName() AS STRING
    RETURN XSharp.FileSearch.FName()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fnext/*" />
FUNCTION FNext() AS LOGIC
    RETURN XSharp.FileSearch.FindNext()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fsize/*" />
FUNCTION FSize() AS DWORD
    RETURN XSharp.FileSearch.FSize()

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ftime/*" />
FUNCTION FTime() AS STRING
    RETURN XSharp.FileSearch.FTime()

// Worker function with normal file search worker
INTERNAL FUNCTION FileSearchWorker(cFile AS STRING) AS STRING
    LOCAL lFirst AS LOGIC
    IF cFile:IndexOf("\") >= 0  .and. System.IO.File.Exists(cFile)
        RETURN Path.GetFullPath( cFile )
    ENDIF
    LOCAL aPaths AS STRING[]
    aPaths := __GetSearchPaths()
    lFirst := TRUE
    FOREACH cPath AS STRING IN aPaths
        VAR cTemp := System.IO.Path.Combine(cPath, cFile)
        IF lFirst
            // store the first path that we looked in even when the file is not found
            // to be compatible with VO
            RuntimeState.LastFound := cTemp
            lFirst := FALSE
        ENDIF
        IF System.IO.File.Exists(cTemp)
            RuntimeState.LastFound := cTemp
            RETURN cTemp
        ENDIF
    NEXT
    RETURN ""

    /// <summary>
    /// Register Worker function for File Search API.
    /// </summary>
    /// <param name="newWorker">Function that implements the worker. Must implement the FileSearcher delegate, so take STRING parameter and return a STRING</param>
    /// <returns>
    /// current Worker function
    /// </returns>
    /// <seealso cref="FileSearcher">FileSearcher Delegate </seealso>
FUNCTION RegisterFileSearch(newWorker AS FileSearcher) AS FileSearcher
    LOCAL oldWorker AS FileSearcher
    oldWorker := XSharp.FileSearch.Worker
    IF newWorker != NULL
        XSharp.FileSearch.Worker := newWorker
    ENDIF
    RETURN oldWorker

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/file/*" />
FUNCTION File(cFileSpec AS STRING) AS LOGIC
    LOCAL lHasWildCards AS LOGIC
    LOCAL aPaths AS STRING[]
    LOCAL lFirst AS LOGIC
    TRY
        cFileSpec := cFileSpec?:Trim()
        IF String.IsNullOrEmpty(cFileSpec)
            RETURN FALSE
        ENDIF
        lHasWildCards := cFileSpec:IndexOfAny( <CHAR>{ '*', '?' } ) > 0
        RuntimeState.LastFound := ""
        IF ! lHasWildCards
            VAR cFound := XSharp.FileSearch.Worker(cFileSpec)
            IF ! String.IsNullOrEmpty(cFound)
                RuntimeState.LastFound := cFound
                RETURN TRUE
            ENDIF
        ELSE
            // wildcard, so use Directory.GetFiles()
            LOCAL files     AS STRING[]

            IF Path.IsPathRooted(cFileSpec)
                files := Directory.GetFiles( Path.GetDirectoryName( cFileSpec ), Path.GetFileName( cFileSpec ) )
                IF files:Length > 0
                    RuntimeState.LastFound := files[0]
                    RETURN TRUE
                ELSE
                    // store the first path that we looked in even when the file is not found
                    // to be compatible with VO
                    RuntimeState.LastFound := cFileSpec
                    RETURN FALSE
                ENDIF
            ELSE
                // Look in current directory first and if that fails through the whole normal search list
                IF __FileHelper(Environment.CurrentDirectory, cFileSpec, FALSE )
                    RETURN TRUE
                ENDIF
                aPaths := __GetSearchPaths()
                lFirst := TRUE
                FOREACH cPath AS STRING IN aPaths
                    IF __FileHelper(cPath, cFileSpec, lFirst )
                        RETURN TRUE
                    ENDIF
                    lFirst := FALSE
                NEXT
            ENDIF
        ENDIF
    CATCH e AS Exception
        XSharp.IO.File.SetErrorState(e)
    END TRY
    RETURN FALSE


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fpathname/*" />
FUNCTION FPathName() AS STRING
    RETURN RuntimeState.LastFound


INTERNAL FUNCTION __FileHelper(cPath AS STRING, cFileSpec AS STRING, lSavePath AS LOGIC) AS LOGIC
    LOCAL cTemp AS STRING
    LOCAL cFile AS STRING
    LOCAL files AS STRING[]
    IF ! System.IO.Directory.Exists(cPath)
        RETURN FALSE
    ENDIF
    cTemp := System.IO.Path.Combine(cPath, cFileSpec)
    cPath := System.IO.Path.GetDirectoryName(cTemp)
    cFile := System.IO.Path.GetFileName(cTemp)
    IF ! System.IO.Directory.Exists(cPath)
        RETURN FALSE
    ENDIF
    files := System.IO.Directory.GetFiles(cPath, cFile)
    IF files:Length > 0
        RuntimeState.LastFound := files[0]
    ELSEIF lSavePath
        RuntimeState.LastFound := cTemp
    ENDIF
    RETURN files:Length > 0


INTERNAL FUNCTION __GetSearchPaths() AS STRING[]
// Not found, now use the path settings from SetDefault and SetPath()
// if SetPath() is empty then we look through the Environment variable Path
LOCAL aDefault AS STRING[]
aDefault := SetPathArray()
IF aDefault != NULL
    IF aDefault:Length > 0
        #pragma options("az", off)
        aDefault[aDefault:Length]  := System.Environment.CurrentDirectory // Current dir might have changed since the last call
        #pragma options("az", default)
    END IF
    RETURN aDefault
ENDIF

VAR aPaths := List<STRING>{}
VAR cPath  := SetDefault()
IF !String.IsNullOrEmpty(cPath)
    aPaths:Add(cPath)
ENDIF
cPath := SetPath()
IF String.IsNullOrEmpty(cPath)
    cPath := GetEnv("PATH")
ENDIF
IF !String.IsNullOrEmpty(cPath)
    VAR aElements := cPath:Split(<CHAR>{ ';' }, StringSplitOptions.RemoveEmptyEntries )
    aPaths:AddRange(aElements)
ENDIF
aPaths:Add(System.Environment.CurrentDirectory) // Always add the current directory as the last element
aDefault := aPaths:ToArray()
SetPathArray(aDefault)
RETURN aDefault
