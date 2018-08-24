//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.IO
USING System.Linq

INTERNAL STATIC CLASS XSharp.FileSearch
    STATIC PRIVATE foundEntries	:= List<OBJECT>{} AS List<OBJECT>
    STATIC PRIVATE enumerator   := NULL AS IEnumerator<OBJECT>
    STATIC PRIVATE currentItem	:= NULL AS OBJECT
    STATIC PRIVATE isAtEnd		:= TRUE AS LOGIC
    CONST timeFormat := "HH:MM:ss" AS STRING
    
    INTERNAL STATIC METHOD FFCount( filespec AS STRING , attributes AS DWORD ) AS DWORD
        FindFirst(filespec,attributes)
        RETURN (DWORD)foundEntries:Count
        
    INTERNAL STATIC METHOD FindFirst( filespec AS STRING , attributes AS DWORD ) AS LOGIC
        // Split filespec in path and mask
        // when path is empty then path is current directory
        // make sure that we only search in the given path
        LOCAL cPath AS STRING
        LOCAL cMask AS STRING
        cPath := Path.GetDirectoryName(filespec)
        cMask := Path.GetFilename(filespec)
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
            IF (attributes & FA_DIRECTORY) == FA_DIRECTORY
                LOCAL directories := oDirInfo:GetDirectories(cMask) AS FileSystemInfo[]
                attributes -= (INT) FA_DIRECTORY
                attributes += (INT) FA_NORMAL
                VAR selectedDirs := FROM DirectoryInfo IN directories WHERE (DirectoryInfo:Attributes & (FileAttributes) attributes ) != 0 SELECT DirectoryInfo
                FOREACH directory AS DirectoryInfo IN selectedDirs
                    foundEntries:Add(directory)
                NEXT
            ELSE
                attributes += (INT) FA_NORMAL
                LOCAL files := oDirInfo:GetFiles(filespec) AS FileInfo[]
                VAR selectedFiles := FROM FileInfo IN files WHERE ( FileInfo:Attributes & (FileAttributes) attributes) != 0 SELECT FileInfo
                FOREACH file AS FileInfo IN files
                    foundEntries:Add(file)
                NEXT
            ENDIF
        ENDIF
        enumerator := foundEntries:GetEnumerator()
        enumerator:Reset()
        isAtEnd := !enumerator:MoveNext()
        IF  !isAtEnd
            currentItem := enumerator:Current
        ENDIF
        RETURN (foundEntries:Count > 0)
        
    INTERNAL STATIC METHOD FindNext() AS LOGIC
        IF !isAtEnd
            isAtEnd := enumerator:MoveNext()
            IF !isAtEnd
                currentItem := enumerator:Current
            ENDIF
        ENDIF
        RETURN isAtEnd
        
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
                size := (INT)((DriveInfo) currentItem):TotalSize
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
/// <summary>
/// Return the number of files that match a given file specification and attribute.
/// </summary>
/// <param name="cFile"></param>
/// <param name="nAttr"></param>
/// <returns>The number of files that match a given file specification and attribute.
/// </returns>
FUNCTION FFCount(cFile AS STRING,nAttr AS DWORD) AS DWORD
    RETURN XSharp.FileSearch.FFCount(cFile,nAttr)
    
    /// <summary>
    /// Find the first file that matches a given file specification or attribute.
    /// </summary>
    /// <param name="cFile"></param>
    /// <param name="nAttr"></param>
    /// <returns>The first file that matches a given file specification or attribute.
    /// </returns>
FUNCTION FFirst(cFile AS STRING,nAttr AS DWORD) AS LOGIC
    RETURN XSharp.FileSearch.FindFirst(cFile,nAttr)
    /// <summary>
    /// Determine the attributes of the file found after FFCount(), FFirst(), or FNext().
    /// </summary>
    /// <returns>The attributes of the file found after FFCount(), FFirst(), or FNext().
    /// </returns>
FUNCTION FAttrib() AS DWORD
    RETURN XSharp.FileSearch.FAttrib()
    
    
    /// <summary>
    /// Return the Date stamp of the file found by FFCount(), FFirst(), or FNext().
    /// </summary>
    /// <returns>The Date stamp of the file found by FFCount(), FFirst(), or FNext().
    /// </returns>
FUNCTION FDate() AS DateTime
    RETURN XSharp.FileSearch.FDate()
    
    /// <summary>
    /// Return the name of the file found by FFCount(), FFirst(), or FNext().
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FName() AS STRING
    RETURN XSharp.FileSearch.FName()
    
    /// <summary>
    /// Find the next file that matches the file previously found by FFirst().
    /// </summary>
    /// <returns>TRUE when a next file is found. Otherwise FALSE.
    /// </returns>
FUNCTION FNext() AS LOGIC
    RETURN XSharp.FileSearch.FindNext()
    
    /// <summary>
    /// Return the size of the file found by FFCount(), FFirst(), or FNext().
    /// </summary>
    /// <returns>The size of the file found by FFCount(), FFirst(), or FNext().
    /// </returns>
FUNCTION FSize() AS DWORD
    RETURN XSharp.FileSearch.FSize()
    /// <summary>
    /// Return the time stamp of the file found by FFCount(), FFirst(), or FNext().
    /// </summary>
    /// <returns>The time stamp of the file found by FFCount(), FFirst(), or FNext().
    /// </returns>
FUNCTION FTime() AS STRING
    RETURN XSharp.FileSearch.FTime()
    
    
    
    
    /// <summary>
    /// Determine if any file matches a given file specification.
    /// </summary>
    /// <param name="cFile">The name oif the file</param>
    /// <returns>
    /// True if the file exists, otherwise false
    /// </returns>
FUNCTION File(cFile AS STRING) AS LOGIC
    LOCAL lFound AS LOGIC
    LOCAL lHasWildCards AS LOGIC
    LOCAL aPaths AS STRING[]
    LOCAL cTemp AS STRING
    LOCAL lFirst AS LOGIC
    IF String.IsNullOrEmpty(cFile)
        RETURN FALSE
    ENDIF
    lHasWildCards := cFile:IndexOfAny( <CHAR>{ '*', '?' } ) > 0
    XSharp.IO.File.LastFound := ""
    IF ! lHasWildCards
        lFound := System.IO.File.Exists(cFile)
        IF lFound
            XSharp.IO.File.LastFound := Path.GetFullPath( cFile )
            RETURN TRUE
        ENDIF
        aPaths := __GetSearchPaths()
        lFirst := TRUE
        FOREACH cPath AS STRING IN aPaths
            cTemp := System.IO.Path.Combine(cPath, cFile)
            IF lFirst
                // store the first path that we looked in even when the file is not found
                // to be compatible with VO
                XSharp.IO.File.LastFound := cTemp
                lFirst := FALSE
            ENDIF
            lFound := System.IO.File.Exists(cTemp)
            IF lFound
                XSharp.IO.File.LastFound := cTemp
                RETURN TRUE
            ENDIF
        NEXT
    ELSE
        // wildcard, so use Directory.GetFiles()
        LOCAL files     AS STRING[]
        
        IF Path.IsPathRooted(cFile)
            files := Directory.GetFiles( Path.GetDirectoryName( cFile ), Path.GetFileName( cFile ) )
            IF files:Length > 0
                XSharp.IO.File.LastFound := files[1]
                RETURN TRUE
            ELSE
                // store the first path that we looked in even when the file is not found
                // to be compatible with VO
                XSharp.IO.File.LastFound := cFile
                RETURN FALSE
            ENDIF
        ELSE
            // Look in current directory first and if that fails through the whole normal search list
            IF __FileHelper(Environment.CurrentDirectory, cFile, FALSE )
                RETURN TRUE
            ENDIF
            aPaths := __GetSearchPaths()
            lFirst := TRUE
            FOREACH cPath AS STRING IN aPaths
                IF __FileHelper(cPath, cFile, lFirst )
                    RETURN TRUE
                ENDIF
                lFirst := FALSE
            NEXT
        ENDIF
    ENDIF
    RETURN FALSE
    /// <summary>
    /// Return the name and path of the file that was used by FXOpen() or File().
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FPathName() AS STRING

    RETURN XSharp.IO.File.LastFound
    
    
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
        XSharp.IO.File.LastFound := files[0]
    ELSEIF lSavePath
        XSharp.IO.File.LastFound := cTemp
    ENDIF
    RETURN files:Length > 0
    
    
INTERNAL FUNCTION __GetSearchPaths() AS STRING[]
// Not found, now use the path settings from SetDefault and SetPath()
// if SetPath() is empty then we look through the Environment variable Path
LOCAL aDefault AS STRING[]
aDefault := __SetPathArray()
IF aDefault != NULL
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
aDefault := aPaths:ToArray()
__SetPathArray(aDefault)
RETURN aDefault
