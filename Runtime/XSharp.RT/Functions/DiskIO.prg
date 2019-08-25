//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
/// <summary>
/// Fill a series of Array with directory information.
/// </summary>
/// <param name="cFileSpec">The file specification for the directory search.
/// Besides a file name, this specification may include an optional drive, directory, and extension.
/// The file name and extension may include the standard wildcard characters (* and ?).
/// If you do not specify a drive and directory, this function uses the SetDefault() setting.</param>
/// <param name="aFName">The array to fill with the file names matching <paramref name="cFileSpec" />.  Each element will contain the file name and extension as a string, in all uppercase letters. </param>
/// <param name="aFSize">The array to fill with the sizes of the corresponding files in <paramref name="aFName" />. </param>
/// <param name="aFDate">The array to fill with the dates of the corresponding files in <paramref name="aFName" />.</param>
/// <param name="aFTime">The array to fill with the times of the corresponding files in <paramref name="aFName" />, in the form hh:mm:ss. </param>
/// <param name="aFAttr">The array to fill with attributes of the corresponding files in <paramref name="aFName" />.  If <paramref name="aFAttr" /> is specified, hidden, system, and
/// directory files are included as well as normal files.  If <paramref name="aFAttr" /> is not specified, only normal files are included.</param>
/// <returns>The number of files matching the directory skeleton described in <paramref name="cFileSpec" />./// </returns>
/// <remarks>ADir() is a compatibility function and therefore not recommended.
/// It is superseded by the Directory() function, which returns all file information in a multidimensional array.</remarks>
FUNCTION ADir(cFileSpec ,aFName ,aFSize ,aFDate,aFTime,aFAttr) AS DWORD CLIPPER
	IF ! cFileSpec:IsString
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(cFileSpec), 1,<OBJECT>{cFileSpec})
	ENDIF
	LOCAL aFiles	AS ARRAY
	LOCAL lHasArg   := FALSE AS LOGIC
	LOCAL aNames	:= NULL_ARRAY AS ARRAY
	LOCAL aSizes	:= NULL_ARRAY AS ARRAY
	LOCAL aDates	:= NULL_ARRAY AS ARRAY
	LOCAL aTimes    := NULL_ARRAY AS ARRAY
	LOCAL aAttribs  := NULL_ARRAY AS ARRAY
	IF aFName:IsArray
		aNames  := aFName
		lHasArg := TRUE
	ENDIF
	IF aFSize:IsArray
		aSizes  := aFSize
		lHasArg := TRUE
	ENDIF
	IF aFDate:IsArray
		aDates  := aFDate
		lHasArg := TRUE
	ENDIF
	IF aFTIME:IsArray
		aTimes  := aFTIME
		lHasArg := TRUE
	ENDIF
	IF aFAttr:IsArray
		aAttribs  := aFAttr
		lHasArg := TRUE
	ENDIF
    if aAttribs != NULL_ARRAY
        aFiles := Directory(cFileSpec,FA_NORMAL+FC_HIDDEN+FC_SYSTEM+FC_READONLY)
    ELSE
        aFiles := Directory(cFileSpec,FA_NORMAL)    
    ENDIF
	IF ALen(aFiles) > 0 .AND. lHasArg
		LOCAL x AS DWORD
		FOR x := 1 TO ALen(aFiles)
			VAR aFile := aFiles[x]
			IF aNames != NULL_ARRAY .AND. x <= ALen(aNames)
				aNames[x] := aFile[ F_NAME]
			ENDIF
			IF aSizes != NULL_ARRAY .AND. x <= ALen(aSizes)
				aSizes[x] := aFile[ F_SIZE]
			ENDIF
			IF aDates != NULL_ARRAY .AND. x <= ALen(aDates)
				aDates[x] := aFile[ F_DATE]
			ENDIF
			IF aTimes != NULL_ARRAY .AND. x <= ALen(aTimes)
				aTimes[x] := aFile[ F_TIME]
			ENDIF
			IF aAttribs != NULL_ARRAY .AND. x <= ALen(aAttribs)
				aAttribs[x] := aFile[ F_ATTR]
			ENDIF
		NEXT
	ENDIF
	RETURN ALen(aFiles)
	
	
	
	
	
/// <summary>
/// Create an Array of directory and file information.
/// </summary>
/// <param name="cFileSpec">The file specification for the search.  Besides a file name, this specification can include an optional drive, directory, and extension.  The file name and extension can include the standard wildcard characters (* and ?).  If you do not specify a drive and directory, the Windows defaults are used.</param>
/// <param name="xAttr">Specifies inclusion of files with special attributes in the returned information.  uAttributes can be a string or a numeric.</param>
/// <returns>An array of subarrays, with each subarray containing information about each file matching cFileSpec.  </returns>
FUNCTION Directory(cFileSpec AS STRING, xAttr := NIL AS USUAL) AS ARRAY
	LOCAL nAttr		AS DWORD
	LOCAL aReturn	AS ARRAY
	LOCAL cPath		AS STRING
	LOCAL cFileMask AS STRING
    LOCAL lWild     AS LOGIC
    IF xAttr:IsNil
		nAttr := 0
	ELSEIF xAttr:IsNumeric
		nAttr := (DWORD) xAttr
	ELSEIF xAttr:IsString
		nAttr := String2FAttr((STRING) xAttr)
	ELSE
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(xAttr), 2,<OBJECT>{xAttr})
	ENDIF
	aReturn := {}
	IF (nAttr & FA_VOLUME ) != 0
		TRY
			AAdd(aReturn, {DriveInfo{cFileSpec}:VolumeLabel, 0, NULL_DATE, "00:00:00", "V"})
		END TRY
	ENDIF
    lWild := cFileSpec:Contains("*") .or. cFileSpec:Contains("?") 
    IF ! lWild
        local sPathSep as STRING
        sPathSep := System.IO.Path.DirectorySeparatorChar:ToString()
	    IF System.IO.Directory.Exists(cFileSpec)
            IF _AND( nAttr,FA_DIRECTORY ) != FA_DIRECTORY
                cFileSpec += sPathSep + "*.*"
            ENDIF
	    ELSEIF cFileSpec:EndsWith(sPathSep)
		    cFileSpec += "*.*"
	    ELSEIF cFileSpec:Length == 2 && cFileSpec[1] == System.IO.Path.VolumeSeparatorChar .AND. Char.IsLetter( cFileSpec, 0 )   // only a drive letter specified
		    VAR  curdir := Environment.CurrentDirectory 
		    IF Char.ToUpper( cFileSpec[0] ) == Char.ToUpper( curdir[0] )  // if same drive, use current directory
                cFileSpec := curdir + sPathSep + "*.*"
		    ELSE
			    cFileSpec += sPathSep  + "*.*"
		    ENDIF   
	    ENDIF
    ENDIF
	TRY
		cPath := Path.GetDirectoryName(cFileSpec)
	CATCH
		cPath := ""
	END TRY   
	
	IF String.IsNullOrEmpty( cPath )
		cPath := System.IO.Directory.GetCurrentDirectory()
	ENDIF
	
	cFileMask := Path.GetFileName( cFileSpec )
	LOCAL files AS STRING[]
	IF _AND( nAttr,FA_DIRECTORY ) == FA_DIRECTORY
		DirectoryHelper.AddDirectoryInfo( aReturn, cFileSpec, nAttr , NULL)
    ENDIF
	// File Info
	    
	TRY
		files := System.IO.Directory.GetFiles( cPath, cFileMask )
		IF files != NULL
			FOREACH cFile AS STRING IN files
				VAR oFile := System.IO.FileInfo{cFile}
				DirectoryHelper.AddFileInfo(aReturn, oFile, nAttr)			
			NEXT
		ENDIF
	CATCH
		NOP
	END TRY

	
	
	// Directory info
	IF _AND( nAttr, FA_DIRECTORY) == FA_DIRECTORY .AND. ( cFileSpec:Contains( "*" ) || cFileSpec:Contains( "?" ) )
	    cPath := Path.GetDirectoryName( cFileSpec )
        VAR cName := Path.GetFileName( cFileSpec )
		IF cPath != Path.GetPathRoot( cFileSpec ) .AND. ( cName == "*.*" || cName == "*" )
			DirectoryHelper.AddDirectoryInfo( aReturn, cPath, nAttr, "." )
			DirectoryHelper.AddDirectoryInfo( aReturn, cPath, nAttr, ".." )
		ENDIF         
		
		TRY 
			files := System.IO.Directory.GetDirectories( cPath, cName )
			FOR VAR i := 1 UPTO files:Length
				DirectoryHelper.AddDirectoryInfo( aReturn, files[i], nAttr, NULL )
			NEXT
		CATCH
			NOP
		END TRY
	ENDIF
	
	IF ALen(aReturn) > 1
		aReturn := ASort( aReturn, 1, aReturn:Length, {|x,y| x[1] < y[1] } )	
	ENDIF
	RETURN aReturn

INTERNAL STATIC CLASS DirectoryHelper

INTERNAL STATIC METHOD AddFileInfo(aReturn AS ARRAY, oFile AS System.IO.FileInfo, nAttr AS DWORD) AS VOID
    
	LOCAL lAdd := TRUE AS LOGIC
    VAR cAttribute := DecodeAttributes(oFile:Attributes, nAttr, REF lAdd)
	IF lAdd
        VAR aFile := DirectoryHelper.FileSystemInfo2Array(oFile, cAttribute)
        aadd(aReturn, aFile)
	ENDIF
	RETURN
	
INTERNAL STATIC METHOD AddDirectoryInfo( aReturn AS ARRAY, cDirectory AS STRING, nAttr AS DWORD, cName AS STRING ) AS VOID
	TRY
		LOCAL oDir := System.IO.DirectoryInfo{ cDirectory } AS DirectoryInfo
		IF oDir:Exists
			LOCAL lAdd      := TRUE AS LOGIC
			VAR cAttribute  := DecodeAttributes(oDir:Attributes, nAttr, REF lAdd)
			IF lAdd
				cAttribute += "D"
                VAR aFile := DirectoryHelper.FileSystemInfo2Array(oDir, cAttribute)
                IF !String.IsNullOrEmpty(cName)
                    aFile[F_NAME] := cName
                ENDIF
                aadd(aReturn, aFile)
			ENDIF
		ENDIF
	CATCH 
		NOP
	END TRY   

	RETURN  

INTERNAL STATIC METHOD FileSystemInfo2Array(info AS FileSystemInfo, cAttribute AS STRING) AS ARRAY
    VAR aFile := ArrayNew(F_LEN)
    aFile[F_NAME] := info:Name
    IF info IS FileInfo
        VAR oFile := (System.IO.FileInfo) info
        aFile[F_SIZE] := oFile:Length
        IF oFile:Length < 0x7FFFFFFFL
            aFile[F_SIZE] := (Int32) oFile:Length
        ENDIF
    ELSE
        aFile[F_SIZE] := 0
    ENDIF
    aFile[F_DATE] := (DATE)  info:LastWriteTime
    aFile[F_TIME] := ConTime(info:LastWriteTime)
    aFile[F_ATTR] := cAttribute
    aFile[F_EA_SIZE] := info:Attributes
    aFile[F_CREATION_DATE] := (DATE)  info:CreationTime
    aFile[F_CREATION_TIME] := ConTime(info:CreationTime)
    aFile[F_ACCESS_DATE] := (DATE)  info:LastAccessTime
    aFile[F_ACCESS_TIME] := ConTime(info:LastAccessTime)
RETURN aFile

INTERNAL STATIC METHOD DecodeAttributes(attributes AS System.IO.FileAttributes, nAttr AS DWORD, lOk REF LOGIC) AS STRING
    VAR cAttribute := ""
    lOk := TRUE
	IF attributes:HasFlag(System.IO.FileAttributes.ReadOnly) 
		cAttribute += "R"
	ENDIF
	IF attributes:HasFlag(System.IO.FileAttributes.Hidden) 
		lOk := lOk .AND. _AND(nAttr,FC_HIDDEN) == FC_HIDDEN
		cAttribute += "H"
	ENDIF
	IF attributes:HasFlag(System.IO.FileAttributes.System) 
		lOk := lOk .AND. _AND(nAttr,FC_SYSTEM) == FC_SYSTEM
		cAttribute += "S"
	ENDIF
	IF attributes:HasFlag(System.IO.FileAttributes.Archive) 
		cAttribute += "A"
	ENDIF
    RETURN cAttribute
END CLASS
