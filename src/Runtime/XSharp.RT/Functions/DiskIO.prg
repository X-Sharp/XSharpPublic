//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adir/*" />
FUNCTION ADir(cFileSpec ,acFileNames ,anSizes ,adDates,acTimes,acAttributes) AS DWORD CLIPPER
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
	IF acFileNames:IsArray
		aNames  := acFileNames
		lHasArg := TRUE
	ENDIF
	IF anSizes:IsArray
		aSizes  := anSizes
		lHasArg := TRUE
	ENDIF
	IF adDates:IsArray
		aDates  := adDates
		lHasArg := TRUE
	ENDIF
	IF acTimes:IsArray
		aTimes  := acTimes
		lHasArg := TRUE
	ENDIF
	IF acAttributes:IsArray
		aAttribs  := acAttributes
		lHasArg := TRUE
	ENDIF
    IF aAttribs != NULL_ARRAY
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
	
	
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/directory/*" />
FUNCTION Directory(cFileSpec AS STRING, uAttributes := NIL AS USUAL) AS ARRAY
	LOCAL nAttr		AS DWORD
	LOCAL aReturn	AS ARRAY
	LOCAL cPath		AS STRING
	LOCAL cFileMask AS STRING
    LOCAL lWild     AS LOGIC
    IF uAttributes:IsNil
		nAttr := 0
	ELSEIF uAttributes:IsNumeric
		nAttr := (DWORD) uAttributes
	ELSEIF uAttributes:IsString
		nAttr := String2FAttr((STRING) uAttributes)
	ELSE
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(uAttributes), 2,<OBJECT>{uAttributes})
	ENDIF
	aReturn := {}
	IF (nAttr & FA_VOLUME ) != 0
		TRY
			AAdd(aReturn, {DriveInfo{cFileSpec}:VolumeLabel, 0, NULL_DATE, "00:00:00", "V"})
        CATCH AS Exception
            NOP
            
		END TRY
	ENDIF
	IF cFileSpec:EndsWith("\")
		cFileSpec += "*"
	END IF
    lWild := cFileSpec:Contains("*") .or. cFileSpec:Contains("?") 
    IF ! lWild
        LOCAL sPathSep AS STRING
        sPathSep := System.IO.Path.DirectorySeparatorChar:ToString()
	    IF System.IO.Directory.Exists(cFileSpec)
            IF _AND( nAttr,FA_DIRECTORY ) != (DWORD) FA_DIRECTORY
                cFileSpec += sPathSep + "*.*"
            ENDIF
	    ELSEIF cFileSpec:EndsWith(sPathSep)
		    cFileSpec += "*.*"
	    ELSEIF cFileSpec:Length == 2 .and. cFileSpec[1] == System.IO.Path.VolumeSeparatorChar .AND. Char.IsLetter( cFileSpec, 0 )   // only a drive letter specified
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
	IF _AND( nAttr,FA_DIRECTORY ) == (DWORD) FA_DIRECTORY
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
	IF _AND( nAttr, FA_DIRECTORY) == (DWORD) FA_DIRECTORY .AND. ( cFileSpec:Contains( "*" ) .OR. cFileSpec:Contains( "?" ) )
        VAR cName := Path.GetFileName( cFileSpec )
		IF cPath != Path.GetPathRoot( cFileSpec ) .AND. ( cName == "*.*" .OR. cName == "*" .OR. cName == "*." )
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
		aReturn := ASort( aReturn, 1, aReturn:Length, {|x AS ARRAY , y AS ARRAY| String.Compare( (STRING)x[1] , (STRING)y[1] , StringComparison.InvariantCultureIgnoreCase ) <= 0 } )	
	ENDIF
	RETURN aReturn

INTERNAL STATIC CLASS DirectoryHelper

INTERNAL STATIC METHOD AddFileInfo(aReturn AS ARRAY, oFile AS System.IO.FileInfo, nAttr AS DWORD) AS VOID
    
	LOCAL lAdd := TRUE AS LOGIC
    VAR cAttribute := DecodeAttributes(oFile:Attributes, nAttr, REF lAdd)
	IF lAdd
        VAR aFile := DirectoryHelper.FileSystemInfo2Array(oFile, cAttribute)
        AAdd(aReturn, aFile)
	ENDIF
	RETURN
	
INTERNAL STATIC METHOD AddDirectoryInfo( aReturn AS ARRAY, cDirectory AS STRING, nAttr AS DWORD, cName AS STRING ) AS VOID
	TRY
        IF cDirectory:IndexOfAny(<CHAR> {c'?',c'*'}) == -1
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
                    AAdd(aReturn, aFile)
			    ENDIF
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
		lOk := lOk .AND. _AND(nAttr,FC_HIDDEN) == (DWORD) FC_HIDDEN
		cAttribute += "H"
	ENDIF
	IF attributes:HasFlag(System.IO.FileAttributes.System) 
		lOk := lOk .AND. _AND(nAttr,FC_SYSTEM) == (DWORD) FC_SYSTEM
		cAttribute += "S"
	ENDIF
	IF attributes:HasFlag(System.IO.FileAttributes.Archive) 
		cAttribute += "A"
	ENDIF
    RETURN cAttribute
END CLASS
