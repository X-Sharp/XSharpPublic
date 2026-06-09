//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.Internal

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/date/*" />
[FoxProFunction("DATE", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Date() AS DATE
    RETURN XSharp.RT.Functions.Date()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/datetime/*" />
[FoxProFunction("DATETIME", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION DateTime() AS DateTime
    RETURN XSharp.Core.Functions.DateTime()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/file/*" />
[FoxProFunction("FILE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION File(cFileSpec AS STRING) AS LOGIC
    RETURN XSharp.Core.Functions.File(cFileSpec)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/directory/*" />
[FoxProFunction("DIRECTORY", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Directory(cDirectoryName AS STRING, nFlags := 0 AS INT) AS LOGIC
    RETURN XSharp.RT.Functions.ALen(XSharp.RT.Functions.Directory(cDirectoryName)) > 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getenv/*" />
[FoxProFunction("GETENV", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION GetEnv(cEnvVariable AS STRING) AS STRING
    RETURN XSharp.Core.Functions.GetEnv(cEnvVariable)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/version/*" />
[FoxProFunction("VERSION", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION Version(nType := 0 AS INT) AS USUAL
    LOCAL cVer AS STRING
    cVer := __VERSION__

    SWITCH nType
        CASE 1
            RETURN "XSharp " + cVer + " for " + XSharp.Core.Functions.OS() + "[" + DateTime.Now:ToString("MMM dd yyyy") + "]"
        CASE 2
            // 0 = Runtime, 1 = Standard, 2 = Professional
            RETURN 2
        CASE 3
            // 00 = English
            RETURN "00"
        CASE 4
            // Format MM.mm.0000.NNNN
            RETURN cVer
        CASE 5
            // Major version. Ej: X# 3.0 -> 300
            LOCAL nMajor AS INT
            IF cVer:Length > 0 .AND. Char.IsDigit(cVer[0])
                nMajor := Int32.Parse(cVer[0]:ToString())
            ELSE
                nMajor := 3
            ENDIF
            RETURN nMajor * 100
    END SWITCH

    RETURN "XSharp " + cVer + " for " + XSharp.Core.Functions.OS()

/// <include file="VfpDocs.xml" path="Runtimefunctions/seconds/*" />
[FoxProFunction("SECONDS", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Seconds() AS REAL8
    RETURN XSharp.Core.Functions.Seconds()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fchsize/*" />
[FoxProFunction("FCHSIZE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FChSize(nFileHandle AS INT64, nNewSize AS INT64) AS INT64
    IF ! XSharp.Core.Functions.FChSize((IntPtr) nFileHandle, nNewSize)
        RETURN -1
    ENDIF
    RETURN XSharp.Core.Functions.FSize((IntPtr) nFileHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fclose/*" />
[FoxProFunction("FCLOSE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FClose(nFileHandle AS INT64) AS LOGIC
    RETURN XSharp.Core.Functions.FClose(nFileHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fcreate/*" />
[FoxProFunction("FCREATE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FCreate(cFileName AS STRING, nFileAttribute := 0 AS INT) AS INT64
    LOCAL pHandle AS IntPtr

    pHandle := XSharp.Core.Functions.FCreate(cFileName, (DWORD) nFileAttribute)

    RETURN pHandle:ToInt64()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/feof/*" />
[FoxProFunction("FEOF", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FEof(nFileHandle AS INT64) AS LOGIC
    RETURN XSharp.Core.Functions.FEof(nFileHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ferror/*" />
[FoxProFunction("FERROR", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FError() AS INT
    RETURN (INT) XSharp.RuntimeState.FileError

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fflush/*" />
[FoxProFunction("FFLUSH", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FFlush(nFileHandle AS INT64) AS LOGIC
    RETURN XSharp.Core.Functions.FFlush(nFileHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fgets/*" />
[FoxProFunction("FGETS", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FGetS(nFileHandle AS INT64, nBytes := 254 AS INT) AS STRING
    RETURN XSharp.Core.Functions.FGetS(nFileHandle, (DWORD) nBytes)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fopen/*" />
[FoxProFunction("FOPEN", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FOpen(cFileName AS STRING, nAttribute := 0 AS INT) AS INT64
    LOCAL nMode AS DWORD
    LOCAL pHandle AS IntPtr

    IF XSharp.Core.Functions.File(cFileName)
        cFileName := XSharp.Core.Functions.FPathName()
    ENDIF

    SWITCH nAttribute
        CASE 1; nMode := FO_WRITE | FO_EXCLUSIVE
        CASE 2; nMode := FO_READWRITE | FO_EXCLUSIVE
        CASE 10; nMode := FO_READ | FO_SHARED | FO_UNBUFFERED
        CASE 11; nMode := FO_WRITE | FO_EXCLUSIVE | FO_UNBUFFERED
        CASE 12; nMode := FO_READWRITE | FO_EXCLUSIVE | FO_UNBUFFERED
        CASE 0; nMode := FO_READ | FO_SHARED
        OTHERWISE
            nMode := FO_READ | FO_SHARED
    END SWITCH

    pHandle := XSharp.Core.Functions.FOpen2(cFileName, nMode)

    RETURN pHandle:ToInt64()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fputs/*" />
[FoxProFunction("FPUTS", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FPutS(nFileHandle AS INT64, cExpression AS STRING, nBytesWritten := 0 AS INT) AS INT
    IF nBytesWritten <= 0
        nBytesWritten := (INT) SLen(cExpression)
    ENDIF
    RETURN (INT) XSharp.Core.Functions.FPutS(nFileHandle, cExpression, (DWORD) nBytesWritten)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fread/*" />
[FoxProFunction("FREAD", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FRead(nFileHandle AS INT64, nBytes AS INT) AS STRING
    RETURN XSharp.Core.Functions.FReadStr(nFileHandle, (DWORD) nBytes)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fseek/*" />
[FoxProFunction("FSEEK", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FSeek(nFileHandle AS INT64, nBytesMoved AS INT, nRelativePosition := 0 AS INT) AS INT
    RETURN XSharp.Core.Functions.FSeek3(nFileHandle, nBytesMoved, (DWORD) nRelativePosition)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fsize/*" />
[FoxProFunction("FSIZE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FSize(cFieldName AS STRING, eWorkArea := NIL AS USUAL) AS INT
    // VFP Behavior: SET COMPATIBLE ON
    IF XSharp.RuntimeState.Compatible .AND. IsNil(eWorkArea)
        VAR cSearchFile := cFieldName

        IF String.IsNullOrEmpty(System.IO.Path.GetExtension(cSearchFile)) AND XSharp.Core.Functions.File(cSearchFile + ".dbf")
            cSearchFile += ".dbf"
        ENDIF

        IF XSharp.Core.Functions.File(cSearchFile)
            RETURN (INT) System.IO.FileInfo{XSharp.Core.Functions.FPathName()}:Length
        ENDIF
        RETURN 0
    ENDIF

    // Default behavior
    LOCAL nArea AS DWORD
    nArea := IIF(IsNil(eWorkArea), XSharp.RuntimeState.CurrentWorkarea, ;
        (DWORD) XSharp.RT.Functions.Select(eWorkArea))

    IF nArea == 0 || !XSharp.RT.Functions.Used(nArea)
        RETURN 0
    ENDIF

    VAR nPos := XSharp.RT.Functions.FieldPos(cFieldName, nArea)
    IF nPos > 0
        LOCAL uLen := NULL AS USUAL
        VAR nOldArea := XSharp.RuntimeState.CurrentWorkarea
        TRY
            XSharp.RuntimeState.CurrentWorkarea := nArea
            XSharp.RT.Functions.VoDbFieldInfo(3, nPos, REF uLen) // 3 = DBS_LEN
        FINALLY
            XSharp.RuntimeState.CurrentWorkarea := nOldArea
        END TRY
        RETURN IIF(IsNumeric(uLen), (INT) uLen, 0)
    ENDIF

    RETURN 0

FUNCTION FSize() AS DWORD
    RETURN XSharp.Core.Functions.FSize()

FUNCTION FSize(pFile AS IntPtr) AS INT64
    RETURN XSharp.Core.Functions.FSize(pFile)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fwrite/*" />
[FoxProFunction("FWRITE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FWrite(nFileHandle AS INT64, cExpression AS STRING, nCharactersWritten := 0 AS INT) AS INT
    IF nCharactersWritten <= 0
        nCharactersWritten := (INT) SLen(cExpression)
    ENDIF
    RETURN (INT) XSharp.Core.Functions.FWrite(nFileHandle, cExpression, (DWORD) nCharactersWritten)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adir/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("ADIR", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION ADir(ArrayName AS USUAL, cFileSkeleton := "" AS STRING, cAttribute := "" AS STRING, nFlag := 0 AS INT) AS INT
    LOCAL aDirInfo AS ARRAY
    LOCAL nFiles AS DWORD
    LOCAL i AS DWORD

    IF String.IsNullOrEmpty(cFileSkeleton)
        cFileSkeleton := "*.*"
    ENDIF

    aDirInfo := XSharp.RT.Functions.Directory(cFileSkeleton, cAttribute)
    nFiles := XSharp.RT.Functions.ALen(aDirInfo)

    IF nFiles > 0
        // Redimensiona o inicializa el USUAL mágicamente creado
        ArrayName := __FoxRedim(ArrayName, nFiles, 5)

        IF ArrayName IS __FoxArray VAR foxArray
            FOR i := 1 TO nFiles
                LOCAL aFile AS ARRAY
                aFile := (ARRAY) aDirInfo[i]
                IF nFlag == 0
                    foxArray[(INT)i, 1] := ((STRING)aFile[1]):ToUpper()
                ELSE
                    foxArray[(INT)i, 1] := (STRING)aFile[1]
                ENDIF
                foxArray[(INT)i, 2] := (INT)aFile[2]
                foxArray[(INT)i, 3] := (DATE)aFile[3]
                foxArray[(INT)i, 4] := (STRING)aFile[4]
                foxArray[(INT)i, 5] := (STRING)aFile[5]
            NEXT
        ELSE
            // Por si nos enviaron un ARRAY nativo tradicional de VO
            XSharp.RT.Functions.ASize(ArrayName, nFiles)
            FOR i := 1 TO nFiles
                LOCAL aFile AS ARRAY
                aFile := (ARRAY) aDirInfo[i]
                IF nFlag == 0
                    aFile[1] := ((STRING)aFile[1]):ToUpper()
                ENDIF
                ((ARRAY)ArrayName)[i] := aFile
            NEXT
        ENDIF
    ELSE
        IF ArrayName IS __FoxArray VAR foxArray
            foxArray:ReDim(0, 0)
        ELSEIF ArrayName IS ARRAY VAR arr
            XSharp.RT.Functions.ASize(arr, 0)
        ENDIF
    ENDIF

    RETURN (INT) nFiles

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/diskspace/*" />
[FoxProFunction("DISKSPACE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION DiskSpace(cDriveName := "" AS STRING, nSpaceType := 1 AS INT) AS REAL8
    LOCAL nResult AS INT64

    // 1 = Total amount of space on the hard disk drive.
    // 2 = Total amount of free space on the hard disk drive. (Default)
    // 3 = Total amount of free space available to the user associated with the calling thread.
    SWITCH nSpaceType
        CASE 1 // Total size
            nResult := XSharp.Core.Functions.DiskSpace(cDriveName)
        CASE 2 // Total free space (Default)
        CASE 3 // User free space (we map it to the total free space for now)
        OTHERWISE
            nResult := XSharp.Core.Functions.DiskFree(cDriveName)
    END SWITCH

    RETURN (REAL8) nResult


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/curdir/*" />
[FoxProFunction("CURDIR", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CurDir(cDrive := "" AS STRING) AS STRING
    RETURN XSharp.Core.Functions.CurDir(cDrive)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/os/*" />
[FoxProFunction("OS", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION OS(nType := 0 AS INT) AS STRING
    VAR oOS := System.Environment.OSVersion
    SWITCH nType
        CASE 0 // Default: name and version, same as CASE 1 in VFP
        CASE 1 // Name and version
            RETURN XSharp.Core.Functions.OS()
        CASE 2 // DBCS Support
            RETURN IIF(System.Text.Encoding.Default:IsSingleByte, "", "DBCS")
        CASE 3 // Major version
            RETURN oOS:Version:Major:ToString()
        CASE 4 // Minor version
            RETURN oOS:Version:Minor:ToString()
        CASE 5 // Build number
            RETURN oOS:Version:Build:ToString()
        CASE 6 // Platform
            RETURN ((INT)oOS:Platform):ToString()
        CASE 7 // Service Pack String
            RETURN oOS:ServicePack
        CASE 8
        CASE 9
        CASE 10 // Product Suite
            RETURN "0"
        CASE 11 // Product Type
            RETURN "1"
    END SWITCH
    RETURN ""

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/oemtoansi/*" />
[FoxProFunction("OEMTOANSI", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION OemToAnsi(cOemString AS STRING) AS STRING
    RETURN XSharp.Core.Functions.Oem2Ansi(cOemString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/set/*" />
[FoxProFunction("SET", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Set(cSETCommand, newValue) AS USUAL CLIPPER
    IF PCount() > 1
        RETURN XSharp.RT.Functions.Set(cSETCommand, newValue)
    ENDIF
    RETURN XSharp.RT.Functions.Set(cSETCommand)
