//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
[FoxProFunction("VERSION", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION Version(nType := 0 AS INT) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/seconds/*" />
[FoxProFunction("SECONDS", FoxFunctionCategory.DateAndTime, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Seconds() AS REAL8
    RETURN XSharp.Core.Functions.Seconds()

// -------------------------------------------------------------
// TODO(irwin): functions to check
// -------------------------------------------------------------
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fchsize/*" />
[FoxProFunction("FCHSIZE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION FChSize(nFileHandle AS INT64, nNewSize AS INT64) AS INT64
    IF ! XSharp.Core.Functions.FChSize(nFileHandle, nNewSize)
        RETURN -1
    ENDIF
    RETURN XSharp.Core.Functions.FSize(nFileHandle)

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
FUNCTION FGets(nFileHandle AS INT64, nBytes := 254 AS INT) AS STRING
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
FUNCTION FPuts(nFileHandle AS INT64, cExpression AS STRING, nBytesWritten := 0 AS INT) AS INT
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
[FoxProFunction("FSIZE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION FSize(cFieldName AS STRING, eWorkArea := NIL AS USUAL) AS INT
    // Todo: Create an overload for function with cFileName parameter
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fwrite/*" />
[FoxProFunction("FWRITE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION FWrite(nFileHandle AS INT64, cExpression AS STRING, nCharactersWritten := 0 AS INT) AS INT
    IF nCharactersWritten <= 0
        nCharactersWritten := (INT) SLen(cExpression)
    ENDIF
    RETURN (INT) XSharp.Core.Functions.FWrite(nFileHandle, cExpression, (DWORD) nCharactersWritten)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adir/*" />
[FoxProFunction("ADIR", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION ADir(ArrayName AS ARRAY, cFileSkeleton := "" AS STRING, cAttribute := "" AS STRING, nFlag := 0 AS INT) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/diskspace/*" />
[FoxProFunction("DISKSPACE", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION Diskspace(cDriveName := "" AS STRING, nSpaceType := 1 AS INT) AS REAL8
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/curdir/*" />
[FoxProFunction("CURDIR", FoxFunctionCategory.FileAndIO, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION Curdir(cDriveDesignator := "" AS STRING) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/os/*" />
[FoxProFunction("OS", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION Os(nType := 0 AS INT) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/oemtoansi/*" />
[FoxProFunction("OEMTOANSI", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Stub, FoxCriticality.Low)];
FUNCTION OemToAnsi(cExpression AS STRING) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/set/*" />
[FoxProFunction("SET", FoxFunctionCategory.EnvironmentAndSystem, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Set(nDefine AS USUAL, newValue AS USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Set(nDefine, newValue)
