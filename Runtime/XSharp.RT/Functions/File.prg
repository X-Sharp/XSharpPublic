//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp
USING System.Runtime.InteropServices

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread3/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead(ptrHandle AS IntPtr,ptrBufferVar AS IntPtr,dwBytes AS DWORD) AS DWORD
    // use Buffer associated with file handle
    VAR bData    := FGetBuffer(ptrHandle, (INT) dwBytes)
	VAR dwResult := FRead3(ptrHandle, bData, dwBytes)
	Marshal.Copy(bData, 0, ptrBufferVar, (INT) dwResult)
	RETURN dwResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread3/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead3(ptrHandle AS IntPtr,ptrBufferVar AS IntPtr,dwBytes AS DWORD) AS DWORD
	RETURN FRead(ptrHandle, ptrBufferVar, dwBytes)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread4/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead4(ptrHandle AS IntPtr, ptrBufferVar AS IntPtr,dwBytes AS DWORD, lAnsi AS LOGIC) AS DWORD
	// use Buffer associated with file handle
    VAR bData := FGetBuffer(ptrHandle, (INT) dwBytes)
	VAR dwResult := FRead4(ptrHandle, bData, dwBytes, lAnsi)
	Marshal.Copy(bData, 0, ptrBufferVar, (INT) dwResult)
	RETURN dwResult



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadline/*" />
FUNCTION FReadLine(ptrHandle ,nMax) AS STRING CLIPPER
	IF nMax == NIL
		RETURN XSharp.Core.Functions.FreadLine((IntPtr) ptrHandle, 0U)
	ELSE
		RETURN XSharp.Core.Functions.FreadLine((IntPtr) ptrHandle, (DWORD) nMax)
	ENDIF

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadtext/*" />
/// <param name="pData">A block of memory to store the data read from the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FReadText3(ptrHandle AS IntPtr,pData AS IntPtr,dwBytes AS DWORD) AS DWORD
    VAR bData := FGetBuffer(ptrHandle, (INT) dwBytes)
	VAR dwResult := XSharp.Core.Functions.FReadText3(ptrHandle, bData, dwBytes)	
	Marshal.Copy(bData, 0, pData, (INT) dwResult)
	RETURN dwResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadtext/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead(ptrHandle AS IntPtr,cBufferVar REF USUAL,dwBytes AS DWORD) AS DWORD
    LOCAL strTemp := "" AS STRING
	VAR dwResult := XSharp.Core.Functions.FReadText(ptrHandle, strTemp, dwBytes)	
    cBufferVar := strTemp
    RETURN dwResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fseek/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FSeek(ptrHandle ,nOffset ,kOrigin ) AS LONG CLIPPER
	IF kOrigin == NIL
		RETURN XSharp.Core.Functions.FSeek3((IntPtr) ptrHandle, (LONG) nOffSet, (DWORD) FS_SET)
	ELSE
		RETURN XSharp.Core.Functions.FSeek3((IntPtr) ptrHandle, (LONG) nOffSet, (DWORD) kOrigin)
	ENDIF
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwrite3/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FWrite3(ptrHandle AS IntPtr,ptrBuffer AS IntPtr,dwBytes AS DWORD) AS DWORD
    // use Buffer associated with file handle
    VAR bData := FGetBuffer(ptrHandle, (INT) dwBytes)
	Marshal.Copy(ptrBuffer, bData, 0, (INT) dwBytes)
	VAR dwResult := FWrite3(ptrHandle, bData, dwBytes)
	RETURN dwResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwrite/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FWrite(ptrHandle ,cBuffer ,nBytes ) AS DWORD CLIPPER
	IF nBytes == NIL
		RETURN XSharp.Core.Functions.Fwrite((IntPtr) ptrHandle, (STRING) cBuffer, SLen(cBuffer))
	ELSE
		RETURN XSharp.Core.Functions.Fwrite((IntPtr) ptrHandle, (STRING) cBuffer, (DWORD) nBytes)
	ENDIF

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwriteline/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FWriteLine(ptrHandle ,cBuffer ,nBytes) AS DWORD CLIPPER
	IF nBytes == NIL
		RETURN XSharp.Core.Functions.FwriteLine((IntPtr) ptrHandle, (STRING) cBuffer)
	ELSE
		RETURN XSharp.Core.Functions.FwriteLine3((IntPtr) ptrHandle, (STRING) cBuffer, (DWORD) nBytes)
	ENDIF

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwritetext/*" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FWriteText(ptrHandle ,cBuffer ,nBytes)  AS DWORD CLIPPER
	IF nBytes == NIL
		RETURN XSharp.Core.Functions.FWrite((IntPtr) ptrHandle, (STRING) cBuffer, SLen(cBuffer), RuntimeState.Ansi)
	ELSE
		RETURN XSharp.Core.Functions.FWrite((IntPtr) ptrHandle, (STRING) cBuffer, (DWORD) nBytes, RuntimeState.Ansi)
	ENDIF 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/*" />
FUNCTION SplitPath(pszPathName AS STRING,pszDrive REF STRING,pszDir REF STRING,pszFile REF STRING,pszExt REF STRING) AS VOID
    _SplitPath(pszPathName, OUT pszDrive, OUT pszDir, OUT pszFile, OUT pszExt)
    RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/*" />
[Obsolete("'SplitPath()' with PSZ arguments is no longer supported. Please use SplitPath() or _SplitPath() (both with STRING arguments) in stead",FALSE)];
FUNCTION SplitPath(pszPathName AS PSZ,pszDrive AS PSZ,pszDir AS PSZ,pszFile AS PSZ,pszExt AS PSZ) AS VOID
   LOCAL cDrive AS STRING
   LOCAL cDir   AS STRING
   LOCAL cName  AS STRING
   LOCAL cExt   AS STRING
   LOCAL cPath  AS STRING
   cPath := Psz2String(pszPathName)
   _SplitPath(cPath, OUT cDrive, OUT cDir, OUT cName, OUT cExt)
	IF pszDrive != NULL_PSZ
		MemCopyString(pszDrive, cDrive, (DWORD) SLen(cDrive)+1)
	ENDIF
	IF pszDir != NULL_PSZ
		MemCopyString(pszDir, cDir, (DWORD) SLen(cDir)+1)
	ENDIF
	IF pszFile != NULL_PSZ
		MemCopyString(pszFile, cName, (DWORD) SLen(cName)+1)
	ENDIF
	IF pszExt != NULL_PSZ
		MemCopyString(pszExt, cExt, (DWORD) SLen(cExt)+1)
	ENDIF
    RETURN 


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfdatetime/*" />
FUNCTION SetFDateTime(cFile AS STRING, dDate AS DATE, cTime AS STRING) AS LOGIC
	LOCAL lOK := TRUE AS LOGIC
	LOCAL ts AS TimeSpan
	LOCAL dt AS DateTime

	TRY
	
		IF dDate == NULL_DATE
			dDate := Today()
		END IF
		IF String.IsNullOrWhiteSpace( cTime )
			ts := DateTime.Now:TimeOfDay
		ELSE
			ts := TimeSpan{ Val( SubStr3(cTime, 1, 2) ) , Val( SubStr3(cTime, 4, 2) ) , Val( SubStr3(cTime, 7, 2) ) }
		END IF
		
		dt := DateTime{ dDate:Year, dDate:Month, dDate:Day, ts:Hours, ts:Minutes, ts:Seconds }
		
		System.IO.File.SetLastWriteTime(cFile, dt)
	
	CATCH
		
		lOK := FALSE
		
	END TRY

RETURN lOK

