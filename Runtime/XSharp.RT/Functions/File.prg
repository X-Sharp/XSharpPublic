//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp
USING System.Runtime.InteropServices

/// <param name="pData">A block of memory to store the data read from the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <inheritdoc cref="M:XSharp.Core.Functions.FRead3(System.IntPtr,System.Byte[],System.UInt32)" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead(pFile AS IntPtr,pData AS IntPtr,dwCount AS DWORD) AS DWORD
    // use Buffer associated with file handle
    VAR bData    := FGetBuffer(pFile, (INT) dwCount)
	VAR dwResult := FRead3(pFile, bData, dwCount)
	Marshal.Copy(bData, 0, pData, (INT) dwResult)
	RETURN dwResult

/// <inheritdoc cref="M:XSharp.RT.Functions.FRead(System.IntPtr,System.IntPtr,System.UInt32)" />
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead3(pFile AS IntPtr,pData AS IntPtr,dwCount AS DWORD) AS DWORD
	RETURN FRead(pFile, pData, dwCount)


/// <summary>
/// Read characters from a file into an allocated buffer with optional OEM to Ansi conversion.
/// </summary>
/// <inheritdoc cref="M:XSharp.RT.Functions.FRead(System.IntPtr,System.IntPtr,System.UInt32)" />
/// <param name="lAnsi">If FALSE an OEM to ANSI conversion is made. </param>
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FRead4(pFile AS IntPtr, pData AS IntPtr,dwCount AS DWORD, lAnsi AS LOGIC) AS DWORD
	// use Buffer associated with file handle
    VAR bData := FGetBuffer(pFile, (INT) dwCount)
	RETURN FRead4(pFile, bData, dwCount, lAnsi)


/// <inheritdoc cref="M:XSharp.Core.Functions.FReadLine(System.IntPtr,System.UInt32)" />"
FUNCTION FReadLine(pFile ,nBuffLen) AS STRING CLIPPER
	IF nBuffLen == NIL
		RETURN XSharp.Core.Functions.FreadLine((IntPtr) pFile, 0U)
	ELSE
		RETURN XSharp.Core.Functions.FreadLine((IntPtr) pFile, (DWORD) nBuffLen)
	ENDIF

/// <inheritdoc cref="M:XSharp.Core.Functions.FReadText3(System.IntPtr,System.Byte[],System.UInt32)" />"
/// <param name="pData">A block of memory to store the data read from the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FReadText3(pFile AS IntPtr,pData AS IntPtr,dwCount AS DWORD) AS DWORD
    VAR bData := FGetBuffer(pFile, (INT) dwCount)
	VAR dwResult := XSharp.Core.Functions.FReadText3(pFile, bData, dwCount)	
	Marshal.Copy(bData, 0, pData, (INT) dwResult)
	RETURN dwResult


/// <inheritdoc cref="M:XSharp.Core.Functions.FReadText(System.IntPtr,System.String@,System.UInt32)" />
FUNCTION FRead(pFile AS IntPtr,strValue REF USUAL,dwCount AS DWORD) AS DWORD
    LOCAL strTemp := "" AS STRING
	VAR dwResult := XSharp.Core.Functions.FReadText(pFile, strTemp, dwCount)	
    strValue := strTemp
    RETURN dwResult

/// <inheritdoc cref="M:XSharp.Core.Functions.FSeek3(System.IntPtr,System.Int32,System.UInt32)" />
FUNCTION FSeek(hFile ,nOffset ,nOrigin ) AS LONG CLIPPER
	IF nOrigin == NIL
		RETURN XSharp.Core.Functions.FSeek3((IntPtr) hFile, (LONG) nOffSet, (DWORD) FS_SET)
	ELSE
		RETURN XSharp.Core.Functions.FSeek3((IntPtr) hFile, (LONG) nOffSet, (DWORD) nOrigin)
	ENDIF
	
/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite(System.IntPtr,System.String,System.UInt32)" />
/// <param name="pData">A block of memory that holds the data to write to the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <include file="RTComments.xml" path="Comments/FileCompat/*" />
FUNCTION FWrite3(pFile AS IntPtr,pData AS IntPtr,dwCount AS DWORD) AS DWORD
    // use Buffer associated with file handle
    VAR bData := FGetBuffer(pFile, (INT) dwCount)
	Marshal.Copy(pData, bData, 0, (INT) dwCount)
	VAR dwResult := FWrite3(pFile, bData, dwCount)
	RETURN dwResult

/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite(System.IntPtr,System.String,System.UInt32)" />
FUNCTION FWrite(pFile ,c ,nCount ) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.Fwrite((IntPtr) pFile, (STRING) c, SLen(c))
	ELSE
		RETURN XSharp.Core.Functions.Fwrite((IntPtr) pFile, (STRING) c, (DWORD) nCount)
	ENDIF

/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" />
FUNCTION FWriteLine(pFile ,c ,nCount) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.FwriteLine((IntPtr) pFile, (STRING) c)
	ELSE
		RETURN XSharp.Core.Functions.FwriteLine3((IntPtr) pFile, (STRING) c, (DWORD) nCount)
	ENDIF

/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite(System.IntPtr,System.String,System.UInt32)" />
FUNCTION FWriteText(pFile ,c ,nCount ) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.FWrite((IntPtr) pFile, (STRING) c, SLen(c), RuntimeState.Ansi)
	ELSE
		RETURN XSharp.Core.Functions.FWrite((IntPtr) pFile, (STRING) c, (DWORD) nCount, RuntimeState.Ansi)
	ENDIF 

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
FUNCTION SplitPath(cPath AS STRING,cDrive REF STRING,cDir REF STRING,cName REF STRING,cExt REF STRING) AS VOID
    _SplitPath(cPath, OUT cDrive, OUT cDir, OUT cName, OUT cExt)
    RETURN


[Obsolete("'SplitPath()' with PSZ arguments is no longer supported. Please use SplitPath() or _SplitPath() (both with STRING arguments) in stead",FALSE)];
FUNCTION SplitPath(pszPath AS PSZ,pszDrive AS PSZ,pszDir AS PSZ,pszName AS PSZ,pszExt AS PSZ) AS VOID
   LOCAL cDrive AS STRING
   LOCAL cDir   AS STRING
   LOCAL cName  AS STRING
   LOCAL cExt   AS STRING
   LOCAL cPath  AS STRING
   cPath := Psz2String(pszPath)
   _SplitPath(cPath, OUT cDrive, OUT cDir, OUT cName, OUT cExt)
	IF pszDrive != NULL_PSZ
		MemCopyString(pszDrive, cDrive, (DWORD) SLen(cDrive)+1)
	ENDIF
	IF pszDir != NULL_PSZ
		MemCopyString(pszDir, cDir, (DWORD) SLen(cDir)+1)
	ENDIF
	IF pszName != NULL_PSZ
		MemCopyString(pszName, cName, (DWORD) SLen(cName)+1)
	ENDIF
	IF pszExt != NULL_PSZ
		MemCopyString(pszExt, cExt, (DWORD) SLen(cExt)+1)
	ENDIF
    RETURN 


/// <summary>
/// Reset the date and time stamps of a File.
/// </summary>
/// <param name="cFileName">The name of the file to modify.</param>
/// <param name="dDate">The new date to use for the date stamp.  If not specified, the default is Today().</param>
/// <param name="cTime">The new time string to use for the time stamp.  If not specified, the default is Time().</param>
/// <returns>TRUE if successful; otherwise FALSE.</returns>
FUNCTION SetFDateTime(cFileName AS STRING, dDate AS DATE, cTime AS STRING) AS LOGIC
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
		
		System.IO.File.SetLastWriteTime(cFileName, dt)
	
	CATCH
		
		lOK := FALSE
		
	END TRY

RETURN lOK

