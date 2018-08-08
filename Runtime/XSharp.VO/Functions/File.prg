//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING XSharp
USING System.Runtime.InteropServices



/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="pData"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION FRead(pHandle AS IntPtr,pData AS IntPtr,dwCount AS DWORD) AS DWORD
	LOCAL bData AS BYTE[]
	LOCAL dwResult AS DWORD
	bData := BYTE[] {(INT) dwCount}
	dwResult := FRead3(pHandle, bData, dwCount)
	Marshal.Copy(bData, 0, pData, (INT) dwResult)
	RETURN dwResult
	



/// <summary>
/// Read a line from an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
FUNCTION FReadLine(pFile ,nBuffLen) AS STRING CLIPPER
	IF nBuffLen == NIL
		RETURN XSharp.Core.Functions.FreadLine(pFile, 0)
	ELSE
		RETURN XSharp.Core.Functions.FreadLine(pFile, nBuffLen)
	ENDIF


/// <summary>
/// Set the file pointer to a new position.
/// </summary>
/// <param name="nFile"></param>
/// <param name="nOffset"></param>
/// <param name="nOrigin"></param>
/// <returns>
/// </returns>
FUNCTION FSeek(hFile ,nOffset ,nOrigin ) AS LONG CLIPPER
	IF nOrigin == NIL
		RETURN XSharp.Core.Functions.FSeek3(hFile, nOffSet, FS_SET)
	ELSE
		RETURN XSharp.Core.Functions.FSeek3(hFile, nOffSet, nOrigin)
	ENDIF
	


/// <summary>
/// Write a string to an open file.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION FWrite(pHandle ,c ,nCount ) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.Fwrite(pHandle, c)
	ELSE
		RETURN XSharp.Core.Functions.Fwrite3(pHandle, c, nCount)
	ENDIF

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION FWriteLine(pFile ,c ,nCount) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.FwriteLine(pFile, c)
	ELSE
		RETURN XSharp.Core.Functions.FwriteLine3(pFile, c, nCount)
	ENDIF

/// <summary>
/// Write a string to an open file, with SetAnsi() dependency.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION FWriteText(pHandle ,c ,nCount ) AS DWORD CLIPPER
	IF nCount == NIL
		RETURN XSharp.Core.Functions.FWriteText3(pHandle, c, Slen(c))
	ELSE
		RETURN XSharp.Core.Functions.FWriteText3(pHandle, c, nCount)
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


/// <summary>
/// Break a path name into its components.
/// </summary>
/// <param name="pszPath"></param>
/// <param name="pszDrive"></param>
/// <param name="pszDir"></param>
/// <param name="pszName"></param>
/// <param name="pszExt"></param>
/// <returns>
/// </returns>
FUNCTION SplitPath(pszPath AS PSZ,pszDrive AS PSZ,pszDir AS PSZ,pszName AS PSZ,pszExt AS PSZ) AS VOID
   LOCAL cDrive AS STRING
   LOCAL cDir   AS STRING
   LOCAL cName  AS STRING
   LOCAL cExt   AS STRING
   LOCAL cPath  AS STRING
   cPath := Psz2String(pszPath)
   _SplitPath(cPath, OUT cDrive, OUT cDir, OUT cName, OUT cExt)
	IF pszDrive != NULL_PSZ
		MemCopyString(pszDrive, cDrive, (DWORD) Slen(cDrive)+1)
	ENDIF
	IF pszDir != NULL_PSZ
		MemCopyString(pszDir, cDir, (DWORD) Slen(cDir)+1)
	ENDIF
	IF pszName != NULL_PSZ
		MemCopyString(pszName, cName, (DWORD) Slen(cName)+1)
	ENDIF
	IF pszExt != NULL_PSZ
		MemCopyString(pszExt, cExt, (DWORD) Slen(cExt)+1)
	ENDIF


//	return
