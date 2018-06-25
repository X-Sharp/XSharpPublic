//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


using XSharp
using System.Runtime.InteropServices



/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="pData"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FRead(pHandle as IntPtr,pData as IntPtr,dwCount as dword) as dword
	local bData as byte[]
	local dwResult as DWORD
	bData := byte[] {(int) dwCount}
	dwResult := FRead3(pHandle, bData, dwCount)
	Marshal.Copy(bData, 0, pData, (int) dwResult)
	return dwResult
	



/// <summary>
/// Read a line from an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FReadLine(pFile ,nBuffLen) as string CLIPPER
	if nBuffLen == NIL
		return XSharp.Core.Functions.FreadLine(pFile, 0)
	else
		return XSharp.Core.Functions.FreadLine(pFile, nBuffLen)
	endif


/// <summary>
/// Set the file pointer to a new position.
/// </summary>
/// <param name="nFile"></param>
/// <param name="nOffset"></param>
/// <param name="nOrigin"></param>
/// <returns>
/// </returns>
function FSeek(hFile ,nOffset ,nOrigin ) as long CLIPPER
	if nOrigin == NIL
		return XSharp.Core.Functions.FSeek3(hFile, nOffSet, FS_SET)
	else
		return XSharp.Core.Functions.FSeek3(hFile, nOffSet, nOrigin)
	endif
	


/// <summary>
/// Write a string to an open file.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWrite(pHandle ,c ,nCount ) as dword CLIPPER
	if nCount == NIL
		return XSharp.Core.Functions.Fwrite(pHandle, c)
	else
		return XSharp.Core.Functions.Fwrite3(pHandle, c, nCount)
	endif

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file.
/// </summary>
/// <param name="pFile"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine(pFile ,c ,nCount) as dword CLIPPER
	if nCount == NIL
		return XSharp.Core.Functions.FwriteLine(pFile, c)
	else
		return XSharp.Core.Functions.FwriteLine3(pFile, c, nCount)
	endif

/// <summary>
/// Write a string to an open file, with SetAnsi() dependency.
/// </summary>
/// <param name="pHandle"></param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteText(pHandle ,c ,nCount ) as dword CLIPPER
	if nCount == NIL
		return XSharp.Core.Functions.FWriteText3(pHandle, c, Slen(c))
	else
		return XSharp.Core.Functions.FWriteText3(pHandle, c, nCount)
	endif 

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
   _SplitPath(cPath, out cDrive, out cDir, out cName, out cExt)
	return


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
function SplitPath(pszPath as Psz,pszDrive as Psz,pszDir as Psz,pszName as Psz,pszExt as Psz) as void
   LOCAL cDrive AS STRING
   LOCAL cDir   AS STRING
   LOCAL cName  AS STRING
   LOCAL cExt   AS STRING
   local cPath  as string
   cPath := Psz2String(pszPath)
   _SplitPath(cPath, out cDrive, out cDir, out cName, out cExt)
	if pszDrive != null_psz
		MemCopyString(pszDrive, cDrive, (DWORD) Slen(cDrive)+1)
	endif
	if pszDir != null_psz
		MemCopyString(pszDir, cDir, (DWORD) Slen(cDir)+1)
	endif
	if pszName != null_psz
		MemCopyString(pszName, cName, (DWORD) Slen(cName)+1)
	endif
	if pszExt != null_psz
		MemCopyString(pszExt, cExt, (DWORD) Slen(cExt)+1)
	endif


//	return
