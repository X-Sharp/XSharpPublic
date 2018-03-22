//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections
using System.Collections.Generic
using System.IO
using System.Linq
using System.Runtime.InteropServices
using System.Security
using Microsoft.Win32.SafeHandles
using System.Runtime
using System.Runtime.ConstrainedExecution
using System.Text

#region Defines
	define F_ERROR :=  -1 // Error value (all functions)
	
	// FERROR() returns, which are not reflected as DOSERROR()
	define FERROR_FULL    := 256   // disk full
	define FERROR_EOF     := 257   // eof was already reached, when a read was tried
	define FERROR_PARAM   := 258   // invalid parameter already detected before giving to DOS
	
	// FSEEK(), _llseek() modes
	define FS_SET         := 0  // Seek from beginning of file
	define FS_RELATIVE    := 1  // Seek from current file position
	define FS_END         := 2  // Seek from end of file
	
	// FOPEN() access modes
	define FO_READ        := 0  // Open for reading (default)
	define FO_WRITE       := 1  // Open for writing
	define FO_READWRITE   := 2  // Open for reading or writing
	
	// FOPEN() sharing modes (combine with open mode using +)
	define FO_COMPAT     := 0x00000000  // Compatibility mode (default)
	define FO_EXCLUSIVE  := 0x00000010  // Exclusive
	define FO_DENYWRITE  := 0x00000020  // Prevent other processes from writing
	define FO_DENYREAD   := 0x00000030  // Prevent other processes from reading
	define FO_DENYNONE   := 0x00000040  // (same as FO_SHARED)
	define FO_SHARED     := 0x00000040  // Allow other processes to read or write
	
	
	define OF_SHARE_COMPAT := 0x00000000
	define OF_SHARE_EXCLUSIVE := 0x00000010
	define OF_SHARE_DENY_WRITE := 0x00000020
	define OF_SHARE_DENY_READ := 0x00000030
	define OF_SHARE_DENY_NONE := 0x00000040
	define OF_PARSE := 0x00000100
	
	define CREATE_NEW := 1
	define CREATE_ALWAYS := 2
	define OPEN_EXISTING := 3
	define OPEN_ALWAYS := 4
	define FO_DELETE := 0x00000200
	define FO_VERIFY := 0x00000400
	define FO_CANCEL := 0x00000800
	define FO_CREATE := 0x00001000
	define FO_PROMPT := 0x00002000
	define FO_EXIST := 0x00004000
	define FO_REOPEN := 0x00008000
	
	
	// FXOPEN() mode
	define FXO_WILD      := 0x00010000  // Allow wildcards in file name
	// FCREATE() file attribute modes (always opens with OF_READWRITE)
	define FC_NORMAL     := 0x00000000  // normal read/write file (default for create)
	define FC_READONLY   := 0x00000001  // read-only file
	define FC_HIDDEN     := 0x00000002  // hidden file
	define FC_SYSTEM     := 0x00000004  // system file
	define FC_ARCHIVED   := 0x00000020
	
	//
	// additional file attribute for DIRECTORY(), FFIRST() and FFCOUNT()
	//
	define FA_VOLUME     := 0x00000008
	define FA_DIRECTORY  := 0x00000010
	define FA_NORMAL	 := 0x00000080
	define FA_TEMPORARY  := 0x00000100
	define FA_COMPRESSED := 0x00000800
	define FA_OFFLINE    := 0x00001000
	
	static define FF_WRITE_THROUGH := 0x80000000
	static define FF_OVERLAPPED := 0x40000000
	static define FF_NO_BUFFERING := 0x20000000
	static define FF_RANDOM_ACCESS := 0x10000000
	static define FF_SEQUENTIAL_SCAN := 0x08000000
	static define FF_DELETE_ON_CLOSE := 0x04000000
	static define FF_BACKUP_SEMANTICS := 0x02000000
	static define FF_POSIX_SEMANTICS := 0x01000000
	
	static define GENERIC_READ := 0x80000000U
	static define GENERIC_WRITE := 0x40000000L
	static define GENERIC_EXECUTE := 0x20000000L
	static define GENERIC_ALL := 0x10000000L
	
	static define FILE_SHARE_READ := 0x00000001
	static define FILE_SHARE_WRITE := 0x00000002
	
	
	//
	
#endregion


begin namespace XSharp.IO
	
	
	internal class VOFileMode
		property lWild			as logic auto
		property FileMode		as FileMode auto 
		property FileAccess		as FileAccess auto 
		property Attributes	    as DWORD auto
		property FileShare		as FileShare auto
		
		constructor(dwMode as dword, dwAttribs as dword)
			// Wildcard
			if (dword)_and(dwMode, FXO_WILD) == FXO_WILD
				lWild := true
				dwMode := dwMode - FXO_WILD
			else
				lWild := false
			endif
			Attributes := dwAttribs
			
			// Access
			fileAccess	:= FileAccess.Read
			if (dword)_and(dwMode,FO_READWRITE) == FO_READWRITE
				fileAccess	:= FileAccess.ReadWrite
			elseif (dword)_and(dwMode,FO_WRITE) == FO_WRITE
				fileAccess	:= FileAccess.Write
			endif
			// Create
			fileMode := FileMode.Open
			if (dword)_and(dwMode,FO_CREATE) == FO_CREATE
				fileMode	:= FileMode.Create
				fileAccess	:= FileAccess.ReadWrite
			endif
			
			FileShare  := FileShare.ReadWrite
			local dwTempMode as dword
			dwTempMode := (dword)_or(OF_SHARE_DENY_WRITE, OF_SHARE_DENY_READ, OF_SHARE_DENY_NONE)
			dwTempMode := (dword)_and(dwMode,dwTempMode)
			
			do case
				case dwTempMode == FO_DENYNONE
					FileShare  := FileShare.ReadWrite
				
				case dwTempMode == FO_DENYWRITE
					FileShare  := FileShare.Read
				
				case dwTempMode == FO_DENYREAD
					FileShare  := FileShare.Write
				
				case dwTempMode == FO_EXCLUSIVE
					FileShare  := FileShare.None
				
				case dwTempMode == FO_COMPAT
					FileShare  := FileShare.ReadWrite
				
			endcase
			return
		
		
	end class
	
	static class File
		public static errorCode	:= 0 as dword
		public static LastFound as string
		private static streams as Dictionary<IntPtr, Tuple<FileStream, dword> >
		private static random as Random
		
		static constructor
			streams := Dictionary<IntPtr, Tuple<FileStream, DWORD> >{}
			random := Random{}
		
		static internal method findStream(pStream as IntPtr) as FileStream
			if streams:ContainsKey(pStream)
				return streams[pStream]:Item1
			endif
			return null_object 
		
		static internal method hasStream(pStream as Intptr) as logic
			return streams:ContainsKey(pStream)
		
		static internal method addStream(pStream as Intptr, oStream as FileStream, attributes as DWORD) as logic
			if ! streams:ContainsKey(pStream)
				streams:Add(pStream, Tuple<FileStream, DWORD> {oStream, attributes})
				return true
			endif
			return false
		
		static internal method removeStream(pStream as Intptr) as logic
			if ! streams:ContainsKey(pStream)
				streams:Remove(pStream)
				return true
			endif
			return false
		

		
		static internal method _createManagedFileStream(cFIle as string, oMode as VOFileMode) as FileStream
			local oStream := null as FileSTream
			try
				oStream := FileStream{cFile, oMode:FileMode, oMode:FileAccess, oMode:FileShare, 4096}
			catch e as Exception
				System.Diagnostics.Trace.WriteLine(e:Message)
				FError((dword)Marshal.GetLastWin32Error())
			end try
			return oStream

		static internal method _createFile(cFIle as string, oMode as VOFileMode) as IntPtr
			local hFile := F_ERROR as IntPtr
			local oStream as FileStream
			if System.IO.File.Exists(cFile) .and. oMode:FileMode == FileMode.Create
				var fi := FileInfo{cFile}
				fi:Attributes := FileAttributes.Normal
			endif
		
				oStream :=_createManagedFileStream(cFile, oMode)
				if oStream != NULL
					hFile := (IntPtr) random:@@Next(1, Int32.MaxValue)
					do while streams:ContainsKey(hFile)
						hFile := (IntPtr) random:@@Next(1, Int32.MaxValue)
					enddo
				else
					hFile := F_ERROR
				endif
			//endif
			if hFile != F_ERROR
				addStream(hFile, oStream, oMode:Attributes)
			endif
			return hFile


		
		
		static internal method _fClose(pStream as IntPtr) as logic
			if hasStream(pStream)
				local oStream      := streams[pStream]:Item1 as FileStream
				LOCAL dwAttributes := streams[pStream]:Item2 as DWORD
				removeStream(pStream)
				oStream:Flush()
				oStream:Close()
				if dwAttributes != 0
					var fi := FileInfo{oStream:Name}
					fi:Attributes := (FileAttributes) dwAttributes
				endif
				oStream:Dispose()
				return true
			endif
			return false
		
		
		
		
		public static method FError( code as dword) as dword
			local lastError := errorCode as dword
			errorCode := code 
			return lastError
		
	end class
	
	
end namespace	


/// <summary>
/// Set the error code for a file operation.
/// </summary>
/// <param name="nSet"></param>
/// <returns>
/// </returns>
function FError(nErrorCode as dword) as dword
	local nOldError as dword
	nOldError := XSharp.IO.File.errorCode
	XSharp.IO.File.errorCode := nErrorCode
	return nOldError


/// <summary>
/// Get the error code for a file operation.
/// </summary>
/// <returns>
/// </returns>
function FError() as dword
	local nOldError as dword
	nOldError := XSharp.IO.File.errorCode
	return nOldError



/// <summary>
/// Change the size of a file opened with a low-level file function.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="nOffset"></param>
/// <returns>
/// </returns>
function FChSize(pHandle as IntPtr,nValue as int64) as int64
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	if oStream != null_object
		try
			oStream:SetLength(nValue)
		catch 
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif	
	return -1

function FChSize(pHandle as IntPtr,nValue as dword) as dword
	return (dword) fChSize(pHandle, (int64) nValue)

/// <summary>
/// Close an open file and write the buffers to disk.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FClose(pFile as IntPtr) as logic
	return XSharp.IO.File._fClose(pFile)

/// <summary>
/// Flush file buffers.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FCommit(pHandle as IntPtr) as void
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	if oStream != null_object
		try
			oStream:Flush(true)
		catch 
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif	
	return 

/// <summary>
/// Determine if the file pointer is positioned at the end-of-file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FEof(pHandle as IntPtr) as logic
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	local lResult := true as logic
	if oStream != null_object
		lResult := oStream:Position == oStream:Length
	endif	 
	return lResult

/// <summary>
/// Lock a portion of an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="iOffset"></param>
/// <param name="iLength"></param>
/// <returns>
/// </returns>
function FFLock(pHandle as IntPtr,iOffset as int64,iLength as int64) as logic
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	local lResult := false as logic
	if oStream != null_object
		try
			oStream:Lock(iOffset, iLength)
			lResult := true
		catch 
			// Catch and save error
			lResult := false
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif	
	return lResult


/// <summary>
/// Lock a portion of an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwOffset"></param>
/// <param name="dwLength"></param>
/// <returns>
/// </returns>
function FFLock(phandle as IntPtr,dwOffset as dword,dwLength as dword) as logic
	return FFLock(pHandle, (int64) dwOffSet, (int64) dwLength)

/// <summary>
/// Flush to disk a file opened with a low-level file function.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FFlush(phandle as IntPtr) as void
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	if oStream != null_object
		try
			oStream:Flush()
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif
	return 

/// <summary>
/// Unlock a portion of an opened file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="iOffset"></param>
/// <param name="iLength"></param>
/// <returns>
/// </returns>
function FFUnLock(phandle as IntPtr,iOffset as int64,iLength as int64) as logic
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	local lResult := false as logic
	if oStream != null_object
		try
			oStream:UnLock(iOffset, iLength)
			lResult := true
		catch 
			// Catch and save error
			lResult := false
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif	
	return lResult   

/// <summary>
/// Unlock a portion of an opened file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwOffset"></param>
/// <param name="dwLength"></param>
/// <returns>
/// </returns>
function FFUnLock(phandle as IntPtr,dwOffset as dword,dwLength as dword) as logic
	return FFUnLock(pHandle, (int64) dwOffSet, (int64) dwLength)

/// <summary>
/// Read a line from an open file, specifying two strongly typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FGetS2(pHandle as IntPtr,nBuffLen as int) as string
	local iResult := 0 as int
	local cResult as string
	local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
	cResult := String.Empty
	if oStream != null_object
		try
			local aBytes := byte[]{nBuffLen} as byte[]
			iResult := oStream:Read(aBytes, 0, nBuffLen)
			cResult  := Bytes2String(aBytes, iResult)
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif
	
	return cResult


function FGetS2(pHandle as IntPtr,nBuffLen as dword) as string
	return FGetS2(pHandle, (int) nBuffLen)


/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FPutS3(pHandle as IntPtr,c as string,nCount as dword) as dword
	return FWriteLine(pHandle, c, nCount)




/// <summary>
/// Read characters from a file into an allocated buffer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer"> Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FRead3(pHandle as IntPtr,pBuffer as byte[],dwCount as int) as int64
	local iResult := 0 as int64
	var oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		try
			iResult := oStream:Read(pBuffer,0,(int) dwCount)
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
		
	endif
	
	return iResult

function FRead3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) Fread3(pHandle, pBuffer, (int) dwCount)



/// <summary>
/// Read characters from a file into an allocated buffer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer"> Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <param name="lAnsi"></param>
/// <returns>
/// </returns>
function FRead4(pHandle as IntPtr,pBuffer as byte[],dwCount as dword,lAnsi as logic) as dword
	local iResult := Fread3(pHandle, pBuffer, dwCount) as int64
	if !lAnsi .and. iResult > 0
		pBuffer := Oem2Ansi(pBuffer)
	endif
	
	return (dword) iResult

/// <summary>
/// Read a line from an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FReadLine2(pFile as IntPtr,nBuffLen as dword) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Read characters from a file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadStr(pHandle as IntPtr,iCount as int) as string
	local aBuffer as byte[]
	local cResult as STRING
	aBuffer := Byte[]{iCount}
	iCount := (int) Fread3(pHandle, aBuffer, iCount)
	cResult := Bytes2String(aBuffer, iCount)
	return cResult

	

/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="refC"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadText(pHandle as IntPtr,refC as object,dwCount as dword) as dword
	/// THROW NotImplementedException{}
	return 0   

/// <summary>
/// Read characters from a file into an allocated buffer, with possible OEM to ANSI conversion, based on the current SetAnsi() setting.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadText3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return Fread4(pHandle, pBuffer, dwCount, ! GetAnsi())	


/// <summary>
/// Set the file pointer at the top of an open file.
/// </summary>
/// <param name="pFile"></param>
/// <returns>
/// </returns>
function FRewind(pHandle as IntPtr) as void
	local oStream as FileStream
	local iResult as int64
	oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		oStream:Position := 0
	endif
	return   

/// <summary>
/// Set the file pointer to a new position, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="lOffset"></param>
/// <param name="dwOrigin"></param>
/// <returns>
/// </returns>
function FSeek3(pHandle as IntPtr,lOffset as long,dwOrigin as dword) as int64
	local oStream as FileStream
	local iResult as int64
	oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		iResult := -1
		try
			switch dwOrigin
				case FS_END
					iResult := oStream:Seek(lOffSet, SeekOrigin.End)
				case FS_RELATIVE
					iResult := oStream:Seek(lOffSet, SeekOrigin.Current)
				case FS_SET
					iResult := oStream:Seek(lOffSet, SeekOrigin.Begin)
				otherwise
					iResult := -1					
			end switch
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
		return iResult 
	endif
	return -1

/// <summary>
/// Return the current position of the file pointer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FTell(pHandle as IntPtr) as int64
	local oStream as FileStream
	oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		try
			return oStream:Position
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
	endif
	return -1



/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FWrite( pHandle as IntPtr, c as string ) as dword
	return (dword) FWrite( pHandle, c,  c:Length )


/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FWrite( pHandle as IntPtr, c as string, nLength as int ) as int
	local oStream	as FileStream
	local iWritten := 0 as int
	oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		try
			local aBytes as byte[]
			aBytes := String2Bytes(c)
			oStream:Write(aBytes, 0, nLength)
			iWritten := nLength
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
		
	endif
	
	return iWritten

/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>	   
function FWrite( pHandle as IntPtr, c as string, nLength as dword ) as dword
	return (dword) FWrite(pHandle, c, (int) nLength)

/// <summary>
/// Write the contents of a buffer to an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="ptrBuffer"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FWrite3(pHandle as IntPtr,pBuffer as byte[],iCount as int) as int
	local oStream	as FileStream
	local iWritten := 0 as int
	oStream := XSharp.IO.File.findStream(pHandle)
	if oStream != null_object
		try
			oStream:Write(pBuffer, 0, iCount)
			iWritten := iCount
		catch
			FError((dword)Marshal.GetLastWin32Error())
		end try
		
	endif
	
	return iWritten

/// <summary>
/// Write the contents of a buffer to an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FWrite3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) fWrite3(pHandle, pBuffer, (int) dwCount)


/// <summary>
/// Write the contents of a buffer to an open file, with an ANSI to OEM conversion option.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <param name="lAnsi"></param>
/// <returns>
/// </returns>
function FWrite4(pHandle as IntPtr,pBuffer as byte[],dwCount as dword,lAnsi as logic) as dword
	if lAnsi
		pBuffer := Ansi2Oem(pBuffer)
	endif
	return FWrite3(pHandle, pBuffer, dwCount)



/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FWriteLine(pHandle as IntPtr,c as string) as dword
	return (dword) FWrite(pHandle, c + e"\r\n",c:Length+2)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine(pHandle as IntPtr,c as string,nCount as dword) as dword
	return FWriteLine3(pHandle, c, nCount)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine3(pHandle as IntPtr,c as string,nCount as dword) as dword
	if nCount < c:Length
		c := Left(c, nCount)
	endif
	return FWrite(pHandle, c + e"\r\n",nCount+2)

/// <summary>
/// Write the contents of a buffer to an open file, with SetAnsi() dependency.
/// </summary>
/// <param name="pHandle">The handle of the file to read from.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FWriteText3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return FWrite4(pHandle ,pBuffer ,dwCount ,!GetAnsi()) 




/// <summary>
/// Remove spaces from a file name specified as a string.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
function AdjustFName(cName as string) as string
	local adjusted := null as string
	if ( !string.IsNullOrEmpty(cName) ) 
		adjusted := System.IO.Path.GetFileNameWithoutExtension(cName):TrimEnd()
		if ( cName:IndexOf('.') > 0 ) 
			adjusted += System.IO.Path.GetExtension(cName)
		endif
	endif
	return adjusted   



/// <summary>
/// Remove spaces from a file name specified as a string, changing the contents of the original file name as well as the returned file name.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
function AdjustFNameA(cName as string) as string
	throw NotImplementedException{}
/// RETURN String.Empty   




/// <summary>
/// Create a file or open and truncate an existing file, specifying two strongly typed arguments.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwAttr"></param>
/// <returns>
/// </returns>
function FCreate2(cFile as string,dwAttr as dword) as IntPtr
	local oFileMode as VOFileMode
	oFileMode := VOFileMode{ FO_CREATE, dwAttr }
	return XSharp.IO.File._createFile(cFile, oFileMode)



/// <summary>
/// Determine if any file matches a given file specification.
/// </summary>
/// <param name="cFile">The name oif the file</param>
/// <returns>
/// True if the file exists, otherwise false
/// </returns>
function File(cFile as string) as logic
	local lFound as logic
	lFOund := System.IO.File.Exists(cFile)
	if lFound
		XSharp.IO.File.LastFOund := cFile
	else
		XSharp.IO.File.LastFOund := ""
	endif
	return lFound

/// <summary>
/// Return the name and path of the file that was used by FXOpen() or File().
/// </summary>
/// <returns>
/// </returns>
function FPathName() as string
	
	return XSharp.IO.File.LastFound

/// <summary>
/// Open a file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwMode"></param>
/// <returns>
/// </returns>
function FOpen2(cFile as string,dwMode as dword) as IntPtr
	local oFileMode as VOFileMode
	oFileMode := VOFileMode{dwMode, 0}
	return XSharp.IO.File._createFile(cFile, oFileMode)






/// <summary>
/// Open a file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwMode"></param>
/// <param name="cPath"></param>
/// <returns>
/// </returns>
function FxOpen(cFile as string,dwMode as dword,cPath as string) as IntPtr
	/// THROW NotImplementedException{}
	return IntPtr.Zero



/// <summary>
/// </summary>
/// <param name="cAttr"></param>
/// <returns>
/// </returns>
function String2FAttr(cAttr as string) as dword
	/// THROW NotImplementedException{}
	return 0   




function Bytes2String(aBytes as byte[], nBuffLen as int) as string
	local aChars := char[]{nBuffLen} as char[]
	local encoding := Encoding.Default as Encoding
	encoding:GetChars(aBytes, 0, nBuffLen, aChars, 0)
	return string{aChars, 0, nBuffLen}

function String2Bytes(sSource as string) as byte[]
	local ret as byte[]
	if sSource != null
		local encoding := Encoding.Default as Encoding  
		ret := encoding:GetBytes( sSource ) 
	else
		ret := byte[]{0}
	endif   
	return ret



