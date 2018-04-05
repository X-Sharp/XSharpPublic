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
	
	define OF_SHARE_COMPAT		:= 0x00000000
	define OF_SHARE_EXCLUSIVE	:= 0x00000010
	define OF_SHARE_DENY_WRITE	:= 0x00000020
	define OF_SHARE_DENY_READ	:= 0x00000030
	define OF_SHARE_DENY_NONE	:= 0x00000040
	define OF_PARSE				:= 0x00000100
	
	define CREATE_NEW := 1
	define CREATE_ALWAYS := 2
	define OPEN_EXISTING := 3
	define OPEN_ALWAYS := 4
	define FO_DELETE := 0x00000200
	define FO_VERIFY := 0x00000400
	define FO_CANCEL := 0x00000800
	define FO_CREATE := 0x00001000
	define FO_PROMPT := 0x00002000
	define FO_EXIST  := 0x00004000
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
	
	
#endregion


begin namespace XSharp.IO
	
	
	internal class VOFileMode
		property lWild			as logic auto
		property FileMode		as FileMode auto 
		property FileAccess		as FileAccess auto 
		property Attributes	    as dword auto
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
			fileAccess	:= FileAccess.read
			if (dword)_and(dwMode,FO_READWRITE) == FO_READWRITE
				fileAccess	:= FileAccess.readWrite
			elseif (dword)_and(dwMode,FO_WRITE) == FO_WRITE
				fileAccess	:= FileAccess.write
			endif
			// Create
			fileMode := FileMode.Open
			if (dword)_and(dwMode,FO_CREATE) == FO_CREATE
				fileMode	:= FileMode.Create
				fileAccess	:= FileAccess.readWrite
			endif
			
			FileShare  := FileShare.readWrite
			local dwTempMode as dword
			dwTempMode := (dword)_or(OF_SHARE_DENY_WRITE, OF_SHARE_DENY_READ, OF_SHARE_DENY_NONE)
			dwTempMode := (dword)_and(dwMode,dwTempMode)
			
			do case
				case dwTempMode == FO_DENYNONE
					FileShare  := FileShare.readWrite
				
				case dwTempMode == FO_DENYWRITE
					FileShare  := FileShare.read
				
				case dwTempMode == FO_DENYREAD
					FileShare  := FileShare.write
				
				case dwTempMode == FO_EXCLUSIVE
					FileShare  := FileShare.None
				
				case dwTempMode == FO_COMPAT
					FileShare  := FileShare.readWrite
				
			endcase
			return
		
		
	end class
	
	internal class FileCacheElement
		public property Stream	   as FileStream auto
		public property Attributes as dword auto
		public property Bytes	   as byte[] auto
		
		constructor (oStream as FileStream, dwAttributes as dword)
			self:Attributes := dwAttributes
			self:Stream     := oStream
			return
	end class
	
	static class File
		public static errorCode	:= 0 as dword
		public static LastFound as string
		private static streams as Dictionary<IntPtr, FileCacheElement >
		private static random as Random
		
		static constructor
			streams := Dictionary<IntPtr, FileCacheElement >{}
			random := Random{}
		
		static private method findStream(pStream as IntPtr) as FileStream
			if streams:ContainsKey(pStream)
				return streams[pStream]:Stream
			endif
			return null_object 
		
		static private method hasStream(pStream as Intptr) as logic
			return streams:ContainsKey(pStream)
		
		static private method addStream(pStream as Intptr, oStream as FileStream, attributes as dword) as logic
			if ! streams:ContainsKey(pStream)
				streams:Add(pStream, FileCacheElement {oStream, attributes})
				return true
			endif
			return false
		
		static private method removeStream(pStream as Intptr) as logic
			if streams:ContainsKey(pStream)
				streams:Remove(pStream)
				return true
			endif
			return false
		
		static private method createManagedFileStream(cFIle as string, oMode as VOFileMode) as FileStream
			local oStream := null as FileSTream
			try
				oStream := FileStream{cFile, oMode:FileMode, oMode:FileAccess, oMode:FileShare, 4096}
			catch e as Exception
				System.Diagnostics.Trace.writeLine(e:Message)
				FError((dword)Marshal.GetLastWin32Error())
			end try
			return oStream
		
		static internal method CreateFile(cFIle as string, oMode as VOFileMode) as IntPtr
			local hFile := F_ERROR as IntPtr
			local oStream as FileStream
			if System.IO.File.Exists(cFile) .and. oMode:FileMode == FileMode.Create
				var fi := FileInfo{cFile}
				fi:Attributes := FileAttributes.Normal
			endif
			
			oStream := createManagedFileStream(cFile, oMode)
			if oStream != null
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
		
		static private method getBuffer(pStream as IntPtr, nSize as int) as byte[]		
			if hasStream(pStream)
				local element := streams[pStream] as FileCacheElement
				if element:Bytes == null .or. element:Bytes:Length < nSize
					element:Bytes := byte[]{nSize}
				endif
				return element:Bytes
			endif
			return byte[]{nSize}
		
		static internal method close(pStream as IntPtr) as logic
			if hasStream(pStream)
				local oStream      := streams[pStream]:Stream as FileStream
				local dwAttributes := streams[pStream]:Attributes as dword
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
		
		internal static method read(pHandle as ptr, refC out string, iCount as int, lOem2Ansi as logic) as int
			local aBuffer := XSharp.IO.File.getBuffer(pHandle, iCount) as byte[]
			iCount := (int) ReadBuff(pHandle, aBuffer, iCount, lOem2Ansi)
			refC := Bytes2String(aBuffer, iCount)
			return iCount
		
		internal static method readBuff(pHandle as IntPtr,pBuffer as byte[],dwCount as int, lOem2Ansi as logic) as int64
			local iResult := 0 as int64
			var oStream := XSharp.IO.File.findStream(pHandle)
			if oStream != null_object
				try
					iResult := oStream:Read(pBuffer,0,(int) dwCount)
					if lOem2Ansi
						pBuffer := Oem2Ansi(pBuffer)
					endif
				catch
					FError((dword)Marshal.GetLastWin32Error())
				end try
			endif
			return iResult
		
		internal static method readLine(pFile as IntPtr,iCount as int) as string
			local cResult := "" as string
			local nPos	as int64
			local oStream := XSharp.IO.File.findStream(pFile) as FileStream
			if iCount <= 0
				// According to the VO docs the default value for the buffer length = 256
				iCount := 256
			endif
			if oStream != null_object
				try
					local pBuffer := XSharp.IO.File.getBuffer(pFile, iCount) as byte[]
					nPos    := oStream:Position
					iCount := oStream:Read(pBuffer,0,(int) iCount)
					cResult := Bytes2Line(pBuffer, ref iCount)
					nPos += iCount	// advance CRLF
					oStream:Position := nPos
				catch
					FError((dword)Marshal.GetLastWin32Error())
				end try
			endif
			return cResult
		
		internal static method seek(pHandle as IntPtr,lOffset as int64,dwOrigin as dword) as int64
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
		
		internal static method write( pHandle as IntPtr, c as string, nLength as int ) as int
			local aBytes := String2Bytes(c) as byte[]
			return writeBuff(pHandle, aBytes, nLength, false)			
		
		internal static method writeBuff(pHandle as IntPtr,pBuffer as byte[],iCount as int, lAnsi2Oem as logic) as int
			local oStream	as FileStream
			local iWritten := 0 as int
			oStream := XSharp.IO.File.findStream(pHandle)
			if oStream != null_object
				try
					if lAnsi2Oem
						pBuffer := Ansi2Oem(pBuffer)
					endif
					oStream:Write(pBuffer, 0, iCount)
					iWritten := iCount
				catch
					FError((dword)Marshal.GetLastWin32Error())
				end try
				
			endif
			
			return iWritten
		
		internal static method writeLine(pHandle as IntPtr,c as string, nLen as int) as int
			if c:Length > nLen
				c := c:Substring(0, nLen)
			endif
			return Write(pHandle, c + e"\r\n",nLen+2)
		
		
		internal static method lock(phandle as IntPtr,iOffset as int64,iLength as int64, lLock as logic) as logic
			local oStream := findStream(pHandle) as FileStream
			local lResult := false as logic
			if oStream != null_object
				try
					if (lLock)
						oStream:Lock(iOffset, iLength)
					else
						oStream:UnLock(iOffset, iLength)
					endif
					lResult := true
				catch 
					// Catch and save error
					lResult := false
					FError((dword)Marshal.GetLastWin32Error())
				end try
			endif	
			return lResult   		
		
		internal static method flush(phandle as IntPtr, lCommit as logic) as logic
			local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
			local lOk := false as logic
			if oStream != null_object
				try
					if lCommit
						oStream:Flush(true)
					else
						oStream:Flush()
					endif
					lOk := true
				catch 
					FError((dword)Marshal.GetLastWin32Error())
					lOk := false 
				end try
			endif	
			return lOk
		internal static method ChSize(pHandle as IntPtr,nValue as dword) as logic
			local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
			local lOk := false as logic
			if oStream != null_object
				try
					oStream:SetLength(nValue)
					lOk := true
				catch 
					FError((dword)Marshal.GetLastWin32Error())
					lOk := false
				end try
			endif	
			return lOk
		
		internal static method Eof(pHandle as IntPtr) as logic
			local oStream := XSharp.IO.File.findStream(pHandle) as FileStream
			local lResult := true as logic
			if oStream != null_object
				lResult := oStream:Position == oStream:Length
			endif	 
			return lResult
		
		internal static method Tell(pHandle as IntPtr) as int64
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
function FChSize(pHandle as IntPtr,nValue as dword) as logic
	return XSharp.IO.File.Chsize(pHandle, nValue)

/// <summary>
/// Close an open file and write the buffers to disk.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FClose(pFile as IntPtr) as logic
	return XSharp.IO.File.Close(pFile)

/// <summary>
/// Flush file buffers.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FCommit(pHandle as IntPtr) as logic
	return XSharp.IO.File.flush(pHandle, true)

/// <summary>
/// Determine if the file pointer is positioned at the end-of-file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FEof(pHandle as IntPtr) as logic
	return XSharp.IO.File.eof(pHandle) 


/// <summary>
/// Lock a portion of an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwOffset"></param>
/// <param name="dwLength"></param>
/// <returns>
/// </returns>
function FFLock(phandle as IntPtr,dwOffset as dword,dwLength as dword) as logic
	return XSharp.IO.File.lock(pHandle, (int64) dwOffset, (int64) dwLength, true)


/// <summary>
/// Flush to disk a file opened with a low-level file function.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FFlush(phandle as IntPtr) as logic
	return XSharp.IO.File.flush(pHandle, false)


/// <summary>
/// Unlock a portion of an opened file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwOffset"></param>
/// <param name="dwLength"></param>
/// <returns>
/// </returns>
function FFUnLock(phandle as IntPtr,dwOffset as dword,dwLength as dword) as logic
	return XSharp.IO.File.lock(pHandle, (int64) dwOffset, (int64) dwLength, false)


/// <summary>
/// Read a line from an open file, specifying two strongly typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FGetS(pHandle as IntPtr) as string
	// According to the VO docs the dedault value for the buffer length = 256
	return XSharp.IO.File.readLine(pHandle,256)


/// <summary>
/// Read a line from an open file, specifying two strongly typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FGetS2(pHandle as IntPtr,nBuffLen as dword) as string
	return XSharp.IO.File.readLine(pHandle, (int) nBuffLen)


/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FPutS(pHandle as IntPtr,c as string) as dword
	return FWriteLine(pHandle, c, (dword) c:Length)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FPutS(pHandle as IntPtr,c as string,nCount as dword) as dword
	return FWriteLine(pHandle, c, nCount)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FPutS3(pHandle as IntPtr,c as string, nCount as dword) as dword
	return FWriteLine(pHandle, c, nCount)


/// <summary>
/// Read characters from a file into an allocated buffer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer"> Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FRead3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) XSharp.IO.File.readBuff(pHandle, pBuffer, (int) dwCount, false)

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
	return (dword) XSharp.IO.File.readBuff(pHandle, pBuffer, (int) dwCount, lAnsi)


/// <summary>
/// Read a line from an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FReadLine(pFile as IntPtr,nBuffLen as dword) as string
	return XSharp.IO.File.readLine(pFile, (int) nBuffLen)


/// <summary>
/// Read a line from an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pFile"></param>
/// <param name="nBuffLen"></param>
/// <returns>
/// </returns>
function FReadLine2(pFile as IntPtr,nBuffLen as dword) as string	
	return XSharp.IO.File.readLine(pFile, (int) nBuffLen)



/// <summary>
/// Read characters from a file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadStr(pHandle as IntPtr,iCount as dword) as string
	local cResult as string
	XSharp.IO.File.read(pHandle, out cResult, (int) iCount, false)
	return cResult


/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="strValue"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadText(pHandle as IntPtr,strValue ref string,dwCount as dword) as dword
	return (dword) XSharp.IO.File.read(pHandle, out strValue, (int) dwCount, !XSharp.RuntimeState.Ansi)

/// <summary>
/// Read characters from a file into an allocated buffer, with possible OEM to ANSI conversion, based on the current SetAnsi() setting.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FReadText3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) XSharp.IO.File.readBuff(pHandle, pBuffer, (int) dwCount, ! GetAnsi())	

/// <summary>
/// Set the file pointer at the top of an open file.
/// </summary>
/// <param name="pFile"></param>
/// <returns>
/// </returns>
function FRewind(pHandle as IntPtr) as logic
	return XSharp.IO.File.Seek(pHandle, 0, FS_SET) == 0


/// <summary>
/// Set the file pointer to a new position, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="lOffset"></param>
/// <param name="dwOrigin"></param>
/// <returns>
/// </returns>
function FSeek3(pHandle as IntPtr,lOffset as long, dwOrigin as dword) as long
	return (long) XSharp.IO.File.Seek(pHandle, (int64) lOffset, dwOrigin)

/// <summary>
/// Return the current position of the file pointer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FTell(pHandle as IntPtr) as dword
	return (DWORD) XSharp.IO.File.Tell(pHandle)


/// <summary>
/// Return the current position of the file pointer.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <returns>
/// </returns>
function FTell64(pHandle as IntPtr) as Int64
	return XSharp.IO.File.Tell(pHandle)


/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FWrite( pHandle as IntPtr, c as string ) as dword
	return (dword) XSharp.IO.File.write( pHandle, c,  c:Length )



/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>	   
function FWrite( pHandle as IntPtr, c as string, nLength as dword ) as dword
	return (dword) XSharp.IO.File.write(pHandle, c, (int) nLength)


/// <summary>
/// Write the contents of a buffer to an open file.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FWrite3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) XSharp.IO.File.writeBuff(pHandle, pBuffer, (int) dwCount, false)


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
	return (dword) XSharp.IO.File.writeBuff(pHandle,pBuffer , (int) dwCount ,lAnsi )


/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <returns>
/// </returns>
function FWriteLine(pHandle as IntPtr,c as string) as dword
	return (dword) XSharp.IO.File.write(pHandle, c + e"\r\n",c:Length+2)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine(pHandle as IntPtr,c as string,nCount as dword) as dword
	return (dword) XSharp.IO.File.writeLine(pHandle, c, (int) nCount)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pHandle">The handle of the file.</param>
/// <param name="c"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function FWriteLine3(pHandle as IntPtr,c as string,nCount as dword) as dword
	return (dword) XSharp.IO.File.writeLine(pHandle, c, (int) nCount )

/// <summary>
/// Write the contents of a buffer to an open file, with SetAnsi() dependency.
/// </summary>
/// <param name="pHandle">The handle of the file to read from.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function FWriteText3(pHandle as IntPtr,pBuffer as byte[],dwCount as dword) as dword
	return (dword) XSharp.IO.File.writeBuff(pHandle ,pBuffer ,(int) dwCount ,!GetAnsi()) 

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
	return XSharp.IO.File.CreateFile(cFile, oFileMode)



/// <summary>
/// Create a file or open and truncate an existing file, specifying the filename
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
function FCreate2(cFile as string) as IntPtr
	return FCreate(cFile, FC_NORMAL)


/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
function FCreate(cFile as string ) as IntPtr
	return FCreate2(cFile, FA_NORMAL)


/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwFileAttr"></param>
/// <returns>
/// </returns>
function FCreate(cFile as string ,dwFileAttr as DWORD) as IntPtr
	return FCreate2(cFile, dwFileAttr)


/// <summary>
/// Create a file or open and truncate an existing file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="iFileAttr "></param>
/// <returns>
/// </returns>
function FCreate(cFile as string ,iFileAttr as int) as IntPtr
	return FCreate2(cFile, (DWORD) iFileAttr)


/// <summary>
/// Open a file, specifying one strongly-typed arguments.
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
function FOpen(cFile as string) as IntPtr
	return FOpen2(cFile, FC_NORMAL)

/// <summary>
/// Open a file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwMode"></param>
/// <returns>
/// </returns>
function FOpen(cFile as string,dwMode as dword) as IntPtr
	return FOpen2(cFile, dwMode)

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
	return XSharp.IO.File.CreateFile(cFile, oFileMode)








/// <summary>
/// Convert file attributes to numbers.
/// </summary>
/// <param name="uxFileAttr"></param>
/// <returns>
/// </returns>
function GetFAttr(cAttributes as string) as dword
	return String2FAttr(cAttributes)
	
	/// <summary>
	/// Convert file attributes to numbers.
	/// </summary>
	/// <param name="dwAttributes"></param>
	/// <returns>
	/// Does not do anything. Returns the original value
	/// </returns>
function GetFAttr(dwAttributes as dword) as dword
	return dwAttributes
	
	
	/// <summary>
	/// Prepare a file specification for wildcard searching.
	/// </summary>
	/// <param name="cFileMask"></param>
	/// <returns>
	/// When the source string ends with ":" or "\" then *.* is added
	/// </returns>
function GetFMask(cFileMask as string) as string
	local cResult as STRING
	if String.IsNullOrEmpty(cFileMask)
		cResult := "*.*"
	else
		var cChar := cFilemask[cFileMask:Length-1]
		switch cChar
		case ':'
		case '\\'
			cResult := cFileMask + "*.*"
		otherwise
			cResult := cFileMask
		end switch
	endif
	return cResult
	
/// </summary>
/// <param name="cAttr"> One or more of the following constants or strings: ADHSRV</param>
/// <returns>
/// </returns>
function String2FAttr(cAttr as string) as dword
	local dwAttributes := FC_NORMAL as dword
	if !String.IsNullOrEmpty(cAttr)
		foreach c as char in cAttr
			switch Char.ToUpper(c)
				case 'A'
					dwAttributes |= FC_ARCHIVED
				case 'C'
					dwAttributes |= FA_COMPRESSED
				case 'D'
					dwAttributes |= FA_DIRECTORY
				case 'H'
					dwAttributes |= FC_HIDDEN
				case 'R'
					dwAttributes |= FC_READONLY
				case 'S'
					dwAttributes |= FC_SYSTEM
				case 'V'
					dwAttributes |= FA_VOLUME
			end switch	
		next
	endif
	return dwAttributes



/// <summary>
/// Display file attributes as a string.
/// </summary>
/// <param name="nAttrib"></param>
/// <returns>
/// </returns>
function FAttr2String(dwAttributes as dword) as string
	local cAttribute := "" as string
	
	if (dword)_and(dwAttributes,FA_DIRECTORY) == FA_DIRECTORY
		cAttribute := cAttribute + "D"
	endif
	if (dword)_and(dwAttributes,FA_VOLUME) == FA_VOLUME
		cAttribute := cAttribute + "V"
	endif
	if (dword)_and(dwAttributes,FC_ARCHIVED) == FC_ARCHIVED
		cAttribute := cAttribute + "A"
	endif
	if (dword)_and(dwAttributes,FC_HIDDEN) == FC_HIDDEN
		cAttribute := cAttribute + "H"
	endif
	if (dword)_and(dwAttributes,FC_READONLY) == FC_READONLY
		cAttribute := cAttribute + "R"
	endif
	if (dword)_and(dwAttributes,FC_SYSTEM) == FC_SYSTEM
		cAttribute := cAttribute + "S"
	endif
	
	return cAttribute



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





function Bytes2Line(aBytes as byte[], nBuffLen ref int) as string
	// determine end of line
	// note that VO looks for CR LF. CR alone or LF alone to not count as end of line
	local nLF as int
	local lFoundCR := false as logic
	local lFoundCRLF := false as logic
	nLF := nBuffLen
	for var nI := 1 to nBuffLen
		switch aBytes[nI]
			case 10
				if lFoundCR		// immediately LF after CR
					nLF := nI -2
					lFoundCRLF := true
				endif
			case 13
				lFoundCR := true
			otherwise
				lFoundCR := false
		end switch
		if lFoundCRLF
			exit
		endif
	next
	// when we have found a CRLF then nLF is the length of the string before the CR
	// otherwise the length of the source string
	local aChars := char[]{nLF} as char[]
	local encoding := Encoding.Default as Encoding
	encoding:GetChars(aBytes, 0, nLF, aChars, 0)
	if lFoundCRLF
		nBuffLen := nLF+2
	endif
	return string{aChars, 0, nLF}
