//
// Copyright (c) XSharp B.V.  All Rights Reserved. 
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.IO
USING System.Linq
USING System.Runtime.InteropServices
USING System.Security
USING Microsoft.Win32.SafeHandles
USING System.Runtime
USING System.Runtime.ConstrainedExecution
USING System.Text

#region Defines
	DEFINE F_ERROR :=  -1 // Error value (all functions)
	
	// FERROR() returns, which are not reflected as DOSERROR()
	DEFINE FERROR_FULL    := 256   // disk full
	DEFINE FERROR_EOF     := 257   // eof was already reached, when a read was tried
	DEFINE FERROR_PARAM   := 258   // invalid parameter already detected before giving to DOS
	
	// FSEEK(), _llseek() modes
	DEFINE FS_SET         := 0  // Seek from beginning of file
	DEFINE FS_RELATIVE    := 1  // Seek from current file position
	DEFINE FS_END         := 2  // Seek from end of file
	
	// FOPEN() access modes
	DEFINE FO_READ        := 0  // Open for reading (default)
	DEFINE FO_WRITE       := 1  // Open for writing
	DEFINE FO_READWRITE   := 2  // Open for reading or writing
	
	// FOPEN() sharing modes (combine with open mode using +)
	DEFINE FO_COMPAT     := 0x00000000  // Compatibility mode (default)
	DEFINE FO_EXCLUSIVE  := 0x00000010  // Exclusive
	DEFINE FO_DENYWRITE  := 0x00000020  // Prevent other processes from writing
	DEFINE FO_DENYREAD   := 0x00000030  // Prevent other processes from reading
	DEFINE FO_DENYNONE   := 0x00000040  // (same as FO_SHARED)
	DEFINE FO_SHARED     := 0x00000040  // Allow other processes to read or write
	
	DEFINE OF_SHARE_COMPAT		:= 0x00000000
	DEFINE OF_SHARE_EXCLUSIVE	:= 0x00000010
	DEFINE OF_SHARE_DENY_WRITE	:= 0x00000020
	DEFINE OF_SHARE_DENY_READ	:= 0x00000030
	DEFINE OF_SHARE_DENY_NONE	:= 0x00000040
	DEFINE OF_PARSE				:= 0x00000100
	
	DEFINE CREATE_NEW := 1
	DEFINE CREATE_ALWAYS := 2
	DEFINE OPEN_EXISTING := 3
	DEFINE OPEN_ALWAYS := 4
	DEFINE FO_DELETE := 0x00000200
	DEFINE FO_VERIFY := 0x00000400
	DEFINE FO_CANCEL := 0x00000800
	DEFINE FO_CREATE := 0x00001000
	DEFINE FO_PROMPT := 0x00002000
	DEFINE FO_EXIST  := 0x00004000
	DEFINE FO_REOPEN := 0x00008000
	
	// FXOPEN() mode
	DEFINE FXO_WILD      := 0x00010000  // Allow wildcards in file name
	
	// FCREATE() file attribute modes (always opens with OF_READWRITE)
	DEFINE FC_NORMAL     := 0x00000000  // normal read/write file (default for create)
	DEFINE FC_READONLY   := 0x00000001  // read-only file
	DEFINE FC_HIDDEN     := 0x00000002  // hidden file
	DEFINE FC_SYSTEM     := 0x00000004  // system file
	DEFINE FC_ARCHIVED   := 0x00000020
	
	//
	// additional file attribute for DIRECTORY(), FFIRST() and FFCOUNT()
	//
	DEFINE FA_VOLUME     := 0x00000008
	DEFINE FA_DIRECTORY  := 0x00000010
	DEFINE FA_NORMAL	 := 0x00000080
	DEFINE FA_TEMPORARY  := 0x00000100
	DEFINE FA_COMPRESSED := 0x00000800
	DEFINE FA_OFFLINE    := 0x00001000
	
	
#endregion


BEGIN NAMESPACE XSharp.IO
	
	
	INTERNAL CLASS VOFileMode
		PROPERTY lWild			AS LOGIC AUTO
		PROPERTY FileMode		AS FileMode AUTO 
		PROPERTY FileAccess		AS FileAccess AUTO 
		PROPERTY Attributes	    AS DWORD AUTO
		PROPERTY FileShare		AS FileShare AUTO
		
		CONSTRUCTOR(dwMode AS DWORD, dwAttribs AS DWORD)
			// Wildcard
			IF (DWORD)_AND(dwMode, FXO_WILD) == FXO_WILD
				lWild := TRUE
				dwMode := dwMode - FXO_WILD
			ELSE
				lWild := FALSE
			ENDIF
			Attributes := dwAttribs
			
			// Access
			fileAccess	:= FileAccess.read
			IF (DWORD)_AND(dwMode,FO_READWRITE) == FO_READWRITE
				fileAccess	:= FileAccess.readWrite
			ELSEIF (DWORD)_AND(dwMode,FO_WRITE) == FO_WRITE
				fileAccess	:= FileAccess.write
			ENDIF
			// Create
			fileMode := FileMode.Open
			IF (DWORD)_AND(dwMode,FO_CREATE) == FO_CREATE
				fileMode	:= FileMode.Create
				fileAccess	:= FileAccess.readWrite
			ENDIF
			
			FileShare  := FileShare.readWrite
			LOCAL dwTempMode AS DWORD
			dwTempMode := (DWORD)_OR(OF_SHARE_DENY_WRITE, OF_SHARE_DENY_READ, OF_SHARE_DENY_NONE)
			dwTempMode := (DWORD)_AND(dwMode,dwTempMode)
			
			DO CASE
				CASE dwTempMode == FO_DENYNONE
					FileShare  := FileShare.readWrite
				
				CASE dwTempMode == FO_DENYWRITE
					FileShare  := FileShare.read
				
				CASE dwTempMode == FO_DENYREAD
					FileShare  := FileShare.write
				
				CASE dwTempMode == FO_EXCLUSIVE
					FileShare  := FileShare.None
				
				CASE dwTempMode == FO_COMPAT
					FileShare  := FileShare.readWrite
				
			ENDCASE
			RETURN
		
		
	END CLASS
	
	INTERNAL CLASS FileCacheElement
		PUBLIC PROPERTY Stream	   AS FileStream AUTO
		PUBLIC PROPERTY Attributes AS DWORD AUTO
		PUBLIC PROPERTY Bytes	   AS BYTE[] AUTO
		
		CONSTRUCTOR (oStream AS FileStream, dwAttributes AS DWORD)
			SELF:Attributes := dwAttributes
			SELF:Stream     := oStream
			RETURN
	END CLASS
	/// <exclude />
	INTERNAL STATIC CLASS File
        /// <exclude />
        PUBLIC STATIC errorCode	:= 0 AS DWORD
        /// <exclude />
		PUBLIC STATIC LastFound AS STRING
		PRIVATE STATIC streams AS Dictionary<IntPtr, FileCacheElement >
		PRIVATE STATIC random AS Random
		
	    /// <exclude />
        STATIC CONSTRUCTOR
			streams := Dictionary<IntPtr, FileCacheElement >{}
			random := Random{}
		
		STATIC PRIVATE METHOD findStream(pStream AS IntPtr) AS FileStream
			IF streams:ContainsKey(pStream)
				RETURN streams[pStream]:Stream
			ENDIF
			RETURN NULL_OBJECT 
		
		STATIC PRIVATE METHOD hasStream(pStream AS Intptr) AS LOGIC
			RETURN streams:ContainsKey(pStream)
		
		STATIC PRIVATE METHOD addStream(pStream AS Intptr, oStream AS FileStream, attributes AS DWORD) AS LOGIC
			IF ! streams:ContainsKey(pStream)
				streams:Add(pStream, FileCacheElement {oStream, attributes})
				RETURN TRUE
			ENDIF
			RETURN FALSE
		
		STATIC PRIVATE METHOD removeStream(pStream AS Intptr) AS LOGIC
			IF streams:ContainsKey(pStream)
				streams:Remove(pStream)
				RETURN TRUE
			ENDIF
			RETURN FALSE
		
		STATIC PRIVATE METHOD createManagedFileStream(cFIle AS STRING, oMode AS VOFileMode) AS FileStream
			LOCAL oStream := NULL AS FileSTream
			TRY
				oStream := FileStream{cFile, oMode:FileMode, oMode:FileAccess, oMode:FileShare, 4096}
			CATCH e AS Exception
				System.Diagnostics.Trace.writeLine(e:Message)
				FError((DWORD)Marshal.GetLastWin32Error())
			END TRY
			RETURN oStream
		
		STATIC INTERNAL METHOD CreateFile(cFIle AS STRING, oMode AS VOFileMode) AS IntPtr
			LOCAL hFile := F_ERROR AS IntPtr
			LOCAL oStream AS FileStream
			IF System.IO.File.Exists(cFile) .AND. oMode:FileMode == FileMode.Create
				VAR fi := FileInfo{cFile}
				fi:Attributes := FileAttributes.Normal
			ENDIF
			
			oStream := createManagedFileStream(cFile, oMode)
			IF oStream != NULL
				hFile := (IntPtr) random:@@Next(1, Int32.MaxValue)
				DO WHILE streams:ContainsKey(hFile)
					hFile := (IntPtr) random:@@Next(1, Int32.MaxValue)
				ENDDO
			ELSE
				hFile := F_ERROR
			ENDIF
			//endif
			IF hFile != F_ERROR
				addStream(hFile, oStream, oMode:Attributes)
			ENDIF
			RETURN hFile
		
		STATIC PRIVATE METHOD getBuffer(pStream AS IntPtr, nSize AS INT) AS BYTE[]		
			IF hasStream(pStream)
				LOCAL element := streams[pStream] AS FileCacheElement
				IF element:Bytes == NULL .OR. element:Bytes:Length < nSize
					element:Bytes := BYTE[]{nSize}
				ENDIF
				RETURN element:Bytes
			ENDIF
			RETURN BYTE[]{nSize}
		
		STATIC INTERNAL METHOD close(pStream AS IntPtr) AS LOGIC
			IF hasStream(pStream)
				LOCAL oStream      := streams[pStream]:Stream AS FileStream
				LOCAL dwAttributes := streams[pStream]:Attributes AS DWORD
				removeStream(pStream)
				oStream:Flush()
				oStream:Close()
				IF dwAttributes != 0
					VAR fi := FileInfo{oStream:Name}
					fi:Attributes := (FileAttributes) dwAttributes
				ENDIF
				oStream:Dispose()
				RETURN TRUE
			ENDIF
			RETURN FALSE
		
		INTERNAL STATIC METHOD read(pFile AS Intptr, refC OUT STRING, iCount AS INT, lOem2Ansi AS LOGIC) AS INT
			LOCAL aBuffer := XSharp.IO.File.getBuffer(pFile, iCount) AS BYTE[]
			iCount := (INT) ReadBuff(pFile, aBuffer, iCount, lOem2Ansi)
			refC := Bytes2String(aBuffer, iCount)
			RETURN iCount
		
		INTERNAL STATIC METHOD readBuff(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS INT, lAnsi AS LOGIC) AS INT64
			LOCAL iResult := 0 AS INT64
			VAR oStream := XSharp.IO.File.findStream(pFile)
			IF oStream != NULL_OBJECT
				TRY
					iResult := oStream:Read(pBuffer,0,(INT) dwCount)
					IF !lAnsi
						Oem2AnsiA(pBuffer)
					ENDIF
				CATCH
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
			ENDIF
			RETURN iResult
		
		INTERNAL STATIC METHOD readLine(pFile AS IntPtr,iCount AS INT) AS STRING
			LOCAL cResult := "" AS STRING
			LOCAL nPos	AS INT64
			LOCAL oStream := XSharp.IO.File.findStream(pFile) AS FileStream
			IF iCount <= 0
				// According to the VO docs the default value for the buffer length = 256
				iCount := 256
			ENDIF
			IF oStream != NULL_OBJECT
				TRY
					LOCAL pBuffer := XSharp.IO.File.getBuffer(pFile, iCount) AS BYTE[]
					nPos    := oStream:Position
					iCount := oStream:Read(pBuffer,0,(INT) iCount)
					cResult := Bytes2Line(pBuffer, REF iCount)
					nPos += iCount	// advance CRLF
					oStream:Position := nPos
				CATCH
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
			ENDIF
			RETURN cResult
		
		INTERNAL STATIC METHOD seek(pFile AS IntPtr,lOffset AS INT64,dwOrigin AS DWORD) AS INT64
			LOCAL oStream AS FileStream
			LOCAL iResult AS INT64
			oStream := XSharp.IO.File.findStream(pFile)
			IF oStream != NULL_OBJECT
				iResult := -1
				TRY
					SWITCH dwOrigin
						CASE FS_END
							iResult := oStream:Seek(lOffSet, SeekOrigin.End)
						CASE FS_RELATIVE
							iResult := oStream:Seek(lOffSet, SeekOrigin.Current)
						CASE FS_SET
							iResult := oStream:Seek(lOffSet, SeekOrigin.Begin)
						OTHERWISE
							iResult := -1					
					END SWITCH
				CATCH
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
				RETURN iResult 
			ENDIF
			RETURN -1
		
		INTERNAL STATIC METHOD write( pFile AS IntPtr, c AS STRING, nLength AS INT ) AS INT
			LOCAL aBytes := String2Bytes(c) AS BYTE[]
			RETURN writeBuff(pFile, aBytes, nLength, FALSE)			
		
		INTERNAL STATIC METHOD writeBuff(pFile AS IntPtr,pBuffer AS BYTE[],iCount AS INT, lAnsi AS LOGIC) AS INT
			LOCAL oStream	AS FileStream
			LOCAL iWritten := 0 AS INT
			oStream := XSharp.IO.File.findStream(pFile)
			IF oStream != NULL_OBJECT
				TRY
					IF !lAnsi
						pBuffer := Ansi2Oem(pBuffer)
					ENDIF
					oStream:Write(pBuffer, 0, iCount)
					iWritten := iCount
				CATCH
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
				
			ENDIF
			
			RETURN iWritten
		
		INTERNAL STATIC METHOD writeLine(pFile AS IntPtr,c AS STRING, nLen AS INT) AS INT
			IF c:Length > nLen
				c := c:Substring(0, nLen)
			ENDIF
			RETURN Write(pFile, c + e"\r\n",nLen+2)
		
		
		INTERNAL STATIC METHOD lock(pFile AS IntPtr,iOffset AS INT64,iLength AS INT64, lLock AS LOGIC) AS LOGIC
			LOCAL oStream := findStream(pFile) AS FileStream
			LOCAL lResult := FALSE AS LOGIC
			IF oStream != NULL_OBJECT
				TRY
					IF (lLock)
						oStream:Lock(iOffset, iLength)
					ELSE
						oStream:UnLock(iOffset, iLength)
					ENDIF
					lResult := TRUE
				CATCH 
					// Catch and save error
					lResult := FALSE
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
			ENDIF	
			RETURN lResult   		
		
		INTERNAL STATIC METHOD flush(pFile AS IntPtr, lCommit AS LOGIC) AS LOGIC
			LOCAL oStream := XSharp.IO.File.findStream(pFile) AS FileStream
			LOCAL lOk := FALSE AS LOGIC
			IF oStream != NULL_OBJECT
				TRY
					IF lCommit
						oStream:Flush(TRUE)
					ELSE
						oStream:Flush()
					ENDIF
					lOk := TRUE
				CATCH 
					FError((DWORD)Marshal.GetLastWin32Error())
					lOk := FALSE 
				END TRY
			ENDIF	
			RETURN lOk
		INTERNAL STATIC METHOD ChSize(pFile AS IntPtr,nValue AS DWORD) AS LOGIC
			LOCAL oStream := XSharp.IO.File.findStream(pFile) AS FileStream
			LOCAL lOk := FALSE AS LOGIC
			IF oStream != NULL_OBJECT
				TRY
					oStream:SetLength(nValue)
					lOk := TRUE
				CATCH 
					FError((DWORD)Marshal.GetLastWin32Error())
					lOk := FALSE
				END TRY
			ENDIF	
			RETURN lOk
		
		INTERNAL STATIC METHOD Eof(pFile AS IntPtr) AS LOGIC
			LOCAL oStream := XSharp.IO.File.findStream(pFile) AS FileStream
			LOCAL lResult := TRUE AS LOGIC
			IF oStream != NULL_OBJECT
				lResult := oStream:Position == oStream:Length
			ENDIF	 
			RETURN lResult
		
		INTERNAL STATIC METHOD Tell(pFile AS IntPtr) AS INT64
			LOCAL oStream AS FileStream
			oStream := XSharp.IO.File.findStream(pFile)
			IF oStream != NULL_OBJECT
				TRY
					RETURN oStream:Position
				CATCH
					FError((DWORD)Marshal.GetLastWin32Error())
				END TRY
			ENDIF
			RETURN -1
	END CLASS
	
	
END NAMESPACE	


/// <summary>
/// Set the error code for a file operation.
/// </summary>
/// <param name="nSet"></param>
/// <returns>The previous errorcode from the last file operation.</returns>
FUNCTION FError(nErrorCode AS DWORD) AS DWORD
	LOCAL nOldError AS DWORD
	nOldError := XSharp.IO.File.errorCode
	XSharp.IO.File.errorCode := nErrorCode
	RETURN nOldError


/// <summary>
/// Get the error code for a file operation.
/// </summary>
/// <returns>The error from the last file operation or the last user-specified setting.  If there was no error, FError() returns 0.</returns>
FUNCTION FError() AS DWORD
	LOCAL nOldError AS DWORD
	nOldError := XSharp.IO.File.errorCode
	RETURN nOldError


/// <summary>
/// Change the size of a file opened with a low-level file function.
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="nSize">The new length to which the file should be set. </param>
/// <returns>The new length of the file if successful; otherwise, F_ERROR.  FError() can be used to determine the specific error.</returns>
FUNCTION FChSize(pFile AS IntPtr,nSize AS DWORD) AS LOGIC
	RETURN XSharp.IO.File.Chsize(pFile, nSize)

/// <summary>
/// Close an open file and write the buffers to disk.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <returns>FALSE if an error occurs while writing; otherwise, TRUE.</returns>
FUNCTION FClose(pFile AS IntPtr) AS LOGIC
	RETURN XSharp.IO.File.Close(pFile)

/// <summary>
/// Flush file buffers.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION FCommit(pFile AS IntPtr) AS LOGIC
	RETURN XSharp.IO.File.flush(pFile, TRUE)

/// <summary>
/// Determine if the file pointer is positioned at the end-of-file.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <returns>TRUE if the file pointer is at end-of-file; otherwise, FALSE.</returns>
FUNCTION FEof(pFile AS IntPtr) AS LOGIC
	RETURN XSharp.IO.File.eof(pFile) 


/// <summary>
/// Lock a portion of an open file.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <param name="dwOffset">The file offset at which to start locking.  A value of zero corresponds to the first byte of the file. </param>
/// <param name="dwLength">The number of bytes to lock. </param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION FFLock(pFile AS IntPtr,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
	RETURN XSharp.IO.File.lock(pFile, (INT64) dwOffset, (INT64) dwLength, TRUE)


/// <summary>
/// Flush to disk a file opened with a low-level file function.
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION FFlush(pFile AS IntPtr) AS LOGIC
	RETURN XSharp.IO.File.flush(pFile, FALSE)


/// <summary>
/// UnLock a portion of an open file.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <param name="dwOffset">The file offset at which to start unlocking.  A value of zero corresponds to the first byte of the file. </param>
/// <param name="dwLength">The number of bytes to unlock. </param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION FFUnLock(pFile AS IntPtr,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
	RETURN XSharp.IO.File.lock(pFile, (INT64) dwOffset, (INT64) dwLength, FALSE)


/// <inheritdoc cref="M:XSharp.Core.Functions.FReadLine(System.IntPtr,System.UInt32)" />"
FUNCTION FGetS(pFile AS IntPtr) AS STRING
	// According to the VO docs the dedault value for the buffer length = 256
	RETURN XSharp.IO.File.readLine(pFile,256)


/// <inheritdoc cref="M:XSharp.Core.Functions.FReadLine(System.IntPtr,System.UInt32)" />"
FUNCTION FGetS2(pFile AS IntPtr,nLineLen AS DWORD) AS STRING
	RETURN XSharp.IO.File.readLine(pFile, (INT) nLineLen)


/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" /> 
FUNCTION FPutS(pFile AS IntPtr,c AS STRING) AS DWORD
	RETURN FWriteLine(pFile, c, (DWORD) c:Length)

/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" /> 
FUNCTION FPutS(pFile AS IntPtr,c AS STRING,nCount AS DWORD) AS DWORD
	RETURN FWriteLine(pFile, c, nCount)

/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" /> 
FUNCTION FPutS3(pFile AS IntPtr,c AS STRING, nCount AS DWORD) AS DWORD
	RETURN FWriteLine(pFile, c, nCount)


/// <summary>
/// Read characters from a file into an allocated buffer.
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="pBuffer">An array of bytes to store the data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount">The number of bytes to read into the buffer.</param>
/// <returns>The number of bytes successfully read.  A return value less than the number of bytes requested indicates end-of-file or some other read error.  FError() can be used to determine the specific error.</returns>
FUNCTION FRead3(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.readBuff(pFile, pBuffer, (INT) dwCount, FALSE)

/// <summary>
/// Read characters from a file into an allocated buffer with optional OEM to Ansi conversion.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.FRead3(System.IntPtr,System.Byte[],System.UInt32)" />
/// <param name="lAnsi">If FALSE an OEM to ANSI conversion is made. </param>
FUNCTION FRead4(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
	RETURN (DWORD) XSharp.IO.File.readBuff(pFile, pBuffer, (INT) dwCount, lAnsi)


/// <summary>
/// Read a line from an open file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="pFile">The handle of the file to read from. </param>
/// <param name="nLineLen">The maximum number of characters to read per line.</param>
/// <returns>The line read.  When the end-of-file is reached while attempting to read, the function returns a NULL_STRING and FError() is set to 257.</returns>
/// <remarks>This function is assumed to handle raw binary data and are not dependent upon the status of SetAnsi().
/// FReadText() and FRead4(), on the other hand, are dependent upon SetAnsi().
/// </remarks>
FUNCTION FReadLine(pFile AS IntPtr,nLineLen AS DWORD) AS STRING
	RETURN XSharp.IO.File.readLine(pFile, (INT) nLineLen)


/// <inheritdoc cref="M:XSharp.Core.Functions.FreadLine(System.IntPtr,System.UInt32)" />"
FUNCTION FReadLine2(pFile AS IntPtr,nLineLen AS DWORD) AS STRING	
	RETURN XSharp.IO.File.readLine(pFile, (INT) nLineLen)



/// <inheritdoc cref="M:XSharp.Core.Functions.FreadLine(System.IntPtr, System.UInt32)" />
/// <summary>
/// Read characters from a file.
/// </summary>
/// <param name="dwCount">The number of bytes to read, beginning at the current DOS file pointer position.  Characters are read up to this number or until end-of-file is encountered.
/// The file pointer is then moved forward.  If the number is greater than the number of bytes from the pointer position to the end of the file, the file pointer is positioned at the last byte in the file.
/// </param>
FUNCTION FReadStr(pFile AS IntPtr,iCount AS DWORD) AS STRING
	LOCAL cResult AS STRING
	XSharp.IO.File.read(pFile, OUT cResult, (INT) iCount, FALSE)
	RETURN cResult

/// <inheritdoc cref="M:XSharp.Core.Functions.FRead3(System.IntPtr,System.Byte[],System.UInt32)" />
/// <summary>
/// Read characters from a file into a buffer variable that is passed by reference.
/// </summary>
/// <param name="strValue"></param>
/// <returns></returns>
FUNCTION FReadText(pFile AS IntPtr,strValue REF STRING,dwCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.read(pFile, OUT strValue, (INT) dwCount, XSharp.RuntimeState.Ansi)

/// <summary>
/// Read characters from a file into an allocated buffer, with possible OEM to ANSI conversion, based on the current SetAnsi() setting.
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <param name="dwCount"></param>
/// <returns><inheritdoc cref="M:XSharp.Core.Functions.FRead3(System.IntPtr,System.Byte[],System.UInt32)" /></returns>
FUNCTION FReadText3(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.readBuff(pFile, pBuffer, (INT) dwCount, XSharp.RuntimeState.Ansi)	

/// <summary>
/// Set the file pointer at the top of an open file.
/// </summary>
/// <param name="pFile"></param>
/// <returns>
/// </returns>
FUNCTION FRewind(pFile AS IntPtr) AS LOGIC
	RETURN XSharp.IO.File.Seek(pFile, 0, FS_SET) == 0


/// <summary>
/// Set the file pointer to a new position, specifying three strongly-typed arguments.
/// </summary>
/// <param name="pFile">The handle of the open file.</param>
/// <param name="lOffset">The number of bytes to move the file pointer, from the position defined in the next parameter.It can be a positive or negative number.
/// A positive number moves the pointer forward in the file, and a negative number moves the pointer backward.
/// If the origin is the end-of-file, then this must be 0 or negative.</param>
/// <param name="dwOrigin">One of the following constants indicating the starting location of the file pointer, telling where to start searching the file:</param>
/// <returns>The new position of the file pointer, relative to the beginning of the file (position 0).  (The original position of the file pointer does not matter.)</returns>
/// <remarks>
/// The possible values for the origin are:
/// <list type="table">
/// <listheader>
/// <term>Constant</term>           <description>Seeks from</description>
/// </listheader>
///	<item><term>FS_END</term>       <description>End-of-file   </description></item>  
/// <item><term>FS_RELATIVE</term>  <description>Current pointer position  </description></item>  
/// <item><term>FS_SET</term>       <description>Beginning-of-file </description></item>
/// </list>
/// </remarks>
FUNCTION FSeek3(pFile AS IntPtr,lOffset AS LONG, dwOrigin AS DWORD) AS LONG
	RETURN (LONG) XSharp.IO.File.Seek(pFile, (INT64) lOffset, dwOrigin)

/// <summary>
/// Return the current position of the file pointer.
/// </summary>
/// <param name="pFile">The handle of an open file.</param>
/// <returns>The current position of the file pointer, relative to the beginning of the file.</returns>
FUNCTION FTell(pFile AS IntPtr) AS DWORD
	RETURN (DWORD) XSharp.IO.File.Tell(pFile)


/// <inheritdoc cref="M:XSharp.Core.Functions.FTell(System.IntPtr)" />
FUNCTION FTell64(pFile AS IntPtr) AS INT64
	RETURN XSharp.IO.File.Tell(pFile)


/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="c"> The string to write. </param>
/// <returns>The number of bytes written.  If the value returned is equal to the length of the string, the operation was successful.
/// If the return value is less than this or 0, this means that the the disk is full, or another error has occurred.
/// FError() can be used to determine the specific error. </returns>
FUNCTION FWrite( pFile AS IntPtr, c AS STRING ) AS DWORD
	RETURN (DWORD) XSharp.IO.File.write( pFile, c,  c:Length )



/// <summary>
/// Write a string to an open file
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="c"> The string to write. </param>
/// <param name="nCount">The number of characters from the string to write, beginning at the current file pointer position.</param>
/// <returns>The number of bytes written.  If the value returned is equal to nCount, the operation was successful.
/// If the return value is less than the nCount or 0, this means that the length of the buffer/string is less than number of bytes,
/// or the disk is full, or another error has occurred.  FError() can be used to determine the specific error. </returns>
FUNCTION FWrite( pFile AS IntPtr, c AS STRING, nCount AS DWORD ) AS DWORD
	RETURN (DWORD) XSharp.IO.File.write(pFile, c, (INT) nCount)


/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite(System.IntPtr,System.String,System.UInt32)" />
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
FUNCTION FWrite3(pFile AS IntPtr,pBuffer AS BYTE[],nCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.writeBuff(pFile, pBuffer, (INT) nCount, TRUE)


/// <summary>
/// Write the contents of a buffer to an open file, with an ANSI to OEM conversion option.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite3(System.IntPtr,System.String,System.UInt32)" />
/// <param name="lAnsi">If FALSE , an ANSI to OEM conversion is made.</param>
/// <returns></returns>
FUNCTION FWrite4(pFile AS IntPtr,pBuffer AS BYTE[],nCount AS DWORD,lAnsi AS LOGIC) AS DWORD
	RETURN (DWORD) XSharp.IO.File.writeBuff(pFile,pBuffer , (INT) nCount ,lAnsi )


/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" />
/// <returns>The number of bytes written.  If the value returned is equal to the length of the string +2, the operation was successful.
/// If the return value is less 0, this means the disk is full, or another error has occurred.</returns>
FUNCTION FWriteLine(pFile AS IntPtr,c AS STRING) AS DWORD
	RETURN (DWORD) XSharp.IO.File.write(pFile, c + e"\r\n",c:Length+2)

/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying strongly-typed arguments.
/// </summary>
/// <param name="pFile">The handle of the file.</param>
/// <param name="c">The string to write.</param>
/// <param name="nCount">The number of bytes in the string to write, beginning at the current file pointer position.</param>
/// <returns>The number of bytes written.  If the value returned is equal to the nCount + 2, the operation was successful.
/// If the return value is less than nCount + 2 or 0, this means that the length of  the string was less than nCount,
/// or the disk is full, or another error has occurred.</returns>
FUNCTION FWriteLine(pFile AS IntPtr,c AS STRING,nCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.writeLine(pFile, c, (INT) nCount)

/// <inheritdoc cref="M:XSharp.Core.Functions.FWriteLine(System.IntPtr,System.String,System.UInt32)" /> 
FUNCTION FWriteLine3(pFile AS IntPtr,c AS STRING,nCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.writeLine(pFile, c, (INT) nCount )

/// <summary>
/// Write the contents of a buffer to an open file, with SetAnsi() dependency.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.FWrite(System.IntPtr,System.String,System.UInt32)" /> 
/// <param name="pBuffer">Pointer to an array of bytes to store data read from the specified file.  The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
FUNCTION FWriteText3(pFile AS IntPtr,pBuffer AS BYTE[],nCount AS DWORD) AS DWORD
	RETURN (DWORD) XSharp.IO.File.writeBuff(pFile ,pBuffer ,(INT) nCount, XSharp.RuntimeState.Ansi) 

/// <summary>
/// Remove spaces from a file name specified as a string.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION AdjustFName(cName AS STRING) AS STRING
	LOCAL adjusted := NULL AS STRING
	IF ( !string.IsNullOrEmpty(cName) ) 
		adjusted := System.IO.Path.GetFileNameWithoutExtension(cName):TrimEnd()
		IF ( cName:IndexOf('.') > 0 ) 
			adjusted += System.IO.Path.GetExtension(cName)
		ENDIF
	ENDIF
	RETURN adjusted   



/// <summary>
/// Remove spaces from a file name specified as a string, changing the contents of the original file name as well as the returned file name.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION AdjustFNameA(cName REF STRING) AS STRING
	cName := AdjustFName(cName)
	RETURN cName




/// <summary>
/// Create a file or open and truncate an existing file, specifying two strongly typed arguments.
/// </summary>
/// <param name="cFile">The name of the file to create, including an optional drive, directory, and extension.
/// SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and
/// directory as part of the file name.  No extension is assumed.  If the file already exists, its length is truncated to 0 without warning.</param>
/// <param name="kAttributes"> A number that indicates the attribute to be set when creating the file. See the remarks section for a list of possible values.</param>
/// <remarks>
/// The possible values for the file attributes are:
/// <list type="table">
/// <listheader>
/// <term>Constant</term> <description>Description</description>
/// </listheader>
///	<item><term>FC_ARCHIVED</term><description>Archived file </description></item>  
/// <item><term>FC_HIDDEN</term> <description>Hidden File</description></item>  
/// <item><term>FC_NORMAL</term> <description>Normal Read/write file</description></item>   
/// <item><term>FC_READONLY</term> <description>Readonly file</description></item>   
/// <item><term>FC_SYSTEM</term> <description>System file</description></item> 
/// </list>
/// </remarks> 
/// <returns>The file handle of the new file.  If an error occurs, the function returns  F_ERROR.  FError() can be used to determine the specific error.</returns>
FUNCTION FCreate2(cFile AS STRING,kAttributes AS DWORD) AS IntPtr
	LOCAL oFileMode AS VOFileMode
	oFileMode := VOFileMode{ FO_CREATE, kAttributes }
	RETURN XSharp.IO.File.CreateFile(cFile, oFileMode)


/// <summary>
/// Create a file or open and truncate an existing file
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.FCreate2(System.String,System.UInt32)" /> 
/// <remarks>This creates the file with a FC_NORMAL attribute </remarks>
FUNCTION FCreate(cFile AS STRING ) AS IntPtr
	RETURN FCreate2(cFile, FC_NORMAL)

/// <inheritdoc cref="M:XSharp.Core.Functions.FCreate2(System.String,System.UInt32)" />
FUNCTION FCreate(cFile AS STRING ,kAttributes AS DWORD) AS IntPtr
	RETURN FCreate2(cFile, kAttributes)



/// <summary>
/// Open a file.
/// </summary>
/// <inheritdoc cref="M:XSharp.Core.Functions.FOpen(System.String,System.UInt32)" />
/// <remarks>This opens the file with FO_READ and FO_EXCLUSIVE mode </remarks> 
FUNCTION FOpen(cFile AS STRING) AS IntPtr
	RETURN FOpen2(cFile, FC_NORMAL)

/// <summary>
/// Open a file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="cFile">The file name, including an optional drive, directory, and extension.
/// SetDefault() and SetPath() settings are ignored; the Windows default is used unless you specify a drive and directory
/// as part of the file name.  No extension is assumed</param>
/// <param name="dwMode">The open mode, which determines the accessibility of the file.  The open mode is composed of elements from the two types of modes:
/// Access mode + Sharing mode.  Specifying an access mode constant indicates how the opened file is to be accessed. The sharing mode determines how other
/// processes can access the file. See the table in the remarks section for possible values.</param>
/// <remarks>
/// The possible values for the file open modes are:
/// <list type="table">
/// <listheader>
/// <term>Access Modes</term>           <description>Operation</description>
/// </listheader>
///	<item><term>FO_READ</term>          <description>Open for reading (default)   </description></item>  
/// <item><term>FO_READWRITE</term>     <description>Open for reading and/or writing </description></item>  
/// <item><term>FO_WRITE</term>         <description>Open for writing </description></item>
/// <item><term> </term>                <description> </description></item>
/// </list>
/// <list type="table">
/// <listheader>
/// <term>Sharing Modes</term>          <description>Operation</description>
/// </listheader>  
/// <item><term>FO_COMPAT</term>        <description>Compatibility mode (default) </description></item>   
/// <item><term>FO_DENYNONE</term>      <description>Allow others to read or write </description></item>
/// <item><term>FO_DENYREAD</term>      <description>Prevent others from reading </description></item>
/// <item><term>FO_DENYWRITE</term>     <description>Prevent others from writing </description></item>
/// <item><term>FO_EXCLUSIVE</term>     <description>Exclusive use </description></item>
/// <item><term>FO_SHARED</term>        <description>Same as FO_DENYNONE </description></item> 
/// </list>
/// </remarks> 
/// <returns>The file handle of the new file.  If an error occurs, the function returns F_ERROR.  FError() can be used to determine the specific error.</returns>
FUNCTION FOpen(cFile AS STRING,dwMode AS DWORD) AS IntPtr
	RETURN FOpen2(cFile, dwMode)

/// <inheritdoc cref="M:XSharp.Core.Functions.FOpen(System.String,System.UInt32)" /> 
FUNCTION FOpen2(cFile AS STRING,dwMode AS DWORD) AS IntPtr
	LOCAL oFileMode AS VOFileMode
	oFileMode := VOFileMode{dwMode, 0}
	RETURN XSharp.IO.File.CreateFile(cFile, oFileMode)



/// <summary>
/// Convert file attributes to numbers.
/// </summary>
/// <param name="uxFileAttr"></param>
/// <returns>
/// </returns>
FUNCTION GetFAttr(cAttributes AS STRING) AS DWORD
	RETURN String2FAttr(cAttributes)
	
	/// <summary>
	/// Convert file attributes to numbers.
	/// </summary>
	/// <param name="dwAttributes"></param>
	/// <returns>
	/// Does not do anything. Returns the original value
	/// </returns>
FUNCTION GetFAttr(dwAttributes AS DWORD) AS DWORD
	RETURN dwAttributes
	
	
	/// <summary>
	/// Prepare a file specification for wildcard searching.
	/// </summary>
	/// <param name="cFileMask"></param>
	/// <returns>
	/// When the source string ends with ":" or "\" then *.* is added
	/// </returns>
FUNCTION GetFMask(cFileMask AS STRING) AS STRING
	LOCAL cResult AS STRING
	IF String.IsNullOrEmpty(cFileMask)
		cResult := "*.*"
	ELSE
		VAR cChar := cFilemask[cFileMask:Length-1]
		SWITCH cChar
		CASE ':'
		CASE '\\'
			cResult := cFileMask + "*.*"
		OTHERWISE
			cResult := cFileMask
		END SWITCH
	ENDIF
	RETURN cResult
	
/// <param name="cAttr"> One or more of the following constants or strings: ADHSRV</param>
/// <returns>
/// </returns>
FUNCTION String2FAttr(cAttr AS STRING) AS DWORD
	LOCAL dwAttributes := FC_NORMAL AS DWORD
	IF !String.IsNullOrEmpty(cAttr)
		FOREACH c AS CHAR IN cAttr
			SWITCH Char.ToUpper(c)
				CASE 'A'
					dwAttributes |= FC_ARCHIVED
				CASE 'C'
					dwAttributes |= FA_COMPRESSED
				CASE 'D'
					dwAttributes |= FA_DIRECTORY
				CASE 'H'
					dwAttributes |= FC_HIDDEN
				CASE 'R'
					dwAttributes |= FC_READONLY
				CASE 'S'
					dwAttributes |= FC_SYSTEM
				CASE 'V'
					dwAttributes |= FA_VOLUME
			END SWITCH	
		NEXT
	ENDIF
	RETURN dwAttributes



/// <summary>
/// Display file attributes as a string.
/// </summary>
/// <param name="nAttrib"></param>
/// <returns>
/// </returns>
FUNCTION FAttr2String(dwAttributes AS DWORD) AS STRING
	LOCAL cAttribute := "" AS STRING
	
	IF (DWORD)_AND(dwAttributes,FA_DIRECTORY) == FA_DIRECTORY
		cAttribute := cAttribute + "D"
	ENDIF
	IF (DWORD)_AND(dwAttributes,FA_VOLUME) == FA_VOLUME
		cAttribute := cAttribute + "V"
	ENDIF
	IF (DWORD)_AND(dwAttributes,FC_ARCHIVED) == FC_ARCHIVED
		cAttribute := cAttribute + "A"
	ENDIF
	IF (DWORD)_AND(dwAttributes,FC_HIDDEN) == FC_HIDDEN
		cAttribute := cAttribute + "H"
	ENDIF
	IF (DWORD)_AND(dwAttributes,FC_READONLY) == FC_READONLY
		cAttribute := cAttribute + "R"
	ENDIF
	IF (DWORD)_AND(dwAttributes,FC_SYSTEM) == FC_SYSTEM
		cAttribute := cAttribute + "S"
	ENDIF
	
	RETURN cAttribute



/// <exclude/>	
FUNCTION Bytes2String(aBytes AS BYTE[], nBuffLen AS INT) AS STRING
	LOCAL aChars := CHAR[]{nBuffLen} AS CHAR[]
	LOCAL encoding := StringHelpers.WinEncoding AS Encoding
	encoding:GetChars(aBytes, 0, nBuffLen, aChars, 0)
	RETURN STRING{aChars, 0, nBuffLen}

/// <exclude/>	
FUNCTION String2Bytes(sSource AS STRING) AS BYTE[]
	LOCAL ret AS BYTE[]
	IF sSource != NULL
		LOCAL encoding := StringHelpers.WinEncoding AS Encoding  
		ret := encoding:GetBytes( sSource ) 
	ELSE
		ret := BYTE[]{0}
	ENDIF   
	RETURN ret



INTERNAL FUNCTION Bytes2Line(aBytes AS BYTE[], nBuffLen REF INT) AS STRING
	// determine end of line
	// note that VO looks for CR LF. CR alone or LF alone to not count as end of line
	LOCAL nLF AS INT
	LOCAL lFoundCR := FALSE AS LOGIC
	LOCAL lFoundCRLF := FALSE AS LOGIC
	nLF := nBuffLen
	FOR VAR nI := 0 TO nBuffLen-1
		SWITCH aBytes[nI]
			CASE 10
				IF lFoundCR		// immediately LF after CR
					nLF := nI -1
					lFoundCRLF := TRUE
				ENDIF
			CASE 13
				lFoundCR := TRUE
			OTHERWISE
				lFoundCR := FALSE
		END SWITCH
		IF lFoundCRLF
			EXIT
		ENDIF
	NEXT
	// when we have found a CRLF then nLF is the length of the string before the CR
	// otherwise the length of the source string
	LOCAL aChars := CHAR[]{nLF} AS CHAR[]
	LOCAL encoding := StringHelpers.WinEncoding AS Encoding
	encoding:GetChars(aBytes, 0, nLF, aChars, 0)
	IF lFoundCRLF
		nBuffLen := nLF+2
	ENDIF
	RETURN STRING{aChars, 0, nLF}
