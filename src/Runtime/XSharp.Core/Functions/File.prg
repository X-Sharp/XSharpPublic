//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.IO
USING System.Linq
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING System.Security
USING System.Runtime
USING System.Runtime.ConstrainedExecution
USING System.Text

#region Defines
DEFINE F_ERROR :=  IntPtr{-1} // Error value (all functions)

// FERROR() returns, which are not reflected as DOSERROR()
DEFINE FERROR_FULL    := 256   // disk full
DEFINE FERROR_EOF     := 257   // eof was already reached, when a read was tried
DEFINE FERROR_PARAM   := 258   // invalid parameter already detected before giving to DOS

// FSEEK(), _llseek() modes
/// <summary>FSeek() Seek Offset: Seek from beginning of file</summary>
DEFINE FS_SET         := 0
/// <summary>FSeek() Seek Offset: Seek from current file position</summary>
DEFINE FS_RELATIVE    := 1
/// <summary>FSeek() Seek Offset: Seek from end of file</summary>
DEFINE FS_END         := 2

// FOPEN() access modes
/// <summary>FOpen() Open Mode: Open for reading (default)</summary>
DEFINE FO_READ        := 0
/// <summary>FOpen() Open Mode: Open for writing</summary>
DEFINE FO_WRITE       := 1
/// <summary>FOpen() Open Mode: Open for reading or writing</summary>
DEFINE FO_READWRITE   := 2
///  <summary>FOpen() Open Mode: UnBuffered. Added to FO_READ, FO_WRITE and FO_READWRITE</summary>
DEFINE FO_UNBUFFERED  := 8

// FOPEN() sharing modes (combine with open mode using +)
/// <summary>FOpen() Sharing Mode: Compatibility mode (default)</summary>
DEFINE FO_COMPAT     := 0x00000000
/// <summary>FOpen() Sharing Mode: Exclusive</summary>
DEFINE FO_EXCLUSIVE  := 0x00000010
/// <summary>FOpen() Sharing Mode: Prevent other processes from writing</summary>
DEFINE FO_DENYWRITE  := 0x00000020
/// <summary>FOpen() Sharing Mode: Prevent other processes from reading</summary>
DEFINE FO_DENYREAD   := 0x00000030
/// <summary>FOpen() Sharing Mode: (same as FO_SHARED)</summary>
DEFINE FO_DENYNONE   := 0x00000040
/// <summary>FOpen() Sharing Mode:  Allow other processes to read or write</summary>
DEFINE FO_SHARED     := 0x00000040  //

DEFINE OF_SHARE_COMPAT		:= 0x00000000
DEFINE OF_SHARE_EXCLUSIVE	:= 0x00000010
DEFINE OF_SHARE_DENY_WRITE	:= 0x00000020
DEFINE OF_SHARE_DENY_READ	:= 0x00000030
DEFINE OF_SHARE_DENY_NONE	:= 0x00000040
//DEFINE OF_PARSE				:= 0x00000100

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
/// <summary>FCreate() attribute mode: normal read/write file (default for create)</summary>
DEFINE FC_NORMAL     := 0x00000000
/// <summary>FCreate() attribute mode: read-only file</summary>
DEFINE FC_READONLY   := 0x00000001
/// <summary>FCreate() attribute mode: hidden file</summary>
DEFINE FC_HIDDEN     := 0x00000002
/// <summary>FCreate() attribute mode: system file</summary>
DEFINE FC_SYSTEM     := 0x00000004
/// <summary>FCreate() attribute mode: archived</summary>
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
        PROPERTY UnBuffered     AS LOGIC AUTO

        CONSTRUCTOR(dwMode AS DWORD, dwAttribs AS DWORD)
            // Wildcard
            IF _AND(dwMode, FXO_WILD) == (DWORD) FXO_WILD
                    SELF:lWild := TRUE
                dwMode := dwMode - FXO_WILD
            ELSE
                SELF:lWild := FALSE
            ENDIF
            SELF:Attributes := dwAttribs
            VAR nMode := _AND(dwMode , 0x0F)    // Get lobyte
            IF nMode >= FO_UNBUFFERED // check for Unbuffered / Buffered
                SELF:UnBuffered := TRUE
            ENDIF
            // Access

            IF _AND(dwMode,FO_READWRITE) == (DWORD) FO_READWRITE
                SELF:FileAccess	:= FileAccess.ReadWrite
            ELSEIF _AND(dwMode,FO_WRITE) == (DWORD) FO_WRITE
                SELF:FileAccess	:= FileAccess.Write
            ELSE
                SELF:FileAccess	:= FileAccess.Read
            ENDIF
            // Create
            SELF:FileMode := FileMode.Open
            SELF:FileShare  := FileShare.ReadWrite
            IF _AND(dwMode,FO_CREATE) == (DWORD) FO_CREATE
                SELF:FileMode	:= FileMode.Create
                SELF:FileAccess	:= FileAccess.ReadWrite
                SELF:FileShare   := FileShare.None
                RETURN
            ENDIF

            LOCAL dwTempMode AS DWORD
            dwTempMode := _OR(OF_SHARE_DENY_WRITE, OF_SHARE_DENY_READ, OF_SHARE_DENY_NONE)
            dwTempMode := _AND(dwMode,dwTempMode)

            SWITCH dwTempMode
                CASE FO_DENYNONE
                CASE FO_COMPAT
                    SELF:FileShare  := FileShare.ReadWrite

                CASE FO_DENYWRITE
                    SELF:FileShare  := FileShare.Read

                CASE FO_DENYREAD
                    SELF:FileShare  := FileShare.Write

                CASE FO_EXCLUSIVE
                    SELF:FileShare  := FileShare.None

            END SWITCH
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
    PRIVATE STATIC streams AS ConcurrentDictionary<IntPtr, FileCacheElement >
    PRIVATE STATIC random AS Random

        /// <exclude />
        STATIC CONSTRUCTOR
            streams := ConcurrentDictionary<IntPtr, FileCacheElement >{}
        random := Random{}

        STATIC INTERNAL METHOD findStream(pStream AS IntPtr) AS FileStream
            LOCAL element := NULL AS FileCacheElement
            BEGIN LOCK streams
                IF streams:TryGetValue(pStream, OUT element)
                    RETURN element:Stream
                ENDIF
            END LOCK
        RETURN NULL

        STATIC PRIVATE METHOD hasStream(pStream AS IntPtr) AS LOGIC
            return streams:ContainsKey(pStream)

        STATIC PRIVATE METHOD addStream(pStream AS IntPtr, oStream AS FileStream, attributes AS DWORD) AS LOGIC
            BEGIN LOCK streams
                IF ! streams:ContainsKey(pStream)
                    RETURN streams:TryAdd(pStream, FileCacheElement {oStream, attributes})
                ENDIF
            END LOCK
        RETURN FALSE

        STATIC INTERNAL METHOD setStream(pStream AS IntPtr, oStream AS FileStream) AS LOGIC
            LOCAL element := NULL AS FileCacheElement
            BEGIN LOCK streams
                IF streams:TryGetValue(pStream, OUT element)
                    element:Stream := oStream
                    RETURN TRUE
                ENDIF
            END LOCK
        RETURN FALSE

        STATIC PRIVATE METHOD removeStream(pStream AS IntPtr) AS LOGIC
            return streams:TryRemove(pStream, out null)

        STATIC PRIVATE METHOD createManagedFileStream(cFile AS STRING, oMode AS VOFileMode) AS FileStream
            LOCAL oStream := NULL AS FileStream
            TRY
                ClearErrorState()
                IF oMode:UnBuffered
                    oStream := FileStream{cFile, oMode:FileMode, oMode:FileAccess, oMode:FileShare, 1,FileOptions.RandomAccess}
                ELSE
                    oStream := XsFileStream.CreateFileStream(cFile, oMode:FileMode, oMode:FileAccess, oMode:FileShare, 16*1024,FileOptions.RandomAccess)
                ENDIF

            CATCH e AS Exception
                System.Diagnostics.Trace.WriteLine(e:Message)
                SetErrorState(e)
            END TRY
        RETURN oStream

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
            STATIC METHOD ClearErrorState() AS VOID
            RuntimeState.FileException := NULL
            RuntimeState.FileError     := 0
            RuntimeState.NetErr := FALSE
        RETURN

        STATIC METHOD SetErrorState ( o AS Exception ) AS VOID
            LOCAL e AS Error
            e := Error{o}
            e:SetStackTrace(o:StackTrace+Environment.NewLine+System.Diagnostics.StackTrace{1,TRUE}:ToString())
            RuntimeState.FileException := e
            RuntimeState.FileError := _AND ( (DWORD) System.Runtime.InteropServices.Marshal.GetHRForException ( o ) , 0x0000FFFF )
            e:OSCode := RuntimeState.FileError
            IF RuntimeState.FileError == 32 // SHARING VIOLATION
                RuntimeState.NetErr := TRUE
            ENDIF
        RETURN

        STATIC INTERNAL METHOD CreateFile(cFile AS STRING, oMode AS VOFileMode) AS IntPtr
            LOCAL hFile := F_ERROR AS IntPtr
            LOCAL oStream AS FileStream
            IF String.IsNullOrEmpty(cFile)
                BadFileParam(__FUNCTION__, nameof(cFile), 1)
                RETURN hFile // F_ERROR
            ENDIF
            IF System.IO.File.Exists(cFile) .AND. oMode:FileMode == FileMode.Create
                VAR fi := FileInfo{cFile}
                IF ! fi:IsReadOnly
                    fi:Attributes := FileAttributes.Normal
                ENDIF
            ENDIF
            oStream := createManagedFileStream(cFile, oMode)
            IF oStream != NULL
                    BEGIN LOCK streams
                        DO WHILE TRUE
                            hFile := (IntPtr) random:Next(1, Int32.MaxValue)
                            DO WHILE streams:ContainsKey(hFile)
                                hFile := (IntPtr) random:Next(1, Int32.MaxValue)
                            ENDDO
                            IF addStream(hFile, oStream, oMode:Attributes)
                                EXIT
                            ENDIF
                        ENDDO
                END LOCK
            ELSE
                hFile := F_ERROR
            ENDIF
        RETURN hFile

        STATIC INTERNAL METHOD GetBuffer(pStream AS IntPtr, nSize AS INT) AS BYTE[]
            BEGIN LOCK streams
                IF hasStream(pStream)
                    LOCAL element := streams[pStream] AS FileCacheElement
                    IF element:Bytes == NULL .OR. element:Bytes:Length < nSize
                        element:Bytes := BYTE[]{nSize}
                    ENDIF
                    RETURN element:Bytes
                ENDIF
            END LOCK
        RETURN NULL

        STATIC INTERNAL METHOD Close(pStream AS IntPtr) AS LOGIC
            BEGIN LOCK streams
                IF hasStream(pStream)
                    LOCAL oStream      := streams[pStream]:Stream AS Stream
                    LOCAL dwAttributes := streams[pStream]:Attributes AS DWORD
                    removeStream(pStream)
                    TRY
                        ClearErrorState()
                        IF oStream:CanWrite
                            oStream:Flush()
                        ENDIF
                        oStream:Close()
                        IF dwAttributes != 0 .AND. oStream IS FileStream VAR oFileStream
                            VAR fi := FileInfo{oFileStream:Name}
                            fi:Attributes := (FileAttributes) dwAttributes
                        ENDIF
                        oStream:Dispose()
                        RETURN TRUE
                    CATCH e AS Exception
                        SetErrorState(e)
                    END TRY
                ENDIF
            END LOCK
        RETURN FALSE

        INTERNAL STATIC METHOD Read(pFile AS IntPtr, refC OUT STRING, iCount AS INT, lOem2Ansi AS LOGIC) AS INT
            LOCAL aBuffer := XSharp.IO.File.GetBuffer(pFile, iCount) AS BYTE[]
            iCount := (INT) ReadBuff(pFile, aBuffer, iCount, lOem2Ansi)
            refC := Bytes2String(aBuffer, iCount)
        RETURN iCount

        INTERNAL STATIC METHOD ReadBuff(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS INT) AS INT64
            LOCAL iCount := 0 AS INT64
            VAR oStream := XSharp.IO.File.findStream(pFile)
            IF oStream IS FileStream
                    oStream:SafeRead(pBuffer, dwCount, OUT VAR iRead)
                iCount := iRead
            ELSEIF oStream != NULL_OBJECT
                TRY
                    ClearErrorState()
                    iCount := oStream:Read(pBuffer,0,(INT) dwCount)
                    IF iCount == 0
                        RuntimeState.FileError := FERROR_EOF
                    ENDIF
                CATCH e AS Exception
                    SetErrorState(e)
                END TRY
            ENDIF
        RETURN iCount

        INTERNAL STATIC METHOD ReadBuff(pFile AS IntPtr,pBuffer AS BYTE[],dwCount AS INT, lAnsi AS LOGIC) AS INT64
            LOCAL iResult := 0 AS INT64
            iResult := ReadBuff(pFile, pBuffer, dwCount)
            IF FError() == 0 .AND. !lAnsi
                Oem2AnsiA(pBuffer)
            ENDIF
        RETURN iResult

        INTERNAL STATIC METHOD ReadLine(pFile AS IntPtr,iCount AS INT) AS STRING
            LOCAL cResult := "" AS STRING
            LOCAL nPos	AS INT64
            LOCAL oStream := XSharp.IO.File.findStream(pFile) AS Stream
            IF iCount <= 0
                // According to the VO docs the default value for the buffer length = 256
                iCount := 256
            ENDIF
            IF oStream != NULL_OBJECT
                TRY
                    LOCAL pBuffer := XSharp.IO.File.GetBuffer(pFile, iCount) AS BYTE[]
                    ClearErrorState()
                    nPos    := oStream:Position
                    iCount := oStream:Read(pBuffer,0,(INT) iCount)
                    IF iCount == 0
                        RuntimeState.FileError := FERROR_EOF
                    ENDIF
                    cResult := Bytes2Line(pBuffer, REF iCount)
                    nPos += iCount	// advance CRLF
                    oStream:Position := nPos
                CATCH e AS Exception
                    SetErrorState(e)
                END TRY
            ENDIF
        RETURN cResult

        INTERNAL STATIC METHOD Seek(pFile AS IntPtr,lOffset AS INT64,dwOrigin AS DWORD) AS INT64
            LOCAL oStream AS Stream
            LOCAL iResult := -1 AS INT64
            oStream := XSharp.IO.File.findStream(pFile)
            IF oStream IS XsFileStream
                iResult := oStream:Seek(lOffset, (SeekOrigin) dwOrigin)
            ELSEIF oStream != NULL_OBJECT
                iResult := -1

                TRY
                    ClearErrorState()
                    SWITCH dwOrigin
                        CASE FS_END
                        CASE FS_RELATIVE
                        CASE FS_SET
                            iResult := oStream:Seek(lOffset, (SeekOrigin) dwOrigin)
                        OTHERWISE
                            iResult := -1
                    END SWITCH
                CATCH e AS Exception
                    SetErrorState(e)
                END TRY
                RETURN iResult
            ENDIF
        RETURN iResult

        INTERNAL STATIC METHOD Write( pFile AS IntPtr, c AS STRING, nLength AS INT, lAnsi AS LOGIC ) AS INT
            LOCAL aBytes AS BYTE[]
            IF nLength == c:Length
                aBytes  := String2Bytes(c)
            ELSEIF nLength < c:Length
                aBytes  := String2Bytes(c:Substring(0, nLength))
            ELSE // nLength >
                aBytes  := String2Bytes(c:PadRight(nLength,' '))
            ENDIF
            RETURN WriteBuff(pFile, aBytes, aBytes:Length, lAnsi)

        INTERNAL STATIC METHOD WriteBuff(pFile AS IntPtr,pBuffer AS BYTE[],iCount AS INT) AS INT
            LOCAL oStream	AS FileStream
            LOCAL iWritten := 0 AS INT
            oStream := XSharp.IO.File.findStream(pFile)
            IF oStream IS XsFileStream
                oStream:SafeWrite(pBuffer, iCount)
                iWritten := iCount
            ELSEIF oStream != NULL_OBJECT
                TRY
                    ClearErrorState()
                    oStream:Write(pBuffer, 0, iCount)
                CATCH e AS Exception
                    SetErrorState(e)
                END TRY

            ENDIF

        RETURN iWritten

        INTERNAL STATIC METHOD WriteBuff(pFile AS IntPtr,pBuffer AS BYTE[],iCount AS INT, lAnsi AS LOGIC) AS INT
            IF !lAnsi
                pBuffer := Ansi2Oem(pBuffer)
            ENDIF
        RETURN WriteBuff(pFile, pBuffer, iCount)

        INTERNAL STATIC METHOD WriteLine(pFile AS IntPtr,c AS STRING, nLen AS INT) AS INT
            IF c:Length > nLen
                c := c:Substring(0, nLen)
            ENDIF
            c += RuntimeState.Eol
        RETURN Write(pFile, c , c:Length, TRUE)


        INTERNAL STATIC METHOD Lock(pFile AS IntPtr,iOffset AS INT64,iLength AS INT64, lLock AS LOGIC) AS LOGIC
            LOCAL oStream := findStream(pFile) AS Stream
            LOCAL lResult := FALSE AS LOGIC
            IF oStream != NULL_OBJECT
                TRY
                    ClearErrorState()
                    IF oStream IS FileStream VAR oFileStream
                        IF (lLock)
                            oFileStream:Lock(iOffset, iLength)
                        ELSE
                            oFileStream:Unlock(iOffset, iLength)
                        ENDIF
                    ENDIF
                    lResult := TRUE
                CATCH e AS Exception
                    // Catch and save error
                    SetErrorState(e)
                    lResult := FALSE
                END TRY
            ENDIF
        RETURN lResult

        INTERNAL STATIC METHOD Flush(pFile AS IntPtr, lCommit AS LOGIC) AS LOGIC
            LOCAL oStream := XSharp.IO.File.findStream(pFile) AS Stream
            LOCAL lOk := FALSE AS LOGIC
            IF oStream != NULL_OBJECT
                TRY
                    IF oStream:CanWrite
                        ClearErrorState()
                        IF lCommit .AND. oStream IS FileStream VAR oFileStream
                            oFileStream:Flush(TRUE)
                        ELSE
                            oStream:Flush()
                        ENDIF
                    ENDIF
                    lOk := TRUE
                CATCH e AS Exception
                    // Catch and save error
                    SetErrorState(e)
                    lOk := FALSE
                END TRY
            ENDIF
        RETURN lOk
        INTERNAL STATIC METHOD ChSize(pFile AS IntPtr,nValue AS DWORD) AS LOGIC
            LOCAL oStream := XSharp.IO.File.findStream(pFile) AS Stream
            LOCAL lOk := FALSE AS LOGIC
            IF oStream != NULL_OBJECT
                TRY
                    ClearErrorState()
                    oStream:SetLength(nValue)
                    lOk := TRUE
                CATCH e AS Exception
                    // Catch and save error
                    SetErrorState(e)
                    lOk := FALSE
                END TRY
            ENDIF
        RETURN lOk

        INTERNAL STATIC METHOD Eof(pFile AS IntPtr) AS LOGIC
            LOCAL oStream := XSharp.IO.File.findStream(pFile) AS Stream
            LOCAL lResult := TRUE AS LOGIC
            IF oStream != NULL_OBJECT
                lResult := oStream:Position == oStream:Length
            ENDIF
        RETURN lResult

        INTERNAL STATIC METHOD Tell(pFile AS IntPtr) AS INT64
            LOCAL oStream AS Stream
            oStream := XSharp.IO.File.findStream(pFile)
            IF oStream != NULL_OBJECT
                TRY
                    ClearErrorState()
                    RETURN oStream:Position
                CATCH e AS Exception
                    // Catch and save error
                    SetErrorState(e)
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
    nOldError := RuntimeState.FileError
    RuntimeState.FileError := nErrorCode
    RETURN nOldError


/// <summary>
/// Get the error code for a file operation.
/// </summary>
/// <returns>The error from the last file operation or the last user-specified setting.  If there was no error, FError() returns 0.</returns>
FUNCTION FError() AS DWORD
    LOCAL nOldError AS DWORD
    nOldError := RuntimeState.FileError
    RETURN nOldError


/// <summary>
/// Get the last exception for a file operation.
/// </summary>
/// <returns>The exception from the last file operation or the last user-specified setting.  If there was no exception, FException() returns null.</returns>
FUNCTION FException() AS Exception
    RETURN RuntimeState.FileException


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fchsize/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FChSize(ptrHandle AS IntPtr,dwOffset AS DWORD) AS LOGIC
    RETURN XSharp.IO.File.ChSize(ptrHandle, dwOffset)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fclose/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FClose(ptrHandle AS IntPtr) AS LOGIC
    RETURN XSharp.IO.File.Close(ptrHandle)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcommit/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FCommit(ptrHandle AS IntPtr) AS LOGIC
    RETURN XSharp.IO.File.Flush(ptrHandle, TRUE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/feof/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FEof(ptrHandle AS IntPtr) AS LOGIC
    RETURN XSharp.IO.File.Eof(ptrHandle)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fflock/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FFLock(ptrHandle AS IntPtr,offset AS DWORD,length AS DWORD) AS LOGIC
    VAR lResult := XSharp.IO.File.Lock(ptrHandle, (INT64) offset, (INT64) length, TRUE)
    //    IF ! lResult
    //        ? "DWLock", dwOffset, dwLength, lResult
    //    ENDIF
    RETURN lResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fflock/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FFLock64(ptrHandle AS IntPtr,offset AS INT64, length AS INT64) AS LOGIC
    VAR lResult := XSharp.IO.File.Lock(ptrHandle, offset, length, TRUE)
    //    IF ! lResult
    //    ? "I64Lock", iOffset, iLength, lResult
    //    ENDIF
    //? ProcName(1), ProcName(2), ProcName(3)
    RETURN lResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fflush/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FFlush(ptrHandle AS IntPtr) AS LOGIC
    RETURN XSharp.IO.File.Flush(ptrHandle, FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fflush/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FFlush(ptrHandle AS IntPtr, lCommit AS LOGIC) AS LOGIC
    RETURN XSharp.IO.File.Flush(ptrHandle, lCommit)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ffunlock/*" />
/// <inheritdoc cref="FFLock" />"
FUNCTION FFUnLock(ptrHandle AS IntPtr,offset AS DWORD,length AS DWORD) AS LOGIC
    VAR lResult := XSharp.IO.File.Lock(ptrHandle, (INT64) offset, (INT64) length, FALSE)
    //? "DWUnLock", dwOffset, dwLength, lResult
    RETURN lResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ffunlock/*" />
/// <inheritdoc cref="FFLock" />"
FUNCTION FFUnLock64(ptrHandle AS IntPtr,offset AS INT64,length AS INT64) AS LOGIC
    VAR lResult := XSharp.IO.File.Lock(ptrHandle,  offset,  length, FALSE)
    //? "I64UnLock", iOffset, iLength, lResult
    //? ProcName(1), ProcName(2), ProcName(3)
    RETURN lResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fgets/*" />
FUNCTION FGetS(ptrHandle AS IntPtr) AS STRING
    // According to the VO docs the dedault value for the buffer length = 256
    RETURN XSharp.IO.File.ReadLine(ptrHandle,256)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fgets/*" />
FUNCTION FGetS(ptrHandle AS IntPtr,nMax AS DWORD) AS STRING
    RETURN XSharp.IO.File.ReadLine(ptrHandle, (INT) nMax)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fgets2/*" />
FUNCTION FGetS2(ptrHandle AS IntPtr,dwMax AS DWORD) AS STRING
    RETURN XSharp.IO.File.ReadLine(ptrHandle, (INT) dwMax)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fputs/*" />
FUNCTION FPutS(ptrHandle AS IntPtr,cBuffer AS STRING) AS DWORD
    RETURN FWriteLine(ptrHandle, cBuffer, (DWORD) cBuffer:Length)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fputs/*" />
FUNCTION FPutS(ptrHandle AS IntPtr,cBuffer AS STRING,nBytes AS DWORD) AS DWORD
    RETURN FWriteLine(ptrHandle, cBuffer, nBytes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fputs3/*" />
FUNCTION FPutS3(ptrHandle AS IntPtr,cBuffer AS STRING, dwBytes AS DWORD) AS DWORD
    RETURN FWriteLine(ptrHandle, cBuffer, dwBytes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread3/*" />
/// <param name="ptrBufferVar">An array of bytes to store the data read from the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FRead3(ptrHandle AS IntPtr,ptrBufferVar AS BYTE[],dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.ReadBuff(ptrHandle, ptrBufferVar, (INT) dwBytes)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread4/*" />
/// <param name="ptrBufferVar">An array of bytes to store the data read from the specified file. The length of this variable must be greater than or equal to the number of bytes in the next parameter.</param>
/// <remarks><include file="CoreComments.xml" path="Comments/Oem2AnsiFileIO/*" /></remarks>
FUNCTION FRead4(ptrHandle AS IntPtr,ptrBufferVar AS BYTE[],dwBytes AS DWORD,lAnsi AS LOGIC) AS DWORD
    RETURN (DWORD) XSharp.IO.File.ReadBuff(ptrHandle, ptrBufferVar, (INT) dwBytes, lAnsi)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadline/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FReadLine(ptrHandle AS IntPtr,nMax AS DWORD) AS STRING
    RETURN XSharp.IO.File.ReadLine(ptrHandle, (INT) nMax)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadline2/*" />
FUNCTION FReadLine2(ptrHandle AS IntPtr,dwMax AS DWORD) AS STRING
    RETURN XSharp.IO.File.ReadLine(ptrHandle, (INT) dwMax)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadstr/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FReadStr(ptrHandle AS IntPtr,dwBytes AS DWORD) AS STRING
    LOCAL cResult AS STRING
    XSharp.IO.File.Read(ptrHandle, OUT cResult, (INT) dwBytes, XSharp.RuntimeState.Ansi)
    RETURN cResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadtext/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FReadText(ptrHandle AS IntPtr,cBufferVar REF STRING,dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.Read(ptrHandle, OUT cBufferVar, (INT) dwBytes, XSharp.RuntimeState.Ansi)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fread/*" />
FUNCTION FRead(ptrHandle AS IntPtr,cBufferVar REF STRING,dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.Read(ptrHandle, OUT cBufferVar, (INT) dwBytes, TRUE)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/freadtext3/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FReadText3(ptrHandle AS IntPtr,ptrBufferVar AS BYTE[],dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.ReadBuff(ptrHandle, ptrBufferVar, (INT) dwBytes, XSharp.RuntimeState.Ansi)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/frewind/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FRewind(ptrHandle AS IntPtr) AS LOGIC
    RETURN XSharp.IO.File.Seek(ptrHandle, 0, FS_SET) == 0

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fseek3/*" />
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
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FSeek3(ptrHandle AS IntPtr,liOffset AS LONG, dwOrigin AS DWORD) AS LONG
    RETURN (LONG) XSharp.IO.File.Seek(ptrHandle, (INT64) liOffset, dwOrigin)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ftell/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FTell(ptrHandle AS IntPtr) AS DWORD
    RETURN (DWORD) XSharp.IO.File.Tell(ptrHandle)

/// <inheritdoc cref="FTell" />
FUNCTION FTell64(pFile AS IntPtr) AS INT64
    RETURN XSharp.IO.File.Tell(pFile)


/// <inheritdoc cref="FWrite" />
FUNCTION FWrite( pFile AS IntPtr, c AS STRING ) AS DWORD
    RETURN (DWORD) XSharp.IO.File.Write( pFile, c,  c:Length, TRUE )

/// <exclude />
FUNCTION FSetErrorState(e AS Exception) AS VOID
    XSharp.IO.File.SetErrorState(e)
    RETURN

/// <exclude />
FUNCTION FClearErrorState() AS VOID
    XSharp.IO.File.ClearErrorState()
    RETURN


/// <overloads>
/// <summary>
/// Write a string to an open file
/// </summary>
/// <include file="CoreComments.xml" path="Comments/File/*" />
/// </overloads>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwrite/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FWrite( ptrHandle AS IntPtr, cBuffer AS STRING, nBytes AS DWORD ) AS DWORD
    RETURN FWrite( ptrHandle, cBuffer, nBytes, TRUE)

/// <inheritdoc cref="FWrite" />
/// <param name="lAnsi">If FALSE an OEM to ANSI conversion is made. </param>
FUNCTION FWrite( ptrHandle AS IntPtr, cBuffer AS STRING, nBytes AS DWORD, lAnsi AS LOGIC) AS DWORD
    RETURN (DWORD) XSharp.IO.File.Write(ptrHandle, cBuffer, (INT) nBytes, lAnsi)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwrite3/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Oem2AnsiFileIO/*" /></remarks>
FUNCTION FWrite3(ptrHandle AS IntPtr,ptrBuffer AS BYTE[],dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.WriteBuff(ptrHandle, ptrBuffer, (INT) dwBytes)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwrite4/*" />
/// <remarks><include file="CoreComments.xml" path="Comments/Oem2AnsiFileIO/*" /></remarks>
FUNCTION FWrite4(ptrHandle AS IntPtr,ptrBuffer AS BYTE[],dwBytes AS DWORD,lAnsi AS LOGIC) AS DWORD
    RETURN (DWORD) XSharp.IO.File.WriteBuff(ptrHandle,ptrBuffer , (INT) dwBytes ,lAnsi )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwriteline/*" />
FUNCTION FWriteLine(ptrHandle AS IntPtr,cBuffer AS STRING) AS DWORD
    cBuffer += RuntimeState.Eol
    RETURN (DWORD) XSharp.IO.File.Write(ptrHandle, cBuffer ,cBuffer:Length, TRUE)

/// <overloads>
/// <summary>
/// Write a string, a carriage-return character, and a linefeed character to an open file.
/// </summary>
/// <include file="CoreComments.xml" path="Comments/File/*" />
/// </overloads>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwriteline/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FWriteLine(ptrHandle AS IntPtr,cBuffer AS STRING,nBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.WriteLine(ptrHandle, cBuffer, (INT) nBytes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwriteline3/*" />
FUNCTION FWriteLine3(ptrHandle AS IntPtr,cBuffer AS STRING,dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.WriteLine(ptrHandle, cBuffer, (INT) dwBytes )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fwritetext3/*" />
FUNCTION FWriteText3(ptrHandle AS IntPtr,ptrBuffer AS BYTE[],dwBytes AS DWORD) AS DWORD
    RETURN (DWORD) XSharp.IO.File.WriteBuff(ptrHandle ,ptrBuffer ,(INT) dwBytes, XSharp.RuntimeState.Ansi)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adjustfname/*" />
FUNCTION AdjustFName(cFileName AS STRING) AS STRING
    LOCAL adjusted := NULL AS STRING
    IF ( !String.IsNullOrEmpty(cFileName) )
        adjusted := System.IO.Path.GetFileNameWithoutExtension(cFileName):TrimEnd()
        IF ( cFileName:IndexOf('.') > 0 )
            adjusted += System.IO.Path.GetExtension(cFileName)
        ENDIF
    ENDIF
    RETURN adjusted



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adjustfnamea/*" />
FUNCTION AdjustFNameA(cFileName REF STRING) AS STRING
    cFileName := AdjustFName(cFileName)
    RETURN cFileName



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcreate2/*" />
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FCreate2(cFileName AS STRING,dwAttributes AS DWORD) AS IntPtr
    LOCAL oFileMode AS VOFileMode
    oFileMode := VOFileMode{ FO_CREATE, dwAttributes }
    RETURN XSharp.IO.File.CreateFile(cFileName, oFileMode)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcreate/*" />
FUNCTION FCreate(cFileName AS STRING ) AS IntPtr
    RETURN FCreate2(cFileName, _OR(FC_NORMAL, FO_EXCLUSIVE))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcreate/*" />
FUNCTION FCreate(cFileName AS STRING ,kAttributes AS DWORD) AS IntPtr
    RETURN FCreate2(cFileName, kAttributes)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fopen/*" />
FUNCTION FOpen(cFileName AS STRING) AS IntPtr
    RETURN FOpen2(cFileName, FC_NORMAL)



/// <overloads>
/// <summary>Open a file.</summary>
/// <include file="CoreComments.xml" path="Comments/File/*" />
/// </overloads>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fopen/*" />
/// <include file="CoreComments.xml" path="Comments/FileHandle/*" /><br/>
/// <include file="CoreComments.xml" path="Comments/File/*" />
FUNCTION FOpen(cFileName AS STRING,kMode AS DWORD) AS IntPtr
    RETURN FOpen2(cFileName, kMode)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fopen2/*" />
FUNCTION FOpen2(cFileName AS STRING,kMode AS DWORD) AS IntPtr
    LOCAL oFileMode AS VOFileMode
    oFileMode := VOFileMode{kMode, 0}
    RETURN XSharp.IO.File.CreateFile(cFileName, oFileMode)

FUNCTION FConvertToMemoryStream(pFile AS IntPtr) AS IntPtr
    RETURN pFile

FUNCTION FConvertToFileStream(pFile AS IntPtr) AS IntPtr
    RETURN pFile

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getfattr/*" />
FUNCTION GetFAttr(uAttributes AS STRING) AS DWORD
    RETURN String2FAttr(uAttributes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getfattr/*" />
FUNCTION GetFAttr(uAttributes AS DWORD) AS DWORD
    RETURN uAttributes


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getfmask/*" />
FUNCTION GetFMask(cString AS STRING) AS STRING
    LOCAL cResult AS STRING
    IF String.IsNullOrEmpty(cString)
        cResult := "*.*"
    ELSE
        VAR cChar := cString[cString:Length-1]
        SWITCH cChar
            CASE ':'
            CASE '\\'
                cResult := cString + "*.*"
            OTHERWISE
                cResult := cString
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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fattr2string/*" />
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

/// <summary>Access or allocate the File I/O Buffer used by the X# Runtime for Low Level File Access for a file.</summary>
/// <param name="pFile"><include file="CoreComments.xml" path="Comments/FileHandle/*" /></param>
/// <param name="nSize">The size of the buffer that you would like to allocate.s</param>
/// <returns>The Byte[] associated with the file handle for an open file.
/// When the file handle is invalid then a NULL object will be returned.</returns>
/// <remarks>This function will either return the existing buffer or allocate a new buffer of the requested size. <br/>
/// When there is already a buffer allocated for the file handle, and this buffer is large enough then the existing buffer is returned.
/// When the size requested exceeds the size of the allocated buffer, or when no buffer exists, then a new byte array will be allocated.
/// </remarks>
/// <seealso cref="FClose" />

FUNCTION FGetBuffer(hFile AS IntPtr, nSize AS INT) AS BYTE[]
    RETURN XSharp.IO.File.GetBuffer(hFile, nSize)

/// <summary>Access the FileStream object used by the X# Runtime for Low Level File Access </summary>
/// <param name="pFile"><include file="CoreComments.xml" path="Comments/FileHandle/*" /></param>
/// <returns>The FileStream object or NULL when the handle is not valid </returns>
/// <remarks><note type="warning">You are not supposed to close the stream object that you retrieve with this function.
/// The Lifetime management of the stream should be left to the X# Runtime <br/>
/// If you want to close the stream, please use the FClose() function </note>
/// </remarks>
/// <seealso cref="FClose" />
FUNCTION FGetStream(pFile AS IntPtr) AS FileStream
    RETURN XSharp.IO.File.findStream(pFile)


/// <summary>Return the filename for a file handle</summary>
/// <param name="pFile"><include file="CoreComments.xml" path="Comments/FileHandle/*" /></param>
FUNCTION FGetFileName(pFile AS IntPtr) AS STRING
    LOCAL oStream AS FileStream
    oStream := XSharp.IO.File.findStream(pFile)
    IF oStream != NULL_OBJECT
        RETURN oStream:Name
    ENDIF
    RETURN String.Empty


/// <summary>Returns the size in bytes of a specified file. </summary>
/// <param name="pFile"><include file="CoreComments.xml" path="Comments/FileHandle/*" /></param>
/// <returns>The size of the file or -1 when the file handle is not valid.</returns>
/// <seealso cref="FClose" />
FUNCTION FSize(pFile AS IntPtr) AS INT64
    VAR oStream := XSharp.IO.File.findStream(pFile)
    IF oStream != NULL
        RETURN oStream:Length
    ENDIF
    RETURN -1

/// <summary>Returns the size in bytes of a specified file. </summary>
/// <param name="cFileName">Specifies a file for which FSIZE( ) returns the size in bytes.</param>
/// <returns>The size of the file or -1 when the file is not found.</returns>
/// <seealso cref="File" />
FUNCTION FSize(cFileName AS STRING) AS INT64
    IF File(cFileName)
        cFileName := FPathName()
        VAR fileInfo := FileInfo{cFileName}
        RETURN fileInfo:Length
    ENDIF
    RETURN 0

