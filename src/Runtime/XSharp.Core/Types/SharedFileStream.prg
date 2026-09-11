//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices
USING System.Collections.Generic
USING System.Threading
USING Microsoft.Win32.SafeHandles

BEGIN NAMESPACE XSharp.IO
    /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream/*" />
    CLASS XsWin32FileStream INHERIT XsFileStream
        PRIVATE CONST ERROR_HANDLE_EOF := 38 AS DWORD

        PRIVATE hFile AS IntPtr
        PRIVATE smallBuff AS BYTE[]
        // The current position of this stream. We do not ask the OS for it, and we do not let the OS file
        // pointer decide where we read and write: every ReadFile() / WriteFile() below passes this offset
        // explicitly in an OVERLAPPED, the way the .Net FileStream does it itself. That makes this field the
        // single source of truth, so nothing that touches the file handle behind our back can desync us, and
        // it saves a syscall per access: no SetFilePointerEx() before the read, and none for FTell(),
        // FEof() or the SafeReadAt() / SafeSetPos() pattern that the RDDs use on every record.
        PRIVATE nPos AS INT64
        INTERNAL CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            // All IO in this class is done with the Win32 API on the OS file pointer. The base class must
            // therefore not buffer: its buffer is filled from the position that the base class maintains
            // itself, which is not the position that we read from and write to.
            SUPER(path, mode, faccess, share, 1, options)
            // SUPER:SafeFileHandle and not SELF:SafeFileHandle, because our override below needs hFile.
            hFile := SUPER:SafeFileHandle:DangerousGetHandle()
            smallBuff := BYTE[]{1}
            // Read the start position from the OS once. It is not always 0: FileMode.Append opens the file
            // positioned at the end.
            IF ! SetFilePointerEx(hFile, 0, OUT nPos, SeekOrigin.Current)
                nPos := 0
            ENDIF
        RETURN

        /// <inheritdoc />
        PUBLIC OVERRIDE PROPERTY SafeFileHandle AS SafeFileHandle
            GET
                // When the handle is exposed, the base class moves the OS file pointer to the position that
                // IT maintains, and that is not our position. Move it back, otherwise our position and the
                // OS file pointer drift apart and every following read uses the wrong offset.
                VAR oHandle := SUPER:SafeFileHandle
                IF SELF:hFile != IntPtr.Zero
                    LOCAL nNew AS INT64
                    IF SetFilePointerEx(SELF:hFile, SELF:nPos, OUT nNew, SeekOrigin.Begin)
                        SELF:nPos := nNew
                    ENDIF
                ENDIF
                RETURN oHandle
            END GET
        END PROPERTY
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Seek/*" />
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            // Pure arithmetic: reads and writes carry their own offset, so seeking never needs a syscall.
            // Only SeekOrigin.End has to ask for the file size. Seeking past the end of the file is legal,
            // exactly like SetFilePointerEx() allowed.
            LOCAL result AS INT64
            SWITCH origin
            CASE SeekOrigin.Begin
                result := offset
            CASE SeekOrigin.Current
                result := SELF:nPos + offset
            OTHERWISE
                result := SELF:Length + offset
            END SWITCH
            IF result >= 0
                SELF:nPos := result
                RETURN result
            ENDIF
            FError(131) // ERROR_NEGATIVE_SEEK, what SetFilePointerEx() used to report here
            THROW IOException{i"Error moving file pointer from {origin} to {offset}"}

        /// <inheritdoc />
        PUBLIC OVERRIDE PROPERTY Position AS INT64
            // Not the position that the base class caches: that one is only updated by the base class' own
            // IO and therefore stays at 0 forever. Until .Net 5 FileStream resynced it from the OS whenever
            // the SafeFileHandle had been exposed. The .Net 6 FileStream rewrite removed that, which made
            // every FTell(), FEof() and SafeReadAt() / SafeWriteAt() on a shared file use the wrong offset.
            GET
                RETURN SELF:nPos
            END GET
            SET
                // The RDDs constantly save the position, read somewhere else and restore it afterwards.
                // When the position does not really change there is nothing to do.
                IF value != SELF:nPos
                    SELF:Seek(value, SeekOrigin.Begin)
                ENDIF
            END SET
        END PROPERTY

        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.SetLength/*" />
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            // warning: does not restore original file pos
            // SetEndOfFile() truncates at the OS file pointer, so this is the one operation that still has
            // to move it. Our reads and writes do not care where it ends up.
            LOCAL nNew AS INT64
            IF ! SetFilePointerEx(SELF:hFile, length, OUT nNew, SeekOrigin.Begin)
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 30 // Dos error Read Fault
                ENDIF
                FError(nErr)
                THROW IOException{i"Error moving file pointer from {SeekOrigin.Begin} to {length}"}
            ENDIF
            SELF:nPos := nNew
            SetEndOfFile(hFile)
        RETURN
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Length/*" />
        PUBLIC OVERRIDE PROPERTY Length AS INT64
            GET
                  IF GetFileSizeEx(SELF:hFile, OUT VAR size)
                      RETURN size
                  ENDIF
                    VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                    if (nErr == 0)
                        nErr := 30 // Dos error Read Fault
                    ENDIF
                    FError(nErr) 
                  THROW IOException{"Could not retrieve file length"}  
            END GET
        END PROPERTY
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Read/*" />
        PUBLIC OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            LOCAL ret := FALSE AS LOGIC
            LOCAL bytesRead := 0 AS INT
            LOCAL ov := SELF:__Overlapped() AS NativeOverlapped
            IF offset == 0
                ret := ReadFile(SELF:hFile, bytes, count, OUT bytesRead, REF ov)
            ELSE
                LOCAL data AS BYTE[]
                data := BYTE[]{count}
                ret := ReadFile(SELF:hFile, data, count, OUT bytesRead, REF ov)
                IF ret .and. bytesRead > 0
                    // Copy the bytes that were really read. Copying count bytes would overwrite bytes in
                    // the target buffer beyond the end of the file with the zeroes from our temp buffer,
                    // and would throw when bytes is only large enough for offset + bytesRead.
                    System.Array.Copy(data, 0, bytes, offset, bytesRead)
                ENDIF
            ENDIF
            IF !ret
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                IF nErr == ERROR_HANDLE_EOF
                    // Reading at or past the end of the file. With an explicit offset Windows reports that
                    // as a failure, where a read from the file pointer simply returned 0 bytes. It is not
                    // an error: the caller decides that 0 bytes means EOF, and FError() stays untouched.
                    RETURN 0
                ENDIF
                if (nErr == 0)
                    nErr := 30 // Dos error Read Fault
                ENDIF
                FError(nErr)
                // Stream:Read() must return a value between 0 and count. Returning -1 breaks every
                // generic Stream consumer. The failure is reported through FError() instead.
                RETURN 0
            ENDIF
            SELF:nPos += bytesRead
        RETURN bytesRead

        // The offset that the next read or write has to use, in the form the Win32 API wants it.
        PRIVATE METHOD __Overlapped() AS NativeOverlapped
            LOCAL ov AS NativeOverlapped
            ov := NativeOverlapped{}
            ov:OffsetLow  := (INT) SELF:nPos
            ov:OffsetHigh := (INT) (SELF:nPos >> 32)
        RETURN ov

        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD ReadByte() AS INT
            // FileStream overrides ReadByte() on .Net Core and reads at its own position, bypassing our
            // Read() override, so it has to be overridden here as well.
            IF SELF:Read(SELF:smallBuff, 0, 1) != 1
                RETURN -1
            ENDIF
        RETURN SELF:smallBuff[0]
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Write/*" />
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            LOCAL ret := FALSE AS LOGIC
            LOCAL bytesWritten := 0 AS INT
            LOCAL ov := SELF:__Overlapped() AS NativeOverlapped
            IF offset == 0
                ret := WriteFile(SELF:hFile, bytes, count, OUT bytesWritten, REF ov)
            ELSE
                LOCAL aCopy AS BYTE[]
                aCopy := BYTE[]{count}
                System.Array.Copy(bytes,offset, aCopy,0, count)
                ret := WriteFile(SELF:hFile, aCopy, count, OUT bytesWritten, REF ov)
            ENDIF
            IF !ret
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 29 // Dos error Write Fault
                ENDIF
                FError(nErr)
                THROW IOException{i"Write: File write failed offset {offset} count {count}"}
            ENDIF
            SELF:nPos += bytesWritten
            IF bytesWritten != count
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 29 // Dos error Write Fault
                ENDIF
                FError(nErr) 
                THROW IOException{i"Write: Not all bytes written to file offset {offset} count {count} written {bytesWritten}"}
            ENDIF
        RETURN
#ifdef NET5_0_OR_GREATER
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Read(buffer AS System.Span<BYTE>) AS INT
            // FileStream overrides the Span overloads on .Net Core. Without these overrides they would
            // read and write at the position that the base class maintains instead of the OS file pointer.
            LOCAL bytes := BYTE[]{buffer:Length} AS BYTE[]
            LOCAL bytesRead := SELF:Read(bytes, 0, buffer:Length) AS INT
            IF bytesRead > 0
                System.MemoryExtensions.AsSpan(bytes, 0, bytesRead):CopyTo(buffer)
            ENDIF
        RETURN bytesRead

        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Write(buffer AS System.ReadOnlySpan<BYTE>) AS VOID
            LOCAL bytes := buffer:ToArray() AS BYTE[]
            SELF:Write(bytes, 0, bytes:Length)
        RETURN
#endif

        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.WriteByte/*" />
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            SELF:smallBuff[0] := b
            SELF:Write(SELF:smallBuff , 0 , 1)
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Lock/*" />
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
            
            LOCAL ret  := FALSE AS LOGIC
            ret := LockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF !ret
                NetErr(TRUE)
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 33 // DOS Lock violation 
                ENDIF
                FError(nErr)  
                THROW IOException{i"Lock: File lock failed, pos: {position}, length: {length} "} 
            ENDIF
        RETURN 
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Unlock/*" />
        PUBLIC OVERRIDE METHOD Unlock( position AS INT64, length AS INT64)  AS VOID
            LOCAL ret := FALSE AS LOGIC
            ret := UnlockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF !ret
                NetErr(TRUE)
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 33 // DOS Lock violation 
                ENDIF
                FError(nErr)  
                THROW IOException{i"UnLock: File Unlock failed, pos: {position}, length: {length} "}
            ENDIF
        RETURN
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Flush/*" />
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            // Note that GetDangerousFileHandle() calls Flush before we have the file handle
            IF lCommit .and. SELF:hFile != NULL
                IF ! FlushFileBuffers(SELF:hFile)
                     XSharp.IO.File.SetErrorState(IOException{i"Flush: Error Flushing File Buffer "})
                ENDIF
            ENDIF
        RETURN
        
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            // Note that GetDangerousFileHandle() calls Flush before we have the file handle
            IF SELF:hFile == NULL
                SUPER:Flush()
                RETURN
            ENDIF
            // Shared FileStream should default to Committing the changes
            SELF:Flush(TRUE)
            RETURN
            
        #region External methods
        /// <exclude />
        // The handle is opened synchronously (no FILE_FLAG_OVERLAPPED), so these calls do not return before
        // the transfer is done. The OVERLAPPED is only there to carry the offset to read or write at.
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "ReadFile")];
        PRIVATE STATIC EXTERN METHOD ReadFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbytesread OUT INT , lpOverlapped REF NativeOverlapped) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "WriteFile")];
        PRIVATE STATIC EXTERN METHOD WriteFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbyteswritten OUT INT , lpOverlapped REF NativeOverlapped) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "SetFilePointerEx")];
        PRIVATE STATIC EXTERN METHOD SetFilePointerEx(handle AS IntPtr, distance AS INT64 , newAddress OUT INT64, origin AS SeekOrigin ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "LockFile")];
        PRIVATE STATIC EXTERN METHOD LockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "UnlockFile")];
        PRIVATE STATIC EXTERN METHOD UnlockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "FlushFileBuffers")];
        PRIVATE STATIC EXTERN METHOD FlushFileBuffers(hFile AS IntPtr ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "SetEndOfFile")];
        PRIVATE STATIC EXTERN METHOD SetEndOfFile(hFile AS IntPtr ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "GetFileSize")];
        PRIVATE STATIC EXTERN METHOD GetFileSize(hFile AS IntPtr , highSize OUT INT) AS DWORD
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "GetFileSizeEx")];
        PRIVATE STATIC EXTERN METHOD GetFileSizeEx(hFile AS IntPtr , FileSize OUT INT64) AS LOGIC

#endregion
        
    END CLASS
    
    
END NAMESPACE
