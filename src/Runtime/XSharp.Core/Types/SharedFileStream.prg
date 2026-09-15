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

BEGIN NAMESPACE XSharp.IO
    /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream/*" />
#ifdef NET6_0_OR_GREATER
    // .Net 6 and later. FileStream reads and writes at an explicit offset and does not cache the length
    // of a file that others may write to, so it reports the truth for shared access by itself. The hand
    // written Win32 layer below is not only unnecessary there, it is broken: it never overrode Position,
    // and the .Net 6 rewrite removed the re-verification of the OS position that used to paper over that.
    /// <remarks>
    /// This class used to do all of its IO with the Win32 API (ReadFile, WriteFile, SetFilePointerEx,
    /// LockFile) on the raw file handle of the base class, because the FileStream of that time cached the
    /// file position and the file length, which is wrong for a file that other processes may change.
    /// <br/>
    /// It does not do that anymore. The .Net FileStream reads and writes at an explicit offset and does not
    /// cache the length of a file that others may write to, so it reports the truth for shared access by
    /// itself, on every platform. What is left here is the one thing that is special about a shared stream:
    /// it must not buffer, and Flush() must commit to disk.
    /// <br/>
    /// The name is kept because this class is public and is handed to user code through DBI_FILESTREAM.
    /// </remarks>
    CLASS XsWin32FileStream INHERIT XsFileStream
        INTERNAL CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            // bufferSize 1 means unbuffered. A buffer would hand out bytes that another process has already
            // changed, and would hold back bytes that another process is waiting for, so the bufferSize of
            // the caller is deliberately ignored: for shared access there is no good buffer size but none.
            SUPER(path, mode, faccess, share, 1, options)
        RETURN

        /// <inheritdoc />
        /// <remarks>A shared file stream commits to disk, so that other processes see the change.</remarks>
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(TRUE)
            RETURN

        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64) AS VOID
            TRY
                SUPER:Lock(position, length)
            CATCH e AS Exception
                SELF:__SetLockError()
                THROW e
            END TRY
            RETURN

        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Unlock(position AS INT64, length AS INT64) AS VOID
            TRY
                SUPER:Unlock(position, length)
            CATCH e AS Exception
                SELF:__SetLockError()
                THROW e
            END TRY
            RETURN

        // A failed lock on a shared file is what NetErr() reports in the xBase world. The base class has
        // already recorded the exception through SetErrorState(), but that only sets NetErr for a sharing
        // violation (32) and a lock violation is 33. The Win32 implementation used below .Net 6 sets NetErr
        // itself, so it has to happen here as well - and only here, so that exclusive streams keep behaving
        // exactly like they always did.
        PRIVATE METHOD __SetLockError() AS VOID
            IF RuntimeState.FileError == 0
                FError(33) // DOS lock violation
            ENDIF
            NetErr(TRUE)
        RETURN

    END CLASS
#else
    // Before .Net 6, unchanged. FileStream of that time cached the file position and the file length,
    // which is wrong for a file that other processes may change, so the IO is done with the Win32 API.
    // Position is not overridden here on purpose: until .Net 5 FileStream re-verified the OS position on
    // every access once the SafeFileHandle had been exposed, which keeps this correct.
    CLASS XsWin32FileStream INHERIT XsFileStream
        PRIVATE hFile AS IntPtr
        PRIVATE smallBuff AS BYTE[]
        INTERNAL CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) 
            SUPER(path, mode, faccess, share, bufferSize, options)
            hFile := SELF:SafeFileHandle:DangerousGetHandle()
            smallBuff := BYTE[]{1}
        RETURN
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Seek/*" />
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            LOCAL result AS INT64
            LOCAL lOk AS LOGIC
            lOk := SetFilePointerEx(hFile, offset, OUT result, origin)
            IF lOk
                RETURN result
            ENDIF
            VAR nErr := (DWORD) Marshal.GetLastWin32Error()
            if (nErr == 0)
                nErr := 30 // Dos error Read Fault
            ENDIF
            FError(nErr) 
            THROW IOException{i"Error moving file pointer from {origin} to {offset}"}
            
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.SetLength/*" />
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            // warning: does not restore original file pos
            SELF:Seek(length, SeekOrigin.Begin)
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
            IF offset == 0
                ret := ReadFile(SELF:hFile, bytes, count, OUT bytesRead, IntPtr.Zero)
            ELSE
                LOCAL data AS BYTE[]
                data := BYTE[]{count}
                ret := ReadFile(SELF:hFile, data, count, OUT bytesRead, IntPtr.Zero)
                System.Array.Copy(data, 0, bytes, offset, count)
            ENDIF
            IF !ret
                RETURN -1
            ENDIF
        RETURN bytesRead
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream.Write/*" />
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            LOCAL ret := FALSE AS LOGIC
            LOCAL bytesWritten := 0 AS INT
            IF offset == 0
                ret := WriteFile(SELF:hFile, bytes, count, OUT bytesWritten, 0)
            ELSE
                LOCAL aCopy AS BYTE[]
                aCopy := BYTE[]{count}
                System.Array.Copy(bytes,offset, aCopy,0, count)
                ret := WriteFile(SELF:hFile, aCopy, count, OUT bytesWritten, 0)
            ENDIF
            IF !ret
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 29 // Dos error Write Fault
                ENDIF
                FError(nErr) 
                THROW IOException{i"Write: File write failed offset {offset} count {count}"}
            ENDIF
            IF bytesWritten != count
                VAR nErr := (DWORD) Marshal.GetLastWin32Error()
                if (nErr == 0)
                    nErr := 29 // Dos error Write Fault
                ENDIF
                FError(nErr) 
                THROW IOException{i"Write: Not all bytes written to file offset {offset} count {count} written {bytesWritten}"}
            ENDIF
        RETURN
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
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "ReadFile")];
        PRIVATE STATIC EXTERN METHOD ReadFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbytesread OUT INT , mustbezero AS IntPtr) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "WriteFile")];
        PRIVATE STATIC EXTERN METHOD WriteFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbyteswritten OUT INT , lpOverlapped AS INT) AS LOGIC
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
#endif

END NAMESPACE
