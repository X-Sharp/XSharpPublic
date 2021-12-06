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
    /// <summary>This class is used for Shared diskaccess on Windows. </summary>
    /// <remarks>The class bypasses some of the default methods in the FileStraem class and directly uses calls to OS functions to make
    /// sure that changes made by other users are visible and not hidden because of caching in the .Net FileStream class.</remarks>
    CLASS XsWin32FileStream INHERIT XsFileStream
        PRIVATE hFile AS IntPtr
        PRIVATE smallBuff AS BYTE[]
        INTERNAL CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) 
            SUPER(path, mode, faccess, share, bufferSize, options)
            hFile := SELF:SafeFileHandle:DangerousGetHandle()
            smallBuff := BYTE[]{1}
        RETURN
        /// <inheritdoc />
        /// <remarks>This method calls the Windows SetFilePointerEx() function directly.</remarks>
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
            
        /// <inheritdoc />
        /// <remarks>This method calls the Windows SetEndOfFile() function directly.</remarks>
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            // warning: does not restore original file pos
            SELF:Seek(length, SeekOrigin.Begin)
            SetEndOfFile(hFile)
        RETURN
        /// <inheritdoc />
        /// <remarks>This method calls the Windows GetFileSize() function directly.</remarks>
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
        /// <inheritdoc />
        /// <remarks>This method calls the Windows ReadFile() function directly.</remarks>
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
        /// <inheritdoc />
        /// <remarks>This method calls the Windows WriteFile() function directly.</remarks>
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
        /// <inheritdoc />
        /// <remarks>This method calls the Windows WriteFile() function directly.</remarks>
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            SELF:smallBuff[0] := b
            SELF:Write(SELF:smallBuff , 0 , 1)
        /// <inheritdoc />
        /// <remarks>This method calls the Windows LockFile() function directly.</remarks>
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
        /// <inheritdoc />
        /// <remarks>This method calls the Windows UnlockFile() function directly.</remarks>
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
        /// <inheritdoc />
        /// <remarks>This method calls the Windows FlushFileBuffers() function directly.</remarks>
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
    
    
END NAMESPACE
