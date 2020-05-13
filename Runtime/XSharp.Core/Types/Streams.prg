USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.IO

    INTERNAL CLASS XsMemoryStream INHERIT MemoryStream
        INTERNAL FileStream AS Stream
        PRIVATE written AS LOGIC
        CONSTRUCTOR (oFileStream AS Stream)
            SUPER((INT) oFileStream:Length)
            SELF:FileStream := oFileStream
            VAR nPos := SELF:FileStream:Position
            SELF:FileStream:Flush()
            SELF:FileStream:Position := 0
            SELF:FileStream:CopyTo(SELF)
            SELF:FileStream:Position := nPos
            SELF:Position := nPos
            written := FALSE
            RETURN

        INTERNAL METHOD Save() AS VOID
            IF SELF:written
                VAR nPos := SELF:Position
                SELF:Flush()
                SELF:FileStream:SetLength(SELF:Length)
                SELF:FileStream:Position := 0
                SELF:Position := 0
                SELF:CopyTo(SELF:FileStream)
                SELF:FileStream:Position := nPos
                SELF:Position := nPos
                SELF:written := FALSE
            ENDIF
            RETURN
            
        OVERRIDE METHOD Write(buffer AS BYTE[], offset AS INT, count AS INT) AS VOID
            SUPER:Write(buffer, offset, count)
            written := TRUE
            
            
        OVERRIDE METHOD WriteByte(aByte AS BYTE) AS VOID
            SUPER:WriteByte(aByte)
            written := TRUE
            
        OVERRIDE METHOD SetLength(len AS INT64) AS VOID
            SUPER:SetLength(len)
            written := TRUE

        OVERRIDE METHOD Close() AS VOID
            SELF:Save()
            SELF:FileStream:Close()
            SUPER:Close()
    END CLASS

    CLASS XsFileStream INHERIT FileStream
        PRIVATE hFile AS IntPtr
        PRIVATE smallBuff AS BYTE[]
        PRIVATE CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) 
            SUPER(path, mode, faccess, share, bufferSize, options)
            hFile := SELF:SafeFileHandle:DangerousGetHandle()
            smallBuff := BYTE[]{1}
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            LOCAL result AS INT64
            LOCAL lOk AS LOGIC
            lOk := SetFilePointerEx(hFile, offset, OUT result, origin)
            IF lOk
                RETURN result
            ENDIF
            THROW System.IO.IOException{"Error moving file pointer"}
            /// <inheritdoc />
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
			// warning: does not restore original file pos
            SELF:Seek(length, SeekOrigin.Begin)
            SetEndOfFile(hFile)
            RETURN
        /// <inheritdoc />
        PUBLIC PROPERTY Length AS INT64
            GET
            
                LOCAL highSize := 0 AS INT
                LOCAL fileSize := 0 AS DWORD
                fileSize := GetFileSize(SELF:hFile, OUT highSize)
                IF (fileSize == UInt32.MaxValue)
                    THROW IOException{"Could not retrieve file length"}
                ENDIF
                LOCAL size := (highSize << 0x20) + ((INT64) fileSize) AS INT64
                RETURN size
            END GET
        END PROPERTY
        /// <inheritdoc />
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
            IF (!ret)
                THROW IOException{"Write: File write failed"}
            ENDIF
            IF bytesWritten != count
                THROW IOException{"Write: Not all bytes written to file"}
            ENDIF
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            SELF:smallBuff[0] := b
            SELF:Write(SELF:smallBuff , 0 , 1)
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
        
            LOCAL ret := FALSE AS LOGIC
            ret := LockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF (!ret)
                THROW IOException{"Lock: File lock failed"}
            ENDIF
            RETURN 
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Unlock( position AS INT64, length AS INT64)  AS VOID
            LOCAL ret := FALSE AS LOGIC
            ret := UnlockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF (!ret)
                THROW IOException{"Lock: File unlock failed"}
            ENDIF
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            IF lCommit
                FlushFileBuffers(SELF:hFile)
            ENDIF
            RETURN
            
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(FALSE)
            RETURN

        #region External methods
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "ReadFile")];
        STATIC EXTERN METHOD ReadFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbytesread OUT INT , mustbezero AS IntPtr) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "WriteFile")];
        STATIC EXTERN METHOD WriteFile(hFile AS IntPtr, bytes AS BYTE[], numbytes AS INT, numbyteswritten OUT INT , lpOverlapped AS INT) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "SetFilePointerEx")];
        STATIC EXTERN METHOD SetFilePointerEx(handle AS IntPtr, distance AS INT64 , newAddress OUT INT64, origin AS SeekOrigin ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "LockFile")];
        STATIC EXTERN METHOD LockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "UnlockFile")];
        STATIC EXTERN METHOD UnlockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "FlushFileBuffers")];
        STATIC EXTERN METHOD FlushFileBuffers(hFile AS IntPtr ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "SetEndOfFile")];
        STATIC EXTERN METHOD SetEndOfFile(hFile AS IntPtr ) AS LOGIC
        /// <exclude />
        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "GetFileSize")];
        STATIC EXTERN METHOD GetFileSize(hFile AS IntPtr , highSize OUT INT) AS DWORD
        #endregion

        #region Construct the filestream
        /// <summary>Create a XsFileStream object on Windows and a normal FileStream object on other OS-es</summary>
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF System.Environment.OSVersion:Platform == System.PlatformID.Win32NT .AND. share != FileShare.None
                RETURN CreateXsFileStream(path, mode, faccess, share, bufferSize, options)
            ELSE
                RETURN FileStream{path, mode, faccess, share, bufferSize, options}
            ENDIF

        INTERNAL STATIC METHOD CreateXsFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
        #endregion
    END CLASS

END NAMESPACE
