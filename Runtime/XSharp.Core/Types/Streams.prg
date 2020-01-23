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

    UNSAFE CLASS XsFileStream INHERIT FileStream
        PRIVATE hFile AS Intptr
        PRIVATE smallBuff AS BYTE[]
        PRIVATE CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) 
            SUPER(path, mode, faccess, share, bufferSize, options)
            hFile := SELF:SafeFileHandle:DangerousGetHandle()
            smallBuff := BYTE[]{1}
            RETURN

        UNSAFE PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            VAR hi := (INT) (offset >> 32)
            VAR lo := SetFilePointerWin32(hFile, (INT)offset, @hi, origin)
            RETURN (((INT64) hi) << 32) + lo

        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
			// warning: does not restore original file pos
            SELF:Seek(length, SeekOrigin.Begin)
            SetEndOfFile(hFile)
            RETURN

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

        PUBLIC UNSAFE OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            LOCAL ret := FALSE AS LOGIC
            LOCAL bytesRead := 0 AS INT
            BEGIN FIXED LOCAL numRef := bytes AS BYTE PTR
                ret := ReadFile(SELF:hFile, numRef + offset, count, OUT bytesRead, IntPtr.Zero)
            END FIXED
            IF !ret
                RETURN -1
            ENDIF
            RETURN bytesRead

        PUBLIC UNSAFE OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            LOCAL ret := FALSE AS LOGIC
            LOCAL bytesWritten := 0 AS INT
            BEGIN FIXED LOCAL numRef := bytes AS BYTE PTR
                ret := WriteFile(SELF:hFile, numRef + offset, count, OUT bytesWritten, 0)
            END FIXED
            IF (!ret)
                THROW IOException{"Write: File write failed"}
            ENDIF
            IF bytesWritten != count
                THROW IOException{"Write: Not all bytes written to file"}
            ENDIF
            RETURN

        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            SELF:smallBuff[1] := b
            SELF:Write(SELF:smallBuff , 0 , 1)
            
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
        
            LOCAL ret := FALSE AS LOGIC
            ret := LockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF (!ret)
                THROW IOException{"Lock: File lock failed"}
            ENDIF
            RETURN 
            
        PUBLIC OVERRIDE METHOD Unlock( position AS INT64, length AS INT64)  AS VOID
            LOCAL ret := FALSE AS LOGIC
            ret := UnLockFile(SELF:hFile, (INT)position, (INT)(position >> 32), (INT)(length), (INT)(length >> 32))
            IF (!ret)
                THROW IOException{"Lock: File unlock failed"}
            ENDIF
            RETURN

        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            IF lCommit
                FlushFileBuffers(SELF:hFile)
            ENDIF
            RETURN

        PUBLIC OVERRIDE METHOD Flush() AS VOID
            RETURN

        #region External methods
        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "ReadFile")];
        UNSAFE STATIC EXTERN METHOD ReadFile(hFile AS IntPtr, bytes AS BYTE PTR, numbytes AS INT, numbytesread OUT INT , mustbezero AS intptr) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "WriteFile")];
        UNSAFE STATIC EXTERN METHOD WriteFile(hFile AS IntPtr, bytes AS BYTE PTR, numbytes AS INT, numbyteswritten OUT INT , lpOverlapped AS INT) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE, EntryPoint := "SetFilePointer")];
        UNSAFE STATIC EXTERN METHOD SetFilePointerWin32(handle AS IntPtr, lo AS INT , hi AS INT PTR, origin AS SeekOrigin ) AS INT

        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "LockFile")];
        STATIC EXTERN METHOD LockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "UnlockFile")];
        STATIC EXTERN METHOD UnlockFile(hFile AS IntPtr , dwFileOffsetLow AS INT , dwFileOffsetHigh AS INT , nNumberOfBytesToLockLow AS INT , nNumberOfBytesToLockHigh AS INT ) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "FlushFileBuffers")];
        STATIC EXTERN METHOD FlushFileBuffers(hFile AS IntPtr ) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "SetEndOfFile")];
        STATIC EXTERN METHOD SetEndOfFile(hFile AS IntPtr ) AS LOGIC

        [DllImport("kernel32.dll", SetLastError := TRUE,EntryPoint := "GetFileSize")];
        STATIC EXTERN METHOD GetFileSize(hFile AS IntPtr , highSize OUT INT) AS DWORD
        #endregion

        #region Construct the filestream
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF System.Environment.OSVersion:Platform == System.PlatformID.Win32NT
                RETURN CreateXsFileStream(path, mode, faccess, share, bufferSize, options)
            ELSE
                RETURN FileStream{path, mode, faccess, share, bufferSize, options}
            ENDIF

        STATIC METHOD CreateXsFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
        #endregion
    END CLASS

END NAMESPACE
