//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
BEGIN NAMESPACE XSharp
CLASS Win32FileStream INHERIT System.IO.FileStream
    // Fields
    PRIVATE hFile 		AS System.IntPtr
    PRIVATE onebytebuf 	AS BYTE[] 
    PRIVATE buffer := IntPtr.Zero AS IntPtr
	PRIVATE buffSize AS LONG    

	PRIVATE METHOD _AllocBuffer(nSize AS LONG) AS IntPtr
		IF nSize > buffSize
			IF buffer != IntPtr.Zero        
				Marshal.FreeHGlobal(buffer)		
			ENDIF
			buffer 		:= Marshal.AllocHGlobal(nSize+1)
			buffSize 	:= nSize
		ENDIF
		RETURN buffer

    CONSTRUCTOR(path AS STRING, mode AS System.IO.FileMode, fileAccess AS System.IO.FileAccess, share AS System.IO.FileShare, bufferSize AS LONG, options AS System.IO.FileOptions)//Inline call to base() in C#
    SUPER(path, mode, fileAccess, share, bufferSize, options)
        SELF:hFile := SELF:SafeFileHandle:DangerousGetHandle()
        SELF:onebytebuf := BYTE[]{1}

	DESTRUCTOR              
		IF buffer != IntPtr.Zero
			Marshal.FreeHGlobal(buffer)		
		ENDIF
		

    VIRTUAL METHOD Flush() AS VOID
		SELF:Flush(TRUE)

    VIRTUAL METHOD Flush(flushToDisk AS LOGIC) AS VOID
        IF (flushToDisk)
            //
            Win32FileStream.FlushFileBuffers(SELF:hFile)
        ENDIF

    VIRTUAL METHOD Lock(position AS INT64, length AS INT64) AS VOID
        IF (! Win32FileStream.LockFile(SELF:hFile, (LONG)position , (LONG)(position >> 32) , (LONG)length , (LONG)(length >> 32) ))
            //
            THROW System.IO.IOException{"Lock: File lock failed"}
        ENDIF

    VIRTUAL METHOD Read(bytes AS BYTE[], offset AS LONG, count AS LONG) AS LONG
        LOCAL fOk AS LOGIC
        LOCAL numBytesRead AS LONG
        LOCAL buffer := IntPtr.Zero AS IntPtr
        //
        fOk 	:= FALSE
        buffer 	:= _AllocBuffer(count)
        fOk 	:= Win32FileStream.ReadFile(SELF:hFile, (buffer + offset), count, OUT numBytesRead, System.IntPtr.Zero)
        IF (! fOk)
            RETURN -1
        ENDIF
        Marshal.Copy(buffer, bytes, 0, numBytesRead)
        RETURN numBytesRead

    VIRTUAL UNSAFE  METHOD Seek(offset AS INT64, origin AS System.IO.SeekOrigin) AS INT64
        LOCAL hi AS LONG
        LOCAL num2 AS LONG
        hi 		:= (LONG)(offset >> 32) 
        num2 	:= Win32FileStream.SetFilePointerWin32(SELF:hFile, (LONG)offset , @hi, origin)
        RETURN ((hi << 32) + num2)

    VIRTUAL METHOD SetLength(value AS INT64) AS VOID
        SELF:Seek(value, System.IO.SeekOrigin.Begin)
        Win32FileStream.SetEndOfFile(SELF:hFile)

    VIRTUAL METHOD Unlock(position AS INT64, length AS INT64) AS VOID
        Win32FileStream.UnlockFile(SELF:hFile, (LONG)position , (LONG)(position >> 32) , (LONG)length , (LONG)(length >> 32) )

    VIRTUAL METHOD Write(bytes AS BYTE[], offset AS LONG, count AS LONG) AS VOID
        LOCAL fOk AS LOGIC
        LOCAL lpNumberOfBytesWritten AS LONG
        LOCAL buffer := IntPtr.Zero AS IntPtr

        fOk := FALSE
        buffer := _AllocBuffer(count)
        Marshal.Copy(bytes, 0, buffer, count)
        fOk := Win32FileStream.WriteFile(SELF:hFile, (buffer + offset), count, OUT lpNumberOfBytesWritten, 0)
        IF (! fOk)
            //
            THROW System.IO.IOException{"Write: File write failed"}
        ENDIF
        IF (lpNumberOfBytesWritten != count)
            //
            THROW System.IO.IOException{"Write: Not all bytes written to file"}
        ENDIF
        
    VIRTUAL METHOD WriteByte(b AS BYTE) AS VOID
        SELF:onebytebuf[1] := b
        SELF:Write(SELF:onebytebuf, 0, 1)


    // Properties
    VIRTUAL PROPERTY Length AS INT64
        GET
            //
            LOCAL highSize AS Int32
            LOCAL fileSize AS Int32
            fileSize := Win32FileStream.GetFileSize(SELF:hFile, OUT highSize)
            IF (fileSize == -1)
                //
                THROW System.IO.IOException{"Could not retrieve file length"}
            ENDIF
            RETURN (highSize + fileSize)
        END GET
    END PROPERTY

#region API
    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    PRIVATE STATIC METHOD FlushFileBuffers(hFile AS System.IntPtr) AS LOGIC

    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    INTERNAL STATIC METHOD GetFileSize(hFile AS System.IntPtr,  highSize OUT LONG) AS LONG

    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    PRIVATE STATIC METHOD LockFile(hFile AS System.IntPtr, dwFileOffsetLow AS LONG, dwFileOffsetHigh AS LONG, nNumberOfBytesToLockLow AS LONG, nNumberOfBytesToLockHigh AS LONG) AS LOGIC

    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    PRIVATE UNSAFE STATIC METHOD ReadFile(handle AS System.IntPtr, bytes AS IntPtr, numBytesToRead AS LONG,  numBytesRead OUT LONG, mustBeZero AS System.IntPtr) AS LOGIC

    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    INTERNAL STATIC METHOD SetEndOfFile(hFile AS System.IntPtr) AS LOGIC

    [DllImport("kernel32.dll", EntryPoint:="SetFilePointer")];
    PRIVATE UNSAFE STATIC METHOD SetFilePointerWin32(handle AS System.IntPtr, lo AS LONG, hi AS LONG PTR, origin AS System.IO.SeekOrigin) AS LONG

    [DllImport("kernel32.dll", SetLastError:=TRUE)];
    PRIVATE STATIC METHOD UnlockFile(hFile AS System.IntPtr, dwFileOffsetLow AS LONG, dwFileOffsetHigh AS LONG, nNumberOfBytesToUnlockLow AS LONG, nNumberOfBytesToUnlockHigh AS LONG) AS LOGIC

    [DllImport("Kernel32.dll", SetLastError:=TRUE)];
    PRIVATE UNSAFE STATIC METHOD WriteFile(hFile AS System.IntPtr, bytes AS IntPtr, nNumberOfBytesToWrite AS LONG,  lpNumberOfBytesWritten OUT LONG, lpOverlapped AS LONG) AS LOGIC

#endregion
END CLASS
END NAMESPACE
 

