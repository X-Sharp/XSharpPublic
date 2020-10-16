USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices
USING System.Collections.Generic


#command STARTIO => TRY ; FClearErrorState()
#command ENDIO   => CATCH e AS Exception ; FSetErrorState(e) ; THROW; END TRY


FUNCTION UseBufferedFileStream(lUse AS LOGIC) AS LOGIC
    LOCAL lOld := XsFileStream.UseBufferedFileStream  AS LOGIC
    XsFileStream.UseBufferedFileStream := lUse
    RETURN lOld
    
FUNCTION UseBufferedFileStream() AS LOGIC
    RETURN XsFileStream.UseBufferedFileStream  


BEGIN NAMESPACE XSharp.IO

    CLASS XsFileStream INHERIT FileStream
        STATIC INTERNAL UseBufferedFileStream := TRUE AS LOGIC
        PROPERTY FileName as STRING AUTO
        CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            SUPER(path, mode, faccess, share, bufferSize, options)
            SELF:FileName := path
            
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            LOCAL result := -1 AS INT64
            STARTIO
            result := SUPER:Seek(offset, origin)
            ENDIO
            RETURN result
            
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            STARTIO
            SUPER:SetLength(length)
            ENDIO
            RETURN
        PUBLIC OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            LOCAL result := -1 AS INT
            STARTIO
            result := SUPER:Read(bytes, offset, count)
            ENDIO
            RETURN result
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            STARTIO
            SUPER:Write(bytes, offset, count)
            ENDIO
            RETURN
            
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            STARTIO
            SUPER:WriteByte(b)
            ENDIO
            RETURN
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
            STARTIO
            SUPER:Lock(position, length)
            ENDIO
            RETURN
            
        PUBLIC OVERRIDE METHOD Unlock(position AS INT64, length AS INT64)  AS VOID
            STARTIO
            SUPER:Unlock(position, length)
            ENDIO
        RETURN
        
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            STARTIO
            SUPER:Flush(lCommit)
            ENDIO
        RETURN
        
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(FALSE)
            RETURN
            
        #region Construct the filestream
        /// <summary>Create a XsFileStream object on Windows and a normal FileStream object on other OS-es</summary>
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF share != FileShare.None
                IF IsRunningOnWindows()
                    RETURN CreateXsSharedFileStream(path, mode, faccess, share, bufferSize, options)
                ELSE
                    RETURN XsFileStream{path, mode, faccess, share, 0xFFFF, options}
                ENDIF
            ELSE
                IF UseBufferedFileStream
                    RETURN XsBufferedFileStream{path, mode, faccess, share, 0xFFFF, options}
                ELSE
                    RETURN XsFileStream{path, mode, faccess, share, 0xFFFF, options}
                ENDIF
            ENDIF
        
        INTERNAL STATIC METHOD CreateXsSharedFileStream(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
        #endregion
        
    END CLASS
    
    END NAMESPACE

