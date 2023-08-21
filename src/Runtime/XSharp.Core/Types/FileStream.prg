USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices
USING System.Collections.Generic


#command STARTIO => TRY ; XSharp.IO.File.ClearErrorState()
#command ENDIO   => CATCH e AS Exception ; XSharp.IO.File.SetErrorState(e) ; THROW; END TRY



/// <summary>Control the buffered File IO setting for the runtime</summary>
/// <remarks>The default behavior for X# 2.7 and later is to use Buffered Filestreams for Exclusive disk io. <br/>
/// With this function you can switch that behavior on and off.</remarks>
/// <param name="lUse">Should the buffer file IO be used ?</param>
/// <returns>Previous setting for the buffered file IO</returns>

FUNCTION UseBufferedFileStream(lUse AS LOGIC) AS LOGIC
    LOCAL lOld := XsFileStream.UseBufferedFileStream  AS LOGIC
    XsFileStream.UseBufferedFileStream := lUse
    RETURN lOld

/// <summary>Retrieve the buffered File IO setting for the runtime</summary>
/// <returns>Current setting for the buffered file IO</returns>
FUNCTION UseBufferedFileStream() AS LOGIC
    RETURN XsFileStream.UseBufferedFileStream


BEGIN NAMESPACE XSharp.IO
    /// <summary>Special Filestream class that sets the runtime IO Errorstate when an error occurs. </summary>
    CLASS XsFileStream INHERIT FileStream
        STATIC INTERNAL UseBufferedFileStream := TRUE AS LOGIC
        /// <summary>The name of the file opened in the stream.</summary>
        PROPERTY FileName as STRING AUTO
        /// <inheritdoc />
        CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            SUPER(path, mode, faccess, share, bufferSize, options)
            SELF:FileName := path
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            LOCAL result := -1 AS INT64
            STARTIO
            result := SUPER:Seek(offset, origin)
            ENDIO
            RETURN result
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            STARTIO
            SUPER:SetLength(length)
            ENDIO
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            LOCAL result := -1 AS INT
            STARTIO
            result := SUPER:Read(bytes, offset, count)
            ENDIO
            RETURN result
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            STARTIO
            SUPER:Write(bytes, offset, count)
            ENDIO
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            STARTIO
            SUPER:WriteByte(b)
            ENDIO
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
            STARTIO
            SUPER:Lock(position, length)
            ENDIO
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Unlock(position AS INT64, length AS INT64)  AS VOID
            STARTIO
            SUPER:Unlock(position, length)
            ENDIO
        RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            STARTIO
            SUPER:Flush(lCommit)
            ENDIO
        RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(FALSE)
            RETURN

        #region Construct the filestream
        /// <summary>Create a XsFileStream object on Windows and a normal FileStream object on other OS-es</summary>
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF share != FileShare.None
                IF RuntimeState.RunningOnWindows
                    RETURN CreateWin32FileStream(path, mode, faccess, share, bufferSize, options)
                ELSE
                    RETURN XsFileStream{path, mode, faccess, share, 0xFFFF, options}
                ENDIF
            ELSE
                IF UseBufferedFileStream
                    RETURN XsBufferedFileStream{path, mode, faccess, share, bufferSize, options}
                ELSE
                    RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
                ENDIF
            ENDIF

        INTERNAL STATIC METHOD CreateWin32FileStream(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsWin32FileStream{path, mode, faccess, share, bufferSize, options}
        #endregion

    END CLASS

    END NAMESPACE

