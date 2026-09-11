USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices
USING System.Collections.Generic


#command STARTIO => TRY ; XSharp.IO.File.ClearErrorState()
#command ENDIO   => CATCH e AS Exception ; XSharp.IO.File.SetErrorState(e) ; THROW; END TRY




/// <include file="XSharp.Core.Docs.xml" path="doc/UseBufferedFileStream/*" />
FUNCTION UseBufferedFileStream(lUse AS LOGIC) AS LOGIC
    LOCAL lOld := XsFileStream.UseBufferedFileStream  AS LOGIC
    XsFileStream.UseBufferedFileStream := lUse
    RETURN lOld

/// <include file="XSharp.Core.Docs.xml" path="doc/UseBufferedFileStream_2/*" />
FUNCTION UseBufferedFileStream() AS LOGIC
    RETURN XsFileStream.UseBufferedFileStream


BEGIN NAMESPACE XSharp.IO
    /// <include file="XSharp.Core.Docs.xml" path="doc/XsFileStream/*" />
    CLASS XsFileStream INHERIT FileStream
        STATIC INTERNAL UseBufferedFileStream := TRUE AS LOGIC
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsFileStream.FileName/*" />
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
            TRY
                XSharp.IO.File.ClearErrorState()
                SUPER:Lock(position, length)
            CATCH e AS Exception
                SELF:__SetLockError(e)
                THROW
            END TRY
            RETURN
        /// <inheritdoc />
        PUBLIC OVERRIDE METHOD Unlock(position AS INT64, length AS INT64)  AS VOID
            TRY
                XSharp.IO.File.ClearErrorState()
                SUPER:Unlock(position, length)
            CATCH e AS Exception
                SELF:__SetLockError(e)
                THROW
            END TRY
        RETURN

        // A failed lock is what NetErr() reports in the xBase world. SetErrorState() only sets NetErr for
        // a sharing violation (32), but a lock violation is 33, so it has to be set here.
        PRIVATE METHOD __SetLockError(e AS Exception) AS VOID
            XSharp.IO.File.SetErrorState(e)
            IF RuntimeState.FileError == 0
                FError(33) // DOS lock violation
            ENDIF
            NetErr(TRUE)
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
        /// <include file="XSharp.Core.Docs.xml" path="doc/XsFileStream.CreateFileStream/*" />
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF share != FileShare.None
                // Shared access, so the same file may change under us at any moment. There is no platform
                // check here anymore: this used to be the branch that decided between a hand written Win32
                // stream on Windows and a buffered stream everywhere else.
                RETURN CreateSharedFileStream(path, mode, faccess, share, bufferSize, options)
            ELSE
                IF UseBufferedFileStream
                    RETURN XsBufferedFileStream{path, mode, faccess, share, bufferSize, options}
                ELSE
                    RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
                ENDIF
            ENDIF

        INTERNAL STATIC METHOD CreateSharedFileStream(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsWin32FileStream{path, mode, faccess, share, bufferSize, options}

        /// <exclude />
        [Obsolete("Use CreateSharedFileStream(). The shared file stream no longer uses the Win32 API.")];
        INTERNAL STATIC METHOD CreateWin32FileStream(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN CreateSharedFileStream(path, mode, faccess, share, bufferSize, options)
        #endregion

    END CLASS

    END NAMESPACE

