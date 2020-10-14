USING System
USING System.IO
USING System.Runtime
USING System.Runtime.InteropServices
USING System.Collections.Generic

BEGIN NAMESPACE XSharp.IO

    CLASS XsFileStream INHERIT FileStream
        PROPERTY FileName as STRING AUTO
        CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            SUPER(path, mode, faccess, share, bufferSize, options)
            SELF:FileName := path
            
        PUBLIC OVERRIDE METHOD Seek(offset AS INT64, origin AS SeekOrigin) AS INT64
            LOCAL result := -1 AS INT64
            TRY
                    FClearErrorState()
                result := SUPER:Seek(offset, origin)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN result
        PUBLIC OVERRIDE PROPERTY Position AS INT64 GET SUPER:Position SET Super:Position := value
        PUBLIC OVERRIDE METHOD SetLength(length AS INT64 ) AS VOID
            TRY
                    FClearErrorState()
                SUPER:SetLength(length)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD Read(bytes AS BYTE[] , offset AS INT, count AS INT) AS INT
            LOCAL result := -1 AS INT
            TRY
                    FClearErrorState()
                result := SUPER:Read(bytes, offset, count)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN result
        PUBLIC OVERRIDE METHOD Write(bytes AS BYTE[] , offset AS INT , count AS INT) AS VOID
            TRY
                    FClearErrorState()
                SUPER:Write(bytes, offset, count)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD WriteByte(b AS BYTE ) AS VOID
            TRY
                    FClearErrorState()
                SUPER:WriteByte(b)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD Lock(position AS INT64, length AS INT64)  AS VOID
            TRY
                    FClearErrorState()
                SUPER:Lock(position, length)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD Unlock(position AS INT64, length AS INT64)  AS VOID
            TRY
                    FClearErrorState()
                SUPER:Unlock(position, length)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD Flush(lCommit AS LOGIC) AS VOID
            TRY
                    FClearErrorState()
                SUPER:Flush(lCommit)
            CATCH e AS Exception
                FSetErrorState(e)
                THROW
            END TRY
        RETURN
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(FALSE)
            RETURN
        #region Construct the filestream
        /// <summary>Create a XsFileStream object on Windows and a normal FileStream object on other OS-es</summary>
        STATIC METHOD CreateFileStream (path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            IF IsRunningOnWindows()
                    IF share != FileShare.None
                        RETURN CreateXsSharedFileStream(path, mode, faccess, share, bufferSize, options)
                    ELSE
                        //RETURN XsBufferedFileStream{path, mode, faccess, share, 0xFFFF, options}
                        RETURN XsFileStream{path, mode, faccess, share, 0xFFFF, options}
                ENDIF//
            ELSE
                RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
        ENDIF
        
        INTERNAL STATIC METHOD CreateXsSharedFileStream(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions) AS FileStream
            RETURN XsFileStream{path, mode, faccess, share, bufferSize, options}
        #endregion
        
    END CLASS
    
    END NAMESPACE

