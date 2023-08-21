//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Extension methods to the FileStream class that capture exceptions and return "smart" values so the calling code 
// does not have to set exception handlers and looks cleaner

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Diagnostics

BEGIN NAMESPACE XSharp
    STATIC CLASS FileStreamExensions
        /// <summary>Read data at a location in the file. Makes sure that file locations are >= 0. Assumes whole buffer must be read.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamRead/*" />
        STATIC METHOD SafeReadAt(SELF oStream AS FileStream, pos AS INT64, buffer AS BYTE[]) AS LOGIC
            Debug.Assert(pos >= 0)
            IF pos < 0
                RETURN FALSE
            ENDIF
            oStream:Position := pos
            var ok := oStream:SafeRead(buffer, buffer:Length)
            return ok

        /// <summary>Read data at a location in the file. Makes sure that file locations are >= 0</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamRead/*" />
        STATIC METHOD SafeReadAt(SELF oStream AS FileStream, pos AS INT64, buffer AS BYTE[], length AS LONG) AS LOGIC
            Debug.Assert(pos >= 0)
            IF pos < 0
                RETURN FALSE
            ENDIF
            Debug.Assert(length >= 0)
            IF length < 0
                RETURN FALSE
            ENDIF            
            oStream:Position := pos
            VAR ok := oStream:SafeRead(buffer, length)
            RETURN ok
            
        /// <summary>Read data from a stream. Assumes that the whole buffer must be read. Reads from current location.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamRead/*" />
        STATIC METHOD SafeRead(SELF oStream AS FileStream, buffer AS BYTE[]) AS LOGIC
            RETURN SafeRead(oStream, buffer, buffer:Length)
            
        /// <summary>Read data from a stream. Contains TRY CATCH mechanism. Reads from current location.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamRead/*" />
        STATIC METHOD SafeRead(SELF oStream AS FileStream, buffer AS BYTE[], length AS LONG) AS LOGIC
            LOCAL result AS LONG
            TRY
                result := oStream:Read(buffer, 0, length)
            CATCH
                result := -1
            END TRY
            RETURN result == length
            
        /// <summary>Read data from a stream. Contains TRY CATCH mechanism. Reads from current location.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamRead/*" />
        STATIC METHOD SafeRead(SELF oStream AS FileStream, buffer AS BYTE[], length AS LONG, lread OUT LONG) AS LOGIC
            LOCAL result AS LOGIC
            lread := -1
            Debug.Assert(length >= 0)
            IF length < 0
                RETURN FALSE
            ENDIF
            TRY
                  
                lread := oStream:Read(buffer, 0, length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

        /// <summary>Write data at a location in the file. Makes sure that file locations are >= 0. Assumes whole buffer must be written.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
        STATIC METHOD SafeWriteAt(SELF oStream AS FileStream, pos AS INT64, buffer AS BYTE[]) AS LOGIC
            Debug.Assert(pos >= 0)
            IF pos < 0
                RETURN FALSE
            ENDIF
            oStream:Position := pos
            var ok := oStream:SafeWrite(buffer, buffer:Length)
            return ok

        /// <summary>Write data at a location in the file. Makes sure that file locations are >= 0. </summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
        STATIC METHOD SafeWriteAt(SELF oStream AS FileStream, pos AS INT64, buffer AS BYTE[], length AS LONG) AS LOGIC
            Debug.Assert(pos >= 0)
            IF pos < 0
                RETURN FALSE
            ENDIF
            Debug.Assert(length >= 0)
            IF length < 0
                RETURN FALSE
            ENDIF
            oStream:Position := pos
            var ok := oStream:SafeWrite(buffer, length)
            return ok
            
        /// <summary>Write data to a stream. Contains TRY CATCH mechanism. Writes to the current location. Assumes the whole buffer must be written.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
       STATIC METHOD SafeWrite(SELF oStream AS FileStream, buffer AS BYTE[]) AS LOGIC
            RETURN SafeWrite(oStream, buffer, buffer:Length)
            
        /// <summary>Write data to a stream. Contains TRY CATCH mechanism. Writes to the current location.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
        STATIC METHOD SafeWrite(SELF oStream AS FileStream, buffer AS BYTE[], length AS LONG) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                oStream:Write(buffer, 0, length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

       /// <summary>Write data to a stream. Contains TRY CATCH mechanism. Writes to the current location.</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
        STATIC METHOD SafeWriteByte(SELF oStream AS FileStream, b AS BYTE) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                oStream:WriteByte(b)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result
            
        /// <summary>Sets the location of a stream. Contains TRY CATCH mechanism. .</summary>
        /// <include file="CoreComments.xml" path="Comments/StreamWrite/*" />
         STATIC METHOD SafeSetPos(SELF oStream AS FileStream, offset AS INT64) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                Debug.Assert(offset >= 0)
                IF offset < 0
                    RETURN FALSE
                ENDIF
                oStream:Position := offset
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result
            
        /// <summary>Locks a region in a stream. Contains TRY CATCH mechanism. </summary>
        /// <param name="oStream">The stream to lock.</param>
        /// <param name="offset">Start of the region to lock. The method checks to see that the location is >= 0.</param>
        /// <param name="length">Size of the region to lock. The method checks to see that the size is > 0.</param>
        /// <returns>TRUE when succesfull, FALSE when an exception occurred during reading or when offset or length were invalid.</returns>
        STATIC METHOD SafeLock(SELF oStream AS FileStream, offset AS INT64, length AS INT64) AS LOGIC
           LOCAL result AS LOGIC
            TRY
                Debug.Assert(offset >= 0)
                IF offset < 0
                    RETURN FALSE
                ENDIF
                Debug.Assert(length > 0)
                IF length <= 0
                    RETURN FALSE
                ENDIF
                oStream:Lock(offset, length)
                result := TRUE
            CATCH 
                result := FALSE
            END TRY
            RETURN result

        /// <summary>Unlocks a region in a stream. Contains TRY CATCH mechanism. </summary>
        /// <param name="oStream">The stream to unlock.</param>
        /// <param name="offset">Start of the region to unlock. The method checks to see that the location is >= 0.</param>
        /// <param name="length">Size of the region to unlock. The method checks to see that the size is > 0.</param>
        /// <returns>TRUE when succesfull, FALSE when an exception occurred during reading or when offset or length were invalid.</returns>
         STATIC METHOD SafeUnlock(SELF oStream AS FileStream, offset AS INT64, length AS INT64) AS LOGIC
           LOCAL result AS LOGIC
            TRY
                Debug.Assert(offset >= 0)
                IF offset < 0
                    RETURN FALSE
                ENDIF
                Debug.Assert(length > 0)
                IF length <= 0
                    RETURN FALSE
                ENDIF
                oStream:Unlock(offset, length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

        /// <summary>Sets the length of a stream. Contains TRY CATCH mechanism. </summary>
        /// <param name="oStream">The stream for which to set the lengt.</param>
        /// <param name="length">The new length of the stream. The method checks to see that the location is &gt;= 0.</param>
        /// <returns>TRUE when succesfull, FALSE when an exception occurred during reading or when the length &lt; 0</returns>
        STATIC METHOD SafeSetLength(SELF oStream AS FileStream, length AS INT64) AS LOGIC
           LOCAL result AS LOGIC
            TRY
                Debug.Assert(length >= 0)
                IF length < 0
                    RETURN FALSE
                ENDIF
                oStream:SetLength(length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result 
    END CLASS
END NAMESPACE    
