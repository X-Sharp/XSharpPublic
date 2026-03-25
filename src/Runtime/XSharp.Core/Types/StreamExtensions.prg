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
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp
    STATIC CLASS FileStreamExensions

        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeReadAt/*" />
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
            RETURN oStream:SafeRead(buffer, length)


        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeRead/*" />
        STATIC METHOD SafeRead(SELF oStream AS FileStream, buffer AS BYTE[], length AS LONG) AS LOGIC
            LOCAL result AS LONG
            TRY
                result := oStream:Read(buffer, 0, length)
            CATCH
                result := -1
            END TRY
            RETURN result == length

        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeRead_2/*" />
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


        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeWriteAt/*" />
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


        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeWrite/*" />
        STATIC METHOD SafeWrite(SELF oStream AS FileStream, buffer AS BYTE[], length AS LONG) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                oStream:Write(buffer, 0, length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeWriteByte/*" />
        STATIC METHOD SafeWriteByte(SELF oStream AS FileStream, b AS BYTE) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                oStream:WriteByte(b)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

         /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeSetPos/*" />
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

        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeLock/*" />
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

         /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeUnlock/*" />
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

        /// <include file="XSharp.Core.Docs.xml" path="doc/FileStreamExensions.SafeSetLength/*" />
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
