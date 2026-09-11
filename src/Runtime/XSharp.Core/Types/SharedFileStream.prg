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
    /// <include file="XSharp.Core.Docs.xml" path="doc/XsWin32FileStream/*" />
    /// <remarks>
    /// This class used to do all of its IO with the Win32 API (ReadFile, WriteFile, SetFilePointerEx,
    /// LockFile) on the raw file handle of the base class, because the FileStream of that time cached the
    /// file position and the file length, which is wrong for a file that other processes may change.
    /// <br/>
    /// It does not do that anymore. The .Net FileStream reads and writes at an explicit offset and does not
    /// cache the length of a file that others may write to, so it reports the truth for shared access by
    /// itself, on every platform. What is left here is the one thing that is special about a shared stream:
    /// it must not buffer, and Flush() must commit to disk.
    /// <br/>
    /// The name is kept because this class is public and is handed to user code through DBI_FILESTREAM.
    /// </remarks>
    CLASS XsWin32FileStream INHERIT XsFileStream
        INTERNAL CONSTRUCTOR(path AS STRING, mode AS FileMode, faccess AS FileAccess, share AS FileShare, bufferSize AS LONG, options AS FileOptions)
            // bufferSize 1 means unbuffered. A buffer would hand out bytes that another process has already
            // changed, and would hold back bytes that another process is waiting for, so the bufferSize of
            // the caller is deliberately ignored: for shared access there is no good buffer size but none.
            SUPER(path, mode, faccess, share, 1, options)
        RETURN

        /// <inheritdoc />
        /// <remarks>A shared file stream commits to disk, so that other processes see the change.</remarks>
        PUBLIC OVERRIDE METHOD Flush() AS VOID
            SELF:Flush(TRUE)
            RETURN

    END CLASS


END NAMESPACE
