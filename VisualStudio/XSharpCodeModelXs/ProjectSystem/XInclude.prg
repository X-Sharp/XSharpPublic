//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Diagnostics

BEGIN NAMESPACE XSharpModel
/// <summary>
/// This class stores the include files that the lexer and preprocessor processed
/// </summary>
[DebuggerDisplay("{FileName,nq}")];
CLASS XInclude
    private _id := -1 as Int64
    private _fileName as STRING
    PROPERTY Id   AS INT64                     GET _id INTERNAL SET _id := value
    PROPERTY FileName as STRING GET _fileName
    CONSTRUCTOR(cFileName as STRING)
        _fileName := cFileName
        RETURN

END CLASS
END NAMESPACE
