//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING LanguageService.CodeAnalysis.XSharp
BEGIN NAMESPACE XSharpModel
STATIC CLASS XDebuggerSettings
    PUBLIC STATIC PROPERTY AllowEditing AS LOGIC AUTO
    PUBLIC STATIC PROPERTY NoLateBinding AS LOGIC AUTO

    // Settings that are set when the debugger is started,
    // so we do not have to read them from the running app
    PUBLIC STATIC PROPERTY Dialect AS INT AUTO
    PUBLIC STATIC PROPERTY LateBinding AS LOGIC AUTO
    PUBLIC STATIC PROPERTY MemVars AS LOGIC AUTO
    PUBLIC STATIC PROPERTY UndeclaredMemvars AS LOGIC AUTO
    PUBLIC STATIC PROPERTY ArrayZero AS LOGIC AUTO
    PUBLIC STATIC PROPERTY CaseSensitive AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo4 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo6 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo7 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo10 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo12 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo13 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY Vo14 AS LOGIC AUTO

    // These reflect the current debugger state
    PUBLIC STATIC PROPERTY DebuggerMode         AS DebuggerMode AUTO
    PUBLIC STATIC PROPERTY DebuggerIsRunning    AS LOGIC GET DebuggerMode != DebuggerMode.Design
    PUBLIC STATIC PROPERTY DebuggingXSharpExe   as LOGIC AUTO



END CLASS

END NAMESPACE
