//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/eof/*" />
[FoxProFunction("EOF", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Eof() AS LOGIC
    RETURN XSharp.RT.Functions.Eof()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/recno/*" />
[FoxProFunction("RECNO", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION RecNo() AS DWORD
    RETURN XSharp.RT.Functions.RecNo()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/reccount/*" />
[FoxProFunction("RECCOUNT", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION RecCount() AS DWORD
    RETURN XSharp.RT.Functions.RecCount()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/found/*" />
[FoxProFunction("FOUND", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Found() AS LOGIC
    RETURN XSharp.RT.Functions.Found()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/bof/*" />
[FoxProFunction("BOF", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Bof() AS LOGIC
    RETURN XSharp.RT.Functions.Bof()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/used/*" />
[FoxProFunction("USED", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Used() AS LOGIC
    RETURN XSharp.RT.Functions.Used()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/select/*" />
[FoxProFunction("SELECT", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Select(uWorkArea AS USUAL) AS USUAL
    RETURN XSharp.RT.Functions.Select(uWorkArea)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alias/*" />
[FoxProFunction("ALIAS", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Alias(uWorkArea AS USUAL) AS STRING
    RETURN XSharp.RT.Functions.Alias(uWorkArea)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fcount/*" />
[FoxProFunction("FCOUNT", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FCount() AS DWORD
    RETURN XSharp.RT.Functions.FCount()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/header/*" />
[FoxProFunction("HEADER", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Header() AS INT
    RETURN XSharp.RT.Functions.Header()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/recsize/*" />
[FoxProFunction("RECSIZE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION RecSize(uArea AS USUAL) AS INT
    RETURN XSharp.RT.Functions.RecSize()

// ---------------------------------------------------------------- //
// TODO(irwin): functions pending to implement
// ---------------------------------------------------------------- //
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/afields/*" />
[FoxProFunction("AFIELDS", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION AFields(ArrayName AS ARRAY, eWorkArea := NIL AS USUAL) AS INT
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dbf/*" />
[FoxProFunction("DBF", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION DBF(eWorkArea := NIL AS USUAL) AS STRING
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/deleted/*" />
[FoxProFunction("DELETED", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION Deleted(eWorkArea := NIL AS USUAL) AS LOGIC
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/flock/*" />
[FoxProFunction("FLOCK", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION Flock(eWorkArea := NIL AS USUAL) AS LOGIC
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rlock/*" />
[FoxProFunction("RLOCK", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION RLock(cRecordList := "" AS STRING, eWorkArea := NIL AS USUAL) AS LOGIC
    THROW NotImplementedException{}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/lupdate/*" />
[FoxProFunction("LUPDATE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Stub, FoxCriticality.High)];
FUNCTION LUpdate(eWorkArea := NIL AS USUAL) AS DATE
    THROW NotImplementedException{}
