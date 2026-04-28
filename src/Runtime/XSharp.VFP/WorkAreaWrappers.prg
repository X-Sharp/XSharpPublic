//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
USING XSharp.Internal
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
FUNCTION FCount(uWorkArea AS USUAL) AS DWORD
    RETURN XSharp.RT.Functions.FCount(uWorkArea)

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
[FoxArrayInputParameter(1)];
[FoxProFunction("AFIELDS", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AFields(ArrayName AS USUAL, eWorkArea := NIL AS USUAL) AS INT
    LOCAL nArea AS DWORD
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF
    IF IsNil(eWorkArea)
        nArea := XSharp.RuntimeState.CurrentWorkarea
    ELSE
        nArea := (DWORD) XSharp.RT.Functions.Select(eWorkArea)
    ENDIF

    IF nArea == 0 || !XSharp.RT.Functions.Used(nArea)
        RETURN 0
    ENDIF

    VAR aStruct := XSharp.RT.Functions.DbStruct(nArea)
    VAR nCount := XSharp.RT.Functions.ALen(aStruct)

    IF nCount == 0
        RETURN 0
    ENDIF

    // FoxPro array (18 cols)
    aFoxArray:ReDim(nCount, 18)
    LOCAL i AS DWORD
    FOR i := 1 TO nCount
        VAR aField := (ARRAY) aStruct[i]
        aFoxArray[(INT)i, 1] := (STRING)aField[1] // Field Name
        aFoxArray[(INT)i, 2] := (STRING)aField[2] // Field Type
        aFoxArray[(INT)i, 3] := (INT)aField[3] // Field Width
        aFoxArray[(INT)i, 4] := (INT)aField[4] // Decimal Places

        LOCAL uFld := NULL AS USUAL
        XSharp.RT.Functions.VoDbFieldInfo(11, i, REF uFld) // 11 = DBS_COLUMNINFO
        IF uFld IS XSharp.RDD.DbColumnInfo VAR colInfo
            aFoxArray[(INT)i, 5] := (LOGIC)colInfo:IsNullable
            aFoxArray[(INT)i, 6] := (LOGIC)colInfo:IsBinary
            aFoxArray[(INT)i, 7] := (STRING)colInfo:RuleExpression
            aFoxArray[(INT)i, 8] := (STRING)colInfo:RuleText
            aFoxArray[(INT)i, 9] := (STRING)colInfo:DefaultValue

            // fillout for compatibility
            aFoxArray[(INT)i, 10] := ""
            aFoxArray[(INT)i, 11] := ""
            aFoxArray[(INT)i, 12] := ""
            aFoxArray[(INT)i, 13] := ""
            aFoxArray[(INT)i, 14] := ""
            aFoxArray[(INT)i, 15] := ""
            aFoxArray[(INT)i, 16] := ""
            aFoxArray[(INT)i, 17] := (INT)colInfo:NextValue
            aFoxArray[(INT)i, 18] := (INT)colInfo:StepValue
        ELSE // default
            aFoxArray[(INT)i, 5] := FALSE
            aFoxArray[(INT)i, 6] := FALSE
            aFoxArray[(INT)i, 7] := ""
            aFoxArray[(INT)i, 8] := ""
            aFoxArray[(INT)i, 9] := ""
            aFoxArray[(INT)i, 10] := ""
            aFoxArray[(INT)i, 11] := ""
            aFoxArray[(INT)i, 12] := ""
            aFoxArray[(INT)i, 13] := ""
            aFoxArray[(INT)i, 14] := ""
            aFoxArray[(INT)i, 15] := ""
            aFoxArray[(INT)i, 16] := ""
            aFoxArray[(INT)i, 17] := 0
            aFoxArray[(INT)i, 18] := 0
        ENDIF
    NEXT
    RETURN (INT)nCount

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dbf/*" />
[FoxProFunction("DBF", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION DBF(uArea IN USUAL) AS STRING
    RETURN XSharp.RT.Functions.DBF(uArea)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/deleted/*" />
[FoxProFunction("DELETED", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Deleted(uArea IN USUAL) AS LOGIC
    RETURN XSharp.RT.Functions.Deleted(uArea)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/flock/*" />
[FoxProFunction("FLOCK", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Flock(uArea IN USUAL) AS LOGIC
    RETURN XSharp.RT.Functions.Flock(uArea)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rlock/*" />
[FoxProFunction("RLOCK", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION RLock(uArg1, uArg2) AS LOGIC CLIPPER
    // 0 parameters => block current record in current workarea
    IF PCount() == 0
        RETURN XSharp.RT.Functions.RLock()
    ENDIF

    // 1 parameter => could be either work area (number) or alias (string)
    IF PCount() == 1
        RETURN XSharp.RT.Functions.RLock(uArg1)
    ENDIF

    // 2 parameters => RLock(recordNumber, workArea | tableAlias)
    IF IsString(uArg1)
        RETURN XSharp.RT.Functions.RLock((STRING)uArg1, uArg2)
    ENDIF

    RETURN FALSE

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/lupdate/*" />
[FoxProFunction("LUPDATE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION LUpdate(uArea IN USUAL) AS DATE
    RETURN XSharp.RT.Functions.LUpdate(uArea)
