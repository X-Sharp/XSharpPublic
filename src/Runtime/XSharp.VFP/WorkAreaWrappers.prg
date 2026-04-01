//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support

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
[FoxProFunction("AFIELDS", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AFields(ArrayName AS ARRAY, eWorkArea := NIL AS USUAL) AS INT
    LOCAL nArea AS DWORD
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
    IF ArrayName IS __FoxArray VAR foxArray
        foxArray:ReDim(nCount, 18)
        LOCAL i AS DWORD
        FOR i := 1 TO nCount
            VAR aField := (ARRAY) aStruct[i]
            foxArray[(INT)i, 1] := (STRING)aField[1] // Field Name
            foxArray[(INT)i, 2] := (STRING)aField[2] // Field Type
            foxArray[(INT)i, 3] := (INT)aField[3] // Field Width
            foxArray[(INT)i, 4] := (INT)aField[4] // Decimal Places

            LOCAL uFld := NULL AS USUAL
            XSharp.RT.Functions.VoDbFieldInfo(11, i, REF uFld) // 11 = DBS_COLUMNINFO
            IF uFld IS XSharp.RDD.DbColumnInfo VAR colInfo
                foxArray[(INT)i, 5] := (LOGIC)colInfo:IsNullable
                foxArray[(INT)i, 6] := (LOGIC)colInfo:IsBinary
                foxArray[(INT)i, 7] := (STRING)colInfo:RuleExpression
                foxArray[(INT)i, 8] := (STRING)colInfo:RuleText
                foxArray[(INT)i, 9] := (STRING)colInfo:DefaultValue

                // fillout for compatibility
                foxArray[(INT)i, 10] := ""
                foxArray[(INT)i, 11] := ""
                foxArray[(INT)i, 12] := ""
                foxArray[(INT)i, 13] := ""
                foxArray[(INT)i, 14] := ""
                foxArray[(INT)i, 15] := ""
                foxArray[(INT)i, 16] := ""
                foxArray[(INT)i, 17] := (INT)colInfo:NextValue
                foxArray[(INT)i, 18] := (INT)colInfo:StepValue
            ELSE // default
                foxArray[(INT)i, 5] := FALSE
                foxArray[(INT)i, 6] := FALSE
                foxArray[(INT)i, 7] := ""
                foxArray[(INT)i, 8] := ""
                foxArray[(INT)i, 9] := ""
                foxArray[(INT)i, 10] := ""
                foxArray[(INT)i, 11] := ""
                foxArray[(INT)i, 12] := ""
                foxArray[(INT)i, 13] := ""
                foxArray[(INT)i, 14] := ""
                foxArray[(INT)i, 15] := ""
                foxArray[(INT)i, 16] := ""
                foxArray[(INT)i, 17] := 0
                foxArray[(INT)i, 18] := 0
            ENDIF
        NEXT
    ELSE
        THROW ArgumentException{__VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))}
    ENDIF

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
