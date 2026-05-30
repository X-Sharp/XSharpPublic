//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD

INTERNAL FUNCTION _DoInArea<T>(uArea as Usual, action as @@Func<T>, defaultValue as T, cFunction as STRING, nArg as DWORD) as T
    IF IsNil(uArea)
        RETURN (T) IIF(Used(), action(), defaultValue)
    ELSEIF IsNumeric(uArea)
        RETURN (T) (uArea)->(IIF(Used(), action(), defaultValue))
    ENDIF
    VAR curArea := RuntimeState.CurrentWorkarea
    VAR newArea := VoDb.SymSelect(uArea)
    RuntimeState.CurrentWorkarea := curArea
    IF newArea == 0
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, cFunction, nameof(uArea), nArg, <OBJECT>{uArea}  )
    ENDIF
    RETURN (T) (newArea)->(action())

// The last 2 params in the function calls below determine the error message generated when the uArea parameter
// is an non existing alias
// The number should match the position of the uArea parameter in the parameter list of the original parameters list
//

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cdx/*" />
[FoxProFunction("CDX", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION  Cdx (nIndexNumber , uArea)  AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME, NIL, nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/cpdbf/*" />
[FoxProFunction("CPDBF", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION CpDbf( uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbInfo(DBI_CODEPAGE) } , 0,__FUNCTION__,1)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/descending/*" />
[FoxProFunction("DESCENDING", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Descending( uIndex, uArea) AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbOrderInfo(DBOI_ISDESC, NIL, uIndex) } , FALSE,__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/field/*" />
[FoxProFunction("FIELD", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Field( uField , uArea, nFlag) AS STRING CLIPPER
    LOCAL nInfo as LONG
    @@Default(@nFlag, 0)
    IF nFlag == 1
        nInfo := DBS_CAPTION
    ELSE
        nInfo := DBS_NAME
    ENDIF
    IF IsString(uField)
        RETURN _DoInArea(uArea, { => (STRING) DbFieldInfo(nInfo, uField) } , "",__FUNCTION__,2)
    ELSEIF IsNumeric(uField)
        RETURN _DoInArea(uArea, { => (STRING) DbFieldInfo(nInfo, uField) } , "",__FUNCTION__,2)
    ELSE
        RETURN ""
    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/filter/*" />
[FoxProFunction("FILTER", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Filter( uArea ) AS USUAL CLIPPER
    RETURN _DoInArea(uArea, { => (STRING) DbInfo(DBI_DBFILTER) } , "",__FUNCTION__,1)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/fldcount/*" />
[FoxProFunction("FLDCOUNT", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION FldCount( ) AS USUAL CLIPPER
    RETURN FCount()

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/for/*" />
[FoxProFunction("FOR", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION For( nIndexNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_CONDITION, NIL, nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/idxcollate/*" />
[FoxProFunction("IDXCOLLATE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IdxCollate( uIndex, nIndex, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_COLLATION, uIndex, nIndex) } , "",__FUNCTION__,3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isflocked/*" />
[FoxProFunction("ISFLOCKED", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IsFlocked( uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbInfo(DBI_ISFLOCK) } , FALSE,__FUNCTION__,1)

/// <include file="VFPDocs.xml" path="Runtimefunctions/isexclusive/*" />
[FoxProFunction("ISEXCLUSIVE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IsExclusive( uArea, nType) AS LOGIC CLIPPER
    IF !IsNil(nType) .AND. (INT) nType == 2
        RETURN FALSE
    ENDIF

    LOCAL nArea := _AreaFromParam(uArea) AS DWORD
    IF nArea == 0
        RETURN FALSE
    ENDIF

    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea

    VAR lResult := !(LOGIC) DbInfo(DBI_SHARED)
    RuntimeState.CurrentWorkarea := nOldArea
    RETURN lResult

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isreadonly/*" />
[FoxProFunction("ISREADONLY", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IsReadOnly( uArea ) AS LOGIC CLIPPER
    // nWorkArea = 0 means current DBF - nor supported yet
    IF !IsNil(uArea) .AND. IsNumeric(uArea) .AND. (INT) uArea == 0
        RETURN FALSE
    ENDIF

    LOCAL nArea := _AreaFromParam(uArea) AS DWORD

    IF nArea == 0
        RETURN FALSE
    ENDIF

    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea

    VAR lResult := (LOGIC) DbInfo(DBI_READONLY)
    RuntimeState.CurrentWorkarea := nOldArea

    RETURN lResult

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/isrlocked/*" />
[FoxProFunction("ISRLOCKED", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IsRlocked( nRecordNumber, uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbRecordInfo(DBRI_LOCKED, nRecordNumber) } , FALSE,__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/key/*" />
[FoxProFunction("KEY", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Key( uIndex, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_EXPRESSION,NIL ,uIndex) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdx/*" />
[FoxProFunction("MDX", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Mdx( nIndexNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME,NIL ,nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ndx/*" />
[FoxProFunction("NDX", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Ndx( nIndexNumber , uArea ) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_INDEXNAME,NIL ,nIndexNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/order/*" />
[FoxProFunction("ORDER", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Order( uArea, nPath) AS STRING CLIPPER
    IF IsNumeric(nPath)
        RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_FULLPATH, 0) } , "",__FUNCTION__,1)
    ENDIF
    RETURN _DoInArea(uArea, { => (STRING) DbOrderInfo(DBOI_NAME, 0) } , "",__FUNCTION__,1)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/relation/*" />
[FoxProFunction("RELATION", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Relation( nRelationNumber , uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { =>  DbRelation(nRelationNumber) } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tag/*" />
[FoxProFunction("TAG", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Tag( CDXFileName, nTagNumber, uArea) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { => (String) DbOrderInfo(DBOI_NAME, CDXFileName, nTagNumber) } , "",__FUNCTION__,3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tagcount/*" />
[FoxProFunction("TAGCOUNT", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION TagCount( CDXFileName , uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_ORDERCOUNT, CDXFileName) } , 0,__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/bagcount/*" />
FUNCTION BagCount( uArea) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_BAGCOUNT) } , 0,__FUNCTION__,1)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/tagno/*" />
[FoxProFunction("TAGNO", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION TagNo( IndexName , CDXFileName , uArea ) AS LONG CLIPPER
    RETURN _DoInArea(uArea, { => (LONG) DbOrderInfo(DBOI_NUMBER, CDXFileName, IndexName) } , 0,__FUNCTION__,3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/target/*" />
[FoxProFunction("TARGET", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION Target( nRelationshipNumber , uArea ) AS STRING CLIPPER
    RETURN _DoInArea(uArea, { =>
        LOCAL nArea AS DWORD
        nArea := DbRSelect(nRelationshipNumber)
        IF nArea != 0
            RETURN Alias(nArea)
        ENDIF
        RETURN ""
        } , "",__FUNCTION__,2)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/unique/*" />
[FoxProFunction("UNIQUE", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION Unique(uArea ) AS LOGIC CLIPPER
    RETURN _DoInArea(uArea, { => (LOGIC) DbOrderInfo(DBOI_UNIQUE , NIL, NIL) } , FALSE,__FUNCTION__,1)


/// <include file="VFPDocs.xml" path="Runtimefunctions/getfldstate/*" />
[FoxProFunction("GETFLDSTATE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION GetFldState(uField, uArea) AS USUAL CLIPPER
    LOCAL nArea := _AreaFromParam(uArea) AS DWORD
    IF nArea == 0
        RETURN NIL
    ENDIF
    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    TRY
        IF !Used()
            RETURN NIL
        ENDIF
        IF Eof()
            RETURN DBNull.Value
        ENDIF
        LOCAL nCount := (int)FCount() AS INT
        IF IsString(uField)
            VAR nFld := CoreDb.CWA(__FUNCTION__):FieldIndex((STRING) uField)
            IF nFld == 0
                RETURN NIL
            ENDIF
            RETURN (INT) _GetFldStateFromCargo(nArea, nFld)
        ELSEIF IsNumeric(uField)
            LOCAL nFldNum := (INT) uField AS INT
            DO CASE
            CASE nFldNum == -1
                VAR sb := System.Text.StringBuilder{}
                sb:Append(_GetFldStateFromCargo(nArea, 0):ToString())
                FOR VAR j := 1 TO nCount
                    sb:Append(_GetFldStateFromCargo(nArea, j):ToString())
                NEXT
                RETURN sb:ToString()
            CASE nFldNum == 0
                RETURN (INT) _GetFldStateFromCargo(nArea, 0)
            CASE nFldNum >= 1 .AND. nFldNum <= nCount
                RETURN (INT) _GetFldStateFromCargo(nArea, nFldNum)
            ENDCASE
        ENDIF
        RETURN NIL
    FINALLY
        RuntimeState.CurrentWorkarea := nOldArea
    END TRY

/// <include file="VFPDocs.xml" path="Runtimefunctions/setfldstate/*" />
[FoxProFunction("SETFLDSTATE", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION SetFldState(uField, nFieldState, uArea) AS LOGIC CLIPPER
    IF IsNil(nFieldState)
        RETURN FALSE
    ENDIF
    LOCAL nState := (INT) nFieldState AS INT
    IF nState < 1 .OR. nState > 4
        RETURN FALSE
    ENDIF
    LOCAL nArea := _AreaFromParam(uArea) AS DWORD
    IF nArea == 0
        RETURN FALSE
    ENDIF
    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    TRY
        IF !Used()
            RETURN FALSE
        ENDIF
        LOCAL nCount := (int)FCount() AS INT
        IF IsString(uField)
            VAR nFld := CoreDb.CWA(__FUNCTION__):FieldIndex((STRING) uField)
            IF nFld == 0
                RETURN FALSE
            ENDIF
            _SetFldStateInCargo(nArea, nFld, (BYTE) nState)
            RETURN TRUE
        ELSEIF IsNumeric(uField)
            LOCAL nFldNum := (INT) uField AS INT
            IF nFldNum == 0
                _SetFldStateInCargo(nArea, 0, (BYTE) nState)
                RETURN TRUE
            ELSEIF nFldNum >= 1 .AND. nFldNum <= nCount
                _SetFldStateInCargo(nArea, nFldNum, (BYTE) nState)
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE
    FINALLY
        RuntimeState.CurrentWorkarea := nOldArea
    END TRY

/// <include file="VFPDocs.xml" path="Runtimefunctions/indexseek/*" />
[FoxProFunction("INDEXSEEK", FoxFunctionCategory.Database, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION IndexSeek( eExpression , lMovePointer , uArea, uIndex) AS LOGIC CLIPPER
    @@Default(@lMovePointer, FALSE)

    LOCAL nArea := _AreaFromParam(uArea) AS DWORD
    IF nArea == 0
        RETURN FALSE
    ENDIF

    VAR nOldArea := RuntimeState.CurrentWorkarea
    VAR lResult := FALSE
    RuntimeState.CurrentWorkarea := nOldArea
    TRY
        VAR cOldOrder := ""
        VAR cOldBag := ""
        VAR lChangeOrder := FALSE

        IF !IsNil(uIndex)
            cOldOrder := OrdName()
            cOldBag := ""
            OrdSetFocus(uIndex)
            lChangeOrder := TRUE
        ENDIF

        LOCAL nOldRecno := 0 AS DWORD
        VAR lWasBof := FALSE

        IF !lMovePointer
            nOldRecno := RecNo()
            lWasBof := Bof()
        ENDIF

        lResult := DbSeek(eExpression)

        IF !lMovePointer
            IF lWasBof
                DbGoTop()
                IF RecCount() > 0
                    DbSkip(-1)
                ENDIF
            ELSE
                DbGoto(nOldRecno)
            ENDIF
        ENDIF

        IF lChangeOrder
            OrdSetFocus(cOldOrder, cOldBag)
        ENDIF
    FINALLY
        RuntimeState.CurrentWorkarea := nOldArea
    END TRY

    RETURN lResult

/// Returns the workarea number for a uArea parameter
/// NIL = current
/// STRING = alias
/// NUMERIC = number
/// Returns 0 if not found / invalid
INTERNAL FUNCTION _AreaFromParam(uArea AS USUAL) AS DWORD
    IF IsNil(uArea)
        RETURN RuntimeState.CurrentWorkarea
    ELSEIF IsString(uArea)
        RETURN RuntimeState.Workareas.FindAlias((STRING) uArea)
    ELSEIF IsNumeric(uArea)
        RETURN (DWORD) uArea
    ENDIF

    RETURN 0

INTERNAL CLASS _WorkareaCargo
    export fldState AS Dictionary<INT, BYTE>
    EXPORT cursorProps AS Dictionary<CursorProperty, OBJECT>

    CONSTRUCTOR()
        fldState := Dictionary<INT, BYTE>{}
        cursorProps := Dictionary<CursorProperty, OBJECT>{}
    END CONSTRUCTOR
END CLASS

INTERNAL FUNCTION _GetWorkareaCargo(nArea AS DWORD) AS _WorkareaCargo
    VAR oCargo := RuntimeState.Workareas:GetCargo(nArea)
    IF oCargo IS _WorkareaCargo VAR cargo
        RETURN cargo
    ENDIF
    VAR newCargo := _WorkareaCargo{}
    RuntimeState.Workareas:SetCargo(nArea, newCargo)
    RETURN newCargo

INTERNAL FUNCTION _GetFldStateFromCargo(nArea AS DWORD, nField AS INT) AS BYTE
    VAR cargo := _GetWorkareaCargo(nArea)
    LOCAL b as BYTE
    if cargo:fldState:TryGetValue(nField, OUT b)
        return b
    ENDIF
    RETURN 1

INTERNAL FUNCTION _SetFldStateInCargo(nArea AS DWORD, nField AS INT, nState AS BYTE) AS VOID
    VAR cargo := _GetWorkareaCargo(nArea)
    cargo:fldState[nField] := nState

INTERNAL STATIC CLASS _CursorPropDefaults
    INTERNAL STATIC _defaults AS Dictionary<CursorProperty, OBJECT>

    STATIC CONSTRUCTOR
        _defaults := Dictionary<CursorProperty, OBJECT>{}
        _defaults:Add(CursorProperty.Buffering, 1)
        _defaults:Add(CursorProperty.AutoIncError, FALSE)
        _defaults:Add(CursorProperty.FetchMemo, FALSE)
        _defaults:Add(CursorProperty.FetchSize, 100)
        _defaults:Add(CursorProperty.MapBinary, FALSE)
        _defaults:Add(CursorProperty.MapVarchar, FALSE)
        _defaults:Add(CursorProperty.MaxRecords, -1)
        _defaults:Add(CursorProperty.Refresh, -2)
        _defaults:Add(CursorProperty.CompareMemo, TRUE)
        _defaults:Add(CursorProperty.FetchAsNeeded, FALSE)
        _defaults:Add(CursorProperty.Prepared, FALSE)
        _defaults:Add(CursorProperty.SendUpdates, FALSE)
        _defaults:Add(CursorProperty.UpdateType, 1)
        _defaults:Add(CursorProperty.WhereType, 3)
        _defaults:Add(CursorProperty.UseMemoSize, 255)
        _defaults:Add(CursorProperty.BatchUpdateCount, 1)
    END CONSTRUCTOR

    INTERNAL STATIC METHOD GetDefault(prop AS CursorProperty) AS OBJECT
        LOCAL result AS OBJECT
        IF _defaults:TryGetValue(prop, OUT result)
            return result
        ENDIF
        RETURN NIL
    END METHOD

    INTERNAL STATIC METHOD SetDefault(prop AS CursorProperty, oValue AS OBJECT) AS VOID
        _defaults[prop] := oValue
    END METHOD
END CLASS

INTERNAL FUNCTION _GetCursorProp(nArea AS DWORD, prop AS CursorProperty) AS OBJECT
    VAR cargo := _GetWorkareaCargo(nArea)
    LOCAL result AS OBJECT
    if cargo:cursorProps:TryGetValue(prop, OUT result)
        RETURN result
    ENDIF

    RETURN _CursorPropDefaults.GetDefault(prop)

INTERNAL FUNCTION _SetCursorProp(nArea AS DWORD, prop AS CursorProperty, oValue AS OBJECT) AS VOID
    LOCAL cargo AS _WorkareaCargo
    cargo := _GetWorkareaCargo(nArea)
    cargo:cursorProps[prop] := oValue


/// <include file="VFPDocs.xml" path="Runtimefunctions/cursorsetprop/*" />
[FoxProFunction("CURSORSETPROP", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION CursorSetProp(cProperty, eExpression, uArea) AS LOGIC CLIPPER
    IF !IsString(cProperty)
        RETURN FALSE
    ENDIF
    VAR cProp := (STRING) cProperty
    VAR nProp := GetCursorProperty(cProp)
    VAR prop := (CursorProperty) nProp
    VAR lSessionDefault := IsNumeric(uArea) .AND. (INT) uArea == 0
    IF lSessionDefault
        IF nProp == (LONG) CursorProperty.Buffering
            IF IsNumeric(eExpression)
                VAR nVal := (INT) eExpression
                IF nVal < 1 .OR. nVal > 5
                    RETURN FALSE
                ENDIF
            ELSE
                RETURN FALSE
            ENDIF
        ENDIF
        _CursorPropDefaults.SetDefault(prop, eExpression)
        RETURN TRUE
    ENDIF
    VAR nArea := _AreaFromParam(uArea)
    IF nArea == 0
        RETURN FALSE
    ENDIF
    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    TRY
        IF !Used()
            RETURN FALSE
        ENDIF
        SWITCH prop
        CASE CursorProperty.Buffering
            IF !IsNumeric(eExpression)
                RETURN FALSE
            ENDIF
            LOCAL nBuff := (INT) eExpression AS INT
            IF nBuff < 1 .OR. nBuff > 5
                RETURN FALSE
            ENDIF
            _SetCursorProp(nArea, CursorProperty.Buffering, nBuff)
            RETURN TRUE
        CASE CursorProperty.AutoIncError
            IF !IsLogic(eExpression)
                RETURN FALSE
            ENDIF
            _SetCursorProp(nArea, CursorProperty.AutoIncError, eExpression)
            RETURN TRUE
        CASE CursorProperty.Refresh
            IF !IsNumeric(eExpression)
                RETURN FALSE
            ENDIF
            _SetCursorProp(nArea, CursorProperty.Refresh, eExpression)
            RETURN TRUE
        OTHERWISE
            _SetCursorProp(nArea, prop, eExpression)
            RETURN TRUE
        END SWITCH
    FINALLY
        RuntimeState.CurrentWorkarea := nOldArea
    END TRY

/// <include file="VFPDocs.xml" path="Runtimefunctions/cursorgetprop/*" />
[FoxProFunction("CURSORGETPROP", FoxFunctionCategory.CursorAndTable, FoxEngine.WorkArea, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION CursorGetProp(cProperty, uArea) AS USUAL CLIPPER
    IF !IsString(cProperty)
        RETURN NIL
    ENDIF
    VAR cProp := (STRING) cProperty
    VAR nProp := GetCursorProperty(cProp)
    VAR prop := (CursorProperty) nProp
    VAR lSessionDefault := IsNumeric(uArea) .AND. (INT) uArea == 0
    IF lSessionDefault
        RETURN _CursorPropDefaults.GetDefault(prop)
    ENDIF
    VAR nArea := _AreaFromParam(uArea)
    IF nArea == 0
        RETURN FALSE
    ENDIF
    VAR nOldArea := RuntimeState.CurrentWorkarea
    RuntimeState.CurrentWorkarea := nArea
    TRY
        IF !Used()
            RETURN FALSE
        ENDIF
        SWITCH prop
        CASE CursorProperty.SourceType
            RETURN 3
        CASE CursorProperty.SourceName
            RETURN DbInfo(DBI_FULLPATH)
        CASE CursorProperty.Database
            RETURN ""
        CASE CursorProperty.SQL
            RETURN ""
        CASE CursorProperty.ConnectHandle
            RETURN 0
        CASE CursorProperty.ConnectName
            RETURN ""
        CASE CursorProperty.Tables
            RETURN ""
        CASE CursorProperty.KeyFieldList
            RETURN ""
        CASE CursorProperty.UpdatableFieldList
            RETURN ""
        CASE CursorProperty.UpdateNameList
            RETURN ""
        CASE CursorProperty.ParameterList
            RETURN ""
        CASE CursorProperty.RecordsFetched
            RETURN -1
        CASE CursorProperty.FetchIsComplete
            RETURN TRUE
        CASE CursorProperty.ADOBookmark
            RETURN NIL
        CASE CursorProperty.ADOCodePage
            RETURN 0
        CASE CursorProperty.ADORecordset
            RETURN NIL
        OTHERWISE
            RETURN _GetCursorProp(nArea, prop)
        END SWITCH
    FINALLY
        RuntimeState.CurrentWorkarea := nOldArea
    END TRY
