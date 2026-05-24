//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

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
