//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
using System.Data.Common
USING XSharp.VFP
USING System.Reflection

BEGIN NAMESPACE XSharp.VFP


/// <summary>
/// This enum emulates the FoxPro defines for the CursorAdapter.WhereType property
/// </summary>
ENUM SqlWhereType
    MEMBER DB_KEY := 1
    MEMBER DB_KEYANDUPDATABLE := 2
    MEMBER DB_KEYANDMODIFIED  := 3
    MEMBER DB_KEYANDTIMESTAMP := 5
END ENUM

/// <summary>
/// This enum emulates the FoxPro values for the CursorAdapter.CursorStatus
/// </summary>
ENUM CursorStatus
    MEMBER Detached := 0
    MEMBER Filled   := 1
    MEMBER Attached := 2
END ENUM

[Flags];
ENUM CursorFlags
    MEMBER None                     := 0x00000
    MEMBER AllowDelete              := 0x00001
    MEMBER AllowInsert              := 0x00002
    MEMBER AllowSimultaneousFetch   := 0x00004
    MEMBER AllowUpdate              := 0x00008
    MEMBER BreakOnError             := 0x00010
    MEMBER CompareMemo              := 0x00020
    MEMBER FetchAsNeeded            := 0x00040
    MEMBER FetchMemo                := 0x00080
    MEMBER MapBinary                := 0x00100
    MEMBER MapVarchar               := 0x00200
    MEMBER NoData                   := 0x00400
    MEMBER Prepared                 := 0x00800
    MEMBER RefreshTimeStamp         := 0x01000
    MEMBER SendUpdates              := 0x02000
    MEMBER UseCursorSchema          := 0x04000
    MEMBER UseDeDataSource          := 0x08000
    MEMBER UseTransactions          := 0x10000
END ENUM
/// <include file="VFPClasses.xml" path="doc/CursorAdapter/*" />
CLASS CursorAdapter

    #region Flags
    private _Flags                          as CursorFlags
    private _oRdd                           as XSharp.RDD.IRdd

    private method _GetFlag(flag as CursorFlags) as logic => _Flags:HasFlag(flag)
    private method _SetFlag(flag as CursorFlags, lSet as logic) as void
        if lSet
            _Flags |= flag
        else
            _Flags &= _NOT(flag)
        endif
        return
    #endregion

    #region Fields
    private _DataSourceType := "" as string
    private _DataSource     := NULL as Object
    private _Connection     as DbConnection
    #endregion

    #region validation
    method _ValidateDataSource as VOID
        switch _DataSourceType:ToUpper()
        case "ODBC"
            if _DataSource IS LONG
                var oStmt := SQLSupport.FindStatement( (long) _DataSource)
                if oStmt != NULL
                    _Connection := oStmt:Connection:NetConnection
                endif
            elseif _DataSource IS IDbConnectionClient var oClient
                _Connection := oClient:DbConnection
            else
                throw ArgumentException{i"Invalid value: {_DataSource}","DataSource"}
            endif
        case "ADO"
        case "SQL"
            nop
        case "NATIVE"
        case ""
            _DataSource := NULL
        end switch
        return
    #endregion

    #region properties

    property DbConnection                   as DbConnection get _Connection

    [Obsolete];
    property ADOCodePage                    as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.Alias/*" />
    property Alias                          as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.AllowDelete/*" />
    property AllowDelete                    as logic ;
        GET _GetFlag(CursorFlags.AllowDelete) ;
        SET _SetFlag(CursorFlags.AllowDelete, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.AllowInsert/*" />
    property AllowInsert                    as logic ;
        GET _GetFlag(CursorFlags.AllowInsert) ;
        SET _SetFlag(CursorFlags.AllowInsert, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.AllowSimultaneousFetch/*" />
    property AllowSimultaneousFetch         as logic ;
        GET _GetFlag(CursorFlags.AllowSimultaneousFetch) ;
        SET _SetFlag(CursorFlags.AllowSimultaneousFetch, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.AllowUpdate/*" />
    property AllowUpdate                    as logic ;
        GET _GetFlag(CursorFlags.AllowUpdate) ;
        SET _SetFlag(CursorFlags.AllowUpdate, value)


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.BatchUpdateCount/*" />
    property BatchUpdateCount               as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.BreakOnError/*" />
    property BreakOnError                   as logic ;
        GET _GetFlag(CursorFlags.BreakOnError) ;
        SET _SetFlag(CursorFlags.BreakOnError, value)


    property BufferModeOverride             as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.CompareMemo/*" />
    property CompareMemo                    as logic ;
        GET _GetFlag(CursorFlags.CompareMemo) ;
        SET _SetFlag(CursorFlags.CompareMemo, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.ConflictCheckCmd/*" />
    property ConflictCheckCmd               as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.ConflictCheckType/*" />
    property ConflictCheckType              as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.ConversionFunc/*" />
    property ConversionFunc                 as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.CursorSchema/*" />
    property CursorSchema                   as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.CursorSchema/*" />
    property CursorStatus                   as CursorStatus auto


    property DataSource                     as Object
        get
            return _DataSource
        end get
        set
            _DataSource := value
            _ValidateDataSource()
        end set
    end property

    property DataSourceType                 as string
        get
            return _DataSourceType
        end get
        set
            value := Upper(value)
            switch value
            case "ODBC"
            case "NATIVE"
            case "ADO"
            case "SQL"
            case "XML"
            case ""
                _DataSourceType := value
            otherwise
                throw ArgumentException{i"Invalid value: {value}","DataSourceType"}
            end switch
        end set
    end property

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.DeleteCmd/*" />
    property DeleteCmd as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.DeleteCmdDataSource/*" />
    [Obsolete];
    property DeleteCmdDataSource            as Object get DataSource

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.DeleteCmdDataSourcetype/*" />
    [Obsolete];
    property DeleteCmdDataSourcetype        as string get DataSourceType

    property FetchAsNeeded                  as logic ;
        GET _GetFlag(CursorFlags.FetchAsNeeded) ;
        SET _SetFlag(CursorFlags.FetchAsNeeded, value)


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.FetchMemo/*" />
    property FetchMemo                      as logic ;
        GET _GetFlag(CursorFlags.FetchMemo) ;
        SET _SetFlag(CursorFlags.FetchMemo, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.FetchMemoCmdList/*" />
    property FetchMemoCmdList               as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.FetchMemoDataSource/*" />
    [Obsolete];
    property FetchMemoDataSource            as Object get DataSource

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.FetchMemoDataSourcetype/*" />
    [Obsolete];
    property FetchMemoDataSourcetype        as string get DataSourceType

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.FetchSize/*" />
    property FetchSize                      as long auto

    property Flags                          as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmd/*" />
    property InsertCmd as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmdDataSource/*" />
    [Obsolete];
    property InsertCmdDataSource            as Object get DataSource

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmdDataSourcetype/*" />
    [Obsolete];
    property InsertCmdDataSourcetype        as string get DataSourceType

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmdRefreshCmd/*" />
    property InsertCmdRefreshCmd            as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmdRefreshFieldList/*" />
    property InsertCmdRefreshFieldList      as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.InsertCmdRefreshKeyFieldList/*" />
    property InsertCmdRefreshKeyFieldList   as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.KeyFieldList/*" />
    property KeyFieldList                   as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.MapBinary/*" />
    property MapBinary                      as logic ;
        GET _GetFlag(CursorFlags.MapBinary) ;
        SET _SetFlag(CursorFlags.MapBinary, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.MapVarchar/*" />
    property MapVarchar                       as logic ;
        GET _GetFlag(CursorFlags.MapVarchar) ;
        SET _SetFlag(CursorFlags.MapVarchar, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.MaxRecords/*" />
    property MaxRecords                       as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.Name/*" />
    property Name                            as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.NoData/*" />
    property NoData                          as logic ;
        GET _GetFlag(CursorFlags.NoData) ;
        SET _SetFlag(CursorFlags.NoData, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.Prepared/*" />
    property Prepared                        as logic ;
        GET _GetFlag(CursorFlags.Prepared) ;
        SET _SetFlag(CursorFlags.Prepared, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshAlias/*" />
    property RefreshAlias                   as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshCmd/*" />
    property RefreshCmd                     as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshCmdDataSource/*" />
    [Obsolete];
    property RefreshCmdDataSource           as Object get DataSource

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshCmdDataSourcetype/*" />
    [Obsolete];
    property RefreshCmdDataSourcetype       as string get DataSourceType

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshIgnoreFieldList/*" />
    property RefreshIgnoreFieldList         as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.RefreshTimeStamp/*" />
    property RefreshTimeStamp               as logic ;
        GET _GetFlag(CursorFlags.RefreshTimeStamp) ;
        SET _SetFlag(CursorFlags.RefreshTimeStamp, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.SelectCmd/*" />
    property SelectCmd                      as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.SendUpdates/*" />
    property SendUpdates                    as logic ;
        GET _GetFlag(CursorFlags.SendUpdates) ;
        SET _SetFlag(CursorFlags.SendUpdates, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.Tables/*" />
    property Tables                         as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.Tag/*" />
    property Tag                            as object auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.TimeStampFieldList/*" />
    property TimeStampFieldList             as string auto


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdatableFieldList /*" />
    property UpdatableFieldList             as string auto


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmdDataSource/*" />
    property UpdateCmd as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmd/*" />
    [Obsolete];
    property UpdateCmdDataSource            as Object get DataSource

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmdDataSourcetype/*" />
    [Obsolete];
    property UpdateCmdDataSourcetype        as string get DataSourceType

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmdRefreshCmd/*" />
    property UpdateCmdRefreshCmd            as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmdRefreshFieldList/*" />
    property UpdateCmdRefreshFieldList      as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateCmdRefreshKeyFieldList/*" />
    property UpdateCmdRefreshKeyFieldList   as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateGram/*" />
    property UpdateGram                     as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateGramSchemaLocation/*" />
    property UpdateGramSchemaLocation       as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateNameList/*" />
    property UpdateNameList                 as string auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UpdateType/*" />
    property UpdateType                     as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UseCursorSchema/*" />
    property UseCursorSchema                as logic ;
        GET _GetFlag(CursorFlags.UseCursorSchema) ;
        SET _SetFlag(CursorFlags.UseCursorSchema, value)


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UseDeDataSource/*" />
    property UseDeDataSource                as logic ;
        GET _GetFlag(CursorFlags.UseDeDataSource) ;
        SET _SetFlag(CursorFlags.UseDeDataSource, value)


    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UseMemoSize/*" />
    property UseMemoSize                    as long auto

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.UseTransactions/*" />
    property UseTransactions                as logic ;
        GET _GetFlag(CursorFlags.UseTransactions) ;
        SET _SetFlag(CursorFlags.UseTransactions, value)

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.WhereType/*" />
    property WhereType                      as SqlWhereType auto

#endregion

    CONSTRUCTOR()
        SELF:BatchUpdateCount   := 1
        SELF:FetchSize          := 100
        SELF:MaxRecords         := -1
        SELF:SendUpdates        := TRUE
        SELF:UpdateType         := 1  // Update old data with new data. (Default)
        SELF:WhereType          := SqlWhereType.DB_KEY
        RETURN

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.CursorFill/*" />

    METHOD CursorFill(lUseCursorSchema, lNoData, nOptions, Source) AS LOGIC CLIPPER
        local useCursorSchema := SELF:UseCursorSchema AS LOGIC
        local noData := SELF:NoData AS LOGIC
        IF !IsNil(lUseCursorSchema)
            useCursorSchema := lUseCursorSchema
        ENDIF
        IF !IsNil(lNoData)
            noData := lNoData
        ENDIF
        // nOptions and Source are optional
        SWITCH SELF:DataSourceType
        CASE "ODBC"
            return CursorFillODBC(useCursorSchema, noData)
        CASE "ADO"
            return CursorFillAdo(useCursorSchema, noData)
        CASE "SQL"
            return CursorFillSQL(useCursorSchema, noData)
        END SWITCH
        RETURN FALSE

        PRIVATE METHOD CursorFillODBC(lUseCursorSchema AS LOGIC, lNoData AS LOGIC) as LOGIC
            var cmd := SELF:SelectCmd
            var nResult := SqlFunctions.SqlExec((LONG) SELF:DataSource,cmd,Alias,NULL_ARRAY)
            if nResult > 0
                local oResult as object
                oResult := DbInfo(DBI_RDD_OBJECT)
                if (oResult is XSharp.RDD.IRdd var ir)
                    SELF:_oRdd := ir
                    if oResult IS XSharp.RDD.Workarea var oRDD
                        if !SELF:AllowUpdate .and. ! SELF:AllowDelete .and. ! SELF:AllowInsert
                            if ! oRDD:ReadOnly
                                SQLReflection.SetFieldValue(oRDD,"_ReadOnly", TRUE)
                            endif
                        endif
                    endif
                    XSharp.RuntimeState.Workareas:SetCargo(ir:Area, SELF)
                endif
            endif
            RETURN nResult > 0
        PRIVATE METHOD CursorFillAdo(lUseCursorSchema AS LOGIC, lNoData AS LOGIC) as LOGIC
            RETURN TRUE
        PRIVATE METHOD CursorFillSQL(lUseCursorSchema AS LOGIC, lNoData AS LOGIC) as LOGIC
            RETURN TRUE

    /// <include file="VFPClasses.xml" path="doc/CursorAdapter.CursorAttach/*" />
    METHOD CursorAttach(cAlias, lInheritCursorProperties  ) AS LOGIC CLIPPER
        local lcAlias := SELF:Alias AS STRING
        local lInherit := FALSE AS LOGIC
        local lOk  := FALSE as LOGIC
        IF ! IsNil(cAlias)
            lcAlias  := cAlias
        ENDIF
        IF ! IsNil(lInheritCursorProperties)
            lInherit := lInheritCursorProperties
        ENDIF
        local nArea    := VoDbGetSelect(lcAlias) as dword
        if nArea != 0
            local cargo :=  XSharp.RuntimeState.Workareas:GetCargo(nArea) as object
            if cargo is CursorAdapter var ca .and. ca != SELF
                lOk := FAlSE
            else
                XSharp.RuntimeState.Workareas:SetCargo(nArea, SELF)
                lOk := TRUE
            endif
            if lInherit
                NOP
            endif
        else
            lOk := FALSE
        endif
        RETURN lOk




END CLASS
END NAMESPACE // XSharp.RDD
