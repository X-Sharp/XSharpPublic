// AbstractMetaDataProvider.prg
// Created by    : robert
// Creation Date : 2/7/2024 4:13:25 PM
// Created for   :
// WorkStation   : LEDA

using XSharp.RDD.Enums
using XSharp.RDD.Support
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlMetadataProviderAbstract class.
/// </summary>
ABSTRACT CLASS SqlMetadataProviderAbstract IMPLEMENTS ISqlMetadataProvider
    private hasDefaults := false as logic
    protected const DefaultSection := "Defaults" as string
    protected _cache as Dictionary<string, SqlDbTableInfo>
    protected _connection AS SqlDbConnection
    /// <summary>
    /// Construct a new instance of the SqlMetadataProviderAbstract class.
    /// </summary>
    /// <param name="conn">Connection to which the provider belongs</param>
    CONSTRUCTOR(conn as SqlDbConnection)
        self:_connection := conn
        self:_cache      := Dictionary<string, SqlDbTableInfo>{StringComparer.OrdinalIgnoreCase}

        RETURN

#region Properties
    /// <summary>
    ///  The connection that this metadata provider is using.
    /// </summary>
    PROPERTY Connection AS SqlDbConnection GET _connection

#endregion
    /// <inheritdoc />
    ABSTRACT METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
    /// <inheritdoc />
    ABSTRACT METHOD GetIndexInfo(oTable AS SqlDbTableInfo, cIndex AS STRING) AS SqlDbIndexInfo

    /// <exclude />
    PROTECTED INTERNAL METHOD FindInCache( cTable as STRING, oTable OUT SqlDbTableInfo) AS LOGIC
        RETURN _cache:TryGetValue(cTable, out oTable)
    /// <exclude />
    PROTECTED INTERNAL METHOD AddToCache( cTable as STRING, oTable AS SqlDbTableInfo) AS VOID
        _cache:Add(cTable, oTable)
        RETURN

    INTERNAL CONST DEFAULT_COLUMNLIST := "*" AS STRING
    INTERNAL CONST DEFAULT_CONDITION:= "" AS STRING
    INTERNAL CONST DEFAULT_EXPRESSION:= "" AS STRING
    INTERNAL CONST DEFAULT_INDEXES:= "" AS STRING
    INTERNAL CONST DEFAULT_KEYCOLUMNS := "*" AS STRING
    INTERNAL CONST DEFAULT_SERVERFILTER := "" AS STRING
    INTERNAL CONST DEFAULT_TAGS:= "" AS STRING
    INTERNAL CONST DEFAULT_UNIQUE:= FALSE AS LOGIC
    INTERNAL CONST DEFAULT_UPDATABLECOLUMNS:= "*" AS STRING
    /// <inheritdoc />
    /// <remarks>The abstract implementation does nothing.</remarks>
    VIRTUAL METHOD CreateTable(cTable AS STRING, info as DbOpenInfo) AS VOID
        RETURN
    /// <inheritdoc />
    /// <remarks>The abstract implementation does nothing.</remarks>
    VIRTUAL METHOD CreateIndex(cTable as String, orderInfo as DbOrderCreateInfo) AS VOID
        RETURN

    ABSTRACT Method GetString(oPar as OBJECT , nReason as SqlRDDEventReason, cDefault as STRING) as STRING
    ABSTRACT Method GetLogic(oPar as OBJECT , nReason as SqlRDDEventReason, lDefault as LOGIC) as LOGIC
    ABSTRACT Method GetInt(oPar as OBJECT , nReason as SqlRDDEventReason, iDefault as LONG) as LONG

    METHOD GetDefaults(oPar as OBJECT) AS VOID
        IF ! hasDefaults
            _connection:LongFieldNames      := SELF:GetLogic(oPar,  SqlRDDEventReason.LongFieldNames, _connection:LongFieldNames)
            _connection:TrimTrailingSpaces  := SELF:GetLogic(oPar,  SqlRDDEventReason.TrimTrailingSpaces, _connection:TrimTrailingSpaces)
            _connection:UpdateAllColumns    := SELF:GetLogic(oPar,  SqlRDDEventReason.UpdateAllColumns, _connection:UpdateAllColumns)
            _connection:AllowUpdates        := SELF:GetLogic(oPar,  SqlRDDEventReason.AllowUpdates, _connection:AllowUpdates)
            _connection:MaxRecords          := SELF:GetInt(oPar,    SqlRDDEventReason.MaxRecords, _connection:MaxRecords)
            _connection:RecnoColumn         := SELF:GetString(oPar, SqlRDDEventReason.RecnoColumn, _connection:RecnoColumn)
            _connection:DeletedColumn       := SELF:GetString(oPar, SqlRDDEventReason.DeletedColumn, _connection:DeletedColumn)
            _connection:CompareMemo         := SELF:GetLogic(oPar,  SqlRDDEventReason.CompareMemo, _connection:CompareMemo)
            _connection:MaxRecnoAsRecCount  := SELF:GetLogic(oPar,  SqlRDDEventReason.MaxRecnoAsRecCount, _connection:MaxRecnoAsRecCount)
            _connection:SeekReturnsSubset   := SELF:GetLogic(oPar,  SqlRDDEventReason.SeekReturnsSubset, _connection:SeekReturnsSubset)
            hasDefaults := TRUE
        ENDIF
        RETURN
    END METHOD


    METHOD ReadTable(oTable as SqlDbTableInfo,oPar as OBJECT ) AS SqlDbTableInfo
        local cTable := oTable:Name as STRING
        oTable:RealName          := SELF:GetString(oPar,  SqlRDDEventReason.RealName,      cTable)
        oTable:AllowUpdates      := SELF:GetLogic(oPar,   SqlRDDEventReason.AllowUpdates,  _connection:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(oPar,  SqlRDDEventReason.DeletedColumn, _connection:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(oPar,   SqlRDDEventReason.LongFieldNames,_connection:LongFieldNames)
        oTable:UpdateAllColumns  := SELF:GetLogic(oPar,   SqlRDDEventReason.UpdateAllColumns,_connection:UpdateAllColumns)
        oTable:MaxRecords        := SELF:GetInt(oPar,     SqlRDDEventReason.MaxRecords,    _connection:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(oPar,  SqlRDDEventReason.RecnoColumn,   _connection:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(oPar,   SqlRDDEventReason.TrimTrailingSpaces, _connection:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(oPar,   SqlRDDEventReason.CompareMemo,   _connection:CompareMemo)
        oTable:MaxRecnoAsRecCount:= SELF:GetLogic(oPar,   SqlRDDEventReason.MaxRecnoAsRecCount,   _connection:MaxRecnoAsRecCount)
        // these fields have no connection level defaults
        oTable:ServerFilter      := SELF:GetString(oPar, SqlRDDEventReason.ServerFilter, DEFAULT_SERVERFILTER)
        oTable:ColumnList        := SELF:GetString(oPar, SqlRDDEventReason.ColumnList, DEFAULT_COLUMNLIST)
        oTable:UpdatableColumns  := SELF:GetString(oPar, SqlRDDEventReason.UpdatableColumns, DEFAULT_UPDATABLECOLUMNS)
        oTable:KeyColumns        := SELF:GetString(oPar, SqlRDDEventReason.KeyColumns, DEFAULT_KEYCOLUMNS)

        return oTable
    END METHOD

END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
