// CallbackMetaDataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:54:13 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlMetadataProviderCallBack class. Provides metadata for the RDD from a callback function.
/// </summary>
CLASS SqlMetadataProviderCallBack Inherit SqlMetadataProviderAbstract
    private const DefaultSection := "Defaults" as string
    private hasDefaults := false as logic
    /// <summary>
    /// Construct a new instance of the SqlMetadataProviderAbstract class.
    /// </summary>
    /// <param name="conn">Connection to which the provider belongs</param>
    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        RETURN

    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            _connection:AllowUpdates        := SELF:GetLogic(DefaultSection, SqlRDDEventReason.AllowUpdates,    _connection:AllowUpdates)
            _connection:CompareMemo         := SELF:GetLogic(DefaultSection, SqlRDDEventReason.CompareMemo,     _connection:CompareMemo )
            _connection:DeletedColumn       := SELF:GetString(DefaultSection, SqlRDDEventReason.DeletedColumn,  _connection:DeletedColumn )
            _connection:LongFieldNames      := SELF:GetLogic(DefaultSection, SqlRDDEventReason.LongFieldNames, _connection:LongFieldNames)
            _connection:MaxRecords          := SELF:GetInt(DefaultSection, SqlRDDEventReason.MaxRecords,        _connection:MaxRecords )
            _connection:RecnoColumn         := SELF:GetString(DefaultSection, SqlRDDEventReason.RecnoColumn,    _connection:RecnoColumn )
            _connection:TrimTrailingSpaces  := SELF:GetLogic(DefaultSection, SqlRDDEventReason.TrimTrailingSpaces, _connection:TrimTrailingSpaces)
            _connection:UpdateAllColumns    := SELF:GetLogic(DefaultSection, SqlRDDEventReason.UpdateAllColumns, _connection:UpdateAllColumns)
            hasDefaults := true
        endif
        RETURN
    END METHOD

    /// <inheritdoc/>
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
        local oTable as SqlDbTableInfo
        ReadDefaults()
        if SELF:FindInCache(cTable, out oTable)
            return oTable
        endif
        oTable := SqlDbTableInfo{cTable, Connection}
        oTable:RealName          := SELF:GetString(cTable, SqlRDDEventReason.RealName, cTable)
        oTable:AllowUpdates      := SELF:GetLogic(cTable, SqlRDDEventReason.AllowUpdates,   _connection:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(cTable, SqlRDDEventReason.DeletedColumn, _connection:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(cTable, SqlRDDEventReason.LongFieldNames, _connection:LongFieldNames)
        oTable:MaxRecords        := SELF:GetInt(cTable, SqlRDDEventReason.MaxRecords,       _connection:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(cTable, SqlRDDEventReason.RecnoColumn,   _connection:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(cTable, SqlRDDEventReason.TrimTrailingSpaces, _connection:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(cTable, SqlRDDEventReason.CompareMemo,     _connection:CompareMemo)
        oTable:UpdateAllColumns  := SELF:GetLogic(DefaultSection, SqlRDDEventReason.UpdateAllColumns, _connection:UpdateAllColumns)

        // these fields have no defaults
        oTable:ServerFilter         := SELF:GetString(cTable, SqlRDDEventReason.ServerFilter,       DEFAULT_SERVERFILTER)
        oTable:ColumnList           := SELF:GetString(cTable, SqlRDDEventReason.ColumnList,         DEFAULT_COLUMNLIST)
        oTable:UpdatableColumns     := SELF:GetString(cTable, SqlRDDEventReason.UpdatableColumns,   DEFAULT_UPDATABLECOLUMNS)
        oTable:KeyColumns           := SELF:GetString(cTable, SqlRDDEventReason.KeyColumns,         DEFAULT_KEYCOLUMNS)

        var cIndexes := SELF:GetString(cTable, SqlRDDEventReason.Indexes, DEFAULT_INDEXES)
        if (!String.IsNullOrEmpty(cIndexes))
            var aIndexes := cIndexes:Split(c",")
            foreach var cIndex in aIndexes
                var oIndexInfo := SELF:GetIndexInfo(oTable, cIndex)
                if (oIndexInfo != null)
                    oTable:Indexes:Add(oIndexInfo)
                endif
            next
        endif
        SELF:AddToCache(cTable, oTable)
        RETURN oTable
    end method

    /// <inheritdoc/>
    OVERRIDE METHOD GetIndexInfo(oTable as SqlDbTableInfo, cIndexName as STRING) AS SqlDbIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlDbIndexInfo{oTable, cIndexName}
        var cTags    := SELF:GetString(cSection, SqlRDDEventReason.Tags, DEFAULT_TAGS)
        if (!String.IsNullOrEmpty(cTags))
            var aTags := cTags:Split(c",")
            foreach var cTag in aTags
                Self:GetTagInfo(oIndex, cTag)
            next
        endif
        return oIndex
    END METHOD
    PROTECTED METHOD GetTagInfo(oIndex as SqlDbIndexInfo, tagName as STRING) AS SqlDbTagInfo
        var cSection := "Tag:"+oIndex:Name+":"+tagName
        var oTag := SqlDbTagInfo{oIndex, tagName}
        oTag:Expression    := SELF:GetString(cSection, SqlRDDEventReason.Expression , DEFAULT_EXPRESSION)
        oTag:Condition     := SELF:GetString(cSection, SqlRDDEventReason.Condition , DEFAULT_CONDITION)
        oTag:Unique        := SELF:GetLogic(cSection, SqlRDDEventReason.Unique , DEFAULT_UNIQUE)
        oIndex:Tags:Add(oTag)
        RETURN oTag
    END METHOD
    private method GetString(cSection as string, nReason as SqlRDDEventReason, cDefault as STRING) AS STRING
        RETURN Connection:RaiseStringEvent(null, nReason, cSection, cDefault)
    END METHOD
    private method GetLogic(cSection as string, nReason as SqlRDDEventReason, lDefault as LOGIC) AS LOGIC
        RETURN Connection:RaiseLogicEvent(null, nReason, cSection, lDefault)
    END METHOD
    private method GetInt(cSection as string, nReason as SqlRDDEventReason, nDefault as INT) AS INT
        RETURN Connection:RaiseIntEvent(null, nReason, cSection, nDefault)
    END METHOD
END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
