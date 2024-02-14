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
/// The CallbackMetaDataProvider class. Provides metadata for the RDD from a callback function.
/// </summary>
CLASS CallBackMetaDataProvider Inherit AbstractMetaDataProvider
    private const DefaultSection := "Defaults" as string
    private hasDefaults := false as logic
    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        RETURN

    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            LongFieldNames      := SELF:GetLogic(DefaultSection, SqlRDDEventReason.LongFieldNames, TRUE)
            AllowUpdates        := SELF:GetLogic(DefaultSection, SqlRDDEventReason.AllowUpdates, TRUE)
            MaxRecords          := SELF:GetInt(DefaultSection, SqlRDDEventReason.MaxRecords, 1000)
            RecnoColumn         := SELF:GetString(DefaultSection, SqlRDDEventReason.RecnoColumn, "")
            DeletedColumn       := SELF:GetString(DefaultSection, SqlRDDEventReason.DeletedColumn, "")
            TrimTrailingSpaces  := SELF:GetLogic(DefaultSection, SqlRDDEventReason.TrimTrailingSpaces, TRUE)
            CompareMemo         := SELF:GetLogic(DefaultSection, SqlRDDEventReason.CompareMemo, TRUE)
            hasDefaults := true
        endif
        RETURN
    END METHOD

    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        ReadDefaults()
        oTable := SqlTableInfo{cTable, Connection}
        oTable:RealName          := SELF:GetString(cTable, SqlRDDEventReason.RealName, cTable)
        oTable:AllowUpdates      := SELF:GetLogic(cTable, SqlRDDEventReason.AllowUpdates,  SELF:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(cTable, SqlRDDEventReason.DeletedColumn, SELF:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(cTable, SqlRDDEventReason.LongFieldNames, SELF:LongFieldNames)
        oTable:MaxRecords        := SELF:GetInt(cTable, SqlRDDEventReason.MaxRecords,       SELF:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(cTable, SqlRDDEventReason.RecnoColumn,   SELF:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(cTable, SqlRDDEventReason.TrimTrailingSpaces, SELF:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(cTable, SqlRDDEventReason.CompareMemo,     SELF:CompareMemo)
        oTable:ServerFilter      := SELF:GetString(cTable, SqlRDDEventReason.ServerFilter, "")

        // these fields have no defaults
        oTable:ColumnList           := SELF:GetString(cTable, SqlRDDEventReason.ColumnList, "*")
        oTable:UpdatableColumns     := SELF:GetString(cTable, SqlRDDEventReason.UpdatableColumns, "*")
        oTable:KeyColumns           := SELF:GetString(cTable, SqlRDDEventReason.KeyColumns, "*")

        var cIndexes := SELF:GetString(cTable, SqlRDDEventReason.Indexes, "")
        if (!String.IsNullOrEmpty(cIndexes))
            var aIndexes := cIndexes:Split(c",")
            foreach var cIndex in aIndexes
                var oIndexInfo := SELF:GetIndexInfo(oTable, cIndex)
                if (oIndexInfo != null)
                    oTable:Indexes:Add(oIndexInfo)
                endif
            next
        endif

        RETURN oTable
    end method

    OVERRIDE METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        var cTags    := SELF:GetString(cSection, SqlRDDEventReason.Tags, "")
        if (!String.IsNullOrEmpty(cTags))
            var aTags := cTags:Split(c",")
            foreach var cTag in aTags
                Self:GetTagInfo(oIndex, cTag)
            next
        endif
        return oIndex
    END METHOD
    PROTECTED METHOD GetTagInfo(oIndex as SqlIndexInfo, tagName as STRING) AS SqlIndexTagInfo
        var cSection := "Tag:"+oIndex:Name+":"+tagName
        var oTag := SqlIndexTagInfo{oIndex, tagName}
        oTag:Expression    := SELF:GetString(cSection, SqlRDDEventReason.Expression , "")
        oTag:Condition     := SELF:GetString(cSection, SqlRDDEventReason.Condition , "")
        oTag:Unique        := SELF:GetLogic(cSection, SqlRDDEventReason.Unique , FALSE)
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
