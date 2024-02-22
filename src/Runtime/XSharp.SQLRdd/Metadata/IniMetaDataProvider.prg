// IniMetaDataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:33:13 AM
// Created for   :
// WorkStation   : LEDA

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers
/// <summary>
/// The IniMetaDataProvider class. Provides metadata for the RDD from an INI file
/// </summary>
CLASS IniMetaDataProvider Inherit AbstractMetaDataProvider
    protect _fileName as STRING
    protect _ini      as IniFile
    private const DefaultSection := "Defaults" as string
    private hasDefaults as LOGIC

    CONSTRUCTOR(conn as SqlDbConnection)
        SELF("SQLRDD.INI", conn)
    END CONSTRUCTOR
    CONSTRUCTOR(cFile as STRING, conn as SqlDbConnection)
        SUPER(conn)
        _fileName := cFile
        _ini      := IniFile{_fileName}
        RETURN
    END CONSTRUCTOR
    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            hasDefaults := true
            LongFieldNames      := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.LongFieldNames), _connection:UseLongNames)
            TrimTrailingSpaces  := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.TrimTrailingSpaces), _connection:TrimTrailingSpaces)
            UpdateAllColumns    := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.UpdateAllColumns), FALSE)
            AllowUpdates        := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.AllowUpdates), TRUE)
            MaxRecords          := SELF:GetInt(DefaultSection,    nameof(SqlRDDEventReason.MaxRecords), 1000)
            RecnoColumn         := SELF:GetString(DefaultSection, nameof(SqlRDDEventReason.RecnoColumn), "")
            DeletedColumn       := SELF:GetString(DefaultSection, nameof(SqlRDDEventReason.DeletedColumn), "")
            CompareMemo         := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.CompareMemo), TRUE)
            ENDIF
        RETURN
    END METHOD

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        ReadDefaults()
        if SELF:FindInCache(cTable, out oTable)
            return oTable
        endif
        oTable := SqlTableInfo{cTable, Connection}
        oTable:RealName          := SELF:GetString(cTable,  nameof(SqlRDDEventReason.RealName),      cTable)
        oTable:AllowUpdates      := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.AllowUpdates),  SELF:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(cTable,  nameof(SqlRDDEventReason.DeletedColumn), SELF:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.LongFieldNames),SELF:LongFieldNames)
        oTable:UpdateAllColumns  := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.UpdateAllColumns),SELF:UpdateAllColumns)
        oTable:MaxRecords        := SELF:GetInt(cTable,     nameof(SqlRDDEventReason.MaxRecords),    SELF:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(cTable,  nameof(SqlRDDEventReason.RecnoColumn),   SELF:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.TrimTrailingSpaces), SELF:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.CompareMemo),   SELF:CompareMemo)

        // these fields have no defaults
         oTable:ServerFilter         := SELF:GetString(cTable, nameof(SqlRDDEventReason.ServerFilter), "")
         oTable:ColumnList           := SELF:GetString(cTable, nameof(SqlRDDEventReason.ColumnList), "*")
         oTable:UpdatableColumns     := SELF:GetString(cTable, nameof(SqlRDDEventReason.UpdatableColumns), "*")
         oTable:KeyColumns           := SELF:GetString(cTable, nameof(SqlRDDEventReason.KeyColumns), "*")

        var cIndexes := SELF:GetString(cTable, nameof(SqlRDDEventReason.Indexes), "")
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
    END METHOD

    /// <inheritdoc />
    OVERRIDE METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        var cTags    := SELF:GetString(cSection, nameof(SqlRDDEventReason.Tags), "")
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
        oTag:Expression    := SELF:GetString(cSection, nameof(SqlRDDEventReason.Expression ), "")
        oTag:Condition     := SELF:GetString(cSection, nameof(SqlRDDEventReason.Condition ), "")
        oTag:Unique        := SELF:GetLogic(cSection, nameof(SqlRDDEventReason.Unique ), FALSE)
        oIndex:Tags:Add(oTag)
        RETURN oTag
    END METHOD

    private method GetString(cSection as string, cKey as string, cDefault as STRING) AS STRING
        RETURN _ini:GetString(cSection, cKey, cDefault)
    END METHOD
    private method GetLogic(cSection as string, cKey as string, lDefault as LOGIC) AS LOGIC
        RETURN _ini:GetLogic(cSection, cKey, lDefault)
    END METHOD
    private method GetInt(cSection as string, cKey as string, nDefault as INT) AS INT
        RETURN _ini:GetInt(cSection, cKey, nDefault)
    END METHOD

END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata

