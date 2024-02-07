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

    CONSTRUCTOR(conn as SqlDbConnection)
        SELF("SQLRDD.INI", conn)
    END CONSTRUCTOR
    CONSTRUCTOR(cFile as STRING, conn as SqlDbConnection)
        SUPER(conn)
        _fileName := cFile
        _ini      := IniFile{_fileName}
        SELF:ReadDefaults()
        RETURN
    END CONSTRUCTOR
    METHOD ReadDefaults() AS VOID
        LongFieldNames      := SELF:GetLogic(DefaultSection,  nameof(LongFieldNames), TRUE)
        AllowUpdates        := SELF:GetLogic(DefaultSection,  nameof(AllowUpdates), TRUE)
        MaxRecords          := SELF:GetInt(DefaultSection,    nameof(MaxRecords), 1000)
        RecnoColumn         := SELF:GetString(DefaultSection, nameof(RecnoColumn), "")
        DeletedColumn       := SELF:GetString(DefaultSection, nameof(DeletedColumn), "")
        TrimTrailingSpaces  := SELF:GetLogic(DefaultSection,  nameof(TrimTrailingSpaces), TRUE)
        CompareMemo         := SELF:GetLogic(DefaultSection,  nameof(CompareMemo), TRUE)
        RETURN
    END METHOD

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        oTable := SqlTableInfo{cTable, Connection}
        oTable:AllowUpdates      := SELF:GetLogic(cTable,   nameof(AllowUpdates),  SELF:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(cTable,  nameof(DeletedColumn), SELF:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(cTable,   nameof(LongFieldNames),SELF:LongFieldNames)
        oTable:MaxRecords        := SELF:GetInt(cTable,     nameof(MaxRecords),    SELF:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(cTable,  nameof(RecnoColumn),   SELF:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(cTable,   nameof(TrimTrailingSpaces), SELF:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(cTable,   nameof(CompareMemo),   SELF:CompareMemo)

        // these fields have no defaults
        oTable:ColumnList           := SELF:GetString(cTable, nameof(oTable:ColumnList), "*")
        oTable:UpdatableColumns     := SELF:GetString(cTable, "UpdatableColumns", "*")
        oTable:KeyColumns           := SELF:GetString(cTable, "KeyColumns", "*")

        var cIndexes := SELF:GetString(cTable, "Indexes", "")
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
    END METHOD
    OVERRIDE METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        var cTags    := SELF:GetString(cSection, "Tags", "")
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
        oTag:Expression    := SELF:GetString(cSection, nameof(oTag:Expression ), "")
        oTag:Condition     := SELF:GetString(cSection, nameof(oTag:Condition ), "")
        oTag:Unique        := SELF:GetLogic(cSection, nameof(oTag:Unique ), FALSE)
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
