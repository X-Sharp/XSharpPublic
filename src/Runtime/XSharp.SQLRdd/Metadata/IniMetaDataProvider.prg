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
CLASS IniMetaDataProvider IMPLEMENTS IMetadataProvider
    protect _connection AS SqlDbConnection
    protect _fileName as STRING
    protect _ini      as IniFile
    private const DefaultSection := "Defaults" as string

    CONSTRUCTOR(conn as SqlDbConnection)
        SELF("SQLRDD.INI", conn)
    END CONSTRUCTOR
    CONSTRUCTOR(cFile as STRING, conn as SqlDbConnection)
        self:_connection := conn
        _fileName := cFile
        _ini      := IniFile{_fileName}
        SELF:ReadDefaults()
        RETURN
    END CONSTRUCTOR
    METHOD ReadDefaults() AS VOID
        LongFieldNames      := _ini:GetLogic(DefaultSection,  nameof(LongFieldNames), TRUE)
        AllowUpdates        := _ini:GetLogic(DefaultSection,  nameof(AllowUpdates), TRUE)
        MaxRecords          := _ini:GetInt(DefaultSection,    nameof(MaxRecords), 1000)
        RecnoColumn         := _ini:GetString(DefaultSection, nameof(RecnoColumn), "")
        DeletedColumn       := _ini:GetString(DefaultSection, nameof(DeletedColumn), "")
        TrimTrailingSpaces  := _ini:GetLogic(DefaultSection,  nameof(TrimTrailingSpaces), TRUE)
        CompareMemo         := _ini:GetLogic(DefaultSection,  nameof(CompareMemo), TRUE)
        RETURN
    END METHOD
#region Properties
    /// <inheritdoc />
    PROPERTY TrimTrailingSpaces AS LOGIC AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY LongFieldNames     AS LOGIC AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY AllowUpdates       AS LOGIC AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY RecnoColumn        AS STRING AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY DeletedColumn      AS STRING AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY MaxRecords         AS LONG  AUTO GET PRIVATE SET
    /// <inheritdoc />
    PROPERTY CompareMemo         AS LOGIC AUTO GET PRIVATE SET
#endregion
    /// <inheritdoc />
    METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        oTable := SqlTableInfo{cTable, _connection}
        oTable:AllowUpdates      := _ini:GetLogic(cTable, nameof(AllowUpdates),  SELF:AllowUpdates)
        oTable:DeletedColumn     := _ini:GetString(cTable, nameof(DeletedColumn), SELF:DeletedColumn)
        oTable:LongFieldNames    := _ini:GetLogic(cTable, nameof(LongFieldNames), SELF:LongFieldNames)
        oTable:MaxRecords        := _ini:GetInt(cTable, nameof(MaxRecords),       SELF:MaxRecords)
        oTable:RecnoColumn       := _ini:GetString(cTable, nameof(RecnoColumn),   SELF:RecnoColumn)
        oTable:TrimTrailingSpaces:= _ini:GetLogic(cTable, nameof(TrimTrailingSpaces), SELF:TrimTrailingSpaces)
        oTable:CompareMemo       := _ini:GetLogic(cTable, nameof(CompareMemo),     SELF:CompareMemo)

        // these fields have no defaults
        oTable:ColumnList           := _ini:GetString(cTable, nameof(oTable:ColumnList), "*")
        oTable:UpdatableColumns     := _ini:GetString(cTable, "UpdatableColumns", "*")
        oTable:KeyColumns           := _ini:GetString(cTable, "KeyColumns", "*")

        var cIndexes := _ini:GetString(cTable, "Indexes", "")
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
    PROTECTED METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        var cTags    := _ini:GetString(cSection, "Tags", "")
        if (!String.IsNullOrEmpty(cTags))
            var aTags := cTags:Split(c",")
            foreach var cTag in aTags
                Self:GetTagInfo(oIndex, cTag)
            next
        endif
        return oIndex
    END METHOD
    PROTECTED METHOD GetTagInfo(oIndex as SqlIndexInfo, tagName as STRING) AS SqlIndexTagInfo
        var cSection := "Tag:"+oIndex:Name+"_"+tagName
        var oTag := SqlIndexTagInfo{oIndex, tagName}
        oTag:Expression    := _ini:GetString(cSection, nameof(oTag:Expression ), "")
        oTag:Condition     := _ini:GetString(cSection, nameof(oTag:Condition ), "")
        oTag:Unique        := _ini:GetLogic(cSection, nameof(oTag:Unique ), FALSE)
        oIndex:Tags:Add(oTag)
        RETURN oTag
    END METHOD


END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
