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
/// The SqlMetadataProviderIni class. Provides metadata for the RDD from an INI file
/// </summary>
CLASS SqlMetadataProviderIni Inherit SqlMetadataProviderAbstract
    protect _fileName as STRING
    protect _ini      as SqlDbIniFile
    private const DefaultSection := "Defaults" as string
    private hasDefaults as LOGIC

    /// <summary>
    /// Construct a new instance of the SqlMetadataProviderIni class.
    /// </summary>
    /// <param name="conn">Connection to which the provider belongs</param>
    CONSTRUCTOR(conn as SqlDbConnection)
        SELF("SQLRDD.INI", conn)
    END CONSTRUCTOR

    /// <summary>
    /// Construct a new instance of the SqlMetadataProviderIni class.
    /// </summary>
    /// <param name="conn">Connection to which the provider belongs</param>
    /// <param name="cFile">File name where the metadata is stored</param>
    CONSTRUCTOR(cFile as STRING, conn as SqlDbConnection)
        SUPER(conn)
        _fileName := cFile
        _ini      := SqlDbIniFile{_fileName}
        RETURN
    END CONSTRUCTOR
    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            hasDefaults := true
            _connection:LongFieldNames      := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.LongFieldNames), _connection:LongFieldNames)
            _connection:TrimTrailingSpaces  := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.TrimTrailingSpaces), _connection:TrimTrailingSpaces)
            _connection:UpdateAllColumns    := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.UpdateAllColumns), _connection:UpdateAllColumns)
            _connection:AllowUpdates        := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.AllowUpdates), _connection:AllowUpdates)
            _connection:MaxRecords          := SELF:GetInt(DefaultSection,    nameof(SqlRDDEventReason.MaxRecords), _connection:MaxRecords)
            _connection:RecnoColumn         := SELF:GetString(DefaultSection, nameof(SqlRDDEventReason.RecnoColumn), _connection:RecnoColumn)
            _connection:DeletedColumn       := SELF:GetString(DefaultSection, nameof(SqlRDDEventReason.DeletedColumn), _connection:DeletedColumn)
            _connection:CompareMemo         := SELF:GetLogic(DefaultSection,  nameof(SqlRDDEventReason.CompareMemo), _connection:CompareMemo)
            ENDIF
        RETURN
    END METHOD

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
        local oTable as SqlDbTableInfo
        ReadDefaults()
        if SELF:FindInCache(cTable, out oTable)
            return oTable
        endif
        oTable := SqlDbTableInfo{cTable, Connection}
        oTable:RealName          := SELF:GetString(cTable,  nameof(SqlRDDEventReason.RealName),      cTable)
        oTable:AllowUpdates      := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.AllowUpdates),  _connection:AllowUpdates)
        oTable:DeletedColumn     := SELF:GetString(cTable,  nameof(SqlRDDEventReason.DeletedColumn), _connection:DeletedColumn)
        oTable:LongFieldNames    := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.LongFieldNames),_connection:LongFieldNames)
        oTable:UpdateAllColumns  := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.UpdateAllColumns),_connection:UpdateAllColumns)
        oTable:MaxRecords        := SELF:GetInt(cTable,     nameof(SqlRDDEventReason.MaxRecords),    _connection:MaxRecords)
        oTable:RecnoColumn       := SELF:GetString(cTable,  nameof(SqlRDDEventReason.RecnoColumn),   _connection:RecnoColumn)
        oTable:TrimTrailingSpaces:= SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.TrimTrailingSpaces), _connection:TrimTrailingSpaces)
        oTable:CompareMemo       := SELF:GetLogic(cTable,   nameof(SqlRDDEventReason.CompareMemo),   _connection:CompareMemo)

        // these fields have no defaults
         oTable:ServerFilter         := SELF:GetString(cTable, nameof(SqlRDDEventReason.ServerFilter), DEFAULT_SERVERFILTER)
         oTable:ColumnList           := SELF:GetString(cTable, nameof(SqlRDDEventReason.ColumnList), DEFAULT_COLUMNLIST)
         oTable:UpdatableColumns     := SELF:GetString(cTable, nameof(SqlRDDEventReason.UpdatableColumns), DEFAULT_UPDATABLECOLUMNS)
         oTable:KeyColumns           := SELF:GetString(cTable, nameof(SqlRDDEventReason.KeyColumns), DEFAULT_KEYCOLUMNS)

        var cIndexes := SELF:GetString(cTable, nameof(SqlRDDEventReason.Indexes), DEFAULT_INDEXES)
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
    OVERRIDE METHOD GetIndexInfo(oTable as SqlDbTableInfo, cIndexName as STRING) AS SqlDbIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var cSection := "Index:"+cIndexName
        var oIndex  := SqlDbIndexInfo{oTable, cIndexName}
        var cTags    := SELF:GetString(cSection, nameof(SqlRDDEventReason.Tags), DEFAULT_TAGS)
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
        oTag:Expression    := SELF:GetString(cSection, nameof(SqlRDDEventReason.Expression ), DEFAULT_EXPRESSION)
        oTag:Condition     := SELF:GetString(cSection, nameof(SqlRDDEventReason.Condition ), DEFAULT_CONDITION)
        oTag:Unique        := SELF:GetLogic(cSection, nameof(SqlRDDEventReason.Unique ), DEFAULT_UNIQUE)
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

