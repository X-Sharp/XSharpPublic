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

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
        local oTable as SqlDbTableInfo
        SUPER:GetDefaults(DefaultSection)
        if SELF:FindInCache(cTable, out oTable)
            return oTable
        endif
        oTable := SqlDbTableInfo{cTable, Connection}
        SUPER:ReadTable(oTable, cTable)

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
    END METHOD

    /// <inheritdoc />
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
#region Overridden Getmethods from Abstract
    override method GetString(oPar as OBJECT, nReason as SqlRDDEventReason, cDefault as STRING) as STRING
        RETURN _ini:GetString((string) oPar, nReason:ToString(), cDefault)
    END METHOD
    override method GetLogic(oPar as OBJECT,  nReason as SqlRDDEventReason, lDefault as LOGIC) AS LOGIC
        RETURN _ini:GetLogic((string) oPar, nReason:ToString(), lDefault)
    END METHOD
    override method GetInt(oPar as OBJECT,  nReason as SqlRDDEventReason, nDefault as INT) AS INT
        RETURN _ini:GetInt((string) oPar, nReason:ToString(), nDefault)
    END METHOD
#endregion
END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata

