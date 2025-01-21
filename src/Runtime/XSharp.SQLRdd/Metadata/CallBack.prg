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
    /// <summary>
    /// Construct a new instance of the SqlMetadataProviderAbstract class.
    /// </summary>
    /// <param name="conn">Connection to which the provider belongs</param>
    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        RETURN

    /// <inheritdoc/>
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
        local oTable as SqlDbTableInfo
        SELF:GetDefaults(DefaultSection)
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
#region Overridden Getmethods from Abstract
    override  method GetString(oPar as OBJECT, nReason as SqlRDDEventReason, cDefault as STRING) AS STRING
        RETURN _connection:RaiseStringEvent(_connection, nReason, (string) oPar, cDefault)
    END METHOD
    override method GetLogic(oPar as OBJECT, nReason as SqlRDDEventReason, lDefault as LOGIC) AS LOGIC
        RETURN _connection:RaiseLogicEvent(_connection, nReason, (string) oPar, lDefault)
    END METHOD
    override method GetInt(oPar as OBJECT, nReason as SqlRDDEventReason, nDefault as INT) AS INT
        RETURN _connection:RaiseIntEvent(_connection, nReason, (string) oPar, nDefault)
    END METHOD
#endregion
END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
