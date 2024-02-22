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
    /// The AbstractMetaDataProvider class.
    /// </summary>
	ABSTRACT CLASS AbstractMetaDataProvider IMPLEMENTS IMetadataProvider
    protected _cache as Dictionary<string, SqlTableInfo>
    protected _connection AS SqlDbConnection

    CONSTRUCTOR(conn as SqlDbConnection)
        self:_connection := conn
        self:_cache      := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}

        RETURN

#region Properties
    /// <inheritdoc />
    PROPERTY TrimTrailingSpaces AS LOGIC AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY LongFieldNames     AS LOGIC AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY AllowUpdates       AS LOGIC AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY RecnoColumn        AS STRING AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY DeletedColumn      AS STRING AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY MaxRecords         AS LONG  AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY CompareMemo         AS LOGIC AUTO GET PROTECTED SET
    /// <inheritdoc />
    PROPERTY UpdateAllColumns    AS LOGIC AUTO GET PROTECTED SET
    /// <summary>
    ///  The connection that this metadata provider is using.
    /// </summary>
    PROPERTY Connection AS SqlDbConnection GET _connection

    #endregion
    /// <inheritdoc />
    ABSTRACT METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
    /// <inheritdoc />
    ABSTRACT METHOD GetIndexInfo(oTable AS SqlTableInfo, cIndex AS STRING) AS SqlIndexInfo

    METHOD FindInCache( cTable as STRING, oTable OUT SqlTableInfo) AS LOGIC
        oTable := null
        RETURN _cache:TryGetValue(cTable, out oTable)

    METHOD AddToCache( cTable as STRING, oTable AS SqlTableInfo) AS VOID
        _cache:Add(cTable, oTable)
        RETURN
	END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
