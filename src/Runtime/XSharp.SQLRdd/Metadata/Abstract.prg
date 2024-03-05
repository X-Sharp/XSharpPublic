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


END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
