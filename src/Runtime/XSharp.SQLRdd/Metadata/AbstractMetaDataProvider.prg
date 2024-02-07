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

    protect _connection AS SqlDbConnection
    CONSTRUCTOR(conn as SqlDbConnection)
        self:_connection := conn
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

    PROPERTY Connection AS SqlDbConnection GET _connection

    #endregion

    ABSTRACT METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
    ABSTRACT METHOD GetIndexInfo(oTable AS SqlTableInfo, cIndex AS STRING) AS SqlIndexInfo

	END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
