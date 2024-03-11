// MetadataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:31:39 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Support


BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The MetadataProvider class. Contains properties for some default settings
/// and a method to retrieve the table information for a given table.
/// </summary>
INTERFACE ISqlMetadataProvider

    /// <summary>
    /// Get the metadata for a table
    /// </summary>
    /// <param name="cTable">Name of the table to lookup the data for</param>
    /// <returns>Filled SqlDbTableInfo object</returns>
    /// <seealso cref="T:XSharp.RDD.SqlRDD.SqlDbTableInfo"/>
    METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo

    /// <summary>
    /// Get the metadata for an Index
    /// </summary>
    /// <param name="oTable">Table object to lookup the data for</param>
    /// <param name="cIndexName">Name of the index to lookup the data for</param>
    /// <returns>Filled SqlDbIndexInfo object</returns>
    /// <seealso cref="T:XSharp.RDD.SqlRDD.SqlDbIndexInfo" />
    /// <remarks> This method should NOT append the indexc to the TableInfo class </remarks>
    METHOD GetIndexInfo(oTable AS SqlDbTableInfo, cIndexName AS STRING) AS SqlDbIndexInfo

    /// <summary>
    /// This method is called when a table is created. It can be used to update the metadata
    /// </summary>
    /// <param name="cTable">TableName</param>
    /// <param name="info">Object with information</param>
    METHOD CreateTable(cTable AS STRING, info as DbOpenInfo) AS VOID
    /// <summary>
    /// This method is called when an index is created. It can be used to update the metadata
    /// </summary>
    /// <param name="cTable">TableName</param>
    /// <param name="info">Object with information about the index and tag that are created</param>
    METHOD CreateIndex(cTable as String, orderInfo as DbOrderCreateInfo) AS VOID

END INTERFACE
END NAMESPACE // XSharp.SQLRdd.Metadata
