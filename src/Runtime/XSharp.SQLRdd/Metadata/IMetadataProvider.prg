// MetadataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:31:39 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The MetadataProvider class. Contains properties for some default settings
/// and a method to retrieve the table information for a given table.
/// </summary>
INTERFACE IMetadataProvider
    /// <summary>
    /// Specifies whether memo fields of type Long text or Long binary are included in the WHERE clause when using automatic updating. This defaults to TRUE
    /// </summary>
    /// <value></value>
    PROPERTY CompareMemo       as LOGIC GET
    /// <summary>
    /// Name of the Deleted column. When empty then rows will be physically deleted from the server
    /// </summary>
    PROPERTY DeletedColumn      as STRING GET
    /// <summary>
    /// Can field names longer than 10 characters be used (true) or should they be truncated (false)
    /// </summary>
    PROPERTY LongFieldNames     as LOGIC GET
    /// <summary>
    /// What is the maximum number of records that the RDD should fetch when unfiltered.
    /// </summary>
    PROPERTY MaxRecords         as INT GET
    /// <summary>
    /// Name of the Recno column. When empty then the relative row number is the record number
    /// </summary>
    PROPERTY RecnoColumn        as STRING GET
    /// <summary>
    /// Should trailing spaces for string columns be trimmed?
    /// </summary>
    PROPERTY TrimTrailingSpaces as LOGIC GET
    /// <summary>
    /// Should trailing spaces for string columns be trimmed?
    /// </summary>
    PROPERTY UpdateAllColumns as LOGIC GET
    /// <summary>
    /// Get the metadata for a table
    /// </summary>
    /// <param name="cTableName">Name of the table to lookup the data for</param>
    /// <returns>Filled SqlTableInfo object</returns>
    /// <seealso cref="T:XSharp.RDD.SqlRDD.SqlTableInfo"/>
    METHOD GetTableInfo(cTableName as STRING) AS SqlTableInfo

    /// <summary>
    /// Get the metadata for an Index
    /// </summary>
    /// <param name="oTable">Table object to lookup the data for</param>
    /// <param name="cIndex">Name of the index to lookup the data for</param>
    /// <returns>Filled SqlIndexInfo object</returns>
    /// <seealso cref="T:XSharp.RDD.SqlRDD.SqlIndexInfo" />
    /// <remarks> This method should NOT append the indexc to the TableInfo class </remarks>
    METHOD GetIndexInfo(oTable AS SqlTableInfo, cIndex AS STRING) AS SqlIndexInfo

END INTERFACE
END NAMESPACE // XSharp.SQLRdd.Metadata
