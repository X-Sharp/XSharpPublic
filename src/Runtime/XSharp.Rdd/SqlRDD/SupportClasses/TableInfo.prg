//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The TableInfo class.<br>
/// </summary>
/// <remarks>
/// This class stores "metadata" about tables. It can be filled from the database, in code or otherwise
/// </remarks>
class SqlTableInfo
    /// <summary>
    /// Table Name
    /// </summary>
    property Name           as string auto
    /// <summary>
    /// List of Columns
    /// </summary>
    property Columns        as List<SqlColumnInfo> auto get private set
    /// <summary>
    /// List of Indexes and tags
    /// </summary>
    property Indexes        as List<SqlIndexInfo>  auto get private set
    /// <summary>
    /// Name of the Recno column. When empty then the relative row number is the record number
    /// </summary>
    property RecnoColumn    as string auto
    /// <summary>
    /// Name of the Deleted column. When empty then rows will be physically deleted from the server
    /// </summary>
    property DeletedColumn  as string auto
    /// <summary>
    /// Additional server side filter that will be used as (additional) where clause when fetching data. This must be
    /// a valid SQL expression for the target server
    /// </summary>
    property ServerFilter   as string auto
    /// <summary>
    /// Can the table be updated ?
    /// </summary>
    property AllowUpdates   as logic auto
    /// <summary>
    /// Can field names longer than 10 characters be used (true) or should they be truncated (false)
    /// </summary>
    property LongFieldNames as logic auto
    /// <summary>
    /// What is the maximum number of records that the RDD should fetch when unfiltered.
    /// </summary>
    property MaxRecords     as long auto

    constructor()
        Columns := List<SqlColumnInfo>{}
        return

    static constructor()
        Cache := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}
    internal static Cache   as Dictionary<string, SqlTableInfo>


end class

/// <summary>
/// The ColumnInfo class.
/// </summary>
/// <remarks>
/// This class stores "metadata" about tables. It can be filled from the database, in code or otherwise
/// </remarks>
class SqlColumnInfo
    /// <summary>
    /// Column Name
    /// </summary>
    property Name       as string auto
    /// <summary>
    /// Is the column nullable
    /// </summary>
    property Nullable   as logic auto
    /// <summary>
    /// Is the column ReadOnly
    /// </summary>
    property ReadOnly   as logic auto
    /// <summary>
    /// Is the column an identity column
    /// </summary>
    property Identity   as logic auto
    /// <summary>
    /// Is the column hidden ?
    /// </summary>
    property Hidden     as logic auto
end class


class SqlIndexInfo
    /// <summary>
    /// Name of the "Index File"
    /// </summary>
    property IndexName as string auto
    /// <summary>
    /// Name of the order (tag)
    /// </summary>
    property OrderName as string auto
    /// <summary>
    /// Index expression in Xbase format
    /// </summary>
    property Expression as string auto
    /// <summary>
    /// Index condition in Xbase format
    /// </summary>
    property Condition as string auto
    /// <summary>
    ///  Should the index be unique ?
    /// </summary>
    property Unique    as logic  auto

end class


end namespace // XSharp.RDD.SqlRDD


