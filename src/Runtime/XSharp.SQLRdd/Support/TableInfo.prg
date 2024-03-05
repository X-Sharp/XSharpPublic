//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



using System
using System.Collections.Generic
using System.Text
using System.Diagnostics
begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// This class stores metadata about tables
/// </summary>
[DebuggerDisplay("{Name,nq}, {Columns.Count}")];
class SqlDbTableInfo inherit SqlDbObject

  /// <summary>
    /// List of columns
    /// </summary>
    /// <value></value>
    property Columns as IList<SqlDbColumnDef> auto get protected set

    /// <summary>
    /// Current select statement for the table
    /// </summary>
    property SelectStatement    as string auto

    /// <summary>
    /// Select statement with a where condition that returns no records
    /// </summary>
    property EmptySelectStatement as string auto


    /// <summary>
    /// Real tablename of the table in the SQL database. This is the name that is used in the SQL statements.
    /// This allows you to declare an alias for a table with for example a server side filter.
    /// </summary>
    property RealName as string auto

    /// <summary>
    /// Can the table be updated ?
    /// </summary>
    property AllowUpdates   as logic auto

    /// <summary>
    /// List of columns that have to be selected. Defaults to "*", which means all columns
    /// </summary>
    property ColumnList as string auto
    /// <summary>
    /// Name of the Recno column. When empty then the relative row number is the record number
    /// </summary>
    property RecnoColumn    as string auto
    /// <summary>
    /// Name of the Deleted column. When empty then rows will be physically deleted from the server
    /// </summary>
    property DeletedColumn  as string auto
    /// <summary>
    /// Specifies a comma separated list of columns that must be used for update commands. Defaults to all columns
    /// </summary>
    property KeyColumns          as String auto
    /// <summary>
    /// List of Indexes and tags
    /// </summary>
    property Indexes     as List<SqlDbIndexInfo>  auto get private set
    /// <summary>
    /// Can field names longer than 10 characters be used (true) or should they be truncated (false)
    /// </summary>
    property LongFieldNames as logic auto
    /// <summary>
    /// What is the maximum number of records that the RDD should fetch when unfiltered.
    /// </summary>
    property MaxRecords     as long auto
    /// <summary>
    /// Additional server side filter that will be used as (additional) where clause when fetching data. This must be
    /// a valid SQL expression for the target server
    /// </summary>
    property ServerFilter   as string auto
    /// <summary>
    /// Should trailing spaces for string columns be trimmed?
    /// </summary>
    property TrimTrailingSpaces as logic auto
    /// <summary>
    /// Should all columns be updated, or only the columns that were changed
    /// </summary>
    property UpdateAllColumns as logic auto

    /// <summary>
    /// Specifies a comma-delimited list of fields in the view and includes fields from the cursor
    /// </summary>
    property UpdatableColumns   as String auto

    /// <summary>
    /// Specifies whether memo fields of type Long text or Long binary are included in the WHERE clause when using automatic updating. This defaults to TRUE
    /// </summary>
    /// <value></value>
    property CompareMemo              as logic auto


    protected _connection as SqlDbConnection

    /// <summary>
    /// Create a new instance of the SqlDbTableInfo class
    /// </summary>
    /// <param name="cName">Table Name</param>
    /// <param name="oConn">Connection to which the table belongs</param>
    constructor(cName as string, oConn as SqlDbConnection)
        super(cName)
        _connection     := oConn
        self:MaxRecords         := oConn:MaxRecords
        self:RecnoColumn        := oConn:RecnoColumn
        self:DeletedColumn      := oConn:DeletedColumn
        self:AllowUpdates       := oConn:AllowUpdates
        self:LongFieldNames     := oConn:LongFieldNames
        self:TrimTrailingSpaces := oConn:TrimTrailingSpaces
        self:ServerFilter    := ""
        self:ColumnList      := "*"
        self:Indexes := List<SqlDbIndexInfo>{}
        self:Columns := List<SqlDbColumnDef>{}


        return
    internal method CopyFromTd(oTd as SqlDbTableInfo) as void
        self:Columns                := oTd:Columns
        self:SelectStatement        := oTd:SelectStatement
        self:EmptySelectStatement   := oTd:EmptySelectStatement
        return

    static constructor()
        Cache := Dictionary<string, SqlDbTableInfo>{StringComparer.OrdinalIgnoreCase}
    internal static Cache   as Dictionary<string, SqlDbTableInfo>


end class

/// <summary>
/// This class stores metadata for indexes
/// </summary>
class SqlDbIndexInfo INHERIT SqlDbObject
    /// <summary>
    /// Table to which the index belongs
    /// </summary>
    property Table  as SqlDbTableInfo auto
    /// <summary>
    /// List of tags
    /// </summary>
    property Tags   as List<SqlDbTagInfo> auto

    /// <summary>
    /// Create a new instance of the SqlDbIndexInfo class
    /// </summary>
    /// <param name="oTable">Table to which the index belongs</param>
    /// <param name="cIndex">Name of the index</param>
    constructor(oTable as SqlDbTableInfo, cIndex as string)
        SUPER(cIndex)
        SELF:Table := oTable
        SELF:Tags := List<SqlDbTagInfo>{}

end class

/// <summary>
/// This class stores metadata for index tags
/// </summary>
class SqlDbTagInfo inherit SqlDbObject
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

    /// <summary>
    /// Index to which the tag belongs
    /// </summary>
    /// <value></value>
    property Index     as SqlDbIndexInfo auto
    /// <summary>
    /// Create a new instance of the SqlDbTagInfo class
    /// </summary>
    /// <param name="oIndex">Index to which the tag belongs</param>
    /// <param name="name">Name of the Tag</param>
    constructor(oIndex as SqlDbIndexInfo, name as string)
        super(name)
        self:Index := oIndex
        return



end class



end namespace // XSharp.RDD.SqlRDD


