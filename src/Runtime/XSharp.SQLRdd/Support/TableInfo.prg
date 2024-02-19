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
/// The TableInfo class.
/// </summary>
/// <remarks>
/// This class stores "metadata" about tables. It can be filled from the database, in code or otherwise
/// </remarks>
class SqlTableInfo inherit SqlDbTableDef

    /// <summary>
    /// Real tablename of the table in the SQL database. This is the name that is used in the SQL statements.
    /// This allows you to declare an alias for a table with for example a server side filter.
    /// </summary>
    property RealName as string auto
    /// <summary>
    /// List of Indexes and tags
    /// </summary>
    property Indexes     as List<SqlIndexInfo>  auto get private set
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
    /// <summary>
    /// Should trailing spaces for string columns be trimmed?
    /// </summary>
    property TrimTrailingSpaces as logic auto
    /// <summary>
    /// Should all columns be updated, or only the columns that were changed
    /// </summary>
    property UpdateAllColumns as logic auto

    /// <summary>
    /// Specifies whether memo fields of type Long text or Long binary are included in the WHERE clause when using automatic updating. This defaults to TRUE
    /// </summary>
    /// <value></value>
    property CompareMemo              as logic auto


    protected _connection as SqlDbConnection

    /// <summary>
    /// Create a new instance of the SqlTableInfo class
    /// </summary>
    /// <param name="cName">Table Name</param>
    /// <param name="oConn">Connection to which the table belongs</param>
    constructor(cName as string, oConn as SqlDbConnection)
        super(cName)
        _connection     := oConn
        MaxRecords      := 1000
        //RecnoColumn := "xs_pk"
        RecnoColumn     := ""
        DeletedColumn   := ""
        ServerFilter    := ""
        ColumnList      := "*"
        AllowUpdates := true
        LongFieldNames := oConn:UseLongNames
        TrimTrailingSpaces := oConn:TrimTrailingSpaces
        Indexes := List<SqlIndexInfo>{}

        return
    internal method CopyFromTd(oTd as SqlDbTableDef) as void
        self:Columns                := oTd:Columns
        self:SelectStatement        := oTd:SelectStatement
        self:EmptySelectStatement   := oTd:EmptySelectStatement
        return

    static constructor()
        Cache := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}
    internal static Cache   as Dictionary<string, SqlTableInfo>


end class

/// <summary>
/// This class stores metadata for indexes
/// </summary>
class SqlIndexInfo INHERIT SqlDbObject
    /// <summary>
    /// Table to which the index belongs
    /// </summary>
    property Table  as SqlTableInfo auto
    /// <summary>
    /// List of tags
    /// </summary>
    property Tags   as List<SqlIndexTagInfo> auto

    /// <summary>
    /// Create a new instance of the SqlIndexInfo class
    /// </summary>
    /// <param name="oTable">Table to which the index belongs</param>
    /// <param name="cIndex">Name of the index</param>
    constructor(oTable as SqlTableInfo, cIndex as string)
        SUPER(cIndex)
        SELF:Table := oTable
        SELF:Tags := List<SqlIndexTagInfo>{}

end class

/// <summary>
/// This class stores metadata for index tags
/// </summary>
class SqlIndexTagInfo inherit SqlDbObject
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
    property Index     as SqlIndexInfo auto
    /// <summary>
    /// Create a new instance of the SqlIndexTagInfo class
    /// </summary>
    /// <param name="oIndex">Index to which the tag belongs</param>
    /// <param name="name">Name of the Tag</param>
    constructor(oIndex as SqlIndexInfo, name as string)
        super(name)
        self:Index := oIndex
        return



end class



end namespace // XSharp.RDD.SqlRDD


