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
class SqlTableInfo inherit SqlDbTableDef

    /// <summary>
    /// Real tablename of the table in the SQL database. This is the name that is used in the SQL statements
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
    /// Specifies whether memo fields of type Long text or Long binary are included in the WHERE clause when using automatic updating. This defaults to TRUE
    /// </summary>
    /// <value></value>
    property CompareMemo              as logic auto


    protected _connection as SqlDbConnection

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

    method CopyFromTd(oTd as SqlDbTableDef) as void
        self:Columns                := oTd:Columns
        self:SelectStatement        := oTd:SelectStatement
        self:EmptySelectStatement   := oTd:EmptySelectStatement
        return

    static constructor()
        Cache := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}
    internal static Cache   as Dictionary<string, SqlTableInfo>


end class


class SqlIndexInfo INHERIT SqlDbObject

    property Table  as SqlTableInfo auto
    property Tags   as List<SqlIndexTagInfo> auto

    constructor(oTable as SqlTableInfo, cIndex as string)
        SUPER(cIndex)
        SELF:Table := oTable
        SELF:Tags := List<SqlIndexTagInfo>{}

end class

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

    constructor(oIndex as SqlIndexInfo, name as string)
        super(name)
        self:Index := oIndex
        return



end class



end namespace // XSharp.RDD.SqlRDD


