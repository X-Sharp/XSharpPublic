//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Diagnostics
using System.Linq

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The TableDef class.
/// </summary>
[DebuggerDisplay("{Name,nq}, {Columns.Count}")];
class SqlDbTableDef inherit SqlDbObject
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
    /// Specifies a comma-delimited list of fields in the view and includes fields from the cursor
    /// </summary>
    property UpdatableColumns   as String auto
    /// <summary>
    /// Specifies a comma separated list of columns that must be used for update commands. Defaults to all columns
    /// </summary>

    property KeyColumns          as String auto

    /// <summary>
    /// Create a new TableDef object
    /// </summary>
    /// <param name="cName">Table name</param>
    constructor(cName as string)
        super(cName)
        self:Columns := List<SqlDbColumnDef>{}
        SELF:KeyColumns := ""
        SELF:UpdatableColumns := ""

    /// <summary>
    /// Create a new TableDef object
    /// </summary>
    /// <param name="cName">Table name</param>
    /// <param name="aColumns">List of columns</param>
    constructor(cName as string, aColumns as IList<SqlDbColumnDef>)
        super(cName)
        self:Columns := aColumns:ToArray()
        SELF:KeyColumns := ""
        SELF:UpdatableColumns := ""
        return

end class
end namespace // XSharp.RDD.SqlRDD
