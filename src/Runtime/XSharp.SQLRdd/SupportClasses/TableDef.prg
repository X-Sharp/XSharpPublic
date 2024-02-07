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
    property Columns as IList<SqlDbColumnDef> auto get protected set
    property SelectStatement    as string auto
    property EmptySelectStatement as string auto
    /// <summary>
    /// Specifies a comma-delimited list of fields in the view and includes fields from the cursor
    /// </summary>

    property UpdatableColumns   as String auto
    /// <summary>
    /// Specifies a comma separated list of columns that must be used for update commands. Defaults to all columns
    /// </summary>

    property KeyColumns          as String auto

    constructor(cName as string)
        super(cName)
        self:Columns := List<SqlDbColumnDef>{}
        SELF:KeyColumns := ""
        SELF:UpdatableColumns := ""

    constructor(cName as string, aColumns as IList<SqlDbColumnDef>)
        super(cName)
        self:Columns := aColumns:ToArray()
        SELF:KeyColumns := ""
        SELF:UpdatableColumns := ""
        return

end class
end namespace // XSharp.RDD.SqlRDD
