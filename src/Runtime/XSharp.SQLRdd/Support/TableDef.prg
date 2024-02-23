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
class SqlDbTableInfo inherit SqlDbObject
  

    /// <summary>
    /// Create a new TableDef object
    /// </summary>
    /// <param name="cName">Table name</param>
    constructor(cName as string)
        super(cName)

    /// <summary>
    /// Create a new TableDef object
    /// </summary>
    /// <param name="cName">Table name</param>
    /// <param name="aColumns">List of columns</param>
    constructor(cName as string, aColumns as IList<SqlDbColumnDef>)
        super(cName)
        self:Columns := aColumns:ToArray()
        return

end class
end namespace // XSharp.RDD.SqlRDD
