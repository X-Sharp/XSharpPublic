//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Diagnostics
USING System.Linq

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The TableDef class.
/// </summary>
[DebuggerDisplay("{Name,nq}, {Columns.Count}")];
CLASS SqlDbTableDef INHERIT SqlDbObject
    PROPERTY Columns AS IList<SqlDbColumnDef> AUTO GET PRIVATE SET

    CONSTRUCTOR(cName AS STRING, aColumns AS IList<SqlDbColumnDef>)
        SUPER(cName)
        SELF:Columns := aColumns:ToArray()
        RETURN

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
