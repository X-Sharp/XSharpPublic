//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The ColumnDef class.
/// </summary>
[DebuggerDisplay("{Name,nq}, {Type.Name,nq} {Length}")];
CLASS SqlDbColumnDef INHERIT SqlDbObject
    PROPERTY OrdinalPosition AS LONG AUTO GET SET
    PROPERTY Type		 AS System.Type AUTO GET SET
    PROPERTY Length		 AS LONG AUTO GET SET
    PROPERTY Precision	 AS LONG AUTO GET SET
    PROPERTY Scale		 AS LONG AUTO GET SET
    PROPERTY bType		 AS BYTE AUTO GET SET
    PROPERTY Updatable	 AS LOGIC AUTO GET SET
    CONSTRUCTOR(ColumnName AS STRING)
        SUPER(ColumnName)
        RETURN


END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
