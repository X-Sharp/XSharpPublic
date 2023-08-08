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
    PROPERTY Type		 AS System.Type GET ColumnInfo:DotNetType
    PROPERTY Length		 AS LONG GET ColumnInfo:Length
    PROPERTY Precision	 AS LONG GET ColumnInfo:NumericPrecision
    PROPERTY Scale		 AS LONG GET ColumnInfo:NumericScale
    PROPERTY ReadOnly	 AS LOGIC GET ColumnInfo:ReadOnly
    PROPERTY ColumnInfo as DbColumnInfo AUTO GET PRIVATE SET
    CONSTRUCTOR(oCol as DbColumnInfo)
        SUPER(oCol:Name)
        SELF:ColumnInfo := oCol
        RETURN


END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
