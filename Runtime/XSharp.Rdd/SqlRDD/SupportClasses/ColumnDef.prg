//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The ColumnDef class.
/// </summary>
[DebuggerDisplay("{Name,nq}, {Type.Name,nq} {Length}")];
class SqlDbColumnDef inherit SqlDbObject
    property OrdinalPosition as long auto get set
    property Type		 as System.Type get ColumnInfo:DotNetType
    property Length		 as long get ColumnInfo:Length
    property Precision	 as long get ColumnInfo:NumericPrecision
    property Scale		 as long get ColumnInfo:NumericScale
    property ReadOnly	 as logic get ColumnInfo:ReadOnly
    property ColumnInfo as DbColumnInfo auto get private set
    constructor(oCol as DbColumnInfo)
        super(oCol:Name)
        self:ColumnInfo := oCol
        return


end class
end namespace // XSharp.RDD.SqlRDD
