//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

    [Flags];
    enum SqlDbColumnFlags
        member Recno   := 1
        member Deleted := 2
    end enum


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
    property ColumnInfo  as DbColumnInfo auto get private set
    property Nullable    as logic auto
    property Identity    as logic auto
    property ColumnFlags as SqlDbColumnFlags auto

    constructor(oCol as DbColumnInfo)
        super(oCol:Name)
        self:ColumnInfo := oCol
        return


end class
end namespace // XSharp.RDD.SqlRDD
