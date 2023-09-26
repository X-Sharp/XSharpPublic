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
/// The Order class.
/// </summary>
class SqlDbOrder inherit SqlDbObject
    property Bag		as SqlDbOrderBag auto
    property WorkArea	as SQLRDD   auto
    property KeyExp     as string   auto
    property AdoKey		as string   auto
    property cbExpr		as object   auto
    property uTopScope	as object   auto
    property uBotScope	as object   auto
    property Descending as logic    auto
    property Unique    	as logic    auto
    property KeyLength	as dword    auto
    property HasFunctions as logic  auto
    property Segments	as IList<SqlDbSegment>   auto
    property ColumnList	as IList<string>    auto
    property OrderList	as IList<string>    auto

    constructor(oRDD as SQLRDD, cName as string, cIndexExpr as string, oBag as SqlDbOrderBag)
        super(cName)
        self:WorkArea       := oRDD
        self:KeyExp         := cIndexExpr
        self:Bag            := oBag
        var oExp            := SqlDbExpression{self,cIndexExpr}
        self:HasFunctions 	:= oExp:HasFunctions
        self:AdoKey			:= oExp:SQLKey
        self:OrderList 	    := oExp:OrderList
        self:ColumnList	    := oExp:ColumnList
        self:Segments		:= oExp:Segments

        return

end class
end namespace // XSharp.RDD.SqlRDD
