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
/// The OrderBag class.
/// </summary>
class SqlDbOrderBag inherit List<SqlDbOrder>
    property FileName           as string auto
    property ProductionIndex    as logic auto
    property WorkArea	        as SQLRDD auto

    constructor(cName as string, oArea as SQLRDD)
        super()
        self:FileName := cName
        self:WorkArea := oArea
        return
    new method Add(oOrder as SqlDbOrder) as void
        super:Add(oOrder)
        //SELF:WorkArea:Orders:Add(oOrder)
        self:Save()
        return
    method Close as logic
        self:Clear()
        return true
    method Load(aTags as IList<string>) as void
        return
    method Remove(nTagPos as long)  as object
        return null
    method Remove(cTagName as string)  as object
        return null
    method Save()  as void
        return
end class
end namespace // XSharp.RDD.SqlRDD
define strTags := "Tags"

