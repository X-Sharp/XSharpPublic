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
class SqlDbOrderBag
    property FileName         as string auto
    property ProductionIndex  as logic auto
    property Rdd              as SQLRDD auto
    property Tags             as List<SqlDbOrder> auto
    internal const BAG_EXTENSION := ".SDX" as string

    property FullPath as string
        get
            var path := (string) self:Rdd:Info(DBI_FULLPATH, null)
            path     := System.IO.Path.GetDirectoryName(path)
            return System.IO.Path.Combine(path, self:FileName, BAG_EXTENSION)
        end get

    end property

    constructor(cName as string, oRdd as SQLRDD)
        super()
        self:FileName := cName
        self:Rdd      := oRdd
        self:Tags   := List<SqlDbOrder>{}
        return
    method Add(oOrder as SqlDbOrder) as void
        self:Tags:Add(oOrder)
        //SELF:WorkArea:Orders:Add(oOrder)
        self:Save()
        return
    method Close as logic
        self:Tags:Clear()
        return true
    method Load(aTags as IList<string>) as void
        return
    method Remove(nTagPos as long)  as object
        return null
    method Remove(cTagName as string)  as object
        return null
    method Save()  as void
        return
    method FindTag(strName as string) as SqlDbOrder
        foreach var tag in Tags
            if String.Compare(tag:Name, strName, true) == 0
                return tag
            endif
        next
        return null
    method FindTag(index as long) as SqlDbOrder
        if index > 0 .and. index <= Tags:Count
            return Tags[index-1]
        endif
        return null

end class
end namespace // XSharp.RDD.SqlRDD
define strTags := "Tags"

