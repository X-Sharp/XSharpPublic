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
    property LogicalName      as string auto
    property ProductionIndex  as logic auto
    property Rdd              as SQLRDD auto
    property Path             as string auto
    property Tags             as List<SqlDbOrder> auto
    internal const BAG_EXTENSION := ".SDX" as string

    property FullPath as string
        get
            return System.IO.Path.Combine(SELF:Path, self:FileName)+ BAG_EXTENSION
        end get

    end property

    constructor(cName as string, oRdd as SQLRDD)
        super()
        self:Rdd      := oRdd
        if File(cName)
            cName       := FPathName()
            SELF:Path        := System.IO.Path.GetDirectoryName(cName)
            SELF:FileName    := System.IO.Path.GetFileNameWithoutExtension(cName)
        else
            SELF:Path        := (string) oRdd:Info(DBI_FULLPATH, NULL)
            SELF:Path        := System.IO.Path.GetDirectoryName(SELF:Path)
            self:FileName    := System.IO.Path.GetFileNameWithoutExtension(cName)
        endif
        SELF:LogicalName := SELF:FileName
        self:Tags     := List<SqlDbOrder>{}
        return
    method Add(oOrder as SqlDbOrder) as void
        self:Tags:Add(oOrder)
        return
    method Close() as logic
        self:Tags:Clear()
        if self:ProductionIndex
            System.IO.File.Delete(self:FullPath)
        endif
        return true
    method Remove(nTagPos as long)  as SqlDbOrder
        if nTagPos > 0 .and. nTagPos <= self:Tags:Count
            var tag := self:Tags[nTagPos-1]
            self:Tags:RemoveAt(nTagPos-1)
            return tag
        endif
        return null
    method Remove(cTagName as string)  as SqlDbOrder
        foreach var tag in self:Tags:ToArray()
            if String.Compare(tag:Name, cTagName, true) == 0
                self:Tags:Remove(tag)
                return tag
            endif
        next
        return null

    method Load() AS LOGIC
        var oIni := IniFile{SELF:FullPath}
        if !oIni:Exists
            return FALSE
        endif
        local cTags := oIni:GetString(INDEXPREFIX+SELF:LogicalName, TAGS,"") as string
        if (!String.IsNullOrEmpty(cTags))
            var aTags := cTags:Split(c",")
            foreach var cTag in aTags
                var tag := Self:GetTagInfo(LogicalName, cTag, oIni)
                if tag != null
                    self:Tags:Add(tag)
                endif
            next
        endif
        return true

    method GetTagInfo(cIndexName as STRING, cTagName as STRING, oIni as IniFile) as SqlDbOrder
        local oOrder as SqlDbOrder
        var section := TAGPREFIX+cIndexName+":"+cTagName
        var cExpression := oIni:GetString(section, EXPRESSION, "")
        var cCondition := oIni:GetString(section, CONDITION, "")
        var lUnique     := oIni:GetLogic(section, UNIQUE, FALSE)
        oOrder := SqlDbOrder{SELF:Rdd,  cTagName, cExpression, SELF}
        oOrder:Condition := cCondition
        oOrder:Unique := lUnique
        RETURN oOrder


    method Save()  as void
        local oIni as IniFile
        local cTags := "" as string
        oIni := IniFile{self:FullPath}
        oIni:WriteString(INDEXPREFIX+SELF:LogicalName, TAGS,"")
        local first := TRUE as logic
        local cPreFix := TAGPREFIX+SELF:LogicalName+":" as string
        foreach var tag in Tags
            if first
                first := false
            else
                cTags += ","
            endif
            cTags += tag:Name
            oIni:WriteString(cPreFix+tag:Name, EXPRESSION, tag:Expression)
            oIni:WriteString(cPreFix+tag:Name, CONDITION, tag:Condition+" ")
            oIni:WriteString(cPreFix+tag:Name, UNIQUE, tag:Unique:ToString())
        next
        oIni:WriteString(INDEXPREFIX+SELF:LogicalName, TAGS,cTags)
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
        #region Constants
        internal const INDEXPREFIX := "Index:" as string
        internal const TAGPREFIX   := "Tag:" as string
        internal const TAGS        := "Tags" as string
        internal const EXPRESSION  := "Expression" as string
        internal const CONDITION   := "Condition" as string
        internal const UNIQUE      := "Unique" as string
        #endregion
end class
end namespace // XSharp.RDD.SqlRDD
define strTags := "Tags"

