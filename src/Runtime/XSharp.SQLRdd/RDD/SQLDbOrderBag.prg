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
internal class SqlDbOrderBag INHERIT BaseIndex
    /// <summary>
    /// Physical file name of the index. This does NOT include the path.
    /// </summary>
    property FileName         as string auto
    /// <summary>
    /// Logical name of the index. For production indexes the logical name is derived from the tablename of the SQL table.
    /// </summary>
    property LogicalName      as string auto
    /// <summary>
    /// The Production index is the index with the same name as the table. Its tags are filled by the Metadata provider.
    /// The production index file is saved when the tags are loaded.
    /// When the index is closed, the production index file is deleted.
    /// </summary>
    property ProductionIndex  as logic auto
    /// <summary>
    /// Reference to the RDD
    /// </summary>
    property RDD              as SQLRDD auto
    /// <summary>
    /// The path to the index file
    /// </summary>
    property Path             as string auto
    /// <summary>
    /// The tags in the index
    /// </summary>
    property Tags             as List<SqlDbOrder> auto
    internal const BAG_EXTENSION := ".SDX" as string

    /// <summary>
    /// The full path to the OrderBag file
    /// </summary>
    /// <value></value>
    property FullPath as string => System.IO.Path.Combine(SELF:Path, self:FileName)+ BAG_EXTENSION

    /// <summary>
    /// Create a new OrderBag object
    /// </summary>
    /// <param name="oRdd">The Owner RDD</param>
    /// <param name="cName">The file name of the index</param>
    constructor(oRdd as SQLRDD, cName as string)
        super(oRdd)
        self:RDD      := oRdd
        if File(cName)
            cName            := FPathName()
            SELF:Path        := System.IO.Path.GetDirectoryName(cName)
            SELF:FileName    := System.IO.Path.GetFileNameWithoutExtension(cName)
        else
            var rddPath      := (string) oRdd:Info(DBI_FULLPATH, NULL)
            SELF:Path        := System.IO.Path.GetDirectoryName(rddPath)
            self:FileName    := System.IO.Path.GetFileNameWithoutExtension(rddPath)
        endif
        SELF:LogicalName := SELF:FileName
        self:Tags        := List<SqlDbOrder>{}
        return
    end constructor

    /// <summary>
    /// Add an order to the orderbag
    /// </summary>
    /// <param name="oOrder"></param>
    method Add(oOrder as SqlDbOrder) as void
        self:Tags:Add(oOrder)
        return
    /// <summary>
    /// Close the orderbag. The generated production index file is deleted.
    /// </summary>
    /// <returns></returns>
    method Close() as logic
        self:Tags:Clear()
        if self:ProductionIndex
            System.IO.File.Delete(self:FullPath)
        endif
        return true
    end method

    /// <summary>
    /// Remove an order/tag from the orderbag
    /// </summary>
    /// <param name="nTagPos">Tag number to delete</param>
    /// <returns>The tag object that was deleted</returns>
    method Remove(nTagPos as long)  as SqlDbOrder
        if nTagPos > 0 .and. nTagPos <= self:Tags:Count
            var tag := self:Tags[nTagPos-1]
            self:Tags:RemoveAt(nTagPos-1)
            return tag
        endif
        return null
    end method

    /// <summary>
    /// Remove an order/tag  from the orderbag
    /// </summary>
    /// <param name="cTagName">Tag name to delete</param>
    /// <returns>The tag object that was deleted</returns>
    method Remove(cTagName as string)  as SqlDbOrder
        foreach var tag in self:Tags:ToArray()
            if String.Compare(tag:Name, cTagName, true) == 0
                self:Tags:Remove(tag)
                return tag
            endif
        next
        return null
    end method

    /// <summary>
    /// Load the orderbag from a file
    /// </summary>
    /// <returns></returns>
    method Load() AS LOGIC
        var oIni := SqlDbIniFile{SELF:FullPath}
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
    end method

    /// <summary>
    /// Read the info from a single Order/Tag from the OrderBag FileName
    /// </summary>
    /// <param name="cIndexName">Logical Index Name</param>
    /// <param name="cTagName">Order/Tag name</param>
    /// <param name="oIni">Ini File object</param>
    /// <returns>The filled Order object</returns>
    method GetTagInfo(cIndexName as STRING, cTagName as STRING, oIni as SqlDbIniFile) as SqlDbOrder
        local oOrder as SqlDbOrder
        var section := TAGPREFIX+cIndexName+":"+cTagName
        var cExpression := oIni:GetString(section, EXPRESSION, "")
        var cCondition := oIni:GetString(section, CONDITION, "")
        var lUnique     := oIni:GetLogic(section, UNIQUE, FALSE)
        oOrder := SqlDbOrder{SELF:RDD,  cTagName, cExpression, SELF}
        oOrder:Condition := cCondition
        oOrder:Unique := lUnique
        return oOrder
    end method


    /// <summary>
    /// Save the index to the FullPath as an .SDX file.
    /// </summary>
    method Save()  as void
        local oIni as SqlDbIniFile
        local cTags := "" as string
        oIni := SqlDbIniFile{self:FullPath}
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
    end method

    /// <summary>
    /// Find an order/tag by name
    /// </summary>
    /// <param name="strName">Name to search</param>
    /// <returns>Order/Tag or NULL when not found</returns>
    method FindTagByName(strName as string) as SqlDbOrder
        foreach var tag in Tags
            if String.Compare(tag:Name, strName, true) == 0
                return tag
            endif
        next
        return null
    end method

    /// <summary>
    /// Find an order/tag by position
    /// </summary>
    /// <param name="index">Position to search</param>
    /// <returns>Order/Tag or NULL when not found</returns>
    method FindTag(index as long) as SqlDbOrder
        if index > 0 .and. index <= Tags:Count
            return Tags[index-1]
        endif
        return null
    end method

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



