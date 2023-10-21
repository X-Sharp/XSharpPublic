// SqlDbTableCommandBuilder.prg
// Created by    : robert
// Creation Date : 9/25/2023 2:54:39 PM
// Created for   :
// WorkStation   : NYX


using System
using System.Collections.Generic
using System.Text
using System.Linq
using XSharp.RDD.Support

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlDbTableCommandBuilder class.
/// </summary>
class SqlDbTableCommandBuilder
    protected _cTable as string
    protected _oInfo as SqlTableInfo
    protected _oRdd  as SQLRDD
    protected _connection as SqlDbConnection
    protected _orderBags      as List<SqlDbOrderBag>
    property Connection as SqlDbConnection  get _connection
    property Provider   as SqlDbProvider    get Connection:Provider

    constructor(cTable as string, oRdd as SQLRDD)
        self:_cTable     := cTable
        self:_oRdd       := oRdd
        _orderBags       := List<SqlDbOrderBag>{}
        _connection      := oRdd:Connection
        return
    method FetchInfo(oRdd as SQLRDD) as SqlTableInfo
        // build initial information needed for SQL Query for a table
        local oInfo as SqlTableInfo
        local cTable as string
        local columnNames := "*" as string
        cTable := self:_cTable
        oInfo := SqlTableInfo{_cTable, _connection}

        if oRdd:IniFile:Exists
            local ini := oRdd:IniFile as IniFile
            oInfo:MaxRecords    := ini:GetInt(cTable, nameof(oInfo:MaxRecords), oInfo:MaxRecords)
            oInfo:RecnoColumn   := ini:GetString(cTable, nameof(oInfo:RecnoColumn), oInfo:RecnoColumn)
            oInfo:DeletedColumn   := ini:GetString(cTable, nameof(oInfo:DeletedColumn), oInfo:DeletedColumn)
            oInfo:LongFieldNames  := ini:GetLogic(cTable, nameof(oInfo:LongFieldNames), oInfo:LongFieldNames)
            oInfo:TrimTrailingSpaces := ini:GetLogic(cTable, nameof(oInfo:TrimTrailingSpaces), oInfo:TrimTrailingSpaces)
            columnNames          := ini:GetString(cTable, "ColumnList", columnNames)
        endif

        // Ask the client for MaxRecords
        oInfo:MaxRecords        := Connection:RaiseIntEvent(Connection, SqlRDDEventReason.MaxRecords,cTable, oInfo:MaxRecords)
        oInfo:RecnoColumn       := Connection:RaiseStringEvent(Connection, SqlRDDEventReason.RecnoColumn,cTable, oInfo:RecnoColumn)
        oInfo:DeletedColumn     := Connection:RaiseStringEvent(Connection, SqlRDDEventReason.DeletedColumn,cTable, oInfo:DeletedColumn)
        oInfo:LongFieldNames    := Connection:RaiseLogicEvent(Connection, SqlRDDEventReason.LongFieldNames,cTable, oInfo:LongFieldNames)
        oInfo:TrimTrailingSpaces:= Connection:RaiseLogicEvent(Connection, SqlRDDEventReason.TrimTrailingSpaces,cTable, oInfo:TrimTrailingSpaces)
        var oTd := Connection:GetStructureForTable(cTable, oInfo:LongFieldNames,columnNames)
        oInfo:CopyFromTd(oTd)
        self:AdjustSelects(oInfo)
        self:_oInfo := oInfo
        self:OpenIndex(cTable, oRdd)  // open production index
        return oInfo

    method OpenIndex(cIndex as string, oRdd as SQLRDD) as void
        local tags as IList<string>
        local ini := null as IniFile
        var section := "Index:"+cIndex
        tags := List<string>{}
        if oRdd:IniFile:Exists
            ini := oRdd:IniFile
        endif
        if ini != null
            var nTags        := ini:GetInt(section, "TagCount",0)
            for var i := 1 to nTags
                var tag := ini:GetString(section, i"Tag{i}","")
                if ! String.IsNullOrEmpty(tag)
                    tags:Add(tag)
                endif
            next
        endif
        tags :=Connection:RaiseListEvent(Connection, SqlRDDEventReason.IndexTags,cIndex, tags)
        if tags?:Count > 0
            var oBag := SqlDbOrderBag{cIndex, oRdd}
            foreach var tagName in tags
                local info as IList<string>
                info := List<string>{}
                if ini != null
                    section := "Tag:"+cIndex+":"+tagName
                    var expr := ini:GetString(section, "Expression","")
                    var cond := ini:GetString(section, "Condition","")
                    info:Add(expr)
                    info:Add(cond)
                endif
                info := Connection:RaiseListEvent(Connection, SqlRDDEventReason.IndexInfo,cIndex+"."+tagName, info)
                if info?:Count > 0
                    var oTag := SqlDbOrder{oRdd, tagName, info[0],oBag}
                    if info?:Count > 1
                        oTag:SetCondition(info[1])
                    endif
                    oBag:Add(oTag)
                endif
            next
            _orderBags:Add(oBag)
        endif

    method SetProductionIndex() as logic
        _oRdd:CurrentOrder := null
        if self:_orderBags:Count > 0
            var bag := self:_orderBags:Where( {bag => bag:FileName == _cTable}):FirstOrDefault()
            if (bag != null)
                _oRdd:CurrentOrder := bag:Tags:FirstOrDefault()
                return true
            endif
        endif
        return false

    method OrderListFocus(orderInfo as DbOrderInfo) as logic
        _oRdd:CurrentOrder := self:FindOrder(orderInfo)
        return _oRdd:CurrentOrder != null

    method FindOrder(orderInfo as DbOrderInfo) as SqlDbOrder
        local selectedBag := null as SqlDbOrderBag
        var currentOrder := _oRdd:CurrentOrder
        var bagName   := orderInfo:BagName
        if (!String.IsNullOrEmpty(bagName))
            foreach var bag in self:_orderBags
                if String.Compare(bag:FileName, bagName, true) == 0
                    selectedBag := bag
                    exit
                endif
            next
        endif
        currentOrder := null
        if orderInfo:Order is long var iOrder
            if selectedBag != null
                currentOrder := selectedBag:FindTag(iOrder)
            else
                foreach var bag in _orderBags
                    currentOrder := bag:FindTag(iOrder)
                    if currentOrder != null
                        exit
                    endif
                    iOrder -= bag:Tags:Count
                next
            endif
        elseif orderInfo:Order is string var strOrder
            if selectedBag != null
               currentOrder := selectedBag:FindTag(strOrder)
            else
                foreach var bag in _orderBags
                    currentOrder :=  bag:FindTag(strOrder)
                    if currentOrder != null
                        exit
                    endif
                next
            endif
        endif
        return currentOrder
    method BuildSqlStatement(sWhereClause as string) as string
        var sb := System.Text.StringBuilder{}
        local scopeWhere := null as string
        var currentOrder := _oRdd:CurrentOrder
        sb:Append(Provider:QuoteIdentifier(self:_cTable))
        if currentOrder != null
            scopeWhere := currentOrder:GetScopeClause()
        endif
        if ! String.IsNullOrEmpty(sWhereClause) .or. ! String.IsNullOrEmpty(scopeWhere)
            if String.IsNullOrEmpty(scopeWhere)
                nop
            elseif String.IsNullOrEmpty(sWhereClause)
                sWhereClause := scopeWhere
            else
                sWhereClause += SqlDbProvider.AndClause + scopeWhere
            endif
            sWhereClause :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.WhereClause, _cTable, sWhereClause)
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(sWhereClause)
        endif
        if currentOrder != null
            sb:Append(Provider.OrderByClause)
            var cOrderby := List2String(currentOrder:OrderList)
            cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
            sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)
        endif
        var selectStmt := sb:ToString()
        sb:Clear()
        sb:Append(Provider:SelectTopStatement)
        sb:Replace(SqlDbProvider.TopCountMacro, _oInfo:MaxRecords:ToString())
        sb:Replace(SqlDbProvider.ColumnsMacro, self:ColumnList(_oInfo))
        sb:Replace(SqlDbProvider.TableNameMacro, selectStmt)

        return _connection:RaiseStringEvent(_connection, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
    method ColumnList(oInfo as SqlTableInfo) as string
        var sb := StringBuilder{}
        var list  := Dictionary<string, SqlDbColumnDef>{StringComparer.OrdinalIgnoreCase}
        var first := true
        foreach var c in oInfo:Columns
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(Provider.QuoteIdentifier(c:ColumnInfo:ColumnName))

            list:Add(c:ColumnInfo:ColumnName, c)
        next
        if !String.IsNullOrEmpty(oInfo:RecnoColumn)
            if !list:ContainsKey(oInfo:RecnoColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(oInfo:RecnoColumn))
                list:Add(oInfo:RecnoColumn, null)
            else
                var col := list[oInfo:RecnoColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Recno
            endif

        endif
        if !String.IsNullOrEmpty(oInfo:DeletedColumn)
            if !list:ContainsKey(oInfo:DeletedColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(oInfo:DeletedColumn))
                list:Add(oInfo:DeletedColumn,null)
            else
                var col := list[oInfo:DeletedColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Deleted
            endif
        endif
        return sb:ToString()
    method AdjustSelects(oInfo as SqlTableInfo) as void
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append(self:ColumnList(oInfo))
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(oInfo:Name))
        oInfo:SelectStatement := sb:ToString()
        sb:Append(SqlDbProvider.WhereClause)
        sb:Append("1=0")
        oInfo:EmptySelectStatement :=sb:ToString()
        return



end class
end namespace // XSharp.SQLRdd.SupportClasses
