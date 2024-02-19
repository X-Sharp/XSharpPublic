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
using XSharp.RDD.SqlRDD.Providers

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlDbTableCommandBuilder class.
/// </summary>
class SqlDbTableCommandBuilder
    protected _cTable as string
    protected _oTable as SqlTableInfo
    protected _oRdd  as SQLRDD
    protected _connection as SqlDbConnection
    property Connection as SqlDbConnection  get _connection
    property Provider   as SqlDbProvider    get Connection:Provider
    Property MetadataProvider as IMetadataProvider get Connection:MetadataProvider
    property OrderBagList as List<SqlDbOrderBag> get _oRdd:OrderBagList
    constructor(cTable as string, oRdd as SQLRDD)
        self:_cTable      := cTable
        self:_oRdd        := oRdd
        _connection       := oRdd:Connection
        return
    method FetchInfo(oRdd as SQLRDD) as SqlTableInfo
        // build initial information needed for SQL Query for a table
        local oTable as SqlTableInfo
        local cTable as string
        cTable := self:_cTable
        oTable := MetadataProvider:GetTableInfo(cTable)
        if String.IsNullOrEmpty(oTable:RealName)
            oTable:RealName := oTable:Name
        ENDIF
        var oTd := Connection:GetStructureForTable(oTable:RealName, oTable,oTable:ColumnList)
        self:_oTable := oTable
        oTable:CopyFromTd(oTd)
        self:AdjustSelects()
        self:OpenIndex(cTable)  // open production index
        return oTable

    method OpenIndex(cIndex as string) as void
        local oProdIndex := NULL as SqlIndexInfo
        foreach var index in _oTable:Indexes
            if String.Compare(index:Name, cIndex, true) == 0
                oProdIndex := index
                exit
            endif
        next
        if oProdIndex != null
            if oProdIndex:Tags:Count > 0
                var oBag := SqlDbOrderBag{_oRdd, cIndex}
                oBag:ProductionIndex := true
                oBag:LogicalName := cIndex
                oBag:FileName := System.IO.Path.GetFileNameWithoutExtension(_oRdd:FileName)
                foreach var tag in oProdIndex:Tags
                    var oTag := SqlDbOrder{_oRdd, tag:Name, tag:Expression, oBag}
                    if !String.IsNullOrEmpty(tag:Condition)
                        oTag:SetCondition(tag:Condition)
                    endif
                    oBag:Add(oTag)
                next
                SELF:OrderBagList:Add(oBag)
                oBag:Save()
            endif
        endif
    method SetProductionIndex() as logic
        _oRdd:CurrentOrder := null
        if self:OrderBagList:Count > 0
            var bag := self:OrderBagList:Where( {bag => bag:FileName == _cTable}):FirstOrDefault()
            if (bag != null)
                bag:ProductionIndex := TRUE
                _oRdd:CurrentOrder := bag:Tags:FirstOrDefault()
                return true
            endif
        endif
        return false

    method CombineWhereClauses(whereList as List<String>) as string
        if whereList:Count == 0
            return ""
        endif
        if whereList:Count == 1
            return whereList:First()
        endif
        local strResult := "" as string
        local first := true as logic

        foreach var whereClause in whereList
            if first
                first := false
            else
                strResult += SqlDbProvider.AndClause
            endif
            strResult += "( " + whereClause + " )"
        next
        return strResult
    method BuildSqlStatement(sWhereClause as string) as string
        var sb := System.Text.StringBuilder{}
        local scopeWhere := null as string
        var currentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RealName))
        if currentOrder != null
            scopeWhere := currentOrder:GetScopeClause()
        endif
        if ! String.IsNullOrEmpty(sWhereClause)
            whereClauses:Add(sWhereClause)
        endif
        if ! String.IsNullOrEmpty(scopeWhere)
            whereClauses:Add(scopeWhere)
        endif
        if ! String.IsNullOrEmpty(_oTable:ServerFilter)
            whereClauses:Add(_oTable:ServerFilter)
        endif
        sWhereClause := self:CombineWhereClauses(whereClauses)
        sWhereClause :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.WhereClause, _cTable, sWhereClause)
        if ! String.IsNullOrEmpty(sWhereClause)
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(sWhereClause)
        endif
        if currentOrder != null
            sb:Append(Provider.OrderByClause)
            var cOrderby := XSharp.SQLRDD.Functions.List2String(currentOrder:OrderList)
            cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
            sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)
        endif
        var selectStmt := sb:ToString()
        sb:Clear()
        sb:Append(Provider:SelectTopStatement)
        sb:Replace(SqlDbProvider.TopCountMacro, _oTable:MaxRecords:ToString())
        sb:Replace(SqlDbProvider.ColumnsMacro, self:ColumnList())
        sb:Replace(SqlDbProvider.TableNameMacro, selectStmt)
        return sb:ToString()
    method ColumnList() as string
        var sb := StringBuilder{}
        var list  := Dictionary<string, SqlDbColumnDef>{StringComparer.OrdinalIgnoreCase}
        var first := true
        foreach var c in _oTable:Columns
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(Provider.QuoteIdentifier(c:ColumnInfo:ColumnName))

            list:Add(c:ColumnInfo:ColumnName, c)
        next
        if !String.IsNullOrEmpty(_oTable:RecnoColumn)
            if !list:ContainsKey(_oTable:RecnoColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(_oTable:RecnoColumn))
                list:Add(_oTable:RecnoColumn, null)
            else
                var col := list[_oTable:RecnoColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Recno
            endif

        endif
        if !String.IsNullOrEmpty(_oTable:DeletedColumn)
            if !list:ContainsKey(_oTable:DeletedColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(_oTable:DeletedColumn))
                list:Add(_oTable:DeletedColumn,null)
            else
                var col := list[_oTable:DeletedColumn]
                if col != null
                    col:ColumnFlags |= SqlDbColumnFlags.Deleted
                endif
            endif
        endif
        return sb:ToString()
    method AdjustSelects() as void
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append(self:ColumnList())
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(_oTable:RealName))
        _oTable:SelectStatement := sb:ToString()
        sb:Append(SqlDbProvider.WhereClause)
        sb:Append("1=0")
        _oTable:EmptySelectStatement :=sb:ToString()
        return



end class
end namespace // XSharp.SQLRdd.SupportClasses
