//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


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
internal class SqlDbTableCommandBuilder
    protected _cTable           as string
    protected _oTable           as SqlDbTableInfo
    protected _oRdd             as SQLRDD
    protected _connection       as SqlDbConnection
    property Connection         as SqlDbConnection  get _connection
    property Provider           as ISqlDbProvider   get Connection:Provider
    Property MetadataProvider   as ISqlMetadataProvider get Connection:MetadataProvider
    property OrderBagList       as List<SqlDbOrderBag> get _oRdd:OrderBagList

    constructor(cTable as string, oRdd as SQLRDD)
        self:_cTable      := cTable
        self:_oRdd        := oRdd
        _connection       := oRdd:Connection
        return
    method FetchInfo(oRdd as SQLRDD) as SqlDbTableInfo
        // build initial information needed for SQL Query for a table
        local oTable as SqlDbTableInfo
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
        local oProdIndex := NULL as SqlDbIndexInfo
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

    method DropIndex(oTag as SqlDbOrder) as logic
        var sb      := StringBuilder{Provider:DropIndexStatement}
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(SELF:_oTable:RealName))
        sb:Replace(SqlDbProvider.IndexNameMacro, Provider:QuoteIdentifier(oTag:Name))
        var stmt := sb:ToString()
        var result := _connection:ExecuteNonQuery(stmt, _cTable)
        return result

    method CreateIndex(oTag as SqlDbOrder) as logic
        SELF:DropIndex(oTag)
        var sb      := StringBuilder{Provider:CreateIndexStatement}
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(SELF:_oTable:RealName))
        sb:Replace(SqlDbProvider.IndexNameMacro, Provider:QuoteIdentifier(oTag:Name))
        sb:Replace(SqlDbProvider.UniqueMacro, iif(oTag:Unique, " unique", ""))
        sb:Replace(SqlDbProvider.FieldListMacro, Functions.List2String(oTag:ColumnList))
        var stmt := sb:ToString()
        var result := _connection:ExecuteNonQuery(stmt, _cTable)
        return result

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
        local First := true as logic

        foreach var whereClause in whereList
            if First
                First := false
            else
                strResult += SqlDbProvider.AndClause
            endif
            strResult += "( " + whereClause + " )"
        next
        return strResult

    method BuildSqlStatement(sWhereClause as string) as string
        var sb := System.Text.StringBuilder{}
        local scopeWhere := null as string
        var CurrentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RealName))
        if CurrentOrder != null
            scopeWhere := CurrentOrder:GetScopeClause()
        endif
        if ! String.IsNullOrEmpty(sWhereClause)
            whereClauses:Add(sWhereClause)
        endif
        if ! String.IsNullOrEmpty(scopeWhere)
            whereClauses:Add(scopeWhere)
        endif
        if SELF:_oTable:HasServerFilter
            whereClauses:Add(_oTable:ServerFilter)
        endif
        sWhereClause := self:CombineWhereClauses(whereClauses)
        sWhereClause :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.WhereClause, _cTable, sWhereClause)
        if ! String.IsNullOrEmpty(sWhereClause)
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(sWhereClause)
        endif
        local cOrderby := "" AS string
        if CurrentOrder != null
            sb:Append(Provider.OrderByClause)
            cOrderby := Functions.List2String(CurrentOrder:OrderList)
            if SELF:_oTable:HasRecnoColumn
                if ! String.IsNullOrEmpty(cOrderby)
                    cOrderby := cOrderby + ", " + Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                else
                    cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                endif
            endif
            cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
        elseif SELF:_oTable:HasRecnoColumn
            cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
        endif
        cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
        sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)
        var selectStmt := sb:ToString()
        sb:Clear()
        sb:Append(Provider:SelectTopStatement)
        sb:Replace(SqlDbProvider.TopCountMacro, _oTable:MaxRecords:ToString())
        sb:Replace(SqlDbProvider.ColumnsMacro, self:ColumnList())
        sb:Replace(SqlDbProvider.TableNameMacro, selectStmt)
        return sb:ToString()

    method ColumnList() as string
        var sb := StringBuilder{}
        var List  := Dictionary<string, SqlDbColumnDef>{StringComparer.OrdinalIgnoreCase}
        var newColumns     := List<SqlDbColumnDef>{}
        var specialColumns := List<SqlDbColumnDef>{}
        var First := true
        var changedOrder := false
        foreach var c in _oTable:Columns
            if _oTable:HasRecnoColumn .and. String.Compare(_oTable:RecnoColumn, c:ColumnInfo:ColumnName, true) == 0
                specialColumns:Add(c)
                changedOrder := true
            elseif _oTable:HasDeletedColumn .and. String.Compare(_oTable:DeletedColumn, c:ColumnInfo:ColumnName, true) == 0
                specialColumns:Add(c)
                changedOrder := true
            else
                if First
                    First := false
                else
                    sb:Append(", ")
                endif
                sb:Append(Provider.QuoteIdentifier(c:ColumnInfo:ColumnName))
                List:Add(c:ColumnInfo:ColumnName, c)
                newColumns:Add(c)
            endif
        next
        foreach var c in specialColumns
            sb:Append(", ")
            sb:Append(Provider.QuoteIdentifier(c:ColumnInfo:ColumnName))
            List:Add(c:ColumnInfo:ColumnName, c)
            newColumns:Add(c)
        next
        if changedOrder
            _oTable:Columns:Clear()
            foreach var c in newColumns
                _oTable:Columns:Add(c)
            next
        endif

        if SELF:_oTable:HasRecnoColumn
            if !List:ContainsKey(_oTable:RecnoColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(_oTable:RecnoColumn))
                List:Add(_oTable:RecnoColumn, null)
            else
                var col := List[_oTable:RecnoColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Recno
            endif

        endif
        if SELF:_oTable:HasDeletedColumn
            if !List:ContainsKey(_oTable:DeletedColumn)
                sb:Append(", ")
                sb:Append(Provider.QuoteIdentifier(_oTable:DeletedColumn))
                List:Add(_oTable:DeletedColumn,null)
            else
                var col := List[_oTable:DeletedColumn]
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


    method GetRecCount() AS LONG
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append("count(*)")
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(_oTable:RealName))
        var stmt := sb:ToString()
        var result := _connection:ExecuteScalar(stmt, _cTable)
        return Convert.ToInt32(result)

    method GetMaxRecno() as LONG
        if ! _oTable:HasRecnoColumn
            return 0
        endif
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append("max(" +Provider.QuoteIdentifier(_oTable:RecnoColumn) +" )")
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(_oTable:RealName))
        var stmt := sb:ToString()
        var result := _connection:ExecuteScalar(stmt, _cTable)
        if result == DBNull.Value
            return 0
        endif
        return Convert.ToInt32(result)

    method GetNextKey() as LONG
        local maxVal := 0 as long
        if _oTable:HasRecnoColumn
            maxVal := self:GetMaxRecno()
        else
            maxVal := Self:GetRecCount()
        endif
        return maxVal + 1

    method ZapStatement() as STRING
        var sb := StringBuilder{}
        sb:Append(Provider:DeleteAllRowsStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        return sb.ToString()

    method PackStatement() as STRING
        var sb := StringBuilder{}
        sb:Append(Provider:DeleteStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        sb:Replace(SqlDbProvider.WhereMacro, _oTable:DeletedColumn + " = "+Provider:TrueLiteral)
        return sb.ToString()

end class
end namespace // XSharp.SQLRdd.SupportClasses
