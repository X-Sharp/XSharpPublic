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
    internal property TableInfo as SqlDbTableInfo get _oTable

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
                    oTag:Descending := tag:Descending
                    oBag:Add(oTag)
                next
                SELF:OrderBagList:Add(oBag)
                // The production index is rebuilt from the metadata on every open, so it is not
                // written to an .SDX file: nothing reads that file back, and writing it through
                // WritePrivateProfileString (which rewrites the whole file for every single key)
                // cost ~20-30ms per table open - the bulk of Open() for code that opens and
                // closes tables frequently.
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
        // A DBF/ADS key expression may legitimately reference the same physical column more
        // than once (e.g. IIF(EMPTY(DATUM), BUCHDATUM, DATUM)) - that's fine for evaluating the
        // key, but a SQL "CREATE INDEX ON table(col, col)" rejects the duplicate column name
        // outright, silently leaving the index missing (surfaces later as "Index nicht
        // vorhanden" the first time something tries to use it). Distinct() here only affects
        // the index's column list, not oTag:ColumnList itself, which other callers (e.g.
        // CalculateKeyLengthFromMetadata()) still need to see every occurrence, since that's
        // summing up how much buffer space the compound expression's actual result needs.
        var distinctColumns := oTag:ColumnList:Distinct(StringComparer.OrdinalIgnoreCase):ToList()
        var columnList := Functions.List2String(distinctColumns)
        // When there is a recno column then we want to add that,to make sure that duplicate keys
        // are sorted by record number
        if SELF:_oTable:HasRecnoColumn .and. !distinctColumns:Contains(Provider:QuoteIdentifier(SELF:_oTable:RecnoColumn), StringComparer.OrdinalIgnoreCase)
            columnList += ", "+ Provider:QuoteIdentifier(SELF:_oTable:RecnoColumn)
        endif
        sb:Replace(SqlDbProvider.FieldListMacro, columnList)
        var stmt := sb:ToString()
        var result := _connection:ExecuteNonQuery(stmt, _cTable)
        return result

    method SetProductionIndex() as logic
        _oRdd:CurrentOrder := null
        if self:OrderBagList:Count > 0
            var bag := self:OrderBagList:Where( {bag => bag:ProductionIndex}):FirstOrDefault()
            if (bag != null)
                _oRdd:CurrentOrder := bag:Tags:FirstOrDefault()
                _oRdd:CurrentOrder:ClearScopes()
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

    /// <summary>
    /// Build a fetch of nCount rows of the current order/scope/filter (further restricted by
    /// sWhereClause), starting at the 0-based row offset nOffset within that result.
    /// </summary>
    /// <param name="lReverse">TRUE to sort in reverse, for reading the rows before a given one.</param>
    method BuildSqlStatement(sWhereClause as string, nOffset as int, nCount as int, lReverse := false as logic) as string
        var sCallerWhere := sWhereClause
        var sb := System.Text.StringBuilder{}
        local scopeWhere := null as string
        var CurrentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append(self:ColumnList())
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RealName))
        if CurrentOrder != null
            scopeWhere := CurrentOrder:GetScopeClause()
            if ! String.IsNullOrEmpty(CurrentOrder:SqlWhere)
                whereClauses:Add(CurrentOrder:SqlWhere)
            endif
        endif
        if ! String.IsNullOrEmpty(sWhereClause)
            whereClauses:Add(sWhereClause)
        endif
        // todo: Add condition from conditional index
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
                    if !cOrderby.Contains(Provider:QuoteIdentifier(self:_oTable:RecnoColumn))
                        cOrderby := cOrderby + ", " + Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                    endif
                else
                    cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                endif
                if CurrentOrder:Descending
                    cOrderby += " DESC"
                endif
            endif
            if lReverse
                // per item, the items can be expressions with commas
                var aItems := List<string>{}
                foreach var cItem in CurrentOrder:OrderList
                    if cItem:EndsWith(" DESC", StringComparison.OrdinalIgnoreCase)
                        aItems:Add(cItem:Substring(0, cItem:Length - 5):TrimEnd())
                    else
                        aItems:Add(cItem + " DESC")
                    endif
                next
                if SELF:_oTable:HasRecnoColumn .and. !Functions.List2String(CurrentOrder:OrderList):Contains(Provider:QuoteIdentifier(self:_oTable:RecnoColumn))
                    aItems:Add(Provider:QuoteIdentifier(self:_oTable:RecnoColumn) + iif(CurrentOrder:Descending, "", " DESC"))
                endif
                cOrderby := Functions.List2String(aItems)
            endif
        elseif SELF:_oTable:HasRecnoColumn
            sb:Append(Provider:OrderByClause)
            cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
            if lReverse
                cOrderby += " DESC"
            endif
        endif
        cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
        sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)

        sb:Append(Provider:PagingClause)
        sb:Replace(SqlDbProvider.PagesizeMacro, nCount:ToString())
        sb:Replace(SqlDbProvider.StartRecMacro, nOffset:ToString())
        // the caller's condition, e.g. the keyset condition of a page (see SqlDbProvider.OrQueryHint)
        if !String.IsNullOrEmpty(sCallerWhere) .and. sCallerWhere:IndexOf(SqlDbProvider.OrClause, StringComparison.OrdinalIgnoreCase) >= 0 ;
            .and. Provider is SqlDbProvider var oProvider .and. !String.IsNullOrEmpty(oProvider:OrQueryHint) ;
            .and. _connection:ApproximateRows(self:_oTable:RealName) >= oProvider:OrQueryHintMinRows
            sb:Append(oProvider:OrQueryHint)
        endif

        return sb:ToString()

    /// <summary>
    /// Build a fetch of the last page of the current order/scope/filter, without the classic
    /// "large OFFSET" performance problem.
    /// </summary>
    /// <remarks>
    /// GoBottom() needs the last PageSize rows of a potentially huge result set. The normal
    /// paging query (BuildSqlStatement) would ask for that via `ORDER BY ... OFFSET (millions)
    /// ROWS FETCH NEXT PageSize ROWS`, which forces SQL Server to walk/skip almost the entire
    /// ordered result before it can return anything - the cost grows with table size even
    /// though the caller only wants a handful of rows. Sorting in reverse and asking for
    /// OFFSET 0 is always cheap regardless of table size; the caller is responsible for
    /// reversing the returned rows back into ascending order.
    /// </remarks>
    method BuildLastPageStatement(nRows := 0 as long) as string
        var sb := System.Text.StringBuilder{}
        local scopeWhere := null as string
        var CurrentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append(self:ColumnList())
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RealName))
        if CurrentOrder != null
            scopeWhere := CurrentOrder:GetScopeClause()
            if ! String.IsNullOrEmpty(CurrentOrder:SqlWhere)
                whereClauses:Add(CurrentOrder:SqlWhere)
            endif
        endif
        if ! String.IsNullOrEmpty(scopeWhere)
            whereClauses:Add(scopeWhere)
        endif
        if SELF:_oTable:HasServerFilter
            whereClauses:Add(_oTable:ServerFilter)
        endif
        var sWhereClause := self:CombineWhereClauses(whereClauses)
        sWhereClause := _connection:RaiseStringEvent(_connection, SqlRDDEventReason.WhereClause, _cTable, sWhereClause)
        if ! String.IsNullOrEmpty(sWhereClause)
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(sWhereClause)
        endif
        local cOrderby := "" AS string
        if CurrentOrder != null
            sb:Append(Provider.OrderByClause)
            foreach var cCol in CurrentOrder:OrderList
                if !String.IsNullOrEmpty(cOrderby)
                    cOrderby += ", "
                endif
                cOrderby += cCol + " DESC"
            next
            if SELF:_oTable:HasRecnoColumn
                var cRecnoCol := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                if ! String.IsNullOrEmpty(cOrderby)
                    if !cOrderby:Contains(cRecnoCol)
                        cOrderby := cOrderby + ", " + cRecnoCol + " DESC"
                    endif
                else
                    cOrderby := cRecnoCol + " DESC"
                endif
            endif
        elseif SELF:_oTable:HasRecnoColumn
            sb:Append(Provider:OrderByClause)
            cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn) + " DESC"
        endif
        cOrderby :=_connection:RaiseStringEvent(_connection, SqlRDDEventReason.OrderByClause, _cTable, cOrderby)
        sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)

        sb:Append(Provider:PagingClause)
        sb:Replace(SqlDbProvider.PagesizeMacro, iif(nRows > 0, nRows, _oTable:PageSize):ToString())
        sb:Replace(SqlDbProvider.StartRecMacro, "0")

        return sb:ToString()

    METHOD BuildRowNumberStatement(nRec as DWORD) AS STRING
        var sb := System.Text.StringBuilder{}

        // current filters, scopes and conditions must be respected
        // to calculate the correct pagenumber !
        var currentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        if SELF:_oTable:HasServerFilter
            whereClauses:Add(_oTable:ServerFilter)
        endif
        if currentOrder != null
            var sWhere := currentOrder:GetScopeClause()
            if !String.IsNullOrEmpty(sWhere)
                whereClauses:Add(sWhere)
            endif
            sWhere := currentOrder:SqlWhere
            if !String.IsNullOrEmpty(sWhere)
                whereClauses:Add(sWhere)
            endif
        endif
        var sWhereClause := SELF:CombineWhereClauses(whereClauses)
        sWhereClause := _connection:RaiseStringEvent(_connection, SqlRDDEventReason.WhereClause, _cTable, sWhereClause)

        sb:Append(Provider:RowNumberStatement)

        // not sure if this is a clever solution,
        // but WhereMacro is already used for nRec
        var cFromWhere := Provider:QuoteIdentifier(self:_oTable:RealName)
        if ! String.IsNullOrEmpty(sWhereClause)
            cFromWhere += SqlDbProvider.WhereClause+sWhereClause
        endif
        sb:Replace(SqlDbProvider.TableNameMacro, cFromWhere)

        var cOrderby := ""
        if currentOrder != null
            cOrderby := Functions.List2String(currentOrder:OrderList)
        endif
        if SELF:_oTable:HasRecnoColumn
            if ! String.IsNullOrEmpty(cOrderby)
                cOrderby := cOrderby + ", " + Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
                if currentOrder != null .and. currentOrder:Descending
                    cOrderby += " DESC"
                endif
            else
                cOrderby := Provider:QuoteIdentifier(self:_oTable:RecnoColumn)
            endif
        endif
        sb:Replace(SqlDbProvider.ColumnsMacro, cOrderby)
        sb:Replace(SqlDbProvider.RecordNumberMacro, self:_oTable:RecnoColumn)
        sb:Replace(SqlDbProvider.WhereMacro, nRec:ToString())
        return sb:ToString()

    /// <summary>
    /// Build a fetch of a single record by its physical recno, ignoring the current order's
    /// FOR-condition/scope and any filter.
    /// </summary>
    /// <remarks>
    /// DBF's GoTo() is a physical positioning operation: it must succeed even for a record that
    /// does not match the active order, unlike BuildRowNumberStatement/BuildSqlStatement which
    /// always fold the order's condition into the query. Found and OrderKeyNo separately reflect
    /// that the record has no valid position in the current order.
    /// </remarks>
    method BuildDirectRecnoStatement(nRec as DWORD) as string
        var sb := System.Text.StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append(self:ColumnList())
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RealName))
        sb:Append(SqlDbProvider.WhereClause)
        sb:Append(Provider:QuoteIdentifier(self:_oTable:RecnoColumn))
        sb:Append(" = ")
        sb:Append(nRec:ToString())
        return sb:ToString()


    /// <summary>The quoted column list of the table, recno and deleted column last.</summary>
    /// <remarks>Built once: every statement of the work area uses it, and for a table with 178 columns
    /// building it took a noticeable part of a seek on the client.</remarks>
    method ColumnList() as string
        if _columnList == null .or. _columnListCount != _oTable:Columns:Count
            _columnList := SELF:_BuildColumnList()
            _columnListCount := _oTable:Columns:Count
        endif
        return _columnList

    private _columnList as string
    private _columnListCount as long

    private method _BuildColumnList() as string
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


    method GetRecCount() AS DWORD
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append("count(*)")
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(_oTable:RealName))
        if _oTable:HasServerFilter
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(_oTable:ServerFilter)
        endif
        var stmt := sb:ToString()
        var result := _connection:ExecuteScalar(stmt, _cTable)
        return Convert.ToUInt32(result)

    method GetOrderKeyCount() AS DWORD
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        sb:Append("count(*)")
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(Provider.QuoteIdentifier(_oTable:RealName))

        var currentOrder := _oRdd:CurrentOrder
        var whereClauses := List<String>{}
        if SELF:_oTable:HasServerFilter
            whereClauses:Add(_oTable:ServerFilter)
        endif
        if currentOrder != null
            var sWhere := currentOrder:GetScopeClause()
            if !String.IsNullOrEmpty(sWhere)
                whereClauses:Add(sWhere)
            endif
            sWhere := currentOrder:SqlWhere
            if !String.IsNullOrEmpty(sWhere)
                whereClauses:Add(sWhere)
            endif
        endif
        var sWhereClause := SELF:CombineWhereClauses(whereClauses)
        if !String.IsNullOrEmpty(sWhereClause)
            sb:Append(SqlDbProvider.WhereClause)
            sb:Append(sWhereClause)
        endif

        var stmt := sb:ToString()
        var result := _connection:ExecuteScalar(stmt, _cTable)
        return Convert.ToUInt32(result)

    method GetMaxRecno() as DWORD
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
        return Convert.ToUInt32(result)

    method GetNextKey() as DWORD
        local maxVal := 0 as DWORD
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
end namespace // XSharp.RDD.SqlRDD
