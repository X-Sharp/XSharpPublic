//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.RDD.Enums
using XSharp.RDD.Support
using System.IO
using System.Collections.Generic
using System.Data
using System.Text
using System.Diagnostics
using System.Linq
using System.Reflection
using System.Data.Common
using XSharp.RDD.SqlRDD.Providers

#undef TRACERDD
begin namespace XSharp.RDD.SqlRDD

// Private methods and fields
partial class SQLRDD

    private _table          as DataTable
    private _phantomRow     as DataRow
    private _creatingIndex  as logic
    private _tableMode      as TableMode
    private _hasData        as logic
    private _hasEOF         as logic
    private _currentPageNo  as long
    private _firstPageNo    as long
    private _connection     as SqlDbConnection
    private _oTd            as SqlDbTableInfo
    private _builder        as SqlDbTableCommandBuilder
    private _command        as SqlDbCommand
    private _trimValues     as logic
    private _creating       as logic
    private _cTable         as string
    private _emptyValues    as object[]
    private _updatableColumns as List<RddFieldInfo>
    private _keyColumns     as List<RddFieldInfo>
    private _updatedRecNos  as List<int>
    /// <summary>Recnos of rows marked for deletion via Delete() when there is no DeletedColumn on the table.
    /// Workarea.Deleted is a hardcoded stub (always FALSE), so this state cannot be tracked in the base class.</summary>
    private _deletedRowIds  as HashSet<int>
    private _orderBagList   as List<SqlDbOrderBag>
    private _rowNumber      as long

    /// <summary>
    /// 0 based Column Number for the column that has the deleted flag
    /// </summary>
    private _deletedColumnNo  as long
    private _deletedColumnIsLogic as LOGIC
    /// <summary>
    /// 0 based Column Number for the column that has the record number
    /// </summary>
    private _recnoColumNo   as long
        // private _recordKeyCache as Dictionary<dword, dword>

    private _numHiddenColumns as long
    private _serverReccount as dword
    /// <summary>TRUE when _serverReccount already reflects the current order/scope/data and
    /// does not need to be recomputed. Cleared whenever the underlying data, order or scope
    /// actually changes (_CloseCursor(), a fresh _OpenTable() fetch, or a write in GoCold()).</summary>
    private _serverReccountValid as logic
    /// <summary>
    /// TRUE when the cursor is positioned (via GoTo()) on a record that physically exists but
    /// does not satisfy the current order's FOR-condition/scope, i.e. OrderKeyNo is 0. Skip()
    /// needs this because RowNumber/_currentPageNo no longer correspond to any real position in
    /// the order's sequence, so the normal relative-skip logic cannot be used from here.
    /// </summary>
    private _outsideOrder as logic

#region Properties
    internal property Connection     as SqlDbConnection get _connection
    internal property Provider       as ISqlDbProvider get _connection?:Provider
    internal property Command        as SqlDbCommand get _command
    internal property OrderBagList   as List<SqlDbOrderBag> get _orderBagList
    internal property CurrentPage    as int => _currentPageNo
    internal property DataTable      as DataTable
        get
            return _table
        end get
        set
            // When we get here then the (temporary) DBFVFP table has already been created and opened
            // and the fields are already read from the DBF header in the temporary table
            // The SqlStatement:CreateFile() method whichs gets called from SqlExec()
            // has the logic that creates the DBF from the Column properties
            //
            if value == null .and. _table != null
                _table:Rows:Clear()
                return
            endif
            _table := value
            if _table is NULL
                SELF:_hasData := FALSE
                return
            endif
            SELF:_hasData := TRUE
            self:_phantomRow 	:= _table:NewRow()
            var prop := _table:GetType():GetProperty("EnforceConstraints", BindingFlags.Instance+BindingFlags.NonPublic)
            if prop != null
                prop:SetValue(_table, false)
            endif
            foreach oColumn as DataColumn in _table:Columns
                var index := oColumn:Ordinal

                local dbColumn := self:_Fields[index] as RddFieldInfo
                // use the BlankValue() from the RddFieldInfo class. One place to define blanks is enough
                var blank := dbColumn:BlankValue()
                if blank is string var strBlank
                    blank := strBlank:PadRight(dbColumn:Length, ' ')
                endif
                self:_phantomRow[index] := blank
                dbColumn:Caption     := oColumn:Caption
                if oColumn:AutoIncrement .or. oColumn:ColumnName == SELF:_oTd:RecnoColumn
                    _recnoColumNo := oColumn:Ordinal
                    oColumn:ReadOnly := false
                endif
                if !oColumn:AllowDBNull
                    oColumn:AllowDBNull := true
                endif
                dbColumn:Flags := DBFFieldFlags.None
            next
            SELF:RowNumber  := 1
            SELF:_CheckEofBof()
        end set
    end property

#endregion

    constructor()
        super()
        SELF:_creatingIndex    := false
        SELF:_tableMode        := TableMode.Query
        SELF:_ReadOnly         := true
        SELF:_connection       := null
        SELF:_builder          := null
        SELF:_deletedColumnNo  := -1
        SELF:_recnoColumNo     := -1
        SELF:_currentPageNo    := 1
        SELF:_firstPageNo      := 1
        self:_trimValues       := true // trim String Valuess
        SELF:_updatedRecNos    := List<int>{}
        SELF:_deletedRowIds    := HashSet<int>{}
        SELF:_keyColumns       := List<RddFieldInfo>{}
        SELF:_updatableColumns := List<RddFieldInfo>{}
        SELF:_orderBagList     := List<SqlDbOrderBag>{}
        SELF:_IsFileBased      := FALSE
        return
    end constructor

    destructor()
        Command?:Dispose()
        // Mirror the explicit Close() path (SQLRDD-Main.prg): unregister from the
        // shared connection so a leaked/finalized work area only closes the physical
        // connection when it was truly the last one AND KeepOpen is off. Calling
        // Dispose() here instead used to force-close and deregister the shared
        // SqlDbConnection unconditionally, ignoring KeepOpen, whenever this work area
        // happened to be the last one registered at finalization time - killing the
        // connection for every other still-open table on the same connection.
        _connection?:UnregisterRdd(self)
    end destructor

    internal method _ClearTable() AS VOID
        IF SELF:DataTable != null
            SELF:DataTable:Rows:Clear()
        ENDIF
        RETURN


    private method _GetTableInfo(cTable as string) as logic
        // First check to see if there is a tableDef for this table in the connection
        self:_builder   := SqlDbTableCommandBuilder{cTable, self}
        self:_cTable    := cTable
        var info        := _builder:FetchInfo(self)
        self:_oTd       := info
        self:_trimValues:= info:TrimTrailingSpaces
        if XSharp.RuntimeState.AutoOpen
            _builder:SetProductionIndex()
        endif
        return true
    end method

    private method _PrepareOpen(info as DbOpenInfo) as logic
        var query := info:FileName
        local strConnection as string
        local pos as int
        strConnection := SqlDbConnection.DefaultConnection
        _connection := Functions.SqlDbGetConnection(strConnection)
        if _connection == null
            return false
        endif
        _connection:RegisterRdd(SELF)
        _command    := SqlDbCommand{info:Alias, _connection}
        pos := query:IndexOf(SqlDbProvider.ConnectionDelimiter)
        if pos > 0
            strConnection := query:Substring(0, pos)
            var oNewConn := Functions.SqlDbGetConnection(strConnection)
            if oNewConn == null
                return false
            endif
            _connection := oNewConn
            query := query:Substring(pos+2)
            info:FileName := query
        endif
        return true
    end method

    private method _CreateSqlFields(aFields as RddFieldInfo[]) as logic
        var sb := System.Text.StringBuilder{}
        var first := true
        foreach var fld in aFields
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(self:Provider:GetSqlColumnInfo(fld, SELF:Connection))
        next
        var columns := sb:ToString()
        sb:Clear()
        sb:Append(Provider:DropTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider.QuoteIdentifier(_cTable))
        _command:CommandText := sb:ToString()
        _command.ExecuteScalar()
        sb:Clear()
        sb:Append(Provider:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider.QuoteIdentifier(_cTable))
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, columns)
        _command.CommandText := sb:ToString()
        _command.ExecuteScalar()
        return true
    end method


    private method _GetEmptyValues() as logic
        var values := List<object>{}
        foreach col as DataColumn in self:DataTable:Columns
            if col:AutoIncrement .or. col:ColumnName == SELF:_oTd:RecnoColumn
                values.Add(DBNull.Value)
                loop
            endif
            switch Type.GetTypeCode(col.DataType)
            case TypeCode.String
                values.Add("")
            case TypeCode.Byte
            case TypeCode.Char
            case TypeCode.Double
            case TypeCode.Single
            case TypeCode.Int16
            case TypeCode.Int32
            case TypeCode.Int64
            case TypeCode.UInt16
            case TypeCode.UInt32
            case TypeCode.UInt64
            case TypeCode.Decimal
            case TypeCode.SByte
                values.Add(0)
            case TypeCode.Boolean
                values.Add(false)
            case TypeCode.DateTime
            case TypeCode.Object
            case TypeCode.DBNull
            case TypeCode.Empty
            otherwise
                values.Add(DBNull.Value)
            end switch
        next
        _emptyValues := values:ToArray()
        return true
    end method

    private method _GetColumn(nFldPos as long) as RddFieldInfo
        local nArrPos := nFldPos -1 as long
        IF nArrPos >= 0 .AND. nArrPos < self:_Fields:Length
            return self:_Fields[ nArrPos ]
        endif
        self:_dbfError(EDB_FIELDINDEX, EG_ARG,"SQLRDD:_GetColumn", "Invalid field index")
        return null
    end method


    private method _ExecuteInsertStatement(row as DataRow) as logic
        var sbColumns  := StringBuilder{}
        var sbValues   := StringBuilder{}
        local iCounter := 1 as long
        _command:ClearParameters()
        foreach c as DataColumn in DataTable:Columns
            if c:AutoIncrement .or. c:ColumnName == SELF:_oTd:RecnoColumn
                loop
            endif
            if sbColumns:Length > 0
                sbColumns:Append(", ")
                sbValues:Append(", ")
            endif
            sbColumns:Append(Provider.QuoteIdentifier(c:ColumnName))
            var name := i"@p{iCounter}"
            sbValues:Append(name)
            _command:AddParameter(name, row[c])
            ++iCounter
        next
        var sb := StringBuilder{}
        sb:Append(Provider:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        sb:Replace(SqlDbProvider.ColumnsMacro, sbColumns:ToString())
        sb:Replace(SqlDbProvider.ValuesMacro, sbValues:ToString())
        var hasGetIdentity := false
        if self:_recnoColumNo != -1
            hasGetIdentity := true
        endif
        try
            _command:Connection:BeginTrans()
            _command:BindParameters()
            local lInsertWithGetIdentity := false as LOGIC
            if hasGetIdentity .and. ! String.IsNullOrEmpty(Provider:GetIdentity)
                sb:Append("; ")
                sb:Append(Provider:GetIdentity)
                lInsertWithGetIdentity := TRUE
            endif
            _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
            if lInsertWithGetIdentity
                var result := _command:ExecuteScalar()
                row[_recnoColumNo] := result
            elseif hasGetIdentity
                _command:ExecuteScalar()
                row[_recnoColumNo] := _builder:GetMaxRecno()
            else
                _command:ExecuteNonQuery()
            endif
            _command:Connection:CommitTrans()
        catch
            _command:Connection:RollBackTrans()
        end try
        return true
    end method

    private method _GetWhereClause(row as DataRow) as string
        var sbWhere    := StringBuilder{}
        local iCounter := 1 as long
        foreach var c in self:_keyColumns
            if sbWhere:Length > 0
                sbWhere:Append(SqlDbProvider.AndClause)
            endif
            sbWhere:Append(Provider:QuoteIdentifier(c:ColumnName))
            var colValue := row[c:ColumnName,DataRowVersion.Original]
            if colValue == DBNull.Value .or. colValue is null
                sbWhere:Append(" is null")
            else
                var oldname := i"@o{iCounter}"
                sbWhere:Append(" = ")
                sbWhere:Append(oldname)
                _command:AddParameter(oldname, colValue)
            endif
            ++iCounter
        next
        _command:BindParameters()
        return sbWhere:ToString()
    end method

    private method _ExecuteUpdateStatement(row as DataRow) as logic
        var sbColumns  := StringBuilder{}
        local iCounter := 1 as long
        local lColumnChanged := false as logic
        _command:ClearParameters()
        foreach c as DataColumn in DataTable:Columns
            if c:AutoIncrement .or. c:ColumnName == SELF:_oTd:RecnoColumn
                loop
            endif
            if !self:_oTd:UpdateAllColumns
                // only update columns that are changed
                local isEqual := true as Logic
                if row[c, DataRowVersion.Original] != DBNull.Value .and. row[c, DataRowVersion.Current] != DBNull.Value
                    isEqual := row[c, DataRowVersion.Original].ToString().Trim().Equals(row[c, DataRowVersion.Current].ToString().Trim())
                else
                    isEqual := row[c, DataRowVersion.Original].Equals(row[c, DataRowVersion.Current])
                endif
                if isEqual
                    loop
                endif
                lColumnChanged := true
            endif
            var name    := i"@p{iCounter}"
            if sbColumns:Length > 0
                sbColumns:Append(", ")
            endif
            sbColumns:Append(Provider:QuoteIdentifier(c:ColumnName))
            sbColumns:Append(" = ")
            sbColumns:Append(name)
            _command:AddParameter(name, row[c])
            ++iCounter
        next
        if !lColumnChanged .and. !self:_oTd:UpdateAllColumns
            // no columns changed, so we do not update this row
            return TRUE
        endif
        var strWhere := SELF:_GetWhereClause(row)
        var sb := StringBuilder{}
        sb:Append(Provider:UpdateStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        sb:Replace(SqlDbProvider.ColumnsMacro, sbColumns:ToString())
        sb:Replace(SqlDbProvider.WhereMacro, strWhere)
        var hasRowCount := false
        if ! String.IsNullOrEmpty(Provider:GetRowCount)
            sb:Append("; ")
            sb:Append(Provider:GetRowCount)
            hasRowCount := true
        endif
        _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
        var res := _command:ExecuteScalar()
        if (hasRowCount)
            var i64 := (int64) Convert.ChangeType(res, typeof(int64))
            IF i64 == 1
                return true
            else
                return false
            endif
        endif
        return true
    end method

    private method _HandleNullDate(oValue as object, oCol as DataColumn) as object
        if oValue is DateTime var dt .and. dt == DateTime.MinValue
            return DBNull.Value
        elseif oValue is IDate var d
            if d:IsEmpty
                return DBNull.Value
            else
                return DateTime{d:Year, d:Month, d:Day}
            endif
        elseif oValue is null .and. oCol:DataType == typeof(DateTime)
            return DBNull.Value
        endif
        return oValue
    end method

    private method _ExecuteDeleteStatement(row as DataRow) as logic
        // TODO: check config if logic or physical delete
        _command:ClearParameters()
        var strWhere := SELF:_GetWhereClause(row)
        var sb := StringBuilder{}
        sb:Append(Provider:DeleteStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        sb:Replace(SqlDbProvider.WhereMacro, strWhere)
        var hasRowCount := false
        if ! String.IsNullOrEmpty(Provider:GetRowCount)
            sb:Append("; ")
            sb:Append(Provider:GetRowCount)
            hasRowCount := true
        endif
        _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
        var res := _command:ExecuteScalar()
        if (hasRowCount)
            return res is int var i .and. i == 1
        endif
        return true
    end method

    /// <summary>
    /// Fetch the last page of the current order/scope/filter directly, without going through
    /// the ascending, huge-OFFSET query GoBottom() would otherwise need for a large table.
    /// </summary>
    /// <remarks>See SqlDbTableCommandBuilder.BuildLastPageStatement for why this exists.</remarks>
    private method _FetchLastPage(nPage as int) as logic
        try
            SELF:_command:CommandText := _builder:BuildLastPageStatement()
            SELF:_command:ClearParameters()
            var newTable := SELF:_command:GetDataTable(SELF:Alias)
            if newTable == null
                return false
            endif
            // The query is sorted DESCENDING (to avoid the large OFFSET) - insert forwards at
            // position 0 so the rows end up in the normal ascending order the rest of the RDD
            // (Skip, RecNo lookups, ...) expects from the buffer.
            for var nRow := 0 upto newTable:Rows:Count-1
                var row := newTable:Rows[nRow]
                var newRow := SELF:DataTable:NewRow()
                newRow:ItemArray := row:ItemArray
                SELF:DataTable:Rows:InsertAt(newRow, 0)
                newRow:AcceptChanges()
            next
            SELF:_currentPageNo := nPage
            SELF:_firstPageNo := nPage
            // This IS the last page by definition - without this, a Skip(1) right after
            // GoBottom() (e.g. GoTo(0)'s GoBottom()+Skip(1)) would not know it's already at
            // the end, and would fall through to fetching "the next page" via the normal
            // ascending, huge-OFFSET query - the exact cost this method exists to avoid.
            SELF:_hasEOF := true
            return true
        catch as Exception
            return false
        end try
    end method

    protected method _ForceOpen() as logic
        if self:_tableMode != TableMode.Table
            return true
        endif
        if self:_creating
            return true
        endif
        if self:_hasData
            return true
        endif
        return self:_OpenTable("")
    end method

    private method _ReadTable(sWhereClause as string) as DataTable
        try
            _command:CommandText := self:_BuildSqlStatement(sWhereClause)
            _command:ClearParameters()
            var oDataTable := _command:GetDataTable(self:Alias)
            return oDataTable
        catch as Exception
            return null
        end try

    private method _OpenTable(sWhereClause as string) as logic
        try
            SELF:_currentPageNo := 1
            SELF:_firstPageNo := 1
            // A fresh open/reposition (e.g. from Seek()) must not inherit a stale "no more
            // rows" flag left over from whatever this buffer was doing before - otherwise
            // Skip() refuses to fetch the next page and reports EOF even though the new
            // WHERE clause has plenty more rows past the first page.
            SELF:_hasEOF := false
            SELF:DataTable := self:_ReadTable(sWhereClause)
            if SELF:DataTable == null
                // _ReadTable() -> Command:GetDataTable()/ExecuteReader() swallow the real
                // ADO.NET exception into Connection:LastException instead of throwing it.
                // Without this check we'd return TRUE here with no data loaded, and the
                // first caller to touch DataTable (GoTo(), GoTop(), ...) would crash with
                // a bare NullReferenceException that hides the actual database error.
                self:_dbfError(self:Connection:LastException, Subcodes.EDB_USE, Gencode.EG_OPEN, "SQLRDD._OpenTable", FALSE)
                return false
            endif
            // A fresh fetch always means a (potentially) different row count - force a real
            // requery here rather than trusting whatever was cached before this WHERE clause.
            self:_serverReccountValid := false
            self:_GetRecCount()
        catch as Exception
            return false
        end try
        return true
    end method


    private method _BuildSqlStatement(sWhereClause as string) as string
        local query as string
        if self:_tableMode == TableMode.Table
            query := _builder:BuildSqlStatement(sWhereClause)
        else
            query := self:_oTd:SelectStatement
        endif
        return query
    end method


    PRIVATE METHOD _CheckEofBof() AS VOID
        VAR nRecs := SELF:RowCount

        IF nRecs == 0
            SELF:_SetEOF(TRUE)
            SELF:_SetBOF(TRUE)
        ELSEIF SELF:RowNumber > nRecs
            SELF:_SetBOF(FALSE)
            SELF:_SetEOF(TRUE)
        ELSE
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(FALSE)
        ENDIF
    END METHOD

    INTERNAL METHOD _SetEOF(lNewValue AS LOGIC) AS VOID
        IF lNewValue != SELF:_EoF
            SELF:_EoF := lNewValue
        ENDIF
    INTERNAL METHOD _SetBOF(lNewValue AS LOGIC) AS VOID
        IF lNewValue != SELF:_BoF
            SELF:_BoF := lNewValue
        ENDIF

    PRIVATE METHOD _adjustCreateFields(aFields AS RddFieldInfo[]) AS RddFieldInfo[]
        local fields := aFields:ToList() as List<RddFieldInfo>
        if ! String.IsNullOrEmpty(SELF:Connection:RecnoColumn)
            var found := false
            foreach var field in fields
                if String.Compare(field:ColumnName, SELF:Connection:RecnoColumn, true) == 0
                    found := true
                endif
            next
            if ! found
                var fld := RddFieldInfo{SELF:Connection:RecnoColumn,"I:+",4,0}
                fld:Flags |= DBFFieldFlags.System
                fields:Add( fld )
                SELF:_RecordLength += 4
            endif
        endif
        if ! String.IsNullOrEmpty(SELF:Connection:DeletedColumn)
            var found := false
            foreach var field in fields
                if String.Compare(field:ColumnName, SELF:Connection:DeletedColumn, true) == 0
                    found := true
                endif
            next
            if ! found
                var fld  := RddFieldInfo{SELF:Connection:DeletedColumn,"L",1,0}
                fld:Flags |= DBFFieldFlags.System
                fields:Add( fld )
                SELF:_RecordLength += 1
            endif
        endif
        return fields:ToArray()
    end method

    private method _CloseCursor() as void
        self:_hasData       := FALSE
        self:_table         := null
        // Whatever the cached count reflected (a prior WHERE clause, order or scope) no longer
        // applies once the cursor is torn down - force the next _GetRecCount() to requery.
        self:_serverReccountValid := false
        return

    private method _GetRecCount() as void
        // Skip the round trip entirely when nothing has changed since the last real count.
        // GoBottom() calls this every time it runs, and callers like Lister's GoTo(0)/GoBottom()
        // handling can retry that many times in a row against the same unchanged data (observed:
        // ~29 repeats, each paying for a fresh COUNT(*) on a large table) - one real query is
        // enough until _OpenTable()/GoCold()/_CloseCursor() actually invalidate the cache.
        if self:_serverReccountValid
            return
        endif
        // Must respect the current order's scope/condition, same as GoBottom() already does -
        // otherwise a scope/seek-scoped browse (e.g. one city's streets) gets its RecCount
        // silently overwritten with the whole unscoped table's count the moment anything
        // triggers a recount (GoCold() does, on every flush of a "hot" row), corrupting the
        // page/EOF math for the rest of the browse.
        if self:CurrentOrder == null
            self:_serverReccount := self:_builder:GetRecCount()
        else
            self:_serverReccount := self:OrderKeyCount
        endif
        self:_serverReccountValid := true
    end method

    private method _FetchPage(nNewPageNo as int ) as logic
        if self:_tableMode != TableMode.Table
            // Exception?
            return true
        endif
        var maxTableSize := _oTd:PageSize * _oTd:BufferSize
        var sizeBefore   := maxTableSize - _oTd:PageSize
        var lForward     := nNewPageNo >= _currentPageNo
        if self:RowCount + _oTd:PageSize > maxTableSize
            // We must delete rows
            if lForward
                // delete rows at the start
                DO WHILE self:RowCount > sizeBefore
                    self:DataTable:Rows:RemoveAt(0)
                ENDDO
                SELF:_firstPageNo += 1
            else
                // delete rows at the end
                DO WHILE self:RowCount > sizeBefore
                    self:DataTable:Rows:RemoveAt(DataTable:Rows:Count-1)
                ENDDO
                IF SELF:_firstPageNo > 1
                    SELF:_firstPageNo -= 1
                ELSE
                    SELF:_firstPageNo := 1
                endif
            endif
        endif
        _currentPageNo := nNewPageNo
        SELF:_hasEOF := false
        var newTable := SELF:_ReadTable("")
        var result := (newTable != null)
        if result
            if lForward
                SELF:RowNumber := SELF:DataTable:Rows:Count + 1

                foreach row as DataRow in newTable:Rows
                    var newRow := SELF:DataTable:NewRow()
                    newRow:ItemArray := row:ItemArray
                    SELF:DataTable:Rows:Add(newRow)
                    newRow:AcceptChanges()
                next
            else
                SELF:RowNumber := newTable:Rows:Count

                for var nRow := newTable:Rows:Count-1 downto 0
                    var row := newTable:Rows[nRow]
                    var newRow := SELF:DataTable:NewRow()
                    newRow:ItemArray := row:ItemArray
                    SELF:DataTable:Rows:InsertAt(newRow, 0)
                    newRow:AcceptChanges()
                next
            endif
            // A short page always means EOF. But when the total record count is an exact
            // multiple of PageSize, the last page comes back FULL - "short page" never fires,
            // so also check whether this page's absolute record range already reaches the
            // known total. Without this, sequential forward paging (unlike GoBottom(), which
            // jumps straight to the last page and marks it via _FetchLastPage) never sets
            // _hasEOF on that exactly-full last page: the next Skip() then fetches a
            // nonexistent page past it, landing on a bogus RowNumber instead of staying put.
            var nAbsoluteRowsSeen := ((nNewPageNo - 1) * _oTd:PageSize) + newTable:Rows:Count
            if lForward .and. (newTable:Rows:Count < _oTd:PageSize .or. nAbsoluteRowsSeen >= SELF:_serverReccount)
                SELF:_hasEOF := true
            else
                _currentPageNo := nNewPageNo
            endif
        endif
        return result

    PRIVATE METHOD _GotoRecord(nRec as DWORD) AS LOGIC
        // GoTo() only calls _GotoRecord() after it already established that nRec is NOT in
        // the currently loaded buffer. Whether that buffer happens to be empty or merely
        // contains the wrong rows makes no difference - either way we must locate and load
        // the page that actually contains nRec, so this brute walk must always run.
        SELF:_command:CommandText := _builder:BuildRowNumberStatement(nRec)
        var result := SELF:_command:ExecuteScalar(SELF:_oTd:Name)
        if result == null .or. result == DBNull.Value
            // nRec does not satisfy the current order's FOR-condition/scope. DBF's GoTo() is a
            // physical positioning operation, independent of the active order: it must still
            // succeed when the record exists at all - Found/OrderKeyNo separately reflect that
            // it has no valid position in this order.
            return SELF:_GotoRecordOutsideOrder(nRec)
        endif
        var iResult := Convert.ToInt64(result)

        // determine correct page
        SELF:_currentPageNo := (INT) ((iResult - 1) / SELF:_oTd:PageSize) + 1
        // This is a freshly loaded page - whether it happens to be the last one needs to be
        // re-determined from here, not inherited from whatever a previous, unrelated GoBottom()
        // (e.g. on a completely different page) left behind. Without this, a stale _hasEOF=true
        // makes every subsequent forward Skip() from this page falsely believe it's already at
        // the end and never fetch the next page.
        SELF:_hasEOF := false
        SELF:_ClearTable()
        SELF:DataTable := SELF:_ReadTable("")

        // locate the row in the page
        SELF:RowNumber := 1
        DO WHILE SELF:RowNumber <= SELF:DataTable:Rows:Count
            IF SELF:RecNo == nRec
                RETURN TRUE
            ENDIF
            SELF:RowNumber+= 1
        ENDDO
        RETURN FALSE

    PRIVATE METHOD _GotoRecordOutsideOrder(nRec as DWORD) AS LOGIC
        try
            SELF:_command:CommandText := _builder:BuildDirectRecnoStatement(nRec)
            SELF:_command:ClearParameters()
            var oTable := SELF:_command:GetDataTable(SELF:Alias)
            if oTable == null .or. oTable:Rows:Count == 0
                // Does not exist even physically.
                return false
            endif
            SELF:_ClearTable()
            SELF:DataTable := oTable
            SELF:RowNumber := 1
            SELF:_outsideOrder := true
            SELF:_hasEOF := false
            return true
        catch as Exception
            return false
        end try

    PRIVATE METHOD _GotoRow(nRow as LONG) AS LOGIC
        SELF:_Found := FALSE
        // DataTable can be null here for a Query-mode table whose SELECT failed (see Open())
        // and that has no recno column to route through _GotoRecord() instead - treat that
        // the same as an empty result set rather than crashing.
        var nCount := IIF(SELF:DataTable == null, 0, SELF:DataTable:Rows:Count)
        IF  nRow <= nCount  .AND.  nRow > 0
            SELF:RowNumber := nRow
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(FALSE)
        ELSEIF nRow < 0 .AND. nCount > 0
            // skip to BOF. Move to record 1.
            SELF:RowNumber := 1
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(TRUE)
        ELSE
            // File empty, or move after last record
            SELF:RowNumber := nCount + 1
            SELF:_SetEOF(TRUE)
            SELF:_SetBOF(nCount == 0)
        ENDIF
        IF SELF:_Relations:Count != 0
            SELF:SyncChildren()
        ENDIF
        SELF:_CheckEofBof()
        RETURN TRUE

    PRIVATE METHOD _UpdateRow(nRecNo AS INT) AS LOGIC
        local row as DataRow
        local lOk := TRUE as logic
        // Reachable via UnLock() -> Close(), which never checks _ForceOpen()/DataTable itself -
        // if the buffer was already torn down (_CloseCursor()) there is nothing left to persist.
        if self:DataTable == null
            return true
        endif
        try
            foreach tableRow as DataRow in self:DataTable:Rows
                if (int)tableRow[self:_recnoColumNo] = nRecNo
                    row := tableRow
                endif
            next

            if row == null
                self:_dbfError(ERDD.WRITE, XSharp.Gencode.EG_WRITE, "SqlRDD:GoCold", "Record "+nRecNo:ToString()+" no longer in buffer, cannot save changes" )
                return false
            endif

            // Check row lock
            var dbLockInfo := DbLockInfo{}
            dbLockInfo:RecId := row[_oTd:RecnoColumn]
            var myLock := false
            var otherLock := false
            SELF:CheckLock(dbLockInfo, StringBuilder{}, myLock, otherLock)
            if otherLock
                return false
            endif

            if !myLock
                SELF:Lock(ref dbLockInfo)
            endif

            lOk := true
            if self:_IsRowDeleted(row)
                local wasNew := false as logic
                // Append from may add deleted rows
                if row:RowState.HasFlag(DataRowState.Added)
                    lOk := SELF:_ExecuteInsertStatement(row)
                    row:AcceptChanges()
                    wasNew  := true
                endif
                if self:_deletedColumnNo > -1
                    if !wasNew
                        // already written with _deletedColumnNo with the correct value
                        lOk := SELF:_ExecuteUpdateStatement(row)
                        if lOk
                            row:AcceptChanges()
                        endif
                    endif
                else
                    lOk := SELF:_ExecuteDeleteStatement(row)
                    // we do not clear the fields, but leave the row unchanged.
                    // the DBF has the deleted flag. This emulates what DBF files do

                    row:AcceptChanges()
                endif

            else
                if row:RowState.HasFlag(DataRowState.Added)
                    lOk := SELF:_ExecuteInsertStatement(row)
                    row:AcceptChanges()
                elseif row:RowState.HasFlag(DataRowState.Modified)
                    lOk := SELF:_ExecuteUpdateStatement(row)
                    row:AcceptChanges()
                endif
            endif
        catch e as Exception
            lOk := false
            self:_dbfError(ERDD.WRITE, XSharp.Gencode.EG_WRITE, "SqlRDD:GoCold", e:Message )
        end try

        RETURN lOk

    PRIVATE METHOD LockRecNo(lockInfo ref DbLockInfo) AS INT
        var lockRecNo := 0
        if lockInfo:Method != XSharp.RDD.Support.DbLockInfo.LockMethod.File
            if lockInfo:RecId != null .and. lockInfo:RecId is int
                lockRecNo := (int)lockInfo:RecId
            else
                lockRecNo := (int)self:RecNo
            endif
        endif
        RETURN lockRecNo

    /// <summary>
    /// Check if someone else (or me) has the lock
    /// </summary>
    /// <param name="lockInfo"></param>
    /// <param name="messageLocked"></param>
    /// <param name="myLock"></param>
    /// <param name="otherLock"></param>
    PRIVATE METHOD CheckLock(lockInfo AS DbLockInfo, messageLocked AS StringBuilder, myLock REF LOGIC, otherLock REF LOGIC) AS VOID
        var sb := StringBuilder{}
        sb:AppendLine("select " + self:Connection:XsLockColumnList())
        sb:AppendLine("from " + SqlDbConnection.LockTableName)
        sb:AppendLine("where tablename = "+self:Provider:ParameterPrefix+"p1")
        if lockInfo:Method != XSharp.RDD.Support.DbLockInfo.LockMethod.File
            sb:AppendLine(" AND (recno = "+self:Provider:ParameterPrefix+"p2 OR recno = 0)")
        endif

        using var cmdCheckLock := SqlDbCommand{"CheckLock", self:Connection, false}
        cmdCheckLock:CommandText := sb:ToString()
        cmdCheckLock:AddParameter(self:Provider:ParameterPrefix+"p1",_oTd:RealName)
        if lockInfo:Method != XSharp.RDD.Support.DbLockInfo.LockMethod.File
            if lockInfo:RecId != null .and. lockInfo:RecId is int
                cmdCheckLock:AddParameter(self:Provider:ParameterPrefix+"p2",(int)lockInfo:RecId)
            else
                lockInfo:RecId := self:RecNo
                cmdCheckLock:AddParameter(self:Provider:ParameterPrefix+"p2",(int)self:RecNo)
            endif
        endif

        using var reader := cmdCheckLock:ExecuteReader()
        do while reader:Read()
            var recNoTemp := (int)reader["recno"]
            var station := reader["station"]:ToString()
            var username := reader["username"]:ToString()
            var connectionId := reader["connectionid"]:ToString()
            var workarea := (int)reader["workarea"]
            var threadId := (int)reader["threadid"]

            if (station = (Environment.MachineName ?? String.Empty) .and. ;
                    username = (Environment.UserName ?? String.Empty) .and. ;
                    connectionId = self:Connection:ConnectionId:ToString() .and. ;
                    workarea = (int)super:Area .and. ;
                    threadId = System.Threading.Thread.CurrentThread.ManagedThreadId)
                if (lockInfo:Method = XSharp.RDD.Support.DbLockInfo.LockMethod.File .and. recNoTemp = 0) .or. ;
                    (lockInfo:Method != XSharp.RDD.Support.DbLockInfo.LockMethod.File .and. recNoTemp = SELF:LockRecNo(lockInfo))
                        myLock := true
                endif
            else
                myLock |= false
                var lockType := iif(recNoTemp = 0, "file", "record")
                messageLocked:AppendLine(i"User {username} on station {station} has the {lockType}lock")
                otherLock := true
            endif
        end do
        reader:Dispose()

end class
end namespace

