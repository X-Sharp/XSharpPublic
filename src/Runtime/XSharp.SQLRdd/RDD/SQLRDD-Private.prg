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
    /// <summary>
    /// 0 based offset of the first buffered row within the result set described by _anchorWhere.
    /// The buffer always holds a contiguous slice of that result set (see _bufferContiguous),
    /// so the row at RowNumber n sits at offset _bufferStart + n - 1.
    /// </summary>
    private _bufferStart    as long
    /// <summary>
    /// Extra condition that defines the coordinate system for _bufferStart. Empty means the
    /// whole current order/scope (absolute positions). After a Seek() this holds the soft-seek
    /// condition for the seek value, so paging forward from the found record stays an index
    /// seek plus a small OFFSET instead of requiring the record's absolute position first.
    /// </summary>
    private _anchorWhere    as string
    /// <summary>
    /// FALSE when the buffer holds rows that are not a contiguous slice of the anchored result
    /// set (e.g. a freshly appended row, or a record outside the current order). Paging past
    /// the buffer edges then has to re-locate the current record first.
    /// </summary>
    private _bufferContiguous as logic
    /// <summary>Empty table with the buffer's columns, to load further query results into.</summary>
    private _tableTemplate as DataTable
    /// <summary>An empty table with the buffer's structure (the last replaced buffer or a page already copied
    /// into it) to load the next query result into, instead of copying _tableTemplate again.</summary>
    private _spareTable as DataTable
    /// <summary>
    /// Rows to fetch with the next forward page (0 = PageSize): doubles with every full page read
    /// on in a row (up to BufferSize pages). After a seek it starts small, because code that
    /// seeks and then skips while the key matches mostly reads only a few records.
    /// </summary>
    private _readAheadRows := 0 as long
    /// <summary>Rows to fetch with the next backward page (0 = PageSize), grows like _readAheadRows.</summary>
    private _readBackRows := 0 as long
    /// <summary>How many GoTo() calls in a row loaded the single buffered record without its position.</summary>
    private _lazyGotos as long
    /// <summary>
    /// TRUE after GoTo(0) positioned on the phantom record while the buffer does not end at the
    /// last record of the order: a backward Skip() then has to start from GoBottom().
    /// </summary>
    private _eofPending     as logic
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
    /// <summary>Recnos that _UpdateRow() had to lock itself because the caller held no lock.</summary>
    private _autoLockedRecNos as List<int>
    /// <summary>
    /// Recnos this work area currently holds a lock on (0 = file lock). Lets Lock() and
    /// _UpdateRow() skip asking the lock table about locks we placed ourselves.
    /// </summary>
    private _ownLocks         as HashSet<int>
    /// <summary>TRUE when a write since the last count may have changed RecCount.</summary>
    private _countMayHaveChanged as logic
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
    /// RecCount/LastRec() like DBF: the highest record number of the whole table (the row count
    /// without a recno column), independent of the current order's condition and scope.
    /// </summary>
    private _tableReccount as dword
    /// <summary>TRUE when _tableReccount does not need to be requeried.</summary>
    private _tableReccountValid as logic
    /// <summary>
    /// TRUE when the cursor is positioned (via GoTo()) on a record that physically exists but
    /// does not satisfy the current order's FOR-condition/scope, i.e. OrderKeyNo is 0. Skip()
    /// needs this because RowNumber/_bufferStart no longer correspond to any real position in
    /// the order's sequence, so the normal relative-skip logic cannot be used from here.
    /// </summary>
    private _outsideOrder as logic

#region Properties
    internal property Connection     as SqlDbConnection get _connection
    internal property Provider       as ISqlDbProvider get _connection?:Provider
    internal property Command        as SqlDbCommand get _command
    internal property OrderBagList   as List<SqlDbOrderBag> get _orderBagList
    internal property RecnoColumnName as string
        get
            // The production index is parsed inside FetchInfo(), before _oTd is assigned.
            var oTable := _oTd ?? _builder?:TableInfo
            if !String.IsNullOrEmpty(oTable?:RecnoColumn)
                return oTable:RecnoColumn
            endif
            return _connection?:RecnoColumn
        end get
    end property
    /// <summary>Length of the character column cName, -1 when it is no character column or unknown.
    /// Also while the production index is parsed: the fields of the work area are not there yet.</summary>
    internal method _CharColumnLength(cName as string) as long
        var oTable := _oTd ?? _builder?:TableInfo
        if oTable?:Columns != null
            foreach var oCol in oTable:Columns
                if String.Equals(oCol:Name, cName, StringComparison.OrdinalIgnoreCase)
                    return iif(oCol:ColumnInfo:FieldType == DbFieldType.Character, oCol:Length, -1)
                endif
            next
        endif
        return -1
    end method
    /// <summary>Maximum number of rows kept in the local buffer.</summary>
    private property _MaxBufferRows  as long => _oTd:PageSize * Math.Max(_oTd:BufferSize, 1)
    /// <summary>Rows for a first read where only a few records are expected to be needed (after a seek,
    /// GoBottom(), looking up the position of a record). Reading on makes the pages grow again.</summary>
    private property _SmallPageRows  as long => Math.Max(1, _oTd:PageSize / 10)
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
            var oOld := _table
            _table := value
            SELF:_ReturnSpareTable(oOld)
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
            if SELF:_tableTemplate == null
                // the structure for all further queries of this work area (same column list),
                // see SqlDbCommand.GetDataTable(cTable, oTemplate)
                SELF:_tableTemplate := _table:Clone()
            endif
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
        SELF:_bufferStart      := 0
        SELF:_anchorWhere      := ""
        SELF:_bufferContiguous := true
        self:_trimValues       := true // trim String Valuess
        SELF:_updatedRecNos    := List<int>{}
        SELF:_autoLockedRecNos := List<int>{}
        SELF:_ownLocks         := HashSet<int>{}
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

    /// <summary>
    /// A Character field holds exactly its declared width, as in DBF: shorter values are padded
    /// with spaces, longer ones truncated.
    /// </summary>
    /// <remarks>
    /// Only when trailing spaces are not trimmed (DBF-like storage). Otherwise rows written by
    /// the RDD had trimmed values next to the padded ones of the existing data, and a key
    /// expression concatenated on the server (e.g. TYP+UPPER(KATEGORIE)+UPPER(BEZEICH))
    /// shifted the following parts of such a row, so seeks on the key prefix missed it.
    /// </remarks>
    private method _FixedWidthValue(nIndex as int, oValue as object) as object
        if !SELF:_trimValues .and. oValue is string var strValue .and. nIndex < SELF:_Fields:Length
            var oField := SELF:_Fields[nIndex]
            if oField:FieldType == DbFieldType.Character .and. oField:Length > 0
                if strValue:Length < oField:Length
                    return strValue:PadRight(oField:Length, ' ')
                elseif strValue:Length > oField:Length
                    return strValue:Substring(0, oField:Length)
                endif
            endif
        endif
        return oValue
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
                values.Add(SELF:_FixedWidthValue(col:Ordinal, ""))
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
        SELF:_countMayHaveChanged := true
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
            var lOwnTrans := _command:Connection:BeginTrans()
            _command:BindParameters()
            local lInsertWithGetIdentity := false as LOGIC
            if hasGetIdentity .and. ! String.IsNullOrEmpty(Provider:GetIdentity)
                sb:Append("; ")
                sb:Append(Provider:GetIdentity)
                lInsertWithGetIdentity := TRUE
            endif
            _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
            // The command methods report a failure through their result and LastException, they
            // do not throw.
            local lOk as logic
            _command:Connection:LastException := null
            if lInsertWithGetIdentity
                var result := _command:ExecuteScalar()
                lOk := result != null .and. result != DBNull.Value
                if lOk
                    row[_recnoColumNo] := result
                endif
            else
                lOk := _command:ExecuteNonQuery()
                if lOk .and. hasGetIdentity
                    row[_recnoColumNo] := _builder:GetMaxRecno()
                endif
            endif
            if lOk
                lOk := _command:Connection:CommitTrans() .or. !lOwnTrans
            endif
            if !lOk
                var oError := _command:Connection:LastException
                throw iif(oError != null, oError, Exception{"The insert returned no record number"})
            endif
        catch e as Exception
            // Reporting success here made a failed insert look like a new record whose changes
            // then went nowhere.
            _command:Connection:RollBackTrans()
            self:_dbfError(ERDD.WRITE, XSharp.Gencode.EG_WRITE, "SqlRDD:Append", e:Message, FALSE)
            return false
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
                var oOriginal := row[c, DataRowVersion.Original]
                var oCurrent  := row[c, DataRowVersion.Current]
                if oOriginal is string var strOriginal .and. oCurrent is string var strCurrent
                    isEqual := strOriginal:Trim():Equals(strCurrent:Trim())
                else
                    // Compare by value, not by text: decimal 1.00 read from the server and the
                    // 1 that a FLOAT written back converts to are equal, but "1.00" <> "1" -
                    // which made every unchanged numeric field look modified and be written.
                    isEqual := Object.Equals(oOriginal, oCurrent)
                endif
                if isEqual
                    loop
                endif
                lColumnChanged := true
            endif
            if !SELF:_countMayHaveChanged .and. SELF:_ColumnAffectsCount(c:ColumnName)
                SELF:_countMayHaveChanged := true
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
            SELF:_ThrowIfStatementFailed(res)
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
            return SELF:_EmptyDateValue(oCol)
        elseif oValue is IDate var d
            if d:IsEmpty
                return SELF:_EmptyDateValue(oCol)
            else
                return DateTime{d:Year, d:Month, d:Day}
            endif
        elseif (oValue is null .or. oValue == DBNull.Value) .and. oCol:DataType == typeof(DateTime)
            return SELF:_EmptyDateValue(oCol)
        endif
        return oValue
    end method

    /// <summary>The value stored for an empty date.</summary>
    /// <remarks>
    /// With UseNulls (the default) that is NULL. Without it - "empty values instead of NULLs" -
    /// a DBF "D" field gets DateTime.MinValue (0001-01-01, valid for a SQL `date` column), the
    /// value GetValue() already reads back as an empty date. Writing NULL there broke every
    /// save of a record with an empty date into a NOT NULL date column ("Cannot insert the
    /// value NULL ..."), which is how DBF-migrated tables typically store empty dates.
    /// </remarks>
    private method _EmptyDateValue(oCol as DataColumn) as object
        if !SELF:Connection:UseNulls .and. oCol:Ordinal < SELF:_Fields:Length ;
            .and. SELF:_Fields[oCol:Ordinal]:FieldType == DbFieldType.Date
            return DateTime.MinValue
        endif
        return DBNull.Value

    /// <summary>
    /// Can changing column cColumn change the cached row count? It counts the rows of the current
    /// order (its FOR condition and scope) or of the table, both subject to the server filter.
    /// Updating any other column - the common case, e.g. recalculating a balance - leaves the
    /// count as it is, so it does not have to be requeried.
    /// </summary>
    private method _ColumnAffectsCount(cColumn as string) as logic
        local pattern := "\b" + System.Text.RegularExpressions.Regex.Escape(cColumn) + "\b" as string
        local options := System.Text.RegularExpressions.RegexOptions.IgnoreCase as System.Text.RegularExpressions.RegexOptions
        if cColumn == SELF:_oTd:DeletedColumn
            return true
        endif
        if SELF:_oTd:HasServerFilter .and. System.Text.RegularExpressions.Regex.IsMatch(SELF:_oTd:ServerFilter, pattern, options)
            return true
        endif
        var order := SELF:CurrentOrder
        if order != null
            if order:Conditional .and. System.Text.RegularExpressions.Regex.IsMatch(order:Condition, pattern, options)
                return true
            endif
            if order:HasScopes .and. System.Text.RegularExpressions.Regex.IsMatch(order:Expression, pattern, options)
                return true
            endif
        endif
        return false

    private method _ExecuteDeleteStatement(row as DataRow) as logic
        // TODO: check config if logic or physical delete
        SELF:_countMayHaveChanged := true
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
            SELF:_ThrowIfStatementFailed(res)
            return res is int var i .and. i == 1
        endif
        return true
    end method

    /// <summary>
    /// The UPDATE/DELETE statements end with a row count query, so a NULL result means the
    /// statement itself failed: SqlDbCommand:ExecuteScalar() swallows the provider exception
    /// into Connection:LastException. Rethrow that, so callers report the database's own
    /// message (e.g. a NOT NULL violation) instead of a follow-up conversion error.
    /// </summary>
    private method _ThrowIfStatementFailed(res as object) as void
        if res == null .or. res == DBNull.Value
            var ex := SELF:Connection:LastException
            if ex != null
                throw ex
            endif
            throw Exception{"The statement did not return a row count: "+_command:CommandText}
        endif
        return

    /// <summary>
    /// Fetch the last page of the current order/scope/filter directly, without going through
    /// the ascending, huge-OFFSET query GoBottom() would otherwise need for a large table.
    /// </summary>
    /// <remarks>
    /// See SqlDbTableCommandBuilder.BuildLastPageStatement for why this exists.
    /// The position of the page in the order is only known from a count of the order. When no valid
    /// count is cached, the page is anchored at its first row instead (as after GoTo(), see
    /// _anchorWhere), and the position is looked up when something needs it. A list of the lines of
    /// an order in our ERP calls GoBottom() on every move to another order: each one counted the
    /// order of a table with 5.4 million rows first (46 ms), 72 times while paging through orders.
    /// </remarks>
    private method _FetchLastPage() as logic
        try
            // A descending order is reversed item by item, like the backward keyset reads do.
            if SELF:CurrentOrder != null .and. SELF:CurrentOrder:Descending
                SELF:_command:CommandText := SELF:_BuildSqlStatement("", 0, SELF:_SmallPageRows, TRUE)
            else
                SELF:_command:CommandText := _builder:BuildLastPageStatement(SELF:_SmallPageRows)
            endif
            SELF:_command:ClearParameters()
            var newTable := SELF:_command:GetDataTable(SELF:Alias, SELF:_tableTemplate, SELF:_TakeSpareTable())
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
            SELF:_ReturnSpareTable(newTable)
            SELF:_anchorWhere := ""
            SELF:_bufferContiguous := true
            if SELF:_serverReccountValid
                SELF:_bufferStart := Math.Max(0, (long) SELF:_serverReccount - SELF:RowCount)
            elseif SELF:RowCount < SELF:_SmallPageRows
                // fewer rows than asked for: the whole order
                SELF:_bufferStart := 0
            else
                var cAnchor := SELF:_KeysetCondition(SELF:DataTable:Rows[0], FALSE, TRUE, TRUE)
                if cAnchor != null
                    SELF:_anchorWhere := cAnchor
                    SELF:_bufferStart := 0
                else
                    SELF:_GetRecCount()
                    SELF:_bufferStart := Math.Max(0, (long) SELF:_serverReccount - SELF:RowCount)
                endif
            endif
            // reading backwards from the bottom starts small as well
            SELF:_readBackRows := SELF:_SmallPageRows
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
        return self:_OpenTable("", "")
    end method

    private method _ReadTable(sWhereClause as string, nOffset as long, nCount as long, lReverse := false as logic) as DataTable
        try
            _command:CommandText := self:_BuildSqlStatement(sWhereClause, nOffset, nCount, lReverse)
            _command:ClearParameters()
            var oDataTable := _command:GetDataTable(self:Alias, SELF:_tableTemplate, SELF:_TakeSpareTable())
            return oDataTable
        catch as Exception
            return null
        end try

    /// <summary>
    /// Load the first page of rows matching sLoadWhere into a fresh buffer.
    /// </summary>
    /// <param name="sLoadWhere">Condition for the rows to load (e.g. a hard seek).</param>
    /// <param name="sAnchorWhere">Condition that defines the coordinate system for later paging
    /// (see _anchorWhere). The rows matching sLoadWhere must be the leading rows of the rows
    /// matching sAnchorWhere, in order - a hard seek and its soft counterpart satisfy that.</param>
    /// <param name="oLoaded">The rows matching sLoadWhere when they were already read (first page), else NULL.</param>
    private method _OpenTable(sLoadWhere as string, sAnchorWhere as string, oLoaded := null as DataTable) as logic
        try
            SELF:_bufferStart := 0
            SELF:_anchorWhere := sAnchorWhere
            SELF:_bufferContiguous := true
            // a hard seek (sLoadWhere narrower than the anchor) mostly continues with a few records
            SELF:_readAheadRows := iif(sLoadWhere != sAnchorWhere .and. !String.IsNullOrEmpty(sAnchorWhere), SELF:_SmallPageRows, 0)
            SELF:_readBackRows := 0
            // A fresh open/reposition (e.g. from Seek()) must not inherit a stale "no more
            // rows" flag left over from whatever this buffer was doing before - otherwise
            // Skip() refuses to fetch the next page and reports EOF even though the new
            // WHERE clause has plenty more rows past the first page.
            SELF:_hasEOF := false
            SELF:DataTable := oLoaded ?? self:_ReadTable(sLoadWhere, 0, _oTd:PageSize)
            if SELF:DataTable == null
                // _ReadTable() -> Command:GetDataTable()/ExecuteReader() swallow the real
                // ADO.NET exception into Connection:LastException instead of throwing it.
                // Without this check we'd return TRUE here with no data loaded, and the
                // first caller to touch DataTable (GoTo(), GoTop(), ...) would crash with
                // a bare NullReferenceException that hides the actual database error.
                self:_dbfError(self:Connection:LastException, Subcodes.EDB_USE, Gencode.EG_OPEN, "SQLRDD._OpenTable", FALSE)
                return false
            endif
            if sLoadWhere == sAnchorWhere .and. SELF:RowCount < _oTd:PageSize
                SELF:_hasEOF := true
            endif
            // A fresh fetch always means a (potentially) different row count - force a real
            // requery rather than trusting whatever was cached before this WHERE clause. The
            // count itself is only run when RecCount is actually asked for: a Seek() does not
            // need it, and counting a large order on every Seek() is expensive.
            self:_serverReccountValid := false
        catch as Exception
            return false
        end try
        return true
    end method


    private method _BuildSqlStatement(sWhereClause as string, nOffset as long, nCount as long, lReverse := false as logic) as string
        local query as string
        if self:_tableMode == TableMode.Table
            query := _builder:BuildSqlStatement(sWhereClause, (int) nOffset, (int) nCount, lReverse)
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

    /// <summary>
    /// Position on the phantom record (EOF) without reading anything. BOF is left to the caller:
    /// GoTo() to a record that does not exist sets it (as ADS does), an order change at EOF and a
    /// failed seek keep or clear it. Computing it here meant counting the order for an empty buffer.
    /// </summary>
    private method _GoToPhantom() as void
        SELF:_Found := FALSE
        SELF:_outsideOrder := FALSE
        SELF:RowNumber := SELF:RowCount + 1
        SELF:_SetEOF(TRUE)
        // When the buffer already ends at the last record, a backward Skip() from the phantom
        // row naturally lands on it; otherwise remember to go to the bottom first.
        SELF:_eofPending := !(SELF:_hasEOF .and. SELF:_bufferContiguous)
        return

    private method _CloseCursor() as void
        self:_eofPending    := FALSE
        self:_hasData       := FALSE
        self:_table         := null
        self:_bufferStart   := 0
        self:_anchorWhere   := ""
        self:_bufferContiguous := true
        // Whatever the cached count reflected (a prior WHERE clause, order or scope) no longer
        // applies once the cursor is torn down - force the next _GetRecCount() to requery.
        self:_serverReccountValid := false
        self:_tableReccountValid  := false
        return

    /// <summary>Number of rows in the current order (condition, scope) or table, counted on demand.</summary>
    private method _OrderRowCount() as dword
        if self:_tableMode == TableMode.Table .and. !self:_serverReccountValid .and. self:_builder != null
            self:_GetRecCount()
        endif
        return self:_serverReccount
    end method

    private method _GetTableRecCount() as void
        // DBF's LastRec() is the highest record number, whatever order or filter is active.
        // Using the order's row count instead made RecNo at EOF (LastRec+1) collide with a real
        // record of a conditional order, and code that treats RecNo > LastRec as EOF (browsers)
        // stopped before the records with the highest numbers.
        if self:_oTd:HasRecnoColumn
            self:_tableReccount := self:_builder:GetMaxRecno()
        else
            self:_tableReccount := self:_builder:GetRecCount()
        endif
        self:_tableReccountValid := true
    end method

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

    /// <summary>
    /// Condition for the rows that follow the last buffered row in the current order (keyset
    /// paging), or NULL when that cannot be expressed and the caller must page with OFFSET.
    /// </summary>
    private method _KeysetAfterLastRow() as string
        if SELF:RowCount == 0 .or. !SELF:_bufferContiguous
            return null
        endif
        return SELF:_KeysetCondition(SELF:DataTable:Rows[SELF:RowCount-1], FALSE, FALSE)
    end method

    /// <summary>
    /// Condition for the rows that come after oRow (lBefore FALSE) or before it (lBefore TRUE) in
    /// the current order, including oRow itself when lInclusive is TRUE. NULL when that cannot be
    /// expressed and the caller must use row offsets instead.
    /// </summary>
    /// <remarks>
    /// The order is the ORDER BY list of the current order plus the recno column, exactly as
    /// SqlDbTableCommandBuilder.BuildSqlStatement() sorts. Every item is compared with its value
    /// for oRow (see _KeysetLiteral(), otherwise read by a subquery on the row's recno, so the
    /// comparison uses the same expressions and collation as the ORDER BY, also for keys with
    /// functions; lLiteralOnly returns NULL instead, see _KeysetLiteral() for the cost). For the
    /// rows after oRow:
    ///   k1 >= v1 AND ( k1 > v1 OR (k1 = v1 AND k2 > v2) OR ... OR (k1 = v1 AND ... AND recno > n) )
    /// The leading bound lets the server seek in an index on k1. Where the order goes towards
    /// lower values (a descending item after oRow, an ascending one before it) the comparison is
    /// "<" and also accepts NULL, which sorts below all values. When oRow has NULL in one of the
    /// key columns its position cannot be expressed this way.
    /// </remarks>
    private method _KeysetCondition(oRow as DataRow, lBefore as logic, lInclusive as logic, lLiteralOnly := false as logic) as string
        if !SELF:_oTd:HasRecnoColumn .or. SELF:_recnoColumNo < 0
            return null
        endif
        var oRecno  := oRow[(int) SELF:_recnoColumNo]
        if oRecno == null .or. oRecno == DBNull.Value
            return null
        endif
        var nRecno  := Convert.ToInt64(oRecno)
        var cRecno  := SELF:Provider:QuoteIdentifier(SELF:_oTd:RecnoColumn)
        var cTable  := SELF:Provider:QuoteIdentifier(SELF:_oTd:RealName)
        var aExpr   := List<string>{}
        var aDesc   := List<logic>{}
        var oOrder  := SELF:CurrentOrder
        if oOrder != null
            foreach var cCol in oOrder:ColumnList
                var nCol := SELF:DataTable:Columns:IndexOf(cCol:Trim(<char>{'[',']','"','`'}))
                if nCol < 0 .or. oRow[nCol] == DBNull.Value
                    return null
                endif
            next
            foreach var cItem in oOrder:OrderList
                var lDesc := cItem:EndsWith(" DESC", StringComparison.OrdinalIgnoreCase)
                aExpr:Add(iif(lDesc, cItem:Substring(0, cItem:Length - 5), cItem):Trim())
                aDesc:Add(lDesc)
            next
        endif
        aExpr:Add(cRecno)
        aDesc:Add(oOrder != null .and. oOrder:Descending)

        var aValue := List<string>{}
        foreach var cExpr in aExpr
            if cExpr == cRecno
                aValue:Add(nRecno:ToString())
            else
                var cLiteral := SELF:_KeysetLiteral(oRow, cExpr)
                if cLiteral == null
                    if lLiteralOnly
                        return null
                    endif
                    cLiteral := "(SELECT " + cExpr + " FROM " + cTable + " WHERE " + cRecno + " = " + nRecno:ToString() + ")"
                endif
                aValue:Add(cLiteral)
            endif
        next

        var nLast := aExpr:Count - 1
        var sb := StringBuilder{}
        sb:Append("(")
        if aExpr:Count > 1
            sb:Append(SELF:_KeysetCompare(aExpr[0], aDesc[0] != lBefore, aValue[0], TRUE))
            sb:Append(SqlDbProvider.AndClause)
        endif
        sb:Append("(")
        for var i := 0 upto nLast
            if i > 0
                sb:Append(SqlDbProvider.OrClause)
            endif
            sb:Append("(")
            for var j := 0 upto i-1
                sb:Append(aExpr[j] + " = " + aValue[j] + SqlDbProvider.AndClause)
            next
            sb:Append(SELF:_KeysetCompare(aExpr[i], aDesc[i] != lBefore, aValue[i], lInclusive .and. i == nLast))
            sb:Append(")")
        next
        sb:Append("))")
        return sb:ToString()
    end method

    /// <summary>
    /// The value of the order item cExpr in oRow as a SQL literal, or NULL when the subquery must
    /// be used: cExpr is not a plain column, or the value cannot be written as a literal that
    /// compares exactly like the stored value.
    /// </summary>
    /// <remarks>
    /// Every subquery has to be optimized by the server: with one per item, compiling the keyset
    /// condition of a two column key took 30-45 ms, executing it 3 ms. Since the text changes with
    /// every page the plan is never reused. The buffer holds the values as read from the server, so
    /// a literal is exact for strings (ASCII only, so no code page conversion can change them),
    /// integers/decimals, and dates without a time of day when the provider has a literal for them
    /// (SqlDbProvider.DateLiteral()). With a subquery for a date, SQL Server could not seek on the
    /// index: every page of a 3.8 million row table read all of it (665,762 pages against 1,941).
    /// </remarks>
    private method _KeysetLiteral(oRow as DataRow, cExpr as string) as string
        var nCol := SELF:DataTable:Columns:IndexOf(cExpr:Trim(<char>{'[',']','"','`'}))
        if nCol < 0
            return null
        endif
        switch oRow[nCol]
        case cValue as string
            // Non-ASCII characters only when the database stores them unchanged in a literal: a key
            // like 'GÜNTHER' fell back to the position of the record, an OFFSET of 323,815 rows in
            // the list of 1.1 million addresses (more than a minute).
            if cValue:IndexOf((char) 92) >= 0 .or. !SELF:Connection:IsExactStringLiteral(cValue)
                return null
            endif
            return Functions.XsValueToSqlValue(cValue)
        case nValue as long
            return nValue:ToString(System.Globalization.CultureInfo.InvariantCulture)
        case nValue as int64
            return nValue:ToString(System.Globalization.CultureInfo.InvariantCulture)
        case nValue as short
            return nValue:ToString(System.Globalization.CultureInfo.InvariantCulture)
        case nValue as decimal
            return nValue:ToString(System.Globalization.CultureInfo.InvariantCulture)
        case dValue as DateTime
            // a time of day would need the precision of the column type
            if dValue:TimeOfDay == TimeSpan.Zero .and. SELF:Provider is SqlDbProvider var oProvider
                return oProvider:DateLiteral(dValue)
            endif
        end switch
        return null
    end method

    private method _KeysetCompare(cExpr as string, lLower as logic, cValue as string, lInclusive as logic) as string
        var cComp := iif(lLower, " <", " >") + iif(lInclusive, "= ", " ")
        if lLower
            return "(" + cExpr + cComp + cValue + SqlDbProvider.OrClause + cExpr + " IS NULL)"
        endif
        return cExpr + cComp + cValue
    end method

    /// <summary>
    /// Append the next page of the anchored result set to the end of the buffer, evicting rows
    /// from the start when the buffer would grow beyond _MaxBufferRows. RowNumber keeps pointing
    /// at the same row. Returns the number of rows fetched (0 = nothing left, or an error).
    /// </summary>
    private method _FetchForward() as long
        if self:_tableMode != TableMode.Table
            return 0
        endif
        var nOffset  := SELF:_bufferStart + SELF:RowCount
        local newTable as DataTable
        local nCount := _oTd:PageSize as long
        var cAfterLast := SELF:_KeysetAfterLastRow()
        if cAfterLast != null
            // Same rows as OFFSET nOffset, but found by an index seek behind the last buffered row
            // instead of skipping nOffset rows on the server for every page (a full sequential
            // read was quadratic: 52.000 rows = 521 pages with OFFSET up to 52.000).
            var cWhere := cAfterLast
            if !String.IsNullOrEmpty(SELF:_anchorWhere)
                cWhere := "(" + SELF:_anchorWhere + ")" + SqlDbProvider.AndClause + cWhere
            endif
            // Reading on page by page (DbEval(), a report, a count): fetch more rows per query, up
            // to what the buffer holds without evicting the current row.
            var nWanted := iif(SELF:_readAheadRows > 0, SELF:_readAheadRows, nCount)
            var nMax    := SELF:_MaxBufferRows - (SELF:RowCount - SELF:RowNumber) - 1
            nCount := iif(nWanted <= nCount, nWanted, Math.Max(nCount, Math.Min(nWanted, nMax)))
            newTable := SELF:_ReadTable(cWhere, 0, nCount)
        else
            if SELF:_readAheadRows > 0 .and. SELF:_readAheadRows < nCount
                nCount := SELF:_readAheadRows
            endif
            newTable := SELF:_ReadTable(SELF:_anchorWhere, nOffset, nCount)
        endif
        if newTable == null
            return 0
        endif
        SELF:_readAheadRows := iif(newTable:Rows:Count < nCount, 0, Math.Min(nCount * 2, _oTd:PageSize * Math.Max(_oTd:BufferSize, 1)))
        var nRows := newTable:Rows:Count
        foreach row as DataRow in newTable:Rows
            var newRow := SELF:DataTable:NewRow()
            newRow:ItemArray := row:ItemArray
            SELF:DataTable:Rows:Add(newRow)
            newRow:AcceptChanges()
        next
        SELF:_ReturnSpareTable(newTable)
        // A short page always means EOF. But when the total record count is an exact
        // multiple of PageSize, the last page comes back FULL - "short page" never fires,
        // so also check whether this page's absolute record range already reaches the
        // known total. Without this, sequential forward paging (unlike GoBottom(), which
        // jumps straight to the last page and marks it via _FetchLastPage) never sets
        // _hasEOF on that exactly-full last page: the next Skip() then fetches a
        // nonexistent page past it, landing on a bogus RowNumber instead of staying put.
        if nRows < nCount
            SELF:_hasEOF := true
        elseif String.IsNullOrEmpty(SELF:_anchorWhere) .and. SELF:_serverReccountValid .and. ;
            nOffset + nRows >= SELF:_serverReccount
            SELF:_hasEOF := true
        endif
        var nRemove := SELF:RowCount - SELF:_MaxBufferRows
        if nRemove > 0
            for var i := 1 upto nRemove
                SELF:DataTable:Rows:RemoveAt(0)
            next
            SELF:_bufferStart += nRemove
            SELF:RowNumber    -= nRemove
        endif
        return nRows

    /// <summary>
    /// Insert the page of the anchored result set that precedes the buffer at the start of the
    /// buffer, evicting rows from the end when the buffer would grow beyond _MaxBufferRows.
    /// RowNumber keeps pointing at the same row. Returns the number of rows fetched.
    /// </summary>
    /// <param name="nNeeded">Rows that must at least be read (when there are that many).</param>
    private method _FetchBackward(nNeeded := 0 as long) as long
        SELF:_readAheadRows := 0
        if self:_tableMode != TableMode.Table .or. SELF:_bufferStart <= 0
            return 0
        endif
        var nKeyset := SELF:_FetchBackwardKeyset(nNeeded)
        if nKeyset >= 0
            return nKeyset
        endif
        var nStart   := Math.Max(0, SELF:_bufferStart - Math.Max(_oTd:PageSize, nNeeded))
        var newTable := SELF:_ReadTable(SELF:_anchorWhere, nStart, SELF:_bufferStart - nStart)
        if newTable == null
            return 0
        endif
        var nRows := newTable:Rows:Count
        for var nRow := nRows-1 downto 0
            var row := newTable:Rows[nRow]
            var newRow := SELF:DataTable:NewRow()
            newRow:ItemArray := row:ItemArray
            SELF:DataTable:Rows:InsertAt(newRow, 0)
            newRow:AcceptChanges()
        next
        SELF:_ReturnSpareTable(newTable)
        SELF:_bufferStart := nStart
        SELF:RowNumber    += nRows
        SELF:_TrimBufferEnd()
        return nRows

    /// <summary>
    /// Like _FetchBackward(), but finds the rows before the buffer by the key of its first row
    /// (sorted in reverse) instead of by their offset, which makes the server read through all
    /// rows before them. Works on a seek-anchored buffer too, which is then anchored at its new
    /// first row. Returns -1 when the key cannot be expressed (see _KeysetCondition()).
    /// </summary>
    private method _FetchBackwardKeyset(nNeeded as long) as long
        if self:_tableMode != TableMode.Table .or. SELF:RowCount == 0 .or. !SELF:_bufferContiguous
            return -1
        endif
        var cBefore := SELF:_KeysetCondition(SELF:DataTable:Rows[0], TRUE, FALSE, TRUE)
        if cBefore == null
            return -1
        endif
        local nCount := iif(SELF:_readBackRows > 0, SELF:_readBackRows, _oTd:PageSize) as long
        nCount := Math.Max(nCount, nNeeded)
        var newTable := SELF:_ReadTable(cBefore, 0, nCount, TRUE)
        if newTable == null
            return -1
        endif
        var nRows := newTable:Rows:Count
        local cAnchor := "" as string
        if nRows == nCount .and. !String.IsNullOrEmpty(SELF:_anchorWhere)
            // The rows before the anchor are not part of the anchored result set.
            cAnchor := SELF:_KeysetCondition(newTable:Rows[nRows-1], FALSE, TRUE, TRUE)
            if cAnchor == null
                return -1
            endif
        endif
        // The rows come nearest first.
        for var nRow := 0 upto nRows-1
            var newRow := SELF:DataTable:NewRow()
            newRow:ItemArray := newTable:Rows[nRow]:ItemArray
            SELF:DataTable:Rows:InsertAt(newRow, 0)
            newRow:AcceptChanges()
        next
        SELF:_ReturnSpareTable(newTable)
        SELF:RowNumber    += nRows
        SELF:_readAheadRows := 0
        SELF:_readBackRows := iif(nRows < nCount, 0, Math.Min(nCount * 2, SELF:_MaxBufferRows))
        if nRows < nCount
            // the buffer now starts at the first row of the order
            SELF:_anchorWhere := ""
            SELF:_bufferStart := 0
        elseif !String.IsNullOrEmpty(SELF:_anchorWhere)
            SELF:_anchorWhere := cAnchor
            SELF:_bufferStart := 0
        else
            SELF:_bufferStart := Math.Max(0, SELF:_bufferStart - nRows)
        endif
        SELF:_TrimBufferEnd()
        return nRows

    /// <summary>Evict rows from the end when the buffer grew beyond _MaxBufferRows.</summary>
    private method _TrimBufferEnd() as void
        var nRemove := SELF:RowCount - SELF:_MaxBufferRows
        if nRemove > 0
            for var i := 1 upto nRemove
                SELF:DataTable:Rows:RemoveAt(SELF:DataTable:Rows:Count-1)
            next
            SELF:_hasEOF := false
        endif
        return

    /// <summary>
    /// Replace the buffer with up to one page of the anchored result set starting at nOffset.
    /// </summary>
    /// <param name="nRows">Rows to read, 0 = PageSize.</param>
    private method _LoadBufferAt(nOffset as long, nRows := 0 as long) as long
        SELF:_readAheadRows := nRows
        SELF:_readBackRows := nRows
        SELF:_ClearTable()
        SELF:_bufferStart := Math.Max(0, nOffset)
        SELF:_bufferContiguous := true
        SELF:_hasEOF := false
        SELF:RowNumber := 1
        return SELF:_FetchForward()

    /// <summary>
    /// Move nToSkip rows relative to the current row, paging the buffer as needed.
    /// Past the last row this positions on the phantom row (RowCount+1) with EOF set, before
    /// the first row it stays on row 1 with BOF set.
    /// </summary>
    private method _SkipInBuffer(nToSkip as long) as logic
        local nNewRow := SELF:RowNumber + nToSkip as long
        local nTarget as long
        if nNewRow >= 1 .and. nNewRow <= SELF:RowCount
            SELF:RowNumber := nNewRow
            SELF:_SetEOF(FALSE)
            return true
        endif
        if SELF:_tableMode == TableMode.Table .and. !SELF:_bufferContiguous .and. !SELF:_EoF .and. SELF:RowCount > 0
            // The buffer is not a contiguous slice of the order (e.g. after Append()), so its
            // edges say nothing about the neighbouring rows: re-locate the current record first.
            var nRec := Convert.ToUInt32(SELF:CurrentRow[self:_recnoColumNo])
            if !SELF:_GotoRecord(nRec) .or. !SELF:_bufferContiguous
                return SELF:GoTop()
            endif
            nNewRow := SELF:RowNumber + nToSkip
        endif
        // Forward, past the end of the buffer
        do while nNewRow > SELF:RowCount
            if SELF:_hasEOF .or. SELF:_tableMode != TableMode.Table
                SELF:RowNumber := SELF:RowCount + 1
                SELF:_SetEOF(TRUE)
                return true
            endif
            nTarget := SELF:_bufferStart + nNewRow - 1
            if nNewRow - SELF:RowCount > _oTd:PageSize
                // Far jump: load the target page directly instead of paging through
                // everything in between.
                if SELF:_LoadBufferAt(nTarget) == 0
                    SELF:GoBottom()
                    SELF:RowNumber := SELF:RowCount + 1
                    SELF:_SetEOF(TRUE)
                    return true
                endif
                nNewRow := 1
                exit
            endif
            // a read-ahead that started small must still reach the target in one page
            if SELF:_readAheadRows > 0 .and. SELF:_readAheadRows < nNewRow - SELF:RowCount
                SELF:_readAheadRows := nNewRow - SELF:RowCount
            endif
            if SELF:_FetchForward() == 0
                SELF:_hasEOF := true
            endif
            nNewRow := nTarget - SELF:_bufferStart + 1
        enddo
        // Backward, before the start of the buffer
        do while nNewRow < 1
            if SELF:_tableMode != TableMode.Table
                SELF:RowNumber := 1
                SELF:_SetBOF(TRUE)
                return true
            endif
            if !String.IsNullOrEmpty(SELF:_anchorWhere) .and. 1 - nNewRow <= SELF:_MaxBufferRows
                // Read the rows before an anchored buffer by key, without looking up its position.
                // Also for more than a page: the position of a buffer near the end of a large order
                // is far down, and the page by position needs an OFFSET of millions of rows (two
                // minutes for 150 rows back from the end of 3.8 million orders).
                var nFetched := SELF:_FetchBackwardKeyset(1 - nNewRow)
                if nFetched == 0
                    SELF:RowNumber := 1
                    SELF:_SetBOF(TRUE)
                    return true
                elseif nFetched > 0
                    nNewRow += nFetched
                    loop
                endif
            endif
            if !String.IsNullOrEmpty(SELF:_anchorWhere)
                // Seek-anchored buffer: the rows before the anchor are not part of the anchored
                // result set, so switch to absolute positions first.
                if !SELF:_ConvertToAbsolute()
                    var nRemaining := nNewRow - SELF:RowNumber
                    var nRec := Convert.ToUInt32(SELF:CurrentRow[self:_recnoColumNo])
                    if !SELF:_GotoRecord(nRec)
                        return SELF:GoTop()
                    endif
                    nNewRow := SELF:RowNumber + nRemaining
                endif
                loop
            endif
            if SELF:_bufferStart <= 0
                // Before the first record
                SELF:RowNumber := 1
                SELF:_SetBOF(TRUE)
                if SELF:RowCount == 0
                    SELF:_SetEOF(TRUE)
                endif
                return true
            endif
            nTarget := SELF:_bufferStart + nNewRow - 1
            // up to what the buffer holds _FetchBackward() reads by key (see above), beyond that by position
            if 1 - nNewRow > SELF:_MaxBufferRows
                SELF:_LoadBufferAt(nTarget)
                if nTarget < 0
                    SELF:RowNumber := 1
                    SELF:_SetBOF(TRUE)
                    return true
                endif
                nNewRow := 1
                exit
            endif
            var nRows := SELF:_FetchBackward(1 - nNewRow)
            if nRows == 0
                SELF:RowNumber := 1
                SELF:_SetBOF(TRUE)
                return true
            endif
            nNewRow += nRows
        enddo
        SELF:RowNumber := nNewRow
        SELF:_SetEOF(FALSE)
        return true

    /// <summary>The 1 based position of the current row within the current order/scope.</summary>
    private method _AbsolutePosition() as long
        if SELF:_tableMode == TableMode.Table .and. (!SELF:_bufferContiguous .or. !SELF:_ConvertToAbsolute())
            // Buffer offsets can't be trusted - ask the server for this record's position.
            if SELF:_EoF .or. SELF:RowCount == 0
                return 0
            endif
            return SELF:_GetAbsoluteOffset(Convert.ToUInt32(SELF:CurrentRow[self:_recnoColumNo])) + 1
        endif
        return SELF:_bufferStart + SELF:RowNumber

    /// <summary>The 0 based offset of the first row of the last page, for nTotal rows.</summary>
    private method _LastPageStart(nTotal as long) as long
        if nTotal <= 0
            return 0
        endif
        return ((nTotal - 1) / _oTd:PageSize) * _oTd:PageSize

    /// <summary>Return the 1 based buffer row that holds nRec, or 0 when it is not buffered.</summary>
    private method _FindRowInBuffer(nRec as DWORD) as long
        if SELF:DataTable == null
            return 0
        endif
        var rowIndex := 1
        foreach oRow as DataRow in SELF:DataTable:Rows
            if Convert.ToUInt32(oRow[self:_recnoColumNo]) == nRec
                return rowIndex
            endif
            rowIndex++
        next
        return 0

    /// <summary>
    /// The 0 based offset of nRec within the current order/scope (absolute coordinates),
    /// or -1 when the record does not satisfy the order's condition/scope or does not exist.
    /// </summary>
    /// <remarks>This needs a ROW_NUMBER() over the order, so its cost grows with the position.</remarks>
    private method _GetAbsoluteOffset(nRec as DWORD) as long
        SELF:_command:CommandText := _builder:BuildRowNumberStatement(nRec)
        var result := SELF:_command:ExecuteScalar(SELF:_oTd:Name)
        if result == null .or. result == DBNull.Value
            return -1
        endif
        return Convert.ToInt32(result) - 1

    /// <summary>
    /// Switch the buffer from seek-anchored coordinates (see _anchorWhere) to absolute
    /// coordinates, keeping its contents. Needed before paging backwards past the seek anchor
    /// and to report an absolute position.
    /// </summary>
    private method _ConvertToAbsolute() as logic
        if String.IsNullOrEmpty(SELF:_anchorWhere)
            return true
        endif
        if SELF:RowCount == 0 .or. !SELF:_bufferContiguous
            return false
        endif
        var nOffset := SELF:_GetAbsoluteOffset(Convert.ToUInt32(SELF:DataTable:Rows[0][self:_recnoColumNo]))
        if nOffset < 0
            return false
        endif
        SELF:_anchorWhere := ""
        SELF:_bufferStart := nOffset
        return true

    PRIVATE METHOD _GotoRecord(nRec as DWORD) AS LOGIC
        if SELF:_tableMode == TableMode.Table .and. ;
            !(SELF:_bufferContiguous .and. String.IsNullOrEmpty(SELF:_anchorWhere) .and. SELF:RowCount > 0) ;
            .and. SELF:_LoadWindowAt(nRec, -1)
            // Nothing that could be extended: the buffer now starts at the record, anchored at its
            // key, and its position in the order is only looked up when needed.
            RETURN TRUE
        endif
        // GoTo() only calls _GotoRecord() after it already established that nRec is NOT in
        // the currently loaded buffer, so its position in the order must be looked up.
        var nOffset := SELF:_GetAbsoluteOffset(nRec)
        if nOffset < 0
            // nRec does not satisfy the current order's FOR-condition/scope. DBF's GoTo() is a
            // physical positioning operation, independent of the active order: it must still
            // succeed when the record exists at all - Found/OrderKeyNo separately reflect that
            // it has no valid position in this order.
            return SELF:_GotoRecordOutsideOrder(nRec)
        endif
        if SELF:_tableMode != TableMode.Table
            SELF:_ClearTable()
            SELF:DataTable := SELF:_ReadTable("", 0, 0)
        else
            // When the record sits right next to the buffered slice, extend the buffer instead
            // of replacing it. A browse that repaints rows on both sides of a page boundary
            // (e.g. GoTo(row) / GoTo(current) for every visible row) otherwise throws away the
            // other page on every single call and has to fetch it again on the next one.
            local lLoaded := false as logic
            if SELF:_bufferContiguous .and. String.IsNullOrEmpty(SELF:_anchorWhere) .and. SELF:RowCount > 0
                var nBufferEnd := SELF:_bufferStart + SELF:RowCount
                if nOffset >= nBufferEnd .and. nOffset < nBufferEnd + _oTd:PageSize .and. !SELF:_hasEOF
                    // the page must reach the record, a smaller read-ahead would stop short of it
                    if SELF:_readAheadRows > 0 .and. SELF:_readAheadRows <= nOffset - nBufferEnd
                        SELF:_readAheadRows := 0
                    endif
                    lLoaded := SELF:_FetchForward() > 0
                elseif nOffset < SELF:_bufferStart .and. nOffset >= SELF:_bufferStart - _oTd:PageSize
                    lLoaded := SELF:_FetchBackward(SELF:_bufferStart - nOffset) > 0
                endif
            endif
            if !lLoaded .and. !SELF:_LoadWindowAt(nRec, nOffset)
                // Center the record in a small page, so that its neighbours on either side are
                // buffered as well.
                SELF:_anchorWhere := ""
                SELF:_LoadBufferAt(nOffset - SELF:_SmallPageRows / 2, SELF:_SmallPageRows)
            endif
        endif
        var nRow := SELF:_FindRowInBuffer(nRec)
        if nRow == 0
            RETURN FALSE
        endif
        SELF:RowNumber := nRow
        SELF:_SetEOF(FALSE)
        SELF:_SetBOF(FALSE)
        RETURN TRUE

    /// <summary>
    /// Replace the buffer with a small page that starts at record nRec, found by its key in the
    /// current order (see _KeysetCondition()) instead of its offset: with OFFSET the server reads
    /// through all rows before it (10 ms for a 10 row page at offset 500 of a 1000 row table, 2 ms
    /// by key). Code that goes to a record mostly reads only it and the next or previous one;
    /// reading on makes the pages grow, in both directions.
    /// </summary>
    /// <param name="nOffset">The record's 0 based offset in the order when known, otherwise -1:
    /// the buffer is then anchored at the record.</param>
    /// <returns>FALSE when the key cannot be expressed or the record is not part of the order.</returns>
    PRIVATE METHOD _LoadWindowAt(nRec as DWORD, nOffset as long) AS LOGIC
        local oRow as DataRow
        if SELF:CurrentOrder != null
            // only orders on plain columns, a key expression has no literal value
            foreach var cItem in SELF:CurrentOrder:OrderList
                var cExpr := iif(cItem:EndsWith(" DESC", StringComparison.OrdinalIgnoreCase), cItem:Substring(0, cItem:Length - 5), cItem)
                if SELF:DataTable == null .or. SELF:DataTable:Columns:IndexOf(cExpr:Trim():Trim(<char>{'[',']','"','`'})) < 0
                    return false
                endif
            next
        endif
        var nRow := SELF:_FindRowInBuffer(nRec)
        if nRow > 0
            oRow := SELF:DataTable:Rows[nRow-1]
        else
            SELF:_command:CommandText := _builder:BuildDirectRecnoStatement(nRec)
            SELF:_command:ClearParameters()
            var oRecTable := SELF:_command:GetDataTable(SELF:Alias, SELF:_tableTemplate, SELF:_TakeSpareTable())
            if oRecTable == null .or. oRecTable:Rows:Count == 0
                SELF:_ReturnSpareTable(oRecTable)
                return false
            endif
            oRow := oRecTable:Rows[0]
        endif
        var cFrom := SELF:_KeysetCondition(oRow, FALSE, TRUE, TRUE)
        if oRow:Table != SELF:DataTable
            // the record was read only for its key
            SELF:_ReturnSpareTable(oRow:Table)
        endif
        if cFrom == null
            return false
        endif
        var nCount   := SELF:_SmallPageRows
        var newTable := SELF:_ReadTable(cFrom, 0, nCount)
        if newTable == null .or. newTable:Rows:Count == 0 .or. Convert.ToUInt32(newTable:Rows[0][self:_recnoColumNo]) != nRec
            // not part of the order (FOR condition, scope)
            return false
        endif
        var nRows := newTable:Rows:Count
        SELF:_lazyGotos := 0
        SELF:_ClearTable()
        SELF:DataTable := newTable
        SELF:RowNumber := 1
        SELF:_outsideOrder := false
        SELF:_bufferContiguous := true
        if nOffset >= 0
            SELF:_anchorWhere := ""
            SELF:_bufferStart := nOffset
        else
            SELF:_anchorWhere := cFrom
            SELF:_bufferStart := 0
        endif
        SELF:_hasEOF := nRows < nCount
        SELF:_readAheadRows := iif(nRows < nCount, 0, nCount * 2)
        SELF:_readBackRows := nCount
        SELF:_SetEOF(FALSE)
        SELF:_SetBOF(FALSE)
        return true

    PRIVATE METHOD _GotoRecordOutsideOrder(nRec as DWORD) AS LOGIC
        if !SELF:_LoadSingleRecord(nRec)
            // Does not exist even physically.
            return false
        endif
        SELF:_outsideOrder := true
        return true

    /// <summary>
    /// Replace the buffer with just record nRec, fetched by its recno (a primary key lookup),
    /// without determining its position in the current order. The buffer is marked as not
    /// contiguous, so the position is only looked up once something actually needs it
    /// (Skip(), OrderKeyNo, ...).
    /// </summary>
    PRIVATE METHOD _LoadSingleRecord(nRec as DWORD) AS LOGIC
        return SELF:_FetchSingleRecord(nRec) == 1

    /// <summary>Like _LoadSingleRecord(): 1 = loaded, 0 = the record does not exist, -1 = error.</summary>
    PRIVATE METHOD _FetchSingleRecord(nRec as DWORD) AS LONG
        try
            SELF:_lazyGotos := 0
            SELF:_command:CommandText := _builder:BuildDirectRecnoStatement(nRec)
            SELF:_command:ClearParameters()
            var oTable := SELF:_command:GetDataTable(SELF:Alias, SELF:_tableTemplate, SELF:_TakeSpareTable())
            if oTable == null
                return -1
            elseif oTable:Rows:Count == 0
                SELF:_ReturnSpareTable(oTable)
                return 0
            endif
            SELF:_ClearTable()
            SELF:DataTable := oTable
            SELF:RowNumber := 1
            SELF:_outsideOrder := false
            SELF:_hasEOF := false
            SELF:_anchorWhere := ""
            SELF:_bufferStart := 0
            SELF:_bufferContiguous := false
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(FALSE)
            return 1
        catch as Exception
            return -1
        end try

    /// <summary>
    /// After the order changed (and the cursor was reset), stay on record nRec like DBF does,
    /// without paying for its position in the new order up front.
    /// </summary>
    /// <remarks>
    /// Code that switches orders around a lookup (SetOrder(n) / Seek() / SetOrder(m)) is common,
    /// and under DBF/ADS the order switches are free. Positioning eagerly here meant a
    /// ROW_NUMBER() over the whole order plus a page fetch at a large OFFSET for every switch.
    /// </remarks>
    PRIVATE METHOD _RepositionAfterOrderChange(nRec as DWORD, lWasEof as LOGIC, aCurrentRow as object[]) AS VOID
        if SELF:_tableMode != TableMode.Table .or. SELF:_recnoColumNo == -1
            SELF:GoTo(iif(lWasEof, 0, nRec))
        elseif lWasEof
            // EOF stays EOF (e.g. switching back to the previous order after a seek that found
            // nothing, before going back to the saved record). GoTo(0) would first load the
            // first page of the new order; an empty buffer is enough to stand on the phantom
            // record. _GoToPhantom() marks the EOF as pending (the empty buffer does not end at
            // the last record), so Skip(1) stays at EOF and Skip(-1) goes to the bottom.
            if SELF:_SetBufferRows(null) .or. SELF:_OpenTable("1=0", "")
                // BOF stays as it was, see OrderListFocus()
                SELF:_GoToPhantom()
            else
                SELF:GoTo(0)
            endif
        elseif aCurrentRow != null .and. SELF:_SetBufferRows(aCurrentRow)
            // The record we stand on was in the buffer a moment ago (written by GoCold() before the
            // order change): keep it instead of reading it again. Its position in the new order is
            // looked up when needed, as after _LoadSingleRecord().
            SELF:_bufferContiguous := false
        elseif !SELF:_LoadSingleRecord(nRec)
            SELF:GoTo(nRec)
        endif
        return

    /// <summary>After an order change at EOF keep BOF as it was before, as ADS does.</summary>
    PRIVATE METHOD _RestoreBofAtEof(lWasEof as logic, lWasBof as logic) AS VOID
        if lWasEof .and. SELF:_EoF
            SELF:_SetBOF(lWasBof)
        endif
        return

    /// <summary>An empty table with the buffer's structure to load a query result into, or NULL.</summary>
    PRIVATE METHOD _TakeSpareTable() AS DataTable
        var oTable := SELF:_spareTable
        SELF:_spareTable := null
        return oTable

    /// <summary>
    /// After a new record lock: read the locked record again when the work area stands on it and has
    /// no pending changes, as ADS does.
    /// </summary>
    /// <remarks>
    /// Code written for DBF/ADS locks a record, reads it and writes it back. A value read before the
    /// lock (from the buffer or the result cache) could overwrite a change another user committed in
    /// the meantime. Only the values of the row are replaced, the buffer and the position stay.
    /// </remarks>
    PRIVATE METHOD _RefreshLockedRow(nRecNo as int) AS VOID
        var row := SELF:CurrentRow
        if nRecNo <= 0 .or. SELF:_tableMode != TableMode.Table .or. SELF:_recnoColumNo < 0 .or. SELF:_EoF ;
            .or. row == null .or. row == SELF:_phantomRow .or. row:RowState != DataRowState.Unchanged .or. SELF:_updatedRecNos:Count > 0
            return
        endif
        if Convert.ToInt32(row[SELF:_recnoColumNo]) != nRecNo
            return
        endif
        try
            SELF:_command:CommandText := _builder:BuildDirectRecnoStatement((DWORD) nRecNo)
            SELF:_command:ClearParameters()
            var oTable := SELF:_command:GetDataTable(SELF:Alias, SELF:_tableTemplate, SELF:_TakeSpareTable())
            if oTable != null
                if oTable:Rows:Count == 1
                    var oNew := oTable:Rows[0]
                    // column by column: the recno column is read only (ItemArray would stop there
                    // and leave the row in edit mode)
                    foreach oColumn as DataColumn in row:Table:Columns
                        if !oColumn:ReadOnly
                            row[oColumn] := oNew[oColumn:Ordinal]
                        endif
                    next
                    row:AcceptChanges()
                endif
                SELF:_ReturnSpareTable(oTable)
            endif
        catch as Exception
            // keep the values read before
            row:RejectChanges()
        end try
        return

    /// <summary>Keep a table that is no longer used (a replaced buffer, a page whose rows were copied into
    /// the buffer) for the next query, see _TakeSpareTable().</summary>
    PRIVATE METHOD _ReturnSpareTable(oTable as DataTable) AS VOID
        if oTable != null .and. oTable != SELF:_table .and. SELF:_tableTemplate != null ;
            .and. oTable:Columns:Count == SELF:_tableTemplate:Columns:Count
            oTable:Rows:Clear()
            SELF:_spareTable := oTable
        endif
        return

    /// <summary>
    /// Replace the buffer with an empty table (aRow NULL) or one row, built from the structure of
    /// the previous buffer without a query. FALSE when that structure is not known yet.
    /// </summary>
    PRIVATE METHOD _SetBufferRows(aRow as object[]) AS LOGIC
        if SELF:_tableTemplate == null
            return false
        endif
        var oTable := SELF:_TakeSpareTable()
        if oTable == null
            oTable := SELF:_tableTemplate:Clone()
        endif
        if aRow != null
            oTable:LoadDataRow(aRow, true)
        endif
        SELF:_lazyGotos := 0
        SELF:_ClearTable()
        SELF:DataTable := oTable
        SELF:RowNumber := 1
        SELF:_outsideOrder := false
        SELF:_hasEOF := false
        SELF:_anchorWhere := ""
        SELF:_bufferStart := 0
        SELF:_bufferContiguous := true
        SELF:_readAheadRows := 0
        SELF:_readBackRows := 0
        SELF:_serverReccountValid := false
        SELF:_SetEOF(oTable:Rows:Count == 0)
        SELF:_SetBOF(FALSE)
        return true

    /// <summary>
    /// When the buffer only holds records whose position in the current order is unknown
    /// (see _LoadSingleRecord()), look that position up for the current record now.
    /// Sets _outsideOrder when the record is not part of the order at all.
    /// Returns FALSE when pending changes could not be written.
    /// </summary>
    PRIVATE METHOD _EnsurePositioned() AS LOGIC
        if SELF:_tableMode == TableMode.Table .and. !SELF:_bufferContiguous .and. !SELF:_outsideOrder ;
            .and. !SELF:_EoF .and. SELF:RowCount > 0 .and. SELF:RowNumber >= 1 .and. SELF:RowNumber <= SELF:RowCount
            // Loading the window replaces the buffer: write the changes of the record first, or
            // GoTo(n), FieldPut(), Skip() would lose them.
            if !SELF:GoCold()
                return false
            endif
            SELF:_GotoRecord(Convert.ToUInt32(SELF:CurrentRow[self:_recnoColumNo]))
        endif
        return true

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
        local row := NULL as DataRow
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

            // Check row lock - unless we hold it (or the file lock) ourselves: our lock row in
            // the lock table already keeps everybody else out, so there is nothing to ask.
            if SELF:Connection:UseLocks .and. !SELF:_ownLocks:Contains(nRecNo) .and. !SELF:_ownLocks:Contains(0)
                var dbLockInfo := DbLockInfo{}
                dbLockInfo:RecId := row[_oTd:RecnoColumn]
                var myLock := false
                var otherLock := false
                SELF:CheckLock(dbLockInfo, StringBuilder{}, REF myLock, REF otherLock)
                if otherLock
                    return false
                endif

                if !myLock .and. SELF:Lock(ref dbLockInfo)
                    // Locked only for this write - GoCold() releases it again afterwards.
                    SELF:_autoLockedRecNos:Add(nRecNo)
                endif
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
                    (lockInfo:Method != XSharp.RDD.Support.DbLockInfo.LockMethod.File .and. recNoTemp = SELF:LockRecNo(REF lockInfo))
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

