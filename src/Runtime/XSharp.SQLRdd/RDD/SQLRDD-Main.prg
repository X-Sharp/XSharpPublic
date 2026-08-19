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
using System.Reflection
using System.Data.Common
using System.Linq
using XSharp.RDD.SqlRDD.Providers

#undef TRACERDD

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlRDD class.
/// </summary>
[DebuggerDisplay("SQLRDD ({Alias,nq})")];
partial class SQLRDD inherit Workarea

    // Overridden properties and methods, these should all be documented

#region Overridden properties
    override property Driver as string get "SQLRDD"
#endregion
    #region Own properties
    /// <summary>Return the # of fields/Columns in the current work area,
    /// including the RecnoColumn and DeletedColum (if they exist).</summary>
    public property RealFieldCount as long => Super:FieldCount

    public override property FieldCount  as long => super:FieldCount - self:_numHiddenColumns

    /// <summary>Returns the # of rows in the local buffer (DataTable).</summary>
    public property RowCount    as INT => iif(Self:DataTable == null, 0, Self:DataTable:Rows:Count)

    /// <summary>The current rownumber in the buffer (DataTable).</summary>
    public property RowNumber   as INT GET _rowNumber private SET _rowNumber := value

    /// <summary>The current row in the buffer (DataTable).
    /// When the server is at EOF then the phantomrow is returned.</summary>
    public property CurrentRow as DataRow
        get
            var nRow := self:RowNumber
            if nRow == 0 .or. nRow > self:RowCount
                return self:_phantomRow
            endif
            if self:_table == null .or. ! SELF:_hasData
                return self:_phantomRow
            endif
            return self:DataTable:Rows[nRow -1]
        end get
    end property
    /// <summary>A numeric value representing the logical record number of the current record.
    /// When the server is not in tablemode 0 is returned</summary>
    public property OrderKeyNo as dword
        get
            if self:_tableMode != TableMode.Table
                return 0
            endif
            try
                SELF:_command:CommandText := _builder:BuildRowNumberStatement(self:RecNo)
                var result := SELF:_command:ExecuteScalar(SELF:_oTd:Name)
                var iResult := Convert.ToUInt32(result)
                return iResult
            catch as Exception
                return 0
            end try
        end get
    end property
    /// <summary>A numeric value representing the number of records in the current order.
    /// When the table is in natural order or not in table mode 0 is returned</summary>
    public property OrderKeyCount as dword
        get
            if self:_tableMode != TableMode.Table .or.;
                self:CurrentOrder = null
                return 0
            endif
            try
                return self:_builder:GetOrderKeyCount()
            catch as Exception
                return 0
            end try
        end get
    end property

    #endregion




    /// <summary>Create a table.</summary>
    /// <param name="info">object describing the file to create.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method Create(info as DbOpenInfo) as logic
        _cTable := System.IO.Path.GetFileName(info:FileName)
        if ! self:_PrepareOpen(info)
            return false
        endif
        self:_tableMode := TableMode.Table
		local lResult := TRUE AS LOGIC
        SELF:SetOpenInfo(info, "", TRUE)
        self:_creating := true
        self:_Fields := self:_adjustCreateFields(self:_Fields)
        self:_creating := false
        self:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        // create SQL table Now
        self:_CreateSqlFields(self:_Fields)
        if lResult
            self:Connection:MetadataProvider:CreateTable(_cTable, info )
        endif
        return lResult
    end method

    /// <summary>Open a table.</summary>
    /// <param name="info">object describing the file to open.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// Open() will open the table and read the table structure from the database.
    /// If the table is a query then the data will be read from the database as well.
    /// If the table is a table name, then the data will be read from the database later
    /// </remarks>
    override method Open(info as DbOpenInfo) as logic
        if ! self:_PrepareOpen(info)
            return false
        endif
		SELF:SetOpenInfo(info, "", FALSE)
        var cQuery := info:FileName
        if cQuery:IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0
            cQuery := System.IO.Path.GetFileNameWithoutExtension(cQuery)
        endif
        // Determine if this is a single table name or a query (select or Execute)
        var selectStmt := XSharp.SQLHelpers.ReturnsRows(cQuery)
        if (selectStmt)
            self:_tableMode     := TableMode.Query
            var longFieldNames  := _connection:LongFieldNames
            self:_oTd           := _connection:GetStructureForQuery(cQuery,"QUERY",longFieldNames)
            _command:CommandText := cQuery
        else
            self:_tableMode := TableMode.Table
            if ! self:_GetTableInfo(cQuery)
                throw Exception{}
            endif
            if !self:_oTd:HasRecnoColumn
                throw Exception{"RecnoColumn is required for table " + info:FileName}
            endif
        endif
        local aUpdatableColumns := null as HashSet<string>
        if _oTd:HasUpdatableColumns
            aUpdatableColumns    := HashSet<string>{_oTd:UpdatableColumns:Split(','), StringComparer.OrdinalIgnoreCase}
        endif
        local aKeyColumns := null as HashSet<string>
        if _oTd:HasKeyColumns .or. _oTd:HasRecnoColumn
            if _oTd:HasKeyColumns
                aKeyColumns          := HashSet<string>{_oTd:KeyColumns:Split(','), System.StringComparer.OrdinalIgnoreCase}
            else
                aKeyColumns          := HashSet<string>{System.StringComparer.OrdinalIgnoreCase}
            endif
            if _oTd:HasRecnoColumn .and. ! aKeyColumns:Contains(_oTd:RecnoColumn)
                aKeyColumns:Add(_oTd:RecnoColumn)
            endif
        endif

        // Get the structure
        var lhasRecnoColumn := false
        var oFields := List<RddFieldInfo>{}
        self:_updatableColumns := List<RddFieldInfo>{}
        self:_keyColumns       := List<RddFieldInfo>{}
        self:_numHiddenColumns := 0
        var colNo := 0
        foreach var oCol in _oTd:Columns
            var oField := oCol:ColumnInfo
            // DBF/ADS fields have no notion of a per-cell SQL NULL - a NULL value already comes
            // back from GetValue() as the field's normal typed blank, same as a non-nullable
            // column would. A genuine ADS/DBF field never carries DBFFieldFlags.Nullable, since
            // that distinction doesn't exist there - so this must be cleared here too, or
            // FieldInfo(DBS_TYPE) decorates the type char with ":0" (e.g. "L:0" instead of "L")
            // purely because the underlying SQL column happens to allow NULL. Generic FIELDGET
            // -style code written once against classic DBF semantics (e.g. an exact "== 'L'"
            // check, with no reason to ever expect a suffix) then silently takes the "unknown
            // type" path for SQL-backed tables only - breaking the "switch ADS/SQL by a flag"
            // compatibility this RDD exists for.
            oField:Flags &= ~DBFFieldFlags.Nullable
            if ! String.Equals(oField:ColumnName, oField:Name, StringComparison.OrdinalIgnoreCase)
                oField:Alias := oField:ColumnName:Trim():ToUpperInvariant()
            endif

            if oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Recno)
                self:_recnoColumNo := colNo
                oField:Flags |= DBFFieldFlags.AutoIncrement
                oField:Flags |= DBFFieldFlags.System
                self:_numHiddenColumns += 1
                lhasRecnoColumn := true
            elseif oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Deleted)
                self:_deletedColumnNo := colNo
                self:_deletedColumnIsLogic := oField:FieldType == DbFieldType.Logic
                oField:Flags |= DBFFieldFlags.System
                self:_numHiddenColumns += 1
            endif
            oFields:Add(oField)
            if aKeyColumns == null
                if self:_oTd:CompareMemo
                    self:_keyColumns:Add(oField)
                elseif !oField:FieldType:IsLong()
                    self:_keyColumns:Add(oField)
                endif
            elseif aKeyColumns:Contains(oField:ColumnName)
                self:_keyColumns:Add(oField)
            endif
            IF !oField:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                if aUpdatableColumns  == null
                    self:_updatableColumns:Add(oField)
                elseif aUpdatableColumns:Contains(oField:ColumnName)
                    self:_updatableColumns:Add(oField)
                endif
            ENDIF
            colNo++
        next

        if !lhasRecnoColumn .and. self:_tableMode = TableMode.Table
            if !String.IsNullOrEmpty(_oTd:RecnoColumn)
                throw Exception{"Recno column '"+_oTd:RecnoColumn+"' not found"}
            else
                throw Exception{"No column is flagged as the Recno column in the result set"}
            endif
        endif

        var aFields := oFields:ToArray()

        info:ReadOnly := false
        SUPER:CreateFields(aFields)
        // Add fields and set flong field names
        for var nI := 1 to aFields:Length
            var aField := aFields[nI-1]
            if !_fieldNames:ContainsKey(aField:ColumnName)
                _fieldNames:Add(aField:ColumnName, nI-1)

            endif
        next
        if self:_tableMode == TableMode.Table
            cQuery := self:_oTd:EmptySelectStatement
            self:_CloseCursor()
        else
            self:DataTable      := _command:GetDataTable(self:Alias)
            if self:DataTable == null
                // Unlike Table mode, _ForceOpen() is a permanent no-op once _tableMode is
                // Query (see _ForceOpen() below), so a failed SELECT here has no later retry
                // path - DataTable would stay null for the object's entire lifetime and every
                // subsequent access (Rows:Count right below, _GotoRow, GoTop, ...) would NPE.
                self:_dbfError(self:Connection:LastException, Subcodes.EDB_USE, Gencode.EG_OPEN, "SQLRDD.Open", FALSE)
                return false
            endif
            SELF:_serverReccount := (DWORD) self:DataTable:Rows:Count
            SELF:_ReadOnly := true
            SELF:_hasEOF := TRUE
        endif
        _command:CommandText := cQuery
        self:_ForceOpen()
        return true
    end method



    /// <summary>Append a blank row and position the cursor to the new row.</summary>
    /// <param name="lReleaseLock">A flag that is TRUE if you want to clear all pending row locks before appending the new row and FALSE if you want to add the new row to the end of the current lock list.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method Append(lReleaseLock as logic) as logic
        if !self:_ForceOpen()
            return false
        endif
        var lResult := SELF:GoCold()
        if lResult
            var key := (dword) self:_builder:GetNextKey()
            var row := self:DataTable:NewRow()
            self:DataTable:Rows:Add(row)
            if _emptyValues == null
                self:_GetEmptyValues()
            endif
            var values := (object[])_emptyValues:Clone()
            foreach c as DataColumn in self:DataTable:Columns
                if c:AutoIncrement .or. c:ColumnName == SELF:_oTd:RecnoColumn
                    row[c] := key
                else
                    row[c] := SELF:_HandleNullDate(values[c:Ordinal],c)
                endif
            next
            self:_ExecuteInsertStatement(row)
            row:AcceptChanges()
            _updatedRecNos:Add((int)row[self:_recnoColumNo])
            SELF:RowNumber := SELF:DataTable:Rows:Count
            self:_serverReccount += 1
            SELF:_SetBOF(FALSE)
            SELF:_SetEOF(FALSE)
            self:GoHot()
        endif
        return lResult
    end method

    /// <summary>Get a value for the specified column.</summary>
    /// <param name="nFldPos">The ONE based position of the column whose value you want to obtain.</param>
    /// <returns>The value of the specified field.</returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method GetValue(nFldPos as int) as object
        if self:CurrentRow == null
            return null
        endif

        // nFldPos is 1 based, the RDD compiles with /az+
        if nFldPos > 0 .and. nFldPos <= self:RealFieldCount
            var col := self:_GetColumn(nFldPos)
            nFldPos -= 1
            local result as object
            result  := SELF:CurrentRow[nFldPos]
            if result is string var strValue .and. ! _creatingIndex
                if self:_trimValues
                    result := strValue:TrimEnd()
                else
                    result := strValue:PadRight(_Fields[nFldPos]:Length,' ')
                endif
            endif
            if result is DateTime var dtValue
                if self:_Fields[nFldPos]:FieldType == DbFieldType.Date
                    if dtValue == DateTime.MinValue
                        result := DbDate{0,0,0}
                    else
                        result := DbDate{dtValue:Year, dtValue:Month, dtValue:Day}
                    endif
                endif
            endif
            if result is Decimal var decValue
                if col:Decimals == 0
                    result := Convert.ToInt64(decValue)
                endif
            endif
            if result == DBNull.Value
                // A DBF-style field has no notion of a per-cell SQL NULL, only "blank" - and
                // the phantom row already holds the correctly-typed blank for every column (see
                // the DataTable setter). UseNulls does NOT gate this: despite its doc comment,
                // it never affects the phantom row itself (that's always blank, never NULL) -
                // skipping this substitution only starves a REAL row's NULL cell of the same
                // typing. Left as raw DBNull.Value, callers expecting the field's real type
                // break - e.g. a nullable "bit" column read as NULL surfaces as an empty string
                // once generic Usual marshaling gets hold of it, so NOT() on that field throws a
                // STRING-to-LOGIC conversion error instead of just seeing FALSE.
                result := _phantomRow[nFldPos]
                if result is DateTime .and. self:_Fields[nFldPos]:FieldType == DbFieldType.Date
                    result := DbDate{0,0,0}
                endif
            elseif _creatingIndex .and. result is string var strResult
                result := strResult:PadRight(_Fields[nFldPos]:Length,' ')
            endif
            #ifdef TRACERDD
                System.Diagnostics.Debug.WriteLine("GetValue recno {0}, row {1}, col {2}, value {3} ", RecNo, RowNumber, nFldPos, result)
            #endif
            return result
        endif
        return super:GetValue(nFldPos)
    end method


    /// <summary>Write a value for a specified column</summary>
    /// <param name="nFldPos">ONE based position for which the value should be written.</param>
    /// <param name="oValue">New value that needs to written to the table this column.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method PutValue(nFldPos as int, oValue as object) as logic
        // nFldPos is 1 based, the RDD compiles with /az+
        if !SELF:_ForceOpen()
            return false
        endif
        if self:_ReadOnly
            self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:PutValue", "Table is not Updatable" )
            return false
        endif
        if self:EoF
            return true
        endif
        var result := false
        if nFldPos == SELF:_recnoColumNo +1 .or. nFldPos == SELF:_deletedColumnNo +1
            // silently ingore the fieldput
            return true
        endif
        if nFldPos > 0 .and. nFldPos <= self:RealFieldCount
            var col := self:_GetColumn(nFldPos)
            if SELF:_updatableColumns:Contains(col)
                var row := SELF:CurrentRow
                if !_updatedRecNos:Contains((int)row[self:_recnoColumNo])
                    _updatedRecNos:Add((int)row[self:_recnoColumNo])
                endif

                row[nFldPos-1] := SELF:_HandleNullDate(oValue,self:DataTable:Columns[nFldPos-1])
                result := true
                SELF:GoHot()
            else
                self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:PutValue", i"Column {col.ColumnName} is not Updatable"  )
            endif
        endif
        return result


    /// <summary>Write the contents of a work area's memory to the data store (usually a disk).</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method GoCold() as logic
        var current := SELF:CurrentRow
        if current == null
            return false
        endif
        // RowState alone misses rows only marked via Delete()/Recall() without a DeletedColumn,
        // since those touch no DataColumn and leave RowState Unchanged.
        // The phantom row (CurrentRow when positioned at EOF/an empty result) is a placeholder
        // created via DataTable:NewRow() but never added to the table, so its RowState is
        // Detached - always "!= Unchanged" even though nothing was ever actually edited. Without
        // excluding it explicitly, every GoCold() call while on an empty/EOF cursor spuriously
        // looked "hot", forcing a full recount (_GetRecCount()) on every single call - observed
        // to defeat the GoBottom() recount cache for any Lister stuck retrying GoTo(0) against a
        // table with no matching rows (e.g. PLZInfo/stradb before a city is chosen).
        var lWasHot := (current != self:_phantomRow) .and. (current:RowState != DataRowState.Unchanged .or. _updatedRecNos:Count > 0)
        local lOk := TRUE as logic
        if lWasHot .and. self:DataTable != null

            // Check file lock
            var dbLockInfo := DbLockInfo{}
            dbLockInfo:RecId := 0
            var myLock := false
            var otherLock := false
            SELF:CheckLock(dbLockInfo, StringBuilder{}, myLock, otherLock)
            if (otherLock)
                return false
            endif

            foreach var rowId in _updatedRecNos
                lOk := self:_UpdateRow(rowId)
                if !lOk
                    exit
                endif
            next
            if lOk
                self:DataTable:AcceptChanges()
                self:UnLock(0)
            else
                self:DataTable:RejectChanges()
            endif
            _updatedRecNos:Clear()
            // A write may have changed the true row count (insert/physical delete) - force a
            // real requery here rather than trusting a cached pre-write value.
            self:_serverReccountValid := false
            self:_GetRecCount()
        endif
        return lOk
    end method

    override method GoHot() as logic
        return true
    end method

    /// <summary>Close a table.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method Close() as logic
        local lOk as logic
        lOk := SELF:GoCold()
        self:UnLock(0)
        // This method deletes the temporary file after the file is closed
        foreach var bag in self:OrderBagList
            bag:Close()
        next
        _connection?:UnregisterRdd(self)

        lOk := super:Close()
        return lOk
    end method

    /// <summary>Is the given row marked for deletion?</summary>
    /// <remarks>
    /// When a DeletedColumn is defined, then the value of that column is checked.
    /// Otherwise the row's recno is looked up in the internal _deletedRowIds set,
    /// since Workarea.Deleted is a hardcoded stub that cannot track this state.
    /// </remarks>
    private method _IsRowDeleted(row as DataRow) as logic
        if self:_deletedColumnNo > -1
            var res := row[self:_deletedColumnNo]
            if res is logic
                return (logic) res
            else
                try
                    var iRes := Convert.ToInt64(res)
                    return iRes != 0
                catch
                    return false
                end try
            endif
        endif
        return self:_deletedRowIds:Contains((int)row[self:_recnoColumNo])
    end method

    /// <summary>Mark the row at the current cursor position for deletion.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When a DeletedColumn is defined, then his set the value of that column to TRUE
    /// Otherwise current row is deleted. The deletion at the server will be done when the record pointer is moved or the table is closed.
    /// </remarks>

    override method Delete() as logic
        var row := self:CurrentRow
        if self:_deletedColumnNo > -1
            if self:_deletedColumnIsLogic
                row[_deletedColumnNo] := true
            else
                row[_deletedColumnNo] := 1
            endif
        else
            self:_deletedRowIds:Add((int)row[self:_recnoColumNo])
        endif
        if !_updatedRecNos:Contains((int)row[self:_recnoColumNo])
            _updatedRecNos:Add((int)row[self:_recnoColumNo])
        endif
        self:GoHot()
        return true
    end method

    /// <summary>Remove the deletion marker from the row at the current cursor position.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When a DeletedColumn is defined, then his set the value of that column to TRUE
    /// Otherwise when the current row is deleted and not persisted to the server yet, then the deletion is undone.
    /// </remarks>
    override method Recall() as logic
        var row := self:CurrentRow
        if self:_deletedColumnNo >= 0
            if self:_deletedColumnIsLogic
                row[_deletedColumnNo] := false
            else
                row[_deletedColumnNo] := 0
            endif
        else
            self:_deletedRowIds:Remove((int)row[self:_recnoColumNo])
        endif
        if !_updatedRecNos:Contains((int)row[self:_recnoColumNo])
            _updatedRecNos:Add((int)row[self:_recnoColumNo])
        endif
        self:GoHot()
        return true
    end method

    /// <summary>Retrieve and optionally change information about a work area.</summary>
    /// <param name="nOrdinal">Specifies the type of information.</param>
    /// <param name="oValue">If specified (not null), then this parameter is used to change the value of a setting.</param>
    override method Info(uiOrdinal as long, oNewValue as object) as object
        if uiOrdinal == DbInfo.DBI_CANPUTREC
            return false
        elseif uiOrdinal == DbInfo.DBI_ISDBF
            return false
        elseif uiOrdinal == DbInfo.DBI_GETLOCKARRAY
            return SELF:_GetMyLockedRecords()
        elseif uiOrdinal == DbInfo.DBI_LOCKCOUNT
            return (int) SELF:_GetMyLockedRecords():Length
        endif
        return super:Info(uiOrdinal, oNewValue)
    end method

    /// <summary>
    /// Records currently locked by this connection in this table, per the xs_locks table.
    /// </summary>
    /// <remarks>
    /// Backs DBI_GETLOCKARRAY/DBI_LOCKCOUNT (and therefore VO's RLockList/IsLocked()), since
    /// SQLRDD's locking is tracked entirely in xs_locks rather than in the base RDD's own
    /// bookkeeping. Without this, IsLocked() always reports false for SQLRDD tables, and
    /// callers relying on it (e.g. HomeBase's transaction end) never commit pending changes.
    /// </remarks>
    private method _GetMyLockedRecords() as DWORD[]
        var recNos := List<DWORD>{}
        if Connection?:Provider is null .or. !self:Connection:IsOpen .or. _oTd == null
            return recNos:ToArray()
        endif
        try
            var sb := StringBuilder{}
            sb:AppendLine("select recno from " + SqlDbConnection.LockTableName)
            sb:AppendLine("where tablename = "+self:Provider:ParameterPrefix+"p1")
            sb:AppendLine(" and connectionid = "+self:Provider:ParameterPrefix+"p2")
            sb:AppendLine(" and workarea = "+self:Provider:ParameterPrefix+"p3")

            using var cmd := SqlDbCommand{"GetLockArray", self:Connection, false}
            cmd:CommandText := sb:ToString()
            cmd:AddParameter(self:Provider:ParameterPrefix+"p1", _oTd:RealName)
            cmd:AddParameter(self:Provider:ParameterPrefix+"p2", self:Connection:ConnectionId:ToString())
            cmd:AddParameter(self:Provider:ParameterPrefix+"p3", (int)super:Area)

            using var reader := cmd:ExecuteReader()
            do while reader:Read()
                var recNoTemp := (int) reader["recno"]
                if recNoTemp > 0
                    recNos:Add((DWORD) recNoTemp)
                endif
            end do
        catch
            nop
        end try
        return recNos:ToArray()
    end method

    /// <inheritdoc />
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method GoTop() as logic
        if !self:_ForceOpen()
            return false
        endif
        SELF:_outsideOrder := FALSE
        SELF:_ClearTable()
        SELF:_FetchPage( 1)
        SELF:RowNumber  := 1
        SELF:_Top       := TRUE
        SELF:_Bottom    := FALSE
        SELF:_SetEOF(FALSE)
        SELF:_SetBOF(FALSE)
        // Apply Filter and SetDeleted
        VAR result := SELF:SkipFilter(1)
        SELF:_CheckEofBof()
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("GoTop Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", result, _serverReccount, RecNo, RowNumber, EoF, BoF)
        #endif
        RETURN result
    end method

    /// <inheritdoc />
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method GoBottom() as logic
        if !self:_ForceOpen()
            return false
        endif
        SELF:_outsideOrder := FALSE
        SELF:_ClearTable()
        // Route through the cached count instead of always querying directly - GoBottom() can
        // be called many times in a row against unchanged data (e.g. Lister construction
        // retrying GoTo(0) when it can't establish a valid position), and each direct query
        // here used to pay for a fresh COUNT(*) every single time.
        self:_GetRecCount()
        local nMaxRecNo := self:_serverReccount as dword
        var nPage       := nMaxRecNo / self:_oTd:PageSize
        if SELF:RecCount % self:_oTd:PageSize != 0
            nPage += 1
        ENDIF
        // For a large table, fetching the last page via the normal ascending, OFFSET-based
        // query (_FetchPage) forces the server to walk/skip almost the entire ordered result -
        // the further from the top, the worse. _FetchLastPage avoids that by sorting in
        // reverse and asking for OFFSET 0 instead, which is always cheap. Only safe when the
        // order isn't already descending (that would invert the reversal); falls back to the
        // original approach for natural order, descending orders, or if it fails outright.
        if self:CurrentOrder == null .or. !self:CurrentOrder:Descending
            if !SELF:_FetchLastPage((INT) nPage)
                SELF:_FetchPage((INT) nPage)
            endif
        else
            SELF:_FetchPage((INT) nPage)
        endif
        SELF:RowNumber  := SELF:RowCount
        SELF:_Top       := FALSE
        SELF:_Bottom    := TRUE
        // Apply Filter and SetDeleted
        VAR result := SELF:SkipFilter(-1)
        SELF:_CheckEofBof()
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("GoBottom Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", result, _serverReccount, RecNo, RowNumber, EoF, BoF)
        #endif
        RETURN result
    end method

    /// <inheritdoc />
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method SkipRaw(nToSkip as long) as logic
        if !self:_ForceOpen()
            return false
        endif
        LOCAL isOK := TRUE AS LOGIC
        SELF:GoCold()
        IF nToSkip == 0
            NOP
        ELSE
            var newRow := SELF:RowNumber + nToSkip
            IF newRow > 0
                if newRow <= SELF:RowCount
                    SELF:RowNumber :=  newRow
                    SELF:_SetEOF(FALSE)
                ELSEIF SELF:_hasEOF
                    SELF:RowNumber := 0
                    SELF:_SetEOF(TRUE)
                ELSE
                    SELF:RowNumber :=  newRow
                    SELF:_FetchPage(SELF:_currentPageNo +1)
                    if SELF:RowNumber > SELF:RowCount
                        // The page we just fetched turned out to be empty (we were already on
                        // the last real row) - report EOF right away instead of leaving RowNumber
                        // past the end with EOF still FALSE, which otherwise sits stale until a
                        // second Skip() happens to see _hasEOF. In between, RecNo/CurrentRow
                        // reflect the phantom row - e.g. nextrec()'s "IF EOF THEN GOTO(oldRecno)"
                        // never fires on the first PgDn past the end, and the recno it captures
                        // on the following call is the phantom row's blank value, not a real one.
                        SELF:RowNumber := 0
                        SELF:_SetEOF(TRUE)
                    endif
                endif
            ELSE
                IF SELF:_currentPageNo == 1
                    isOK := SELF:GoTop()
                    SELF:_SetBOF(TRUE)
                    SELF:_currentPageNo := 1
                ELSE
                    SELF:_FetchPage(SELF:_currentPageNo -1)
                ENDIF
            ENDIF
        ENDIF
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("SkipRaw Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", isOK, _serverReccount, RecNo, RowNumber, EoF, BoF)
        #endif
        return isOK
    end method


    OVERRIDE METHOD GoToId(oRec AS OBJECT) AS LOGIC
	    LOCAL result AS LOGIC
		TRY
            VAR nRec := Convert.ToUInt32( oRec )
            // CurrentRow is null when the table has never been successfully opened (the
            // phantom row is only built once a SELECT actually loaded a schema - see the
            // DataTable setter) - same case GetValue() already guards against above.
            if self:CurrentRow == null .or. oRec != self:CurrentRow[self:_recnoColumNo]
                result := SELF:GoTo( (DWORD) nRec )
            endif
		CATCH ex AS Exception
			SELF:_dbfError(ex, Subcodes.EDB_GOTO,Gencode.EG_DATATYPE,  "SQLRDD.GoToId",FALSE)
			result := FALSE
		END TRY
        SELF:_CheckEofBof()
    RETURN result
    /// <inheritdoc />
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database <br/>
    /// When a RecnoColumn is defined, then the cursor will be positioned on the row with the specified Recno, when it exists.
    /// If the recno does not exist, or when no RecnoColumn is defined, then the cursor will be positioned on the phantom row at the end of the table.
    /// </remarks>
    override method GoTo(nRec as DWORD) as logic
        if !self:_ForceOpen()
            return false
        endif
        SELF:GoCold()
        IF nRec == 0
            SELF:GoBottom()
            RETURN SELF:Skip(1)
        ENDIF
        // Normal positioning, VO resets FOUND to FALSE after a recprd movement
        SELF:_Found := FALSE
        SELF:_outsideOrder := FALSE
        IF SELF:_tableMode == TableMode.Query .and. self:_recnoColumNo == -1
            RETURN SELF:_GotoRow((LONG) nRec)
        ENDIF
        // Check to see if we have the record in the current buffer. DataTable can be null
        // here if _ForceOpen() above reported success while the underlying SELECT actually
        // failed (see _OpenTable()) - treat that as "not in the current buffer" rather than
        // crashing, and fall through to the direct single-record fetch below.
        if SELF:DataTable != null
            var rowIndex := 1
            foreach oRow as DataRow in SELF:DataTable:Rows
                if (int)oRow[self:_recnoColumNo] = nRec
                    SELF:RowNumber := rowIndex
                    SELF:_SetEOF(FALSE)
                    SELF:_SetBOF(FALSE)
                    SELF:_Found := TRUE
                    return true
                endif
                rowIndex++
            next
        endif

        var found := SELF:_GotoRecord(nRec)
        SELF:_CheckEofBof()
        RETURN found
    end method


    /// <summary>The physical row identifier at the current cursor position.</summary>
    /// <remarks>
    /// When a RecnoColumn is defined, then this will return the value of that column.
    /// Otherwise the relative position inside the cursor will be returned.
    /// </remarks>
    override property RecNo		as DWORD
        get
            if self:_tableMode == TableMode.Query
                return (DWORD) SELF:RowNumber
            endif
            var row := SELF:CurrentRow
            if  ! SELF:_EoF .and. row != null
                var obj := row[self:_recnoColumNo]
                return Convert.ToUInt32(obj)
            endif
            return SELF:RecCount+1
        end get
    end property

    /// <inheritdoc />
    override property RecCount as dword
        get
            return Self:_serverReccount
        end get
    end property

    /// <inheritdoc />
    override method Skip(nToSkip as long) as logic
        LOCAL result := FALSE AS LOGIC
        LOCAL oldRow   := SELF:RowNumber as int
        LOCAL oldRecNo := SELF:RecNo as dword
        SELF:_ForceOpen()
        SELF:_Top := FALSE
        SELF:_Bottom := FALSE
        IF nToSkip == 0
            result := SELF:GoCold()
        ELSEIF SELF:_outsideOrder
            // Positioned (via GoTo()) on a record outside the current order (OrderKeyNo 0).
            // RowNumber/_currentPageNo do not correspond to any real position in the order's
            // sequence, so a relative skip from here is meaningless. Matching DBF: a negative
            // skip always lands on the first record of the order; a positive skip lands on
            // record nToSkip, as if skipping forward from just before the top.
            SELF:GoCold()
            result := SELF:GoTop()
            IF result
                IF nToSkip < 0
                    SELF:BoF := TRUE
                ELSEIF nToSkip > 1
                    result := SELF:Skip(nToSkip - 1)
                ENDIF
            ENDIF
        ELSE
            result := SELF:SkipRaw( nToSkip )
            if result
                result := SELF:SkipFilter( iif(nToSkip > 0, 1, -1) )
            endif
            // We reached the top ?
            IF result
                IF ( nToSkip < 0 ) .AND. SELF:_BoF
                    SELF:GoTop()
                    SELF:BoF := TRUE
                ENDIF
                // when we land at EOF then do not reset the EOF flag
                IF nToSkip < 0 .AND. SELF:RowNumber < SELF:RowCount
                    SELF:_SetEOF(FALSE)
                ELSEIF nToSkip > 0
                    SELF:_SetBOF(FALSE)
                ENDIF
            ENDIF
        ENDIF
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine(" Skip {0}, from row {1}, recno {2} to row {3}, recno {4}, EOF {5}, BOF {6}", nToSkip, oldRow, oldRecNo, SELF:RowNumber, SELF:RecNo, SELF:EoF, SELF:BoF)
        #endif
        RETURN result
    end method

    /// <summary>Is the current row deleted?</summary>
    /// <remarks>
    /// When a DeletedColumn is defined, then his will return the value of that column.
    /// Otherwise the state of the current row is returned.
    /// </remarks>
    override property Deleted		as logic
        get
            if self:CurrentRow == null
                return false
            endif
            return self:_IsRowDeleted(self:CurrentRow)
        end get
    end property

    /// <inheritdoc />
    /// <remarks>This method will delete all rows from the table and then close the cursor</remarks>
    override method Zap() as logic
        if self:_ReadOnly
            self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:Zap", "Table is not Updatable" )
            return false
        endif
        self:GoCold()
        if self:_tableMode == TableMode.Table
            var stmt := self:_builder:ZapStatement()
            _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, stmt)
            _command:ExecuteNonQuery()
        endif
        self:_CloseCursor()
        return true
    end method

    /// <inheritdoc />
    /// <remarks>
    /// This method will delete all rows from the table where the deleted column has the value true / 1 and then close the cursor. <br/>
    /// When the there is no deleted column then the rows are already deleted from the server,
    /// and then they will also be deleted from the local cursor.
    /// </remarks>
    override method Pack() as logic
        if self:_ReadOnly
            self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:Pack", "Table is not Updatable" )
            return false
        endif
        self:GoCold()
        if self:_tableMode == TableMode.Table .and. _oTd:HasDeletedColumn
            var stmt := self:_builder:PackStatement()
            _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, stmt)
            _command:ExecuteNonQuery()
        endif
        self:_CloseCursor()
        return super:Pack()
    end method

    OVERRIDE METHOD SetFilter(info AS DbFilterInfo) AS LOGIC
        var cbFilter := info:FilterBlock
        if cbFilter != NULL
            try
                SELF:EvalFilter(cbFilter)
            catch ex as Exception
                SELF:_dbfError(ex, Subcodes.EDB_SETFILTER,Gencode.EG_ARG,"SQLRDD.SetFilter",FALSE)
                return false
            end try
        endif
        RETURN SUPER:SetFilter(info)
    end method

    public override method Flush() as logic
        RETURN SELF:GoCold()
    end method

    #region Lock / Unlock

    public override method Lock(lockInfo ref DbLockInfo) as logic
        // TODO thomas: implement Multiple Lock
        if Connection?:Provider is null .or. !self:Connection:IsOpen
            lockInfo:Result := false
            return false
        endif
        var sb := StringBuilder{}
        var messageLocked := StringBuilder{}

        try
            var otherLock := false
            var myLock := false
            self:CheckLock(lockInfo, messageLocked, ref myLock, ref otherLock)

            if otherLock
                lockInfo:Result := false
                // TODO: thomas add message to output
                return false
            endif

            if myLock
                lockInfo:Result := true
                return true
            endif

            // Write Lock
            sb:Clear()
            sb:Append(self:Connection:Provider:InsertStatement)
            sb:Replace(SqlDbProvider.TableNameMacro, SqlDbConnection.LockTableName)
            sb:Replace(SqlDbProvider.ColumnsMacro, self:Connection:XsLockColumnList())
            sb:Replace(SqlDbProvider.ValuesMacro, self:Provider:ParameterPrefix+"p1, "+self:Provider:ParameterPrefix+"p2, "+ ;
                self:Provider:ParameterPrefix+"p3, "+self:Provider:ParameterPrefix+"p4, "+self:Provider:ParameterPrefix+"p5, "+ ;
                self:Provider:CurrentDateTime + ", "+self:Provider:ParameterPrefix+"p6, "+self:Provider:ParameterPrefix+"p7")

            using var cmdInsertLock := SqlDbCommand{"InsertLock", self:Connection, false}
            cmdInsertLock:CommandText := sb:ToString()
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p1", Environment.MachineName ?? String.Empty)
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p2", Environment.UserName ?? String.Empty)
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p3", self:Connection:ConnectionId:ToString())
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p4", (int)super:Area)
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p5", System.Threading.Thread.CurrentThread.ManagedThreadId)
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p6", _oTd:RealName ?? String.Empty)
            cmdInsertLock:AddParameter(self:Provider:ParameterPrefix+"p7", SELF:LockRecNo(lockInfo))

            if !cmdInsertLock:ExecuteNonQuery()
                messageLocked:AppendLine("Could not create lock")
                // TODO: thomas add message to output
                lockInfo:Result := false
                return false
            endif
        catch
            messageLocked:AppendLine("An error occured while locking")
            // TODO: thomas add message to output
            lockInfo:Result := false
            return false
        end try

        lockInfo:Result := true
        return true
    end method

    public override method UnLock(oRecId as object) as logic
        if Connection?:Provider is null .or. !self:Connection:IsOpen
            return false
        endif

        try
            var sb := StringBuilder{}
            var sbWhere := StringBuilder{}

            sbWhere:AppendLine(" station = "+self:Provider:ParameterPrefix+"p1")
            sbWhere:AppendLine(" and username = "+self:Provider:ParameterPrefix+"p2")
            sbWhere:AppendLine(" and connectionid = "+self:Provider:ParameterPrefix+"p3")
            sbWhere:AppendLine(" and tablename = "+self:Provider:ParameterPrefix+"p4")
            sbWhere:AppendLine(" and workarea = "+self:Provider:ParameterPrefix+"p5")
            if oRecId != null .and. oRecId is int .and. ((int)oRecId) > 0
                sbWhere:AppendLine(" and recno = "+self:Provider:ParameterPrefix+"p6")
            endif

            sb:Append(self:Provider:DeleteStatement)
            sb:Replace(SqlDbProvider.TableNameMacro, SqlDbConnection.LockTableName)
            sb:Replace(SqlDbProvider.WhereMacro, sbWhere:ToString())

            using var cmd := SqlDbCommand{"ConnectionCleanupLock", self:Connection, false}
            cmd:CommandText := sb:ToString()
            cmd:AddParameter(self:Provider:ParameterPrefix+"p1", Environment.MachineName ?? String.Empty)
            cmd:AddParameter(self:Provider:ParameterPrefix+"p2", Environment.UserName ?? String.Empty)
            cmd:AddParameter(self:Provider:ParameterPrefix+"p3", self:Connection:ConnectionId:ToString())
            cmd:AddParameter(self:Provider:ParameterPrefix+"p4", _oTd?:RealName ?? super:Alias ?? String.Empty)
            cmd:AddParameter(self:Provider:ParameterPrefix+"p5", (int)super:Area)
            if oRecId != null .and. oRecId is int .and. ((int)oRecId) > 0
                cmd:AddParameter(self:Provider:ParameterPrefix+"p6", (int)oRecId)
            endif
            cmd:ExecuteNonQuery()
        catch
            return false
        end try

        // In ADS this happens in here: ACE.AdsUnlockRecord(SELF:_Table, dwRecno)
        if self:Deleted
            self:_ExecuteDeleteStatement(CurrentRow)
        endif
        if self:_updatedRecNos.Count > 0
            foreach var nRecNo in self:_updatedRecNos
                self:_UpdateRow(nRecNo)
            next
            self:_updatedRecNos:Clear()
        endif

        return true
    end method

    #endregion

end class

end namespace // XSharp.RDD.SqlRDD
