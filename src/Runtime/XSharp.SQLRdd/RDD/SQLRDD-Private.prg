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
    private _updatedRows    as List<DataRow>
    private _orderBagList   as List<SqlDbOrderBag>
    private _rowNumber         as long

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

#region Properties
    internal property Connection     as SqlDbConnection get _connection
    internal property Provider       as ISqlDbProvider get _connection:Provider
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
        SELF:_updatedRows      := List<DataRow>{}
        SELF:_keyColumns       := List<RddFieldInfo>{}
        SELF:_updatableColumns := List<RddFieldInfo>{}
        SELF:_orderBagList     := List<SqlDbOrderBag>{}
        SELF:_IsFileBased      := FALSE
        return
    end constructor

    destructor()
        Command:Close()
    end destructor

    internal method _ClearTable() AS VOID
        SELF:DataTable:Rows:Clear()
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
        if ! lColumnChanged
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
            SELF:DataTable := self:_ReadTable(sWhereClause)
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
        return

    private method _GetRecCount() as void
        self:_serverReccount := self:_builder:GetRecCount()
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
                    SELF:DataTable:Rows:Add(row:ItemArray)
                next
            else
                SELF:RowNumber := newTable:Rows:Count

                for var nRow := newTable:Rows:Count-1 downto 0
                    var row := newTable:Rows[nRow]
                    var newRow := SELF:DataTable:NewRow()
                    newRow:ItemArray := row:ItemArray
                    SELF:DataTable:Rows:InsertAt(newRow, 0)
                next
            endif
            if lForward .and. newTable:Rows:Count < _oTd:PageSize
                SELF:_hasEOF := true
            else
                _currentPageNo := nNewPageNo
            endif
        endif
        return result

    PRIVATE METHOD _GotoRecord(nRec as DWORD) AS LOGIC

        if SELF:DataTable:Rows:Count < 1
            // Brute walk
            SELF:_command:CommandText := _builder:BuildRowNumberStatement(nRec)
            var result := SELF:_command:ExecuteScalar(SELF:_oTd:Name)
            var iResult := Convert.ToInt64(result)
            // shouldn't this be ToUInt32?

            // determine correct page
            SELF:_currentPageNo := (INT) ((iResult - 1) / SELF:_oTd:PageSize) + 1
            SELF:_ClearTable()
            SELF:DataTable := SELF:_ReadTable("")
        end if

        // locate the row in the page
        SELF:RowNumber := 1
        DO WHILE SELF:RowNumber <= SELF:DataTable:Rows:Count
            IF SELF:RecNo == nRec
                RETURN TRUE
            ENDIF
            SELF:RowNumber+= 1
        ENDDO
        RETURN FALSE

    PRIVATE METHOD _GotoRow(nRow as LONG) AS LOGIC
        SELF:_Found := FALSE
        var nCount := SELF:DataTable:Rows:Count
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

