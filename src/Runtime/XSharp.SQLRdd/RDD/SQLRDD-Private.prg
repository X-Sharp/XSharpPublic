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
using XSharp.RDD.SqlRDD.Providers


begin namespace XSharp.RDD.SqlRDD

// Private methods and fields
partial class SQLRDD inherit DBFVFP

    private _table          as DataTable
    private _phantomRow     as DataRow
    private _creatingIndex  as logic
    private _identityKey    as long
    private _identityColumn as DataColumn
    private _deletedColumn  as long
    private _tableMode      as TableMode
    private _hasData        as logic
    private _realOpen       as logic
    private _connection     as SqlDbConnection
    private _oTd            as SqlDbTableInfo
    private _obuilder       as SqlDbTableCommandBuilder
    private _command        as SqlDbCommand
    private _trimValues     as logic
    private _creating       as logic
    private _cTable         as string
    private _emptyValues    as object[]
    private _updatableColumns as List<RddFieldInfo>
    private _keyColumns     as List<RddFieldInfo>
    private _updatedRows    as List<DataRow>
    private _orderBagList   as List<SqlDbOrderBag>
    private _recnoColumn    as long
    private _recordKeyCache as Dictionary<long, long>
    private _relativeRecNo as logic

#region Properties
    internal property Connection     as SqlDbConnection get _connection
    internal property Provider       as ISqlDbProvider get _connection:Provider
    internal property Command        as SqlDbCommand get _command
    internal property OrderBagList   as List<SqlDbOrderBag> get _orderBagList
    internal property DataTable as DataTable
        get
            return _table
        end get
        set
            // When we get here then the (temporary) DBFVFP table has already been created and opened
            // and the fields are already read from the DBF header in the temporary table
            // The SqlStatement:CreateFile() method whichs gets called from SqlExec()
            // has the logic that creates the DBF from the Column properties
            //
            if value == null
                _table:Rows:Clear()
                return
            endif
            _table := value
            self:_RecNo := 1
            self:_RecCount   := _table:Rows:Count
            self:_phantomRow := _table:NewRow()
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
                if oColumn:AutoIncrement
                    _identityColumn := oColumn
                    oColumn:ReadOnly := false
                endif
                if !oColumn:AllowDBNull
                    oColumn:AllowDBNull := true
                endif
                dbColumn:Flags := DBFFieldFlags.None
            next
            if self:_recnoColumn > 0
                _recordKeyCache := Dictionary<long, long>{_RecCount}
                // save the record numbers
                var rowNum := 0
                foreach row as DataRow in _table:Rows
                    var recno  := (LONG) row[self:_recnoColumn-1]
                    _recordKeyCache[recno] := rowNum
                    rowNum++
                next
            else
                _recordKeyCache := null
            endif
            self:Header:RecCount := _RecCount
            // set file length
            local lOffset   := self:_HeaderLength + self:_RecCount * self:_RecordLength as int64
            // Note FoxPro does not write EOF character for files with 0 records
            _oStream:SafeSetPos(lOffset)
            _oStream:SafeWriteByte(26)
            _oStream:SafeSetLength(lOffset+1)
            // now set the file size and reccount in the header
            self:GoTop()
        end set
    end property

#endregion

   constructor()
        super()
        _identityKey    := -1
        _creatingIndex   := false
        _tableMode       := TableMode.Query
        _ReadOnly        := true
        _connection      := null
        _obuilder        := null
        _deletedColumn   := -1
        _recnoColumn     := -1
        self:_trimValues := true // trim String Valuess
        self:DeleteOnClose := TRUE
        _updatedRows     := List<DataRow>{}
        _keyColumns      := List<RddFieldInfo>{}
        _updatableColumns:= List<RddFieldInfo>{}
        _orderBagList    := List<SqlDbOrderBag>{}
        return
    end constructor

    destructor()
        Command:Close()
    end destructor

#region Hacks
    // The field and property below allow us to access internal fields from the XSharp.RDD assembly
    private _deleteOnClose as PropertyInfo
    private method _GetDeleteOnClose() as void
        if _deleteOnClose == null
            var type := typeof(DBFVFP)
            var prop := type:GetProperty("DeleteOnClose", BindingFlags.Instance + BindingFlags.NonPublic)
            _deleteOnClose := prop
        endif
        return
    end method

    /// <exclude />
    property DeleteOnClose as logic
        get
            SELF:_GetDeleteOnClose()
            return (logic) _deleteOnClose:GetValue(self)
        end get
        set
            SELF:_GetDeleteOnClose()
            _deleteOnClose:SetValue(self, value)
        end set
    end property
#endregion

    private method _TempFileName(info as DbOpenInfo) as string
        local result as string
        repeat
            var folder := Path.GetTempPath()
            var nId  := SqlDbHandles.GetId(0xFFFFF)
            var name := i"SQL"+nId:ToInt32():ToString("X5")
            result := Path.Combine(folder, name+".DBF")
        until ! File.Exists(result)
        info:FileName := Path.Combine(Path.GetDirectoryName(result), Path.GetFileNameWithoutExtension(result))
        info:Extension := ".DBF"
        return result
    end method

    private method _GetTableInfo(cTable as string) as logic
        // First check to see if there is a tableDef for this table in the connection
        self:_obuilder  := SqlDbTableCommandBuilder{cTable, self}
        self:_cTable    := cTable
        var info        := _obuilder:FetchInfo(self)
        self:_oTd       := info
        self:_trimValues:= info:TrimTrailingSpaces
        if XSharp.RuntimeState.AutoOpen
            _obuilder:SetProductionIndex()
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
        _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
        _command.ExecuteScalar()
        sb:Clear()
        sb:Append(Provider:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider.QuoteIdentifier(_cTable))
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, columns)
        _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
        _command.ExecuteScalar()
        return true
    end method


    private method _GetEmptyValues() as logic
        var values := List<object>{}
        foreach col as DataColumn in self:DataTable:Columns
            if col:AutoIncrement
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
                values.Add(DateTime.MinValue)
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
            if c:AutoIncrement
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
        _command:BindParameters()
        var sb := StringBuilder{}
        sb:Append(Provider:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, Provider:QuoteIdentifier(self:_cTable))
        sb:Replace(SqlDbProvider.ColumnsMacro, sbColumns:ToString())
        sb:Replace(SqlDbProvider.ValuesMacro, sbValues:ToString())
        var hasGetIdentity := false
        if self:_identityColumn != null .and. !String.IsNullOrEmpty(Provider:GetIdentity)
            sb:Append("; ")
            sb:Append(Provider:GetIdentity)
            hasGetIdentity := true
        endif
        _command.CommandText := Connection:RaiseStringEvent(_command, SqlRDDEventReason.CommandText, _cTable, sb:ToString())
        var res := _command:ExecuteScalar()
        if hasGetIdentity
            row[_identityColumn] := res
        endif
        return true
    end method

    private method _GetWhereClause(row as DataRow) as string
        var sbWhere    := StringBuilder{}
        local iCounter := 1 as long
        foreach var c in self:_keyColumns
            var oldname := i"@o{iCounter}"
            if sbWhere:Length > 0
                sbWhere:Append(SqlDbProvider.AndClause)
            endif
            sbWhere:Append(Provider:QuoteIdentifier(c:ColumnName))
            var colValue := row[c:ColumnName,DataRowVersion.Original]
            if colValue == DBNull.Value
                sbWhere:Append(" is null")
            else
                sbWhere:Append(" = ")
                sbWhere:Append(oldname)
                _command:AddParameter(oldname, colValue)
            endif
            ++iCounter
        next
        return sbWhere:ToString()
    end method

    private method _ExecuteUpdateStatement(row as DataRow) as logic
        var sbColumns  := StringBuilder{}
        local iCounter := 1 as long
        _command:ClearParameters()
        foreach c as DataColumn in DataTable:Columns
            if c:AutoIncrement
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
        var strWhere := _GetWhereClause(row)
        _command:BindParameters()
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
            return res is int var i .and. i == 1
        endif
        return true
    end method

    private method _HandleNullDate(oValue as object) as object
        if oValue is DateTime var dt .and. dt == DateTime.MinValue
            return DBNull.Value
        elseif oValue is IDate var d
            if d:IsEmpty
                return DBNull.Value
            else
                return DateTime{d:Year, d:Month, d:Day}
            endif
        endif
        return oValue
    end method

    private method _ExecuteDeleteStatement(row as DataRow) as logic
        _command:ClearParameters()
        var strWhere := _GetWhereClause(row)
        _command:BindParameters()
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

    private method _OpenTable(sWhereClause as string) as logic
        try
            _command:CommandText := self:_BuildSqlStatement(sWhereClause)
            _command:ClearParameters()
            self:_hasData    := true
            self:DataTable   := _command:GetDataTable(self:Alias)
        catch as Exception
            return false
        end try
        return true
    end method


    private method _BuildSqlStatement(sWhereClause as string) as string
        local query as string
        if ! self:_realOpen
            query := self:_oTd:EmptySelectStatement
        else
            if self:_tableMode == TableMode.Table
                query := _obuilder:BuildSqlStatement(sWhereClause)
            else
                query := self:_oTd:SelectStatement
            endif
        endif
        return query
    end method


end class

end namespace // XSharp.RDD.SqlRDD
