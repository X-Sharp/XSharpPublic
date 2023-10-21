//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#define TEST
using XSharp.RDD.Enums
using XSharp.RDD.Support
using System.IO
using System.Collections.Generic
using System.Data
using System.Text
using System.Diagnostics
using System.Reflection
using System.Data.Common

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlRDD class.
/// </summary>
[DebuggerDisplay("SQLRDD ({Alias,nq})")];
class SQLRDD inherit DBFVFP
    protect _table          as DataTable
    protect _phantomRow     as DataRow
    protect _creatingIndex  as logic
    protect _identityKey   as long
    protect _identityColumn as DataColumn
        //protect _recnoColumn     as long
    protect _deletedColumn   as long
    protect _tableMode      as TableMode
    protect _rowNum         as long
    protect _maxRec         as long
    protect _hasData        as logic
    protect _realOpen       as logic
    protect _connection     as SqlDbConnection
    protect _oTd            as SqlDbTableDef
    protect _obuilder       as SqlDbTableCommandBuilder
    protect _command        as SqlDbCommand
    protect _currentOrder   as SqlDbOrder
    protect _trimValues     as logic
    private _oIni           as IniFile
    private _creating       as logic
    private _cTable         as string
    private _emptyValues    as object[]

    new internal property CurrentOrder   as SqlDbOrder get _currentOrder set _currentOrder := value
    internal property Connection         as SqlDbConnection get _connection
    internal property Provider           as SqlDbProvider get _connection:Provider
    internal property Command            as SqlDbCommand get _command
    internal property IniFile            as IniFile get _oIni
#region Overridden properties
    override property Driver as string get "SQLRDD"
#endregion
    constructor()
        super()
        _identityKey    := -1
        _creatingIndex   := false
        _tableMode       := TableMode.Query
        _ReadOnly        := true
        _connection      := null
        _obuilder        := null
        //_recnoColumn     := -1
        _deletedColumn   := -1
        _maxRec          := -1
        _oIni            := IniFile{"SQLRDD.INI"}
        self:_trimValues := true // trim String Valuess
        return
    destructor()
        Command:Close()

    private method TempFileName(info as DbOpenInfo) as string
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

    private method GetTableInfo(cTable as string) as logic
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

    private method __PrepareOpen(info as DbOpenInfo) as logic
        var query := info:FileName
        local strConnection as string
        local pos as int
        strConnection := SqlDbConnection.DefaultConnection
        _connection := SqlDbGetConnection(strConnection)
        if _connection == null
            return false
        endif
        _command    := SqlDbCommand{info:Alias, _connection}
        pos := query:IndexOf(SqlDbProvider.ConnectionDelimiter)
        if pos > 0
            strConnection := query:Substring(0, pos)
            var oNewConn := SqlDbGetConnection(strConnection)
            if oNewConn == null
                return false
            endif
            _connection := oNewConn
            query := query:Substring(pos+2)
            info:FileName := query
        endif
        return true
    override method Create(info as DbOpenInfo) as logic
        _cTable := System.IO.Path.GetFileName(info:FileName)
        if ! self:__PrepareOpen(info)
            return false
        endif
        self:_tableMode := TableMode.Table
        self:TempFileName(info)
        self:_creating := true
        var fields := self:_Fields
        var lResult := super:Create(info)
        self:_creating := false
        self:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        // create SQL table Now
        self:CreateSqlFields(fields)

        return lResult

    method CreateSqlFields(aFields as RddFieldInfo[]) as logic
        var sb := System.Text.StringBuilder{}
        var first := true
        foreach var fld in aFields
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(self:Provider:GetSqlColumnInfo(fld))
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

    override method Open(info as DbOpenInfo) as logic
        if ! self:__PrepareOpen(info)
            return false
        endif
        var query := info:FileName
        // Determine if this is a single table name or a query (select or Execute)
        var selectStmt := XSharp.SQLHelpers.ReturnsRows(query)
        if (selectStmt)
            self:_tableMode := TableMode.Query
            var longFieldNames := _connection:RaiseLogicEvent(_connection, SqlRDDEventReason.LongFieldNames,query,true)
            self:_oTd := _connection:GetStructureForQuery(query,"QUERY",longFieldNames)
        else
            self:_tableMode := TableMode.Table
            if ! self:GetTableInfo(query)
                throw Exception{}
            endif
        endif

        // Get the structure
        var oFields := List<RddFieldInfo>{}
        foreach var oCol in _oTd:Columns
            if oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Recno)
                //self:_recnoColumn := oCol:ColumnInfo:Ordinal+1
                nop
            elseif oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Deleted)
                self:_deletedColumn := oCol:ColumnInfo:Ordinal+1
            endif

            oFields:Add(oCol:ColumnInfo)
        next
        var aFields := oFields:ToArray()
        var tempFile   := self:TempFileName(info)
        CoreDb.Create(tempFile,aFields,typeof(DBFVFP),true,"SQLRDD-TEMP","",false,false)
        info:ReadOnly := false
        self:_realOpen := false
        super:Open(info)
        // Assoctiate the extra properties
        for var nI := 1 to aFields:Length
            var aField := aFields[nI-1]
            self:FieldInfo(nI, DBS_COLUMNINFO, aField)
        next
        if self:_tableMode == TableMode.Table
            query := self:_oTd:EmptySelectStatement
            self:_hasData := false
        endif
        _command:CommandText := query
        self:_realOpen := true
        return true

    /// <inheritdoc />
    override method SetFieldExtent(nFields as long) as logic
        var result := super:SetFieldExtent(nFields)
        return result


    /// <inheritdoc />

    private method GetEmptyValues() as logic
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
    override method Append(lReleaseLock as logic) as logic
        self:ForceOpen()
        var lResult := super:Append(lReleaseLock)
        if lResult
            var key := _identityKey
            var row := self:DataTable:NewRow()
            if _identityColumn != null
                _identityKey -= 1
            endif
            if row is IDbRow var dbRow
                dbRow:RecNo := super:RecNo
            endif
            _RecNo := super:RecNo
            self:DataTable:Rows:Add(row)
            if _emptyValues == null
                self:GetEmptyValues()
            endif
            var values := (object[])_emptyValues:Clone()
            foreach c as DataColumn in self:DataTable:Columns
                if c:AutoIncrement
                    row[c] := key
                else
                    row[c] := values[c:Ordinal]
                endif
            next
            self:_Hot := true
        endif
        return lResult
    override method FieldIndex(fieldName as string) as int
        local result as int
        // SUPER:FieldIndex uses a dictionary, so that is fast, If that fails then
        // check again for colum names.
        result := super:FieldIndex(fieldName)
        if result == 0
            foreach var oColumn in self:_Fields
                if oColumn != null .and. String.Compare(oColumn:ColumnName, fieldName, true) == 0
                    return oColumn:Ordinal
                endif
            next
        endif
        return result

    /// <inheritdoc />
    override method GetValue(nFldPos as int) as object
        // nFldPos is 1 based, the RDD compiles with /az+
        if nFldPos > 0 .and. nFldPos <= self:FieldCount
            nFldPos -= 1
            local result as object
            if !self:EoF
                var row := self:DataTable:Rows[self:_RecNo -1]
                result  := row[nFldPos]
            else
                result := _phantomRow[nFldPos]
            endif
            if result is string var strValue .and. ! _creatingIndex
                if self:_trimValues
                    result := strValue:TrimEnd()
                else
                    result := strValue:PadRight(_Fields[nFldPos]:Length,' ')
                endif
            endif

            if result == DBNull.Value
                // The phantom row already is padded with trailing spaces
                if ! self:_connection:UseNulls
                    result := _phantomRow[nFldPos]
                endif
            elseif _creatingIndex .and. result is string var strResult
                result := strResult:PadRight(_Fields[nFldPos]:Length,' ')
            endif

            return result
        endif
        return super:GetValue(nFldPos)

    /// <inheritdoc />
    override method PutValue(nFldPos as int, oValue as object) as logic
        // nFldPos is 1 based, the RDD compiles with /az+
        if self:_ReadOnly
            self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            return false
        endif
        if self:EoF
            return false
        endif
        var result := false
        if nFldPos > 0 .and. nFldPos <= self:FieldCount
            var row := self:DataTable:Rows[self:_RecNo -1]
            row[nFldPos-1] := oValue
            result := true
            self:_Hot := true
        endif
        return result

    override method GoCold() as logic
        local lWasHot := self:_Hot as logic
        local lOk := super:GoCold() as logic
        if lWasHot .and. self:DataTable != null .and. self:DataTable:Rows:Count >=self:_RecNo
            var row := self:DataTable:Rows[self:_RecNo-1]
            switch row:RowState
            case DataRowState.Added
                lOk := _ExecuteInsertStatement(row)
            case DataRowState.Deleted
                lOk := _ExecuteDeleteStatement(row)
            case DataRowState.Modified
                lOk := _ExecuteUpdateStatement(row)
            case DataRowState.Unchanged
            case DataRowState.Detached
                lOk := true
                if super:Deleted
                    lOk := _ExecuteDeleteStatement(row)
                endif
            end switch
            if lOk
                self:DataTable:AcceptChanges()
            endif
        endif
        return lOk

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

    private method _GetWhereClause(row as DataRow) as string
        var sbWhere    := StringBuilder{}
        local iCounter := 1 as long
        foreach c as DataColumn in DataTable:Columns
            var oldname := i"@o{iCounter}"
            if sbWhere:Length > 0
                sbWhere:Append(SqlDbProvider.AndClause)
            endif
            sbWhere:Append(Provider:QuoteIdentifier(c:ColumnName))
            var colValue := row[c,DataRowVersion.Original]
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

    private method _ExecuteUpdateStatement(row as DataRow) as logic
        var sbColumns  := StringBuilder{}
        local iCounter := 1 as long
        _command:ClearParameters()
        foreach c as DataColumn in DataTable:Columns
            if c:AutoIncrement
                loop
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
    /// <summary>
    /// This property returns the DataTable object that is used to cache the results locally
    /// </summary>
    /// <value></value>
    property DataTable as DataTable
        get
            return _table
        end get
        set
            // When we get here then the (temporary) DBFVFP table has already been created and opened
            // and the fields are already read from the DBF header in the temporary table
            // The SqlStatement:CreateFile() method whichs gets called from SqlExec()
            // has the logic that creates the DBF from the Column properties
            //
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

    /// <inheritdoc />
    override method Close() as logic
        local lOk as logic
        // This method deletes the temporary file after the file is closed
        _connection:RemoveRdd(self)
        lOk := super:Close()
#ifndef TEST
        local cMemoName := "" as string
        if self:_Memo is AbstractMemo var memo
            cMemoName := memo:FileName
        endif
        local cFileName := self:_FileName as string
        if lOk
            if File(cFileName)
                FErase(FPathName())
            endif
            if ! String.IsNullOrEmpty(cMemoName) .and. File(cMemoName)
                FErase(FPathName())
            endif
        endif
#endif
        return lOk

    override method Delete() as logic
        if self:_deletedColumn >= 0
            return self:PutValue(self:_deletedColumn, 1)
        else
            return super:Delete()
        endif

    override method Recall() as logic
        if self:_deletedColumn >= 0
            return self:PutValue(self:_deletedColumn, 0)
        else
            return super:Recall()
        endif

    /// <inheritdoc />
    override method Info(uiOrdinal as long, oNewValue as object) as object
        if uiOrdinal == DbInfo.DBI_CANPUTREC
            return false
        endif
        return super:Info(uiOrdinal, oNewValue)

    /// <inheritdoc />
    override method OrderCreate(orderInfo as DbOrderCreateInfo ) as logic
        var result := super:OrderCreate(orderInfo)
        return result

    /// <inheritdoc />
    override method OrderListRebuild() as logic
        var result := super:OrderListRebuild()
        return result

    override method OrderListFocus(orderInfo as DbOrderInfo) as logic
        if self:_tableMode != TableMode.Table
            return super:OrderListFocus(orderInfo)
        endif
        self:_hasData := false
        return self:_obuilder:OrderListFocus(orderInfo)



    method ForceOpen() as logic
        if self:_tableMode != TableMode.Table
            return true
        endif
        if self:_creating
            return true
        endif
        if self:_hasData
            return true
        endif
        return self:OpenTable("")

    method OpenTable(sWhereClause as string) as logic
        try
            var query := self:BuildSqlStatement(sWhereClause)
            _command:CommandText := query
            self:_hasData    := true
            self:DataTable   := _command:GetDataTable(self:Alias)
        catch e as Exception
            return false
        end try
        return true


    method BuildSqlStatement(sWhereClause as string) as string
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
        // create filter from seek
        // add server side filter
        query := self:_connection:RaiseStringEvent(_connection, SqlRDDEventReason.CommandText, _oTd:Name, query)
        return query

    override method GoTop() as logic
        if !self:ForceOpen()
            return false
        endif
        return super:GoTop()
    override method GoBottom() as logic
        if !self:ForceOpen()
            return false
        endif
        return super:GoBottom()
    override method SkipRaw(move as long) as logic
        if !self:ForceOpen()
            return false
        endif
        return super:SkipRaw(move)
    override method GoTo(nRec as long) as logic
        if !self:ForceOpen()
            return false
        endif
        //         if self:_recnoColumn != 0
        //             // find the row based on the value of _recnoColumn
        //             local result := -1 as long
        //             for var i := 0 to _table:Rows:Count -1
        //                 var row := _table:Rows[i]
        //                 var recno := (long) row[_recnoColumn-1]
        //                 if (recno == nRec)
        //                     result := i
        //                     exit
        //                 endif
        //             next
        //             nRec := result
        //             return super:GoTo(nRec)
        //         else
        self:_RecNo := nRec
        return super:GoTo(nRec)
        //         endif

    override property RecNo		as int
        get
            self:ForceRel()
            //             if self:_recnoColumn != 0
            //                 return (int) self:GetValue(self:_recnoColumn)
            //             else
            return super:RecNo
            //             endif
        end get
    end property

    override method Seek(seekInfo as DbSeekInfo) as logic
        local oKey as object
        oKey := seekInfo:Value
        if oKey == null         // Seek NIL
            if seekInfo:Last
                return self:GoBottom()
            else
                return self:GoTop()
            endif
        endif
        if self:CurrentOrder == null
            self:_dbfError(Subcodes.ERDD_DATATYPE, Gencode.EG_NOORDER )
            return false
        endif
        var cSeekExpr := CurrentOrder:SeekExpression(seekInfo )
        self:OpenTable(cSeekExpr)
        return true
    override method OrderInfo(nOrdinal as dword , info as DbOrderInfo ) as object
        local isOk := true as logic
        local oBag := null as SqlDbOrderBag
        var hasBagName := ! String.IsNullOrEmpty(info:BagName)
        if self:_tableMode != TableMode.Table
            return super:OrderInfo(nOrdinal, info)
        endif
        local workOrder as SqlDbOrder
        if !info:IsEmpty
            workOrder := self:_obuilder:FindOrder(info)
        else
            workOrder := self:CurrentOrder
        endif
        begin switch nOrdinal
        case DBOI_DEFBAGEXT
            info:Result := SqlDbOrderBag.BAG_EXTENSION
        case DBOI_CONDITION
            if workOrder != null
                info:Result := workOrder:Condition
            else
                info:Result := ""
            endif
        case DBOI_EXPRESSION
            if workOrder != null
                info:Result := workOrder:Expression
            else
                info:Result := ""
            endif
        case DBOI_ORDERCOUNT
            if oBag == null
                if hasBagName
                    info:Result := 0
                else
                    info:Result := self:_indexList:Count
                endif
            else
                info:Result := oBag:Tags:Count
            endif
            //         case DBOI_POSITION
            //         case DBOI_RECNO
            //             var oState := self:_GetState()
            //             if workOrder == null
            //                 info:Result := self:RecNo
            //             else
            //                 isOk := workOrder:_getRecPos( ref result)
            //                 if isOk
            //                     info:Result := result
            //                 endif
            //             endif
            //             self:_SetState(oState)
            //         case DBOI_KEYCOUNT
            //             result := 0
            //             var oState := self:_GetState()
            //             if workOrder != null
            //                 info:Result := 0
            //                 isOk := workOrder:_CountRecords(ref result)
            //             else
            //                 isOk := true
            //             endif
            //             if isOk
            //                 info:Result := result
            //             endif
            //             self:_SetState(oState)
            //         case DBOI_NUMBER
            //             info:Result := self:_indexList:OrderPos(workOrder)
        case DBOI_BAGEXT
            // according to the docs this should always return the default extension and not the actual extension
            if workOrder != null
                info:Result := System.IO.Path.GetExtension(workOrder:OrderBag:FullPath)
            else
                info:Result := SqlDbOrderBag.BAG_EXTENSION
            endif
        case DBOI_FULLPATH
            if workOrder != null
                info:Result := workOrder:OrderBag:FullPath
            else
                info:Result := String.Empty
            endif
        case DBOI_BAGCOUNT
            info:Result := self:_indexList:BagCount
        case DBOI_BAGNAME
            //CASE DBOI_INDEXNAME // alias
            if info:Order is long var nOrder
                info:Result := self:_indexList:BagName(nOrder)
            elseif workOrder != null
                info:Result := workOrder:FileName
            else
                info:Result :=String.Empty
            endif

        case DBOI_NAME
            if workOrder != null
                info:Result := workOrder:Name
            else
                info:Result := String.Empty
            endif
            //         case DBOI_COLLATION
            //             info:Result := ""
            //             if workOrder != null
            //                 local collation as VfpCollation
            //                 collation := workOrder:Collation
            //                 if collation  != null
            //                     info:Result := collation:Name
            //                 endif
            //             endif

            //         case DBOI_FILEHANDLE
            //             if workOrder != null
            //                 info:Result := workOrder:OrderBag:Handle
            //             else
            //                 info:Result := IntPtr.Zero
            //             endif
            //         case DBOI_FILESTREAM
            //             if workOrder != null
            //                 info:Result := workOrder:OrderBag:Stream
            //             else
            //                 info:Result := null
            //             endif
        case DBOI_ISDESC
            if workOrder != null
                var oldValue  := workOrder:Descending
                if info:Result is logic var descend
                    workOrder:Descending := descend
                endif
                info:Result := oldValue
            else
                info:Result := false
            endif
        case DBOI_ISCOND
            if workOrder != null
                info:Result := workOrder:Conditional
            else
                info:Result := false
            endif
            //         case DBOI_KEYTYPE
            //             if workOrder != null
            //                 info:Result := workOrder:KeyType
            //             else
            //                 info:Result := 0
            //             endif
        case DBOI_KEYSIZE
            if workOrder != null
                info:Result := workOrder:KeyLength
            else
                info:Result := 0
            endif
        case DBOI_KEYDEC
            info:Result := 0
        case DBOI_HPLOCKING
            info:Result := false
        case DBOI_UNIQUE
            if workOrder != null
                info:Result := workOrder:Unique
            else
                info:Result := false
            endif
            //         case DBOI_LOCKOFFSET
            //             if workOrder != null
            //                 info:Result := workOrder:OrderBag:_LockOffSet
            //             else
            //                 info:Result := 0
            //             endif
        case DBOI_SETCODEBLOCK
            if workOrder != null
                info:Result := workOrder:KeyCodeBlock
            endif
        case DBOI_KEYVAL
            if workOrder != null
                isOk := true
                try
                    info:Result := self:EvalBlock(workOrder:KeyCodeBlock)
                catch ex as Exception
                    isOk := false
                    self:_dbfError(ex, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX, "DBFCDX.OrderInfo")
                end try
                if !isOk
                    info:Result := DBNull.Value
                endif
            else
                info:Result := DBNull.Value
            endif
        case DBOI_SCOPETOPCLEAR
        case DBOI_SCOPEBOTTOMCLEAR
            if workOrder != null
                workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                self:_hasData := false
            endif
            info:Result := null
        case DBOI_SCOPETOP
        case DBOI_SCOPEBOTTOM
            if workOrder != null
                local oldValue as object
                if nOrdinal == DBOI_SCOPETOP
                    oldValue := workOrder:TopScope
                elseif nOrdinal == DBOI_SCOPEBOTTOM
                    oldValue := workOrder:BottomScope
                else
                    oldValue := DBNull.Value
                endif
                if info:Result != null
                    workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                endif
                info:Result := oldValue
                self:_hasData := false
            else
                info:Result := DBNull.Value
            endif
            //         case DBOI_KEYADD
            //             if workOrder != null
            //                 info:Result := workOrder:AddKey(self:RecNo)
            //             else
            //                 info:Result := false
            //             endif
            //         case DBOI_KEYDELETE
            //             if workOrder != null
            //                 info:Result := workOrder:DeleteKey(self:RecNo)
            //             else
            //                 info:Result := false
            //             endif
            //         case DBOI_CUSTOM
            //             if workOrder != null
            //                 local lOld as logic
            //                 lOld := workOrder:Custom
            //                 if info:Result is logic var custom
            //                     if custom
            //                         workOrder:SetCustom()
            //                     endif
            //                 endif
            //                 info:Result := lOld
            //             else
            //                 info:Result := false
            //             endif
            //
            //         case DBOI_USER + 42
            //         case DBOI_DUMP
            //             // Dump Cdx to Txt file
            //             var oState := self:_GetState()
            //             if workOrder != null
            //                 workOrder:_dump()
            //             endif
            //             self:_SetState(oState)
            //         case DBOI_VALIDATE
            //             // Validate integrity of the current Order
            //             var oState := self:_GetState()
            //             if workOrder != null
            //                 info:Result := workOrder:_validate()
            //             endif
            //             self:_SetState(oState)
            //         case DBOI_SKIPUNIQUE
            //             if workOrder != null
            //                 local nToSkip := 1 as long
            //                 if info:Result is long var nNum
            //                     nToSkip := nNum
            //                 endif
            //                 info:Result := workOrder:SkipUnique(nToSkip)
            //             endif
        otherwise
            super:OrderInfo(nOrdinal, info)
        end switch
        return info:Result

    override property Deleted		as logic
        get
            self:ForceRel()
            if self:_deletedColumn > 0
                return (logic) self:GetValue(self:_deletedColumn)
            else
                return super:Deleted
            endif
        end get
    end property


end class

end namespace // XSharp.RDD.SqlRDD
