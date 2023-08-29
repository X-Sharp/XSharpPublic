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
    protect _incrementKey   as long
    protect _incrementColumn as DataColumn
    protect _tableMode      as TableMode
    protect _hasData        as logic
    protect _connection     as SqlDbConnection
    protect _oTd            as SqlDbTableDef
    protect _oTableInfo     as SqlTableInfo
    protect _command        as SqlDbCommand

#region Overridden properties
    override property Driver as string get "SQLRDD"
#endregion
    constructor()
        super()
        _incrementKey    := -1
        _creatingIndex   := false
        _tableMode       := TableMode.Query
        _ReadOnly        := true
        _connection      := null
        _oTableInfo      := null
        return

    private method TempFileName() as string
        local result as string
        repeat
            var folder := Path.GetTempPath()
            var nId  := SqlDbHandles.GetId(0xFFFFF)
            var name := i"SQL"+nId:ToInt32():ToString("X5")
            result := Path.Combine(folder, name+".DBF")
        until ! File.Exists(result)
        return result

    private method GetTableInfo(cTable as string) as logic
        // First check to see if there is a tableDef for this table in the connection
        local oTd as SqlDbTableDef

        oTd := self:_connection:GetStructure(cTable)
        if oTd != null
            self:_oTd := oTd
            return true
        endif
        return false

    override method Open(info as DbOpenInfo) as logic
        var query := info:FileName
        local strConnection as string
        local pos as int
        strConnection := SqlDbConnection.DefaultConnection
        _connection := SqlDbGetConnection(strConnection)
        if _connection == null
            // Exception
            nop
        endif
        _connection:AddRdd(self)
        _command    := SqlDbCommand{info:Alias, _connection}
        pos := query:IndexOf(SqlDbProvider.ConnectionDelimiter)
        if pos > 0
            strConnection := query:Substring(0, pos)
            query := query:Substring(pos+2)
            info:FileName := query
        endif

        // Determine if this is a single table name or a query (select or Execute)
        var selectStmt := XSharp.SQLHelpers.ReturnsRows(query)
        if (selectStmt)
            self:_tableMode := TableMode.Query
            self:_oTd := _connection:GetStructureForQuery(query,"QUERY")
        else
            self:_tableMode := TableMode.Table
            if ! self:GetTableInfo(query)
                  throw Exception{}
            endif


        endif

        // Get the structure
        var oFields := List<RddFieldInfo>{}
        foreach var oCol in _oTd:Columns
            oFields:Add(oCol:ColumnInfo)
        next
        var aFields := oFields:ToArray()
        var tempFile   := self:TempFileName()
        CoreDb.Create(tempFile,aFields,typeof(DBFVFP),true,"SQLRDD-TEMP","",false,false)
        info:FileName := Path.Combine(Path.GetDirectoryName(tempFile), Path.GetFileNameWithoutExtension(tempFile))
        info:Extension := ".DBF"
        info:ReadOnly := false
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
        self:DataTable   := _command:GetDataTable(info:Alias)
        return true



    /// <inheritdoc />
    override method SetFieldExtent(nFields as long) as logic
        var result := super:SetFieldExtent(nFields)
        return result

    /// <inheritdoc />
    override method Create(info as DbOpenInfo) as logic
        var lResult := super:Create(info)
        self:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        return lResult

    /// <inheritdoc />
    override method Append(lReleaseLock as logic) as logic
        var lResult := super:Append(lReleaseLock)
        if lResult
            var row := _table:NewRow()
            if _incrementColumn != null
                row[_incrementColumn] := _incrementKey
                _incrementKey -= 1
            endif
            if row is IDbRow var dbRow
                dbRow:RecNo := super:RecNo
            endif
            _table:Rows:Add(row)
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
                var row := _table:Rows[self:_RecNo -1]
                result  := row[nFldPos]
            else
                result := _phantomRow[nFldPos]
            endif
            if result is string var strValue .and. ! _creatingIndex
                if self:_connection:TrimTrailingSpaces
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
            var row := _table:Rows[self:_RecNo -1]
            row[nFldPos-1] := oValue
            result := true
        endif
        return result

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
                    _incrementColumn := oColumn
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
        local cFileName := self:_FileName as string
        local cMemoName := "" as string
        _connection:RemoveRdd(self)
        if self:_Memo is AbstractMemo var memo
            cMemoName := memo:FileName
        endif
        lOk := super:Close()
        if lOk
            if File(cFileName)
                FErase(FPathName())
            endif
            if ! String.IsNullOrEmpty(cMemoName) .and. File(cMemoName)
                FErase(FPathName())
            endif
        endif
        return lOk

    /// <inheritdoc />
    override method Info(uiOrdinal as long, oNewValue as object) as object
        if uiOrdinal == DbInfo.DBI_CANPUTREC
            return false
        endif
        return super:Info(uiOrdinal, oNewValue)

    /// <inheritdoc />
    override method OrderCreate(orderInfo as DbOrderCreateInfo ) as logic
        self:_creatingIndex := true
        var result := super:OrderCreate(orderInfo)
        self:_creatingIndex := false
        return result

    /// <inheritdoc />
    override method OrderListRebuild() as logic
        self:_creatingIndex := true
        var result := super:OrderListRebuild()
        self:_creatingIndex := false
        return result

    method PrepareToMove() as void
        if self:_tableMode != TableMode.Table
            return
        endif
        if self:_hasData
            return
        endif
        var query := self:BuildSqlStatement()
        _command:CommandText := query
        self:_hasData    := true
        self:DataTable   := _command:GetDataTable(self:Alias)

    method BuildSqlStatement() as string
        var query := self:_oTd:SelectStatement
        // create filter from seek
        // add server side filter
        return query

    override method GoTop() as logic
        if ! self:_hasData
            self:PrepareToMove()
            return true
        else
            return super:GoTop()
        endif
    override method GoBottom() as logic
        if ! self:_hasData
            self:PrepareToMove()
        endif
        return super:GoBottom()
    override method SkipRaw(move as long) as logic
        if ! self:_hasData
            self:PrepareToMove()
        endif
        return super:SkipRaw(move)



end class

end namespace // XSharp.RDD.SqlRDD
