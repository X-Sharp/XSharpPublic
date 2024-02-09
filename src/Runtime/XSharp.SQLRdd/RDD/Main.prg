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

/// <summary>
/// The SqlRDD class.
/// </summary>
[DebuggerDisplay("SQLRDD ({Alias,nq})")];
partial class SQLRDD inherit DBFVFP

// Overridden properties and methods, these should all be documented

#region Overridden properties
    override property Driver as string get "SQLRDD"
#endregion

    /// <inheritdoc />
    override method Create(info as DbOpenInfo) as logic
        _cTable := System.IO.Path.GetFileName(info:FileName)
        if ! self:_PrepareOpen(info)
            return false
        endif
        self:_tableMode := TableMode.Table
        self:_TempFileName(info)
        self:_creating := true
        var fields := self:_Fields
        var lResult := super:Create(info)
        self:_creating := false
        self:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        // create SQL table Now
        self:_CreateSqlFields(fields)

        return lResult
    end method

    /// <inheritdoc />
    override method Open(info as DbOpenInfo) as logic
        if ! self:_PrepareOpen(info)
            return false
        endif
        var cQuery := info:FileName
        var tempFile   := self:_TempFileName(info)
        self:_FileName := tempFile
        // Determine if this is a single table name or a query (select or Execute)
        var selectStmt := XSharp.SQLHelpers.ReturnsRows(cQuery)
        if (selectStmt)
            self:_tableMode     := TableMode.Query
            var longFieldNames  := _connection:MetadataProvider:LongFieldNames
            self:_oTd           := _connection:GetStructureForQuery(cQuery,"QUERY",longFieldNames)
            _command:CommandText := cQuery
        else
            self:_tableMode := TableMode.Table
            if ! self:_GetTableInfo(cQuery)
                throw Exception{}
            endif
        endif
        local aUpdatableColumns as string[]
        var strUpdatableColumns  := _oTd:UpdatableColumns
        if String.IsNullOrEmpty(strUpdatableColumns) .or. strUpdatableColumns == "*"
            aUpdatableColumns := null
        else
            aUpdatableColumns    := strUpdatableColumns:ToLower():Split(',')
        endif
        local aKeyColumns as string[]
        var strKeyColumns        := _oTd:KeyColumns
        if String.IsNullOrEmpty(strKeyColumns) .or. strKeyColumns == "*"
            aKeyColumns := null
        else
            aKeyColumns    := strKeyColumns:ToLower():Split(',')
        endif

        // Get the structure
        var oFields := List<RddFieldInfo>{}
        self:_updatableColumns := List<RddFieldInfo>{}
        self:_keyColumns       := List<RddFieldInfo>{}
        foreach var oCol in _oTd:Columns
            var oField := oCol:ColumnInfo
            if oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Recno)
                nop
            elseif oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Deleted)
                self:_deletedColumn := oField:Ordinal+1
            endif
            oFields:Add(oField)
            if aKeyColumns == null
                if self:_oTd:CompareMemo
                    self:_keyColumns:Add(oField)
                elseif !oField:FieldType:IsLong()
                    self:_keyColumns:Add(oField)
                endif
            elseif System.Array.IndexOf(aKeyColumns,oField:ColumnName:ToLower()) != -1
                self:_keyColumns:Add(oField)
            endif
            IF !oField:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                if aUpdatableColumns  == null
                    self:_updatableColumns:Add(oField)
                elseif System.Array.IndexOf(aUpdatableColumns, oField:ColumnName:ToLower()) != -1
                    self:_updatableColumns:Add(oField)
                endif
            ENDIF
        next
        var aFields := oFields:ToArray()
        CoreDb.Create(tempFile,aFields,typeof(DBFVFP),true,"SQLRDD-TEMP","",false,false)
        File.SetAttributes(tempFile, _OR(File.GetAttributes(tempFile), FileAttributes.Temporary))

        info:ReadOnly := false
        self:_realOpen := false
        super:Open(info)
        self:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        // Associate the extra properties
        for var nI := 1 to aFields:Length
            var aField := aFields[nI-1]
            self:FieldInfo(nI, DBS_COLUMNINFO, aField)
        next
        // Add long field names
        for var nI := 1 to aFields:Length
            var aField := aFields[nI-1]
            if !_fieldNames:ContainsKey(aField:ColumnName)
                _fieldNames:Add(aField:ColumnName, nI)
            endif
        next
        if self:_tableMode == TableMode.Table
            cQuery := self:_oTd:EmptySelectStatement
            self:_hasData := false
        else
            self:DataTable      := _command:GetDataTable(self:Alias)
        endif
        _command:CommandText := cQuery
        self:_realOpen := true
        return true
    end method

#ifdef DEBUG
    /// <inheritdoc />
    override method SetFieldExtent(nFields as long) as logic
        var result := super:SetFieldExtent(nFields)
        return result
    end method

#endif



    /// <inheritdoc />
    override method Append(lReleaseLock as logic) as logic
        self:_ForceOpen()
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
            _updatedRows:Add(row)
            if _emptyValues == null
                self:_GetEmptyValues()
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
    end method

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
    end method


    /// <inheritdoc />
    override method PutValue(nFldPos as int, oValue as object) as logic
        // nFldPos is 1 based, the RDD compiles with /az+
        if self:_ReadOnly
            self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:PutValue", "Table is not Updatable" )
            return false
        endif
        if self:EoF
            return false
        endif
        var result := false
        if nFldPos > 0 .and. nFldPos <= self:FieldCount
            var col := self:_GetColumn(nFldPos)
            if SELF:_updatableColumns:Contains(col)
                var row := self:DataTable:Rows[self:_RecNo -1]
                if !_updatedRows:Contains(row)
                    _updatedRows:Add(row)
                endif
                row[nFldPos-1] := oValue
                result := true
                self:_Hot := true
            else
                self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:PutValue", i"Column {col.ColumnName} is not Updatable"  )
            endif
        endif
        return result

    /// <inheritdoc />
    override method GoCold() as logic
        local lWasHot := self:_Hot as logic
        local lOk := super:GoCold() as logic
        if lWasHot .and. self:DataTable != null .and. self:DataTable:Rows:Count >=self:_RecNo
            foreach var row in _updatedRows
                try
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
                catch e as Exception
                    lOk := false
                    self:_dbfError(ERDD.WRITE, XSharp.Gencode.EG_WRITE, "SqlRDD:PutValue", e:Message )
                end try
                if !lOk
                    exit
                endif
            next
            if lOk
                self:DataTable:AcceptChanges()
            else
                self:DataTable:RejectChanges()
            endif
            _updatedRows:Clear()
        endif
        return lOk
    end method


    /// <inheritdoc />
    override method Close() as logic
        local lOk as logic
        // This method deletes the temporary file after the file is closed
        foreach var bag in self:IndexList
            bag:Close()
        next
        _connection:RemoveRdd(self)
        lOk := super:Close()
        if File.Exists(SELF:FileName)
            File.Delete(SELF:FileName)
        endif
        return lOk
    end method

    /// <inheritdoc />
    override method Delete() as logic
        if self:_deletedColumn >= 0
            return self:PutValue(self:_deletedColumn, 1)
        else
            return super:Delete()
        endif
    end method

    /// <inheritdoc />
    override method Recall() as logic
        if self:_deletedColumn >= 0
            return self:PutValue(self:_deletedColumn, 0)
        else
            return super:Recall()
        endif
    end method

    /// <inheritdoc />
    override method Info(uiOrdinal as long, oNewValue as object) as object
        if uiOrdinal == DbInfo.DBI_CANPUTREC
            return false
        elseif uiOrdinal == DbInfo.DBI_ISDBF
            return false
        endif
        return super:Info(uiOrdinal, oNewValue)
    end method


    /// <inheritdoc />
    override method GoTop() as logic
        if !self:_ForceOpen()
            return false
        endif
        return super:GoTop()
    end method

    /// <inheritdoc />
    override method GoBottom() as logic
        if !self:_ForceOpen()
            return false
        endif
        return super:GoBottom()
    end method

    /// <inheritdoc />
    override method SkipRaw(move as long) as logic
        if !self:_ForceOpen()
            return false
        endif
        return super:SkipRaw(move)
    end method

    /// <inheritdoc />
    override method GoTo(nRec as long) as logic
        if !self:_ForceOpen()
            return false
        endif
        self:_RecNo := nRec
        return super:GoTo(nRec)
    end method

    /// <inheritdoc />
    override property RecNo		as int
        get
            self:ForceRel()
            return super:RecNo
        end get
    end property

    /// <inheritdoc />
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
        self:_OpenTable(cSeekExpr)
        return true
    end method

    /// <inheritdoc />
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
