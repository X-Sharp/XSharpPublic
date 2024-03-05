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

	/// <summary>Create a table.</summary>
	/// <param name="info">object describing the file to create.</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
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
        var cQuery := info:FileName
        if cQuery:IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0
            cQuery := System.IO.Path.GetFileNameWithoutExtension(cQuery)
        endif
        var tempFile   := self:_TempFileName(info)
        self:_FileName := tempFile
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
        endif
        local aUpdatableColumns as HashSet<string>
        var strUpdatableColumns  := _oTd:UpdatableColumns
        if String.IsNullOrEmpty(strUpdatableColumns) .or. strUpdatableColumns == "*"
            aUpdatableColumns := null
        else
            aUpdatableColumns    := HashSet<string>{strUpdatableColumns:Split(','), StringComparer.OrdinalIgnoreCase}
        endif
        local aKeyColumns as HashSet<string>
        var strKeyColumns        := _oTd:KeyColumns
        if String.IsNullOrEmpty(strKeyColumns) .or. strKeyColumns == "*"
            aKeyColumns := null
        else
            aKeyColumns    := HashSet<string>{strKeyColumns:Split(','), System.StringComparer.OrdinalIgnoreCase}
        endif

        // Get the structure
        var oFields := List<RddFieldInfo>{}
        self:_updatableColumns := List<RddFieldInfo>{}
        self:_keyColumns       := List<RddFieldInfo>{}
        foreach var oCol in _oTd:Columns
            var oField := oCol:ColumnInfo
            if oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Recno)
                self:_recnoColumn   := oField:Ordinal+1
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
                _fieldNames:Add(aField:ColumnName, nI-1)
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



	/// <summary>Append a blank row and position the cursor to the new row.</summary>
	/// <param name="lReleaseLock">A flag that is TRUE if you want to clear all pending row locks before appending the new row and FALSE if you want to add the new row to the end of the current lock list.</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
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
                    row[c] := SELF:_HandleNullDate(values[c:Ordinal])
                endif
            next
            self:_Hot := true
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
        // nFldPos is 1 based, the RDD compiles with /az+
        SELF:_ForceOpen()
        if nFldPos > 0 .and. nFldPos <= self:FieldCount
            nFldPos -= 1
            local result as object
            if self:DataTable:Rows:Count >= self:_RecNo .and. !self:EoF
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
            if result is DateTime var dtValue
                if self:_Fields[nFldPos]:FieldType == DbFieldType.Date
                    if dtValue == DateTime.MinValue
                        result := DbDate{0,0,0}
                    else
                        result := DbDate{dtValue:Year, dtValue:Month, dtValue:Day}
                    endif
                endif
            endif
            if result == DBNull.Value
                // The phantom row already is padded with trailing spaces
                if ! self:_connection:UseNulls
                    result := _phantomRow[nFldPos]
                endif
                if result is DateTime .and. self:_Fields[nFldPos]:FieldType == DbFieldType.Date
                    result := DbDate{0,0,0}
                endif
            elseif _creatingIndex .and. result is string var strResult
                result := strResult:PadRight(_Fields[nFldPos]:Length,' ')
            endif

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
        SELF:_ForceOpen()
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
                row[nFldPos-1] := SELF:_HandleNullDate(oValue)
                result := true
                self:_Hot := true
            else
                self:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "SqlRDD:PutValue", i"Column {col.ColumnName} is not Updatable"  )
            endif
        endif
        return result

    /// <summary>Write the contents of a work area's memory to the data store (usually a disk).</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method GoCold() as logic
        local lWasHot := self:_Hot as logic
        local lOk := super:GoCold() as logic
        if lWasHot .and. self:DataTable != null .and. self:DataTable:Rows:Count >=self:_RecNo
            foreach var row in _updatedRows
                try
                    lOk := true
                    if super:Deleted
                        if self:_deletedColumn > 0
                            lOk := _ExecuteUpdateStatement(row)
                            if lOk
                                row:AcceptChanges()
                            endif
                        else
                            lOk := _ExecuteDeleteStatement(row)
                            row:CancelEdit()  // keep the row in the collection, so the record numbers match
                        endif
                    else
                        if row:RowState.HasFlag(DataRowState.Added)
                            lOk := _ExecuteInsertStatement(row)
                        endif
                        if row:RowState.HasFlag(DataRowState.Modified)
                            lOk := _ExecuteUpdateStatement(row)
                        endif
                    endif
                catch e as Exception
                    lOk := false
                    self:_dbfError(ERDD.WRITE, XSharp.Gencode.EG_WRITE, "SqlRDD:GoCold", e:Message )
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


	/// <summary>Close a table.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method Close() as logic
        local lOk as logic
        // This method deletes the temporary file after the file is closed
        foreach var bag in self:OrderBagList
            bag:Close()
        next
        _connection:UnregisterRdd(self)
        lOk := super:Close()
        if File.Exists(SELF:FileName)
            File.Delete(SELF:FileName)
        endif
        return lOk
    end method

	/// <summary>Mark the row at the current cursor position for deletion.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	/// <remarks>
    /// When a DeletedColumn is defined, then his set the value of that column to TRUE
    /// Otherwise current row is deleted. The deletion at the server will be done when the record pointer is moved or the table is closed.
	/// </remarks>

    override method Delete() as logic
        if self:_deletedColumn >= 0
            self:PutValue(self:_deletedColumn, 1)
         endif
        return super:Delete()
    end method

	/// <summary>Remove the deletion marker from the row at the current cursor position.</summary>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
	/// <remarks>
    /// When a DeletedColumn is defined, then his set the value of that column to TRUE
    /// Otherwise when the current row is deleted and not persisted to the server yet, then the deletion is undone.
	/// </remarks>
    override method Recall() as logic
        if self:_deletedColumn >= 0
            return self:PutValue(self:_deletedColumn, 0)
        endif
        return super:Recall()
    end method

	/// <summary>Retrieve and optionally change information about a work area.</summary>
    /// <param name="nOrdinal">Specifies the type of information.</param>
    /// <param name="oValue">If specified (not null), then this parameter is used to change the value of a setting.</param>
    override method Info(uiOrdinal as long, oNewValue as object) as object
        if uiOrdinal == DbInfo.DBI_CANPUTREC
            return false
        elseif uiOrdinal == DbInfo.DBI_ISDBF
            return false
        endif
        return super:Info(uiOrdinal, oNewValue)
    end method


	/// <summary>Position the cursor to the first logical row.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method GoTop() as logic
        if !self:_ForceOpen()
            return false
        endif
        IF SELF:_tableMode == TableMode.Table
            self:_RecNo := 1
            return TRUE
        else
            return super:GoTop()
        endif
    end method

	/// <summary>Position the cursor to the last logical row.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>

    override method GoBottom() as logic
        if !self:_ForceOpen()
            return false
        endif
        return super:GoBottom()
    end method

	/// <summary>Position the cursor regardless of scope and filter conditions.</summary>
	/// <param name="nToSkip">The number of rows to skip.  If this argument is positive, the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
	/// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method SkipRaw(move as long) as logic
        if !self:_ForceOpen()
            return false
        endif
        var old := SELF:_relativeRecNo
        SELF:_relativeRecNo := TRUE
        var result := super:SkipRaw(move)
        SELF:_relativeRecNo := old
        RETURN result
    end method


	/// <summary>Position the cursor to a specific, physical row.</summary>
	/// <param name="nRec">The ONE based row number of the new cursor position.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database <br/>
    /// When a RecnoColumn is defined, then the cursor will be positioned on the row with the specified Recno, when it exists.
    /// If the recno does not exist, or when no RecnoColumn is defined, then the cursor will be positioned on the phantom row at the end of the table.
    /// </remarks>
    override method GoTo(nRec as long) as logic
        if !self:_ForceOpen()
            return false
        endif
        if self:_recnoColumn > 0 .and. nRec > 0
            local result as logic
            IF ! _relativeRecNo
                if self:_recordKeyCache:TryGetValue(nRec, out var nRowNum)
                    self:_RecNo := nRowNum + 1
                else
                    self:_RecNo := nRec
                endif
                var old := SELF:_relativeRecNo
                SELF:_relativeRecNo := TRUE
                result := super:GoTo(_RecNo)
                SELF:_relativeRecNo := old
                return result
            ENDIF
        endif
        self:_RecNo := nRec
        return super:GoTo(nRec)
    end method

    private miCheckEofBof as MethodInfo
    private method _GetCheckEofBof() as logic
        if miCheckEofBof == null
            miCheckEofBof := typeof(DBF):GetMethod("_CheckEofBof", BindingFlags.NonPublic+BindingFlags.Instance)
        endif
        return miCheckEofBof != null
    end method
	/// <summary>The physical row identifier at the current cursor position.</summary>
	/// <remarks>
    /// When a RecnoColumn is defined, then his will return the value of that column.
    /// Otherwise the relative position inside the cursor will be returned.
	/// </remarks>
    override property RecNo		as int
        get
            self:ForceRel()
            if self:_recnoColumn > 0
                // HACK  The code inside Xsharp.RDD for _CheckEofBof should check _RecNo and not RecNo
                if _GetCheckEofBof()
                    LOCAL st := StackTrace{ FALSE } AS StackTrace
                    IF st:GetFrame(1):GetMethod() == miCheckEofBof
                        RETURN super:RecNo
                    endif
                endif
                if ! _relativeRecNo
                    return (int) self:GetValue(self:_recnoColumn)
                endif
            endif
            return super:RecNo
        end get
    end property
	/// <summary>Position the cursor relative to its current position.</summary>
	/// <param name="nToSkip">The number of rows to skip.
    /// If this argument is positive, the cursor moves forward (toward the end-of-file).  If it is negative, the cursor moves backward (toward the beginning-of-file).</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method Skip(nSkip as long) as logic
        var old := SELF:_relativeRecNo
        SELF:_relativeRecNo := TRUE
        var result := super:Skip(nSkip)
        self:_relativeRecNo := old
        return result
    end method

	/// <summary>Perform a seek operation on the current selected index for the current Workarea.</summary>
	/// <param name="info">An object containing containing the necessary seek information.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>The result of the actial seek operation is stored in the Found property of the RDD and the EOF property.</remarks>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
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
            self:_dbfError(Subcodes.ERDD_DATATYPE, Gencode.EG_NOORDER, "SQLRDD:Seek","No current Order" )
            return false
        endif
        var cSeekExpr := CurrentOrder:SeekExpression(seekInfo )
        self:_OpenTable(cSeekExpr)
        return true
    end method

	/// <summary>Is the current row deleted?</summary>
	/// <remarks>
    /// When a DeletedColumn is defined, then his will return the value of that column.
    /// Otherwise the state of the current row is returned.
	/// </remarks>
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
