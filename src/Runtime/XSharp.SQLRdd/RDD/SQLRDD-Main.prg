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
partial class SQLRDD inherit DBFVFP

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
    public property RowCount    as long => iif(Self:DataTable == null, 0, Self:DataTable:Rows:Count)

    /// <summary>The current rownumber in the buffer (DataTable).</summary>
    public property RowNumber   as long GET _RecNo INTERNAL SET _RecNo := value

    /// <summary>The current row in the buffer (DataTable).
    /// When the server is at EOF then the phantomrow is returned.</summary>
    public property CurrentRow as DataRow
        get
            if self:RowNumber == 0 .or. self:RowNumber > self:RowCount
                return self:_phantomRow
            endif
            return self:DataTable:Rows[self:RowNumber -1]
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
        self:_TempFileName(info)
        self:_creating := true
        self:_Fields := self:_adjustCreateFields(self:_Fields)
        var lResult := super:Create(info)
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
        var oFields := List<RddFieldInfo>{}
        self:_updatableColumns := List<RddFieldInfo>{}
        self:_keyColumns       := List<RddFieldInfo>{}
        self:_numHiddenColumns := 0
        foreach var oCol in _oTd:Columns
            var oField := oCol:ColumnInfo
            if oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Recno)
                self:_recnoColumNo   := oField:Ordinal
                oField:Flags |= DBFFieldFlags.AutoIncrement
                oField:Flags |= DBFFieldFlags.System
                self:_numHiddenColumns += 1
            elseif oCol:ColumnFlags:HasFlag(SqlDbColumnFlags.Deleted)
                self:_deletedColumnNo := oField:Ordinal
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
        next
        var aFields := oFields:ToArray()
        CoreDb.Create(tempFile,aFields,typeof(DBFVFP),true,"SQLRDD-TEMP","",false,false)
        File.SetAttributes(tempFile, _OR(File.GetAttributes(tempFile), FileAttributes.Temporary))

        info:ReadOnly := false
        self:_getStructureOnly := true
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
            self:_CloseCursor()
        else
            self:DataTable      := _command:GetDataTable(self:Alias)
        endif
        _command:CommandText := cQuery
        self:_getStructureOnly := false
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
        var old := self:_baseRecno
        self:_baseRecno  := true
        var lResult := super:Append(lReleaseLock)
        self:_baseRecno  := old
        if lResult
            var key := self:_builder:GetNextKey()
            var row := self:DataTable:NewRow()
            self:DataTable:Rows:Add(row)
            _updatedRows:Add(row)
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
            if self:_recnoColumNo > -1
                self:_recordKeyCache:Add(key, SELF:RowCount-1)
            endif
            self:_serverReccount += 1
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
        // nFldPos is 1 based, the RDD compiles with /az+
        SELF:_ForceOpen()
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
        SELF:_ForceOpen()
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
                if !_updatedRows:Contains(row)
                    _updatedRows:Add(row)
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
        local lWasHot := self:_Hot as logic
        local lOk := super:GoCold() as logic
        if lWasHot .and. self:DataTable != null
            foreach var row in _updatedRows
                try
                    lOk := true
                    if super:Deleted
                        local wasNew := false as logic
                        // Append from may add deleted rows
                        if row:RowState.HasFlag(DataRowState.Added)
                            lOk := _ExecuteInsertStatement(row)
                            row:AcceptChanges()
                            wasNew  := true
                        endif
                        if self:_deletedColumnNo > -1
                            if !wasNew
                                // already written with _deletedColumnNo with the correct value
                                lOk := _ExecuteUpdateStatement(row, true)
                                if lOk
                                    row:AcceptChanges()
                                endif
                            endif
                        else
                            lOk := _ExecuteDeleteStatement(row, true)
                            // we do not clear the fields, but leave the row unchanged.
                            // the DBF has the deleted flag. This emulates what DBF files do

                            row:AcceptChanges()
                        endif

                    else
                        if row:RowState.HasFlag(DataRowState.Added)
                            lOk := _ExecuteInsertStatement(row)
                            row:AcceptChanges()
                        elseif row:RowState.HasFlag(DataRowState.Modified)
                            lOk := _ExecuteUpdateStatement(row, true)
                            row:AcceptChanges()
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
            self:_GetRecCount()
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
        if self:_deletedColumnNo > -1
            var row := self:CurrentRow
            if self:_deletedColumnIsLogic
                row[_deletedColumnNo] := true
            else
                row[_deletedColumnNo] := 1
            endif
        endif
        // Must position the DBF on the right row for the deletion
        var old := self:_baseRecno
        self:_baseRecno  := true
        super:GoTo(SELF:RowNumber)
        self:_baseRecno  := old
        return super:Delete()
    end method

    /// <summary>Remove the deletion marker from the row at the current cursor position.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>
    /// When a DeletedColumn is defined, then his set the value of that column to TRUE
    /// Otherwise when the current row is deleted and not persisted to the server yet, then the deletion is undone.
    /// </remarks>
    override method Recall() as logic
        if self:_deletedColumnNo >= 0
            var row := self:CurrentRow
            if self:_deletedColumnIsLogic
                row[_deletedColumnNo] := false
            else
                row[_deletedColumnNo] := 0
            endif
        endif
        // Must position the DBF on the right row for the recall
        var old := self:_baseRecno
        self:_baseRecno  := true
        super:GoTo(SELF:RowNumber)
        self:_baseRecno  := old
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


    /// <inheritdoc />
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method GoTop() as logic
        if !self:_ForceOpen()
            return false
        endif
        SELF:RowNumber  := 1
        SELF:_Top       := TRUE
        SELF:_Bottom    := FALSE
        SELF:_BufferValid := FALSE
        SELF:_SetEOF(FALSE)
        SELF:_SetBOF(FALSE)
        // Apply Filter and SetDeleted
        VAR result := SELF:SkipFilter(1)
        SELF:_CheckEofBof()
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("GoTop Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", result, _RecCount, RecNo, RowNumber, EoF, BoF)
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
        SELF:RowNumber  := SELF:RowCount
        SELF:_Top       := FALSE
        SELF:_Bottom    := TRUE
        SELF:_BufferValid := FALSE
        // Apply Filter and SetDeleted
        VAR result := SELF:SkipFilter(-1)
        SELF:_CheckEofBof()
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("GoBottom Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", result, _RecCount, RecNo, RowNumber, EoF, BoF)
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
        //
        IF nToSkip == 0
            // Refresh current Recno
            SELF:GoCold()
        ELSE
            var newRow := SELF:RowNumber + nToSkip
            IF newRow > 0
                if newRow <= SELF:RowCount
                    SELF:RowNumber := newRow
                    SELF:_SetEOF(FALSE)
                ELSE
                    SELF:RowNumber := 0
                    SELF:_SetEOF(TRUE)
                endif
            ELSE
                isOK := SELF:GoTop()
                SELF:_SetBOF(TRUE)
            ENDIF
        ENDIF
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine("SkipRaw Result: {0}, RecCount {1}, Recno {2}, RowNumber {3}, EOF {4}, BOF {5}", isOK, _RecCount, RecNo, RowNumber, EoF, BoF)
        #endif
        return isOK
    end method


    OVERRIDE METHOD GoToId(oRec AS OBJECT) AS LOGIC
	    LOCAL result AS LOGIC
		TRY
			VAR nRec := Convert.ToInt32( oRec )
			result := SELF:GoTo( nRec )
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
    override method GoTo(nRec as long) as logic
        local lSuccess := TRUE as logic
        if !self:_ForceOpen()
            return false
        endif
        SELF:GoCold()
        if self:_recnoColumNo > -1 .and. nRec != 0
            if self:_recordKeyCache:TryGetValue(nRec, out var nRowNum)
                nRec := nRowNum + 1
            else
                // when record number does not exist, then go to phantom record and return FALSE
                lSuccess := FALSE
                nRec     := 0
            endif
        endif
        LOCAL nCount := SELF:RowCount AS LONG
        // Normal positioning, VO resets FOUND to FALSE after a recprd movement
        SELF:_Found := FALSE
        SELF:_BufferValid := FALSE
        IF  nRec <= nCount  .AND.  nRec > 0
            SELF:RowNumber := nRec
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(FALSE)
        ELSEIF nRec < 0 .AND. nCount > 0
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
        RETURN lSuccess
    end method


    /// <summary>The physical row identifier at the current cursor position.</summary>
    /// <remarks>
    /// When a RecnoColumn is defined, then this will return the value of that column.
    /// Otherwise the relative position inside the cursor will be returned.
    /// </remarks>
    override property RecNo		as int
        get
            self:ForceRel()
            if !SELF:_baseRecno .and. self:_recnoColumNo > -1 .and. ! SELF:EoF
                var obj := SELF:CurrentRow[self:_recnoColumNo]
                return Convert.ToInt32(obj)
            endif
            return SELF:RowNumber
        end get
    end property

    /// <inheritdoc />
    override property RecCount as int
        get
            return Self:_serverReccount
        end get
    end property

    /// <inheritdoc />
    override method Skip(nToSkip as long) as logic
        LOCAL result := FALSE AS LOGIC
        //var oldRow := SELF:RowNumber
        //var oldRecNo := SELF:RecNo
        SELF:ForceRel()
        IF SELF:IsOpen

			SELF:_Top := SELF:_Bottom := FALSE
            IF nToSkip == 0
                result := SELF:GoCold()
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
        endif
        #ifdef TRACERDD
        System.Diagnostics.Debug.WriteLine(" Skip {0}, from row {1}, recno {2} to row {3}, recno {4}, EOF {5}, BOF {6}", nToSkip, oldRow, oldRecNo, SELF:RowNumber, SELF:RecNo, SELF:EoF, SELF:BoF)
        #endif
        RETURN result
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
        // change behavior when all rows are read.
        // In that case we can search in the local buffer
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
            if self:_deletedColumnNo > 0
                var res:= CurrentRow[self:_deletedColumnNo]
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
            else
                super:_RecNo := self:RowNumber
                return super:Deleted
            endif
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
        return super:Zap()
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


end class

end namespace // XSharp.RDD.SqlRDD
