//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO
USING System.Collections.Generic
USING System.Data
USING System.Diagnostics
USING System.Reflection
USING System.Data.Common

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The SqlRDD class.
/// </summary>
[DebuggerDisplay("SQLRDD ({Alias,nq})")];
CLASS SQLRDD INHERIT DBFVFP
    PROTECT _table          AS DataTable
    PROTECT _phantomRow     AS DataRow
    PROTECT _creatingIndex  AS LOGIC
    PROTECT _incrementKey   AS LONG
    PROTECT _incrementColumn AS DataColumn
    PROTECT _tableMode      AS TableMode
    PROTECT _connection     AS SqlDbConnection
    PROTECT _oTd            as SqlDbTableDef

#region Overridden properties
    OVERRIDE PROPERTY Driver AS STRING GET "SQLRDD"
#endregion
    CONSTRUCTOR()
        SUPER()
        _incrementKey    := -1
        _creatingIndex   := FALSE
        _tableMode       := TableMode.Query
        _ReadOnly        := TRUE
        _connection      := NULL
        RETURN

    PRIVATE METHOD TempFileName() AS STRING
        local result as string
        REPEAT
            var folder := Path.GetTempPath()
            var nId  := SqlDbConnection.GetId() / 0x100000
            var name := i"SQL{nId:X5}"
            result := Path.Combine(folder, name)
            result := Path.ChangeExtension(result, ".DBF")
        UNTIL ! File.Exists(result)
        return result
    OVERRIDE METHOD Open(info as DbOpenInfo) AS LOGIC
        var query := info:FileName
        local strConnection as STRING
        local pos as INT
        strConnection := SqlDbConnection.DefaultConnection
        pos := query:IndexOf(SqlDbProvider.ConnectionDelimiter)
        if pos > 0
            strConnection := query:Substring(0, pos)
            query := query:Substring(pos+2)
            info:FileName := query
        endif
        _connection := SqlDbGetConnection(strConnection)
        _connection:AddRdd(SELF)
        // Get the structure
        _oTd := _connection:GetStructureForQuery(query,"QUERY")
        var oFields := List<RddFieldInfo>{}
        foreach var oCol in _oTd:Columns
            oFields:Add(oCol:ColumnInfo)
        next
        var aFields := oFields:ToArray()
        var tempFile   := TempFileName()
        CoreDb.Create(tempFile,aFields,Typeof(DBFVFP),TRUE,"SQLRDD-TEMP","",FALSE,FALSE)
        info:FileName := Path.Combine(Path.GetDirectoryName(tempFile), Path.GetFileNameWithoutExtension(tempFile))
        info:Extension := ".DBF"
        info:ReadOnly := FALSE
        SUPER:Open(info)
        // Assoctiate the extra properties
        FOR VAR nI := 1 to aFields:Length
            var aField := aFields[nI-1]
            SELF:FieldInfo(nI, DBS_COLUMNINFO, aField)
        NEXT
        var cmd   := _connection:Provider:CreateCommand()
        cmd:CommandText := query
        cmd:Connection := _connection:DbConnection
        using var reader := cmd:ExecuteReader()
        local oDataTable as DataTable
        oDataTable := DataTable{}
        oDataTable:Load(reader)
        self:DataTable := oDataTable
        return true

    /// <inheritdoc />
    OVERRIDE METHOD SetFieldExtent(nFields AS LONG) AS LOGIC
        VAR result := SUPER:SetFieldExtent(nFields)
        RETURN result

    /// <inheritdoc />
    OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
        VAR lResult := SUPER:Create(info)
        SELF:_RecordLength := 2 // 1 byte "pseudo" data + deleted flag
        RETURN lResult

    /// <inheritdoc />
    OVERRIDE METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
        VAR lResult := SUPER:Append(lReleaseLock)
        IF lResult
            VAR row := _table:NewRow()
            IF _incrementColumn != NULL
                row[_incrementColumn] := _incrementKey
                _incrementKey -= 1
            ENDIF
            IF row IS IDbRow VAR dbRow
                dbRow:RecNo := SUPER:RecNo
            ENDIF
            _table:Rows:Add(row)
        ENDIF
        RETURN lResult
    OVERRIDE METHOD FieldIndex(fieldName AS STRING) AS INT
        LOCAL result AS INT
        // SUPER:FieldIndex uses a dictionary, so that is fast, If that fails then
        // check again for colum names.
        result := SUPER:FieldIndex(fieldName)
        IF result == 0
            FOREACH VAR oColumn IN SELF:_Fields
                IF oColumn != NULL .AND. String.Compare(oColumn:ColumnName, fieldName, TRUE) == 0
                    RETURN oColumn:Ordinal
                ENDIF
            NEXT
        ENDIF
        RETURN result

    /// <inheritdoc />
    OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
        // nFldPos is 1 based, the RDD compiles with /az+
        IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
            nFldPos -= 1
            LOCAL result AS OBJECT
            IF !SELF:EoF
                VAR row := _table:Rows[SELF:_RecNo -1]
                result  := row[nFldPos]
            ELSE
                result := _phantomRow[nFldPos]
            ENDIF
            IF result == DBNull.Value
                // The phantom row already is padded with trailing spaces
                result := _phantomRow[nFldPos]
            ELSEIF _creatingIndex .AND. result IS STRING VAR strResult
                result := strResult:PadRight(_Fields[nFldPos]:Length,' ')
            ENDIF
            RETURN result
        ENDIF
        RETURN SUPER:GetValue(nFldPos)

    /// <inheritdoc />
    OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        // nFldPos is 1 based, the RDD compiles with /az+
        IF SELF:_ReadOnly
            SELF:_dbfError(ERDD.READONLY, XSharp.Gencode.EG_READONLY )
            RETURN FALSE
        ENDIF
        IF SELF:EoF
            RETURN FALSE
        ENDIF
        VAR result := FALSE
        IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
            VAR row := _table:Rows[SELF:_RecNo -1]
            row[nFldPos-1] := oValue
            result := TRUE
        ENDIF
        RETURN result

    /// <summary>
    /// This property returns the DataTable object that is used to cache the results locally
    /// </summary>
    /// <value></value>
    PROPERTY DataTable AS DataTable
        GET
            RETURN _table
        END GET
        SET
            // When we get here then the (temporary) DBFVFP table has already been created and opened
            // and the fields are already read from the DBF header in the temporary table
            // The SqlStatement:CreateFile() method whichs gets called from SqlExec()
            // has the logic that creates the DBF from the Column properties
            //
            _table := VALUE
            SELF:_RecNo := 1
            SELF:_RecCount   := _table:Rows:Count
            SELF:_phantomRow := _table:NewRow()
            VAR prop := _table:GetType():GetProperty("EnforceConstraints", BindingFlags.Instance+BindingFlags.NonPublic)
            IF prop != NULL
                prop:SetValue(_table, FALSE)
            ENDIF
            FOREACH oColumn AS DataColumn IN _table:Columns
                VAR index := oColumn:Ordinal
                LOCAL dbColumn := SELF:_Fields[index] AS RddFieldInfo
                // use the BlankValue() from the RddFieldInfo class. One place to define blanks is enough
                VAR blank := dbColumn:BlankValue()
                IF blank IS STRING VAR strBlank
                    blank := strBlank:PadRight(dbColumn:Length, ' ')
                ENDIF
                SELF:_phantomRow[index] := blank
                dbColumn:Caption     := oColumn:Caption
                IF oColumn:AutoIncrement
                    _incrementColumn := oColumn
                ENDIF
                IF !oColumn:AllowDBNull
                    oColumn:AllowDBNull := TRUE
                ENDIF
                dbColumn:Flags := DBFFieldFlags.None
            NEXT
            SELF:Header:RecCount := _RecCount
            // set file length
            LOCAL lOffset   := SELF:_HeaderLength + SELF:_RecCount * SELF:_RecordLength AS INT64
            // Note FoxPro does not write EOF character for files with 0 records
            _oStream:SafeSetPos(lOffset)
            _oStream:SafeWriteByte(26)
            _oStream:SafeSetLength(lOffset+1)
            // now set the file size and reccount in the header
            SELF:GoTop()
        END SET
    END PROPERTY

    /// <inheritdoc />
    OVERRIDE METHOD Close() AS LOGIC
        LOCAL lOk AS LOGIC
        // This method deletes the temporary file after the file is closed
        LOCAL cFileName := SELF:_FileName AS STRING
        LOCAL cMemoName := "" AS STRING
        _connection:RemoveRdd(SELF)
        IF SELF:_Memo IS AbstractMemo VAR memo
            cMemoName := memo:FileName
        ENDIF
        lOk := SUPER:Close()
        IF lOk
            IF File(cFileName)
                FErase(FPathName())
            ENDIF
            IF ! String.IsNullOrEmpty(cMemoName) .AND. File(cMemoName)
                FErase(FPathName())
            ENDIF
        ENDIF
        RETURN lOk

    /// <inheritdoc />
    OVERRIDE METHOD Info(uiOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
        IF uiOrdinal == DbInfo.DBI_CANPUTREC
            RETURN FALSE
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)

    /// <inheritdoc />
    OVERRIDE METHOD OrderCreate(orderInfo AS DbOrderCreateInfo ) AS LOGIC
        SELF:_creatingIndex := TRUE
        VAR result := SUPER:OrderCreate(orderInfo)
        SELF:_creatingIndex := FALSE
        RETURN result

    /// <inheritdoc />
    OVERRIDE METHOD OrderListRebuild() AS LOGIC
        SELF:_creatingIndex := TRUE
        VAR result := SUPER:OrderListRebuild()
        SELF:_creatingIndex := FALSE
        RETURN result
END CLASS

END NAMESPACE // XSharp.RDD.SqlRDD
