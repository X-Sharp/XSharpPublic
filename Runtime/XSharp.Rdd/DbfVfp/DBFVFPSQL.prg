//
// Copyright (c) B.V.  All Rights Reserved.
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

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFVFPSQL RDD. DBFCDX with support for the FoxPro field types and a List of Object values as backing collection for the data.</summary>

    [DebuggerDisplay("DBFVFPSQL ({Alias,nq})")];
    CLASS DBFVFPSQL INHERIT DBFVFP
        PROTECT _table as DataTable
        PROTECT _phantomRow as DataRow
        PROTECT _creatingIndex AS LOGIC
        PROTECT _incrementKey as LONG
        PROTECT _incrementColumn as DataColumn
        #region Overridden properties
        OVERRIDE PROPERTY Driver AS STRING GET "DBFVFPSQL"

        #endregion

        CONSTRUCTOR()
            SUPER()
            _incrementKey    := -1
            _creatingIndex      := FALSE
            RETURN

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
                var row := _table:NewRow()
                IF _incrementColumn != NULL
                    row[_incrementColumn] := _incrementKey
                    _incrementKey -= 1
                ENDIF
                if row IS IDbRow VAR dbRow
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
                FOREACH var oColumn in SELF:_Fields
                    if oColumn != NULL .and. String.Compare(oColumn:ColumnName, fieldName, TRUE) == 0
                        return oColumn:Ordinal
                    ENDIF
                NEXT
            ENDIF
            RETURN result


        OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            // nFldPos is 1 based, the RDD compiles with /az+
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                nFldPos -= 1
                LOCAL result as OBJECT
                IF !SELF:EoF
                    var row := _table:Rows[SELF:_RecNo -1]
                    result  := row[nFldPos]
                ELSE
                    result := _phantomRow[nFldPos]
                ENDIF
                IF result == DBNull.Value
                    // The phantom row already is padded with trailing spaces
                    result := _phantomRow[nFldPos]
                ELSEIF _creatingIndex .and. result IS String var strResult
                    result := strResult:PadRight(_Fields[nFldPos]:Length,' ')
                ENDIF
                RETURN result
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

         OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // nFldPos is 1 based, the RDD compiles with /az+
            var result := FALSE 
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                var row := _table:Rows[SELF:_RecNo -1]
                row[nFldPos-1] := oValue
                result := TRUE
            ENDIF
            RETURN result


        PROPERTY DataTable as DataTable
            GET
                return _table
            END GET
            SET
                // When we get here then the (temporary) DBFVFP table has already been created and opened
                // and the fields are already read from the DBF header in the temporary table
                // The SqlStatement:CreateFile() method whichs gets called from SqlExec()
                // has the logic that creates the DBF from the Column properties
                //
                _table := value
                SELF:_RecNo := 1
                SELF:_RecCount   := _table:Rows:Count
                SELF:_phantomRow := _table:NewRow()
                var prop := _table:GetType():GetProperty("EnforceConstraints", BindingFlags.Instance+BindingFlags.NonPublic)
                IF prop != null
                    prop:SetValue(_table, FALSE)
                ENDIF
                FOREACH oColumn as DataColumn in _table:Columns
                    var index := oColumn:Ordinal
                    LOCAL dbColumn := self:_Fields[index] as RddFieldInfo
                    // use the BlankValue() from the RddFieldInfo class. One place to define blanks is enough
                    var blank := dbColumn:BlankValue()
                    if blank IS STRING VAR strBlank
                        blank := strBlank:PadRight(dbColumn:Length, ' ')
                    ENDIF
                    SELF:_phantomRow[index] := blank
                    dbColumn:Caption     := oColumn:Caption
                    if oColumn:AutoIncrement
                        _incrementColumn := oColumn
                    endif
                    if !oColumn:AllowDBNull
                        oColumn:AllowDBNull := TRUE
                    endif
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
            END SET
        END PROPERTY

        /// <inheritdoc />
        OVERRIDE METHOD Close() AS LOGIC
            LOCAL lOk AS LOGIC
            // This method deletes the temporary file after the file is closed
            LOCAL cFileName := SELF:_FileName AS STRING
            LOCAL cMemoName := "" AS STRING
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

    OVERRIDE METHOD OrderCreate(orderInfo AS DbOrderCreateInfo ) AS LOGIC
        SELF:_creatingIndex := TRUE
        VAR result := SUPER:OrderCreate(orderInfo)
        SELF:_creatingIndex := FALSE
        RETURN result

    OVERRIDE METHOD OrderListRebuild() AS LOGIC
        SELF:_creatingIndex := TRUE
        VAR result := SUPER:OrderListRebuild()
        SELF:_creatingIndex := FALSE
        RETURN result


    END CLASS

END NAMESPACE

