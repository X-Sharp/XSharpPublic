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
        PROTECT _padStrings AS LOGIC
        PROTECT _incrementKey as LONG
        PROTECT _incrementColumn as DataColumn
        #region Overridden properties
        OVERRIDE PROPERTY Driver AS STRING GET "DBFVFPSQL"

        #endregion

        CONSTRUCTOR()
            SUPER()
            _incrementKey    := -1
            _padStrings      := FALSE
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

        VIRTUAL METHOD FieldIndex(fieldName AS STRING) AS INT
            LOCAL result AS INT
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
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                IF !SELF:EoF
                    var row := _table:Rows[SELF:_RecNo -1]
                    return row[nFldPos-1]
                ENDIF
                RETURN _phantomRow[nFldPos-1]
            ENDIF
            RETURN SUPER:GetValue(nFldPos)
            
        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                var row := _table:Rows[SELF:_RecNo -1]
                row[nFldPos-1] := oValue
            ENDIF
            RETURN SUPER:PutValue(nFldPos, oValue)


        PROPERTY DataTable as DataTable
            GET
                return _table
            END GET
            SET
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
        SELF:_padStrings := TRUE
        VAR result := SUPER:OrderCreate(orderInfo)
        SELF:_padStrings := FALSE
        RETURN result

    OVERRIDE METHOD OrderListRebuild() AS LOGIC
        SELF:_padStrings := TRUE
        VAR result := SUPER:OrderListRebuild()
        SELF:_padStrings := FALSE
        RETURN result


    END CLASS  

END NAMESPACE

