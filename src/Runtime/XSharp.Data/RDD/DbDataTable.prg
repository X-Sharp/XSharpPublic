//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Data
USING XSharp.RDD
USING System.Reflection
USING XSharp.RDD.Support
USING System.Collections.Generic

/// <summary>This class is used to create a System.Data.DataTable from a workarea</summary>
/// <remarks>The data in this table is detached from the workarea. So if you make changes to the
/// data you will have to write that back to the workarea yourself.
/// </remarks>
CLASS XSharp.DbDataTable INHERIT DataTable
PROTECT _nAdding AS LONG
PROTECT _nArea   AS LONG

        
        
        CONSTRUCTOR()
            SUPER()
        /// <summary>Create a datatable from an IRdd object</summary>
        /// <remarks>The structure of the table will be derived from the structure of the workarea.
        /// and for each row in the workarea there will be a DbDataRow added to the table
        /// You can use the normal events for the DataTable class to monitor changes.
        /// and use the GetChanges() method from the DataTable class to retrieve a subset of the table with changed rows.
        /// </remarks>
        CONSTRUCTOR(oRDD AS IRdd)
            SUPER(oRDD:Alias)
            SELF:_nArea := (INT) oRDD:Area
            SELF:BuildColumns(oRDD)
            SELF:AddData(oRDD)
            SELF:AcceptChanges()
        
        
        PRIVATE METHOD BuildColumns(oRDD AS IRdd) AS VOID
            LOCAL nI AS LONG
            LOCAL aColumns AS DbColumnInfo[]
            aColumns := DbColumnInfo[]{oRDD:FieldCount}
            IF oRDD:Driver == "DBFVFPSQL"
                    FOR nI := 1 TO aColumns:Length
                        LOCAL oResult AS OBJECT
                        oResult := oRDD:FieldInfo(nI, DBS_COLUMNINFO,NULL)
                        IF oResult IS DbColumnInfo VAR info
                            aColumns[nI] := info
                        ELSE
                            LOCAL cField := (STRING) oRDD:FieldInfo(nI, DBS_ALIAS,NULL) AS STRING
                            LOCAL cType  := (STRING) oRDD:FieldInfo(nI, DBS_TYPE,NULL) AS STRING
                            LOCAL nLen   := (LONG) oRDD:FieldInfo(nI, DBS_LEN,NULL) AS LONG
                            LOCAL nDec   := (LONG) oRDD:FieldInfo(nI, DBS_DEC,NULL) AS LONG
                            VAR oColumn := DbColumnInfo{cField:ToLower(), cType+":0", nLen, nDec}
                            oColumn:Ordinal := nI
                            aColumns[nI] := oColumn
                        ENDIF
                NEXT
            ELSE
                FOR nI := 1 TO aColumns:Length
                    LOCAL cField := (STRING) oRDD:FieldInfo(nI, DBS_ALIAS,NULL) AS STRING
                    LOCAL cType  := (STRING) oRDD:FieldInfo(nI, DBS_TYPE,NULL) AS STRING
                    LOCAL nLen   := (LONG) oRDD:FieldInfo(nI, DBS_LEN,NULL) AS LONG
                    LOCAL nDec   := (LONG) oRDD:FieldInfo(nI, DBS_DEC,NULL) AS LONG
                    VAR oColumn := DbColumnInfo{cField:ToLower(), cType+":0", nLen, nDec}
                    oColumn:Ordinal := nI
                    aColumns[nI] := oColumn
                NEXT
            ENDIF
            FOREACH oDbCol AS DbColumnInfo IN aColumns
                SELF:Columns:Add(DbDataColumn{oDbCol})
            NEXT
        RETURN
        
        
        PRIVATE METHOD AddData(oRDD AS IRdd) AS VOID
            VAR nOld := oRDD:RecNo
            oRDD:GoTop()
            VAR nFldCount := oRDD:FieldCount
            DO WHILE ! oRDD:EoF
                LOCAL oData AS OBJECT[]
                oData := OBJECT[]{nFldCount}
                FOR VAR nI := 1 TO nFldCount
                    oData[nI] := oRDD:GetValue(nI)
                    IF oData[nI] IS STRING VAR strValue
                        oData[nI] := strValue:TrimEnd()
                    ENDIF
                NEXT
                SELF:_AddRow(oData,oRDD:RecNo)
                oRDD:Skip(1)
            ENDDO
            oRDD:GoTo(nOld)
        
        
        OVERRIDE PROTECTED METHOD NewRowFromBuilder(builder AS DataRowBuilder ) AS DataRow
            local row as DbDataRow
            IF SELF:_nAdding == 0
                row := DbDataRow{builder}
                row:RecNo := SELF:Rows:Count+1
            ELSE
                row := DbDataRow{builder, SELF:_nAdding}
            ENDIF
            RETURN row
        
        PRIVATE METHOD _AddRow(oData AS OBJECT[], nRecord AS LONG) AS VOID
            SELF:_nAdding := nRecord
            SELF:Rows:Add(oData)
        RETURN

        /// <summary>Save the changes in the DbDataTable to the underlying RDD</summary>
        /// <param name="oRDD">The RDD object for the area where the changes must be saved.</param>
        METHOD Save(oRDD AS IRdd) AS LOGIC
            LOCAL lockInfo AS DbLockInfo
            TRY
                lockInfo := DbLockInfo{}
                lockInfo:Method  := DbLockInfo.LockMethod.Exclusive
                FOREACH row AS DbDataRow IN SELF:Rows
                    LOCAL srow := (DataRow) row AS DataRow
                    VAR columnsChanged := List<DataColumn>{}
                    SWITCH srow:RowState
                        CASE DataRowState.Unchanged
                            NOP
                        CASE DataRowState.Deleted
                            IF ! oRDD:GoTo(row:RecNo)
                                THROW Error.VOError(EG_READ, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            lockInfo:RecId := row:RecNo
                            IF ! oRDD:Lock(REF lockInfo)
                                THROW Error.VOError(EG_LOCK, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            IF ! oRDD:Delete()
                                THROW Error.VOError(EG_WRITE, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            IF ! oRDD:UnLock(row:RecNo)
                                THROW Error.VOError(EG_LOCK, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                                
                        CASE DataRowState.Added
                            IF ! oRDD:Append(TRUE)
                                THROW Error.VOError(EG_WRITE, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            FOREACH oCol AS DataColumn IN SELF:Columns
                                columnsChanged:Add(oCol)
                            NEXT
                        CASE DataRowState.Modified
                            IF ! oRDD:GoTo(row:RecNo)
                                THROW Error.VOError(EG_READ, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            lockInfo:RecId := row:RecNo
                            IF ! oRDD:Lock(REF lockInfo)
                                THROW Error.VOError(EG_LOCK_ERROR, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                            ENDIF
                            // check if the record is unchanged
                            FOREACH oCol AS DataColumn IN SELF:Columns
                                VAR oldvalue := row[oCol:ColumnName, DataRowVersion.Original]
                                VAR current  := row[oCol:ColumnName, DataRowVersion.Current]
                                IF oldvalue != current
                                    VAR strOld := IIF(oldvalue == DBNull.Value, "", oldvalue:ToString()):Trim()
                                    VAR strNew := IIF(current  == DBNull.Value, "", current:ToString()):Trim()
                                    IF !strOld:Equals(strNew)
                                        columnsChanged:Add(oCol)
                                    ENDIF
                                ENDIF
                            NEXT
                            IF columnsChanged:Count == 0
                                IF ! oRDD:UnLock(row:RecNo)
                                    THROW Error.VOError(EG_LOCK, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                                ENDIF
                                RETURN FALSE
                            ENDIF
                    END SWITCH
                    IF columnsChanged:Count > 0
                        FOREACH oCol AS DataColumn IN columnsChanged
                            VAR newvalue := row[oCol:ColumnName, DataRowVersion.Current]
                            VAR oldvalue := row[oCol:ColumnName, DataRowVersion.Original]
                            IF (newvalue != oldvalue)
                                VAR pos      := oRDD:FieldIndex(oCol:ColumnName)
                                IF newvalue == DBNull.Value
                                    newvalue := NULL
                                ENDIF
                                IF ! oRDD:PutValue(pos, newvalue)
                                    THROW Error.VOError(EG_WRITE, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                                ENDIF
                            ENDIF
                        NEXT
                        IF ! oRDD:UnLock(row:RecNo)
                            THROW Error.VOError(EG_LOCK, "DbDataTable.Save","RecNo", 1, <OBJECT>{row:RecNo})
                        ENDIF
                    ENDIF
                        
            NEXT
        CATCH AS Exception
            THROW 
        END TRY 
        RETURN TRUE
END CLASS
        
        
        
        
