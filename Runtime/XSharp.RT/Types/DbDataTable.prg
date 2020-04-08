//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Data
USING XSharp.RDD
USING XSharp.RDD.Support
USING System.Reflection

/// <summary>This class is used to create a System.Data.DataTable from a workarea</summary>
CLASS XSharp.DbDataTable INHERIT DataTable
    PROTECT _nAdding AS LONG
    PROTECT _nArea   AS LONG
    PROPERTY Filling AS LOGIC AUTO GET SET
    
    CONSTRUCTOR(oRDD AS IRdd)
        SUPER(oRDD:Alias)
        SELF:_nArea := (INT) oRDD:Area
        SELF:BuildColumns(oRDD)
        SELF:AddData(oRDD)
        SELF:AcceptChanges()
        SELF:ColumnChanging     += Table_ColumnChanging
        SELF:ColumnChanged      += Table_ColumnChanged
        SELF:RowChanging        += Table_RowChanging
        SELF:RowChanged         += Table_RowChanged
        SELF:RowDeleting        += Table_RowDeleting
        SELF:RowDeleted         += Table_RowDeleted
        SELF:TableNewRow        += Table_RowAdded
        SELF:TableCleared       += Table_TableCleared
        SELF:TableClearing       += Table_TableClearing


    PRIVATE METHOD BuildColumns(oRDD AS IRdd) AS VOID
        LOCAL nI AS LONG
        LOCAL aColumns AS DbColumnInfo[]
        LOCAL aStruct AS ARRAY
        aColumns := DbColumnInfo[]{oRDD:FieldCount}
        aStruct  := DbStruct()
        IF oRDD:Driver == "DBFVFPSQL"
            FOR nI := 1 TO aColumns:Length
                LOCAL oResult AS OBJECT
                oResult := oRDD:FieldInfo(nI, DBS_COLUMNINFO,NULL)
                IF oResult IS DbColumnInfo VAR info
                    aColumns[nI] := info
                ELSE
                    LOCAL aField  := aStruct[nI] AS ARRAY
                    VAR oColumn := DbColumnInfo{aField[DBS_NAME], aField[DBS_TYPE]+":0", aField[DBS_LEN], aField[DBS_DEC]}
                    oColumn:Ordinal := nI
                    aColumns[nI] := oColumn
                ENDIF
            NEXT
        ELSE
            FOR nI := 1 TO aColumns:Length
                LOCAL aField  := aStruct[nI] AS ARRAY
                VAR oColumn := DbColumnInfo{aField[DBS_NAME], aField[DBS_TYPE]+":0", aField[DBS_LEN], aField[DBS_DEC]}
                oColumn:Ordinal := nI
                aColumns[nI] := oColumn
            NEXT
        ENDIF
        FOREACH oDbCol AS DbColumnInfo IN aColumns
            SELF:Columns:Add(DbDataColumn{oDbCol})
        NEXT
        RETURN


    PRIVATE METHOD AddData(oRDD AS IRdd) AS VOID
        LOCAL oMIGet := NULL AS MethodInfo 
        // DBFVFPSQL has a method GetData to get the array that stores all the field values
        oMIGet := oRDD:GetType():GetMethod("GetData", BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
        VAR nOld := oRDD:RecNo
        oRDD:GoTop()
        IF oMIGet != NULL
            VAR GetData := (DbGetData) oMIGet:CreateDelegate(typeof(DbGetData), oRDD) 
            DO WHILE ! oRDD:EoF
                VAR data := GetData()
                SELF:AddRow(data,oRDD:RecNo)
                oRDD:Skip(1)
            ENDDO
        ELSE
            VAR nFldCount := oRDD:FieldCount
            DO WHILE ! oRDD:EoF
                LOCAL oData AS OBJECT[]
                oData := OBJECT[]{nFldCount}
                FOR VAR nI := 1 TO nFldCount
                    oData[nI] := oRDD:GetValue(nI)
                NEXT
                SELF:AddRow(oData,oRDD:RecNo)
                oRDD:Skip(1)
            ENDDO
        ENDIF
        oRDD:GoTo(nOld)

    
    OVERRIDE PROTECTED METHOD NewRowFromBuilder(builder AS DataRowBuilder ) AS DataRow
        RETURN DbDataRow{builder, SELF:_nAdding}
    
    METHOD AddRow(oData AS OBJECT[], nRecord AS LONG) AS VOID
    SELF:_nAdding := nRecord
    SELF:Rows:Add(oData)
    RETURN
    
    PRIVATE METHOD Table_ColumnChanging( sender AS OBJECT , e AS DataColumnChangeEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_ColumnChanged( sender AS OBJECT , e AS DataColumnChangeEventArgs ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_RowChanging( sender AS OBJECT , e AS DataRowChangeEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_RowChanged( sender AS OBJECT , e AS DataRowChangeEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_RowDeleting( sender AS OBJECT , e AS DataRowChangeEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_RowDeleted( sender AS OBJECT , e AS DataRowChangeEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_RowAdded( sender AS OBJECT , e AS DataTableNewRowEventArgs  ) AS VOID
        RETURN
        
    PRIVATE METHOD Table_TableClearing( sender AS OBJECT , e AS DataTableClearEventArgs   ) AS VOID
        RETURN
    
    PRIVATE METHOD Table_TableCleared( sender AS OBJECT , e AS DataTableClearEventArgs   ) AS VOID
        RETURN
        
    PROPERTY LastRow AS DbDataRow
        GET
            IF SELF:Rows:Count == 0
                RETURN NULL
            ENDIF
            RETURN (DbDataRow) SELF:Rows[SELF:Rows:Count -1]
        END GET
    END PROPERTY
    
END CLASS


/// <summary>This class represents a DBF Row in a DbDataTable class.</summary>
CLASS XSharp.DbDataRow INHERIT DataRow
    PROPERTY RecNo AS LONG AUTO 
    PROTECTED INTERNAL CONSTRUCTOR(builder AS DataRowBuilder , nRecord AS LONG)
        SUPER(builder)
    SELF:RecNo := nRecord
END CLASS


/// <summary>This class represents a DBF Field in a DbDataTable class.</summary>
CLASS XSharp.DbDataColumn INHERIT DataColumn
    PROPERTY ColumnInfo AS DbColumnInfo AUTO
    
    CONSTRUCTOR(info AS DbColumnInfo)
    SUPER(info:ColumnName, info:DotNetType)
    SELF:ColumnInfo := info
    SELF:AllowDBNull := info:IsNullable
    IF info:IsAutoIncrement
        SELF:AutoIncrement := info:IsAutoIncrement
        SELF:AutoIncrementSeed := -1
        SELF:AutoIncrementStep := -1
        SELF:ReadOnly          := TRUE
    ENDIF
    
END CLASS
