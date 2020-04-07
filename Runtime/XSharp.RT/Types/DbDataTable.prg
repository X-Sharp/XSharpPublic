// DbDataTable.prg
// Created by    : robert
// Creation Date : 4/7/2020 5:06:01 PM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Data
USING XSharp.RDD
USING XSharp.RDD.Support

/// <summary>This class is used to create a System.Data.DataTable from a workarea</summary>
CLASS DbDataTable INHERIT DataTable
    PROTECT _nAdding AS LONG
    PROTECT _nArea   AS LONG
    PROPERTY Filling AS LOGIC AUTO GET SET
    CONSTRUCTOR()
        SUPER(Alias())
        _nArea := (LONG) VoDb.GetSelect()
        SELF:ColumnChanging     += Table_ColumnChanging
        SELF:ColumnChanged      += Table_ColumnChanged
        SELF:RowChanging        += Table_RowChanging
        SELF:RowChanged         += Table_RowChanged
        SELF:RowDeleting        += Table_RowDeleting
        SELF:RowDeleted         += Table_RowDeleted
        SELF:TableNewRow        += Table_RowAdded
        SELF:TableCleared       += Table_TableCleared
        SELF:TableClearing       += Table_TableClearing
        
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
CLASS DbDataRow INHERIT DataRow
    PROPERTY RecNo AS LONG AUTO 
    PROTECTED INTERNAL CONSTRUCTOR(builder AS DataRowBuilder , nRecord AS LONG)
        SUPER(builder)
        SELF:RecNo := nRecord
    END CLASS


/// <summary>This class represents a DBF Field in a DbDataTable class.</summary>
CLASS DbDataColumn INHERIT DataColumn
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
