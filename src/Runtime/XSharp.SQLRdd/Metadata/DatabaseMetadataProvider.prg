// DatabaseMetadataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:54:42 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Data
USING System.Data.Common
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The DatabaseMetadataProvider class. Reads Metadata from tables in the database.
/// </summary>
/// <remarks>
/// It uses the following tables (when they exist)
/// - xs_tableInfo
/// - xs_indexInfo
/// - xs_defaults
/// </remarks>
CLASS DatabaseMetadataProvider INHERIT AbstractMetaDataProvider

    private cache as Dictionary<string, SqlTableInfo>

    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        SELF:ReadDefaults()
        cache := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}
        RETURN

    PRIVATE METHOD TableFields() as List<RddFieldInfo>
        var cols := List<RddFieldInfo>{}
        cols:Add(RddFieldInfo{nameof(TableName),"C", 50,0})
        cols:Add(RddFieldInfo{nameof(LongFieldNames),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(AllowUpdates),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(MaxRecords),"N", 10,0})
        cols:Add(RddFieldInfo{nameof(RecnoColumn),"C", 50,0})
        cols:Add(RddFieldInfo{nameof(DeletedColumn),"C", 50,0})
        cols:Add(RddFieldInfo{nameof(TrimTrailingSpaces),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(CompareMemo),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(Indexes),"C", 250,0})
        cols:Add(RddFieldInfo{nameof(ColumnList),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(KeyColumns),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(UpdatableColumns),"C", 255,0})
        RETURN cols
    END METHOD
    PRIVATE METHOD IndexFields() AS List<RddFieldInfo>
        var cols := List<RddFieldInfo>{}
        cols:Add(RddFieldInfo{nameof(TableName)         ,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(IndexName)         ,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(Ordinal)           ,"N", 10,0})
        cols:Add(RddFieldInfo{nameof(IndexTag)          ,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(IndexExpression)   ,"C", 250,0})
        cols:Add(RddFieldInfo{nameof(IndexCondition)    ,"C", 250,0})
        cols:Add(RddFieldInfo{nameof(IndexUnique)       ,"L", 1,0})
        RETURN cols
    END METHOD

    PRIVATE METHOD CreateDefaultColumnValues(cols as List<RddFieldInfo>, tableName as string) as string
        var sb  := StringBuilder{}
        var first := TRUE
        foreach var col in cols
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            switch col:FieldTypeStr
            case "C"
                if col:Name == nameof(TableName)
                    sb:Append(i"'{tableName}'")
                else
                    sb:Append("''")
                endif
            case "L"
                sb:Append("1")
            case "N"
                if col:Name == nameof(MaxRecords)
                    sb:Append("1000")
                else
                    sb:Append("0")
                endif
            otherwise
                sb:Append("NULL")
            end switch
        next
        return sb:ToString()
    PRIVATE METHOD CreateColumnNames(cols as List<RddFieldInfo>) as string
        var sb  := StringBuilder{}
        var first := TRUE
        foreach var col in cols
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            sb:Append(col:Name)
        next
        return sb:ToString()

    PRIVATE METHOD CreateDictionary() as VOID
        var sb  := StringBuilder{}
        var prov := Connection:Provider

        sb:Clear()
        sb:Append(prov:DropTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        Connection:Execute(sb:ToString(), out var _)
        sb:Clear()
        // build fields list
        var cols := TableFields()
        var first := TRUE
        foreach var col in cols
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            sb:Append(prov:GetSqlColumnInfo(col))
        next
        var fieldList := sb:ToString()
        var fieldValues := CreateDefaultColumnValues(cols, DefaultSection)
        var colNames := CreateColumnNames(cols)
        sb:Clear()
        sb:Append(prov:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        Connection:Execute(sb:ToString(), out var _)
        // create default values
        sb:Clear()
        sb:Append(prov:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
        sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
        Connection:Execute(sb:ToString(), out var _)

        var tables := Connection:GetTables()
        foreach var table in tables
            if table  != TableDictionary .and. table != IndexDictionary
                sb:Clear()
                sb:Append(prov:InsertStatement)
                colNames := CreateColumnNames(cols)
                fieldValues := CreateDefaultColumnValues(cols, table)
                sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
                sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
                sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
                Connection:Execute(sb:ToString(), out var _)
            endif
        next
        cols := IndexFields()
        sb:Clear()
        first := TRUE
        foreach var col in cols
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            sb:Append(prov:GetSqlColumnInfo(col))
        next
        fieldList := sb:ToString()
        sb:Clear()
        sb:Append(prov:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, IndexDictionary)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        Connection:Execute(sb:ToString(), out var _)
        RETURN
    METHOD ReadDefaults() AS VOID
        IF !Connection:DoesTableExist(TableDictionary)
            SELF:CreateDictionary()
        ELSEIF !Connection:DoesTableExist(IndexDictionary)
            SELF:CreateDictionary()
        ELSE
            // Check if the table dictionary has at least one record
            var sql1 := i"SELECT COUNT(*) FROM {TableDictionary}"
            Connection:Execute(sql1, out var count)
            if Convert.ToInt64(count) == 0
                SELF:CreateDictionary()
            endif
        ENDIF
        var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{DefaultSection}'"
        var cmd := SqlDbCommand{"DEFAULT", Connection}
        cmd:CommandText := sql
        local tbl := cmd:GetDataTable("TABLE") as DataTable
        if tbl:Rows:Count > 0
            var row := tbl:Rows[0]
            LongFieldNames      := (LOGIC) row[nameof(LongFieldNames)]
            AllowUpdates        := (LOGIC) row[nameof(AllowUpdates)]
            MaxRecords          := Convert.ToInt32(row[nameof(MaxRecords)])
            RecnoColumn         := (STRING) row[nameof(RecnoColumn)]
            DeletedColumn       := (STRING) row[nameof(DeletedColumn)]
            TrimTrailingSpaces  := (LOGIC) row[nameof(TrimTrailingSpaces)]
            CompareMemo         := (LOGIC) row[nameof(CompareMemo)]
        endif
        RETURN
    END METHOD
    private method AsString(oValue as object) as string
        if oValue is string var strValue
            return strValue:TrimEnd()
        endif
        return ""
    private method AsNumber(oValue as object) as int
        if oValue is int var iValue
            return iValue
        elseif oValue is real8 var r8Value
            return Convert.ToInt32(r8Value)
        elseif oValue is real4 var r4Value
            return Convert.ToInt32(r4Value)
        elseif oValue is decimal var decValue
            return Convert.ToInt32(decValue)
        endif
        return 0
    private method AsLogic(oValue as object) as logic
    if oValue is logic var lValue
        return lValue
    endif
    return AsNumber(oValue) != 0
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        if cache:TryGetValue(cTable, out oTable)
            return oTable
        endif
        oTable := SqlTableInfo{cTable, Connection}
        var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{cTable}'"
        var cmd := SqlDbCommand{"TABLEINFO", Connection}
        cmd:CommandText := sql
        local tbl := cmd:GetDataTable("TABLE") as DataTable
        if tbl:Rows:Count > 0
            var row := tbl:Rows[0]
            oTable:LongFieldNames      := AsLogic(row[nameof(LongFieldNames)])
            oTable:AllowUpdates        := AsLogic( row[nameof(AllowUpdates)])
            oTable:MaxRecords          := AsNumber(row[nameof(MaxRecords)])
            oTable:RecnoColumn         := AsString(row[nameof(RecnoColumn)])
            oTable:DeletedColumn       := AsString(row[nameof(DeletedColumn)])
            oTable:TrimTrailingSpaces  := AsLogic(row[nameof(TrimTrailingSpaces)])
            oTable:CompareMemo         := AsLogic(row[nameof(CompareMemo)])
            oTable:UpdatableColumns    := AsString(row[nameof(oTable:UpdatableColumns)])
            oTable:KeyColumns          := AsString(row[nameof(oTable:KeyColumns)])
            var cIndexes               := AsString(row[nameof(Indexes)])
            if (!String.IsNullOrEmpty(cIndexes))
                var aIndexes := cIndexes:Split(c",")
                foreach var cIndex in aIndexes
                    var oIndexInfo := SELF:GetIndexInfo(oTable, cIndex)
                    if (oIndexInfo != null)
                        oTable:Indexes:Add(oIndexInfo)
                    endif
                next
            endif
        endif
        cmd:Close()
        cache:Add(cTable, oTable)
        RETURN oTable

    OVERRIDE METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var sql := i"SELECT * FROM [{IndexDictionary}] WHERE {nameof(TableName)} = '{oTable.Name}' "+ ;
            i"and [{nameof(IndexName)}] = '{cIndexName}' Order by [Ordinal]"
        var cmd := SqlDbCommand{"INDEXINFO", Connection}
        cmd:CommandText := sql
        local tbl := cmd:GetDataTable("TABLE") as DataTable
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        if tbl:Rows:Count > 0
            foreach oRow as DataRow in tbl:Rows
                var oTag := SqlIndexTagInfo{oIndex, AsString(oRow[nameof(IndexTag)])}
                oTag:Expression    := AsString(oRow[nameof(IndexExpression)])
                oTag:Condition     := AsString(oRow[nameof(IndexCondition)])
                oTag:Unique        := AsLogic(oRow[nameof(IndexUnique)])
                oIndex:Tags:Add(oTag)
            next
        endif
        cmd:Close()
        return oIndex

#region constants
    INTERNAL CONST TableDictionary   := "xs_tableinfo" as string
    INTERNAL CONST IndexDictionary   := "xs_indexinfo" as string
    INTERNAL const Indexes           := "Indexes" as string
    internal const DefaultSection    := "Default" as string
    internal const TableName         := "TableName" as string
    internal const IndexName         := "IndexName" as string
    internal const Ordinal           := "Ordinal" as string
    internal const IndexTag          := "IndexTag" as string
    internal const IndexExpression   := "IndexExpression" as string
    internal const IndexCondition    := "IndexCondition" as string
    internal const IndexUnique       := "IndexUnique" as string
    internal const UpdatableColumns  := "UpdatableColumns" as string
    internal const KeyColumns        := "KeyColumns" as string
    internal const ColumnList        := "ColumnList" as string

#endregion
END CLASS

END NAMESPACE // XSharp.SQLRdd.Metadata
