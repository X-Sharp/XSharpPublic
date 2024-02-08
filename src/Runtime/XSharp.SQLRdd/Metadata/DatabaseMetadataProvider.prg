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
        cols:Add(RddFieldInfo{TableName,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.RealName),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.UpdatableColumns),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.LongFieldNames),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.AllowUpdates),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.MaxRecords),"N", 10,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.RecnoColumn),"C", 50,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.DeletedColumn),"C", 50,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.TrimTrailingSpaces),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.CompareMemo),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.Indexes),"C", 250,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.ColumnList),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.KeyColumns),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.ServerFilter),"C", 255,0})
        RETURN cols
    END METHOD
    PRIVATE METHOD IndexFields() AS List<RddFieldInfo>
        var cols := List<RddFieldInfo>{}
        cols:Add(RddFieldInfo{TableName         ,"C", 50,0})
        cols:Add(RddFieldInfo{IndexName         ,"C", 50,0})
        cols:Add(RddFieldInfo{Ordinal           ,"N", 10,0})
        cols:Add(RddFieldInfo{TagName           ,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.Expression)   ,"C", 250,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.Condition)    ,"C", 250,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.Unique)       ,"L", 1,0})
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
        // Read the defaults from the database
        var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{DefaultSection}'"
        var cmd := SqlDbCommand{"DEFAULT", Connection}
        cmd:CommandText := sql
        local tbl := cmd:GetDataTable("TABLE") as DataTable
        if tbl:Rows:Count > 0
            var row := tbl:Rows[0]
            LongFieldNames      := AsLogic (row[nameof(SqlRDDEventReason.LongFieldNames)])
            AllowUpdates        := AsLogic (row[nameof(SqlRDDEventReason.AllowUpdates)])
            MaxRecords          := AsNumber(row[nameof(SqlRDDEventReason.MaxRecords)])
            RecnoColumn         := AsString(row[nameof(SqlRDDEventReason.RecnoColumn)])
            DeletedColumn       := AsString(row[nameof(SqlRDDEventReason.DeletedColumn)])
            TrimTrailingSpaces  := AsLogic (row[nameof(SqlRDDEventReason.TrimTrailingSpaces)])
            CompareMemo         := AsLogic (row[nameof(SqlRDDEventReason.CompareMemo)])
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
            oTable:RealName            := AsString(row[nameof(SqlRDDEventReason.RealName)])
            oTable:LongFieldNames      := AsLogic (row[nameof(SqlRDDEventReason.LongFieldNames)])
            oTable:AllowUpdates        := AsLogic (row[nameof(SqlRDDEventReason.AllowUpdates)])
            oTable:MaxRecords          := AsNumber(row[nameof(SqlRDDEventReason.MaxRecords)])
            oTable:RecnoColumn         := AsString(row[nameof(SqlRDDEventReason.RecnoColumn)])
            oTable:DeletedColumn       := AsString(row[nameof(SqlRDDEventReason.DeletedColumn)])
            oTable:TrimTrailingSpaces  := AsLogic (row[nameof(SqlRDDEventReason.TrimTrailingSpaces)])
            oTable:CompareMemo         := AsLogic (row[nameof(SqlRDDEventReason.CompareMemo)])
            oTable:UpdatableColumns    := AsString(row[nameof(SqlRDDEventReason.UpdatableColumns)])
            oTable:KeyColumns          := AsString(row[nameof(SqlRDDEventReason.KeyColumns)])
            oTable:ServerFilter        := AsString(row[nameof(SqlRDDEventReason.ServerFilter)])
            var cIndexes               := AsString(row[nameof(SqlRDDEventReason.Indexes)])
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
                var oTag := SqlIndexTagInfo{oIndex, AsString(oRow[nameof(TagName)])}
                oTag:Expression    := AsString(oRow[nameof(SqlRDDEventReason.Expression)])
                oTag:Condition     := AsString(oRow[nameof(SqlRDDEventReason.Condition)])
                oTag:Unique        := AsLogic(oRow[nameof(SqlRDDEventReason.Unique)])
                oIndex:Tags:Add(oTag)
            next
        endif
        cmd:Close()
        return oIndex

#region constants
    INTERNAL CONST TableDictionary   := "xs_tableinfo" as string
    INTERNAL CONST IndexDictionary   := "xs_indexinfo" as string
    INTERNAL CONST DefaultSection    := "default" as string
    INTERNAL CONST TableName         := nameof(TableName) as string
    INTERNAL CONST IndexName         := nameof(IndexName) as string
    INTERNAL CONST TagName           := nameof(TagName) as string
    INTERNAL CONST Ordinal           := nameof(Ordinal) as string

#endregion
END CLASS

END NAMESPACE // XSharp.SQLRdd.Metadata
