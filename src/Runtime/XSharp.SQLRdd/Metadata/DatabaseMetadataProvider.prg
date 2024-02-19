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
    private hasDefaults as logic

    /// <summary>
    /// Create a new instance of the DatabaseMetadataProvider class.
    /// </summary>
    /// <param name="conn">Connection associated with the provider</param>

    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        cache := Dictionary<string, SqlTableInfo>{StringComparer.OrdinalIgnoreCase}
        RETURN

    PRIVATE METHOD TableFields() as List<RddFieldInfo>
        var cols := List<RddFieldInfo>{}
        cols:Add(RddFieldInfo{TableName,"C", 50,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.RealName),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.UpdatableColumns),"C", 255,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.LongFieldNames),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.AllowUpdates),"L", 1,0})
        cols:Add(RddFieldInfo{nameof(SqlRDDEventReason.UpdateAllColumns),"L", 1,0})
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

    PRIVATE METHOD CreateDefaultColumnValues(cols as List<RddFieldInfo>, TableName as string) as string
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
                    sb:Append(i"'{TableName}'")
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
    end method

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
    end method

    PRIVATE METHOD CreateDictionary() as VOID
        var sb  := StringBuilder{}
        var prov := Connection:Provider

        sb:Clear()
        sb:Append(prov:DropTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        Connection:ExecuteNonQuery(sb:ToString())
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
        Connection:ExecuteNonQuery(sb:ToString())
        // create default values
        sb:Clear()
        sb:Append(prov:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
        sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
        Connection:ExecuteNonQuery(sb:ToString())

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
                Connection:ExecuteNonQuery(sb:ToString())
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
        Connection:ExecuteNonQuery(sb:ToString())
        RETURN
    end method

    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            // Check if the table dictionary exists
            IF !Connection:DoesTableExist(TableDictionary)
                SELF:CreateDictionary()
            ELSEIF !Connection:DoesTableExist(IndexDictionary)
                SELF:CreateDictionary()
            ELSE
                // Check if the table dictionary has at least one record
                var sql1 := i"SELECT COUNT(*) FROM {TableDictionary}"
                var count := Connection:ExecuteScalar(sql1)
                if Convert.ToInt64(count) == 0
                    SELF:CreateDictionary()
                endif
            ENDIF
            // Read the defaults from the database
            var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{DefaultSection}'"
            local rdr := Connection:ExecuteReader(sql) as DbDataReader
            if rdr != null
                if rdr:Read()
                    LongFieldNames      := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.LongFieldNames))
                    AllowUpdates        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.AllowUpdates))
                    MaxRecords          := SELF:_GetNumber(rdr, nameof(SqlRDDEventReason.MaxRecords))
                    RecnoColumn         := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RecnoColumn))
                    DeletedColumn       := SELF:_GetString(rdr, nameof(SqlRDDEventReason.DeletedColumn))
                    TrimTrailingSpaces  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.TrimTrailingSpaces))
                    CompareMemo         := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.CompareMemo))
                    UpdateAllColumns    := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.UpdateAllColumns))
                endif
                rdr:Close()
            endif
            hasDefaults := TRUE
        endif
        return
    end method

    private method _GetPos(rdr as DbDataReader, cName as string) AS Long
        var pos := rdr:GetOrdinal(cName)
        if pos >= 0
            return pos
        else
            Throw Exception{String.Format("Field {0} not found in the resultset", cName)}
        endif
    end method

    private method _GetString(rdr as DbDataReader, cName as string) as string
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                return rdr:GetString(pos)
            endif
        catch
            return ""
        end try
        return ""
    end method

    private method _GetNumber(rdr as DbDataReader, cName as string) as int
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                var num := rdr:GetDecimal(pos)
                return Convert.ToInt32(num)
            endif
        catch
            return 0
        end try
        return 0
    end method

    private method _GetLogic(rdr as DbDataReader, cName as string) as logic
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                return rdr:GetBoolean(pos)
            endif
        catch
            return false
        end try
        return false
    end method

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlTableInfo
        local oTable as SqlTableInfo
        SELF:ReadDefaults()

        if cache:TryGetValue(cTable, out oTable)
            return oTable
        endif
        oTable := SqlTableInfo{cTable, Connection}
        var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{cTable}'"
        local rdr := SELF:Connection:ExecuteReader(sql) as DbDataReader
        if rdr != null
            if rdr:Read()
                oTable:RealName            := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RealName))
                oTable:LongFieldNames      := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.LongFieldNames))
                oTable:AllowUpdates        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.AllowUpdates))
                oTable:MaxRecords          := SELF:_GetNumber(rdr, nameof(SqlRDDEventReason.MaxRecords))
                oTable:RecnoColumn         := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RecnoColumn))
                oTable:DeletedColumn       := SELF:_GetString(rdr, nameof(SqlRDDEventReason.DeletedColumn))
                oTable:TrimTrailingSpaces  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.TrimTrailingSpaces))
                oTable:CompareMemo         := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.CompareMemo))
                oTable:UpdatableColumns    := SELF:_GetString(rdr, nameof(SqlRDDEventReason.UpdatableColumns))
                oTable:KeyColumns          := SELF:_GetString(rdr, nameof(SqlRDDEventReason.KeyColumns))
                oTable:ServerFilter        := SELF:_GetString(rdr, nameof(SqlRDDEventReason.ServerFilter))
                oTable:UpdateAllColumns    := SELF:_GetLogic(rdr, nameof(SqlRDDEventReason.UpdateAllColumns))
                var cIndexes               := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Indexes))
                rdr:Close()
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
        endif
        cache:Add(cTable, oTable)
        RETURN oTable
    end method

    /// <inheritdoc />
    OVERRIDE METHOD GetIndexInfo(oTable as SqlTableInfo, cIndexName as STRING) AS SqlIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var sql := i"SELECT * FROM [{IndexDictionary}] WHERE {nameof(TableName)} = '{oTable.Name}' "+ ;
            i"and [{nameof(IndexName)}] = '{cIndexName}' Order by [Ordinal]"

        local rdr := Connection:ExecuteReader(sql) as DbDataReader
        var oIndex  := SqlIndexInfo{oTable, cIndexName}
        if rdr != null
            while rdr:Read()
                var oTag           := SqlIndexTagInfo{oIndex, SELF:_GetString(rdr, nameof(TagName))}
                oTag:Expression    := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Expression))
                oTag:Condition     := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Condition))
                oTag:Unique        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.Unique))
                oIndex:Tags:Add(oTag)
            enddo
            rdr:Close()
        endif
        return oIndex
    end method

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
