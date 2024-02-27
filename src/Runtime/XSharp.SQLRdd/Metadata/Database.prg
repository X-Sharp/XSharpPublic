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
/// The SqlMetadataProviderDatabase class. Reads Metadata from tables in the database.
/// </summary>
/// <remarks>
/// It uses the following tables (when they exist)
/// - xs_tableInfo
/// - xs_indexInfo
/// - xs_defaults
/// </remarks>
CLASS SqlMetadataProviderDatabase INHERIT SqlMetadataProviderAbstract
    internal class MetaFieldInfo
        internal property FieldInfo as RddFieldInfo AUTO
        internal property Value  as object AUTO
        constructor(oField as RddFieldInfo, oValue as object)
            FieldInfo := oField
            Value := oValue
        end constructor
    end class

    private hasDefaults as logic

    /// <summary>
    /// Create a new instance of the DatabaseMetadataProvider class.
    /// </summary>
    /// <param name="conn">Connection associated with the provider</param>

    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        RETURN

    PRIVATE METHOD TableFields() as List<MetaFieldInfo>
        var cols := List<MetaFieldInfo>{}
        cols:Add(MetaFieldInfo{RddFieldInfo{TableName,"C", 50,0},""})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.RealName),"C", 255,0},""})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.LongFieldNames),"L", 1,0},_connection:LongFieldNames})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.AllowUpdates),"L", 1,0},_connection:AllowUpdates})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.UpdateAllColumns),"L", 1,0},_connection:UpdateAllColumns})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.MaxRecords),"N", 10,0},_connection:MaxRecords})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.RecnoColumn),"C", 50,0},_connection:RecnoColumn})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.DeletedColumn),"C", 50,0},_connection:DeletedColumn})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.TrimTrailingSpaces),"L", 1,0},_connection:TrimTrailingSpaces})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.CompareMemo),"L", 1,0},_connection:CompareMemo})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.UpdatableColumns),"C", 255,0},DEFAULT_UPDATABLECOLUMNS})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.ColumnList),"C", 255,0},DEFAULT_COLUMNLIST})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.KeyColumns),"C", 255,0},DEFAULT_KEYCOLUMNS})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.ServerFilter),"C", 255,0},DEFAULT_SERVERFILTER})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Indexes),"C", 250,0},DEFAULT_INDEXES})
        RETURN cols
    END METHOD

    PRIVATE METHOD IndexFields() AS List<MetaFieldInfo>
        var cols := List<MetaFieldInfo>{}
        cols:Add(MetaFieldInfo{RddFieldInfo{TableName         ,"C", 50,0},""})
        cols:Add(MetaFieldInfo{RddFieldInfo{IndexName         ,"C", 50,0},""})
        cols:Add(MetaFieldInfo{RddFieldInfo{Ordinal           ,"N", 10,0},0})
        cols:Add(MetaFieldInfo{RddFieldInfo{TagName           ,"C", 50,0},""})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Expression)   ,"C", 250,0},DEFAULT_EXPRESSION})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Condition)    ,"C", 250,0},DEFAULT_CONDITION})
        cols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Unique)       ,"L", 1,0},DEFAULT_UNIQUE})
        RETURN cols
    END METHOD

    PRIVATE METHOD CreateDefaultColumnValues(cols as List<MetaFieldInfo>, TableName as string) as string
        var sb  := StringBuilder{}
        var first := TRUE
        foreach var col in cols
            var fi := col:FieldInfo
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            if fi:Name == nameof(TableName)
                sb:Append(i"'{TableName}'")
            elseif col:Value is string var strValue
                sb:Append(i"'{strValue}'")
            elseif col:Value is logic var logValue
                sb:Append( iif(logValue, 1, 0))
            else
                sb:Append(col:Value)
            endif
        next
        return sb:ToString()
    end method

    PRIVATE METHOD CreateColumnNames(cols as List<MetaFieldInfo>) as string
        var sb  := StringBuilder{}
        var first := TRUE
        foreach var col in cols
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            sb:Append(col:FieldInfo:Name)
        next
        return sb:ToString()
    end method

    PRIVATE METHOD CreateDictionary() as VOID
        var sb  := StringBuilder{}
        var prov := Connection:Provider

        sb:Clear()
        sb:Append(prov:DropTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        Connection:ExecuteNonQuery(sb:ToString(), "Metadata")
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
            sb:Append(prov:GetSqlColumnInfo(col:FieldInfo, SELF:Connection))
        next
        var fieldList := sb:ToString()
        var fieldValues := CreateDefaultColumnValues(cols, DefaultSection)
        var colNames := CreateColumnNames(cols)
        sb:Clear()
        sb:Append(prov:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        Connection:ExecuteNonQuery(sb:ToString(),"Metadata")
        // create default values
        sb:Clear()
        sb:Append(prov:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, TableDictionary)
        sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
        sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
        Connection:ExecuteNonQuery(sb:ToString(),"Metadata")

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
                Connection:ExecuteNonQuery(sb:ToString(),"Metadata")
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
            sb:Append(prov:GetSqlColumnInfo(col:FieldInfo, SELF:Connection))
        next
        fieldList := sb:ToString()
        sb:Clear()
        sb:Append(prov:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, IndexDictionary)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        Connection:ExecuteNonQuery(sb:ToString(), "Metadata")
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
                var count := Connection:ExecuteScalar(sql1, "Metadata")
                if Convert.ToInt64(count) == 0
                    SELF:CreateDictionary()
                endif
            ENDIF
            // Read the defaults from the database
            var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{DefaultSection}'"
            local rdr := Connection:ExecuteReader(sql, "Metadata") as DbDataReader
            if rdr != null
                if rdr:Read()
                    _connection:LongFieldNames      := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.LongFieldNames),_connection:LongFieldNames )
                    _connection:AllowUpdates        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.AllowUpdates),_connection:AllowUpdates)
                    _connection:MaxRecords          := SELF:_GetNumber(rdr, nameof(SqlRDDEventReason.MaxRecords), _connection:MaxRecords)
                    _connection:RecnoColumn         := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RecnoColumn), _connection:RecnoColumn )
                    _connection:DeletedColumn       := SELF:_GetString(rdr, nameof(SqlRDDEventReason.DeletedColumn), _connection:DeletedColumn )
                    _connection:TrimTrailingSpaces  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.TrimTrailingSpaces),_connection:TrimTrailingSpaces)
                    _connection:CompareMemo         := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.CompareMemo),_connection:CompareMemo)
                    _connection:UpdateAllColumns    := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.UpdateAllColumns), _connection:UpdateAllColumns)
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

    private method _GetString(rdr as DbDataReader, cName as string, strDefault as string) as string
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                return rdr:GetString(pos):Trim()
            endif
        catch
            return strDefault:Trim()
        end try
        return strDefault
    end method

    private method _GetNumber(rdr as DbDataReader, cName as string, nDefault as int) as int
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                var num := rdr:GetDecimal(pos)
                return Convert.ToInt32(num)
            endif
        catch
            return nDefault
        end try
        return nDefault
    end method

    private method _GetLogic(rdr as DbDataReader, cName as string, lDefault as LOGIC) as logic
        try
            var pos := SELF:_GetPos(rdr, cName)
            if pos >= 0
                return rdr:GetBoolean(pos)
            endif
        catch
            return lDefault
        end try
        return lDefault
    end method

    /// <inheritdoc />
    OVERRIDE METHOD GetTableInfo(cTable as STRING) AS SqlDbTableInfo
        local oTable as SqlDbTableInfo
        SELF:ReadDefaults()

        if SELF:FindInCache(cTable, out oTable)
            return oTable
        endif
        oTable := SqlDbTableInfo{cTable, Connection}
        var sql := i"SELECT * FROM [{TableDictionary}] WHERE {nameof(TableName)} = '{cTable}'"
        local rdr := SELF:Connection:ExecuteReader(sql, "Metadata") as DbDataReader
        if rdr != null
            if rdr:Read()
                oTable:RealName            := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RealName),cTable)
                oTable:LongFieldNames      := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.LongFieldNames), _connection:LongFieldNames)
                oTable:AllowUpdates        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.AllowUpdates), _connection:AllowUpdates)
                oTable:MaxRecords          := SELF:_GetNumber(rdr, nameof(SqlRDDEventReason.MaxRecords),_connection:MaxRecords)
                oTable:RecnoColumn         := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RecnoColumn),_connection:RecnoColumn)
                oTable:DeletedColumn       := SELF:_GetString(rdr, nameof(SqlRDDEventReason.DeletedColumn),_connection:DeletedColumn)
                oTable:TrimTrailingSpaces  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.TrimTrailingSpaces),_connection:TrimTrailingSpaces)
                oTable:CompareMemo         := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.CompareMemo),_connection:CompareMemo)
                oTable:UpdateAllColumns    := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.UpdateAllColumns),_connection:UpdateAllColumns)
                oTable:UpdatableColumns    := SELF:_GetString(rdr, nameof(SqlRDDEventReason.UpdatableColumns),DEFAULT_UPDATABLECOLUMNS)
                oTable:KeyColumns          := SELF:_GetString(rdr, nameof(SqlRDDEventReason.KeyColumns),DEFAULT_KEYCOLUMNS)
                oTable:ServerFilter        := SELF:_GetString(rdr, nameof(SqlRDDEventReason.ServerFilter),DEFAULT_SERVERFILTER)
                var cIndexes               := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Indexes),DEFAULT_INDEXES)
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
            rdr:Close()
        endif
        SELF:AddToCache(cTable, oTable)
        RETURN oTable
    end method

    /// <inheritdoc />
    OVERRIDE METHOD GetIndexInfo(oTable as SqlDbTableInfo, cIndexName as STRING) AS SqlDbIndexInfo
        // Indexes are stored in a section TableName_IndexName
        var sql := i"SELECT * FROM [{IndexDictionary}] WHERE {nameof(TableName)} = '{oTable.Name}' "+ ;
            i"and [{nameof(IndexName)}] = '{cIndexName}' Order by [Ordinal]"

        local rdr := Connection:ExecuteReader(sql, "Metadata") as DbDataReader
        var oIndex  := SqlDbIndexInfo{oTable, cIndexName}
        if rdr != null
            while rdr:Read()
                var oTag           := SqlDbTagInfo{oIndex, SELF:_GetString(rdr, nameof(TagName),"")}
                oTag:Expression    := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Expression),DEFAULT_EXPRESSION)
                oTag:Condition     := SELF:_GetString(rdr, nameof(SqlRDDEventReason.Condition),DEFAULT_CONDITION)
                oTag:Unique        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.Unique), DEFAULT_UNIQUE)
                oIndex:Tags:Add(oTag)
            enddo
            rdr:Close()
        endif
        return oIndex
    end method

#region constants
    INTERNAL CONST TableDictionary   := "xs_tableinfo" as string
    INTERNAL CONST IndexDictionary   := "xs_indexinfo" as string
    INTERNAL CONST DefaultSection    := "defaults" as string
    INTERNAL CONST TableName         := nameof(TableName) as string
    INTERNAL CONST IndexName         := nameof(IndexName) as string
    INTERNAL CONST TagName           := nameof(TagName) as string
    INTERNAL CONST Ordinal           := nameof(Ordinal) as string

#endregion
END CLASS

END NAMESPACE // XSharp.SQLRdd.Metadata
