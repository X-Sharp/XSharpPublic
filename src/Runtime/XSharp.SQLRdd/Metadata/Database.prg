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
    private tableDict       as string
    private tableColumn     as string
    private indexDict       as string
    private indexColumn       as string


    /// <summary>
    /// Create a new instance of the DatabaseMetadataProvider class.
    /// </summary>
    /// <param name="conn">Connection associated with the provider</param>

    CONSTRUCTOR(conn as SqlDbConnection)
        SUPER(conn)
        tableDict   := SELF:Connection:Provider:QuoteIdentifier(TableDictionary)
        tableColumn := SELF:Connection:Provider:QuoteIdentifier(nameof(TableName))
        indexDict   := SELF:Connection:Provider:QuoteIdentifier(IndexDictionary)
        indexColumn := SELF:Connection:Provider:QuoteIdentifier(nameof(IndexName))
        RETURN

    private _tablecols as List<MetaFieldInfo>
    PRIVATE METHOD TableFields() as List<MetaFieldInfo>
        if _tablecols == null
            _tablecols := List<MetaFieldInfo>{}
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{TableName,"C", 50,0},""})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.RealName),"C", 255,0},""})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.LongFieldNames),"L", 1,0},_connection:LongFieldNames})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.AllowUpdates),"L", 1,0},_connection:AllowUpdates})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.UpdateAllColumns),"L", 1,0},_connection:UpdateAllColumns})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.MaxRecords),"N", 10,0},_connection:MaxRecords})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.RecnoColumn),"C", 50,0},_connection:RecnoColumn})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.DeletedColumn),"C", 50,0},_connection:DeletedColumn})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.TrimTrailingSpaces),"L", 1,0},_connection:TrimTrailingSpaces})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.CompareMemo),"L", 1,0},_connection:CompareMemo})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.MaxRecnoAsRecCount),"L", 1,0},_connection:MaxRecnoAsRecCount})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.UpdatableColumns),"C", 255,0},DEFAULT_UPDATABLECOLUMNS})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.ColumnList),"C", 255,0},DEFAULT_COLUMNLIST})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.KeyColumns),"C", 255,0},DEFAULT_KEYCOLUMNS})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.ServerFilter),"C", 255,0},DEFAULT_SERVERFILTER})
            _tablecols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Indexes),"C", 250,0},DEFAULT_INDEXES})
        endif
        RETURN _tablecols
    END METHOD
    private _indexcols as List<MetaFieldInfo>
    PRIVATE METHOD IndexFields() AS List<MetaFieldInfo>
        if _indexcols == null
            _indexcols := List<MetaFieldInfo>{}
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{TableName         ,"C", 50,0},""})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{IndexName         ,"C", 50,0},""})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{Ordinal           ,"N", 10,0},0})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{TagName           ,"C", 50,0},""})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Expression)   ,"C", 250,0},DEFAULT_EXPRESSION})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Condition)    ,"C", 250,0},DEFAULT_CONDITION})
            _indexcols:Add(MetaFieldInfo{RddFieldInfo{nameof(SqlRDDEventReason.Unique)       ,"L", 1,0},DEFAULT_UNIQUE})
        endif
        RETURN _indexcols
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

    PRIVATE METHOD CreateTableDict() AS VOID
        var sb  := StringBuilder{}
        var prov := Connection:Provider
        sb:Clear()
        sb:Append(prov:DropTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, tableDict)
        using var cmd := SqlDbCommand{"Metadata", Connection, false}
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery()
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
        sb:Replace(SqlDbProvider.TableNameMacro, tableDict)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery()
        // create default values
        sb:Clear()
        sb:Append(prov:InsertStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, tableDict)
        sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
        sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery()
        RETURN

    PRIVATE METHOD DoesTableExistInDictionary(cTable as string) as LOGIC
        cTable      := Connection:Provider:CaseSync(cTable)
        using var cmd := SqlDbCommand{"Metadata", Connection, false}
        cmd:CommandText := i"SELECT COUNT(*) FROM {tableDict} WHERE {tableColumn} = @p1"
        cmd:AddParameter("@p1", cTable)
        var count := cmd:ExecuteScalar(cmd:Name)
        return Convert.ToInt64(count) > 0

    PRIVATE METHOD AddTableToDictionary(cTable AS String)  AS LOGIC
        local result := FALSE AS LOGIC
        TRY
            var sb          := StringBuilder{}
            var prov        := Connection:Provider
            var cols        := TableFields()
            var colNames    := CreateColumnNames(cols)
            var fieldValues := CreateDefaultColumnValues(cols, cTable)
            using var cmd := SqlDbCommand{"Metadata", Connection, false}
            sb:Append(prov:InsertStatement)
            sb:Replace(SqlDbProvider.TableNameMacro, tableDict)
            sb:Replace(SqlDbProvider.ColumnsMacro, colNames)
            sb:Replace(SqlDbProvider.ValuesMacro, fieldValues)
            cmd:CommandText := sb:ToString()
            cmd:ExecuteNonQuery(cmd:Name)
            result := TRUE
        catch e as Exception
            self:Connection:LastException := e
        end try
        return result

    PRIVATE METHOD FillExistingTables() as VOID
        var tables  := Connection:GetTables()
        foreach var table in tables
            if table  != TableDictionary .and. table != IndexDictionary
                if !DoesTableExistInDictionary(table)
                    AddTableToDictionary(table)
                endif
            endif
        next

    PRIVATE METHOD CreateIndexDict() as VOID
        var sb  := StringBuilder{}
        var prov := Connection:Provider
        var cols := IndexFields()
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
        sb:Clear()
        sb:Append(prov:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro, indexDict)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, fieldList)
        using var cmd := SqlDbCommand{"Metadata", Connection, false}
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery()


    PRIVATE METHOD CreateDictionary() as VOID
        SELF:CreateTableDict()
        SELF:FillExistingTables()
        SELF:CreateIndexDict()
        RETURN
    end method

    PRIVATE METHOD ReadDefaults() AS VOID
        if ! hasDefaults
            using var cmd := SqlDbCommand{"ReadDefaults", Connection, false}
            // Check if the table dictionary exists
            IF !Connection:DoesTableExist(TableDictionary)
                SELF:CreateDictionary()
            ELSEIF !Connection:DoesTableExist(IndexDictionary)
                SELF:CreateDictionary()
            ELSE
                // Check if the table dictionary has at least one record

                cmd:CommandText := i"SELECT COUNT(*) FROM {tableDict}"
                var count := cmd:ExecuteScalar()
                if Convert.ToInt64(count) == 0
                    SELF:CreateDictionary()
                endif
                cmd:CommandText := i"Select maxrecnoasreccount from {tableDict}"
                local ok := false as logic

                ok := cmd:ExecuteNonQuery(tableDict)
                if ! ok
                    var oInfo := RddFieldInfo{nameof(SqlRDDEventReason.MaxRecnoAsRecCount),"L", 1,0}
                    var colInfo := Connection:Provider:GetSqlColumnInfo(oInfo, Connection)
                    cmd:CommandText := i"alter table {tableDict} add "+colInfo
                    ok := cmd:ExecuteNonQuery(tableDict)
                    if ok
                        colInfo := Connection:Provider:QuoteIdentifier(oInfo:Name)
                        cmd:CommandText := i"update {tableDict} set "+colInfo+" = "+Connection:Provider:FalseLiteral
                        ok := cmd:ExecuteNonQuery(tableDict)
                    endif
                endif
            ENDIF
            // Read the defaults from the database
            cmd:AddParameter("@p1",DefaultSection)
            cmd:CommandText := i"SELECT * FROM {tableDict} WHERE {nameof(TableName)} = @p1"
            local rdr := cmd:ExecuteReader("Metadata") as DbDataReader
            if rdr != null
                if rdr:Read() .and. rdr.HasRows
                    _connection:LongFieldNames      := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.LongFieldNames),_connection:LongFieldNames )
                    _connection:AllowUpdates        := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.AllowUpdates),_connection:AllowUpdates)
                    _connection:MaxRecords          := SELF:_GetNumber(rdr, nameof(SqlRDDEventReason.MaxRecords), _connection:MaxRecords)
                    _connection:RecnoColumn         := SELF:_GetString(rdr, nameof(SqlRDDEventReason.RecnoColumn), _connection:RecnoColumn )
                    _connection:DeletedColumn       := SELF:_GetString(rdr, nameof(SqlRDDEventReason.DeletedColumn), _connection:DeletedColumn )
                    _connection:TrimTrailingSpaces  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.TrimTrailingSpaces),_connection:TrimTrailingSpaces)
                    _connection:CompareMemo         := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.CompareMemo),_connection:CompareMemo)
                    _connection:UpdateAllColumns    := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.UpdateAllColumns), _connection:UpdateAllColumns)
                    _connection:MaxRecnoAsRecCount  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.MaxRecnoAsRecCount), _connection:MaxRecnoAsRecCount)
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
        if !SELF:DoesTableExistInDictionary(cTable)
            SELF:AddTableToDictionary(cTable)
        endif
        oTable   := SqlDbTableInfo{cTable, Connection}
        var tbl  := SELF:Connection:Provider:CaseSync(cTable)
        var sql  := i"SELECT * FROM {tableDict} WHERE {nameof(TableName)} = @p1"
        using var cmd := SqlDbCommand{"ReadDefaults", Connection, false}
        cmd:CommandText := sql
        cmd:AddParameter("@p1",tbl)
        local rdr := cmd:ExecuteReader() as DbDataReader
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
                oTable:MaxRecnoAsRecCount  := SELF:_GetLogic (rdr, nameof(SqlRDDEventReason.MaxRecnoAsRecCount),_connection:MaxRecnoAsRecCount)
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
        var tbl   := SELF:Connection:Provider:CaseSync(oTable.Name)
        var ind   := SELF:Connection:Provider:CaseSync(cIndexName)
        var ord   := SELF:Connection:Provider:CaseSync(nameof(Ordinal))

        using var cmd := SqlDbCommand{"Metadata", Connection, false}
        cmd:CommandText := i"SELECT * FROM {indexDict} WHERE {tableColumn} = @p1 and {indexColumn} = @p2 Order by {ord}"
        cmd:AddParameter("@p1",tbl)
        cmd:AddParameter("@p2",ind)
        var rdr := cmd:ExecuteReader()
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

    /// <inheritdoc />
    OVERRIDE METHOD CreateTable(cTable AS STRING, info as DbOpenInfo) AS VOID
        SELF:ReadDefaults()
        if !SELF:DoesTableExistInDictionary(cTable)
            SELF:AddTableToDictionary(cTable)
        endif
        RETURN
    /// <inheritdoc />
    /// <remarks>The abstract implementation does nothing.</remarks>
    OVERRIDE METHOD CreateIndex(cTable as String, orderInfo as DbOrderCreateInfo) AS VOID
        SELF:ReadDefaults()
        if !SELF:DoesTableExistInDictionary(cTable)
            SELF:AddTableToDictionary(cTable)
        endif
        // check to see if the tag exists in the dictionary
        cTable      := Connection:Provider:CaseSync(cTable)
        var cIndex  := Connection:Provider:CaseSync(orderInfo:BagName)
        if String.IsNullOrEmpty(cIndex)
            cIndex := cTable
        else
            cIndex := System.IO.Path.GetFileNameWithoutExtension(cIndex)
        endif
        LOCAL cTag  := cIndex as string
        if orderInfo:Order is string var strOrder
            cTag := strOrder
        endif
        var cWhere   := i" {tableColumn} = @p1 AND {indexColumn} = @p2"

        using var cmd := SqlDbCommand{"CreateIndex", Connection, false}
        var sql     := i"SELECT COUNT(*) FROM {indexDict} WHERE "+ cWhere + i" AND {nameof(TagName)} = @p3"
        cmd:CommandText := sql
        cmd:ClearParameters()
        cmd:CommandText := sql
        cmd:AddParameter("@p1",cTable)
        cmd:AddParameter("@p2",cIndex)
        cmd:AddParameter("@p3",cTag)

        var count := cmd:ExecuteScalar(cTable)
        var sb := StringBuilder{}
        if Convert.ToInt64(count)  == 0
            // fetch all the rows for the index to determine the ordinal
            var ord     := Connection:Provider:QuoteIdentifier(nameof(Ordinal))
            var cSelect := i"Select {ord} from " + indexDict + " where " + cWhere
            cmd:CommandText := cSelect
            cmd:ClearParameters()
            cmd:AddParameter("@p1",cTable)
            cmd:AddParameter("@p2",cIndex)

            var tbl     := cmd:GetDataTable(cTable)
            var nOrdinal := 0
            foreach row as DataRow in tbl:Rows
                nOrdinal := Math.Max(nOrdinal, (Int32) row:Item[0])
            next
            nOrdinal++
            // create insert command
            var cols := IndexFields()
            var first := TRUE
            var sbValues   := StringBuilder{}
            local iCounter := 1 as long
            sb:Append(i"INSERT INTO {indexDict} (")
            foreach var col in cols
                if first
                    first := FALSE
                else
                    sb:Append(", ")
                    sbValues:Append(", ")
                endif
                sb:Append(Connection:Provider:QuoteIdentifier(col:FieldInfo:Name))
                var name := i"@p{iCounter}"
                iCounter++
                sbValues:Append(name)
            next
            sb:Append(") VALUES (")
            sb:Append(sbValues:ToString())
            sb:Append(")")
            cmd:CommandText := sb:ToString()
            cmd:ClearParameters()
            cmd:AddParameter("@p1", cTable)
            cmd:AddParameter("@p2", cIndex)
            cmd:AddParameter("@p3", nOrdinal)
            cmd:AddParameter("@p4", cTag)
            cmd:AddParameter("@p5", orderInfo:Expression)
            if orderInfo:OrdCondInfo != null
                cmd:AddParameter("@p6", orderInfo:OrdCondInfo:ForExpression)
            else
                cmd:AddParameter("@p6", "")
            endif
            cmd:AddParameter("@p7", orderInfo:Unique)
            cmd:ExecuteNonQuery()
        endif
        // Now check to see if the indexes field in the tableinfo table already contains the index
        sb:Clear()

        var indexesColumn := Connection:Provider:QuoteIdentifier(nameof(SqlRDDEventReason.Indexes))
        sb:Append(i"select {indexesColumn} from {tableDict} where {tableColumn} = @p1")
        cmd:ClearParameters()
        cmd:CommandText := sb:ToString()
        cmd:AddParameter("@p1",cTable)

        var result := cmd:ExecuteScalar(cTable)
        var update := false
        var newValue := cIndex
        if result is string var strResult
            if !strResult:Contains(cIndex)
                // Add the index to the tableinfo table
                if !String.IsNullOrEmpty(strResult)
                    newValue := strResult + "," +cIndex
                endif
                update := true
            else
                update := false
            endif
        else
            update := true
        endif
        if update
            cmd:CommandText := i"update {tableDict} set {indexesColumn} = @p1 where {tableColumn} = @p2"
            cmd:ClearParameters()
            cmd:AddParameter("@p1",newValue)
            cmd:AddParameter("@p2",cTable)
            cmd:ExecuteNonQuery(cTable)

        endif

        RETURN


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
