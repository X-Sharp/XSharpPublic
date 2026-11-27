// TestSupport.prg
USING System
USING System.Diagnostics
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Parsers
USING XSharp.RDD.Enums
USING XSharp.Data
USING XSharp.Data.Query
USING XSharp.VFP

FUNCTION InsertTemporalKeyRow(aliasName AS STRING, id AS INT, dateValue AS USUAL, ;
    timestampValue AS USUAL, logicalValue AS USUAL) AS VOID
    DbSelectArea(aliasName)
    DbAppend()
    FieldPut(1, id)
    FieldPut(2, dateValue)
    FieldPut(3, timestampValue)
    FieldPut(4, logicalValue)
    RETURN

FUNCTION PrintTable() AS DWORD
    LOCAL count := 0 AS DWORD

    IF Verbose
        ? "RECORDS:"
        LOCAL resultTable AS STRING
        LOCAL fields AS ARRAY
        resultTable = DbDataSource():Name
        fields := DbStruct()
        GO TOP
        DO WHILE !EOF()
            count++
            ? "  ", "| Rec:", count:ToString(), "| "
            FOREACH VAR f IN fields
                ?? f[1]+":", &(resultTable+"->"+f[1]), "| "
            NEXT
            SKIP
        ENDDO
    ENDIF

    RETURN count

FUNCTION VerifyTable(expectedRecords AS DWORD, message AS STRING, closeTable := FALSE AS LOGIC, expectedData := NULL AS ARRAY) AS VOID
    VAR  actualRecords := RecCount()
    PrintTable()
    IF expectedRecords != actualRecords
        IF closeTable
            DbCloseArea()
        ENDIF
        THROW Exception{message + " (Expected records: " + expectedRecords:ToString() + ", Got: " + actualRecords:ToString() + ")"}
    ENDIF
    IF expectedData != NULL
        LOCAL fieldCount := FCount() AS DWORD
        GO TOP
        FOR VAR row := 1 TO expectedData:Count
            IF EOF()
                IF closeTable
                    DbCloseArea()
                ENDIF
                THROW Exception{message + " (Expected row " + row:ToString() + " but reached end of file)"}
            ENDIF
            LOCAL aRow := expectedData[row] AS ARRAY
            FOR VAR col := 1 TO aRow:Count
                IF col > fieldCount
                    IF closeTable
                        DbCloseArea()
                    ENDIF
                    THROW Exception{message + " (Row " + row:ToString() + ", Field " + col:ToString() + ": Expected field does not exist)"}
                ENDIF
                LOCAL expectedValue := aRow[col]
                LOCAL actualValue := FieldGet(col)
                IF !ValueSame(expectedValue, actualValue)
                    IF closeTable
                        DbCloseArea()
                    ENDIF
                    LOCAL expectedText := "<NULL>" AS STRING
                    LOCAL actualText := "<NULL>" AS STRING
                    IF expectedValue != NULL
                        expectedText := expectedValue:ToString()
                    ENDIF
                    IF actualValue != NULL
                        actualText := actualValue:ToString()
                    ENDIF
                    THROW Exception{message + " (Row " + row:ToString() + ", Field " + col:ToString() + ": Expected '" + expectedText + "', Got '" + actualText + "')"}
                ENDIF
            NEXT
            SKIP
        NEXT
        IF !EOF()
            IF closeTable
                DbCloseArea()
            ENDIF
            THROW Exception{message + " (Unexpected additional records in table)"}
        ENDIF
    ENDIF
    IF closeTable
        DbCloseArea()
    ENDIF
    RETURN

FUNCTION VerifyTable(expectedRecords AS DWORD, message AS STRING, closeTable := TRUE AS LOGIC) AS VOID STRICT
    LOCAL actualCount AS DWORD
    actualCount := RecCount()

    IF actualCount != expectedRecords
        ? "FAILED:", message
        ? "  Expected:", expectedRecords, "records"
        ? "  Actual:", actualCount, "records"
        THROW Exception{message + " Expected: " + expectedRecords:ToString() + " Actual: " + actualCount:ToString()}
    ENDIF

    IF Verbose
        ? "PASSED: Expected", expectedRecords, "records - Got", actualCount
    ENDIF

    IF closeTable
        DbCloseArea()
    ENDIF
RETURN
FUNCTION ValueSame(value1 AS USUAL, value2 AS USUAL) AS LOGIC
    IF IsNull(value1) .OR. IsNull(value2)
        RETURN IsNull(value1) .AND. IsNull(value2)
    ENDIF
    IF IsNumericValue(value1) .AND. IsNumericValue(value2)
        RETURN value1 == value2
    ENDIF
    // For nonnumeric values, ignore type-specific formatting and padding.
    RETURN AllTrim(value1:ToString()) == AllTrim(value2:ToString())

FUNCTION IsNumericValue(value AS USUAL) AS LOGIC
    SWITCH UsualType(value)
    CASE __UsualType.Long
    CASE __UsualType.Float
    CASE __UsualType.Fixed
    CASE __UsualType.Byte
    CASE __UsualType.ShortInt
    CASE __UsualType.Word
    CASE __UsualType.DWord
    CASE __UsualType.Real4
    CASE __UsualType.Real8
    CASE __UsualType.Int64
    CASE __UsualType.UInt64
    CASE __UsualType.Decimal
    CASE __UsualType.Currency
        RETURN TRUE
    OTHERWISE
        RETURN FALSE
    END SWITCH

FUNCTION TestValueComparison() AS VOID
    IF !ValueSame(100.50, 100.5)
        THROW Exception{"Equivalent numeric values should compare equal"}
    ENDIF
    IF ValueSame(100.50, 100.51)
        THROW Exception{"Different numeric values should not compare equal"}
    ENDIF
    RETURN

FUNCTION __SqlDelete (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    IF Verbose
        ? sCommand
        IF ! parser:ParseDeleteStatement(out var table)
            ? parser:Error
        ENDIF
        ? table:TableName
        foreach var t in table:TableList
            ? t
        next
        foreach var j in table:JoinList
            ? j
        next
        ? table:WhereClause
    ENDIF
    RETURN

FUNCTION __SqlUpdate (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    VAR result := parser:ParseUpdateStatement(out var table)
    IF Verbose
        ? sCommand
        IF ! result
            ? parser:Error
        ENDIF
        ? "TableName", table:TableName
        foreach var c in table:ColumnList
            ? "Column", c
        next
        foreach var t in table:ValueList
            ? "Value", t:ToString()
        next
        foreach var t in table:TableList
            ? "From Table", t
        next
        foreach var j in table:JoinList
            ? "Join Table", j
        next
        ? "Where", table:WhereClause:ToString()
    ENDIF

    VAR o := XSharp.MacroCompiler.MacroOptions.FoxPro
    o:AllowOldStyleAssignments := False
    VAR mc := XSharp.Runtime.MacroCompiler{o}
    // VAR mc := XSharp.Runtime.MacroCompiler.GetScriptCompiler(XSharpDialect.FoxPro)
    VAR cbWhere := mc:CompileCodeblock(table:WhereClause:ToString())
    //VAR res := (OBJECT) cb:Eval(args)

    VAR values := Dictionary<STRING,CODEBLOCK>{}
    FOR VAR i := 0 TO table:ColumnList:Count-1
        values[table:ColumnList[i]] := mc:CompileCodeblock(table:ValueList[i]:ToString())
    NEXT

    LOCAL FUNCTION UpdateRow() AS VOID
        FOREACH VAR item IN values
            __FieldSet(item:key, item:value:Eval())
        NEXT
    END FUNCTION

    LOCAL FUNCTION FilterRow() AS LOGIC
        FOREACH VAR t IN table:TableList
            IF (t)->DbLocate( cbWhere )
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE
    END FUNCTION

    (table:TableName)->(DbEval( {||DbAutoLock(), UpdateRow(), DbAutoUnLock()}, {||FilterRow()} ))

    RETURN

FUNCTION Title(title AS STRING) AS VOID
    IF Verbose
        ? ""
        ? "===", title, "==="
    ENDIF

