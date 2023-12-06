// EmbeddedSQL.prg
// Created by    : robert
// Creation Date : 11/21/2023 1:33:05 PM
// Created for   :
// WorkStation   : NYX
#pragma options("memvar", off)
USING SYstem.Collections.Generic
USING SYstem.Text
USING SYstem.Linq
using SYstem.Diagnostics
using XSharp.RDD.Support
using XSharp.RDD.Enums
using XSharp.Parsers
using XSharp.Internal



FUNCTION __SqlCreateCursor(sCommand as STRING) AS LOGIC
    var oContext := ParseSqlCreate(sCommand, TRUE)
    if (oContext != NULL)
        RETURN __CreateTableCursor(oContext)
    endif
    RETURN FALSE


FUNCTION __SqlCreateTable(sCommand as STRING) AS LOGIC
    // FoxPro creates the table and keeps it open
    var oContext := ParseSqlCreate(sCommand, FALSE)
    if (oContext != NULL)
        RETURN __CreateTableCursor(oContext)
    endif
    RETURN FALSE

STATIC FUNCTION ParseSqlCreate(sCommand as STRING, lCursor as LOGIC) AS FoxCreateTableContext
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    local table as FoxCreateTableContext
    IF lCursor
        IF ! parser:ParseCreateCursor(out table)
            return null
        ENDIF
    ELSE
        IF ! parser:ParseCreateTable(out  table)
            return null
        ENDIF
    endif
    return table



STATIC FUNCTION __CreateTableCursor(oTable as FoxCreateTableContext) AS LOGIC
    if oTable != NULL
        local aStruct as ARRAY
        aStruct := {}
        foreach col as FoxCreateColumnContext in oTable:Columns
            var aField := {col:Name, ((Char)col:FieldType):ToString(), col:Length, col:Decimals, col:Name, col:Flags}
            AAdd(aStruct, aField)
        next
        VAR cTable := oTable:Name
        if oTable:IsCursor
            cTable := SYstem.IO.Path.GetTempFileName()
        ENDIF
        DbCreate(cTable, aStruct, "DBFVFP", TRUE, oTable:Name)
        DbCloseArea(oTable:Name)
        DbUseArea(TRUE, "DBFVFP", cTable, oTable:Name, FALSE, FALSE)
        FOR var nI := 1 to oTable:Columns:Count
            var oCol := oTable:Columns[nI-1]
            DbFieldInfo(DBS_CAPTION, nI, oCol:Caption)
        NEXT
        if oTable:IsCursor
            DBInfo(DBI_ISTEMPORARY, TRUE)
        endif
        RETURN TRUE
    endif
    RETURN FALSE
