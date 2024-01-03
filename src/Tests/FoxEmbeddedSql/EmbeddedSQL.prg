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


FUNCTION __SqlAlterTable(sCommand as STRING) AS LOGIC
    var oContext := ParseSqlAlter(sCommand)
    if (oContext != NULL)
        RETURN TRUE
    endif
    RETURN FALSE


STATIC FUNCTION ParseSqlAlter(sCommand as STRING) AS FoxAlterTableContext
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    var table := parser:ParseAlterTable()
    ? sCommand
    return table






