//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.Data
using XSharp.Parsers
using XSharp.Internal
using XSharp.RDD
using XSharp.RDD.Support
using XSharp.Data.Query
using System.Collections.Generic
using System.IO
using System.Linq
using System.Text
using System.Text.RegularExpressions
[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlDropTable(sTable AS STRING) AS LOGIC
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    LOCAL tableAlias := System.IO.Path.GetFileNameWithoutExtension(AllTrim(sTable)) AS STRING
    TRY
        IF Used(tableAlias)
            DbCloseArea(tableAlias)
        ENDIF
        IF !XSharp.RDD.DbcManager.RemoveTable(AllTrim(sTable), TRUE, FALSE)
            THROW Error{"Could not drop table: " + sTable}
        ENDIF
        RETURN TRUE
    FINALLY
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
    END TRY

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlCreateView(sCommand AS STRING) AS LOGIC
    LOCAL pattern := "^\s*CREATE\s+(?:SQL\s+)?VIEW\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)\s*(?<options>[\s\S]*?)\bAS\s+(?<select>SELECT\b[\s\S]*)$" AS STRING
    LOCAL regexResult AS System.Text.RegularExpressions.Match
    regexResult := System.Text.RegularExpressions.Regex.Match(sCommand, pattern, RegexOptions.IgnoreCase)
    IF !regexResult:Success
        THROW Error{"Invalid CREATE VIEW command: " + sCommand}
    ENDIF
    LOCAL options := regexResult:Groups["options"]:Value AS STRING
    IF Regex.IsMatch(options, "\b(REMOTE|CONNECTION|SHARE)\b", RegexOptions.IgnoreCase)
        THROW Error{"CREATE VIEW currently supports local views without REMOTE, CONNECTION, or SHARE options"}
    ENDIF
    LOCAL viewName := regexResult:Groups["name"]:Value AS STRING
    LOCAL selectSql := regexResult:Groups["select"]:Value:Trim() AS STRING
    VAR context := FoxEmbeddedSQL.ParseSqlSelect(selectSql)
    LOCAL fields := List<STRING>{} AS List<STRING>
    IF context:FieldSelectionList != NULL
        FOREACH VAR selection IN context:FieldSelectionList
            IF !String.IsNullOrWhiteSpace(selection:Alias)
                fields:Add(selection:Alias)
            ELSEIF selection:Expression IS SqlNameExpressionContext VAR nameExpression .AND. ;
                   !String.IsNullOrWhiteSpace(nameExpression:Name) .AND. nameExpression:Name != "*"
                fields:Add(nameExpression:Name)
            ELSE
                fields:Add(FoxEmbeddedSQL.SqlExpressionToFieldName(selection:Expression))
            ENDIF
        NEXT
    ENDIF
    IF !XSharp.RDD.DbcManager.AddView(viewName, selectSql, fields)
        THROW Error{"Could not create view: " + viewName}
    ENDIF
    RETURN TRUE

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlDropView(sView AS STRING) AS LOGIC
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    LOCAL viewAlias := AllTrim(sView) AS STRING
    TRY
        IF Used(viewAlias)
            DbCloseArea(viewAlias)
        ENDIF
        IF !XSharp.RDD.DbcManager.RemoveView(viewAlias)
            THROW Error{"Could not drop view: " + viewAlias}
        ENDIF
        RETURN TRUE
    FINALLY
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
    END TRY

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlUpdate(sCommand AS STRING) AS LOGIC
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    LOCAL updatedCount := 0 AS LONG
    LOCAL targetAlias := "" AS STRING
    LOCAL sourceAlias := "" AS STRING

    TRY
        VAR parser := SQLParser{XTokenList{XSqlLexer{sCommand}:AllTokens()}}
        IF !parser:ParseUpdateStatement(OUT VAR updateContext)
            THROW Error{"Could not parse UPDATE command: " + parser:Error}
        ENDIF
        IF updateContext:TableList:Count != 1 .OR. updateContext:JoinList:Count != 0
            THROW Error{"UPDATE FROM currently supports exactly one source table"}
        ENDIF
        IF updateContext:WhereClause == NULL
            THROW Error{"UPDATE FROM requires a WHERE join condition"}
        ENDIF

        targetAlias := AllTrim(updateContext:TableName)
        sourceAlias := AllTrim(updateContext:TableList[0])
        IF String.Equals(targetAlias, sourceAlias, StringComparison.OrdinalIgnoreCase)
            THROW Error{"UPDATE target and source must be different work areas"}
        ENDIF
        IF !FoxEmbeddedSQL.OpenArea(targetAlias) .OR. !FoxEmbeddedSQL.OpenArea(sourceAlias)
            THROW Error{"Could not open UPDATE target or source table"}
        ENDIF

        VAR aliases := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
        aliases[targetAlias] := targetAlias
        aliases[sourceAlias] := sourceAlias
        VAR whereExpression := updateContext:WhereClause:ToResolvedString(aliases)
        VAR whereBlock := MCompile(whereExpression)
        IF whereBlock == NULL
            THROW Error{"Could not compile UPDATE WHERE expression: " + whereExpression}
        ENDIF
        VAR assignments := Dictionary<STRING, CODEBLOCK>{StringComparer.OrdinalIgnoreCase}
        FOR LOCAL assignmentIndex := 0 AS INT TO updateContext:ColumnList:Count - 1
            VAR columnName := updateContext:ColumnList[assignmentIndex]
            IF (targetAlias)->FieldPos(columnName) == 0
                THROW Error{"UPDATE target column not found: " + columnName}
            ENDIF
            VAR assignmentExpression := updateContext:ValueList[assignmentIndex]:ToResolvedString(aliases)
            VAR assignmentBlock := MCompile(assignmentExpression)
            IF assignmentBlock == NULL
                THROW Error{"Could not compile UPDATE assignment: " + assignmentExpression}
            ENDIF
            assignments[columnName] := assignmentBlock
        NEXT

        DbSelectArea(targetAlias)
        LOCAL targetRowCount := (LONG)RecCount() AS LONG
        IF targetRowCount > 0
            DbGoTop()
            FOR LOCAL rowIndex := 1 AS LONG TO targetRowCount
                DbSelectArea(sourceAlias)
                DbGoTop()
                IF DbLocate(whereBlock)
                    DbSelectArea(targetAlias)
                    DbAutoLock()
                    FOREACH VAR assignment IN assignments
                        __FieldSet(assignment:Key, assignment:Value:Eval())
                    NEXT
                    DbAutoUnLock()
                    updatedCount++
                ELSE
                    DbSelectArea(targetAlias)
                ENDIF
                IF rowIndex < targetRowCount
                    DbSkip(1)
                ENDIF
            NEXT
        ENDIF

        RuntimeState.Tally := updatedCount
        _TALLY := updatedCount
        RETURN TRUE
    FINALLY
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
    END TRY

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlDelete(sCommand AS STRING) AS LOGIC
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    LOCAL deletedCount := 0 AS LONG
    LOCAL targetAlias := "" AS STRING
    LOCAL sourceAlias := "" AS STRING
    LOCAL isSubqueryDelete := FALSE AS LOGIC
    LOCAL deletePredicate := NULL AS SqlThreeValuePredicate
    LOCAL candidateRows AS List<LONG>
    LOCAL whereBlock := NULL AS CODEBLOCK

    TRY
        VAR parser := SQLParser{XTokenList{XSqlLexer{sCommand}:AllTokens()}}
        IF !parser:ParseDeleteStatement(OUT VAR deleteContext)
            THROW Error{"Could not parse DELETE command: " + parser:Error}
        ENDIF
        IF deleteContext:WhereClause == NULL
            THROW Error{"DELETE requires a WHERE condition"}
        ENDIF

        VAR aliases := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
        candidateRows := NULL
        IF String.IsNullOrWhiteSpace(deleteContext:TableName)
            IF deleteContext:TableList:Count != 1 .OR. deleteContext:JoinList:Count != 0
                THROW Error{"DELETE subquery currently supports one target table"}
            ENDIF
            isSubqueryDelete := TRUE
            targetAlias := AllTrim(deleteContext:TableList[0])
            IF !FoxEmbeddedSQL.OpenArea(targetAlias)
                THROW Error{"Could not open DELETE target table: " + targetAlias}
            ENDIF
            aliases[targetAlias] := targetAlias
            deletePredicate := FoxEmbeddedSQL.PrepareDeleteInPredicate( ;
                deleteContext:WhereClause, aliases, targetAlias, OUT candidateRows)
        ELSE
            IF deleteContext:JoinList:Count != 0
                THROW Error{"DELETE source-list currently requires comma-separated sources"}
            ENDIF
            targetAlias := AllTrim(deleteContext:TableName)
            LOCAL sourceCount := 0 AS INT
            FOREACH VAR tableName IN deleteContext:TableList
                IF !String.Equals(AllTrim(tableName), targetAlias, StringComparison.OrdinalIgnoreCase)
                    sourceCount++
                    sourceAlias := AllTrim(tableName)
                ENDIF
            NEXT
            IF sourceCount != 1
                THROW Error{"DELETE source-list currently supports exactly one source table besides the target"}
            ENDIF
            IF String.Equals(targetAlias, sourceAlias, StringComparison.OrdinalIgnoreCase)
                THROW Error{"DELETE target and source must be different work areas"}
            ENDIF
            IF !FoxEmbeddedSQL.OpenArea(targetAlias) .OR. !FoxEmbeddedSQL.OpenArea(sourceAlias)
                THROW Error{"Could not open DELETE target or source table"}
            ENDIF

            aliases[targetAlias] := targetAlias
            aliases[sourceAlias] := sourceAlias
            VAR whereExpression := deleteContext:WhereClause:ToResolvedString(aliases)
            whereBlock := MCompile(whereExpression)
            IF whereBlock == NULL
                THROW Error{"Could not compile DELETE WHERE expression: " + whereExpression}
            ENDIF
        ENDIF

        DbSelectArea(targetAlias)
        LOCAL targetRowCount := (LONG)RecCount() AS LONG
        LOCAL targetIterationCount := targetRowCount AS LONG
        IF candidateRows != NULL
            targetIterationCount := candidateRows:Count
        ENDIF
        FOR LOCAL targetIndex := 0 AS LONG TO targetIterationCount - 1
            LOCAL targetRecord := IIF(candidateRows != NULL, candidateRows[targetIndex], targetIndex + 1) AS LONG
            DbGoto(targetRecord, targetAlias)
            LOCAL matched := FALSE AS LOGIC
            IF isSubqueryDelete
                matched := deletePredicate:Evaluate() == 1
            ELSE
                DbSelectArea(sourceAlias)
                LOCAL sourceRowCount := (LONG)RecCount() AS LONG
                FOR LOCAL sourceRecord := 1 AS LONG TO sourceRowCount
                    DbGoto(sourceRecord, sourceAlias)
                    VAR matchValue := (USUAL)whereBlock:Eval()
                    IF !IsNil(matchValue) .AND. !matchValue:IsNull .AND. (LOGIC)matchValue
                        matched := TRUE
                        EXIT
                    ENDIF
                NEXT
            ENDIF

            IF matched
                DbSelectArea(targetAlias)
                DbGoto(targetRecord, targetAlias)
                __DbDelete()
                deletedCount++
            ENDIF
        NEXT

        RuntimeState.Tally := deletedCount
        _TALLY := deletedCount
        RETURN TRUE
    FINALLY
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
    END TRY

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlInsertMemVar(sTable as STRING) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // FoxPro only writes memvars that exist and also uses locals
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    local nMax    := FCount() as DWORD
    local nFld as DWORD
    FOR nFld := 1 to nMax
        var fName := FieldName(nFld)
        var uValue :=  XSharp.MemVar.GetSafe(fName)
        if uValue != nil
            FieldPut(nFld, FoxEmbeddedSQL.Convert(FieldGet(nFld), uValue))
        ENDIF
    NEXT
    DbUnLock()
    RETURN Used()


FUNCTION __SqlInsertFromObject(sTable as STRING, oValues as Object) AS Logic
    // FoxPro opens the table when needed and keeps it open
    // FoxPro enumerates the properties from the object and writes the properties that
    // have a name in the current area
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    if oValues IS XSharp.VFP.Empty VAR eValue
        var props := eValue:Properties
        foreach var prop in props
            var pos := FieldPos(prop:Name)
            IF pos > 0
                FieldPut(pos, FoxEmbeddedSQL.Convert(FieldGet(pos), prop:Value))
            endif
        next
    endif
    DbUnLock()
    RETURN Used()

FUNCTION __SqlInsertFromArray(sTable as STRING, aValues as ARRAY) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // FoxPro walks the array and assigns values
    // Note that FoxPro does "optimistic" type conversions
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    DbAppend()
    local nMax := Min(FCount(), ALen(aValues)) as DWORD
    local nFld as DWORD
    FOR nFld := 1 to nMax
        FieldPut(nFld, FoxEmbeddedSQL.Convert(FieldGet(nFld), aValues[nFld]))
    NEXT
    DbUnLock()
    RETURN Used()

FUNCTION __SqlInsertValues(sTable as STRING, aFields as ARRAY, aValues as ARRAY) AS LOGIC
    // FoxPro opens the table when needed and keeps it open
    // what does foxpro do when aValues > # of fields
    // what does foxpro do when aValues < # of fields
    local i as DWORD
    local nMax as DWORD
    IF ! FoxEmbeddedSQL.OpenArea(sTable)
        return FALSE
    ENDIF
    IF ALen(aFields) == 0 .and. ALen(aValues) > 0
        nMax := ALen(aValues)
        aFields := ArrayNew(ALen(aValues))
        FOR i := 1 to nMax
            aFields[i] := FieldName(i)
        NEXT
    ELSE
        nMax := Min(ALen(aFields), ALen(aValues))
    ENDIF
    DbAppend()
    FOR i := 1 to nMax
        __FieldSet(aFields[i], aValues[i])
    NEXT
    DbUnLock()
    RETURN TRUE

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlInsertFromSQL(cUdc AS STRING, sTable AS STRING, aFields AS ARRAY, sSelect AS STRING) AS LOGIC
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    LOCAL resultAlias := "SQLINSERT" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
    LOCAL resultPath := "" AS STRING
    LOCAL insertedCount := 0 AS LONG

    TRY
        VAR selectContext := FoxEmbeddedSQL.ParseSqlSelect(sSelect)
        IF selectContext == NULL
            THROW Error{"Invalid SELECT clause in INSERT command: " + sSelect}
        ENDIF
        selectContext:TargetCursor := resultAlias
        FoxEmbeddedSQL.ExecuteSelect(selectContext)
        IF !Used(resultAlias)
            THROW Error{"INSERT SELECT did not produce a result cursor"}
        ENDIF
        resultPath := (resultAlias)->DbInfo(DBI_FULLPATH)

        IF !FoxEmbeddedSQL.OpenArea(sTable)
            THROW Error{"Could not open INSERT target: " + sTable}
        ENDIF

        LOCAL resultColumnCount := (resultAlias)->FCount() AS DWORD
        LOCAL targetColumnCount := IIF(ALen(aFields) > 0, ALen(aFields), FCount()) AS DWORD
        IF resultColumnCount != targetColumnCount
            THROW Error{"The number of SELECT values doesn't match the number of INSERT columns"}
        ENDIF

        LOCAL targetPositions := DWORD[]{targetColumnCount} AS DWORD[]
        FOR LOCAL columnIndex := 1 AS DWORD TO targetColumnCount
            LOCAL fieldName := IIF(ALen(aFields) > 0, (STRING)aFields[columnIndex], FieldName(columnIndex)) AS STRING
            targetPositions[columnIndex] := FieldPos(fieldName)
            IF targetPositions[columnIndex] == 0
                THROW Error{"INSERT target column not found: " + fieldName}
            ENDIF
        NEXT

        LOCAL rowCount := (LONG)(resultAlias)->RecCount() AS LONG
        IF rowCount > 0
            (resultAlias)->DbGoTop()
            FOR LOCAL rowIndex := 1 AS LONG TO rowCount
                LOCAL values := USUAL[]{resultColumnCount} AS USUAL[]
                FOR LOCAL columnIndex := 1 AS DWORD TO resultColumnCount
                    values[columnIndex] := (resultAlias)->FieldGet(columnIndex)
                NEXT

                DbSelectArea(sTable)
                DbAppend()
                FOR LOCAL columnIndex := 1 AS DWORD TO resultColumnCount
                    LOCAL targetPosition := targetPositions[columnIndex] AS DWORD
                    LOCAL targetValue := FieldGet(targetPosition) AS USUAL
                    LOCAL sourceValue := values[columnIndex] AS USUAL
                    IF sourceValue:IsNull
                        FieldPut(targetPosition, NULL)
                    ELSE
                        FieldPut(targetPosition, FoxEmbeddedSQL.Convert(targetValue, sourceValue))
                    ENDIF
                NEXT
                DbUnLock()
                insertedCount++

                IF rowIndex < rowCount
                    (resultAlias)->DbSkip(1)
                ENDIF
            NEXT
        ENDIF
        RuntimeState.Tally := insertedCount
        _TALLY := insertedCount
        RETURN TRUE
    FINALLY
        IF Used(resultAlias)
            DbCloseArea(resultAlias)
        ENDIF
        IF !String.IsNullOrEmpty(resultPath)
            FOREACH VAR extension IN <STRING>{".DBF", ".FPT", ".CDX"}
                VAR filePath := Path.ChangeExtension(resultPath, extension)
                IF File.Exists(filePath)
                    File.Delete(filePath)
                ENDIF
            NEXT
        ENDIF
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
    END TRY

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlAlterTable(sCommand as STRING) AS LOGIC
    var oContext := FoxEmbeddedSQL.ParseSqlAlter(sCommand)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.SqlAlterTable(oContext)
    endif
    RETURN FALSE

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlCreateCursor(sCommand as STRING) AS LOGIC
    var oContext := FoxEmbeddedSQL.ParseSqlCreate(sCommand, TRUE)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.CreateTableCursor(oContext)
    endif
    RETURN FALSE

[NeedsAccessToLocals(FALSE)];
FUNCTION __SqlCreateTable(sCommand as STRING) AS LOGIC
    // FoxPro creates the table and keeps it open
    var oContext := FoxEmbeddedSQL.ParseSqlCreate(sCommand, FALSE)
    if (oContext != NULL)
        RETURN FoxEmbeddedSQL.CreateTableCursor(oContext)
    endif
    THROW Error{"Syntax error in command: "+sCommand}

[NeedsAccessToLocals(TRUE)];
FUNCTION __SqlSelect(sCommand as STRING) AS VOID
    LOCAL savedArea := VoDbGetSelect() AS DWORD
    TRY
        var oContext := FoxEmbeddedSQL.ParseSqlSelect(sCommand)
        if (oContext != NULL)
            IF !String.IsNullOrEmpty(oContext:TargetArray)
                LOCAL originalTargetCursor := oContext:TargetCursor AS STRING
                LOCAL arrayCursor := "SQLARRAY" + System.Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
                LOCAL cursorPath := "" AS STRING
                TRY
                    oContext:TargetCursor := arrayCursor
                    FoxEmbeddedSQL.ExecuteSelect(oContext)
                    IF !Used(arrayCursor)
                        THROW Error{"SELECT INTO ARRAY did not produce a result cursor"}
                    ENDIF
                    cursorPath := (arrayCursor)->DbInfo(DBI_FULLPATH)
                    FoxEmbeddedSQL.CopySelectCursorToArray(arrayCursor, oContext:TargetArray)
                FINALLY
                    IF Used(arrayCursor)
                        (arrayCursor)->DbCloseArea()
                    ENDIF
                    IF !String.IsNullOrEmpty(cursorPath)
                        IF File.Exists(cursorPath)
                            File.Delete(cursorPath)
                        ENDIF
                        VAR memoPath := System.IO.Path.ChangeExtension(cursorPath, ".fpt")
                        IF File.Exists(memoPath)
                            File.Delete(memoPath)
                        ENDIF
                    ENDIF
                    oContext:TargetCursor := originalTargetCursor
                    IF savedArea > 0
                        DbSelectArea(savedArea)
                    ENDIF
                END TRY
            ELSEIF !String.IsNullOrEmpty(oContext:TargetTable)
                LOCAL originalTargetCursor := oContext:TargetCursor AS STRING
                LOCAL tableCursor := "SQLTABLE" + System.Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
                LOCAL cursorPath := "" AS STRING
                TRY
                    oContext:TargetCursor := tableCursor
                    FoxEmbeddedSQL.ExecuteSelect(oContext)
                    IF !Used(tableCursor)
                        THROW Error{"SELECT INTO TABLE did not produce a result cursor"}
                    ENDIF
                    cursorPath := (tableCursor)->DbInfo(DBI_FULLPATH)
                    FoxEmbeddedSQL.CopySelectCursorToTable(tableCursor, oContext:TargetTable)
                FINALLY
                    IF Used(tableCursor)
                        (tableCursor)->DbCloseArea()
                    ENDIF
                    IF !String.IsNullOrEmpty(cursorPath)
                        IF File.Exists(cursorPath)
                            File.Delete(cursorPath)
                        ENDIF
                        VAR memoPath := System.IO.Path.ChangeExtension(cursorPath, ".fpt")
                        IF File.Exists(memoPath)
                            File.Delete(memoPath)
                        ENDIF
                    ENDIF
                    oContext:TargetCursor := originalTargetCursor
                END TRY
            ELSE
                FoxEmbeddedSQL.ExecuteSelect(oContext)
                IF !String.IsNullOrEmpty(oContext:TargetCursor) .AND. !oContext:TargetCursorReadWrite
                    LOCAL cursorPath := (oContext:TargetCursor)->DbInfo(DBI_FULLPATH) AS STRING
                    DbCloseArea(oContext:TargetCursor)
                    IF !DbUseArea(TRUE, "DBFVFP", cursorPath, oContext:TargetCursor, TRUE, TRUE)
                        THROW Error{"Could not reopen SELECT cursor as read-only: " + oContext:TargetCursor}
                    ENDIF
                    DbSelectArea(oContext:TargetCursor)
                ENDIF
            ENDIF
        endif
    CATCH
        IF savedArea > 0
            DbSelectArea(savedArea)
        ENDIF
        THROW
    END TRY
    RETURN


PARTIAL STATIC CLASS FoxEmbeddedSQL

    STATIC METHOD CreateTableCursor(oTable as FoxCreateTableContext) AS LOGIC
        if oTable != NULL
            local aStruct as ARRAY
            aStruct := {}
            // Build long-name list for DBC registration alongside the struct array
            VAR aLongNames := List<STRING>{}
            foreach col as FoxColumnContext in oTable:Columns
                // Position 5 = DBS_ALIAS: prefer the long field name from the SQL parser;
                // fall back to the (possibly truncated) physical name when no alias set.
                VAR cFieldAlias := IIF(String.IsNullOrEmpty(col:Alias), col:Name, col:Alias)
                var aField := {col:Name, ((Char)col:FieldType):ToString(), col:Length, col:Decimals, cFieldAlias, col:Flags}
                AAdd(aStruct, aField)
                aLongNames:Add(cFieldAlias)
            next

            VAR cTable := oTable:Name
            VAR cAlias := Path.GetFileNameWithoutExtension(oTable:Name)
            if oTable:IsCursor
                cTable := Path.Combine(Path.GetTempPath(), System.Guid.NewGuid():ToString("N") + ".dbf")
                cAlias := oTable:Name
            ENDIF
            if RuntimeState.Workareas:FindAlias(cAlias) != 0
                DbCloseArea(cAlias)
            endif
            DbCreate(cTable, aStruct, "DBFVFP", TRUE, cAlias)
            // Compute the full absolute path from the SQL-parsed name and SET DEFAULT.
            // We cannot rely on FPathName() here because subsequent internal opens
            // (AddTable, Reload) shift the current work area before DbUseArea is called.
            LOCAL cFullPath := cTable AS STRING
            IF !Path.IsPathRooted(cFullPath)
                LOCAL getDefault := SetDefault() AS STRING
                cFullPath := Path.Combine(getDefault , cFullPath)
            ENDIF
            IF String.IsNullOrEmpty(Path.GetExtension(cFullPath))
                cFullPath := cFullPath + ".DBF"
            ENDIF
            DbCloseArea(cAlias)

            // Register the newly created table in the active DBC when it is a persistent
            // (non-cursor, non-FREE) table and a database is currently active.
            IF !oTable:IsCursor .AND. !oTable:Free
                VAR oActiveDbc := XSharp.RDD.DbcManager.ActiveDatabase
                IF oActiveDbc != NULL_OBJECT
                    VAR cTableLong := IIF(String.IsNullOrEmpty(oTable:LongName), ;
                        Path.GetFileNameWithoutExtension(cFullPath), oTable:LongName)
                    XSharp.RDD.DbcManager.AddTable(cFullPath, cTableLong, aLongNames)
                ENDIF
            ENDIF

            // Use the resolved absolute path and shared mode so the open succeeds
            // even when an exclusively-locked DBC is active in DbcDataSession.
            DbUseArea(TRUE, "DBFVFP", cFullPath, cAlias, TRUE, FALSE)
            FOR var nI := 1 to oTable:Columns:Count
                var oCol := oTable:Columns[nI-1]
                DbFieldInfo(DBS_CAPTION, nI, oCol:Caption)
            NEXT
            if oTable:IsCursor
                DbInfo(DBI_ISTEMPORARY, TRUE)
            endif
            RETURN TRUE
        endif
        RETURN FALSE

    STATIC METHOD ParseSqlCreate(sCommand as STRING, lCursor as LOGIC) AS FoxCreateTableContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        local table as FoxCreateTableContext
        IF lCursor
            IF ! parser:ParseCreateCursor(out table)
                THROW Error{"Syntax error in command: "+parser:Error+CRLF+sCommand}
            ENDIF
        ELSE
            IF ! parser:ParseCreateTable(out table)
                THROW Error{"Syntax error in command: "+parser:Error+CRLF+sCommand}
            ENDIF
        endif
        return table

    STATIC METHOD ParseSqlAlter(sCommand as STRING) AS FoxAlterTableContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        var table := parser:ParseAlterTable()
        if table == NULL
            Throw Error{parser:Error+" in command: "+sCommand}
        endif
        return table

    STATIC METHOD OpenArea(sTable as STRING) AS LOGIC
        IF ! DbSelectArea(sTable)
            DbUseArea(TRUE, "DBFVFP", sTable, sTable, TRUE, FALSE)
        ENDIF
        RETURN Used()

    STATIC METHOD Convert(currentVal as USUAL, newVal as USUAL) AS USUAL
        if UsualType(currentVal) == UsualType(newVal)
            return newVal
        endif
        local oValue as OBJECT
        oValue := __castclass(object, newVal)
        var sValue := oValue:ToString()
        SWITCH UsualType(currentVal)
        CASE __UsualType.Long
            if Int32.TryParse(sValue, OUT Var result)
                return result
            endif
            return 0
        CASE __UsualType.Float
        CASE __UsualType.Currency
            if Double.TryParse(sValue, OUT Var result)
                return result
            endif
            return 0.0
        CASE __UsualType.Date
            RETURN CToD(sValue)
        CASE __UsualType.String
        CASE __UsualType.Memo
            return sValue
        CASE __UsualType.Binary
            return (Binary) sValue
        END SWITCH
        RETURN NIL

STATIC METHOD SqlAlterTable(table as FoxAlterTableContext) AS LOGIC
    IF ! FoxEmbeddedSQL.OpenArea(table:Name)
        RETURN FALSE
    ENDIF
    var area := RuntimeState.Workareas:FindAlias(table:Name)
    if area == 0
        Throw Error{"Table "+table:Name+" not found"}
    ENDIF
    var oRdd := RuntimeState.Workareas:GetRDD(area)
    local fields := NULL as RddFieldInfo[]
    if oRdd is XSharp.RDD.Workarea var oWA
        fields := oWA:_Fields
    endif
    var column := table:ColumnInfo
    var index  := oRdd:FieldIndex(column:Name)
    local aStruct := DbStruct() as ARRAY
    switch table:Mode
    case FoxAlterMode.AddColumn
        if index > 0
            Throw Error{"Column "+column:Name+" already exists"}
        endif
        AAdd(aStruct, {column:Name, column:FieldTypeStr, column:Length, column:Decimals, column:Alias, column:Flags})

    case FoxAlterMode.DropColumn
        if index == 0
            Throw Error{"Column "+column:Name+" does not exist"}
        endif
        ADel(aStruct, (DWORD) index)
        ASize(aStruct, ALen(aStruct)-1)
    case FoxAlterMode.AlterColumn
        if index == 0
            Throw Error{"Column "+column:Name+" does not exist"}
        endif
        aStruct[index] := { column:Name, column:FieldTypeStr, column:Length, column:Decimals, column:Alias, column:Flags}
    end switch

    var oldFile  := (String) oRdd:Info(DBI_FULLPATH,null)
    var memoExt  := (String) oRdd:Info(DBI_MEMOEXT, null)
    var dbfExt   := Path.GetExtension(oldFile)
    var oldMemo  := Path.ChangeExtension(oldFile, memoExt)
    var rand     := Path.GetRandomFileName()
    var newFile  := Path.Combine(Path.GetDirectoryName(oldFile), Path.GetFileNameWithoutExtension(rand))
    newFile      := Path.ChangeExtension(newFile, dbfExt)
    var newMemo  := Path.ChangeExtension(newFile, memoExt)
    DbCloseArea()
    IF !DbCreate(newFile, aStruct)
        RETURN FALSE
    ENDIF
    IF ! DbUseArea(TRUE, "DBFVFP", newFile, table:Name, FALSE, FALSE)
        RETURN FALSE
    ENDIF
    IF ! DbApp(oldFile)
        RETURN FALSE
    ENDIF
    DbCloseArea()
    var cBak := Path.ChangeExtension(oldFile, "bak")
    FErase(cBak)
    FRename(oldFile, cBak)
    IF File(oldMemo)
        cBak := Path.ChangeExtension(oldFile, "tbk")
        FErase(cBak)
        FRename(oldMemo, cBak)
    ENDIF
    FRename(newFile, oldFile)
    IF File(newMemo)
        FRename(newMemo, oldMemo)
    ENDIF
    DbUseArea(TRUE, "DBFVFP", oldFile, table:Name, FALSE, FALSE)
    area := RuntimeState.Workareas:FindAlias(table:Name)
    oRdd := RuntimeState.Workareas:GetRDD(area)
    if fields != NULL .and. oRdd is XSharp.RDD.Workarea var oWANew
        foreach var fld in oWANew:_Fields
            var oldFld := fields:FirstOrDefault({f => f:Name == fld:Name})
            if oldFld != NULL
                fld:Caption := oldFld:Caption
            else // Must be the new column
                fld:Caption := column:Caption
            endif
        next
    endif
    RETURN TRUE
END CLASS

