//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.Parsers
using XSharp.Internal
using XSharp.RDD
using XSharp.RDD.Support
using System.Collections.Generic
using System.IO
using System.Linq
using System.Text

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

FUNCTION __SqlSelect(sCommand as STRING) AS VOID
    // Parse the SELECT command using the SQL parser
    var oContext := FoxEmbeddedSQL.ParseSqlSelect(sCommand)
    if (oContext != NULL)
        // Execute the SELECT statement based on the parsed context
        FoxEmbeddedSQL.ExecuteSelect(oContext)
    endif
    RETURN


STATIC CLASS FoxEmbeddedSQL

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
                cTable := Path.GetTempFileName()
                cAlias := oTable:Name
            ENDIF
            if RuntimeState.Workareas.FindAlias(cAlias) != 0
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
                THROW Error{"Syntax error in command: "+parser.Error+CRLF+sCommand}
            ENDIF
        ELSE
            IF ! parser:ParseCreateTable(out table)
                THROW Error{"Syntax error in command: "+parser.Error+CRLF+sCommand}
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

    STATIC METHOD ParseSqlSelect(sCommand as STRING) AS FoxSelectContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        var selectCtx := FoxSelectContext{}
        IF ! parser:ParseSelectStatement(out selectCtx)
            THROW Error{"Syntax error in SELECT command: "+parser:Error+CRLF+sCommand}
        ENDIF
        RETURN selectCtx

    STATIC METHOD ExecuteSelect(selectCtx as FoxSelectContext) AS VOID
        // Implementation of SELECT execution
        // This will handle opening tables, selecting records, and returning results

        // Validate that we have tables to query
        IF selectCtx:TableList:Count == 0
            THROW Error{"No tables specified in SELECT statement"}
        ENDIF

        // Check if any expression in the select list is '*' (TODO: what if we have both '*' and named fields?)
        LOCAL hasStarSelection AS LOGIC
        hasStarSelection := FALSE
        FOR LOCAL idx := 0 AS INT TO selectCtx:SelectList:Count - 1
            LOCAL expr AS SqlExpressionContext
            expr := selectCtx:SelectList[idx]
            IF AllTrim(expr:ToString()) == "*"
                hasStarSelection := TRUE
                EXIT
            ENDIF
        NEXT

        // Open the first table (TODO: basic implementation - doesn't handle joins yet)
        LOCAL mainTable AS STRING
        mainTable := selectCtx:TableList[0]

        // Check if the table was already open before we opened it
        LOCAL wasAlreadyOpen AS LOGIC
        wasAlreadyOpen := DbSelectArea(mainTable)  // This selects the area if it's open
        IF !wasAlreadyOpen
            // If not already open, open it
            IF ! FoxEmbeddedSQL.OpenArea(mainTable)
                THROW Error{"Could not open table: " + mainTable}
            ENDIF
        ENDIF

        VAR resultTable := "QUERYRESULT" // Default table name for SELECT results

        LOCAL fieldList AS ARRAY
        LOCAL selectFieldNames AS ARRAY
        LOCAL selectExpressions AS ARRAY

        // Process the SELECT list to determine which fields to include
        IF selectCtx:SelectList:Count == 0 .OR. hasStarSelection
            // If no specific fields are mentioned or if we have '*', select all fields
            LOCAL struct AS ARRAY
            struct := DbStruct()
            fieldList := {}
            selectFieldNames := {}
            selectExpressions := {}

            FOR LOCAL i := 1 AS DWORD TO ALen(struct)
                LOCAL fieldInfo AS ARRAY
                fieldInfo := struct[i]
                AAdd(selectFieldNames, fieldInfo[1])  // Field name
                AAdd(selectExpressions, mainTable + "->" + fieldInfo[1])  // Expression

                // Create field definition for result table
                AAdd(fieldList, {fieldInfo[1], fieldInfo[2], fieldInfo[3], fieldInfo[4], fieldInfo[1], fieldInfo[6]})
            NEXT
        ELSE
            // Process specific fields in the SELECT list (no '*' involved)
            fieldList := {}
            selectFieldNames := {}
            selectExpressions := {}

            FOR LOCAL i := 0 AS INT TO selectCtx:SelectList:Count - 1
                LOCAL expr AS SqlExpressionContext
                expr := selectCtx:SelectList[i]

                // Apply field resolution to the expression
                LOCAL exprStr, resolvedExprStr AS STRING
                resolvedExprStr := expr:ToResolvedString(selectCtx:TableAliases)
                exprStr := AllTrim(expr:ToString())

                // Find the field in the source table to get its type
                LOCAL fieldPos AS DWORD
                fieldPos := FieldPos(exprStr)
                IF fieldPos == 0
                    exprStr := "FIELD" + i:ToString()
                ENDIF

                AAdd(selectFieldNames, exprStr)
                AAdd(selectExpressions, resolvedExprStr)

                IF fieldPos > 0
                    LOCAL fieldInfo AS ARRAY
                    fieldInfo := DbStruct()[fieldPos]
                    AAdd(fieldList, {fieldInfo[1], fieldInfo[2], fieldInfo[3], fieldInfo[4], fieldInfo[1], fieldInfo[6]})
                ELSE
                    // If field doesn't exist in source, assume it's an expression (TODO: determine the type)
                    AAdd(fieldList, {exprStr, "C", 50, 0, exprStr, 0})
                ENDIF
            NEXT
        ENDIF

        // Create the result table
        LOCAL tempPath AS STRING
        tempPath := System.IO.Path.GetTempFileName()
        tempPath := System.IO.Path.ChangeExtension(tempPath, ".dbf")

        IF ! DbCreate(tempPath, fieldList, "DBFVFP", TRUE, resultTable)
            THROW Error{"Could not create result table: " + resultTable}
        ENDIF

        LOCAL whereClause AS STRING
        LOCAL whereCodeBlock AS CODEBLOCK
        LOCAL hasWhereClause AS LOGIC
        hasWhereClause := FALSE

        IF selectCtx:WhereClause != NULL
            whereClause := selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
            hasWhereClause := TRUE
            whereCodeBlock := MCompile(whereClause)
        ENDIF

        // Navigate through the source table and copy matching records to result
        LOCAL recordCount AS DWORD
        recordCount := 0

        // Check if DISTINCT is specified
        LOCAL isDistinct AS LOGIC
        isDistinct := selectCtx:IsDistinct

        // If TOP is specified, limit the number of records
        LOCAL maxRecords AS LONG
        maxRecords := -1 // -1 means no limit
        IF ! String.IsNullOrEmpty(selectCtx:TopCount)
            IF Int32.TryParse(selectCtx:TopCount, OUT maxRecords) .AND. maxRecords <= 0
                maxRecords := 1 // At least one record if TOP is specified
            ENDIF
        ENDIF

        LOCAL distinctValues AS HashSet<STRING>
        IF isDistinct
            distinctValues := HashSet<STRING>{}
        ENDIF

        (mainTable)->DbGoTop()

        DO WHILE !(mainTable)->Eof()
            LOCAL includeRecord AS LOGIC
            includeRecord := TRUE

            // Check WHERE condition if present
            IF hasWhereClause
                TRY
                    includeRecord := whereCodeBlock:Eval()
                CATCH ex AS Exception
                    includeRecord := FALSE
                    ? "Error evaluating WHERE clause: " + ex:Message
                END TRY
            ENDIF

            // If the record matches the criteria, add it to the result
            IF includeRecord
                LOCAL recordAdded AS LOGIC
                recordAdded := FALSE

                LOCAL sourceValues AS USUAL[]
                sourceValues := USUAL[]{ALen(selectFieldNames)}

                FOR LOCAL i := 1 AS DWORD TO ALen(selectFieldNames)
                    sourceValues[i] := &(selectExpressions[i])
                NEXT

                IF isDistinct
                    // For DISTINCT, check if we've already seen this combination of values
                    VAR recordKey := ""
                    FOR LOCAL i := 1 AS DWORD TO ALen(selectFieldNames)
                        recordKey += sourceValues[i]:ToString() + "|"
                    NEXT

                    IF ! distinctValues:Contains(recordKey)
                        distinctValues:Add(recordKey)
                        recordAdded := TRUE
                    ENDIF
                ELSE
                    recordAdded := TRUE
                ENDIF

                IF recordAdded
                    // Add the record to the result table
                    (resultTable)->DbAppend()

                    FOR LOCAL i := 1 AS DWORD TO ALen(selectFieldNames)
                        IF fieldList[i][2] == "C"
                            (resultTable)->FieldPut(i, sourceValues[i]:ToString())
                        ELSE
                            (resultTable)->FieldPut(i, sourceValues[i])
                        ENDIF
                    NEXT

                    recordCount++

                    // Check if we've reached the TOP limit
                    IF maxRecords > 0 .AND. recordCount >= maxRecords
                        EXIT
                    ENDIF
                ENDIF
            ENDIF

            (mainTable)->DbSkip(1)
        ENDDO

        // Set the result table as the current work area
        (resultTable)->DbSelectArea()

        IF !wasAlreadyOpen
            (mainTable)->DbCloseArea()
        ENDIF

        RETURN

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
    var area := RuntimeState.Workareas.FindAlias(table:Name)
    if area == 0
        Throw Error{"Table "+table:Name+" not found"}
    ENDIF
    var oRdd := RuntimeState.Workareas.GetRDD(area)
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
    area := RuntimeState.Workareas.FindAlias(table:Name)
    oRdd := RuntimeState.Workareas.GetRDD(area)
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






