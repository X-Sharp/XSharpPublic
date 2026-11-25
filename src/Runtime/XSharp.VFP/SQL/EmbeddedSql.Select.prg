//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.Data
using XSharp.Data.Query
using XSharp.Parsers
using XSharp.Internal
using XSharp.RDD
using XSharp.RDD.Support
using System.Collections.Generic
using System.IO
using System.Linq
using System.Text
using System.Text.RegularExpressions

BEGIN NAMESPACE XSharp.VFP

INTERNAL CLASS SqlSelectResultRow
    PUBLIC PROPERTY Values AS USUAL[] AUTO
    PUBLIC PROPERTY SortValues AS USUAL[] AUTO
    PUBLIC PROPERTY Sequence AS LONG AUTO

    INTERNAL CONSTRUCTOR(values AS USUAL[], sortValues AS USUAL[], sequence AS LONG)
        SELF:Values := values
        SELF:SortValues := sortValues
        SELF:Sequence := sequence
        RETURN
END CLASS

INTERNAL CLASS SqlSelectResultRowComparer IMPLEMENTS IComparer<SqlSelectResultRow>
    PRIVATE _orderBy AS IList<FoxOrderByItem>

    INTERNAL CONSTRUCTOR(orderBy AS IList<FoxOrderByItem>)
        SELF:_orderBy := orderBy
        RETURN

    PUBLIC METHOD Compare(left AS SqlSelectResultRow, right AS SqlSelectResultRow) AS INT
        FOR LOCAL i := 0 AS INT UPTO _orderBy:Count - 1
            VAR difference := left:SortValues[i+1]:CompareTo(right:SortValues[i+1])
            IF difference != 0
                RETURN IIF(_orderBy[i]:Descending, -difference, difference)
            ENDIF
        NEXT
        RETURN left:Sequence:CompareTo(right:Sequence)
END CLASS

INTERNAL CLASS SqlHashJoinCursor
    PRIVATE _outerTable AS STRING
    PRIVATE _outerKeyCodeBlock AS CODEBLOCK
    PRIVATE _keyBuckets AS Dictionary<USUAL,List<LONG>>
    PRIVATE _currentBucket AS List<LONG>
    PRIVATE _bucketIndex AS INT
    PRIVATE _currentOuterRecNo AS LONG
    PRIVATE _started AS LOGIC

    INTERNAL CONSTRUCTOR(outerTable AS STRING, outerKeyCodeBlock AS CODEBLOCK, ;
        keyBuckets AS Dictionary<USUAL,List<LONG>>)
        SELF:_outerTable := outerTable
        SELF:_outerKeyCodeBlock := outerKeyCodeBlock
        SELF:_keyBuckets := keyBuckets
        SELF:_currentBucket := NULL
        SELF:_bucketIndex := 0
        SELF:_currentOuterRecNo := 0
        SELF:_started := FALSE
        RETURN

    INTERNAL METHOD MoveNext(outerRecNo OUT LONG, innerRecNo OUT LONG) AS LOGIC
        outerRecNo := 0
        innerRecNo := 0
        DO WHILE TRUE
            IF SELF:_currentBucket != NULL .AND. SELF:_bucketIndex < SELF:_currentBucket:Count
                outerRecNo := SELF:_currentOuterRecNo
                innerRecNo := SELF:_currentBucket[SELF:_bucketIndex]
                SELF:_bucketIndex++
                RETURN TRUE
            ENDIF

            IF SELF:_started
                (SELF:_outerTable)->DbSkip(1)
            ELSE
                (SELF:_outerTable)->DbGoTop()
                SELF:_started := TRUE
            ENDIF
            IF (SELF:_outerTable)->Eof()
                RETURN FALSE
            ENDIF

            SELF:_currentOuterRecNo := (LONG)(SELF:_outerTable)->RecNo()
            VAR keyValue := (USUAL) SELF:_outerKeyCodeBlock:Eval()
            SELF:_currentBucket := NULL
            SELF:_bucketIndex := 0
            IF !keyValue:IsNull
                SELF:_keyBuckets:TryGetValue(keyValue, OUT SELF:_currentBucket)
            ENDIF
        ENDDO
END CLASS

PARTIAL STATIC CLASS FoxEmbeddedSQL

    PRIVATE STATIC METHOD FindHashJoinKey(expression AS SqlExpressionContext, ;
        tableAliases AS IDictionary<STRING,STRING>, outerTable AS STRING, innerTable AS STRING, ;
        outerKey OUT SqlNameExpressionContext, innerKey OUT SqlNameExpressionContext) AS LOGIC
        outerKey := NULL
        innerKey := NULL

        IF expression IS SqlParenExpressionContext VAR parenExpr
            RETURN FindHashJoinKey(parenExpr:Expr, tableAliases, outerTable, innerTable, OUT outerKey, OUT innerKey)
        ENDIF

        IF expression IS SqlLogicExpressionContext VAR logicExpr
            IF logicExpr:Op:Type == XTokenType.AND
                IF FindHashJoinKey(logicExpr:Left, tableAliases, outerTable, innerTable, OUT outerKey, OUT innerKey)
                    RETURN TRUE
                ENDIF
                RETURN FindHashJoinKey(logicExpr:Right, tableAliases, outerTable, innerTable, OUT outerKey, OUT innerKey)
            ENDIF
            RETURN FALSE
        ENDIF

        IF expression IS SqlCompareExpressionContext VAR compareExpr
            IF compareExpr:Op:Type != XTokenType.EQ .OR. ;
               compareExpr:Left IS NOT SqlNameExpressionContext .OR. ;
               compareExpr:Right IS NOT SqlNameExpressionContext
                RETURN FALSE
            ENDIF

            VAR leftName := (SqlNameExpressionContext) compareExpr:Left
            VAR rightName := (SqlNameExpressionContext) compareExpr:Right
            IF String.IsNullOrEmpty(leftName:Table) .OR. String.IsNullOrEmpty(rightName:Table)
                RETURN FALSE
            ENDIF

            VAR leftDependencies := leftName:GetTableDependencies(tableAliases)
            VAR rightDependencies := rightName:GetTableDependencies(tableAliases)
            IF leftDependencies:Count != 1 .OR. rightDependencies:Count != 1
                RETURN FALSE
            ENDIF

            IF leftDependencies[0]:Equals(outerTable, StringComparison.OrdinalIgnoreCase) .AND. ;
               rightDependencies[0]:Equals(innerTable, StringComparison.OrdinalIgnoreCase)
                outerKey := leftName
                innerKey := rightName
                RETURN TRUE
            ELSEIF rightDependencies[0]:Equals(outerTable, StringComparison.OrdinalIgnoreCase) .AND. ;
                   leftDependencies[0]:Equals(innerTable, StringComparison.OrdinalIgnoreCase)
                outerKey := rightName
                innerKey := leftName
                RETURN TRUE
            ENDIF
        ENDIF

        RETURN FALSE

    PRIVATE STATIC METHOD HashJoinFieldType(tableName AS STRING, fieldName AS STRING) AS STRING
        VAR fieldPosition := (tableName)->FieldPos(fieldName)
        IF fieldPosition <= 0
            RETURN NULL
        ENDIF
        VAR structure := (tableName)->DbStruct()
        RETURN (STRING) structure[fieldPosition][2]

    PRIVATE STATIC METHOD IsHashJoinFieldType(fieldType AS STRING) AS LOGIC
        IF String.IsNullOrEmpty(fieldType)
            RETURN FALSE
        ENDIF
        SWITCH fieldType:ToUpperInvariant()
        CASE "N" // Numeric
        CASE "F" // Float
        CASE "B" // Double
        CASE "I" // Integer
        CASE "Y" // Currency
        CASE "D" // Date
        CASE "T" // DateTime
        CASE "L" // Logical
            RETURN TRUE
        OTHERWISE
            // Character equality can be directional when SET EXACT is off, and its
            // runtime comparison rules do not share a general-purpose hash key.
            RETURN FALSE
        END SWITCH


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

        // Open all tables (for cross join implementation)
        LOCAL tableNames AS ARRAY
        LOCAL originalAreas AS ARRAY

        tableNames := {}
        originalAreas := {}

        FOR LOCAL i := 0 AS INT TO selectCtx:TableList:Count - 1
            LOCAL tableName AS STRING
            tableName := selectCtx:TableList[i]

            // Check if the table was already open before we opened it
            LOCAL wasAlreadyOpen AS LOGIC
            wasAlreadyOpen := DbSelectArea(tableName)  // This selects the area if it's open
            AAdd(originalAreas, wasAlreadyOpen)

            IF !wasAlreadyOpen
                // If not already open, open it
                IF ! FoxEmbeddedSQL.OpenArea(tableName)
                    THROW Error{"Could not open table: " + tableName}
                ENDIF
            ENDIF

            AAdd(tableNames, tableName)
        NEXT

        LOCAL fieldList AS ARRAY
        LOCAL selectFieldNames AS ARRAY
        LOCAL selectExpressions AS ARRAY

        // Process the SELECT list to determine which fields to include
        LOCAL allFieldNames AS List<STRING>
        allFieldNames := List<STRING>{}

        IF selectCtx:SelectList:Count == 0 .OR. hasStarSelection
            // If no specific fields are mentioned or if we have '*', select all fields
            fieldList := {}
            selectFieldNames := {}
            selectExpressions := {}

            // Loop through all tables to get all fields
            FOR LOCAL tableIdx := 1 AS DWORD TO ALen(tableNames)
                LOCAL currentTable AS STRING
                currentTable := tableNames[tableIdx]

                LOCAL struct AS ARRAY
                struct := (currentTable)->DbStruct()

                FOR LOCAL i := 1 AS DWORD TO ALen(struct)
                    LOCAL fieldInfo AS ARRAY
                    fieldInfo := struct[i]
                    LOCAL fieldName AS STRING
                    fieldName := fieldInfo[1]

                    // Resolve any conflicts with existing field names
                    fieldName := ResolveConflictingFieldName(fieldName, allFieldNames)
                    allFieldNames:Add(fieldName:ToUpper())

                    AAdd(selectFieldNames, fieldName)  // Field name
                    AAdd(selectExpressions, currentTable + "->" + fieldName)  // Expression

                    // Create field definition for result table
                    AAdd(fieldList, {fieldName, fieldInfo[2], fieldInfo[3], fieldInfo[4], fieldName, fieldInfo[6]})
                NEXT
            NEXT
        ELSE
            // Check if we have field selections with aliases (new functionality)
            LOCAL hasFieldSelections AS LOGIC
            hasFieldSelections := selectCtx:FieldSelectionList != NULL .AND. selectCtx:FieldSelectionList:Count > 0

            // Process specific fields in the SELECT list (no '*' involved)
            fieldList := {}
            selectFieldNames := {}
            selectExpressions := {}

            FOR LOCAL i := 0 AS INT TO selectCtx:SelectList:Count - 1
                LOCAL expr AS SqlExpressionContext
                expr := selectCtx:SelectList[i]

                LOCAL fieldAlias AS STRING
                fieldAlias := NULL

                // Use alias if available from the new field selection list
                IF hasFieldSelections .AND. i < selectCtx:FieldSelectionList:Count
                    fieldAlias := selectCtx:FieldSelectionList[i]:Alias
                ENDIF

                // Apply field resolution to the expression
                LOCAL exprStr, resolvedExprStr AS STRING
                resolvedExprStr := expr:ToResolvedString(selectCtx:TableAliases)

                // Use the alias if provided, otherwise generate a field name from the expression
                IF fieldAlias != NULL .AND. fieldAlias:Trim():Length > 0
                    exprStr := fieldAlias
                ELSE
                    exprStr := SqlExpressionToFieldName(expr)  // Use the new method to get VFP-compatible field name
                ENDIF

                // Resolve any conflicts with existing field names
                exprStr := ResolveConflictingFieldName(exprStr, allFieldNames)
                allFieldNames:Add(exprStr:ToUpper())

                // Find the field in the source tables to get its type
                LOCAL fieldFound AS LOGIC
                fieldFound := FALSE
                LOCAL fieldInfo AS ARRAY

                LOCAL exprTable := NULL AS STRING
                LOCAL exprField := NULL AS STRING
                IF expr IS SqlNameExpressionContext VAR nameCtx
                    exprTable := nameCtx:Table
                    exprField := nameCtx:Name
                ENDIF

                IF ! System.String.IsNullOrEmpty(exprTable)
                    LOCAL sourceTable := exprTable AS STRING
                    selectCtx:TableAliases:TryGetValue(exprTable, OUT sourceTable)
                    VAR fieldPos := (sourceTable)->FieldPos(exprField)
                    IF fieldPos > 0
                        fieldFound := TRUE
                        fieldInfo := (sourceTable)->DbStruct()[fieldPos]
                    ENDIF
                ELSEIF ! System.String.IsNullOrEmpty(exprField)
                    // Loop through all tables to find the field
                    FOR LOCAL tableIdx := 1 AS DWORD TO ALen(tableNames)
                        LOCAL currentTable AS STRING
                        currentTable := tableNames[tableIdx]

                        VAR fieldPos := (currentTable)->FieldPos(exprField)
                        IF fieldPos > 0
                            fieldFound := TRUE
                            fieldInfo := (currentTable)->DbStruct()[fieldPos]
                            EXIT
                        ENDIF
                    NEXT
                ENDIF

                AAdd(selectFieldNames, exprStr)
                AAdd(selectExpressions, resolvedExprStr)

                IF fieldFound .AND. fieldInfo != NULL
                    AAdd(fieldList, {exprStr, fieldInfo[2], fieldInfo[3], fieldInfo[4], exprStr, fieldInfo[6]})
                ELSE
                    // If field doesn't exist in source, assume it's an expression (TODO: determine the type)
                    AAdd(fieldList, {exprStr, "C", 50, 0, exprStr, 0})
                ENDIF
            NEXT
        ENDIF

        // Determine the result table name - use target cursor if specified
        VAR resultTable := "QUERYRESULT" // Default table name for SELECT results
        IF ! String.IsNullOrEmpty(AllTrim(selectCtx:TargetCursor))
            resultTable := AllTrim(selectCtx:TargetCursor)
        ENDIF

        // Close any existing work area with this alias to avoid "Alias already defined" error
        // TODO: This differs from the VFP behavior that reuses the same area.
        IF Used(resultTable)
            (resultTable)->DbCloseArea()
        ENDIF

        // Create the result table
        LOCAL tempPath AS STRING
        tempPath := System.IO.Path.GetTempFileName()
        tempPath := System.IO.Path.ChangeExtension(tempPath, ".dbf")

        IF ! DbCreate(tempPath, fieldList, "DBFVFP", TRUE, resultTable)
            THROW Error{"Could not create result table: " + resultTable}
        ENDIF

        // Resolve WHERE clause
        LOCAL whereClause AS STRING
        LOCAL whereCodeBlock AS CODEBLOCK
        LOCAL hasWhereClause AS LOGIC
        hasWhereClause := FALSE
        IF selectCtx:WhereClause != NULL
            whereClause := selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
            hasWhereClause := TRUE
            whereCodeBlock := MCompile(whereClause)
        ENDIF

        LOCAL joinCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        FOREACH VAR joinContext IN selectCtx:Joins
            IF joinContext:Kind == FoxSelectJoinKind.Inner
                VAR joinExpression := joinContext:OnClause:ToResolvedString(selectCtx:TableAliases)
                VAR joinCodeBlock := MCompile(joinExpression)
                IF joinCodeBlock == NULL
                    THROW Error{"Could not compile JOIN ON expression: " + joinExpression}
                ENDIF
                joinCodeBlocks:Add(joinCodeBlock)
            ENDIF
        NEXT

        // Build candidate pairs for the narrow two-table equijoin shape. The full ON
        // expression remains in joinCodeBlocks and is evaluated for every candidate.
        LOCAL useHashJoin := FALSE AS LOGIC
        LOCAL hashJoinCursor := NULL AS SqlHashJoinCursor
        IF tableNames:Count == 2 .AND. selectCtx:Joins:Count == 1 .AND. ;
           selectCtx:Joins[0]:Kind == FoxSelectJoinKind.Inner .AND. ;
           !((STRING)tableNames[1]):Equals((STRING)tableNames[2], StringComparison.OrdinalIgnoreCase)
            VAR outerTable := (STRING)tableNames[1]
            VAR innerTable := (STRING)tableNames[2]
            IF FindHashJoinKey(selectCtx:Joins[0]:OnClause, selectCtx:TableAliases, ;
                               outerTable, innerTable, OUT VAR outerKey, OUT VAR innerKey)
                VAR outerFieldType := HashJoinFieldType(outerTable, outerKey:Name)
                VAR innerFieldType := HashJoinFieldType(innerTable, innerKey:Name)
                IF IsHashJoinFieldType(outerFieldType) .AND. ;
                   outerFieldType:Equals(innerFieldType, StringComparison.OrdinalIgnoreCase)
                    VAR outerKeyExpression := outerKey:ToResolvedString(selectCtx:TableAliases)
                    VAR innerKeyExpression := innerKey:ToResolvedString(selectCtx:TableAliases)
                    VAR outerKeyCodeBlock := MCompile(outerKeyExpression)
                    VAR innerKeyCodeBlock := MCompile(innerKeyExpression)
                    IF outerKeyCodeBlock != NULL .AND. innerKeyCodeBlock != NULL
                        VAR keyBuckets := Dictionary<USUAL,List<LONG>>{}
                        TRY
                            (innerTable)->DbGoTop()
                            DO WHILE !(innerTable)->Eof()
                                VAR keyValue := (USUAL) innerKeyCodeBlock:Eval()
                                IF !keyValue:IsNull
                                    VAR innerRecNo := (LONG)(innerTable)->RecNo()
                                    IF keyBuckets:ContainsKey(keyValue)
                                        keyBuckets[keyValue]:Add(innerRecNo)
                                    ELSE
                                        VAR bucket := List<LONG>{}
                                        bucket:Add(innerRecNo)
                                        keyBuckets[keyValue] := bucket
                                    ENDIF
                                ENDIF
                                (innerTable)->DbSkip(1)
                            ENDDO

                            hashJoinCursor := SqlHashJoinCursor{outerTable, outerKeyCodeBlock, keyBuckets}
                            useHashJoin := TRUE
                        CATCH ex AS Exception
                            hashJoinCursor := NULL
                            useHashJoin := FALSE
                        END TRY
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

        // Try to optimize the WHERE clause using QueryOptimizer
        LOCAL optimizedBitmap AS RecordBitmap
        LOCAL useOptimization AS LOGIC
        useOptimization := FALSE

        IF hasWhereClause .AND. tableNames:Count == 1
            // Get the record count for optimization (only single table queries supported)
            LOCAL tableName AS STRING
            tableName := tableNames[1]
            VAR workarea := DbSelect()
            (tableName)->DbSelectArea()
            LOCAL recCount AS LONG
            recCount := (LONG)((tableName)->RecCount())
            (workarea)->DbSelectArea()

            // Try to optimize the WHERE clause
            optimizedBitmap := QueryOptimizer.OptimizeQuery(selectCtx:WhereClause, recCount, selectCtx:TableAliases)

            // Only use optimization if we got a valid bitmap and it's not all matches
            // (if all records match, we can skip optimization overhead)
            IF optimizedBitmap != NULL .AND. !optimizedBitmap:IsAllMatch()
                useOptimization := TRUE
            ENDIF
        ENDIF

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

        LOCAL hasOrderBy := selectCtx:OrderByClause:Count > 0 AS LOGIC
        LOCAL orderCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        LOCAL orderedRows := List<SqlSelectResultRow>{} AS List<SqlSelectResultRow>
        IF hasOrderBy
            FOREACH VAR orderItem IN selectCtx:OrderByClause
                VAR orderExpression := orderItem:Expression:ToResolvedString(selectCtx:TableAliases)
                VAR orderCodeBlock := MCompile(orderExpression)
                IF orderCodeBlock == NULL
                    THROW Error{"Could not compile ORDER BY expression: " + orderExpression}
                ENDIF
                orderCodeBlocks:Add(orderCodeBlock)
            NEXT
        ENDIF

        LOCAL recordCount AS DWORD
        recordCount := 0

        // Perform cross join by iterating through all possible combinations
        LOCAL tablePositions AS ARRAY
        tablePositions := ArrayNew(ALen(tableNames))

        // Initialize all positions to 1 (first record) and go to top of each table
        FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
            tablePositions[i] := 1
            (tableNames[i])->DbGoTop()
        NEXT

        // Main loop to iterate through all combinations
        LOCAL continueProcessing AS LOGIC
        LOCAL currentOuterRecNo AS LONG
        LOCAL currentInnerRecNo AS LONG
        continueProcessing := TRUE
        IF continueProcessing .AND. useHashJoin
            continueProcessing := hashJoinCursor:MoveNext(OUT currentOuterRecNo, OUT currentInnerRecNo)
            IF continueProcessing
                DbGoto(currentOuterRecNo, tableNames[1])
                DbGoto(currentInnerRecNo, tableNames[2])
            ENDIF
        ENDIF

        // For optimization: track current record number for bitmap lookup
        // Only works for single-table queries; multi-table needs different approach
        LOCAL recNo AS LONG
        recNo := 0

        // Initialize first table to top if using optimization with single table
        IF useOptimization .AND. ALen(tableNames) == 1
            (tableNames[1])->DbGoTop()
            recNo := 1
        ENDIF

        DO WHILE continueProcessing
            // Check WHERE clause if present
            LOCAL includeRecord AS LOGIC
            includeRecord := TRUE

            // Evaluate join predicates before WHERE, matching SQL's row-combination semantics.
            FOREACH VAR joinCodeBlock IN joinCodeBlocks
                IF !includeRecord
                    EXIT
                ENDIF
                TRY
                    includeRecord := joinCodeBlock:Eval()
                CATCH ex AS Exception
                    includeRecord := FALSE
                END TRY
            NEXT

            IF includeRecord .AND. hasWhereClause
                // Use optimized bitmap when available, otherwise evaluate codeblock
                IF useOptimization .AND. ALen(tableNames) == 1 .AND. recNo > 0
                    // The bitmap is indexed by raw record numbers, but we're traversing in index order
                    // We need to seek to the physical record number from the bitmap
                    LOCAL currentRecNo := (LONG)(tableNames[1])->RecNo() as LONG
                    IF optimizedBitmap:GetMatchState(currentRecNo) == RecordBitmap.RecordMatchState.Match
                        includeRecord := TRUE
                    ELSE
                        includeRecord := FALSE
                    ENDIF
                ELSEIF hasWhereClause
                    TRY
                        includeRecord := whereCodeBlock:Eval()
                    CATCH ex AS Exception
                        includeRecord := FALSE
                    END TRY
                ENDIF
            ENDIF

            // If the record matches the criteria, add it to the result
            IF includeRecord
                // Get values from current record combination
                LOCAL sourceValues AS USUAL[]
                sourceValues := USUAL[]{ALen(selectFieldNames)}
                FOR LOCAL i := 1 AS DWORD TO ALen(selectFieldNames)
                    sourceValues[i] := &(selectExpressions[i])
                NEXT

                LOCAL recordAdded AS LOGIC
                recordAdded := FALSE

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
                    IF hasOrderBy
                        LOCAL sortValues := USUAL[]{orderCodeBlocks:Count} AS USUAL[]
                        FOR LOCAL i := 1 AS DWORD TO orderCodeBlocks:Count
                            sortValues[i] := orderCodeBlocks[(INT)i-1]:Eval()
                        NEXT
                        orderedRows:Add(SqlSelectResultRow{sourceValues, sortValues, (LONG)recordCount})
                    ELSE
                        AppendSelectResult(resultTable, fieldList, sourceValues)
                    ENDIF

                    recordCount++

                    // ORDER BY must be applied before TOP; collect all matching rows first.
                    IF !hasOrderBy .AND. maxRecords > 0 .AND. recordCount >= maxRecords
                        continueProcessing := FALSE
                    ENDIF
                ENDIF
            ENDIF

            // Only continue if we haven't reached the TOP limit
            IF continueProcessing
                // For single-table optimization, advance the table and check for EOF
                IF useHashJoin
                    continueProcessing := hashJoinCursor:MoveNext(OUT currentOuterRecNo, OUT currentInnerRecNo)
                    IF continueProcessing
                        DbGoto(currentOuterRecNo, tableNames[1])
                        DbGoto(currentInnerRecNo, tableNames[2])
                    ELSE
                        (tableNames[1])->DbGoTop()
                        (tableNames[2])->DbGoTop()
                    ENDIF
                ELSEIF useOptimization .AND. ALen(tableNames) == 1
                    (tableNames[1])->DbSkip(1)
                    recNo++

                    // Check if we've reached the end of this table
                    IF (tableNames[1])->Eof()
                        continueProcessing := FALSE
                        (tableNames[1])->DbGoTop()  // Reset for cleanup
                    ENDIF
                ELSE
                    // Move to the next combination of records - implement proper nested loop logic
                    LOCAL positionIdx AS DWORD
                    positionIdx := ALen(tableNames)

                    // Handle carry-over when a table reaches EOF (like incrementing a multi-digit number)
                    DO WHILE positionIdx >= 1
                        // Advance the current table
                        (tableNames[positionIdx])->DbSkip(1)
                        tablePositions[positionIdx]++

                        // If this table hasn't reached EOF, we're done with carry-over
                        IF !(tableNames[positionIdx])->Eof()
                            EXIT  // Exit the carry-over loop
                        ENDIF

                        // If this table reached EOF, reset it and continue carry-over to next table
                        (tableNames[positionIdx])->DbGoTop()
                        tablePositions[positionIdx] := 1

                        positionIdx--
                    ENDDO

                    // If we've gone past all tables (positionIdx > ALen(tableNames)),
                    // it means we've processed all combinations
                    IF positionIdx == 0
                        continueProcessing := FALSE
                    ENDIF
                ENDIF
            ENDIF
        ENDDO

        IF hasOrderBy
            orderedRows:Sort(SqlSelectResultRowComparer{selectCtx:OrderByClause})
            LOCAL rowsToWrite := orderedRows:Count AS LONG
            IF maxRecords > 0
                rowsToWrite := Math.Min(rowsToWrite, maxRecords)
            ENDIF
            FOR LOCAL rowIndex := 0 AS LONG UPTO rowsToWrite - 1
                AppendSelectResult(resultTable, fieldList, orderedRows[(INT)rowIndex]:Values)
            NEXT
        ENDIF

        // Close tables that weren't originally open
        FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
            IF !originalAreas[i]
                (tableNames[i])->DbCloseArea()
            ENDIF
        NEXT

        // Set the result table as the current work area
        DbSelectArea(resultTable)

        RETURN

    STATIC METHOD AppendSelectResult(resultTable AS STRING, fieldList AS ARRAY, sourceValues AS USUAL[]) AS VOID
        (resultTable)->DbAppend()
        FOR LOCAL i := 1 AS DWORD TO sourceValues:Length
            IF fieldList[i][2] == "C"
                (resultTable)->FieldPut(i, sourceValues[i]:ToString())
            ELSE
                (resultTable)->FieldPut(i, sourceValues[i])
            ENDIF
        NEXT
        RETURN

    STATIC METHOD SqlExpressionToFieldName(expr AS SqlExpressionContext) AS STRING
        // Convert an SQL expression context to a VFP-compatible field name
        // This method handles different types of expressions and generates appropriate field names

        LOCAL result AS STRING
        result := ""

        IF expr IS SqlNameExpressionContext VAR nameCtx
            // For name expressions, use the name part
            result := nameCtx:Name
        ELSEIF expr IS SqlCompositeExpressionContext VAR compCtx
            // For composite expressions, use the first name if available
            IF compCtx:Names:Count > 0
                result := compCtx:Names[0]:Name
            ELSE
                result := compCtx:ToString()
            ENDIF
        ELSEIF expr IS SqlParenExpressionContext
            // For parenthesized expressions, return a generic name
            result := "EXPR"
        ELSEIF expr IS SqlBinaryExpressionContext
            // For binary expressions, return a generic name
            result := "EXPR"
        ELSEIF expr IS SqlPrefixExpressionContext
            // For prefix expressions, return a generic name
            result := "EXPR"
        ELSE
            // For any other expression type, just get the expression
            result := expr:ToString()
        ENDIF

        // Clean up the field name to make it VFP-compatible
        // Remove any special characters that aren't allowed in VFP field names
        result := Regex.Replace(result, "[^a-zA-Z0-9_]", "_")

        // Ensure the field name starts with a letter or underscore
        IF result:Length > 0 .AND. !Char.IsLetter(result[0]) .AND. result[0] != c'_'
            result := "_" + result
        ENDIF

        // Limit the length to 10 characters
        IF result:Length > 10
            result := result:Substring(0, 10)
        ENDIF

        RETURN result

    STATIC METHOD ResolveConflictingFieldName(newName AS STRING, existingNames AS List<STRING>) AS STRING
        // Resolve conflicting field names the same way as VFP
        // If the newName already exists in existingNames, append a number to make it unique

        LOCAL result AS STRING
        result := newName

        // VFP field names are limited to 10 characters, so we need to account for this
        LOCAL baseName AS STRING
        baseName := result

        // Truncate to 10 characters
        IF baseName:Length > 10
            baseName := baseName:Substring(0, 10)
        ENDIF

        LOCAL counter AS DWORD
        counter := 1
        LOCAL testName AS STRING
        testName := baseName

        // Check if the name already exists in the list of existing names
        WHILE existingNames:Contains(testName:ToUpper())
            testName := baseName + counter:ToString()
            // Ensure the name doesn't exceed VFP's field name length limit
            IF testName:Length > 10
                // If the name with counter exceeds 10 chars, truncate the base name further
                LOCAL availableChars AS INT
                availableChars := 10 - counter:ToString():Length
                IF availableChars < 1
                    // If counter is too large, use a standard format
                    testName := "FIELD" + counter:ToString()
                    IF testName:Length > 10
                        testName := testName:Substring(0, 10)
                    ENDIF
                    EXIT
                ENDIF
                testName := baseName:Substring(0, availableChars) + counter:ToString()
            ENDIF
            counter++
        END WHILE

        RETURN testName

END CLASS

END NAMESPACE
