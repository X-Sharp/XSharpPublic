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

PARTIAL STATIC CLASS FoxEmbeddedSQL

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
                    VAR fieldPos := (exprTable)->FieldPos(exprField)
                    IF fieldPos > 0
                        fieldFound := TRUE
                        fieldInfo := (exprTable)->DbStruct()[fieldPos]
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
        continueProcessing := TRUE

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

            IF hasWhereClause
                // Use optimized bitmap when available, otherwise evaluate codeblock
                IF useOptimization .AND. ALen(tableNames) == 1 .AND. recNo > 0
                    includeRecord := optimizedBitmap:GetMatchState(recNo) == RecordBitmap.RecordMatchState.Match
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
                        continueProcessing := FALSE
                    ENDIF
                ENDIF
            ENDIF

            // Only continue if we haven't reached the TOP limit
            IF continueProcessing
                // For single-table optimization, advance the table and check for EOF
                IF useOptimization .AND. ALen(tableNames) == 1
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

        // Close tables that weren't originally open
        FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
            IF !originalAreas[i]
                (tableNames[i])->DbCloseArea()
            ENDIF
        NEXT

        // Set the result table as the current work area
        DbSelectArea(resultTable)

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
