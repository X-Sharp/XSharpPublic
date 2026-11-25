//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Linq
USING XSharp.Parsers
USING XSharp.Data
USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.Data.Query

PUBLIC CLASS QueryOptimizer

    PUBLIC STATIC METHOD OptimizeQuery(query AS SqlExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL result AS RecordBitmap
        result := RecordBitmap{}
        result:Length := recordCount

        // If there's no query filter, all records match
        IF query == NULL
            result:Fill()
            RETURN result
        ENDIF

        // Determine if the expression is optimizable
        IF IsOptimizable(query, tableAliases)
            RETURN EvaluateOptimizableExpression(query, recordCount, tableAliases)
        ELSE
            // If not fully optimizable, return a bitmap with all records marked for evaluation
            result:Fill()
            RETURN result
        ENDIF

    PUBLIC STATIC METHOD OptimizeQuery(query AS SqlExpressionContext, recordCount AS LONG) AS RecordBitmap
        LOCAL emptyTableAliases AS Dictionary<STRING, STRING>
        emptyTableAliases := Dictionary<STRING, STRING>{}
        RETURN OptimizeQuery(query, recordCount, emptyTableAliases)

    /// <summary>
    /// Builds per-table candidate bitmaps from safe predicates in a multi-table WHERE expression.
    /// The caller must still evaluate the complete expression for every remaining row combination.
    /// </summary>
    PUBLIC STATIC METHOD OptimizeMultiTableQuery(query AS SqlExpressionContext, ;
        tableAliases AS Dictionary<STRING, STRING>, ;
        tableRecordCounts AS Dictionary<STRING, LONG>) AS QueryOptimizationPlan

        LOCAL plan := QueryOptimizationPlan{} AS QueryOptimizationPlan
        IF query != NULL
            CollectTableCandidates(query, tableAliases, tableRecordCounts, plan)
        ENDIF
        RETURN plan

    PRIVATE STATIC METHOD CollectTableCandidates(expression AS SqlExpressionContext, ;
        tableAliases AS Dictionary<STRING, STRING>, ;
        tableRecordCounts AS Dictionary<STRING, LONG>, ;
        plan AS QueryOptimizationPlan) AS VOID

        // Parentheses do not change which table domains a safe candidate can constrain.
        IF expression IS SqlParenExpressionContext VAR parenExpr
            CollectTableCandidates(parenExpr:Expr, tableAliases, tableRecordCounts, plan)
            RETURN
        ENDIF

        IF expression IS SqlLogicExpressionContext VAR logicExpr
            // A same-table logical subtree has an exact bitmap representation. If an AND
            // spans tables, visit each side independently to extract safe conjuncts.
            IF IsOptimizable(expression, tableAliases)
                AddTableCandidate(expression, tableAliases, tableRecordCounts, plan)
            ELSEIF logicExpr:Op:Type == XTokenType.AND
                CollectTableCandidates(logicExpr:Left, tableAliases, tableRecordCounts, plan)
                CollectTableCandidates(logicExpr:Right, tableAliases, tableRecordCounts, plan)
            ENDIF
            RETURN
        ENDIF

        AddTableCandidate(expression, tableAliases, tableRecordCounts, plan)
        RETURN

    PRIVATE STATIC METHOD AddTableCandidate(expression AS SqlExpressionContext, ;
        tableAliases AS Dictionary<STRING, STRING>, ;
        tableRecordCounts AS Dictionary<STRING, LONG>, ;
        plan AS QueryOptimizationPlan) AS VOID

        IF !IsOptimizable(expression, tableAliases)
            RETURN
        ENDIF

        VAR dependencies := expression:GetTableDependencies(tableAliases)
        IF dependencies:Count != 1
            RETURN
        ENDIF

        LOCAL recordCount AS LONG
        IF !tableRecordCounts:TryGetValue(dependencies[0], OUT recordCount)
            RETURN
        ENDIF

        VAR candidateBitmap := EvaluateOptimizableExpression(expression, recordCount, tableAliases)
        LOCAL existingBitmap AS RecordBitmap
        IF plan:CandidateBitmaps:TryGetValue(dependencies[0], OUT existingBitmap)
            existingBitmap:And(candidateBitmap)
        ELSE
            plan:CandidateBitmaps[dependencies[0]] := candidateBitmap
        ENDIF
        RETURN

    PRIVATE STATIC METHOD IsOptimizable(expression AS SqlExpressionContext, tableAliases AS Dictionary<STRING, STRING>) AS LOGIC
        // An expression is optimizable if it depends on at most one table and contains comparisons
        IF expression IS SqlSimpleExpressionContext .OR. expression IS SqlCompositeExpressionContext
            LOCAL dependencies := expression:GetTableDependencies(tableAliases) as IList<STRING>
            RETURN dependencies:Count <= 1
        ENDIF

        // For parenthesized expressions, check the inner expression
        IF expression IS SqlParenExpressionContext VAR parenExpr
            RETURN IsOptimizable(parenExpr:Expr, tableAliases)
        ENDIF

        // For compare and logic expressions, check if both sides are optimizable
        IF expression IS SqlCompareExpressionContext VAR compExpr
            VAR leftInvariant := compExpr:Left:GetTableDependencies(tableAliases):Count == 0
            VAR rightInvariant := compExpr:Right:GetTableDependencies(tableAliases):Count == 0
            VAR leftOpt := IsOptimizable(compExpr:Left, tableAliases)
            VAR rightOpt := IsOptimizable(compExpr:Right, tableAliases)
            VAR dependencies := compExpr:GetTableDependencies(tableAliases)
            RETURN leftOpt .AND. rightOpt .AND. dependencies:Count == 1 .AND. (leftInvariant .OR. rightInvariant)
        ENDIF

        IF expression IS SqlLogicExpressionContext VAR logicExpr
            VAR leftOpt := IsOptimizable(logicExpr:Left, tableAliases)
            VAR rightOpt := IsOptimizable(logicExpr:Right, tableAliases)

            IF leftOpt .AND. rightOpt
                // A single RecordBitmap indexes one table's record numbers. Combining
                // predicates is safe only when both sides share that same table domain.
                VAR dependencies := logicExpr:GetTableDependencies(tableAliases)
                RETURN dependencies:Count == 1
            ENDIF
            RETURN FALSE
        ENDIF

        // For prefix expressions, check the inner expression
        IF expression IS SqlPrefixExpressionContext VAR prefixExpr
            RETURN IsOptimizable(prefixExpr:Expr, tableAliases)
        ENDIF

        // Default to not optimizable
        RETURN FALSE

    PRIVATE STATIC METHOD EvaluateOptimizableExpression(expression AS SqlExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL result := RecordBitmap{} as RecordBitmap
        result:Length := recordCount

        TRY
            // Handle compare expressions (field = value, field > value, etc.)
            IF expression IS SqlCompareExpressionContext VAR compExpr
                RETURN EvaluateCompareExpression(compExpr, recordCount, tableAliases)
            ENDIF

            // Handle parenthesized expressions
            IF expression IS SqlParenExpressionContext VAR parenExpr
                RETURN EvaluateOptimizableExpression(parenExpr:Expr, recordCount, tableAliases)
            ENDIF

            // Handle logic expressions (AND, OR)
            IF expression IS SqlLogicExpressionContext VAR logicExpr
                RETURN EvaluateLogicExpression(logicExpr, recordCount, tableAliases)
            ENDIF

            // Handle prefix expressions (NOT)
            IF expression IS SqlPrefixExpressionContext VAR prefixExpr
                RETURN EvaluatePrefixExpression(prefixExpr, recordCount, tableAliases)
            ENDIF

            // For other cases, evaluate using codeblock fallback
            result:Fill()
        CATCH ex AS Exception
            result:Fill()  // On error, include all records (safe fallback)
        END TRY

        RETURN result

    PRIVATE STATIC METHOD EvaluateCompareExpression(compareExpr AS SqlCompareExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL result := RecordBitmap{} as RecordBitmap
        result:Length := recordCount

        // Check dependencies - expression should depend on at most one table
        LOCAL leftDeps := compareExpr:Left:GetTableDependencies(tableAliases) as IList<STRING>
        LOCAL rightDeps := compareExpr:Right:GetTableDependencies(tableAliases) as IList<STRING>

        LOCAL totalDeps := List<STRING>{} as List<STRING>
        FOREACH VAR dep IN leftDeps
            IF !totalDeps:Contains(dep)
                totalDeps:Add(dep)
            ENDIF
        NEXT
        FOREACH VAR dep IN rightDeps
            IF !totalDeps:Contains(dep)
                totalDeps:Add(dep)
            ENDIF
        NEXT

        // Can only optimize if expression depends on single table
        IF totalDeps:Count > 1
            result:Fill()
            RETURN result
        ENDIF

        // Determine which side is the field (table-dependent) and which is invariant
        LOCAL fieldName := "" AS STRING
        LOCAL tableName := "" AS STRING
        LOCAL invariantExpr := NULL AS SqlExpressionContext
        LOCAL fieldOnRight := FALSE AS LOGIC

        IF leftDeps:Count == 0 .AND. rightDeps:Count > 0
            // Right side is the field, left side is invariant value
            IF compareExpr:Left IS SqlSimpleExpressionContext VAR simpleLeft
                fieldName := compareExpr:Right:ToString()
                tableName := rightDeps[0]
                invariantExpr := compareExpr:Left
                fieldOnRight := TRUE
            ENDIF
        ELSEIF rightDeps:Count == 0 .AND. leftDeps:Count > 0
            // Left side is the field, right side is invariant value
            IF compareExpr:Right IS SqlSimpleExpressionContext VAR simpleRight
                fieldName := compareExpr:Left:ToString()
                tableName := leftDeps[0]
                invariantExpr := compareExpr:Right
            ENDIF
        ENDIF

        // If we couldn't identify field and invariant, fall back to full evaluation
        IF invariantExpr == NULL
            result:Fill()
            RETURN result
        ENDIF

        // Get table name if not already resolved
        IF String.IsNullOrEmpty(tableName)
            IF totalDeps:Count > 0
                tableName := totalDeps[0]
            ELSE
                result:Fill()
                RETURN result
            ENDIF
        ENDIF

        // Save current workarea and open target table
        LOCAL savedArea := DbSelect() as DWORD

        IF !DbSelectArea(tableName)
            IF !DbUseArea(TRUE, "DBFVFP", tableName, tableName, TRUE, FALSE)
                result:Fill()
                RETURN result
            ENDIF
        ENDIF

        LOCAL savedFilterActive := FALSE AS LOGIC
        LOCAL savedFilterBlock AS ICodeblock
        LOCAL savedFilterText := "" AS STRING
        VAR rddObject := DbInfo(DBI_RDD_OBJECT)
        IF rddObject IS Workarea VAR workarea
            IF workarea:FilterInfo != NULL
                savedFilterActive := workarea:FilterInfo:Active
                savedFilterBlock := workarea:FilterInfo:FilterBlock
                savedFilterText := workarea:FilterInfo:FilterText
            ENDIF
        ENDIF

        TRY
            // Check if there are any orders/indexes available for optimization
            LOCAL orderCount := DbOrderInfo(DBOI_ORDERCOUNT, 0, 0) as LONG

            IF orderCount > 0
                // Try to use index-based seek instead of full table scan with filter
                TRY
                    VAR bitmap := EvaluateWithIndex(compareExpr, recordCount, tableName, leftDeps, rightDeps)
                    IF bitmap != NULL
                        RETURN bitmap
                    ENDIF
                CATCH ex AS Exception
                    // If index-based evaluation fails, fall back to filter approach
                    NOP
                END TRY
            ENDIF

            // Build filter expression by replacing field reference with actual table->field syntax
            LOCAL filterExpr := invariantExpr:ToResolvedString(tableAliases) as STRING
            LOCAL opText := compareExpr:Op:Type as XTokenType
            // The filter is evaluated in one table's work area, so use the bare
            // field name instead of a source alias/table qualifier.
            LOCAL fieldExpression AS SqlExpressionContext
            IF fieldOnRight
                fieldExpression := compareExpr:Right
            ELSE
                fieldExpression := compareExpr:Left
            ENDIF
            IF fieldExpression IS SqlNameExpressionContext VAR fieldNameExpression
                fieldName := fieldNameExpression:Name
            ENDIF

            // If the field is on the right, reverse directional comparisons so
            // the generated filter remains equivalent to the original predicate.
            IF fieldOnRight
                SWITCH opText
                CASE XTokenType.LT; opText := XTokenType.GT
                CASE XTokenType.GT; opText := XTokenType.LT
                CASE XTokenType.LTE; opText := XTokenType.GTE
                CASE XTokenType.GTE; opText := XTokenType.LTE
                END SWITCH
            ENDIF
            LOCAL opSymbol := "" AS STRING

            SWITCH opText
            CASE XTokenType.EQ
                opSymbol := "="
            CASE XTokenType.NEQ
                opSymbol := "<>"
            CASE XTokenType.LT
                opSymbol := "<"
            CASE XTokenType.GT
                opSymbol := ">"
            CASE XTokenType.LTE
                opSymbol := "<="
            CASE XTokenType.GTE
                opSymbol := ">="
            END SWITCH

            // Build the filter: unqualified field op invariant value.
            LOCAL resolvedField := fieldName AS STRING

            // Use MCompile to evaluate the invariant expression and build filter
            LOCAL macroValue := "" AS STRING
            TRY
                VAR cb := MCompile(invariantExpr:ToString())
                IF cb != NULL
                    LOCAL val := (OBJECT)cb:Eval(NULL) as OBJECT
                    IF val != NULL
                        macroValue := IIF(UsualType(val) == __UsualType.String, ;
                            "\"" + AllTrim((STRING)val) + \""", val:ToString())
                    ENDIF
                ENDIF
            CATCH ex AS Exception
                // If macro evaluation fails, use the resolved string
                macroValue := invariantExpr:ToResolvedString(tableAliases)
            END TRY

            filterExpr := resolvedField + " " + opSymbol + " " + macroValue

            // Apply filter and scan records
            DbSetFilter(NULL)

            (tableName)->DbSetFilter(filterExpr)
            (tableName)->DbGoTop()

            DO WHILE !(tableName)->Eof()
                VAR recNo := (LONG)(tableName)->RecNo()
                result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                (tableName)->DbSkip(1)
                IF (tableName)->Eof()
                    EXIT
                ENDIF
            ENDDO

        FINALLY
            // Restore the caller's table filter after the temporary SQL predicate.
            IF savedFilterActive
                (tableName)->DbSetFilter(savedFilterBlock, savedFilterText)
            ELSE
                (tableName)->DbSetFilter(NULL)
            ENDIF

            // Restore workarea
            IF savedArea > 0
                DbSelectArea(savedArea)
            ELSE
                DbCloseArea()
            ENDIF
        END TRY

        RETURN result

    PRIVATE STATIC METHOD EvaluateLogicExpression(logicExpr AS SqlLogicExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL leftBitmap := EvaluateOptimizableExpression(logicExpr:Left, recordCount, tableAliases) as RecordBitmap
        LOCAL rightBitmap := EvaluateOptimizableExpression(logicExpr:Right, recordCount, tableAliases) as RecordBitmap

        // Apply the logical operator to combine the bitmaps
        SWITCH logicExpr:Op:Type
        CASE XTokenType.AND
            leftBitmap:And(rightBitmap)
        CASE XTokenType.OR
            leftBitmap:Or(rightBitmap)
        OTHERWISE
            leftBitmap:And(rightBitmap)  // Default to AND
        END SWITCH

        RETURN leftBitmap

PRIVATE STATIC METHOD EvaluatePrefixExpression(prefixExpr AS SqlPrefixExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL innerBitmap := EvaluateOptimizableExpression(prefixExpr:Expr, recordCount, tableAliases) as RecordBitmap

        // Apply the prefix operator (currently only NOT is supported)
        SWITCH prefixExpr:Op:Type
        CASE XTokenType.NOT
            innerBitmap:Not()
        OTHERWISE
            NOP
        END SWITCH

        RETURN innerBitmap

    PRIVATE STATIC METHOD EvaluateWithIndex(compareExpr AS SqlCompareExpressionContext, recordCount AS LONG, tableName AS STRING, leftDeps AS IList<STRING>, rightDeps AS IList<STRING>) AS RecordBitmap
        LOCAL result := RecordBitmap{} as RecordBitmap
        result:Length := recordCount

        // Determine which side is the field (table-dependent) and which is invariant value
        LOCAL opText := compareExpr:Op:Type as XTokenType

        // Only optimize for equality (=), range operators (<, <=, >, >=)
        IF opText != XTokenType.EQ .AND. opText != XTokenType.LT .AND. opText != XTokenType.GT .AND. opText != XTokenType.LTE .AND. opText != XTokenType.GTE
            RETURN NULL  // Fall back to filter approach for other operators like <>
        ENDIF

        LOCAL invariantValue := NULL AS OBJECT
        LOCAL fieldName := "" AS STRING

        IF leftDeps:Count == 0 .AND. rightDeps:Count > 0
            // Right side is the field, left side is the value
            IF compareExpr:Left IS SqlSimpleExpressionContext VAR simpleLeft
                fieldName := compareExpr:Right:ToString()
                TRY
                    VAR cb := MCompile(compareExpr:Left:ToString())
                    IF cb != NULL
                        invariantValue := (OBJECT)cb:Eval(NULL)
                    ENDIF
                CATCH ex AS Exception
                    RETURN NULL  // Fall back to filter approach
                END TRY
            ELSE
                RETURN NULL
            ENDIF
        ELSEIF rightDeps:Count == 0 .AND. leftDeps:Count > 0
            // Left side is the field, right side is the value
            IF compareExpr:Right IS SqlSimpleExpressionContext VAR simpleRight
                fieldName := compareExpr:Left:ToString()
                TRY
                    VAR cb := MCompile(compareExpr:Right:ToString())
                    IF cb != NULL
                        invariantValue := (OBJECT)cb:Eval(NULL)
                    ENDIF
                CATCH ex AS Exception
                    RETURN NULL  // Fall back to filter approach
                END TRY
            ELSE
                RETURN NULL
            ENDIF
        ELSE
            RETURN NULL  // Both sides depend on table or neither does - can't optimize
        ENDIF

        IF invariantValue == NULL .OR. String.IsNullOrEmpty(fieldName)
            RETURN NULL
        ENDIF

        // Try to use CoreDb.Seek with the index
        LOCAL lSoftSeek := FALSE AS LOGIC
        LOCAL lLast := FALSE AS LOGIC

        SWITCH opText
        CASE XTokenType.EQ
            lSoftSeek := FALSE
            lLast := FALSE
        CASE XTokenType.LT
            lSoftSeek := TRUE
            lLast := FALSE
        CASE XTokenType.LTE
            lSoftSeek := TRUE
            lLast := TRUE
        CASE XTokenType.GT
            lSoftSeek := TRUE
            lLast := FALSE
        CASE XTokenType.GTE
            lSoftSeek := TRUE
            lLast := FALSE
        END SWITCH

        LOCAL success := CoreDb.Seek(invariantValue, lSoftSeek, lLast) as LOGIC

        IF !success
            // Seek failed (record not found or other issue)
            // For soft seek with LT/GTE operators, we might still have positioned somewhere
            // Let's check if we're at EOF or BOF
            IF CoreDb.Eof() .OR. CoreDb.Bof()
                RETURN result  // Empty bitmap
            ENDIF
        ENDIF

        // Build the bitmap by traversing from current position based on operator
        SWITCH opText
        CASE XTokenType.EQ
            // For equality: only mark the record we seeked to (if found)
            IF success .AND. !CoreDb.Eof()
                VAR recNo := (LONG)CoreDb.Recno()
                result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
            ENDIF

        CASE XTokenType.LT
            // For less than: mark all records from BOF up to (but not including) current position
            IF !CoreDb.Bof() .AND. !CoreDb.Eof()
                VAR currentPosition := CoreDb.Recno()
                CoreDb.GoTop()
                DO WHILE !CoreDb.Eof() .AND. CoreDb.Recno() < currentPosition .AND. CoreDb.Recno() <= result:Length
                    VAR recNo := (LONG)CoreDb.Recno()
                    result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                    CoreDb.Skip(1)
                ENDDO
            ELSEIF CoreDb.Eof()
                // All records match (cursor at EOF means all records are less than value)
                DO WHILE !CoreDb.Bof()
                    VAR recNo := (LONG)CoreDb.Recno()
                    result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                    CoreDb.Skip(-1)
                ENDDO
            ENDIF

        CASE XTokenType.LTE
            // For less than or equal: mark all records from BOF up to current position (inclusive)
            IF !CoreDb.Bof()
                VAR currentPosition := CoreDb.Recno()
                CoreDb.GoTop()
                DO WHILE !CoreDb.Eof() .AND. CoreDb.Recno() <= currentPosition .AND. CoreDb.Recno() <= result:Length
                    VAR recNo := (LONG)CoreDb.Recno()
                    result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                    CoreDb.Skip(1)
                ENDDO
            ENDIF

        CASE XTokenType.GT
            // For greater than: skip past current position first, then mark all records to EOF
            IF !CoreDb.Eof()
                CoreDb.Skip(1)  // Skip the record we're currently on (might be == value)
                DO WHILE !CoreDb.Eof() .AND. CoreDb.Recno() <= result:Length
                    VAR recNo := (LONG)CoreDb.Recno()
                    result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                    CoreDb.Skip(1)
                ENDDO
            ENDIF

        CASE XTokenType.GTE
            // For greater than or equal: mark all records from current position to EOF (inclusive)
            IF !CoreDb.Eof()
                DO WHILE !CoreDb.Eof() .AND. CoreDb.Recno() <= result:Length
                    VAR recNo := (LONG)CoreDb.Recno()
                    result:SetMatchState(recNo, RecordBitmap.RecordMatchState.Match)
                    CoreDb.Skip(1)
                ENDDO
            ENDIF
        END SWITCH

        RETURN result

END CLASS

END NAMESPACE

