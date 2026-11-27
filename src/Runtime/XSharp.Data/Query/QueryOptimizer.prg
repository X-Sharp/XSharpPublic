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

INTERNAL STATIC CLASS QueryOptimizerDiagnostics
    INTERNAL STATIC IndexEqualitySeeks AS INT
END CLASS

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
        CATCH AS Exception
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
        LOCAL savedFilterBlock := NULL AS ICodeblock
        LOCAL savedFilterText := "" AS STRING
        LOCAL savedRecordNumber := (LONG)(tableName)->RecNo() AS LONG
        VAR rddObject := DbInfo(DBI_RDD_OBJECT)
        IF rddObject IS Workarea VAR workarea
            IF workarea:FilterInfo != NULL
                savedFilterActive := workarea:FilterInfo:Active
                savedFilterBlock := workarea:FilterInfo:FilterBlock
                savedFilterText := workarea:FilterInfo:FilterText
            ENDIF
        ENDIF

        TRY
            // Match the scan fallback below: candidate generation evaluates
            // the SQL predicate over the full table, independent of a
            // pre-existing workarea filter. The original filter is restored
            // in FINALLY after candidate construction.
            (tableName)->DbSetFilter(NULL)

            // Check if there are any orders/indexes available for optimization
            LOCAL orderCount := DbOrderInfo(DBOI_ORDERCOUNT, 0, 0) as LONG

            IF orderCount > 0
                // Try to use index-based seek instead of full table scan with filter
                TRY
                    VAR bitmap := EvaluateWithIndex(compareExpr, recordCount, tableName, leftDeps, rightDeps)
                    IF bitmap != NULL
                        RETURN bitmap
                    ENDIF
                CATCH AS Exception
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
            CATCH AS Exception
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

            IF savedRecordNumber > 0
                (tableName)->DbGoto(savedRecordNumber)
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

        IF opText != XTokenType.EQ .AND. opText != XTokenType.LT .AND. ;
           opText != XTokenType.LTE .AND. opText != XTokenType.GT .AND. opText != XTokenType.GTE
            RETURN NULL
        ENDIF
        LOCAL invariantValue := NULL AS OBJECT
        LOCAL fieldName := "" AS STRING
        LOCAL fieldOnRight := FALSE AS LOGIC

        IF leftDeps:Count == 0 .AND. rightDeps:Count > 0
            // Right side is the field, left side is the value
            IF compareExpr:Left IS SqlSimpleExpressionContext VAR simpleLeft
                IF compareExpr:Right IS SqlNameExpressionContext VAR rightName
                    fieldName := rightName:Name
                ELSE
                    RETURN NULL
                ENDIF
                fieldOnRight := TRUE
                TRY
                    invariantValue := TryEvaluateIndexInvariant(compareExpr:Left)
                    IF invariantValue == NULL
                        RETURN NULL
                    ENDIF
                CATCH AS Exception
                    RETURN NULL  // Fall back to filter approach
                END TRY
            ELSE
                RETURN NULL
            ENDIF
        ELSEIF rightDeps:Count == 0 .AND. leftDeps:Count > 0
            // Left side is the field, right side is the value
            IF compareExpr:Right IS SqlSimpleExpressionContext VAR simpleRight
                IF compareExpr:Left IS SqlNameExpressionContext VAR leftName
                    fieldName := leftName:Name
                ELSE
                    RETURN NULL
                ENDIF
                TRY
                    invariantValue := TryEvaluateIndexInvariant(compareExpr:Right)
                    IF invariantValue == NULL
                        RETURN NULL
                    ENDIF
                CATCH AS Exception
                    RETURN NULL  // Fall back to filter approach
                END TRY
            ELSE
                RETURN NULL
            ENDIF
        ELSE
            RETURN NULL  // Both sides depend on table or neither does - can't optimize
        ENDIF

        IF invariantValue == NULL
            RETURN NULL
        ENDIF
        IF String.IsNullOrEmpty(fieldName)
            RETURN NULL
        ENDIF

        IF fieldOnRight
            SWITCH opText
            CASE XTokenType.LT; opText := XTokenType.GT
            CASE XTokenType.GT; opText := XTokenType.LT
            CASE XTokenType.LTE; opText := XTokenType.GTE
            CASE XTokenType.GTE; opText := XTokenType.LTE
            END SWITCH
        ENDIF

        // A seek uses the currently selected order. Do not use it unless that
        // order is a plain, unfiltered key on this exact field; a different,
        // composite, expression, or conditional order could produce false
        // negatives in the candidate bitmap.
        IF !IsActiveSimpleFieldOrder(tableName, fieldName)
            RETURN NULL
        ENDIF
        System.Threading.Interlocked.Increment(REF QueryOptimizerDiagnostics.IndexEqualitySeeks)

        LOCAL descending := (LOGIC)(tableName)->DbOrderInfo(DBOI_ISDESC, "", NIL) AS LOGIC

        IF opText == XTokenType.EQ
            // Seek both ends of the duplicate run. The bitmap remains keyed by
            // physical RecNo so results from different index orders compose.
            CoreDb.Seek(invariantValue, FALSE, FALSE)
            IF !CoreDb.Found()
                RETURN result
            ENDIF
            LOCAL firstRecNo := (LONG)CoreDb.Recno() AS LONG
            CoreDb.Seek(invariantValue, FALSE, TRUE)
            IF !CoreDb.Found()
                RETURN NULL
            ENDIF
            LOCAL lastRecNo := (LONG)CoreDb.Recno() AS LONG
            CoreDb.Seek(invariantValue, FALSE, FALSE)
            IF (LONG)CoreDb.Recno() != firstRecNo
                RETURN NULL
            ENDIF
            // DbSkip(1) always advances to the next entry in the active order,
            // including when that order is descending. The key direction only
            // changes how ranges are traversed; it must not reverse a duplicate
            // run walk between the first and last seek results.
            LOCAL traversed AS LONG
            traversed := 0
            LOCAL reachedLast AS LOGIC
            reachedLast := FALSE
            DO WHILE traversed < recordCount .AND. !CoreDb.Eof() .AND. !CoreDb.Bof()
                VAR physicalRecNo := (LONG)CoreDb.Recno()
                result:SetMatchState(physicalRecNo, RecordBitmap.RecordMatchState.Match)
                IF physicalRecNo == lastRecNo
                    reachedLast := TRUE
                    EXIT
                ENDIF
                CoreDb.Skip(1)
                traversed++
            ENDDO
            // Do not return a partial bitmap if the index navigation did not
            // reach the endpoint. The caller will use the safe scan fallback.
            IF !reachedLast
                RETURN NULL
            ENDIF
            RETURN result
        ENDIF

        LOCAL skipDirection := 1 AS LONG
        LOCAL seekLast := FALSE AS LOGIC
        SWITCH opText
        CASE XTokenType.LT
            seekLast := FALSE
            skipDirection := -1
        CASE XTokenType.LTE
            seekLast := TRUE
            skipDirection := -1
        CASE XTokenType.GT
            seekLast := TRUE
            skipDirection := 1
        CASE XTokenType.GTE
            seekLast := FALSE
            skipDirection := 1
        END SWITCH
        CoreDb.Seek(invariantValue, TRUE, seekLast)
        IF descending
            seekLast := !seekLast
            skipDirection := -skipDirection
        ENDIF
        IF opText == XTokenType.LT .OR. opText == XTokenType.GT
            CoreDb.Skip(skipDirection)
        ENDIF
        DO WHILE !CoreDb.Eof() .AND. !CoreDb.Bof()
            VAR physicalRecNo := (LONG)CoreDb.Recno()
            result:SetMatchState(physicalRecNo, RecordBitmap.RecordMatchState.Match)
            CoreDb.Skip(skipDirection)
        ENDDO

        RETURN result

    PRIVATE STATIC METHOD TryEvaluateIndexInvariant(expression AS SqlExpressionContext) AS OBJECT
        IF expression IS NOT SqlSimpleExpressionContext
            RETURN NULL
        ENDIF
        TRY
            VAR evaluated := Evaluate(expression:ToString())
            IF IsNil(evaluated) .OR. ((USUAL)evaluated):IsNull
                RETURN NULL
            ENDIF
            RETURN (OBJECT)evaluated
        CATCH AS Exception
            RETURN NULL
        END TRY

    PRIVATE STATIC METHOD IsActiveSimpleFieldOrder(tableName AS STRING, fieldName AS STRING) AS LOGIC
        TRY
            VAR orderNumber := (LONG)(tableName)->DbOrderInfo(DBOI_NUMBER, "", NIL)
            IF orderNumber <= 0
                RETURN FALSE
            ENDIF
            VAR keyExpression := (USUAL)(tableName)->DbOrderInfo(DBOI_EXPRESSION, "", orderNumber)
            VAR condition := (USUAL)(tableName)->DbOrderInfo(DBOI_CONDITION, "", orderNumber)
            IF keyExpression:IsNull
                RETURN FALSE
            ENDIF
            IF !condition:IsNull .AND. !String.IsNullOrWhiteSpace(condition:ToString())
                RETURN FALSE
            ENDIF
            LOCAL matchesField := String.Equals(keyExpression:ToString():Trim(), fieldName:Trim(), StringComparison.OrdinalIgnoreCase) AS LOGIC
            IF !matchesField
                RETURN FALSE
            ENDIF
            RETURN TRUE
        CATCH AS Exception
            RETURN FALSE
        END TRY

END CLASS

END NAMESPACE

