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
            RETURN leftOpt .AND. rightOpt .AND. (leftInvariant .OR. rightInvariant)
        ENDIF

        IF expression IS SqlLogicExpressionContext VAR logicExpr
            VAR leftOpt := IsOptimizable(logicExpr:Left, tableAliases)
            VAR rightOpt := IsOptimizable(logicExpr:Right, tableAliases)
            RETURN leftOpt .AND. rightOpt
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

        IF leftDeps:Count == 0 .AND. rightDeps:Count > 0
            // Right side is the field, left side is invariant value
            IF compareExpr:Left IS SqlSimpleExpressionContext VAR simpleLeft
                fieldName := compareExpr:Right:ToString()
                tableName := rightDeps[0]
                invariantExpr := compareExpr:Left
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

        // Build filter expression by replacing field reference with actual table->field syntax
        LOCAL filterExpr := invariantExpr:ToResolvedString(tableAliases) as STRING
        LOCAL opText := compareExpr:Op:Type as XTokenType
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

        // Build the filter: field op invariant_value
        LOCAL resolvedField := "" AS STRING
        IF String.IsNullOrEmpty(compareExpr:Left:ToString())
            resolvedField := compareExpr:Right:ToResolvedString(tableAliases)
        ELSE
            resolvedField := compareExpr:Left:ToResolvedString(tableAliases)
        ENDIF

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

        // Save current workarea and open target table
        LOCAL savedArea := DbSelect() as DWORD

        IF !DbSelectArea(tableName)
            IF !DbUseArea(TRUE, "DBFVFP", tableName, tableName, TRUE, FALSE)
                result:Fill()
                RETURN result
            ENDIF
        ENDIF

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

        // Clear filter and restore workarea
        (tableName)->DbSetFilter(NULL)

        IF savedArea > 0
            DbSelectArea(savedArea)
        ELSE
            DbCloseArea()
        ENDIF

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

END CLASS

END NAMESPACE

