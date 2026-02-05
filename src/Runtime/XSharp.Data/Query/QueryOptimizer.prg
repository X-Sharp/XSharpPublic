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
        // For elementary expressions (simple and composite), check if they depend on only one table
        IF expression IS SqlSimpleExpressionContext .OR. expression IS SqlCompositeExpressionContext
            LOCAL dependencies AS IList<STRING>
            dependencies := expression:GetTableDependencies(tableAliases)
            RETURN dependencies:Count <= 1  // Can optimize if depends on at most one table
        ENDIF

        // For parenthesized expressions, check the inner expression
        IF expression IS SqlParenExpressionContext VAR parenExpr
            RETURN IsOptimizable(parenExpr:Expr, tableAliases)
        ENDIF

        // For compare expressions, check if both operands are optimizable
        IF expression IS SqlCompareExpressionContext VAR compareExpr
            RETURN IsOptimizable(compareExpr:Left, tableAliases) .AND. IsOptimizable(compareExpr:Right, tableAliases)
        ENDIF

        // For logic expressions, check if both operands are optimizable
        IF expression IS SqlLogicExpressionContext VAR logicExpr
            RETURN IsOptimizable(logicExpr:Left, tableAliases) .AND. IsOptimizable(logicExpr:Right, tableAliases)
        ENDIF

        // For prefix expressions, check the inner expression
        IF expression IS SqlPrefixExpressionContext VAR prefixExpr
            RETURN IsOptimizable(prefixExpr:Expr, tableAliases)
        ENDIF

        // Default to not optimizable
        RETURN FALSE

    PRIVATE STATIC METHOD EvaluateOptimizableExpression(expression AS SqlExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        // Handle elementary expressions (simple and composite)
        IF expression IS SqlSimpleExpressionContext .OR. expression IS SqlCompositeExpressionContext
            RETURN EvaluateElementaryExpression(expression, recordCount, tableAliases)
        ENDIF

        // Handle parenthesized expressions
        IF expression IS SqlParenExpressionContext VAR parenExpr
            RETURN EvaluateOptimizableExpression(parenExpr:Expr, recordCount, tableAliases)
        ENDIF

        // Handle compare expressions
        IF expression IS SqlCompareExpressionContext VAR compareExpr
            RETURN EvaluateCompareExpression(compareExpr, recordCount, tableAliases)
        ENDIF

        // Handle logic expressions
        IF expression IS SqlLogicExpressionContext VAR logicExpr
            RETURN EvaluateLogicExpression(logicExpr, recordCount, tableAliases)
        ENDIF

        // Handle prefix expressions
        IF expression IS SqlPrefixExpressionContext VAR prefixExpr
            RETURN EvaluatePrefixExpression(prefixExpr, recordCount, tableAliases)
        ENDIF

        // Default case: return all records as matching
        LOCAL result AS RecordBitmap
        result := RecordBitmap{}
        result:Length := recordCount
        result:Fill()
        RETURN result

    PRIVATE STATIC METHOD EvaluateElementaryExpression(expression AS SqlExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL result AS RecordBitmap
        result := RecordBitmap{}
        result:Length := recordCount

        // Convert the expression to a string representation
        LOCAL exprStr AS STRING
        exprStr := AllTrim(expression:ToResolvedString(tableAliases))

        // In a full implementation, we would check if the expression matches an index
        // For now, we'll create a temporary index lookup or evaluate directly
        // Extract field name and value if this is a field=value expression
        result:Fill()  // For now, mark all records as matching

        RETURN result

    PRIVATE STATIC METHOD EvaluateCompareExpression(compareExpr AS SqlCompareExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL result AS RecordBitmap
        result := RecordBitmap{}
        result:Length := recordCount

        // Check if either operand matches an index
        LOCAL leftDependencies AS IList<STRING>
        LOCAL rightDependencies AS IList<STRING>
        leftDependencies := compareExpr:Left:GetTableDependencies(tableAliases)
        rightDependencies := compareExpr:Right:GetTableDependencies(tableAliases)

        // If one side is a field and the other is a constant, we might be able to use an index
        LOCAL leftExprStr AS STRING
        LOCAL rightExprStr AS STRING
        leftExprStr := AllTrim(compareExpr:Left:ToResolvedString(tableAliases))
        rightExprStr := AllTrim(compareExpr:Right:ToResolvedString(tableAliases))

        // For now, we'll evaluate the comparison directly
        // In a full implementation, we would check for index availability and use IndexLookup
        result:Fill()  // For now, mark all records as matching

        RETURN result

    PRIVATE STATIC METHOD EvaluateLogicExpression(logicExpr AS SqlLogicExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        LOCAL leftBitmap AS RecordBitmap
        LOCAL rightBitmap AS RecordBitmap
        leftBitmap := EvaluateOptimizableExpression(logicExpr:Left, recordCount, tableAliases)
        rightBitmap := EvaluateOptimizableExpression(logicExpr:Right, recordCount, tableAliases)

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
        LOCAL innerBitmap AS RecordBitmap
        innerBitmap := EvaluateOptimizableExpression(prefixExpr:Expr, recordCount, tableAliases)

        // Apply the prefix operator (currently only NOT is supported)
        SWITCH prefixExpr:Op:Type
        CASE XTokenType.NOT
            innerBitmap:Not()
        OTHERWISE
            NOP
        END SWITCH

        RETURN innerBitmap

    PUBLIC STATIC METHOD CreateOptimizedBitmap(tableName AS STRING, query AS SqlExpressionContext, recordCount AS LONG, tableAliases AS Dictionary<STRING, STRING>) AS RecordBitmap
        RETURN OptimizeQuery(query, recordCount, tableAliases)
    END METHOD

    PUBLIC STATIC METHOD CreateOptimizedBitmap(tableName AS STRING, query AS SqlExpressionContext, recordCount AS LONG) AS RecordBitmap
        LOCAL emptyTableAliases AS Dictionary<STRING, STRING>
        emptyTableAliases := Dictionary<STRING, STRING>{}
        RETURN OptimizeQuery(query, recordCount, emptyTableAliases)

    PRIVATE STATIC METHOD IndexExists(tableName AS STRING, fieldName AS STRING) AS LOGIC
        // In a full implementation, this would check the table metadata
        // to determine if an index exists for the specified field
        RETURN FALSE  // Placeholder implementation

    PRIVATE STATIC METHOD IndexLookup(tableName AS STRING, fieldName AS STRING, value AS OBJECT, recordCount AS LONG) AS RecordBitmap
        LOCAL result AS RecordBitmap
        result := RecordBitmap{}
        result:Length := recordCount

        // In a full implementation, this would use the index to quickly
        // identify which records match the specified value
        // For now, we'll return all records as non-matching to indicate no optimization
        result:Clear()

        RETURN result

END CLASS

END NAMESPACE
