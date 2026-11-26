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
using XSharp.RDD.Enums
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

INTERNAL CLASS SqlAggregateState
    PUBLIC PROPERTY FunctionName AS STRING AUTO
    PUBLIC PROPERTY Count AS LONG AUTO
    PUBLIC PROPERTY NumericTotal AS FLOAT AUTO
    PUBLIC PROPERTY Value AS USUAL AUTO
    PUBLIC PROPERTY HasValue AS LOGIC AUTO
    INTERNAL CONSTRUCTOR(functionName AS STRING)
        SELF:FunctionName := functionName
        SELF:Count := 0
        SELF:NumericTotal := 0
        SELF:HasValue := FALSE
        RETURN
END CLASS

INTERNAL CLASS SqlAggregateGroup
    PUBLIC PROPERTY KeyValues AS USUAL[] AUTO
    PUBLIC PROPERTY States AS Dictionary<SqlAggregateExpressionContext,SqlAggregateState> AUTO
    PUBLIC PROPERTY Sequence AS LONG AUTO
    PUBLIC PROPERTY InputCount AS LONG AUTO
    INTERNAL CONSTRUCTOR(keyValues AS USUAL[], aggregates AS IList<SqlAggregateExpressionContext>, sequence AS LONG)
        SELF:KeyValues := keyValues
        SELF:Sequence := sequence
        SELF:InputCount := 0
        SELF:States := Dictionary<SqlAggregateExpressionContext,SqlAggregateState>{}
        FOREACH VAR aggregate IN aggregates
            SELF:States[aggregate] := SqlAggregateState{aggregate:FunctionName}
        NEXT
        RETURN
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

    PRIVATE STATIC METHOD CollectAggregates(expression AS SqlExpressionContext, aggregates AS List<SqlAggregateExpressionContext>, insideAggregate := FALSE AS LOGIC) AS VOID
        IF expression == NULL
            RETURN
        ENDIF
        IF expression IS SqlAggregateExpressionContext VAR aggregate
            IF insideAggregate
                THROW Error{"Nested aggregate functions are not supported"}
            ENDIF
            IF !aggregates:Contains(aggregate)
                aggregates:Add(aggregate)
            ENDIF
            IF aggregate:Argument != NULL
                CollectAggregates(aggregate:Argument, aggregates, TRUE)
            ENDIF
        ELSEIF expression IS SqlCompositeExpressionContext VAR composite
            FOREACH VAR child IN composite:Exprs
                CollectAggregates(child, aggregates, insideAggregate)
            NEXT
        ELSEIF expression IS SqlParenExpressionContext VAR parenthesized
            CollectAggregates(parenthesized:Expr, aggregates, insideAggregate)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binary
            CollectAggregates(binary:Left, aggregates, insideAggregate)
            CollectAggregates(binary:Right, aggregates, insideAggregate)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefix
            CollectAggregates(prefix:Expr, aggregates, insideAggregate)
        ENDIF
        RETURN

    PRIVATE STATIC METHOD AggregateLiteral(value AS USUAL) AS STRING
        IF value:IsNull
            RETURN "NIL"
        ENDIF
        SWITCH UsualType(value)
        CASE __UsualType.String
        CASE __UsualType.Memo
            RETURN Chr(34) + value:ToString():Replace(Chr(34), Chr(34) + Chr(34)) + Chr(34)
        CASE __UsualType.Logic
            RETURN IIF((LOGIC)value, ".T.", ".F.")
        CASE __UsualType.Date
            RETURN "CTOD(" + Chr(34) + value:ToString():Replace(Chr(34), Chr(34) + Chr(34)) + Chr(34) + ")"
        OTHERWISE
            RETURN value:ToString():Replace(",", ".")
        END SWITCH

    PRIVATE STATIC METHOD GroupKey(groupValues AS USUAL[]) AS STRING
        VAR key := StringBuilder{}
        FOR LOCAL i := 1 AS INT TO groupValues:Length
            VAR value := groupValues[i]
            VAR textValue := IIF(value:IsNull, "", value:ToString())
            IF UsualType(value) == __UsualType.String
                textValue := AllTrim(textValue):ToUpperInvariant()
            ELSEIF !value:IsNull .AND. IsAggregateNumeric(value)
                textValue := ((FLOAT)value):ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            ENDIF
            key:Append(UsualType(value):ToString())
            key:Append(":")
            key:Append(textValue:Length)
            key:Append(":")
            key:Append(textValue)
            key:Append("|")
        NEXT
        RETURN key:ToString()

    PRIVATE STATIC METHOD IsAggregateNumeric(value AS USUAL) AS LOGIC
        SWITCH UsualType(value)
        CASE __UsualType.Long
        CASE __UsualType.Float
        CASE __UsualType.Fixed
        CASE __UsualType.Byte
        CASE __UsualType.ShortInt
        CASE __UsualType.Word
        CASE __UsualType.DWord
        CASE __UsualType.Real4
        CASE __UsualType.Real8
        CASE __UsualType.Int64
        CASE __UsualType.UInt64
        CASE __UsualType.Decimal
        CASE __UsualType.Currency
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH

    PRIVATE STATIC METHOD UpdateAggregate(state AS SqlAggregateState, value AS USUAL, isStar AS LOGIC) AS VOID
        IF state:FunctionName == "COUNT" .OR. state:FunctionName == "CNT"
            IF isStar .OR. !value:IsNull
                state:Count++
            ENDIF
            RETURN
        ENDIF
        IF value:IsNull
            RETURN
        ENDIF
        VAR valueType := UsualType(value)
        IF state:FunctionName == "SUM" .OR. state:FunctionName == "AVG"
            SWITCH valueType
            CASE __UsualType.Long
            CASE __UsualType.Float
            CASE __UsualType.Fixed
            CASE __UsualType.Byte
            CASE __UsualType.ShortInt
            CASE __UsualType.Word
            CASE __UsualType.DWord
            CASE __UsualType.Real4
            CASE __UsualType.Real8
            CASE __UsualType.Int64
            CASE __UsualType.UInt64
            CASE __UsualType.Decimal
            CASE __UsualType.Currency
                state:NumericTotal += (FLOAT)value
                state:Count++
            OTHERWISE
                THROW Error{i"{state:FunctionName} requires a numeric expression"}
            END SWITCH
            RETURN
        ENDIF
        IF !state:HasValue
            state:Value := value
            state:HasValue := TRUE
            RETURN
        ENDIF
        VAR comparison := value:CompareTo(state:Value)
        IF (state:FunctionName == "MIN" .AND. comparison < 0) .OR. ;
           (state:FunctionName == "MAX" .AND. comparison > 0)
            state:Value := value
        ENDIF
        state:Count++
        RETURN

    PRIVATE STATIC METHOD AggregateValue(aggregate AS SqlAggregateExpressionContext, group AS SqlAggregateGroup) AS USUAL
        VAR state := group:States[aggregate]
        IF group:InputCount == 0
            RETURN NIL
        ENDIF
        SWITCH state:FunctionName
        CASE "COUNT"
        CASE "CNT"
            RETURN state:Count
        CASE "SUM"
            IF state:Count == 0
                RETURN NIL
            ENDIF
            RETURN state:NumericTotal
        CASE "AVG"
            IF state:Count == 0
                RETURN NIL
            ENDIF
            RETURN state:NumericTotal / state:Count
        CASE "MIN"
        CASE "MAX"
            IF !state:HasValue
                RETURN NIL
            ENDIF
            RETURN state:Value
        OTHERWISE
            THROW Error{"Unsupported aggregate function: " + state:FunctionName}
        END SWITCH

    PRIVATE STATIC METHOD GroupExpressionValue(expression AS SqlExpressionContext, selectCtx AS FoxSelectContext, ;
        group AS SqlAggregateGroup, aggregates AS List<SqlAggregateExpressionContext>, groupExpressions AS IList<SqlExpressionContext>) AS STRING
        FOR LOCAL groupIndex := 0 AS INT TO groupExpressions:Count - 1
            IF expression:ToResolvedString(selectCtx:TableAliases):Equals( ;
               groupExpressions[groupIndex]:ToResolvedString(selectCtx:TableAliases), StringComparison.OrdinalIgnoreCase)
                RETURN AggregateLiteral(group:KeyValues[groupIndex+1])
            ENDIF
        NEXT
        IF expression IS SqlAggregateExpressionContext VAR aggregate
            RETURN AggregateLiteral(AggregateValue(aggregate, group))
        ENDIF
        IF expression IS SqlNameExpressionContext VAR name
            FOR LOCAL selectIndex := 0 AS INT TO selectCtx:FieldSelectionList:Count - 1
                IF !String.IsNullOrEmpty(selectCtx:FieldSelectionList[selectIndex]:Alias) .AND. ;
                   name:Name:Equals(selectCtx:FieldSelectionList[selectIndex]:Alias, StringComparison.OrdinalIgnoreCase)
                    FOR LOCAL groupIndex := 0 AS INT TO groupExpressions:Count - 1
                        IF selectCtx:FieldSelectionList[selectIndex]:Expression:ToResolvedString(selectCtx:TableAliases):Equals( ;
                           groupExpressions[groupIndex]:ToResolvedString(selectCtx:TableAliases), StringComparison.OrdinalIgnoreCase)
                            RETURN AggregateLiteral(group:KeyValues[groupIndex+1])
                        ENDIF
                    NEXT
                    RETURN GroupExpressionValue(selectCtx:FieldSelectionList[selectIndex]:Expression, ;
                        selectCtx, group, aggregates, groupExpressions)
                ENDIF
            NEXT
            FOR LOCAL i := 0 AS INT TO groupExpressions:Count - 1
                IF groupExpressions[i] IS SqlNameExpressionContext VAR groupName
                    IF name:Name:Equals(groupName:Name, StringComparison.OrdinalIgnoreCase) .AND. ;
                       (String.IsNullOrEmpty(name:Table) .OR. String.IsNullOrEmpty(groupName:Table) .OR. ;
                        name:Table:Equals(groupName:Table, StringComparison.OrdinalIgnoreCase))
                        RETURN AggregateLiteral(group:KeyValues[i+1])
                    ENDIF
                ENDIF
                IF groupExpressions[i]:ToString():Equals(name:Name, StringComparison.OrdinalIgnoreCase) .AND. ;
                   i < selectCtx:SelectList:Count
                    RETURN AggregateLiteral(group:KeyValues[i+1])
                ENDIF
                IF i < selectCtx:FieldSelectionList:Count .AND. ;
                   !String.IsNullOrEmpty(selectCtx:FieldSelectionList[i]:Alias) .AND. ;
                   name:Name:Equals(selectCtx:FieldSelectionList[i]:Alias, StringComparison.OrdinalIgnoreCase)
                    RETURN AggregateLiteral(group:KeyValues[i+1])
                ENDIF
            NEXT
            THROW Error{"Column '" + name:Name + "' must appear in GROUP BY or be used in an aggregate"}
        ENDIF
        IF expression IS SqlCompositeExpressionContext VAR composite
            VAR sb := StringBuilder{}
            FOREACH VAR child IN composite:Exprs
                sb:Append(GroupExpressionValue(child, selectCtx, group, aggregates, groupExpressions))
            NEXT
            RETURN sb:ToString()
        ENDIF
        IF expression IS SqlParenExpressionContext VAR paren
            RETURN "(" + GroupExpressionValue(paren:Expr, selectCtx, group, aggregates, groupExpressions) + ")"
        ENDIF
        IF expression IS SqlBinaryExpressionContext VAR binary
            RETURN GroupExpressionValue(binary:Left, selectCtx, group, aggregates, groupExpressions) + ;
                binary:Op:Leadingws + binary:Op:Text + ;
                GroupExpressionValue(binary:Right, selectCtx, group, aggregates, groupExpressions)
        ENDIF
        IF expression IS SqlPrefixExpressionContext VAR prefix
            RETURN prefix:Op:Text + GroupExpressionValue(prefix:Expr, selectCtx, group, aggregates, groupExpressions)
        ENDIF
        RETURN expression:ToResolvedString(selectCtx:TableAliases)

    PRIVATE STATIC METHOD ResolveGroupItem(groupItem AS SqlExpressionContext, selectCtx AS FoxSelectContext) AS SqlExpressionContext
        IF groupItem IS SqlSimpleExpressionContext VAR simple .AND. simple:Tokens:Count == 1
            IF Int32.TryParse(simple:Tokens[0]:Text, OUT VAR ordinal)
                IF ordinal < 1 .OR. ordinal > selectCtx:SelectList:Count
                    THROW Error{"GROUP BY ordinal is outside the SELECT list"}
                ENDIF
                RETURN selectCtx:SelectList[ordinal-1]
            ENDIF
        ENDIF
        IF groupItem IS SqlNameExpressionContext VAR aliasName
            FOR LOCAL i := 0 AS INT TO selectCtx:FieldSelectionList:Count - 1
                IF !String.IsNullOrEmpty(selectCtx:FieldSelectionList[i]:Alias) .AND. ;
                   aliasName:Name:Equals(selectCtx:FieldSelectionList[i]:Alias, StringComparison.OrdinalIgnoreCase)
                    RETURN selectCtx:FieldSelectionList[i]:Expression
                ENDIF
            NEXT
        ENDIF
        RETURN groupItem

    PRIVATE STATIC METHOD EvaluateGroupedExpression(expression AS SqlExpressionContext, selectCtx AS FoxSelectContext, ;
        group AS SqlAggregateGroup, aggregates AS List<SqlAggregateExpressionContext>, ;
        groupExpressions AS IList<SqlExpressionContext>) AS USUAL
        IF group:InputCount == 0
            RETURN NIL
        ENDIF
        IF expression IS SqlAggregateExpressionContext VAR aggregate
            RETURN AggregateValue(aggregate, group)
        ENDIF
        FOR LOCAL i := 0 AS INT TO groupExpressions:Count - 1
            IF expression:ToResolvedString(selectCtx:TableAliases):Equals( ;
               groupExpressions[i]:ToResolvedString(selectCtx:TableAliases), StringComparison.OrdinalIgnoreCase)
                RETURN group:KeyValues[i+1]
            ENDIF
        NEXT
        VAR source := GroupExpressionValue(expression, selectCtx, group, aggregates, groupExpressions)
        VAR groupEvalBlock := MCompile(source)
        IF groupEvalBlock == NULL
            THROW Error{"Could not compile grouped expression: " + source}
        ENDIF
        RETURN groupEvalBlock:Eval()

    PRIVATE STATIC METHOD UpdateGroup(group AS SqlAggregateGroup, aggregates AS List<SqlAggregateExpressionContext>, ;
        aggregateCodeBlocks AS Dictionary<SqlAggregateExpressionContext,CODEBLOCK>) AS VOID
        group:InputCount++
        FOREACH VAR aggregate IN aggregates
            LOCAL value AS USUAL
            IF aggregate:IsStar
                value := NIL
            ELSE
                value := aggregateCodeBlocks[aggregate]:Eval()
            ENDIF
            UpdateAggregate(group:States[aggregate], value, aggregate:IsStar)
        NEXT
        RETURN

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

    PRIVATE STATIC METHOD SqlOptimizationsEnabled() AS LOGIC
        LOCAL disabled := FALSE AS LOGIC
        AppContext.TryGetSwitch("XSharp.VFP.EmbeddedSql.DisableOptimizations", OUT disabled)
        RETURN !disabled

    STATIC METHOD ParseSqlSelect(sCommand as STRING) AS FoxSelectContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        var selectCtx := FoxSelectContext{}
        IF ! parser:ParseSelectStatement(out selectCtx)
            THROW Error{"Syntax error in SELECT command: "+parser:Error+CRLF+sCommand}
        ENDIF
        RETURN selectCtx

    PRIVATE STATIC METHOD PrepareSubqueryExpression(expression AS SqlExpressionContext) AS VOID
        IF expression == NULL
            RETURN
        ENDIF
        IF expression IS SqlInSubqueryExpressionContext VAR inExpression
            VAR subquery := inExpression:Subquery
            IF HasExternalSubqueryReference(subquery)
                THROW Error{"Correlated or unresolved references in IN subqueries are not supported yet"}
            ENDIF
            IF subquery:SelectList:Count != 1
                THROW Error{"IN subquery must return exactly one column"}
            ENDIF
            LOCAL alias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
            subquery:TargetCursor := alias
            VAR savedArea := DbSelect()
            TRY
                ExecuteSelect(subquery)
                IF !Used(alias)
                    THROW Error{"IN subquery did not produce a result cursor"}
                ENDIF
                IF (alias)->FCount() != 1
                    THROW Error{"IN subquery must return exactly one column"}
                ENDIF
                VAR values := List<USUAL>{}
                (alias)->DbGoTop()
                DO WHILE !(alias)->Eof()
                    values:Add((USUAL)(alias)->FieldGet(1))
                    (alias)->DbSkip(1)
                ENDDO
                inExpression:Values := values
            FINALLY
                IF Used(alias)
                    (alias)->DbCloseArea()
                ENDIF
                IF savedArea > 0
                    DbSelectArea(savedArea)
                ENDIF
            END TRY
            RETURN
        ENDIF
        IF expression IS SqlParenExpressionContext VAR parenExpression
            PrepareSubqueryExpression(parenExpression:Expr)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            PrepareSubqueryExpression(prefixExpression:Expr)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            PrepareSubqueryExpression(binaryExpression:Left)
            PrepareSubqueryExpression(binaryExpression:Right)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                PrepareSubqueryExpression(child)
            NEXT
        ENDIF
        RETURN

    PRIVATE STATIC METHOD HasExternalSubqueryReference(selectCtx AS FoxSelectContext) AS LOGIC
        FOREACH VAR expression IN selectCtx:SelectList
            IF HasExternalSubqueryDependency(expression, selectCtx:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        IF HasExternalSubqueryDependency(selectCtx:WhereClause, selectCtx:TableAliases) .OR. ;
           HasExternalSubqueryDependency(selectCtx:HavingClause, selectCtx:TableAliases)
            RETURN TRUE
        ENDIF
        FOREACH VAR expression IN selectCtx:GroupByItems
            IF HasExternalSubqueryDependency(expression, selectCtx:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        FOREACH VAR join IN selectCtx:Joins
            IF HasExternalSubqueryDependency(join:OnClause, selectCtx:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        FOREACH VAR orderItem IN selectCtx:OrderByClause
            IF HasExternalSubqueryDependency(orderItem:Expression, selectCtx:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        FOREACH VAR branch IN selectCtx:UnionBranches
            IF HasExternalSubqueryReference(branch)
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE

    PRIVATE STATIC METHOD HasExternalSubqueryDependency(expression AS SqlExpressionContext, ;
            aliases AS Dictionary<STRING,STRING>) AS LOGIC
        IF expression == NULL
            RETURN FALSE
        ENDIF
        FOREACH VAR dependency IN expression:GetTableDependencies(aliases)
            LOCAL found := FALSE AS LOGIC
            FOREACH VAR tableName IN aliases:Values
                IF tableName:Equals(dependency, StringComparison.OrdinalIgnoreCase)
                    found := TRUE
                    EXIT
                ENDIF
            NEXT
            IF !found
                RETURN TRUE
            ENDIF
        NEXT
        IF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN HasExternalSubqueryDependency(parenExpression:Expr, aliases)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            RETURN HasExternalSubqueryDependency(prefixExpression:Expr, aliases)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            RETURN HasExternalSubqueryDependency(binaryExpression:Left, aliases) .OR. ;
                   HasExternalSubqueryDependency(binaryExpression:Right, aliases)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                IF HasExternalSubqueryDependency(child, aliases)
                    RETURN TRUE
                ENDIF
            NEXT
        ELSEIF expression IS SqlAggregateExpressionContext VAR aggregateExpression
            RETURN HasExternalSubqueryDependency(aggregateExpression:Argument, aliases)
        ELSEIF expression IS SqlInListExpressionContext VAR inListExpression
            IF HasExternalSubqueryDependency(inListExpression:ValueExpression, aliases)
                RETURN TRUE
            ENDIF
            FOREACH VAR item IN inListExpression:Items
                IF HasExternalSubqueryDependency(item, aliases)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    STATIC METHOD ExecuteSelect(selectCtx as FoxSelectContext) AS VOID
        // Implementation of SELECT execution
        // This will handle opening tables, selecting records, and returning results

        IF selectCtx:UnionBranches != NULL .AND. selectCtx:UnionBranches:Count > 0
            ExecuteUnionSelect(selectCtx)
            RETURN
        ENDIF

        LOCAL hasLeftOuterJoin := FALSE AS LOGIC
        FOREACH VAR joinContext IN selectCtx:Joins
            IF joinContext:Kind == FoxSelectJoinKind.Left
                hasLeftOuterJoin := TRUE
                EXIT
            ENDIF
        NEXT

        IF hasLeftOuterJoin .AND. selectCtx:OuterJoinRows == NULL
            ExecuteLeftOuterJoinSelect(selectCtx)
            RETURN
        ENDIF

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
                    LOCAL outputFlags := (LONG)fieldInfo[6] AS LONG
                    IF hasLeftOuterJoin
                        outputFlags := outputFlags | (LONG)DBFFieldFlags.Nullable
                    ENDIF
                    AAdd(fieldList, {fieldName, fieldInfo[2], fieldInfo[3], fieldInfo[4], fieldName, outputFlags})
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
                ELSEIF expr IS SqlAggregateExpressionContext VAR schemaAggregate .AND. ;
                       (schemaAggregate:FunctionName == "MIN" .OR. schemaAggregate:FunctionName == "MAX") .AND. ;
                       schemaAggregate:Argument IS SqlNameExpressionContext VAR schemaName
                    exprTable := schemaName:Table
                    exprField := schemaName:Name
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
                    LOCAL expressionAggregates := List<SqlAggregateExpressionContext>{} AS List<SqlAggregateExpressionContext>
                    CollectAggregates(expr, expressionAggregates)
                    IF expressionAggregates:Count > 0
                        AAdd(fieldList, {exprStr, fieldInfo[2], fieldInfo[3], fieldInfo[4], exprStr, ;
                            ((LONG)fieldInfo[6]) | (LONG)DBFFieldFlags.Nullable})
                    ELSE
                        LOCAL outputFlags := (LONG)fieldInfo[6] AS LONG
                        IF hasLeftOuterJoin
                            outputFlags := outputFlags | (LONG)DBFFieldFlags.Nullable
                        ENDIF
                        AAdd(fieldList, {exprStr, fieldInfo[2], fieldInfo[3], fieldInfo[4], exprStr, outputFlags})
                    ENDIF
                ELSE
                    LOCAL expressionAggregates := List<SqlAggregateExpressionContext>{} AS List<SqlAggregateExpressionContext>
                    CollectAggregates(expr, expressionAggregates)
                    IF expressionAggregates:Count > 0
                        LOCAL aggregateFunction := expressionAggregates[0]:FunctionName AS STRING
                        IF aggregateFunction == "COUNT" .OR. aggregateFunction == "CNT"
                            AAdd(fieldList, {exprStr, "N", 10, 0, exprStr, (LONG)DBFFieldFlags.Nullable})
                        ELSE
                            AAdd(fieldList, {exprStr, "N", 20, 4, exprStr, (LONG)DBFFieldFlags.Nullable})
                        ENDIF
                    ELSE
                        // If field doesn't exist in source, assume it's an expression.
                        AAdd(fieldList, {exprStr, "C", 50, 0, exprStr, ;
                            IIF(hasLeftOuterJoin, (LONG)DBFFieldFlags.Nullable, 0)})
                    ENDIF
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
            PrepareSubqueryExpression(selectCtx:WhereClause)
            whereClause := selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
            hasWhereClause := TRUE
            whereCodeBlock := MCompile(whereClause)
        ENDIF

        LOCAL joinCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        FOREACH VAR joinContext IN selectCtx:Joins
            IF joinContext:Kind == FoxSelectJoinKind.Inner .OR. joinContext:Kind == FoxSelectJoinKind.Left
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
        IF SqlOptimizationsEnabled() .AND. tableNames:Count == 2 .AND. selectCtx:Joins:Count == 1 .AND. ;
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
        LOCAL multiTableCandidates AS Dictionary<STRING,RecordBitmap>
        LOCAL multiTableCandidateRows AS Dictionary<STRING,List<LONG>>
        useOptimization := FALSE
        multiTableCandidates := NULL
        multiTableCandidateRows := NULL

        IF SqlOptimizationsEnabled() .AND. hasWhereClause .AND. tableNames:Count == 1
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
        ELSEIF SqlOptimizationsEnabled() .AND. hasWhereClause .AND. tableNames:Count > 1 .AND. ;
               !useHashJoin .AND. !hasLeftOuterJoin
            // Build conservative per-table candidates for the nested-loop path.
            // The full WHERE and JOIN ON predicates are still evaluated below.
            LOCAL tableRecordCounts := Dictionary<STRING,LONG>{StringComparer.OrdinalIgnoreCase} AS Dictionary<STRING,LONG>
            VAR workarea := DbSelect()
            FOR LOCAL tableIdx := 1 AS DWORD TO ALen(tableNames)
                VAR tableName := (STRING)tableNames[tableIdx]
                (tableName)->DbSelectArea()
                tableRecordCounts[tableName] := (LONG)(tableName)->RecCount()
            NEXT
            (workarea)->DbSelectArea()

            VAR candidatePlan := QueryOptimizer.OptimizeMultiTableQuery( ;
                selectCtx:WhereClause, selectCtx:TableAliases, tableRecordCounts)
            multiTableCandidates := candidatePlan:CandidateBitmaps

            // Materialize only selective candidates. Traversing with DbGoTop/DbSkip
            // preserves the table's current RDD order while capturing physical recnos.
            multiTableCandidateRows := Dictionary<STRING,List<LONG>>{StringComparer.OrdinalIgnoreCase}
            FOR LOCAL tableIdx := 1 AS DWORD TO ALen(tableNames)
                VAR candidateTable := (STRING)tableNames[tableIdx]
                LOCAL candidateBitmap AS RecordBitmap
                IF multiTableCandidates:TryGetValue(candidateTable, OUT candidateBitmap) .AND. ;
                   !candidateBitmap:IsAllMatch()
                    VAR candidateRecNos := List<LONG>{}
                    (tableNames[tableIdx])->DbGoTop()
                    DO WHILE !(tableNames[tableIdx])->Eof()
                        VAR candidateRecNo := (LONG)(tableNames[tableIdx])->RecNo()
                        IF candidateBitmap:GetMatchState(candidateRecNo) == RecordBitmap.RecordMatchState.Match
                            candidateRecNos:Add(candidateRecNo)
                        ENDIF
                        (tableNames[tableIdx])->DbSkip(1)
                    ENDDO
                    multiTableCandidateRows[candidateTable] := candidateRecNos
                ENDIF
            NEXT
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
        LOCAL aggregates := List<SqlAggregateExpressionContext>{} AS List<SqlAggregateExpressionContext>
        FOREACH VAR selectExpression IN selectCtx:SelectList
            CollectAggregates(selectExpression, aggregates)
        NEXT
        CollectAggregates(selectCtx:HavingClause, aggregates)
        FOREACH VAR orderItem IN selectCtx:OrderByClause
            CollectAggregates(orderItem:Expression, aggregates)
        NEXT
        LOCAL aggregateMode := aggregates:Count > 0 .OR. selectCtx:GroupByItems:Count > 0 AS LOGIC
        LOCAL groupExpressions := List<SqlExpressionContext>{} AS List<SqlExpressionContext>
        LOCAL groupCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        LOCAL aggregateCodeBlocks := Dictionary<SqlAggregateExpressionContext,CODEBLOCK>{} AS Dictionary<SqlAggregateExpressionContext,CODEBLOCK>
        LOCAL havingCodeBlock AS CODEBLOCK
        LOCAL hasRowHaving := selectCtx:HavingClause != NULL .AND. !aggregateMode AS LOGIC
        IF aggregateMode
            IF hasStarSelection
                THROW Error{"SELECT * is not valid with GROUP BY or aggregate functions"}
            ENDIF
            FOREACH VAR groupItem IN selectCtx:GroupByItems
                VAR resolvedGroupItem := ResolveGroupItem(groupItem, selectCtx)
                VAR groupAggregates := List<SqlAggregateExpressionContext>{}
                CollectAggregates(resolvedGroupItem, groupAggregates)
                IF groupAggregates:Count > 0
                    THROW Error{"Aggregate functions are not allowed in GROUP BY"}
                ENDIF
                IF resolvedGroupItem IS SqlNameExpressionContext VAR groupField
                    LOCAL sourceTable := groupField:Table AS STRING
                    IF !String.IsNullOrEmpty(sourceTable)
                        selectCtx:TableAliases:TryGetValue(sourceTable, OUT sourceTable)
                    ELSE
                        FOREACH VAR tableAlias IN selectCtx:TableAliases
                            VAR candidateTable := tableAlias:Value
                            VAR fieldPosition := (candidateTable)->FieldPos(groupField:Name)
                            IF fieldPosition > 0
                                sourceTable := candidateTable
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF
                    IF !String.IsNullOrEmpty(sourceTable)
                        VAR fieldPosition := (sourceTable)->FieldPos(groupField:Name)
                        IF fieldPosition > 0
                            VAR groupFieldType := ((STRING)(sourceTable)->DbStruct()[fieldPosition][2]):ToUpperInvariant()
                            IF groupFieldType == "M" .OR. groupFieldType == "G" .OR. groupFieldType == "W"
                                THROW Error{"Memo, General, and Blob fields cannot be used as GROUP BY keys"}
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
                groupExpressions:Add(resolvedGroupItem)
                VAR groupExpression := resolvedGroupItem:ToResolvedString(selectCtx:TableAliases)
                VAR groupCodeBlock := MCompile(groupExpression)
                IF groupCodeBlock == NULL
                    THROW Error{"Could not compile GROUP BY expression: " + groupExpression}
                ENDIF
                groupCodeBlocks:Add(groupCodeBlock)
            NEXT
            FOREACH VAR aggregate IN aggregates
                IF aggregate:FunctionName != "COUNT" .AND. aggregate:FunctionName != "CNT" .AND. ;
                   aggregate:FunctionName != "SUM" .AND. aggregate:FunctionName != "AVG" .AND. ;
                   aggregate:FunctionName != "MIN" .AND. aggregate:FunctionName != "MAX"
                    THROW Error{"Unsupported aggregate function: " + aggregate:FunctionName}
                ENDIF
                IF !aggregate:IsStar
                    VAR argument := aggregate:Argument:ToResolvedString(selectCtx:TableAliases)
                    VAR argumentCodeBlock := MCompile(argument)
                    IF argumentCodeBlock == NULL
                        THROW Error{"Could not compile aggregate argument: " + argument}
                    ENDIF
                    aggregateCodeBlocks[aggregate] := argumentCodeBlock
                ELSEIF aggregate:FunctionName != "COUNT" .AND. aggregate:FunctionName != "CNT"
                    THROW Error{"Only COUNT(*) is supported with '*'"}
                ENDIF
            NEXT
        ENDIF
        IF hasRowHaving
            VAR havingExpression := selectCtx:HavingClause:ToResolvedString(selectCtx:TableAliases)
            havingCodeBlock := MCompile(havingExpression)
            IF havingCodeBlock == NULL
                THROW Error{"Could not compile HAVING expression: " + havingExpression}
            ENDIF
        ENDIF
        LOCAL orderCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        LOCAL orderedRows := List<SqlSelectResultRow>{} AS List<SqlSelectResultRow>
        IF hasOrderBy .AND. !aggregateMode
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
        LOCAL aggregateGroups := Dictionary<STRING,SqlAggregateGroup>{} AS Dictionary<STRING,SqlAggregateGroup>
        LOCAL aggregateGroupList := List<SqlAggregateGroup>{} AS List<SqlAggregateGroup>
        IF aggregateMode .AND. groupExpressions:Count == 0
            VAR emptyGroup := SqlAggregateGroup{USUAL[]{0}, aggregates, 0}
            aggregateGroups[""] := emptyGroup
            aggregateGroupList:Add(emptyGroup)
        ENDIF

        // Position the precomputed LEFT JOIN tuples or initialize the regular traversal.
        LOCAL tablePositions AS ARRAY
        tablePositions := ArrayNew(ALen(tableNames))

        IF selectCtx:OuterJoinRows != NULL
            FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
                tablePositions[i] := 1
                IF selectCtx:OuterJoinRows:Count > 0
                    DbGoto(selectCtx:OuterJoinRows[0][i], tableNames[i])
                ENDIF
            NEXT
        ELSE
            FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
                tablePositions[i] := 1
                LOCAL candidateRecNos AS List<LONG>
                IF multiTableCandidateRows != NULL .AND. ;
                   multiTableCandidateRows:TryGetValue((STRING)tableNames[i], OUT candidateRecNos)
                    IF candidateRecNos:Count > 0
                        DbGoto(candidateRecNos[0], tableNames[i])
                    ENDIF
                ELSE
                    (tableNames[i])->DbGoTop()
                ENDIF
            NEXT
        ENDIF

        // Main loop to iterate through all combinations
        LOCAL continueProcessing AS LOGIC
        LOCAL currentOuterRecNo AS LONG
        LOCAL currentInnerRecNo AS LONG
        IF selectCtx:OuterJoinRows != NULL
            continueProcessing := selectCtx:OuterJoinRows:Count > 0
        ELSE
            continueProcessing := TRUE
            FOR LOCAL tableIndex := 1 AS DWORD TO ALen(tableNames)
                LOCAL candidateRecNos AS List<LONG>
                IF multiTableCandidateRows != NULL .AND. ;
                   multiTableCandidateRows:TryGetValue((STRING)tableNames[tableIndex], OUT candidateRecNos)
                    IF candidateRecNos:Count == 0
                        continueProcessing := FALSE
                        EXIT
                    ENDIF
                ELSEIF (tableNames[tableIndex])->Eof()
                    continueProcessing := FALSE
                    EXIT
                ENDIF
            NEXT
        ENDIF
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
            IF includeRecord .AND. selectCtx:OuterJoinRows == NULL
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
            ENDIF

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

            IF includeRecord .AND. hasRowHaving
                TRY
                    includeRecord := havingCodeBlock:Eval()
                CATCH ex AS Exception
                    includeRecord := FALSE
                END TRY
            ENDIF

            // If the record matches the criteria, add it to the result
            IF includeRecord
                IF aggregateMode
                    LOCAL keyValues := USUAL[]{groupCodeBlocks:Count} AS USUAL[]
                    FOR LOCAL keyIndex := 1 AS INT TO groupCodeBlocks:Count
                        keyValues[keyIndex] := groupCodeBlocks[keyIndex-1]:Eval()
                    NEXT
                    VAR key := GroupKey(keyValues)
                    LOCAL group AS SqlAggregateGroup
                    IF !aggregateGroups:TryGetValue(key, OUT group)
                        group := SqlAggregateGroup{keyValues, aggregates, (LONG)aggregateGroupList:Count}
                        aggregateGroups[key] := group
                        aggregateGroupList:Add(group)
                    ENDIF
                    UpdateGroup(group, aggregates, aggregateCodeBlocks)
                ELSE
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
                ENDIF // aggregateMode
            ENDIF

            // Only continue if we haven't reached the TOP limit
            IF continueProcessing
                // For single-table optimization, advance the table and check for EOF
                IF selectCtx:OuterJoinRows != NULL
                    LOCAL nextOuterRow := tablePositions[1] AS LONG
                    IF nextOuterRow < selectCtx:OuterJoinRows:Count
                        FOR LOCAL i := 1 AS DWORD TO ALen(tableNames)
                            DbGoto(selectCtx:OuterJoinRows[(INT)nextOuterRow][i], tableNames[i])
                            tablePositions[i] := nextOuterRow + 1
                        NEXT
                    ELSE
                        continueProcessing := FALSE
                    ENDIF
                ELSEIF useHashJoin
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
                        LOCAL candidateRecNos AS List<LONG>
                        IF multiTableCandidateRows != NULL .AND. ;
                           multiTableCandidateRows:TryGetValue((STRING)tableNames[positionIdx], OUT candidateRecNos)
                            tablePositions[positionIdx]++
                            IF tablePositions[positionIdx] <= candidateRecNos:Count
                                DbGoto(candidateRecNos[(INT)tablePositions[positionIdx]-1], tableNames[positionIdx])
                            ELSE
                                (tableNames[positionIdx])->DbGoBottom()
                                (tableNames[positionIdx])->DbSkip(1)
                            ENDIF
                        ELSE
                            // Advance an unconstrained table with its normal RDD traversal.
                            (tableNames[positionIdx])->DbSkip(1)
                            tablePositions[positionIdx]++
                        ENDIF

                        // If this table hasn't reached EOF, we're done with carry-over
                        IF !(tableNames[positionIdx])->Eof()
                            EXIT  // Exit the carry-over loop
                        ENDIF

                        // If this table reached EOF, reset it and continue carry-over to next table
                        tablePositions[positionIdx] := 1
                        IF multiTableCandidateRows != NULL .AND. ;
                           multiTableCandidateRows:TryGetValue((STRING)tableNames[positionIdx], OUT candidateRecNos)
                            DbGoto(candidateRecNos[0], tableNames[positionIdx])
                        ELSE
                            (tableNames[positionIdx])->DbGoTop()
                        ENDIF

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

        IF aggregateMode
            FOREACH VAR group IN aggregateGroupList
                IF selectCtx:HavingClause != NULL
                    VAR havingValue := EvaluateGroupedExpression(selectCtx:HavingClause, selectCtx, group, aggregates, groupExpressions)
                    IF havingValue:IsNull .OR. !(LOGIC)havingValue
                        LOOP
                    ENDIF
                ENDIF
                LOCAL resultValues := USUAL[]{selectCtx:SelectList:Count} AS USUAL[]
                FOR LOCAL selectIndex := 0 AS INT TO selectCtx:SelectList:Count - 1
                    resultValues[selectIndex+1] := EvaluateGroupedExpression( ;
                        selectCtx:SelectList[selectIndex], selectCtx, group, aggregates, groupExpressions)
                NEXT
                LOCAL recordAdded := TRUE AS LOGIC
                IF isDistinct
                    VAR distinctKey := GroupKey(resultValues)
                    recordAdded := !distinctValues:Contains(distinctKey)
                    IF recordAdded
                        distinctValues:Add(distinctKey)
                    ENDIF
                ENDIF
                IF recordAdded
                    IF hasOrderBy
                        LOCAL sortValues := USUAL[]{selectCtx:OrderByClause:Count} AS USUAL[]
                        FOR LOCAL orderIndex := 0 AS INT TO selectCtx:OrderByClause:Count - 1
                            sortValues[orderIndex+1] := EvaluateGroupedExpression( ;
                                selectCtx:OrderByClause[orderIndex]:Expression, selectCtx, group, aggregates, groupExpressions)
                        NEXT
                        orderedRows:Add(SqlSelectResultRow{resultValues, sortValues, group:Sequence})
                    ELSEIF maxRecords <= 0 .OR. recordCount < maxRecords
                        AppendSelectResult(resultTable, fieldList, resultValues)
                    ENDIF
                    recordCount++
                ENDIF
            NEXT
        ENDIF

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

    PRIVATE STATIC METHOD ExecuteLeftOuterJoinSelect(source AS FoxSelectContext) AS VOID
        LOCAL temporaryAliases := List<STRING>{} AS List<STRING>
        LOCAL temporaryPaths := List<STRING>{} AS List<STRING>
        LOCAL execution AS FoxSelectContext
        TRY
            execution := CreateLeftJoinExecutionContext(source, OUT temporaryAliases, OUT temporaryPaths)
            execution:OuterJoinRows := BuildLeftJoinRows(execution)
            ExecuteSelect(execution)
        FINALLY
            FOREACH VAR temporaryAlias IN temporaryAliases
                IF Used(temporaryAlias)
                    (temporaryAlias)->DbCloseArea()
                ENDIF
            NEXT
            FOREACH VAR temporaryPath IN temporaryPaths
                IF System.IO.File.Exists(temporaryPath)
                    System.IO.File.Delete(temporaryPath)
                ENDIF
                VAR memoPath := System.IO.Path.ChangeExtension(temporaryPath, ".fpt")
                IF System.IO.File.Exists(memoPath)
                    System.IO.File.Delete(memoPath)
                ENDIF
            NEXT
        END TRY
        RETURN

    PRIVATE STATIC METHOD CreateLeftJoinExecutionContext(source AS FoxSelectContext, ;
                                                          temporaryAliases OUT List<STRING>, ;
                                                          temporaryPaths OUT List<STRING>) AS FoxSelectContext
        temporaryAliases := List<STRING>{}
        temporaryPaths := List<STRING>{}
        IF source:Joins:Count != source:TableList:Count - 1
            THROW Error{"LEFT JOIN requires an explicit join chain without comma-separated tables"}
        ENDIF

        VAR execution := CopySelectBranch(source, source:TargetCursor)
        execution:OuterJoinRows := NULL
        execution:TableList := List<STRING>{}
        execution:TableAliases := Dictionary<STRING,STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR tableName IN source:TableList
            execution:TableList:Add(tableName)
        NEXT
        FOREACH VAR aliasEntry IN source:TableAliases
            execution:TableAliases[aliasEntry:Key] := aliasEntry:Value
        NEXT

        execution:Joins := List<FoxSelectJoinContext>{}
        FOR LOCAL joinIndex := 0 AS INT TO source:Joins:Count - 1
            VAR sourceJoin := source:Joins[joinIndex]
            VAR executionJoin := FoxSelectJoinContext{}
            executionJoin:TableName := sourceJoin:TableName
            executionJoin:Alias := sourceJoin:Alias
            executionJoin:Kind := sourceJoin:Kind
            executionJoin:OnClause := sourceJoin:OnClause
            executionJoin:NullRecordNumber := 0

            IF sourceJoin:Kind == FoxSelectJoinKind.Left
                LOCAL nullRecordNumber AS LONG
                LOCAL temporaryPath AS STRING
                VAR temporaryAlias := CreateNullableOuterJoinCopy(sourceJoin:TableName, OUT nullRecordNumber, OUT temporaryPath)
                temporaryAliases:Add(temporaryAlias)
                temporaryPaths:Add(temporaryPath)
                execution:TableList[joinIndex + 1] := temporaryAlias
                execution:TableAliases[sourceJoin:Alias] := temporaryAlias
                executionJoin:TableName := temporaryAlias
                executionJoin:NullRecordNumber := nullRecordNumber
            ENDIF
            execution:Joins:Add(executionJoin)
        NEXT
        RETURN execution

    PRIVATE STATIC METHOD CreateNullableOuterJoinCopy(sourceAlias AS STRING, nullRecordNumber OUT LONG, ;
                                                        temporaryPath OUT STRING) AS STRING
        LOCAL previousArea := DbSelect() AS DWORD
        LOCAL sourceWasOpen := DbSelectArea(sourceAlias) AS LOGIC
        IF !sourceWasOpen .AND. !FoxEmbeddedSQL.OpenArea(sourceAlias)
            THROW Error{"Could not open LEFT JOIN source table: " + sourceAlias}
        ENDIF

        VAR temporaryAlias := "FOXOUTER" + Guid.NewGuid():ToString("N"):Substring(0, 8):ToUpper()
        temporaryPath := System.IO.Path.Combine(System.IO.Path.GetTempPath(), ;
            "foxouter_" + Guid.NewGuid():ToString("N") + ".dbf")
        LOCAL temporaryCreated := FALSE AS LOGIC
        nullRecordNumber := 0
        TRY
            VAR sourceSchema := (sourceAlias)->DbStruct()
            LOCAL nullableSchema := ArrayNew(ALen(sourceSchema)) AS ARRAY
            FOR LOCAL fieldIndex := 1 AS INT TO ALen(sourceSchema)
                nullableSchema[fieldIndex] := AClone(sourceSchema[fieldIndex])
                nullableSchema[fieldIndex][6] := (LONG)nullableSchema[fieldIndex][6] | (LONG)DBFFieldFlags.Nullable
            NEXT
            IF !DbCreate(temporaryPath, nullableSchema, "DBFVFP", TRUE, temporaryAlias)
                THROW Error{"Could not create LEFT JOIN work table: " + temporaryAlias}
            ENDIF
            temporaryCreated := TRUE

            (sourceAlias)->DbGoTop()
            DO WHILE !(sourceAlias)->Eof()
                (temporaryAlias)->DbAppend()
                FOR LOCAL fieldIndex := 1 AS INT TO ALen(sourceSchema)
                    VAR sourceValue := (USUAL)(sourceAlias)->FieldGet(fieldIndex)
                    IF IsNull(sourceValue)
                        (temporaryAlias)->FieldPut(fieldIndex, NULL)
                    ELSE
                        (temporaryAlias)->FieldPut(fieldIndex, sourceValue)
                    ENDIF
                NEXT
                (sourceAlias)->DbSkip(1)
            ENDDO

            // An all-NULL record is the explicit row used for an unmatched LEFT JOIN.
            (temporaryAlias)->DbAppend()
            FOR LOCAL fieldIndex := 1 AS INT TO ALen(sourceSchema)
                (temporaryAlias)->FieldPut(fieldIndex, NULL)
            NEXT
            nullRecordNumber := (LONG)(temporaryAlias)->RecNo()
            RETURN temporaryAlias
        CATCH
            IF temporaryCreated .AND. Used(temporaryAlias)
                (temporaryAlias)->DbCloseArea()
            ENDIF
            THROW
        FINALLY
            IF !sourceWasOpen .AND. Used(sourceAlias)
                (sourceAlias)->DbCloseArea()
            ENDIF
            IF previousArea > 0
                DbSelectArea(previousArea)
            ENDIF
        END TRY

    PRIVATE STATIC METHOD BuildLeftJoinRows(selectCtx AS FoxSelectContext) AS List<LONG[]>
        LOCAL rows := List<LONG[]>{} AS List<LONG[]>
        IF selectCtx:TableList:Count == 0
            RETURN rows
        ENDIF

        LOCAL tableNames AS ARRAY
        tableNames := {}
        FOREACH VAR sourceTable IN selectCtx:TableList
            AAdd(tableNames, sourceTable)
        NEXT
        LOCAL joinCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        FOR LOCAL joinIndex := 0 AS INT TO selectCtx:Joins:Count - 1
            VAR joinContext := selectCtx:Joins[joinIndex]
            IF joinContext:Kind != FoxSelectJoinKind.Cross
                VAR joinExpression := joinContext:OnClause:ToResolvedString(selectCtx:TableAliases)
                VAR joinCodeBlock := MCompile(joinExpression)
                IF joinCodeBlock == NULL
                    THROW Error{"Could not compile JOIN ON expression: " + joinExpression}
                ENDIF
                joinCodeBlocks:Add(joinCodeBlock)
            ELSE
                joinCodeBlocks:Add(NULL)
            ENDIF
        NEXT

        LOCAL firstTable := (STRING)tableNames[1] AS STRING
        (firstTable)->DbGoTop()
        DO WHILE !(firstTable)->Eof()
            LOCAL row := LONG[]{ALen(tableNames)} AS LONG[]
            row[1] := (LONG)(firstTable)->RecNo()
            AppendLeftJoinRows(selectCtx, tableNames, joinCodeBlocks, 2, row, rows)
            (firstTable)->DbSkip(1)
        ENDDO
        RETURN rows

    PRIVATE STATIC METHOD AppendLeftJoinRows(selectCtx AS FoxSelectContext, tableNames AS ARRAY, ;
                                               joinCodeBlocks AS List<CODEBLOCK>, tableIndex AS INT, ;
                                               row AS LONG[], rows AS List<LONG[]>) AS VOID
        IF tableIndex > ALen(tableNames)
            LOCAL completedRow AS LONG[]
            completedRow := LONG[]{row:Length}
            FOR LOCAL i := 1 AS INT TO row:Length
                completedRow[i] := row[i]
            NEXT
            rows:Add(completedRow)
            RETURN
        ENDIF

        VAR joinContext := selectCtx:Joins[tableIndex - 2]
        VAR tableName := (STRING)tableNames[tableIndex]
        LOCAL foundMatch := FALSE AS LOGIC
        (tableName)->DbGoTop()
        DO WHILE !(tableName)->Eof()
            VAR candidateRecordNumber := (LONG)(tableName)->RecNo()
            IF joinContext:Kind == FoxSelectJoinKind.Left .AND. ;
               candidateRecordNumber == joinContext:NullRecordNumber
                (tableName)->DbSkip(1)
                LOOP
            ENDIF
            row[tableIndex] := candidateRecordNumber
            FOR LOCAL previousTable := 1 AS INT TO tableIndex - 1
                DbGoto(row[previousTable], tableNames[previousTable])
            NEXT

            LOCAL matches := joinContext:Kind == FoxSelectJoinKind.Cross AS LOGIC
            IF !matches
                TRY
                    matches := (LOGIC)joinCodeBlocks[tableIndex - 2]:Eval()
                CATCH ex AS Exception
                    matches := FALSE
                END TRY
            ENDIF
            IF matches
                foundMatch := TRUE
                AppendLeftJoinRows(selectCtx, tableNames, joinCodeBlocks, tableIndex + 1, row, rows)
            ENDIF
            (tableName)->DbSkip(1)
        ENDDO

        IF !foundMatch .AND. joinContext:Kind == FoxSelectJoinKind.Left
            row[tableIndex] := joinContext:NullRecordNumber
            AppendLeftJoinRows(selectCtx, tableNames, joinCodeBlocks, tableIndex + 1, row, rows)
        ENDIF
        RETURN

    PRIVATE STATIC METHOD ExecuteUnionSelect(selectCtx AS FoxSelectContext) AS VOID
        LOCAL branchContexts := List<FoxSelectContext>{} AS List<FoxSelectContext>
        branchContexts:Add(selectCtx)
        branchContexts:AddRange(selectCtx:UnionBranches)

        LOCAL branchAliases := List<STRING>{} AS List<STRING>
        LOCAL unionRows AS List<USUAL[]>
        LOCAL firstStruct AS ARRAY
        LOCAL resultAlias := "QUERYRESULT" AS STRING
        IF !String.IsNullOrEmpty(AllTrim(selectCtx:TargetCursor))
            resultAlias := AllTrim(selectCtx:TargetCursor)
        ENDIF

        TRY
            FOR LOCAL branchIndex := 0 AS INT TO branchContexts:Count - 1
                VAR branchAlias := "UNION" + (branchIndex + 1):ToString()
                WHILE Used(branchAlias) .OR. branchAliases:Contains(branchAlias)
                    branchAlias += "X"
                END WHILE
                branchAliases:Add(branchAlias)

                VAR branch := CopySelectBranch(branchContexts[branchIndex], branchAlias)
                ExecuteSelect(branch)

                IF branchIndex == 0
                    firstStruct := (branchAlias)->DbStruct()
                    unionRows := ReadUnionRows(branchAlias, firstStruct)
                ELSE
                    VAR branchStruct := (branchAlias)->DbStruct()
                    ValidateUnionSchema(firstStruct, branchStruct)
                    VAR branchRows := ReadUnionRows(branchAlias, branchStruct)
                    IF selectCtx:UnionAll[branchIndex - 1]
                        unionRows:AddRange(branchRows)
                    ELSE
                        VAR uniqueRows := List<USUAL[]>{}
                        FOREACH VAR row IN unionRows
                            AddUnionRowIfUnique(uniqueRows, row)
                        NEXT
                        FOREACH VAR row IN branchRows
                            AddUnionRowIfUnique(uniqueRows, row)
                        NEXT
                        unionRows := uniqueRows
                    ENDIF
                ENDIF
            NEXT

            IF Used(resultAlias)
                (resultAlias)->DbCloseArea()
            ENDIF

            LOCAL tempPath := System.IO.Path.GetTempFileName() AS STRING
            tempPath := System.IO.Path.ChangeExtension(tempPath, ".dbf")
            IF !DbCreate(tempPath, firstStruct, "DBFVFP", TRUE, resultAlias)
                THROW Error{"Could not create UNION result table: " + resultAlias}
            ENDIF

            IF selectCtx:OrderByClause:Count > 0
                VAR orderedRows := List<SqlSelectResultRow>{}
                FOR LOCAL rowIndex := 0 AS INT TO unionRows:Count - 1
                    LOCAL sortValues := USUAL[]{selectCtx:OrderByClause:Count} AS USUAL[]
                    FOR LOCAL orderIndex := 0 AS INT TO selectCtx:OrderByClause:Count - 1
                        VAR fieldIndex := ResolveUnionOrderField(selectCtx:OrderByClause[orderIndex]:Expression, firstStruct)
                        sortValues[orderIndex + 1] := unionRows[rowIndex][fieldIndex]
                    NEXT
                    orderedRows:Add(SqlSelectResultRow{unionRows[rowIndex], sortValues, rowIndex})
                NEXT
                orderedRows:Sort(SqlSelectResultRowComparer{selectCtx:OrderByClause})
                FOREACH VAR orderedRow IN orderedRows
                    AppendSelectResult(resultAlias, firstStruct, orderedRow:Values)
                NEXT
            ELSE
                FOREACH VAR row IN unionRows
                    AppendSelectResult(resultAlias, firstStruct, row)
                NEXT
            ENDIF

            DbSelectArea(resultAlias)
        FINALLY
            FOREACH VAR branchAlias IN branchAliases
                IF Used(branchAlias)
                    (branchAlias)->DbCloseArea()
                ENDIF
            NEXT
        END TRY
        RETURN

    PRIVATE STATIC METHOD CopySelectBranch(source AS FoxSelectContext, targetCursor AS STRING) AS FoxSelectContext
        LOCAL branch := FoxSelectContext{} AS FoxSelectContext
        branch:TopCount := source:TopCount
        branch:IsDistinct := source:IsDistinct
        branch:SelectList := source:SelectList
        branch:FieldSelectionList := source:FieldSelectionList
        branch:TableList := source:TableList
        branch:TableAliases := source:TableAliases
        branch:JoinList := source:JoinList
        branch:Joins := source:Joins
        branch:TargetCursor := targetCursor
        branch:WhereClause := source:WhereClause
        branch:GroupByClause := source:GroupByClause
        branch:GroupByItems := source:GroupByItems
        branch:HavingClause := source:HavingClause
        RETURN branch

    PRIVATE STATIC METHOD ReadUnionRows(alias AS STRING, schema AS ARRAY) AS List<USUAL[]>
        LOCAL rows := List<USUAL[]>{} AS List<USUAL[]>
        DbSelectArea(alias)
        DbGoTop()
        DO WHILE !Eof()
            LOCAL values := USUAL[]{ALen(schema)} AS USUAL[]
            FOR LOCAL fieldIndex := 1 AS INT TO ALen(schema)
                VAR fieldValue := FieldGet(fieldIndex)
                IF IsNull(fieldValue)
                    values[fieldIndex] := NULL
                ELSE
                    values[fieldIndex] := fieldValue
                ENDIF
            NEXT
            rows:Add(values)
            DbSkip(1)
        ENDDO
        RETURN rows

    PRIVATE STATIC METHOD ValidateUnionSchema(firstSchema AS ARRAY, nextSchema AS ARRAY) AS VOID
        IF ALen(firstSchema) != ALen(nextSchema)
            THROW Error{"UNION branches must return the same number of columns"}
        ENDIF
        FOR LOCAL fieldIndex := 1 AS INT TO ALen(firstSchema)
            LOCAL firstType := ((STRING)firstSchema[fieldIndex][2]):ToUpper() AS STRING
            LOCAL nextType := ((STRING)nextSchema[fieldIndex][2]):ToUpper() AS STRING
            IF firstType == nextType
                LOOP
            ENDIF
            LOCAL firstCode := firstType:Substring(0, 1) AS STRING
            LOCAL nextCode := nextType:Substring(0, 1) AS STRING
            LOCAL firstNumeric := "NIYFB":Contains(firstCode) AS LOGIC
            LOCAL nextNumeric := "NIYFB":Contains(nextCode) AS LOGIC
            LOCAL firstCharacter := firstCode == "C" .OR. firstCode == "V" AS LOGIC
            LOCAL nextCharacter := nextCode == "C" .OR. nextCode == "V" AS LOGIC
            IF !(firstNumeric .AND. nextNumeric) .AND. !(firstCharacter .AND. nextCharacter)
                THROW Error{"UNION column " + fieldIndex:ToString() + " has incompatible types " + firstType + " and " + nextType}
            ENDIF
        NEXT
        RETURN

    PRIVATE STATIC METHOD AddUnionRowIfUnique(rows AS List<USUAL[]>, candidate AS USUAL[]) AS VOID
        FOREACH VAR existing IN rows
            IF UnionRowsEqual(existing, candidate)
                RETURN
            ENDIF
        NEXT
        rows:Add(candidate)
        RETURN

    PRIVATE STATIC METHOD UnionRowsEqual(left AS USUAL[], right AS USUAL[]) AS LOGIC
        IF left:Length != right:Length
            RETURN FALSE
        ENDIF
        FOR LOCAL fieldIndex := 1 AS INT TO left:Length
            IF left[fieldIndex]:IsNull .OR. right[fieldIndex]:IsNull
                IF left[fieldIndex]:IsNull != right[fieldIndex]:IsNull
                    RETURN FALSE
                ENDIF
            ELSEIF left[fieldIndex]:CompareTo(right[fieldIndex]) != 0
                RETURN FALSE
            ENDIF
        NEXT
        RETURN TRUE

    PRIVATE STATIC METHOD ResolveUnionOrderField(expression AS SqlExpressionContext, schema AS ARRAY) AS INT
        LOCAL requestedName := AllTrim(expression:ToString()) AS STRING
        LOCAL ordinal AS INT
        IF Int32.TryParse(requestedName, OUT ordinal)
            IF ordinal >= 1 .AND. ordinal <= ALen(schema)
                RETURN ordinal
            ENDIF
            THROW Error{"UNION ORDER BY ordinal is outside the result column range"}
        ENDIF
        IF expression IS SqlNameExpressionContext VAR nameExpression
            requestedName := nameExpression:Name
        ENDIF
        FOR LOCAL fieldIndex := 1 AS INT TO ALen(schema)
            IF String.Equals((STRING)schema[fieldIndex][1], requestedName, StringComparison.OrdinalIgnoreCase)
                RETURN fieldIndex
            ENDIF
        NEXT
        THROW Error{"UNION ORDER BY must refer to a result column or ordinal: " + requestedName}

    STATIC METHOD AppendSelectResult(resultTable AS STRING, fieldList AS ARRAY, sourceValues AS USUAL[]) AS VOID
        (resultTable)->DbAppend()
        FOR LOCAL i := 1 AS DWORD TO sourceValues:Length
            // Explicitly set the null bit; append defaults vary by field type and RDD.
            IF sourceValues[i]:IsNull
                (resultTable)->FieldPut(i, NULL)
            ELSEIF fieldList[i][2] == "C"
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
        ELSEIF expr IS SqlAggregateExpressionContext VAR aggregate
            VAR functionName := aggregate:FunctionName
            IF functionName == "COUNT"
                functionName := "CNT"
            ENDIF
            VAR argumentName := "ROWS"
            IF aggregate:Argument IS SqlNameExpressionContext VAR aggregateName
                argumentName := aggregateName:Name
            ENDIF
            result := functionName + "_" + argumentName
        ELSEIF expr IS SqlCompositeExpressionContext VAR compCtx
            VAR expressionAggregates := List<SqlAggregateExpressionContext>{}
            CollectAggregates(expr, expressionAggregates)
            IF expressionAggregates:Count > 0
                result := "EXP"
            // For ordinary composite expressions, preserve the existing name rule.
            ELSEIF compCtx:Names:Count > 0
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
