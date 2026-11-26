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
    PUBLIC PROPERTY SourceRecordNumbers AS LONG[] AUTO
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

INTERNAL STATIC CLASS SqlOptimizationDiagnostics
    INTERNAL STATIC CorrelatedInPlans AS INT
    INTERNAL STATIC HashJoins AS INT
    INTERNAL STATIC ExistsExecutions AS INT
    INTERNAL STATIC ScalarExecutions AS INT
    INTERNAL STATIC CorrelatedScalarCachePlans AS INT
    INTERNAL STATIC CorrelatedScalarCacheHits AS INT
    INTERNAL STATIC CorrelatedScalarNullKeys AS INT
    INTERNAL STATIC MembershipBitmaps AS INT
    INTERNAL STATIC MembershipCandidates AS INT
    INTERNAL STATIC MembershipScanned AS INT
    INTERNAL STATIC DerivedTableExecutions AS INT
END CLASS

INTERNAL CLASS SqlCorrelatedScalarMemo
    INTERNAL OuterValueCodeBlocks AS IList<CODEBLOCK>
    INTERNAL Values AS Dictionary<USUAL[],USUAL>

    INTERNAL CONSTRUCTOR(outerValueCodeBlocks AS IList<CODEBLOCK>)
        SELF:OuterValueCodeBlocks := outerValueCodeBlocks
        SELF:Values := Dictionary<USUAL[],USUAL>{SqlScalarMemoKeyComparer{}}
        RETURN
END CLASS

INTERNAL CLASS SqlScalarMemoKeyComparer IMPLEMENTS IEqualityComparer<USUAL[]>
    PUBLIC METHOD Equals(left AS USUAL[], right AS USUAL[]) AS LOGIC
        IF left == right
            RETURN TRUE
        ENDIF
        IF left == NULL .OR. right == NULL .OR. left:Length != right:Length
            RETURN FALSE
        ENDIF
        FOR LOCAL i := 1 AS INT TO left:Length
            IF UsualType(left[i]) == __UsualType.String .AND. UsualType(right[i]) == __UsualType.String
                IF !String.Equals(left[i]:ToString(), right[i]:ToString(), StringComparison.Ordinal)
                    RETURN FALSE
                ENDIF
            ELSEIF !EqualityComparer<USUAL>.Default:Equals(left[i], right[i])
                RETURN FALSE
            ENDIF
        NEXT
        RETURN TRUE

    PUBLIC METHOD GetHashCode(values AS USUAL[]) AS INT
        IF values == NULL
            RETURN 0
        ENDIF
        LOCAL hash := 17 AS LONG
        FOR LOCAL i := 1 AS INT TO values:Length
            LOCAL componentHash AS INT
            IF UsualType(values[i]) == __UsualType.String
                componentHash := StringComparer.Ordinal:GetHashCode(values[i]:ToString())
            ELSE
                componentHash := EqualityComparer<USUAL>.Default:GetHashCode(values[i])
            ENDIF
            hash := (hash * 31 + componentHash) % 2147483647
        NEXT
        RETURN (INT)hash
END CLASS

/// Runtime switches used by the embedded SQL implementation.
PUBLIC STATIC CLASS EmbeddedSqlRuntimeOptions
    PUBLIC CONST DisableOptimizationsSwitch := "XSharp.VFP.EmbeddedSql.DisableOptimizations" AS STRING
    PUBLIC CONST DisableOutOfScopeJoinWorkAreaCompatibilitySwitch := ;
        "XSharp.VFP.EmbeddedSql.DisableOutOfScopeJoinWorkAreaCompatibility" AS STRING

    PUBLIC STATIC METHOD OptimizationsEnabled() AS LOGIC
        LOCAL disabled := FALSE AS LOGIC
        AppContext.TryGetSwitch(DisableOptimizationsSwitch, OUT disabled)
        RETURN !disabled

    PUBLIC STATIC METHOD OutOfScopeJoinWorkAreaCompatibilityEnabled() AS LOGIC
        LOCAL disabled := FALSE AS LOGIC
        AppContext.TryGetSwitch(DisableOutOfScopeJoinWorkAreaCompatibilitySwitch, OUT disabled)
        RETURN !disabled
END CLASS

INTERNAL CLASS SqlDerivedTableExecution
    INTERNAL SourceIndex AS INT
    INTERNAL Alias AS STRING
    INTERNAL CursorName AS STRING
    INTERNAL OriginalTableName AS STRING
    INTERNAL OriginalAliasTarget AS STRING
    INTERNAL OriginalJoinTableName AS STRING
END CLASS

// -1 is UNKNOWN, 0 is FALSE and 1 is TRUE.
INTERNAL CLASS SqlThreeValuePredicate
    INTERNAL Kind AS STRING
    INTERNAL Op AS XTokenType
    INTERNAL Left AS SqlThreeValuePredicate
    INTERNAL Right AS SqlThreeValuePredicate
    INTERNAL ValueBlock AS CODEBLOCK
    INTERNAL Scalar AS SqlScalarSubqueryExpressionContext
    INTERNAL InQuery AS SqlInSubqueryExpressionContext
    INTERNAL QuantifiedQuery AS SqlQuantifiedSubqueryExpressionContext
    INTERNAL ExistsQuery AS SqlExistsSubqueryExpressionContext
    INTERNAL Items AS List<SqlThreeValuePredicate>
    INTERNAL Negated AS LOGIC

    INTERNAL METHOD Value() AS USUAL
        IF SELF:Scalar != NULL
            RETURN SELF:Scalar:Value
        ENDIF
        RETURN SELF:ValueBlock:Eval()

    INTERNAL STATIC METHOD IsUnknown(value AS USUAL) AS LOGIC
        RETURN IsNil(value) .OR. value:IsNull

    INTERNAL METHOD Evaluate() AS INT
        SWITCH SELF:Kind
        CASE "NOT"
            VAR state := SELF:Left:Evaluate()
            RETURN IIF(state < 0, -1, 1 - state)
        CASE "AND"
            VAR leftState := SELF:Left:Evaluate()
            IF leftState == 0
                RETURN 0
            ENDIF
            VAR rightState := SELF:Right:Evaluate()
            IF rightState == 0
                RETURN 0
            ENDIF
            RETURN IIF(leftState < 0 .OR. rightState < 0, -1, 1)
        CASE "OR"
            VAR leftOrState := SELF:Left:Evaluate()
            IF leftOrState == 1
                RETURN 1
            ENDIF
            VAR rightOrState := SELF:Right:Evaluate()
            IF rightOrState == 1
                RETURN 1
            ENDIF
            RETURN IIF(leftOrState < 0 .OR. rightOrState < 0, -1, 0)
        CASE "EXISTS"
            RETURN IIF(SELF:ExistsQuery:HasRows, 1, 0)
        CASE "COMPARE"
            VAR leftValue := SELF:Left:Value()
            VAR rightValue := SELF:Right:Value()
            IF IsUnknown(leftValue) .OR. IsUnknown(rightValue)
                RETURN -1
            ENDIF
            LOCAL matches := FALSE AS LOGIC
            SWITCH SELF:Op
            CASE XTokenType.EQ
                matches := leftValue = rightValue
            CASE XTokenType.EEQ
                matches := leftValue == rightValue
            CASE XTokenType.NEQ
                matches := leftValue != rightValue
            CASE XTokenType.LT
                matches := leftValue < rightValue
            CASE XTokenType.LTE
                matches := leftValue <= rightValue
            CASE XTokenType.GT
                matches := leftValue > rightValue
            CASE XTokenType.GTE
                matches := leftValue >= rightValue
            END SWITCH
            RETURN IIF(matches, 1, 0)
        CASE "IN"
            VAR candidate := SELF:Left:Value()
            LOCAL hasNull := FALSE AS LOGIC
            LOCAL found := FALSE AS LOGIC
            LOCAL itemCount AS INT
            IF SELF:InQuery != NULL
                itemCount := SELF:InQuery:Values:Count
            ELSE
                itemCount := SELF:Items:Count
            ENDIF
            IF itemCount == 0
                RETURN IIF(SELF:Negated, 1, 0)
            ENDIF
            IF IsUnknown(candidate)
                RETURN -1
            ENDIF
            FOR LOCAL i := 0 AS INT TO itemCount - 1
                LOCAL value AS USUAL
                IF SELF:InQuery != NULL
                    value := SELF:InQuery:Values[i]
                ELSE
                    value := SELF:Items[i]:Value()
                ENDIF
                IF IsUnknown(value)
                    hasNull := TRUE
                ELSEIF candidate = value
                    found := TRUE
                    EXIT
                ENDIF
            NEXT
            IF found
                RETURN IIF(SELF:Negated, 0, 1)
            ENDIF
            IF hasNull
                RETURN -1
            ENDIF
            RETURN IIF(SELF:Negated, 1, 0)
        CASE "QUANTIFIED"
            VAR leftValue := SELF:Left:Value()
            LOCAL hasUnknown := FALSE AS LOGIC
            FOREACH VAR rightValue IN SELF:QuantifiedQuery:Values
                IF IsUnknown(leftValue) .OR. IsUnknown(rightValue)
                    hasUnknown := TRUE
                    LOOP
                ENDIF
                LOCAL matches := FALSE AS LOGIC
                SWITCH SELF:QuantifiedQuery:Op:Type
                CASE XTokenType.EQ
                    matches := leftValue = rightValue
                CASE XTokenType.EEQ
                    matches := leftValue == rightValue
                CASE XTokenType.NEQ
                    matches := leftValue != rightValue
                CASE XTokenType.LT
                    matches := leftValue < rightValue
                CASE XTokenType.LTE
                    matches := leftValue <= rightValue
                CASE XTokenType.GT
                    matches := leftValue > rightValue
                CASE XTokenType.GTE
                    matches := leftValue >= rightValue
                END SWITCH
                IF SELF:QuantifiedQuery:Quantifier == "ALL" .AND. !matches
                    RETURN 0
                ELSEIF SELF:QuantifiedQuery:Quantifier != "ALL" .AND. matches
                    RETURN 1
                ENDIF
            NEXT
            IF hasUnknown
                RETURN -1
            ENDIF
            RETURN IIF(SELF:QuantifiedQuery:Quantifier == "ALL", 1, 0)
        CASE "BOOL"
            VAR boolValue := SELF:Value()
            IF IsUnknown(boolValue)
                RETURN -1
            ENDIF
            RETURN IIF((LOGIC)boolValue, 1, 0)
        END SWITCH
        THROW Error{"Unsupported SQL predicate"}
END CLASS

INTERNAL CLASS SqlCorrelatedInPlan
    PRIVATE _outerValueCodeBlock AS CODEBLOCK
    PRIVATE _innerValues AS Dictionary<USUAL,BYTE>
    PRIVATE _isNegated AS LOGIC

    INTERNAL CONSTRUCTOR(outerValueCodeBlock AS CODEBLOCK, innerValues AS Dictionary<USUAL,BYTE>, ;
        isNegated AS LOGIC)
        SELF:_outerValueCodeBlock := outerValueCodeBlock
        SELF:_innerValues := innerValues
        SELF:_isNegated := isNegated
        RETURN

    INTERNAL METHOD Matches() AS LOGIC
        LOCAL outerValue := (USUAL)SELF:_outerValueCodeBlock:Eval() AS USUAL
        LOCAL found := FALSE AS LOGIC
        IF !outerValue:IsNull
            found := SELF:_innerValues:ContainsKey(outerValue)
        ENDIF
        // The supported correlated equality filters the inner rows by this same
        // value, so NOT IN is the anti-semi-join complement, including NULL outer
        // values whose correlated subquery is empty.
        RETURN IIF(SELF:_isNegated, !found, found)
END CLASS

INTERNAL CLASS SqlCorrelationCounter
    INTERNAL PROPERTY Count AS INT AUTO
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

    PRIVATE STATIC METHOD PositionAggregateGroupSources(selectCtx AS FoxSelectContext, group AS SqlAggregateGroup) AS VOID
        IF group:SourceRecordNumbers == NULL .OR. group:SourceRecordNumbers:Length != selectCtx:TableList:Count
            RETURN
        ENDIF
        FOR LOCAL sourceIndex := 0 AS INT TO selectCtx:TableList:Count - 1
            DbGoto(group:SourceRecordNumbers[sourceIndex+1], selectCtx:TableList[sourceIndex])
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
        LOCAL descriptor := (STRING) structure[fieldPosition][2] AS STRING
        IF String.IsNullOrEmpty(descriptor)
            RETURN NULL
        ENDIF
        descriptor := descriptor:ToUpperInvariant()
        IF descriptor:Length == 1
            RETURN descriptor
        ENDIF
        // RddFieldInfo.FieldTypeFlags appends a colon and field flags.
        // Only nullability is needed here; other flags retain the fallback.
        IF descriptor:Length == 3 .AND. descriptor[1] == c':' .AND. descriptor[2] == c'0'
            RETURN descriptor:Substring(0, 1)
        ENDIF
        RETURN NULL

    PRIVATE STATIC METHOD IsHashJoinFieldType(fieldType AS STRING) AS LOGIC
        IF String.IsNullOrEmpty(fieldType)
            RETURN FALSE
        ENDIF

        SWITCH fieldType:ToUpperInvariant()
        CASE "N" // Numeric
        CASE "I" // Integer
            RETURN TRUE
        OTHERWISE
            // Other field types need comparison and hash compatibility coverage.
            RETURN FALSE
        END SWITCH

    PRIVATE STATIC METHOD IsCorrelatedScalarMemoFieldType(fieldType AS STRING) AS LOGIC
        IF String.IsNullOrEmpty(fieldType)
            RETURN FALSE
        ENDIF
        SWITCH fieldType:ToUpperInvariant()
        CASE "N" // Numeric
        CASE "I" // Integer
        CASE "Y" // Currency
        CASE "C" // Fixed-width character; memo keys use exact ordinal identity.
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH
    PRIVATE STATIC METHOD SqlOptimizationsEnabled() AS LOGIC
        RETURN EmbeddedSqlRuntimeOptions.OptimizationsEnabled()

    STATIC METHOD ParseSqlSelect(sCommand as STRING) AS FoxSelectContext
        VAR lexer := XSqlLexer{sCommand}
        VAR tokens := lexer:AllTokens()
        var parser := SQLParser{XTokenList{tokens}}
        var selectCtx := FoxSelectContext{}
        IF ! parser:ParseSelectStatement(out selectCtx)
            THROW Error{"Syntax error in SELECT command: "+parser:Error+CRLF+sCommand}
        ENDIF
        RETURN selectCtx

    PRIVATE STATIC METHOD PrepareSubqueryExpression(expression AS SqlExpressionContext, ;
        outerTableAliases := NULL AS Dictionary<STRING,STRING>, deferCorrelated := FALSE AS LOGIC, ;
        refreshCorrelated := FALSE AS LOGIC, ;
        scalarMemos := NULL AS Dictionary<SqlScalarSubqueryExpressionContext,SqlCorrelatedScalarMemo>) AS VOID
        IF expression == NULL
            RETURN
        ENDIF
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            VAR scalarQuery := scalarExpression:Subquery
            LOCAL scalarCorrelated := HasExternalSubqueryReference(scalarQuery) AS LOGIC
            IF refreshCorrelated .AND. !scalarCorrelated .AND. scalarExpression:IsPrepared
                RETURN
            ENDIF
            IF scalarCorrelated .AND. (outerTableAliases == NULL .OR. ;
               HasUnsupportedSubqueryCorrelation(scalarQuery, outerTableAliases))
                THROW Error{"Only references to the immediate outer query in a scalar subquery WHERE clause are supported"}
            ENDIF
            IF scalarQuery:SelectList:Count != 1
                THROW Error{"Scalar subquery must return exactly one column"}
            ENDIF
            IF scalarCorrelated .AND. deferCorrelated
                scalarExpression:Value := NULL
                IF scalarExpression:ResultFieldDefinition == NULL
                    scalarExpression:ResultFieldDefinition := InferCorrelatedScalarResultFieldDefinition(scalarQuery)
                ENDIF
                IF SqlOptimizationsEnabled() .AND. scalarMemos != NULL .AND. ;
                   !scalarMemos:ContainsKey(scalarExpression)
                    VAR deferredMemo := TryCreateCorrelatedScalarMemo(scalarQuery, outerTableAliases)
                    IF deferredMemo != NULL
                        scalarMemos[scalarExpression] := deferredMemo
                        System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.CorrelatedScalarCachePlans)
                    ENDIF
                ENDIF
                scalarExpression:IsPrepared := TRUE
                RETURN
            ENDIF
            LOCAL scalarMemo := NULL AS SqlCorrelatedScalarMemo
            LOCAL scalarMemoKey := USUAL[]{1} AS USUAL[]
            LOCAL useScalarMemo := FALSE AS LOGIC
            IF scalarMemos != NULL
                useScalarMemo := scalarMemos:TryGetValue(scalarExpression, OUT scalarMemo)
            ENDIF
            IF useScalarMemo
                scalarMemoKey := USUAL[]{scalarMemo:OuterValueCodeBlocks:Count}
                LOCAL hasNullScalarKey := FALSE AS LOGIC
                FOR LOCAL keyIndex := 1 AS INT TO scalarMemo:OuterValueCodeBlocks:Count
                    scalarMemoKey[keyIndex] := (USUAL)scalarMemo:OuterValueCodeBlocks[keyIndex-1]:Eval()
                    IF scalarMemoKey[keyIndex]:IsNull
                        hasNullScalarKey := TRUE
                        EXIT
                    ENDIF
                NEXT
                IF hasNullScalarKey
                    scalarExpression:Value := NULL
                    scalarExpression:IsPrepared := TRUE
                    System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.CorrelatedScalarNullKeys)
                    RETURN
                ENDIF
                LOCAL memoValue AS USUAL
                IF scalarMemo:Values:TryGetValue(scalarMemoKey, OUT memoValue)
                    scalarExpression:Value := memoValue
                    scalarExpression:IsPrepared := TRUE
                    System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.CorrelatedScalarCacheHits)
                    RETURN
                ENDIF
            ENDIF
            LOCAL scalarAlias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
            LOCAL savedScalarArea := VoDbGetSelect() AS DWORD
            LOCAL previousScalarTarget := scalarQuery:TargetCursor AS STRING
            LOCAL addedScalarAliases := List<STRING>{} AS List<STRING>
            scalarExpression:IsPrepared := FALSE
            TRY
                scalarQuery:TargetCursor := scalarAlias
                IF scalarCorrelated
                    FOREACH VAR outerAlias IN outerTableAliases
                        IF !scalarQuery:TableAliases:ContainsKey(outerAlias:Key)
                            scalarQuery:TableAliases[outerAlias:Key] := outerAlias:Value
                            addedScalarAliases:Add(outerAlias:Key)
                        ENDIF
                    NEXT
                ENDIF
                System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.ScalarExecutions)
                ExecuteSelect(scalarQuery)
                IF !Used(scalarAlias) .OR. (scalarAlias)->FCount() != 1
                    THROW Error{"Scalar subquery must return exactly one column"}
                ENDIF
                scalarExpression:ResultFieldDefinition := (scalarAlias)->DbStruct()[1]
                (scalarAlias)->DbGoTop()
                scalarExpression:Value := NULL
                IF !(scalarAlias)->Eof()
                    scalarExpression:Value := (USUAL)(scalarAlias)->FieldGet(1)
                    (scalarAlias)->DbSkip(1)
                    IF !(scalarAlias)->Eof()
                        THROW Error{"Scalar subquery returned more than one record"}
                    ENDIF
                ENDIF
                scalarExpression:IsPrepared := TRUE
                IF useScalarMemo
                    scalarMemo:Values[scalarMemoKey] := scalarExpression:Value
                ENDIF
            FINALLY
                scalarQuery:TargetCursor := previousScalarTarget
                FOREACH VAR outerAlias IN addedScalarAliases
                    scalarQuery:TableAliases:Remove(outerAlias)
                NEXT
                IF Used(scalarAlias)
                    (scalarAlias)->DbCloseArea()
                ENDIF
                IF savedScalarArea > 0
                    DbSelectArea(savedScalarArea)
                ENDIF
            END TRY
            RETURN
        ENDIF
        IF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            VAR existsQuery := existsExpression:Subquery
            VAR existsAggregates := List<SqlAggregateExpressionContext>{}
            FOREACH VAR selectedExpression IN existsQuery:SelectList
                CollectAggregates(selectedExpression, existsAggregates)
            NEXT
            IF existsAggregates:Count > 0
                THROW Error{"Aggregate expressions in EXISTS subqueries are not supported"}
            ENDIF
            LOCAL existsCorrelated := HasExternalSubqueryReference(existsQuery) AS LOGIC
            IF refreshCorrelated .AND. !existsCorrelated .AND. existsExpression:IsPrepared
                RETURN
            ENDIF
            IF existsCorrelated .AND. (outerTableAliases == NULL .OR. ;
               HasUnsupportedSubqueryCorrelation(existsQuery, outerTableAliases))
                THROW Error{"Only references to the immediate outer query in an EXISTS subquery WHERE clause are supported"}
            ENDIF
            IF existsCorrelated .AND. deferCorrelated
                existsExpression:HasRows := FALSE
                existsExpression:IsPrepared := TRUE
                RETURN
            ENDIF
            LOCAL existsAlias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
            LOCAL savedExistsArea := VoDbGetSelect() AS DWORD
            LOCAL addedExistsAliases := List<STRING>{} AS List<STRING>
            LOCAL previousTarget := existsQuery:TargetCursor AS STRING
            existsExpression:IsPrepared := FALSE
            TRY
                existsQuery:TargetCursor := existsAlias
                IF existsCorrelated
                    FOREACH VAR outerAlias IN outerTableAliases
                        IF !existsQuery:TableAliases:ContainsKey(outerAlias:Key)
                            existsQuery:TableAliases[outerAlias:Key] := outerAlias:Value
                            addedExistsAliases:Add(outerAlias:Key)
                        ENDIF
                    NEXT
                ENDIF
                System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.ExistsExecutions)
                ExecuteSelect(existsQuery)
                IF !Used(existsAlias)
                    THROW Error{"EXISTS subquery did not produce a result cursor"}
                ENDIF
                (existsAlias)->DbGoTop()
                existsExpression:HasRows := !(existsAlias)->Eof()
                existsExpression:IsPrepared := TRUE
            FINALLY
                existsQuery:TargetCursor := previousTarget
                FOREACH VAR outerAlias IN addedExistsAliases
                    existsQuery:TableAliases:Remove(outerAlias)
                NEXT
                IF Used(existsAlias)
                    (existsAlias)->DbCloseArea()
                ENDIF
                IF savedExistsArea > 0
                    DbSelectArea(savedExistsArea)
                ENDIF
            END TRY
            RETURN
        ENDIF
        IF expression IS SqlInSubqueryExpressionContext VAR inExpression
            VAR subquery := inExpression:Subquery
            LOCAL isCorrelated := HasExternalSubqueryReference(subquery) AS LOGIC
            IF refreshCorrelated .AND. !isCorrelated .AND. inExpression:Values != NULL
                RETURN
            ENDIF
            IF isCorrelated
                IF outerTableAliases == NULL .OR. HasUnsupportedSubqueryCorrelation(subquery, outerTableAliases)
                    THROW Error{"Only references to the immediate outer query in an IN subquery WHERE clause are supported"}
                ENDIF
                IF deferCorrelated
                    inExpression:Values := List<USUAL>{}
                    RETURN
                ENDIF
            ENDIF
            IF subquery:SelectList:Count != 1
                THROW Error{"IN subquery must return exactly one column"}
            ENDIF
            LOCAL alias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
            LOCAL previousInTarget := subquery:TargetCursor AS STRING
            subquery:TargetCursor := alias
            VAR savedArea := VoDbGetSelect()
            LOCAL addedOuterAliases := List<STRING>{} AS List<STRING>
            TRY
                // Correlated names are lexical references, not additional FROM
                // sources. Add their parent alias mappings only while resolving
                // and executing this inner SELECT.
                IF isCorrelated
                    FOREACH VAR outerAlias IN outerTableAliases
                        IF !subquery:TableAliases:ContainsKey(outerAlias:Key)
                            subquery:TableAliases[outerAlias:Key] := outerAlias:Value
                            addedOuterAliases:Add(outerAlias:Key)
                        ENDIF
                    NEXT
                ENDIF
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
                subquery:TargetCursor := previousInTarget
                FOREACH VAR outerAlias IN addedOuterAliases
                    subquery:TableAliases:Remove(outerAlias)
                NEXT
                IF Used(alias)
                    (alias)->DbCloseArea()
                ENDIF
                IF savedArea > 0
                    DbSelectArea(savedArea)
                ENDIF
            END TRY
            RETURN
        ENDIF
        IF expression IS SqlQuantifiedSubqueryExpressionContext VAR quantifiedExpression
            VAR quantifiedQuery := quantifiedExpression:Subquery
            LOCAL isQuantifiedCorrelated := HasExternalSubqueryReference(quantifiedQuery) AS LOGIC
            IF refreshCorrelated .AND. !isQuantifiedCorrelated .AND. quantifiedExpression:Values != NULL
                RETURN
            ENDIF
            IF isQuantifiedCorrelated
                IF outerTableAliases == NULL .OR. HasUnsupportedSubqueryCorrelation(quantifiedQuery, outerTableAliases)
                    THROW Error{"Only references to the immediate outer query in a quantified subquery WHERE clause are supported"}
                ENDIF
                IF deferCorrelated
                    quantifiedExpression:Values := List<USUAL>{}
                    RETURN
                ENDIF
            ENDIF
            IF quantifiedQuery:SelectList:Count != 1
                THROW Error{"Quantified subquery must return exactly one column"}
            ENDIF
            LOCAL quantifiedAlias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
            LOCAL previousQuantifiedTarget := quantifiedQuery:TargetCursor AS STRING
            quantifiedQuery:TargetCursor := quantifiedAlias
            VAR savedQuantifiedArea := VoDbGetSelect()
            LOCAL addedQuantifiedAliases := List<STRING>{} AS List<STRING>
            TRY
                IF isQuantifiedCorrelated
                    FOREACH VAR outerAlias IN outerTableAliases
                        IF !quantifiedQuery:TableAliases:ContainsKey(outerAlias:Key)
                            quantifiedQuery:TableAliases[outerAlias:Key] := outerAlias:Value
                            addedQuantifiedAliases:Add(outerAlias:Key)
                        ENDIF
                    NEXT
                ENDIF
                ExecuteSelect(quantifiedQuery)
                IF !Used(quantifiedAlias) .OR. (quantifiedAlias)->FCount() != 1
                    THROW Error{"Quantified subquery must return exactly one column"}
                ENDIF
                VAR quantifiedValues := List<USUAL>{}
                (quantifiedAlias)->DbGoTop()
                DO WHILE !(quantifiedAlias)->Eof()
                    quantifiedValues:Add((USUAL)(quantifiedAlias)->FieldGet(1))
                    (quantifiedAlias)->DbSkip(1)
                ENDDO
                quantifiedExpression:Values := quantifiedValues
            FINALLY
                quantifiedQuery:TargetCursor := previousQuantifiedTarget
                FOREACH VAR outerAlias IN addedQuantifiedAliases
                    quantifiedQuery:TableAliases:Remove(outerAlias)
                NEXT
                IF Used(quantifiedAlias)
                    (quantifiedAlias)->DbCloseArea()
                ENDIF
                IF savedQuantifiedArea > 0
                    DbSelectArea(savedQuantifiedArea)
                ENDIF
            END TRY
            RETURN
        ENDIF
        IF expression IS SqlParenExpressionContext VAR parenExpression
            PrepareSubqueryExpression(parenExpression:Expr, outerTableAliases, deferCorrelated, refreshCorrelated, scalarMemos)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            PrepareSubqueryExpression(prefixExpression:Expr, outerTableAliases, deferCorrelated, refreshCorrelated, scalarMemos)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            PrepareSubqueryExpression(binaryExpression:Left, outerTableAliases, deferCorrelated, refreshCorrelated, scalarMemos)
            PrepareSubqueryExpression(binaryExpression:Right, outerTableAliases, deferCorrelated, refreshCorrelated, scalarMemos)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                PrepareSubqueryExpression(child, outerTableAliases, deferCorrelated, refreshCorrelated, scalarMemos)
            NEXT
        ENDIF
        RETURN

    PRIVATE STATIC METHOD TryCreateCorrelatedScalarMemo(subquery AS FoxSelectContext, ;
        outerTableAliases AS Dictionary<STRING,STRING>) AS SqlCorrelatedScalarMemo
        IF subquery:SelectList:Count != 1 .OR. subquery:TableList:Count == 0 .OR. ;
           subquery:GroupByItems:Count != 0 .OR. ;
           subquery:HavingClause != NULL .OR. subquery:OrderByClause:Count != 0 .OR. ;
           subquery:UnionBranches:Count != 0 .OR. subquery:IsDistinct .OR. ;
           !String.IsNullOrEmpty(subquery:TopCount)
            RETURN NULL
        ENDIF

        LOCAL correlation AS SqlCorrelationCounter
        correlation := SqlCorrelationCounter{}
        LOCAL outerReferences := List<SqlNameExpressionContext>{} AS List<SqlNameExpressionContext>
        VAR residualWhere := ExtractCorrelatedScalarEquality(subquery:WhereClause, ;
            subquery:TableAliases, outerTableAliases, correlation, outerReferences)
        IF correlation:Count < 1 .OR. outerReferences:Count != correlation:Count .OR. ;
           (residualWhere != NULL .AND. ;
            (HasExternalSubqueryDependency(residualWhere, subquery:TableAliases) .OR. ;
             ContainsInSubqueryExpression(residualWhere)))
            RETURN NULL
        ENDIF
        LOCAL outerValueCodeBlocks := List<CODEBLOCK>{} AS List<CODEBLOCK>
        FOREACH VAR outerField IN outerReferences
            LOCAL resolvedOuterTable := outerField:Table AS STRING
            outerTableAliases:TryGetValue(outerField:Table, OUT resolvedOuterTable)
            IF !IsCorrelatedScalarMemoFieldType(HashJoinFieldType(resolvedOuterTable, outerField:Name))
                RETURN NULL
            ENDIF
            VAR outerValueCodeBlock := MCompile(outerField:ToResolvedString(outerTableAliases))
            IF outerValueCodeBlock == NULL
                RETURN NULL
            ENDIF
            outerValueCodeBlocks:Add(outerValueCodeBlock)
        NEXT
        RETURN SqlCorrelatedScalarMemo{outerValueCodeBlocks}

    PRIVATE STATIC METHOD ExtractCorrelatedScalarEquality(expression AS SqlExpressionContext, ;
        innerAliases AS Dictionary<STRING,STRING>, ;
        outerAliases AS Dictionary<STRING,STRING>, counter AS SqlCorrelationCounter, ;
        outerFields AS IList<SqlNameExpressionContext>) AS SqlExpressionContext
        IF expression == NULL
            RETURN NULL
        ENDIF
        IF expression IS SqlParenExpressionContext VAR paren .AND. paren:Expr IS SqlCompareExpressionContext
            VAR residual := ExtractCorrelatedScalarEquality(paren:Expr, innerAliases, ;
                outerAliases, counter, outerFields)
            IF residual == NULL
                RETURN NULL
            ENDIF
            RETURN SqlParenExpressionContext{} { Open := paren:Open, Expr := residual, Close := paren:Close }
        ENDIF
        IF expression IS SqlLogicExpressionContext VAR logicNode .AND. logicNode:Op:Type == XTokenType.AND
            VAR leftResidual := ExtractCorrelatedScalarEquality(logicNode:Left, innerAliases, ;
                outerAliases, counter, outerFields)
            VAR rightResidual := ExtractCorrelatedScalarEquality(logicNode:Right, innerAliases, ;
                outerAliases, counter, outerFields)
            IF leftResidual == NULL
                RETURN rightResidual
            ELSEIF rightResidual == NULL
                RETURN leftResidual
            ENDIF
            RETURN SqlLogicExpressionContext{} { Left := leftResidual, Op := logicNode:Op, Right := rightResidual }
        ENDIF
        IF expression IS SqlCompareExpressionContext VAR compare .AND. compare:Op:Type == XTokenType.EQ
            LOCAL inner := NULL AS SqlNameExpressionContext
            LOCAL outer := NULL AS SqlNameExpressionContext
            IF compare:Left IS SqlNameExpressionContext VAR leftName .AND. ;
               compare:Right IS SqlNameExpressionContext VAR rightName
                IF !String.IsNullOrEmpty(leftName:Table) .AND. !String.IsNullOrEmpty(rightName:Table)
                    IF innerAliases:ContainsKey(leftName:Table) .AND. outerAliases:ContainsKey(rightName:Table)
                        inner := leftName
                        outer := rightName
                    ELSEIF innerAliases:ContainsKey(rightName:Table) .AND. outerAliases:ContainsKey(leftName:Table)
                        inner := rightName
                        outer := leftName
                    ENDIF
                ENDIF
            ENDIF
            IF inner != NULL .AND. outer != NULL
                counter:Count++
                outerFields:Add(outer)
                RETURN NULL
            ENDIF
        ENDIF
        RETURN expression

    PRIVATE STATIC METHOD HasCorrelatedInSubquery(expression AS SqlExpressionContext, ;
        tableAliases AS Dictionary<STRING,STRING>) AS LOGIC
        IF expression == NULL
            RETURN FALSE
        ENDIF
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            IF HasExternalSubqueryReference(scalarExpression:Subquery)
                IF HasUnsupportedSubqueryCorrelation(scalarExpression:Subquery, tableAliases)
                    THROW Error{"Only references to the immediate outer query in a scalar subquery WHERE clause are supported"}
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
        ENDIF
        IF expression IS SqlQuantifiedSubqueryExpressionContext VAR quantifiedExpression
            IF HasExternalSubqueryReference(quantifiedExpression:Subquery)
                IF HasUnsupportedSubqueryCorrelation(quantifiedExpression:Subquery, tableAliases)
                    THROW Error{"Only references to the immediate outer query in a quantified subquery WHERE clause are supported"}
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
        ENDIF
        IF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            IF HasExternalSubqueryReference(existsExpression:Subquery)
                IF HasUnsupportedSubqueryCorrelation(existsExpression:Subquery, tableAliases)
                    THROW Error{"Only references to the immediate outer query in an EXISTS subquery WHERE clause are supported"}
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
        ENDIF
        IF expression IS SqlInSubqueryExpressionContext VAR inExpression
            IF HasExternalSubqueryReference(inExpression:Subquery)
                IF HasUnsupportedSubqueryCorrelation(inExpression:Subquery, tableAliases)
                    THROW Error{"Only references to the immediate outer query in an IN subquery WHERE clause are supported"}
                ENDIF
                RETURN TRUE
            ENDIF
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN HasCorrelatedInSubquery(parenExpression:Expr, tableAliases)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            RETURN HasCorrelatedInSubquery(prefixExpression:Expr, tableAliases)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            RETURN HasCorrelatedInSubquery(binaryExpression:Left, tableAliases) .OR. ;
                   HasCorrelatedInSubquery(binaryExpression:Right, tableAliases)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                IF HasCorrelatedInSubquery(child, tableAliases)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD HasUnsupportedSubqueryCorrelation(subquery AS FoxSelectContext, ;
        outerTableAliases AS Dictionary<STRING,STRING>) AS LOGIC
        // This incremental implementation supports correlation only from the
        // subquery WHERE clause to aliases in its immediate parent SELECT.
        FOREACH VAR expression IN subquery:SelectList
            IF HasExternalSubqueryDependency(expression, subquery:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        IF HasExternalSubqueryDependency(subquery:HavingClause, subquery:TableAliases)
            RETURN TRUE
        ENDIF
        FOREACH VAR expression IN subquery:GroupByItems
            IF HasExternalSubqueryDependency(expression, subquery:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        FOREACH VAR join IN subquery:Joins
            IF HasExternalSubqueryDependency(join:OnClause, subquery:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        FOREACH VAR orderItem IN subquery:OrderByClause
            IF HasExternalSubqueryDependency(orderItem:Expression, subquery:TableAliases)
                RETURN TRUE
            ENDIF
        NEXT
        IF subquery:UnionBranches:Count > 0
            RETURN TRUE
        ENDIF
        IF subquery:WhereClause == NULL
            RETURN TRUE
        ENDIF
        FOREACH VAR dependency IN subquery:WhereClause:GetTableDependencies(subquery:TableAliases)
            LOCAL isInnerTable := FALSE AS LOGIC
            FOREACH VAR innerTable IN subquery:TableAliases:Values
                IF innerTable:Equals(dependency, StringComparison.OrdinalIgnoreCase)
                    isInnerTable := TRUE
                    EXIT
                ENDIF
            NEXT
            IF isInnerTable
                LOOP
            ENDIF
            IF !outerTableAliases:ContainsKey(dependency)
                LOCAL isOuterTable := FALSE AS LOGIC
                FOREACH VAR outerTable IN outerTableAliases:Values
                    IF outerTable:Equals(dependency, StringComparison.OrdinalIgnoreCase)
                        isOuterTable := TRUE
                        EXIT
                    ENDIF
                NEXT
                IF !isOuterTable
                    RETURN TRUE
                ENDIF
            ENDIF
        NEXT
        RETURN FALSE

    // Validate lexical scopes before correlation aliases are temporarily added
    // to a subquery. Otherwise a grandparent can appear to be its parent.
    PRIVATE STATIC METHOD ContainsScalarSubquery(expression AS SqlExpressionContext) AS LOGIC
        IF expression IS SqlScalarSubqueryExpressionContext .OR. expression IS SqlQuantifiedSubqueryExpressionContext
            RETURN TRUE
        ELSEIF expression IS SqlParenExpressionContext VAR paren
            RETURN ContainsScalarSubquery(paren:Expr)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefix
            RETURN ContainsScalarSubquery(prefix:Expr)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binary
            RETURN ContainsScalarSubquery(binary:Left) .OR. ContainsScalarSubquery(binary:Right)
        ELSEIF expression IS SqlCompositeExpressionContext VAR composite
            FOREACH VAR item IN composite:Exprs
                IF ContainsScalarSubquery(item)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    // Only a positive conjunct can safely discard rows before the full WHERE.
    PRIVATE STATIC METHOD FindMembershipCandidate(expression AS SqlExpressionContext, ;
        aliases AS Dictionary<STRING,STRING>, tableName AS STRING) AS SqlInSubqueryExpressionContext
        IF expression IS SqlParenExpressionContext VAR paren
            RETURN FindMembershipCandidate(paren:Expr, aliases, tableName)
        ELSEIF expression IS SqlLogicExpressionContext VAR logicExpression .AND. ;
               logicExpression:Op:Type == XTokenType.AND
            VAR candidate := FindMembershipCandidate(logicExpression:Left, aliases, tableName)
            IF candidate != NULL
                RETURN candidate
            ENDIF
            RETURN FindMembershipCandidate(logicExpression:Right, aliases, tableName)
        ELSEIF expression IS SqlInSubqueryExpressionContext VAR inExpression
            IF inExpression:Values == NULL .OR. HasExternalSubqueryReference(inExpression:Subquery) .OR. ;
               !(inExpression:ValueExpression IS SqlNameExpressionContext VAR field) .OR. ;
               String.IsNullOrEmpty(field:Table)
                RETURN NULL
            ENDIF
            LOCAL resolved := field:Table AS STRING
            IF aliases:ContainsKey(field:Table)
                resolved := aliases[field:Table]
            ENDIF
            IF String.Equals(resolved, tableName, StringComparison.OrdinalIgnoreCase)
                RETURN inExpression
            ENDIF
        ENDIF
        RETURN NULL

    // Pruning must not suppress a scalar cardinality error or a failure opening
    // a correlated source. Only admit correlated shapes whose execution is
    // known to be single-row (COUNT) or existence-only and uses open fields.
    PRIVATE STATIC METHOD IsSafeCorrelatedCandidate(query AS FoxSelectContext, ;
        outerAliases AS Dictionary<STRING,STRING>, scalar AS LOGIC) AS LOGIC
        IF query:TableList:Count != 1 .OR. query:SelectList:Count != 1 .OR. ;
           query:Joins:Count != 0 .OR. query:GroupByItems:Count != 0 .OR. ;
           query:HavingClause != NULL .OR. query:UnionBranches:Count != 0 .OR. ;
           query:OrderByClause:Count != 0 .OR. query:IsDistinct .OR. ;
           !String.IsNullOrEmpty(query:TopCount)
            RETURN FALSE
        ENDIF
        LOCAL innerTable := (STRING)query:TableList[0] AS STRING
        IF !Used(innerTable)
            RETURN FALSE
        ENDIF
        IF scalar
            IF !(query:SelectList[0] IS SqlAggregateExpressionContext VAR aggregate) .OR. ;
               !aggregate:IsStar .OR. ;
               !(String.Equals(aggregate:FunctionName, "COUNT", StringComparison.OrdinalIgnoreCase) .OR. ;
                 String.Equals(aggregate:FunctionName, "CNT", StringComparison.OrdinalIgnoreCase))
                RETURN FALSE
            ENDIF
        ELSE
            IF !(query:SelectList[0] IS SqlNameExpressionContext VAR selected) .OR. ;
               String.IsNullOrEmpty(selected:Table) .OR. !query:TableAliases:ContainsKey(selected:Table) .OR. ;
               !String.Equals(query:TableAliases[selected:Table], innerTable, StringComparison.OrdinalIgnoreCase) .OR. ;
               (innerTable)->FieldPos(selected:Name) == 0 .OR. ;
               !IsHashJoinFieldType(HashJoinFieldType(innerTable, selected:Name))
                RETURN FALSE
            ENDIF
        ENDIF
        IF !(query:WhereClause IS SqlCompareExpressionContext VAR comparison) .OR. ;
           !(comparison:Left IS SqlNameExpressionContext VAR leftField) .OR. ;
           !(comparison:Right IS SqlNameExpressionContext VAR rightField) .OR. ;
           comparison:Op:Type != XTokenType.EQ
            RETURN FALSE
        ENDIF
        IF String.IsNullOrEmpty(leftField:Table) .OR. String.IsNullOrEmpty(rightField:Table)
            RETURN FALSE
        ENDIF
        LOCAL innerField AS SqlNameExpressionContext
        LOCAL outerField AS SqlNameExpressionContext
        IF query:TableAliases:ContainsKey(leftField:Table) .AND. outerAliases:ContainsKey(rightField:Table)
            innerField := leftField
            outerField := rightField
        ELSEIF query:TableAliases:ContainsKey(rightField:Table) .AND. outerAliases:ContainsKey(leftField:Table)
            innerField := rightField
            outerField := leftField
        ELSE
            RETURN FALSE
        ENDIF
        VAR outerTable := outerAliases[outerField:Table]
        IF !String.Equals(query:TableAliases[innerField:Table], innerTable, StringComparison.OrdinalIgnoreCase) .OR. ;
           !Used(outerTable) .OR. (innerTable)->FieldPos(innerField:Name) == 0 .OR. ;
           (outerTable)->FieldPos(outerField:Name) == 0
            RETURN FALSE
        ENDIF
        VAR innerType := HashJoinFieldType(innerTable, innerField:Name)
        VAR outerType := HashJoinFieldType(outerTable, outerField:Name)
        RETURN IsHashJoinFieldType(innerType) .AND. String.Equals(innerType, outerType, StringComparison.OrdinalIgnoreCase)

    PRIVATE STATIC METHOD HasUnsafeCorrelatedCandidate(expression AS SqlExpressionContext, ;
        outerAliases AS Dictionary<STRING,STRING>) AS LOGIC
        IF expression IS SqlQuantifiedSubqueryExpressionContext
            RETURN TRUE
        ELSEIF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            RETURN HasExternalSubqueryReference(scalarExpression:Subquery) .AND. ;
                   !IsSafeCorrelatedCandidate(scalarExpression:Subquery, outerAliases, TRUE)
        ELSEIF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            RETURN HasExternalSubqueryReference(existsExpression:Subquery) .AND. ;
                   !IsSafeCorrelatedCandidate(existsExpression:Subquery, outerAliases, FALSE)
        ELSEIF expression IS SqlInSubqueryExpressionContext VAR inExpression
            RETURN HasExternalSubqueryReference(inExpression:Subquery)
        ELSEIF expression IS SqlParenExpressionContext VAR paren
            RETURN HasUnsafeCorrelatedCandidate(paren:Expr, outerAliases)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefix
            RETURN HasUnsafeCorrelatedCandidate(prefix:Expr, outerAliases)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binary
            RETURN HasUnsafeCorrelatedCandidate(binary:Left, outerAliases) .OR. ;
                   HasUnsafeCorrelatedCandidate(binary:Right, outerAliases)
        ELSEIF expression IS SqlCompositeExpressionContext VAR composite
            FOREACH VAR item IN composite:Exprs
                IF HasUnsafeCorrelatedCandidate(item, outerAliases)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD IsSafeMembershipOperand(expression AS SqlExpressionContext, ;
        outerAliases AS Dictionary<STRING,STRING>) AS LOGIC
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            RETURN !HasExternalSubqueryReference(scalarExpression:Subquery) .OR. ;
                   IsSafeCorrelatedCandidate(scalarExpression:Subquery, outerAliases, TRUE)
        ELSEIF expression IS SqlNameExpressionContext VAR field
            IF String.IsNullOrEmpty(field:Table) .OR. !outerAliases:ContainsKey(field:Table)
                RETURN FALSE
            ENDIF
            VAR tableName := outerAliases[field:Table]
            RETURN Used(tableName) .AND. (tableName)->FieldPos(field:Name) > 0
        ELSEIF expression IS SqlSimpleExpressionContext VAR simple
            IF simple:Tokens == NULL .OR. simple:Tokens:Count != 1
                RETURN FALSE
            ENDIF
            VAR tokenType := simple:Tokens[0]:Type
            RETURN tokenType == XTokenType.NUMERIC_CONST .OR. tokenType == XTokenType.STRING_CONST .OR. ;
                   tokenType == XTokenType.DATE_CONST .OR. tokenType == XTokenType.DATETIME_CONST .OR. ;
                   tokenType == XTokenType.TRUE_CONST .OR. tokenType == XTokenType.FALSE_CONST
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD IsNumericMembershipOperand(expression AS SqlExpressionContext, ;
        outerAliases AS Dictionary<STRING,STRING>) AS LOGIC
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            // Deferred correlated values have not been read yet. Eligibility
            // already restricts them to COUNT(*), which is numeric.
            RETURN IsNil(scalarExpression:Value) .OR. scalarExpression:Value:IsNull .OR. scalarExpression:Value:IsNumeric
        ELSEIF expression IS SqlNameExpressionContext VAR field
            IF String.IsNullOrEmpty(field:Table) .OR. !outerAliases:ContainsKey(field:Table)
                RETURN FALSE
            ENDIF
            RETURN IsHashJoinFieldType(HashJoinFieldType(outerAliases[field:Table], field:Name))
        ELSEIF expression IS SqlSimpleExpressionContext VAR simple
            RETURN simple:Tokens != NULL .AND. simple:Tokens:Count == 1 .AND. ;
                   simple:Tokens[0]:Type == XTokenType.NUMERIC_CONST
        ENDIF
        RETURN FALSE

    // Do not skip evaluation of volatile expressions or expressions that can
    // fail on rows rejected by the bitmap.
    PRIVATE STATIC METHOD IsSafeMembershipRemainder(expression AS SqlExpressionContext, ;
        outerAliases AS Dictionary<STRING,STRING>, hasScalar AS LOGIC) AS LOGIC
        IF expression IS SqlParenExpressionContext VAR paren
            RETURN IsSafeMembershipRemainder(paren:Expr, outerAliases, hasScalar)
        ELSEIF expression IS SqlLogicExpressionContext VAR logicExpression
            RETURN logicExpression:Op:Type == XTokenType.AND .AND. ;
                   IsSafeMembershipRemainder(logicExpression:Left, outerAliases, hasScalar) .AND. ;
                   IsSafeMembershipRemainder(logicExpression:Right, outerAliases, hasScalar)
        ELSEIF expression IS SqlCompareExpressionContext VAR comparison
            RETURN IsSafeMembershipOperand(comparison:Left, outerAliases) .AND. ;
                   IsSafeMembershipOperand(comparison:Right, outerAliases) .AND. ;
                   (!hasScalar .OR. (IsNumericMembershipOperand(comparison:Left, outerAliases) .AND. ;
                                     IsNumericMembershipOperand(comparison:Right, outerAliases)))
        ELSEIF expression IS SqlInSubqueryExpressionContext VAR inExpression
            RETURN inExpression:Values != NULL .AND. !HasExternalSubqueryReference(inExpression:Subquery) .AND. ;
                   IsSafeMembershipOperand(inExpression:ValueExpression, outerAliases)
        ELSEIF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            RETURN !HasExternalSubqueryReference(existsExpression:Subquery) .OR. ;
                   IsSafeCorrelatedCandidate(existsExpression:Subquery, outerAliases, FALSE)
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD BuildMembershipCandidates(expression AS SqlInSubqueryExpressionContext, ;
        aliases AS Dictionary<STRING,STRING>, tableName AS STRING) AS List<LONG>
        LOCAL selectedArea := VoDbGetSelect() AS DWORD
        LOCAL savedRecNo := (LONG)(tableName)->RecNo() AS LONG
        LOCAL wasBof := (tableName)->Bof() AS LOGIC
        LOCAL wasEof := (tableName)->Eof() AS LOGIC
        LOCAL candidates := NULL AS List<LONG>
        TRY
            VAR predicate := MCompile(expression:ToResolvedString(aliases))
            IF predicate == NULL
                RETURN NULL
            ENDIF
            VAR bitmap := RecordBitmap{}
            bitmap:Length := (LONG)(tableName)->RecCount()
            VAR visited := List<LONG>{}
            (tableName)->DbGoTop()
            DO WHILE !(tableName)->Eof()
                LOCAL recordNumber := (LONG)(tableName)->RecNo() AS LONG
                VAR matchValue := (USUAL)predicate:Eval()
                visited:Add(recordNumber)
                IF !IsNil(matchValue) .AND. !matchValue:IsNull .AND. (LOGIC)matchValue
                    bitmap:SetMatchState(recordNumber, RecordBitmap.RecordMatchState.Match)
                ENDIF
                (tableName)->DbSkip(1)
            ENDDO
            candidates := List<LONG>{}
            FOREACH VAR recordNumber IN visited
                IF bitmap:GetMatchState(recordNumber) == RecordBitmap.RecordMatchState.Match
                    candidates:Add(recordNumber)
                ENDIF
            NEXT
            System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.MembershipBitmaps)
            System.Threading.Interlocked.Add(REF SqlOptimizationDiagnostics.MembershipCandidates, candidates:Count)
            System.Threading.Interlocked.Add(REF SqlOptimizationDiagnostics.MembershipScanned, visited:Count)
        CATCH ex AS Exception
            candidates := NULL
        FINALLY
            IF wasBof .AND. !wasEof
                (tableName)->DbGoTop()
                (tableName)->DbSkip(-1)
            ELSEIF wasEof
                (tableName)->DbGoBottom()
                (tableName)->DbSkip(1)
            ELSE
                DbGoto(savedRecNo, tableName)
            ENDIF
            IF selectedArea > 0
                DbSelectArea(selectedArea)
            ENDIF
        END TRY
        RETURN candidates

    PRIVATE STATIC METHOD BuildThreeValueOperand(expression AS SqlExpressionContext, ;
        aliases AS Dictionary<STRING,STRING>) AS SqlThreeValuePredicate
        VAR operand := SqlThreeValuePredicate{}
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalar
            operand:Scalar := scalar
        ELSE
            IF ContainsScalarSubquery(expression) .OR. ContainsInSubqueryExpression(expression)
                THROW Error{"Unsupported subquery inside a SQL value expression"}
            ENDIF
            operand:ValueBlock := MCompile(expression:ToResolvedString(aliases))
            IF operand:ValueBlock == NULL
                THROW Error{"Could not compile SQL predicate operand"}
            ENDIF
        ENDIF
        RETURN operand

    PRIVATE STATIC METHOD BuildThreeValuePredicate(expression AS SqlExpressionContext, ;
        aliases AS Dictionary<STRING,STRING>) AS SqlThreeValuePredicate
        VAR node := SqlThreeValuePredicate{}
        IF expression IS SqlParenExpressionContext VAR paren
            RETURN BuildThreeValuePredicate(paren:Expr, aliases)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefix .AND. prefix:Op:Type == XTokenType.NOT
            node:Kind := "NOT"
            node:Left := BuildThreeValuePredicate(prefix:Expr, aliases)
        ELSEIF expression IS SqlLogicExpressionContext VAR logicExpression
            IF logicExpression:Op:Type == XTokenType.AND
                node:Kind := "AND"
            ELSEIF logicExpression:Op:Type == XTokenType.OR
                node:Kind := "OR"
            ELSE
                THROW Error{"Unsupported logical operator in scalar subquery predicate"}
            ENDIF
            node:Left := BuildThreeValuePredicate(logicExpression:Left, aliases)
            node:Right := BuildThreeValuePredicate(logicExpression:Right, aliases)
        ELSEIF expression IS SqlCompareExpressionContext VAR comparison
            node:Kind := "COMPARE"
            node:Op := comparison:Op:Type
            node:Left := BuildThreeValueOperand(comparison:Left, aliases)
            node:Right := BuildThreeValueOperand(comparison:Right, aliases)
        ELSEIF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            node:Kind := "EXISTS"
            node:ExistsQuery := existsExpression
        ELSEIF expression IS SqlInSubqueryExpressionContext VAR inExpression
            node:Kind := "IN"
            node:InQuery := inExpression
            node:Negated := inExpression:IsNegated
            node:Left := BuildThreeValueOperand(inExpression:ValueExpression, aliases)
        ELSEIF expression IS SqlQuantifiedSubqueryExpressionContext VAR quantifiedExpression
            node:Kind := "QUANTIFIED"
            node:QuantifiedQuery := quantifiedExpression
            node:Op := quantifiedExpression:Op:Type
            node:Left := BuildThreeValueOperand(quantifiedExpression:ValueExpression, aliases)
        ELSEIF expression IS SqlInListExpressionContext VAR inList
            node:Kind := "IN"
            node:Negated := inList:IsNegated
            node:Left := BuildThreeValueOperand(inList:ValueExpression, aliases)
            node:Items := List<SqlThreeValuePredicate>{}
            FOREACH VAR item IN inList:Items
                node:Items:Add(BuildThreeValueOperand(item, aliases))
            NEXT
        ELSEIF expression IS SqlNameExpressionContext .OR. expression IS SqlSimpleExpressionContext
            node := BuildThreeValueOperand(expression, aliases)
            node:Kind := "BOOL"
        ELSE
            THROW Error{"Unsupported predicate in WHERE containing a scalar subquery"}
        ENDIF
        RETURN node

    PRIVATE STATIC METHOD ValidateNestedSubqueries(expression AS SqlExpressionContext, ;
        parentAliases AS Dictionary<STRING,STRING>) AS VOID
        IF expression == NULL
            RETURN
        ENDIF
        LOCAL child := NULL AS FoxSelectContext
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            child := scalarExpression:Subquery
        ELSEIF expression IS SqlExistsSubqueryExpressionContext VAR existsExpression
            child := existsExpression:Subquery
        ELSEIF expression IS SqlInSubqueryExpressionContext VAR inExpression
            ValidateNestedSubqueries(inExpression:ValueExpression, parentAliases)
            child := inExpression:Subquery
        ELSEIF expression IS SqlQuantifiedSubqueryExpressionContext VAR quantifiedExpression
            ValidateNestedSubqueries(quantifiedExpression:ValueExpression, parentAliases)
            child := quantifiedExpression:Subquery
        ELSEIF expression IS SqlParenExpressionContext VAR paren
            ValidateNestedSubqueries(paren:Expr, parentAliases)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefix
            ValidateNestedSubqueries(prefix:Expr, parentAliases)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binary
            ValidateNestedSubqueries(binary:Left, parentAliases)
            ValidateNestedSubqueries(binary:Right, parentAliases)
        ELSEIF expression IS SqlCompositeExpressionContext VAR composite
            FOREACH VAR item IN composite:Exprs
                ValidateNestedSubqueries(item, parentAliases)
            NEXT
        ELSEIF expression IS SqlInListExpressionContext VAR inList
            ValidateNestedSubqueries(inList:ValueExpression, parentAliases)
            FOREACH VAR item IN inList:Items
                ValidateNestedSubqueries(item, parentAliases)
            NEXT
        ENDIF
        IF child != NULL
            IF HasExternalSubqueryReference(child) .AND. HasUnsupportedSubqueryCorrelation(child, parentAliases)
                THROW Error{"Only references to the immediate outer query in a subquery WHERE clause are supported"}
            ENDIF
            ValidateSelectSubqueries(child)
        ENDIF
        RETURN

    PRIVATE STATIC METHOD ValidateSelectSubqueries(query AS FoxSelectContext) AS VOID
        FOREACH VAR derivedEntry IN query:DerivedTables
            IF HasExternalSubqueryReference(derivedEntry:Value)
                THROW Error{"Derived table subqueries cannot be correlated"}
            ENDIF
            ValidateSelectSubqueries(derivedEntry:Value)
        NEXT
        FOREACH VAR expression IN query:SelectList
            IF HasCorrelatedProjectionSubquery(expression) .AND. IsAggregateSelect(query)
                ValidateCorrelatedProjectionGrouping(expression, query)
            ENDIF
            ValidateNestedSubqueries(expression, query:TableAliases)
        NEXT
        ValidateNestedSubqueries(query:WhereClause, query:TableAliases)
        FOREACH VAR branch IN query:UnionBranches
            ValidateSelectSubqueries(branch)
        NEXT
        RETURN

    PRIVATE STATIC METHOD HasCorrelatedProjectionSubquery(expression AS SqlExpressionContext) AS LOGIC
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            RETURN HasExternalSubqueryReference(scalarExpression:Subquery)
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN HasCorrelatedProjectionSubquery(parenExpression:Expr)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            RETURN HasCorrelatedProjectionSubquery(prefixExpression:Expr)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            RETURN HasCorrelatedProjectionSubquery(binaryExpression:Left) .OR. ;
                   HasCorrelatedProjectionSubquery(binaryExpression:Right)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                IF HasCorrelatedProjectionSubquery(child)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD IsAggregateSelect(query AS FoxSelectContext) AS LOGIC
        IF query:GroupByItems:Count > 0
            RETURN TRUE
        ENDIF
        VAR aggregates := List<SqlAggregateExpressionContext>{}
        FOREACH VAR expression IN query:SelectList
            CollectAggregates(expression, aggregates)
        NEXT
        CollectAggregates(query:HavingClause, aggregates)
        FOREACH VAR orderItem IN query:OrderByClause
            CollectAggregates(orderItem:Expression, aggregates)
        NEXT
        RETURN aggregates:Count > 0

    PRIVATE STATIC METHOD ValidateCorrelatedProjectionGrouping(expression AS SqlExpressionContext, ;
            parentQuery AS FoxSelectContext) AS VOID
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            VAR subquery := scalarExpression:Subquery
            IF HasExternalSubqueryReference(subquery) .AND. ;
               HasUngroupedOuterReference(subquery:WhereClause, subquery:TableAliases, parentQuery)
                THROW Error{"Correlated scalar projection references must appear in GROUP BY"}
            ENDIF
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            ValidateCorrelatedProjectionGrouping(parenExpression:Expr, parentQuery)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            ValidateCorrelatedProjectionGrouping(prefixExpression:Expr, parentQuery)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            ValidateCorrelatedProjectionGrouping(binaryExpression:Left, parentQuery)
            ValidateCorrelatedProjectionGrouping(binaryExpression:Right, parentQuery)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                ValidateCorrelatedProjectionGrouping(child, parentQuery)
            NEXT
        ENDIF
        RETURN

    PRIVATE STATIC METHOD HasUngroupedOuterReference(expression AS SqlExpressionContext, ;
            innerAliases AS Dictionary<STRING,STRING>, parentQuery AS FoxSelectContext) AS LOGIC
        IF expression == NULL
            RETURN FALSE
        ENDIF
        IF expression IS SqlNameExpressionContext VAR nameExpression
            IF !String.IsNullOrEmpty(nameExpression:Table) .AND. ;
               !innerAliases:ContainsKey(nameExpression:Table) .AND. ;
               parentQuery:TableAliases:ContainsKey(nameExpression:Table)
                VAR resolvedReference := nameExpression:ToResolvedString(parentQuery:TableAliases)
                FOREACH VAR groupItem IN parentQuery:GroupByItems
                    VAR resolvedGroupExpression := ResolveGroupItem(groupItem, parentQuery)
                    VAR resolvedGroup := resolvedGroupExpression:ToResolvedString(parentQuery:TableAliases)
                    IF resolvedReference:Equals(resolvedGroup, StringComparison.OrdinalIgnoreCase)
                        RETURN FALSE
                    ENDIF
                NEXT
                RETURN TRUE
            ENDIF
            RETURN FALSE
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN HasUngroupedOuterReference(parenExpression:Expr, innerAliases, parentQuery)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            RETURN HasUngroupedOuterReference(prefixExpression:Expr, innerAliases, parentQuery)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            RETURN HasUngroupedOuterReference(binaryExpression:Left, innerAliases, parentQuery) .OR. ;
                   HasUngroupedOuterReference(binaryExpression:Right, innerAliases, parentQuery)
        ELSEIF expression IS SqlAggregateExpressionContext VAR aggregateExpression
            RETURN HasUngroupedOuterReference(aggregateExpression:Argument, innerAliases, parentQuery)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                IF HasUngroupedOuterReference(child, innerAliases, parentQuery)
                    RETURN TRUE
                ENDIF
            NEXT
        ELSEIF expression IS SqlInListExpressionContext VAR inListExpression
            IF HasUngroupedOuterReference(inListExpression:ValueExpression, innerAliases, parentQuery)
                RETURN TRUE
            ENDIF
            FOREACH VAR item IN inListExpression:Items
                IF HasUngroupedOuterReference(item, innerAliases, parentQuery)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD TryCreateCorrelatedInPlan(inExpression AS SqlInSubqueryExpressionContext, ;
        outerTableAliases AS Dictionary<STRING,STRING>, outerTableName AS STRING) AS SqlCorrelatedInPlan
        VAR subquery := inExpression:Subquery
        IF !HasExternalSubqueryReference(subquery) .OR. ;
           HasUnsupportedSubqueryCorrelation(subquery, outerTableAliases) .OR. ;
           subquery:SelectList:Count != 1 .OR. subquery:TableList:Count != 1 .OR. ;
           subquery:Joins:Count != 0 .OR. subquery:GroupByItems:Count != 0 .OR. ;
           subquery:HavingClause != NULL .OR. subquery:OrderByClause:Count != 0 .OR. ;
           subquery:UnionBranches:Count != 0 .OR. subquery:IsDistinct .OR. ;
           !String.IsNullOrEmpty(subquery:TopCount)
            RETURN NULL
        ENDIF

        IF !(inExpression:ValueExpression IS SqlNameExpressionContext VAR outerValue) .OR. ;
           !(subquery:SelectList[0] IS SqlNameExpressionContext VAR innerValue) .OR. ;
           String.IsNullOrEmpty(outerValue:Table) .OR. String.IsNullOrEmpty(innerValue:Table)
            RETURN NULL
        ENDIF

        LOCAL resolvedOuterTable := outerValue:Table AS STRING
        outerTableAliases:TryGetValue(outerValue:Table, OUT resolvedOuterTable)
        IF !resolvedOuterTable:Equals(outerTableName, StringComparison.OrdinalIgnoreCase)
            RETURN NULL
        ENDIF
        LOCAL resolvedInnerTable := innerValue:Table AS STRING
        subquery:TableAliases:TryGetValue(innerValue:Table, OUT resolvedInnerTable)
        IF !resolvedInnerTable:Equals((STRING)subquery:TableList[0], StringComparison.OrdinalIgnoreCase)
            RETURN NULL
        ENDIF

        VAR outerType := HashJoinFieldType(resolvedOuterTable, outerValue:Name)
        VAR innerType := HashJoinFieldType(resolvedInnerTable, innerValue:Name)
        IF !IsHashJoinFieldType(outerType) .OR. !outerType:Equals(innerType, StringComparison.OrdinalIgnoreCase)
            RETURN NULL
        ENDIF

        LOCAL correlationCounter := SqlCorrelationCounter{} AS SqlCorrelationCounter
        VAR innerOnlyWhere := RemoveCorrelatedEquality( ;
            subquery:WhereClause, innerValue, outerValue, correlationCounter)
        IF correlationCounter:Count != 1 .OR. ;
           (innerOnlyWhere != NULL .AND. ;
            (HasExternalSubqueryDependency(innerOnlyWhere, subquery:TableAliases) .OR. ;
             ContainsInSubqueryExpression(innerOnlyWhere)))
            RETURN NULL
        ENDIF

        VAR outerValueCodeBlock := MCompile(outerValue:ToResolvedString(outerTableAliases))
        IF outerValueCodeBlock == NULL
            RETURN NULL
        ENDIF

        LOCAL innerAlias := "SQ" + Guid.NewGuid():ToString("N"):Substring(0, 8) AS STRING
        VAR innerSelect := CopySelectBranch(subquery, innerAlias)
        innerSelect:WhereClause := innerOnlyWhere
        LOCAL savedArea := VoDbGetSelect() AS DWORD
        VAR innerValues := Dictionary<USUAL,BYTE>{}
        TRY
            ExecuteSelect(innerSelect)
            IF !Used(innerAlias)
                RETURN NULL
            ENDIF
            (innerAlias)->DbGoTop()
            DO WHILE !(innerAlias)->Eof()
                VAR value := (USUAL)(innerAlias)->FieldGet(1)
                IF !value:IsNull .AND. !innerValues:ContainsKey(value)
                    innerValues[value] := 0
                ENDIF
                (innerAlias)->DbSkip(1)
            ENDDO
        FINALLY
            IF Used(innerAlias)
                (innerAlias)->DbCloseArea()
            ENDIF
            IF savedArea > 0
                DbSelectArea(savedArea)
            ENDIF
        END TRY
        System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.CorrelatedInPlans)
        RETURN SqlCorrelatedInPlan{outerValueCodeBlock, innerValues, inExpression:IsNegated}

    PRIVATE STATIC METHOD RemoveCorrelatedEquality(expression AS SqlExpressionContext, ;
        innerField AS SqlNameExpressionContext, outerField AS SqlNameExpressionContext, ;
        counter AS SqlCorrelationCounter) AS SqlExpressionContext
        RETURN RemoveCorrelatedEqualityCore(expression, innerField, outerField, counter)

    PRIVATE STATIC METHOD RemoveCorrelatedEqualityCore(expression AS SqlExpressionContext, ;
        innerField AS SqlNameExpressionContext, outerField AS SqlNameExpressionContext, ;
        counter AS SqlCorrelationCounter) AS SqlExpressionContext
        IF expression IS SqlParenExpressionContext VAR parenExpression
            LOCAL oldCount := counter:Count AS INT
            VAR innerExpression := RemoveCorrelatedEqualityCore( ;
                parenExpression:Expr, innerField, outerField, counter)
            IF oldCount == counter:Count
                RETURN expression
            ENDIF
            IF innerExpression == NULL
                RETURN NULL
            ENDIF
            RETURN SqlParenExpressionContext{} { ;
                Open := parenExpression:Open, Expr := innerExpression, Close := parenExpression:Close }
        ENDIF

        IF expression IS SqlLogicExpressionContext VAR logicExpression .AND. ;
           logicExpression:Op:Type == XTokenType.AND
            LOCAL oldCount := counter:Count AS INT
            VAR leftExpression := RemoveCorrelatedEqualityCore( ;
                logicExpression:Left, innerField, outerField, counter)
            VAR rightExpression := RemoveCorrelatedEqualityCore( ;
                logicExpression:Right, innerField, outerField, counter)
            IF oldCount == counter:Count
                RETURN expression
            ENDIF
            IF leftExpression == NULL
                RETURN rightExpression
            ELSEIF rightExpression == NULL
                RETURN leftExpression
            ENDIF
            RETURN SqlLogicExpressionContext{} { ;
                Left := leftExpression, Op := logicExpression:Op, Right := rightExpression }
        ENDIF

        IF expression IS SqlCompareExpressionContext VAR compareExpression .AND. ;
           compareExpression:Op:Type == XTokenType.EQ
            IF (IsSameSqlField(compareExpression:Left, innerField) .AND. ;
                IsSameSqlField(compareExpression:Right, outerField)) .OR. ;
               (IsSameSqlField(compareExpression:Right, innerField) .AND. ;
                IsSameSqlField(compareExpression:Left, outerField))
                counter:Count++
                RETURN NULL
            ENDIF
        ENDIF
        RETURN expression

    PRIVATE STATIC METHOD IsSameSqlField(expression AS SqlExpressionContext, field AS SqlNameExpressionContext) AS LOGIC
        IF expression IS SqlNameExpressionContext VAR nameExpression
            RETURN String.Equals(nameExpression:Name, field:Name, StringComparison.OrdinalIgnoreCase) .AND. ;
                   String.Equals(nameExpression:Table, field:Table, StringComparison.OrdinalIgnoreCase)
        ENDIF
        RETURN FALSE

    PRIVATE STATIC METHOD ContainsInSubqueryExpression(expression AS SqlExpressionContext) AS LOGIC
        IF expression == NULL
            RETURN FALSE
        ENDIF
        IF expression IS SqlInSubqueryExpressionContext .OR. expression IS SqlQuantifiedSubqueryExpressionContext .OR. ;
           expression IS SqlExistsSubqueryExpressionContext .OR. expression IS SqlScalarSubqueryExpressionContext
            RETURN TRUE
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN ContainsInSubqueryExpression(parenExpression:Expr)
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            RETURN ContainsInSubqueryExpression(prefixExpression:Expr)
        ELSEIF expression IS SqlBinaryExpressionContext VAR binaryExpression
            RETURN ContainsInSubqueryExpression(binaryExpression:Left) .OR. ;
                   ContainsInSubqueryExpression(binaryExpression:Right)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            FOREACH VAR child IN compositeExpression:Exprs
                IF ContainsInSubqueryExpression(child)
                    RETURN TRUE
                ENDIF
            NEXT
        ENDIF
        RETURN FALSE

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

    PRIVATE STATIC METHOD InferScalarProjectionFieldDefinition(expression AS SqlExpressionContext, ;
        aliases AS Dictionary<STRING,STRING>, ;
        derivedTables := NULL AS Dictionary<STRING,FoxSelectContext>) AS ARRAY
        IF expression IS SqlScalarSubqueryExpressionContext VAR scalarExpression
            RETURN scalarExpression:ResultFieldDefinition
        ELSEIF expression IS SqlParenExpressionContext VAR parenExpression
            RETURN InferScalarProjectionFieldDefinition(parenExpression:Expr, aliases, derivedTables)
        ELSEIF expression IS SqlNameExpressionContext VAR nameExpression
            LOCAL derivedQuery AS FoxSelectContext
            IF derivedTables != NULL .AND. !String.IsNullOrEmpty(nameExpression:Table) .AND. ;
               derivedTables:TryGetValue(nameExpression:Table, OUT derivedQuery)
                RETURN InferDerivedTableFieldDefinition(derivedQuery, nameExpression:Name)
            ENDIF
            LOCAL sourceTable := nameExpression:Table AS STRING
            IF String.IsNullOrEmpty(sourceTable)
                FOREACH VAR tableAlias IN aliases
                    VAR candidateTable := tableAlias:Value
                    VAR fieldPosition := (candidateTable)->FieldPos(nameExpression:Name)
                    IF fieldPosition > 0
                        sourceTable := candidateTable
                        EXIT
                    ENDIF
                NEXT
            ELSE
                aliases:TryGetValue(sourceTable, OUT sourceTable)
            ENDIF
            IF !String.IsNullOrEmpty(sourceTable) .AND. Used(sourceTable)
                VAR fieldPosition := (sourceTable)->FieldPos(nameExpression:Name)
                IF fieldPosition > 0
                    RETURN (sourceTable)->DbStruct()[fieldPosition]
                ENDIF
            ENDIF
            RETURN NULL
        ELSEIF expression IS SqlSimpleExpressionContext VAR simpleExpression
            IF simpleExpression:Tokens == NULL .OR. simpleExpression:Tokens:Count == 0
                RETURN NULL
            ENDIF
            LOCAL literalType := NULL AS STRING
            LOCAL literalCount := 0 AS INT
            LOCAL nonPlusOperator := FALSE AS LOGIC
            FOREACH VAR token IN simpleExpression:Tokens
                SWITCH token:Type
                CASE XTokenType.PLUS
                CASE XTokenType.MINUS
                CASE XTokenType.MULT
                CASE XTokenType.DIV
                CASE XTokenType.EXP
                    IF token:Type != XTokenType.PLUS
                        nonPlusOperator := TRUE
                    ENDIF
                    LOOP
                CASE XTokenType.NUMERIC_CONST
                    IF literalType != NULL .AND. literalType != "N"
                        RETURN NULL
                    ENDIF
                    literalType := "N"
                    literalCount++
                CASE XTokenType.STRING_CONST
                    IF literalType != NULL .AND. literalType != "C"
                        RETURN NULL
                    ENDIF
                    literalType := "C"
                    literalCount++
                CASE XTokenType.TRUE_CONST
                CASE XTokenType.FALSE_CONST
                    IF literalType != NULL .AND. literalType != "L"
                        RETURN NULL
                    ENDIF
                    literalType := "L"
                    literalCount++
                CASE XTokenType.DATE_CONST
                    IF literalCount > 0
                        RETURN NULL
                    ENDIF
                    literalType := "D"
                    literalCount++
                CASE XTokenType.DATETIME_CONST
                    IF literalCount > 0
                        RETURN NULL
                    ENDIF
                    literalType := "T"
                    literalCount++
                OTHERWISE
                    RETURN NULL
                END SWITCH
            NEXT
            IF literalCount == 0 .OR. (literalType == "C" .AND. nonPlusOperator) .OR. ;
               ((literalType == "D" .OR. literalType == "T" .OR. literalType == "L") .AND. literalCount > 1)
                RETURN NULL
            ENDIF
            SWITCH literalType
            CASE "N"
                RETURN {NULL, "N", 20, 4, NULL, (LONG)DBFFieldFlags.Nullable}
            CASE "C"
                RETURN {NULL, "C", Math.Min(254, 254 * literalCount), 0, NULL, ;
                    (LONG)DBFFieldFlags.Nullable}
            CASE "L"
                RETURN {NULL, "L", 1, 0, NULL, (LONG)DBFFieldFlags.Nullable}
            CASE "D"
                RETURN {NULL, "D", 8, 0, NULL, (LONG)DBFFieldFlags.Nullable}
            CASE "T"
                RETURN {NULL, "T", 8, 0, NULL, (LONG)DBFFieldFlags.Nullable}
            END SWITCH
            RETURN NULL
        ELSEIF expression IS SqlAggregateExpressionContext VAR aggregateExpression
            IF aggregateExpression:FunctionName == "COUNT" .OR. aggregateExpression:FunctionName == "CNT"
                RETURN {NULL, "N", 10, 0, NULL, (LONG)DBFFieldFlags.Nullable}
            ELSEIF aggregateExpression:FunctionName == "MIN" .OR. aggregateExpression:FunctionName == "MAX"
                IF aggregateExpression:Argument IS SqlNameExpressionContext
                    RETURN InferScalarProjectionFieldDefinition(aggregateExpression:Argument, aliases, derivedTables)
                ENDIF
            ELSE
                RETURN {NULL, "N", 20, 4, NULL, (LONG)DBFFieldFlags.Nullable}
            ENDIF
            RETURN NULL
        ELSEIF expression IS SqlCompareExpressionContext .OR. expression IS SqlLogicExpressionContext
            RETURN {NULL, "L", 1, 0, NULL, (LONG)DBFFieldFlags.Nullable}
        ELSEIF expression IS SqlPrefixExpressionContext VAR prefixExpression
            IF prefixExpression:Op:Type == XTokenType.NOT
                RETURN {NULL, "L", 1, 0, NULL, (LONG)DBFFieldFlags.Nullable}
            ENDIF
            RETURN InferScalarProjectionFieldDefinition(prefixExpression:Expr, aliases, derivedTables)
        ELSEIF expression IS SqlCompositeExpressionContext VAR compositeExpression
            LOCAL numericOnly := TRUE AS LOGIC
            LOCAL characterOnly := TRUE AS LOGIC
            LOCAL sawOperand := FALSE AS LOGIC
            LOCAL characterLength := 0 AS INT
            FOREACH VAR item IN compositeExpression:Exprs
                IF item IS SqlSimpleExpressionContext VAR operatorExpression .AND. ;
                   operatorExpression:Tokens != NULL .AND. operatorExpression:Tokens:Count == 1
                    VAR operatorTokenType := operatorExpression:Tokens[0]:Type
                    IF operatorTokenType == XTokenType.PLUS .OR. operatorTokenType == XTokenType.MINUS .OR. ;
                       operatorTokenType == XTokenType.MULT .OR. operatorTokenType == XTokenType.DIV .OR. ;
                       operatorTokenType == XTokenType.EXP
                        LOOP
                    ENDIF
                ENDIF
                VAR operandDefinition := InferScalarProjectionFieldDefinition(item, aliases, derivedTables)
                IF operandDefinition == NULL
                    RETURN NULL
                ENDIF
                sawOperand := TRUE
                VAR operandType := ((STRING)operandDefinition[2]):ToUpperInvariant()
                VAR baseType := operandType:Substring(0, 1)
                IF baseType == "N" .OR. baseType == "I" .OR. baseType == "Y" .OR. ;
                   baseType == "F" .OR. baseType == "B"
                    characterOnly := FALSE
                ELSEIF baseType == "C" .OR. baseType == "V" .OR. baseType == "M"
                    numericOnly := FALSE
                    characterLength += (INT)operandDefinition[3]
                ELSE
                    numericOnly := FALSE
                    characterOnly := FALSE
                ENDIF
            NEXT
            IF !sawOperand
                RETURN NULL
            ELSEIF numericOnly
                RETURN {NULL, "N", 20, 4, NULL, (LONG)DBFFieldFlags.Nullable}
            ELSEIF characterOnly
                RETURN {NULL, "C", Math.Min(254, Math.Max(1, characterLength)), 0, NULL, (LONG)DBFFieldFlags.Nullable}
            ENDIF
            RETURN NULL
        ENDIF
        RETURN NULL

    PRIVATE STATIC METHOD InferDerivedTableFieldDefinition(query AS FoxSelectContext, fieldName AS STRING) AS ARRAY
        IF query == NULL .OR. query:SelectList == NULL .OR. query:SelectList:Count == 0 .OR. ;
           (query:SelectList:Count == 1 .AND. query:SelectList[0] IS SqlSimpleExpressionContext VAR star .AND. ;
            star:Tokens != NULL .AND. star:Tokens:Count == 1 .AND. star:Tokens[0]:Type == XTokenType.MULT)
            RETURN NULL
        ENDIF
        LOCAL existingNames := List<STRING>{} AS List<STRING>
        FOR LOCAL i := 0 AS INT TO query:SelectList:Count - 1
            LOCAL outputName := NULL AS STRING
            IF query:FieldSelectionList != NULL .AND. i < query:FieldSelectionList:Count .AND. ;
               !String.IsNullOrWhiteSpace(query:FieldSelectionList[i]:Alias)
                outputName := query:FieldSelectionList[i]:Alias
            ELSE
                outputName := SqlExpressionToFieldName(query:SelectList[i])
            ENDIF
            outputName := ResolveConflictingFieldName(outputName, existingNames)
            existingNames:Add(outputName:ToUpperInvariant())
            IF outputName:Equals(fieldName, StringComparison.OrdinalIgnoreCase)
                RETURN InferScalarProjectionFieldDefinition(query:SelectList[i], ;
                    query:TableAliases, query:DerivedTables)
            ENDIF
        NEXT
        RETURN NULL

    PRIVATE STATIC METHOD InferCorrelatedScalarResultFieldDefinition(query AS FoxSelectContext) AS ARRAY
        IF query:SelectList:Count != 1
            RETURN NULL
        ENDIF
        LOCAL previousArea := VoDbGetSelect() AS DWORD
        LOCAL openedTables := List<STRING>{} AS List<STRING>
        TRY
            IF !OpenScalarInferenceTables(query, openedTables)
                RETURN NULL
            ENDIF
            RETURN InferScalarProjectionFieldDefinition(query:SelectList[0], ;
                query:TableAliases, query:DerivedTables)
        FINALLY
            FOREACH VAR tableName IN openedTables
                IF Used(tableName)
                    (tableName)->DbCloseArea()
                ENDIF
            NEXT
            IF previousArea > 0
                DbSelectArea(previousArea)
            ENDIF
        END TRY

    PRIVATE STATIC METHOD OpenScalarInferenceTables(query AS FoxSelectContext, ;
        openedTables AS IList<STRING>) AS LOGIC
        IF query == NULL
            RETURN TRUE
        ENDIF
        FOREACH VAR tableName IN query:TableList
            LOCAL derivedQuery AS FoxSelectContext
            IF query:DerivedTables != NULL .AND. query:DerivedTables:TryGetValue(tableName, OUT derivedQuery)
                IF !OpenScalarInferenceTables(derivedQuery, openedTables)
                    RETURN FALSE
                ENDIF
            ELSEIF !Used(tableName)
                IF !FoxEmbeddedSQL.OpenArea(tableName)
                    RETURN FALSE
                ENDIF
                openedTables:Add(tableName)
            ENDIF
        NEXT
        RETURN TRUE

    STATIC METHOD ExecuteSelect(selectCtx AS FoxSelectContext) AS VOID
        LOCAL savedArea := VoDbGetSelect() AS DWORD
        LOCAL target := IIF(String.IsNullOrEmpty(selectCtx:TargetCursor), "QUERYRESULT", AllTrim(selectCtx:TargetCursor)) AS STRING
        LOCAL targetAlreadyOpen := Used(target) AS LOGIC
        LOCAL derivedExecutions := List<SqlDerivedTableExecution>{} AS List<SqlDerivedTableExecution>
        TRY
            ValidateSelectSubqueries(selectCtx)
            IF selectCtx:DerivedTables:Count > 0
                IF selectCtx:TableList:Count != selectCtx:Joins:Count + 1
                    THROW Error{"Derived tables require a valid ordered FROM source list"}
                ENDIF
                FOR LOCAL tableIndex := 0 AS INT TO selectCtx:TableList:Count - 1
                    VAR sourceAlias := (STRING)selectCtx:TableList[tableIndex]
                    IF selectCtx:DerivedTables:TryGetValue(sourceAlias, OUT VAR derivedQuery)
                        VAR execution := SqlDerivedTableExecution{}
                        execution:SourceIndex := tableIndex
                        execution:Alias := sourceAlias
                        execution:CursorName := "DT" + Guid.NewGuid():ToString("N"):Substring(0, 8)
                        execution:OriginalTableName := sourceAlias
                        execution:OriginalAliasTarget := selectCtx:TableAliases[sourceAlias]
                        IF tableIndex > 0
                            execution:OriginalJoinTableName := selectCtx:Joins[tableIndex - 1]:TableName
                        ENDIF
                        derivedExecutions:Add(execution)

                        VAR previousDerivedTarget := derivedQuery:TargetCursor
                        TRY
                            derivedQuery:TargetCursor := execution:CursorName
                            System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.DerivedTableExecutions)
                            ExecuteSelect(derivedQuery)
                        FINALLY
                            derivedQuery:TargetCursor := previousDerivedTarget
                        END TRY
                        IF !Used(execution:CursorName)
                            THROW Error{"Derived table subquery did not produce a result cursor"}
                        ENDIF
                        selectCtx:TableList[tableIndex] := execution:CursorName
                        selectCtx:TableAliases[sourceAlias] := execution:CursorName
                        IF tableIndex > 0
                            selectCtx:Joins[tableIndex - 1]:TableName := execution:CursorName
                        ENDIF
                    ENDIF
                NEXT
                IF derivedExecutions:Count != selectCtx:DerivedTables:Count
                    THROW Error{"A derived table alias was not found in the FROM source list"}
                ENDIF
            ENDIF
            ValidateSelectSubqueries(selectCtx)
            ExecuteSelectCore(selectCtx)
        CATCH
            IF !targetAlreadyOpen .AND. Used(target)
                (target)->DbCloseArea()
            ENDIF
            IF savedArea > 0
                DbSelectArea(savedArea)
            ENDIF
            THROW
        FINALLY
            FOR LOCAL executionIndex := derivedExecutions:Count - 1 AS INT DOWNTO 0
                VAR execution := derivedExecutions[executionIndex]
                selectCtx:TableList[execution:SourceIndex] := execution:OriginalTableName
                selectCtx:TableAliases[execution:Alias] := execution:OriginalAliasTarget
                IF execution:SourceIndex > 0
                    selectCtx:Joins[execution:SourceIndex - 1]:TableName := execution:OriginalJoinTableName
                ENDIF
                IF Used(execution:CursorName)
                    (execution:CursorName)->DbCloseArea()
                ENDIF
            NEXT
        END TRY
        RETURN

    PRIVATE STATIC METHOD ExecuteSelectCore(selectCtx as FoxSelectContext) AS VOID
        // Implementation of SELECT execution
        // This will handle opening tables, selecting records, and returning results

        ValidateSelectSubqueries(selectCtx)

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
            IF EmbeddedSqlRuntimeOptions.OutOfScopeJoinWorkAreaCompatibilityEnabled()
                selectCtx:WorkAreaRecordSnapshot := CaptureWorkAreaRecordNumbers(selectCtx)
            ELSE
                selectCtx:WorkAreaRecordSnapshot := NULL
            ENDIF
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
            IF expr IS SqlSimpleExpressionContext VAR simpleSelection .AND. ;
               simpleSelection:Tokens != NULL .AND. simpleSelection:Tokens:Count == 1 .AND. ;
               simpleSelection:Tokens[0]:Type == XTokenType.MULT
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

        // Prepare uncorrelated projection subqueries once. Correlated values are
        // deferred until their current outer row or group is positioned.
        LOCAL scalarMemos := Dictionary<SqlScalarSubqueryExpressionContext,SqlCorrelatedScalarMemo>{} ;
            AS Dictionary<SqlScalarSubqueryExpressionContext,SqlCorrelatedScalarMemo>
        FOREACH VAR projectionExpression IN selectCtx:SelectList
            PrepareSubqueryExpression(projectionExpression, selectCtx:TableAliases, TRUE, FALSE, scalarMemos)
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

                IF !fieldFound .AND. ContainsScalarSubquery(expr)
                    fieldInfo := InferScalarProjectionFieldDefinition(expr, selectCtx:TableAliases, ;
                        selectCtx:DerivedTables)
                    IF fieldInfo == NULL
                        THROW Error{"Could not infer the result type of SELECT expression containing a scalar subquery: " + resolvedExprStr}
                    ENDIF
                    fieldFound := TRUE
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
                        IF ContainsScalarSubquery(expr)
                            outputFlags := outputFlags | (LONG)DBFFieldFlags.Nullable
                        ENDIF
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
        LOCAL hasCorrelatedWhere := FALSE AS LOGIC
        LOCAL threeValueWhere := NULL AS SqlThreeValuePredicate
        LOCAL correlatedInPlan AS SqlCorrelatedInPlan
        correlatedInPlan := NULL
        hasWhereClause := FALSE
        IF selectCtx:WhereClause != NULL
            IF SqlOptimizationsEnabled() .AND. tableNames:Count == 1 .AND. selectCtx:Joins:Count == 0 .AND. ;
               selectCtx:WhereClause IS SqlInSubqueryExpressionContext VAR correlatedInExpression
                correlatedInPlan := TryCreateCorrelatedInPlan( ;
                    correlatedInExpression, selectCtx:TableAliases, (STRING)tableNames[1])
            ENDIF
            IF correlatedInPlan == NULL
                hasCorrelatedWhere := HasCorrelatedInSubquery(selectCtx:WhereClause, selectCtx:TableAliases)
                PrepareSubqueryExpression(selectCtx:WhereClause, selectCtx:TableAliases, hasCorrelatedWhere)
                IF ContainsScalarSubquery(selectCtx:WhereClause)
                    threeValueWhere := BuildThreeValuePredicate(selectCtx:WhereClause, selectCtx:TableAliases)
                ELSE
                    whereClause := selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
                    whereCodeBlock := MCompile(whereClause)
                ENDIF
            ELSE
                // The semi-join plan evaluates the root predicate directly and
                // does not need a generated INLIST expression.
                whereClause := ""
            ENDIF
            hasWhereClause := TRUE
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
                            System.Threading.Interlocked.Increment(REF SqlOptimizationDiagnostics.HashJoins)
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
        LOCAL useMembershipCandidates := FALSE AS LOGIC
        useOptimization := FALSE
        multiTableCandidates := NULL
        multiTableCandidateRows := NULL

        IF SqlOptimizationsEnabled() .AND. hasWhereClause .AND. correlatedInPlan == NULL .AND. ;
           !HasUnsafeCorrelatedCandidate(selectCtx:WhereClause, selectCtx:TableAliases) .AND. ;
           IsSafeMembershipRemainder(selectCtx:WhereClause, selectCtx:TableAliases, ContainsScalarSubquery(selectCtx:WhereClause)) .AND. ;
           tableNames:Count == 1 .AND. selectCtx:Joins:Count == 0
            VAR membershipCandidate := FindMembershipCandidate(selectCtx:WhereClause, selectCtx:TableAliases, (STRING)tableNames[1])
            IF membershipCandidate != NULL
                VAR candidateRows := BuildMembershipCandidates(membershipCandidate, selectCtx:TableAliases, (STRING)tableNames[1])
                IF candidateRows != NULL
                    multiTableCandidateRows := Dictionary<STRING,List<LONG>>{StringComparer.OrdinalIgnoreCase}
                    multiTableCandidateRows[(STRING)tableNames[1]] := candidateRows
                    useMembershipCandidates := TRUE
                ENDIF
            ENDIF
        ENDIF

        IF SqlOptimizationsEnabled() .AND. hasWhereClause .AND. !hasCorrelatedWhere .AND. threeValueWhere == NULL .AND. correlatedInPlan == NULL .AND. ;
           multiTableCandidateRows == NULL .AND. ;
           tableNames:Count == 1
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
        ELSEIF SqlOptimizationsEnabled() .AND. hasWhereClause .AND. !hasCorrelatedWhere .AND. threeValueWhere == NULL .AND. tableNames:Count > 1 .AND. ;
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
                    ELSEIF useMembershipCandidates
                        (tableNames[i])->DbGoTop()
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

            IF includeRecord .AND. hasWhereClause .AND. hasCorrelatedWhere .AND. correlatedInPlan == NULL
                // A correlated subquery depends on the current outer row. Rebuild its
                // value set and the WHERE codeblock for each row combination.
                PrepareSubqueryExpression(selectCtx:WhereClause, selectCtx:TableAliases, FALSE, TRUE)
                IF threeValueWhere == NULL
                    whereClause := selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
                    whereCodeBlock := MCompile(whereClause)
                    IF whereCodeBlock == NULL
                        THROW Error{"Could not compile correlated WHERE expression: " + whereClause}
                    ENDIF
                ENDIF
            ENDIF

            IF includeRecord .AND. hasWhereClause
                // Use optimized bitmap when available, otherwise evaluate codeblock
                IF correlatedInPlan != NULL
                    includeRecord := correlatedInPlan:Matches()
                ELSEIF threeValueWhere != NULL
                    includeRecord := threeValueWhere:Evaluate() == 1
                ELSEIF useOptimization .AND. ALen(tableNames) == 1 .AND. recNo > 0
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
                    IF group:InputCount == 0
                        group:SourceRecordNumbers := LONG[]{ALen(tableNames)}
                        FOR LOCAL sourceIndex := 1 AS INT TO ALen(tableNames)
                            group:SourceRecordNumbers[sourceIndex] := (LONG)(tableNames[sourceIndex])->RecNo()
                        NEXT
                    ENDIF
                    UpdateGroup(group, aggregates, aggregateCodeBlocks)
                ELSE
                // Get values from current record combination
                LOCAL sourceValues AS USUAL[]
                sourceValues := USUAL[]{ALen(selectFieldNames)}
                FOR LOCAL i := 1 AS DWORD TO ALen(selectFieldNames)
                    IF selectCtx:SelectList:Count == ALen(selectFieldNames) .AND. ;
                       HasCorrelatedProjectionSubquery(selectCtx:SelectList[(INT)i - 1])
                        PrepareSubqueryExpression(selectCtx:SelectList[(INT)i - 1], ;
                            selectCtx:TableAliases, FALSE, TRUE, scalarMemos)
                        selectExpressions[i] := selectCtx:SelectList[(INT)i - 1]:ToResolvedString(selectCtx:TableAliases)
                    ENDIF
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
                            IF useMembershipCandidates .AND. ALen(tableNames) == 1
                                (tableNames[positionIdx])->DbGoTop()
                            ELSE
                                DbGoto(candidateRecNos[0], tableNames[positionIdx])
                            ENDIF
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
                    IF HasCorrelatedProjectionSubquery(selectCtx:SelectList[selectIndex])
                        PositionAggregateGroupSources(selectCtx, group)
                        PrepareSubqueryExpression(selectCtx:SelectList[selectIndex], ;
                            selectCtx:TableAliases, FALSE, TRUE, scalarMemos)
                    ENDIF
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
            THROW Error{"LEFT JOIN requires an ordered FROM source list"}
        ENDIF

        VAR execution := CopySelectBranch(source, source:TargetCursor)
        execution:OuterJoinRows := NULL
        execution:WorkAreaRecordSnapshot := source:WorkAreaRecordSnapshot
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
            executionJoin:IsCommaSource := sourceJoin:IsCommaSource

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
            (firstTable)->DbGoto(row[1])
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
                    RestoreOutOfScopeJoinSources(selectCtx, tableIndex - 2)
                    matches := (LOGIC)joinCodeBlocks[tableIndex - 2]:Eval()
                CATCH ex AS Exception
                    matches := FALSE
                END TRY
            ENDIF
            IF matches
                foundMatch := TRUE
                AppendLeftJoinRows(selectCtx, tableNames, joinCodeBlocks, tableIndex + 1, row, rows)
            ENDIF
            (tableName)->DbGoto(candidateRecordNumber)
            (tableName)->DbSkip(1)
        ENDDO

        IF !foundMatch .AND. joinContext:Kind == FoxSelectJoinKind.Left
            row[tableIndex] := joinContext:NullRecordNumber
            AppendLeftJoinRows(selectCtx, tableNames, joinCodeBlocks, tableIndex + 1, row, rows)
        ENDIF
        RETURN

    PRIVATE STATIC METHOD CaptureWorkAreaRecordNumbers(selectCtx AS FoxSelectContext) AS Dictionary<STRING,LONG>
        VAR result := Dictionary<STRING,LONG>{StringComparer.OrdinalIgnoreCase}
        VAR previousArea := DbSelect()
        FOREACH VAR sourceName IN selectCtx:TableList
            IF Used(sourceName) .AND. !result:ContainsKey(sourceName)
                result[sourceName] := (LONG)(sourceName)->RecNo()
            ENDIF
        NEXT
        IF previousArea > 0
            DbSelectArea(previousArea)
        ENDIF
        RETURN result

    PRIVATE STATIC METHOD RestoreOutOfScopeJoinSources(selectCtx AS FoxSelectContext, joinIndex AS INT) AS VOID
        IF !EmbeddedSqlRuntimeOptions.OutOfScopeJoinWorkAreaCompatibilityEnabled() .OR. ;
           selectCtx:WorkAreaRecordSnapshot == NULL .OR. joinIndex < 0 .OR. joinIndex >= selectCtx:Joins:Count
            RETURN
        ENDIF
        // A comma starts a new table expression. Earlier comma groups are outside
        // the operands of joins in the current group and VFP reads their current row.
        LOCAL scopeStart := 0 AS INT
        FOR LOCAL previousJoin := joinIndex - 1 AS INT DOWNTO 0
            IF selectCtx:Joins[previousJoin]:IsCommaSource
                scopeStart := previousJoin + 1
                EXIT
            ENDIF
        NEXT
        IF scopeStart == 0
            RETURN
        ENDIF
        VAR dependencies := selectCtx:Joins[joinIndex]:OnClause:GetTableDependencies(selectCtx:TableAliases)
        FOREACH VAR dependency IN dependencies
            LOCAL sourceIndex := -1 AS INT
            FOR LOCAL candidateIndex := 0 AS INT TO selectCtx:TableList:Count - 1
                IF String.Equals((STRING)selectCtx:TableList[candidateIndex], dependency, StringComparison.OrdinalIgnoreCase)
                    sourceIndex := candidateIndex
                    EXIT
                ENDIF
            NEXT
            IF sourceIndex >= 0 .AND. sourceIndex < scopeStart
                LOCAL recordNumber AS LONG
                IF selectCtx:WorkAreaRecordSnapshot:TryGetValue(dependency, OUT recordNumber) .AND. recordNumber > 0
                    DbGoto(recordNumber, dependency)
                ENDIF
            ENDIF
        NEXT
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
            ELSEIF fieldList[i][2] == "N" .AND. UsualType(sourceValues[i]) == __UsualType.String .AND. ;
                   Double.TryParse(sourceValues[i]:ToString():Trim(), OUT VAR numericValue)
                (resultTable)->FieldPut(i, numericValue)
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
