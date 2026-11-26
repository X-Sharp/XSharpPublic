// SQLParser.prg
// Created by    : nikos
// Creation Date : 11/15/2025 11:02:32 PM
// Created for   :
// WorkStation   : DESKTOP-TJFSDLK


USING System
USING System.Linq
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.Parsers
PARTIAL CLASS SQLParser

    PRIVATE METHOD MatchingParen() AS XTokenType
        SWITCH SELF:La1
        CASE XTokenType.LT
            RETURN XTokenType.GT
        CASE XTokenType.LPAREN
            RETURN XTokenType.RPAREN
        CASE XTokenType.LBRKT
            RETURN XTokenType.RBRKT
        CASE XTokenType.LCURLY
            RETURN XTokenType.RCURLY
        CASE XTokenType.GT
            RETURN XTokenType.LT
        CASE XTokenType.RPAREN
            RETURN XTokenType.LPAREN
        CASE XTokenType.RBRKT
            RETURN XTokenType.LBRKT
        CASE XTokenType.RCURLY
            RETURN XTokenType.LCURLY
        OTHERWISE
            THROW ArgumentException{i"Invalid parenthesis: {SELF:La1}","SQLParser"}
        END SWITCH

    PRIVATE METHOD Eoe() AS LOGIC
        SWITCH SELF:La1
        CASE XTokenType.AS
        CASE XTokenType.IS
        CASE XTokenType.COMMA
        CASE XTokenType.CHECK
        CASE XTokenType.DEFAULT
        CASE XTokenType.AUTOINC
        CASE XTokenType.PRIMARY
        CASE XTokenType.UNIQUE
        CASE XTokenType.ERROR
        CASE XTokenType.FROM
        CASE XTokenType.GROUP
        CASE XTokenType.HAVING
        CASE XTokenType.INTO
        CASE XTokenType.ORDER
        CASE XTokenType.UNION
        CASE XTokenType.JOIN
        CASE XTokenType.IN
        CASE XTokenType.WHERE
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH

    PRIVATE METHOD CompareOp() AS LOGIC
        SWITCH SELF:La1
        CASE XTokenType.LT
        CASE XTokenType.GT
        CASE XTokenType.LTE
        CASE XTokenType.GTE
        CASE XTokenType.EQ
        CASE XTokenType.EEQ
        CASE XTokenType.NEQ
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH

    PRIVATE METHOD PrefixOp() AS LOGIC
        SWITCH SELF:La1
        CASE XTokenType.NOT
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH

    PRIVATE METHOD CombineOp() AS LOGIC
        SWITCH SELF:La1
        CASE XTokenType.OR
        CASE XTokenType.AND
            RETURN TRUE
        OTHERWISE
            RETURN FALSE
        END SWITCH

    METHOD ParseExpressionContext() AS SqlExpressionContext
        RETURN SELF:ParseCombineExpression()

    PRIVATE METHOD ParseCombineExpression() AS SqlExpressionContext
        VAR e := SELF:ParseAndExpression()
        WHILE SELF:Matches(XTokenType.OR)
            VAR o := SELF:ConsumeAndGet()
            VAR r := SELF:ParseAndExpression()
            e := SqlLogicExpressionContext{} { Left := e, Op := o, Right := r }
        ENDDO
        IF ! SELF:Eos() .AND. ( SELF:CompareOp() .OR. SELF:PrefixOp() .OR. SELF:CombineOp() )
            THROW ArgumentException{i"Unexpected operator: {SELF:La1}","SQLParser"}
        ENDIF
        RETURN e

    PRIVATE METHOD ParseAndExpression() AS SqlExpressionContext
        VAR e := SELF:ParsePrefixExpression()
        WHILE SELF:Matches(XTokenType.AND)
            VAR o := SELF:ConsumeAndGet()
            VAR r := SELF:ParsePrefixExpression()
            e := SqlLogicExpressionContext{} { Left := e, Op := o, Right := r }
        ENDDO
        RETURN e

    PRIVATE METHOD ParsePrefixExpression() AS SqlExpressionContext
        LOCAL e := NULL AS SqlExpressionContext
        IF SELF:Eos() .OR. SELF:La1 == XTokenType.RPAREN .OR. SELF:CombineOp()
            THROW ArgumentException{"Expected expression","SQLParser"}
        ENDIF
        IF SELF:PrefixOp()
            VAR o := SELF:ConsumeAndGet()
            VAR c := SELF:ParseCompareExpression()
            IF o:Type == XTokenType.NOT .AND. c IS SqlInSubqueryExpressionContext VAR inSubquery
                inSubquery:IsNegated := !inSubquery:IsNegated
                e := inSubquery
            ELSEIF o:Type == XTokenType.NOT .AND. c IS SqlInListExpressionContext VAR inList
                inList:IsNegated := !inList:IsNegated
                e := inList
            ELSE
                e := SqlPrefixExpressionContext{} { Op := o, Expr := c }
            ENDIF
        ELSE
            e := SELF:ParseCompareExpression()
        ENDIF
        RETURN e

    // The opening parenthesis has already been consumed by the caller.
    PRIVATE METHOD ParseSubquery(predicateName AS STRING) AS FoxSelectContext
        IF !SELF:Matches(XTokenType.SELECT)
            THROW ArgumentException{"Expected SELECT in " + predicateName + " subquery", "SQLParser"}
        ENDIF
        VAR subqueryTokens := List<XToken>{}
        LOCAL depth := 1 AS LONG
        DO WHILE !SELF:Eoi()
            IF SELF:La1 == XTokenType.LPAREN
                depth++
            ELSEIF SELF:La1 == XTokenType.RPAREN
                depth--
                IF depth == 0
                    EXIT
                ENDIF
            ENDIF
            subqueryTokens:Add(SELF:ConsumeAndGet())
        ENDDO
        IF depth != 0 .OR. !SELF:Expect(XTokenType.RPAREN)
            THROW ArgumentException{"Unclosed " + predicateName + " subquery", "SQLParser"}
        ENDIF
        VAR subParser := SQLParser{XTokenList{subqueryTokens}}
        LOCAL subquery AS FoxSelectContext
        IF !subParser:ParseSelectStatement(OUT subquery) .OR. !subParser:Eoi()
            THROW ArgumentException{"Invalid " + predicateName + " subquery: " + subParser:Error, "SQLParser"}
        ENDIF
        RETURN subquery

    PRIVATE METHOD ParseCompareExpression() AS SqlExpressionContext
        IF SELF:Expect(XTokenType.EXISTS)
            IF !SELF:Expect(XTokenType.LPAREN)
                THROW ArgumentException{"Expected opening parenthesis after EXISTS", "SQLParser"}
            ENDIF
            RETURN SqlExistsSubqueryExpressionContext{} { Subquery := SELF:ParseSubquery("EXISTS") }
        ENDIF
        VAR e := SELF:ParseExpressionTerm()
        LOCAL isNegated := FALSE AS LOGIC
        IF SELF:Matches(XTokenType.NOT) .AND. SELF:La2 == XTokenType.IN
            SELF:Consume()
            isNegated := TRUE
        ENDIF
        IF SELF:Expect(XTokenType.IN)
            IF !SELF:Expect(XTokenType.LPAREN)
                THROW ArgumentException{"Expected opening parenthesis after IN", "SQLParser"}
            ENDIF
            IF !SELF:Matches(XTokenType.SELECT)
                VAR listExpression := SqlInListExpressionContext{} { ValueExpression := e, IsNegated := isNegated }
                DO WHILE TRUE
                    IF SELF:Matches(XTokenType.RPAREN) .OR. SELF:Eos()
                        THROW ArgumentException{"IN list cannot be empty", "SQLParser"}
                    ENDIF
                    listExpression:Items:Add(SELF:ParseExpressionContext())
                    IF SELF:Expect(XTokenType.COMMA)
                        LOOP
                    ENDIF
                    IF !SELF:Expect(XTokenType.RPAREN)
                        THROW ArgumentException{"Expected closing parenthesis for IN list", "SQLParser"}
                    ENDIF
                    EXIT
                ENDDO
                RETURN listExpression
            ENDIF
            RETURN SqlInSubqueryExpressionContext{} { ValueExpression := e, Subquery := SELF:ParseSubquery("IN"), IsNegated := isNegated }
        ELSEIF isNegated
            THROW ArgumentException{"Expected IN after NOT", "SQLParser"}
        ENDIF
        IF SELF:CompareOp()
            VAR o := SELF:ConsumeAndGet()
            LOCAL r AS SqlExpressionContext
            IF SELF:Matches(XTokenType.LPAREN) .AND. SELF:La2 == XTokenType.SELECT
                SELF:Consume()
                r := SqlScalarSubqueryExpressionContext{} { Subquery := SELF:ParseSubquery("scalar") }
            ELSE
                r := SELF:ParseExpressionTerm()
            ENDIF
            IF SELF:CompareOp()
                // At this point we are lost, keep parsing the whole thing as a composite expression
                VAR terms := List<SqlExpressionContext>{} { e, SqlSimpleExpressionContext{} { Tokens := List<XToken>{}{o} }, r}
                VAR names := List<SqlNameExpressionContext>{}
                IF e IS SqlCompositeExpressionContext VAR ce
                    names:AddRange(ce:Names)
                ELSEIF e IS SqlNameExpressionContext VAR ne
                    names:Add(ne)
                ENDIF
                IF r IS SqlCompositeExpressionContext VAR cr
                    names:AddRange(cr:Names)
                ELSEIF r IS SqlNameExpressionContext VAR nr
                    names:Add(nr)
                ENDIF
                WHILE SELF:CompareOp()
                    terms:Add(SqlSimpleExpressionContext{} { Tokens := List<XToken>{}{SELF:ConsumeAndGet()} })
                    VAR a := SELF:ParseExpressionTerm()
                    IF a IS SqlCompositeExpressionContext VAR ca
                        names:AddRange(ca:Names)
                    ELSEIF r IS SqlNameExpressionContext VAR na
                        names:Add(na)
                    ENDIF
                    terms:Add(a)
                    e := SqlCompositeExpressionContext{} { Exprs := terms, Names := names }
                END WHILE
            ELSE
                e := SqlCompareExpressionContext{} { Left := e, Op := o, Right := r }
            ENDIF
        ENDIF
        RETURN e

    PRIVATE METHOD ParseExpressionTerm() AS SqlExpressionContext
        VAR done := FALSE
        VAR hasComplexTerms := FALSE
        VAR hasAggregateTerm := FALSE
        VAR terms := List<SqlExpressionContext>{}
        VAR names := List<SqlNameExpressionContext>{}
        VAR parens := Stack<XTokenType>{}
        VAR tokens := List<XToken>{}

        LOCAL FUNCTION AddPartialTerm() AS VOID
            IF tokens:Count > 0
                terms:Add( SqlSimpleExpressionContext{} { Tokens := tokens } )
                tokens := List<XToken>{}
            ENDIF
        END FUNCTION

        LOCAL FUNCTION AddNameTerm( idTokens AS List<XToken> ) AS VOID
            IF idTokens:Count == 3
                AddPartialTerm()
                VAR name := SqlNameExpressionContext{} { Tokens := idTokens, Table := idTokens[0]:Text, Name := idTokens[2]:Text }
                terms:Add(name)
                names:Add(name)
            ELSEIF idTokens:Count == 1
                AddPartialTerm()
                VAR name := SqlNameExpressionContext{} { Tokens := idTokens, Table := NULL, Name := idTokens[0]:Text }
                terms:Add(name)
                names:Add(name)
            ELSE
                tokens:AddRange( idTokens )
            ENDIF
        END FUNCTION

        DO WHILE ! SELF:Eos() .AND. ! done
            IF SELF:La1 == XTokenType.CROSS .AND. SELF:La2 == XTokenType.JOIN
                done := TRUE
                LOOP
            ENDIF
            SWITCH SELF:La1
            CASE XTokenType.LPAREN
                AddPartialTerm()
                parens:Push(SELF:MatchingParen())
                VAR o := SELF:ConsumeAndGet()
                VAR e := SELF:ParseExpressionContext()
                IF parens:Pop() != SELF:La1
                    THROW ArgumentException{i"Unmatched closing parenthesis: {SELF:La1}","SQLParser"}
                ENDIF
                VAR c := SELF:ConsumeAndGet()
                IF e IS NOT SqlSimpleExpressionContext .AND. e IS NOT SqlCompositeExpressionContext
                    hasComplexTerms := TRUE
                ENDIF
                IF e IS SqlCompositeExpressionContext VAR ce
                    names:AddRange(ce:Names)
                ENDIF
                terms:Add( SqlParenExpressionContext{} { Open := o, Expr := e, Close := c } )
                LOOP
            CASE XTokenType.RPAREN
                done := TRUE
            CASE XTokenType.ID
                IF (SELF:La2 == XTokenType.JOIN .OR. ;
                    (SELF:Lt1:Text:Equals("LEFT", StringComparison.OrdinalIgnoreCase) .AND. ;
                     SELF:Lt2:Text:Equals("OUTER", StringComparison.OrdinalIgnoreCase) .AND. ;
                     SELF:La3 == XTokenType.JOIN)) .AND. ;
                        (SELF:Lt1:Text:Equals("INNER", StringComparison.OrdinalIgnoreCase) .OR. ;
                         SELF:Lt1:Text:Equals("LEFT", StringComparison.OrdinalIgnoreCase) .OR. ;
                         SELF:Lt1:Text:Equals("CROSS", StringComparison.OrdinalIgnoreCase) .OR. ;
                         SELF:Lt1:Text:Equals("RIGHT", StringComparison.OrdinalIgnoreCase) .OR. ;
                         SELF:Lt1:Text:Equals("FULL", StringComparison.OrdinalIgnoreCase) .OR. ;
                         SELF:Lt1:Text:Equals("OUTER", StringComparison.OrdinalIgnoreCase))
                    done := TRUE
                    LOOP
                ENDIF
                LOCAL isOrderDirection := SELF:Lt1:Text:Equals("ASC", StringComparison.OrdinalIgnoreCase) .OR. ;
                    SELF:Lt1:Text:Equals("DESC", StringComparison.OrdinalIgnoreCase) AS LOGIC
                IF SELF:_parsingOrderBy .AND. isOrderDirection .AND. (tokens:Count > 0 .OR. terms:Count > 0)
                    done := TRUE
                ELSE
                    IF SELF:La2 == XTokenType.LPAREN .AND. ;
                       (SELF:Lt1:Text:Equals("COUNT", StringComparison.OrdinalIgnoreCase) .OR. ;
                        SELF:Lt1:Text:Equals("CNT", StringComparison.OrdinalIgnoreCase) .OR. ;
                        SELF:Lt1:Text:Equals("SUM", StringComparison.OrdinalIgnoreCase) .OR. ;
                        SELF:Lt1:Text:Equals("AVG", StringComparison.OrdinalIgnoreCase) .OR. ;
                        SELF:Lt1:Text:Equals("MIN", StringComparison.OrdinalIgnoreCase) .OR. ;
                        SELF:Lt1:Text:Equals("MAX", StringComparison.OrdinalIgnoreCase))
                        VAR functionName := SELF:ConsumeAndGetText()
                        SELF:Consume() // opening parenthesis
                        VAR aggregate := SqlAggregateExpressionContext{} { FunctionName := functionName:ToUpperInvariant() }
                        AddPartialTerm()
                        IF SELF:Matches(XTokenType.MULT)
                            aggregate:IsStar := TRUE
                            SELF:Consume()
                        ELSE
                            aggregate:Argument := SELF:ParseExpressionContext()
                        ENDIF
                        IF !SELF:Expect(XTokenType.RPAREN)
                            THROW ArgumentException{i"Expected closing parenthesis for aggregate {functionName}","SQLParser"}
                        ENDIF
                        terms:Add(aggregate)
                        hasComplexTerms := TRUE
                        hasAggregateTerm := TRUE
                        LOOP
                    ENDIF
                    VAR idTokens := List<XToken>{}
                    DO WHILE SELF:La2 == XTokenType.DOT .AND. SELF:La3 == XTokenType.ID
                        idTokens:Add(SELF:ConsumeAndGet())
                        idTokens:Add(SELF:ConsumeAndGet())
                    ENDDO
                    idTokens:Add(SELF:ConsumeAndGet())
                    AddNameTerm(idTokens)
                    LOOP
                ENDIF
            CASE XTokenType.LBRKT
            CASE XTokenType.LCURLY
                parens:Push(SELF:MatchingParen())
            CASE XTokenType.RBRKT
            CASE XTokenType.RCURLY
                IF parens:Count == 0
                    done := TRUE
                ELSEIF parens:Pop() != SELF:La1
                    THROW ArgumentException{i"Unmatched closing parenthesis: {SELF:La1}","SQLParser"}
                ENDIF
            OTHERWISE
                IF SELF:CompareOp() .OR. SELF:CombineOp() .OR. SELF:PrefixOp() .OR. SELF:Eoe()
                    IF parens:Count() == 0
                        done := TRUE
                    ENDIF
                ENDIF
            END SWITCH
            IF !done
                tokens:Add(SELF:ConsumeAndGet())
            ENDIF
        ENDDO
        AddPartialTerm()
        IF terms:Count > 1
            IF hasComplexTerms .AND. !hasAggregateTerm
                THROW ArgumentException{i"Unsupported combine or compare operator within expression term","SQLParser"}
            ENDIF
            RETURN SqlCompositeExpressionContext{} { Exprs := terms, Names := names }
        ELSEIF terms:Count == 1
            RETURN terms[0]
        ELSE // Empty term
            RETURN SqlSimpleExpressionContext{} { Tokens := tokens }
        ENDIF

END CLASS
END NAMESPACE

