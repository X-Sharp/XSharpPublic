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
        CASE XTokenType.JOIN
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
        VAR e := SELF:ParsePrefixExpression()
        IF SELF:CombineOp()
            VAR o := SELF:ConsumeAndGet()
            VAR r := SELF:ParsePrefixExpression()
            e := SqlLogicExpressionContext{} { Left := e, Op := o, Right := r }
        ENDIF
        IF ! SELF:Eos() .AND. ( SELF:CompareOp() .OR. SELF:PrefixOp() )
            THROW ArgumentException{i"Unexpected operator: {SELF:La1}","SQLParser"}
        ENDIF
        RETURN e

    PRIVATE METHOD ParsePrefixExpression() AS SqlExpressionContext
        LOCAL e := NULL AS SqlExpressionContext
        IF SELF:PrefixOp()
            VAR o := SELF:ConsumeAndGet()
            VAR c := SELF:ParseCompareExpression()
            e := SqlPrefixExpressionContext{} { Op := o, Expr := c }
        ELSE
            e := SELF:ParseCompareExpression()
        ENDIF
        RETURN e

    PRIVATE METHOD ParseCompareExpression() AS SqlExpressionContext
        VAR e := SELF:ParseExpressionTerm()
        IF SELF:CompareOp()
            VAR o := SELF:ConsumeAndGet()
            VAR r := SELF:ParseExpressionTerm()
            e := SqlCompareExpressionContext{} { Left := e, Op := o, Right := r }
        ENDIF
        RETURN e

    PRIVATE METHOD ParseExpressionTerm() AS SqlExpressionContext
        VAR done := FALSE
        VAR hasComplexTerms := FALSE
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
                IF e IS NOT SqlSimpleExpressionContext .AND. e IS NOT SqlCompsiteExpressionContext
                    hasComplexTerms := TRUE
                ENDIF
                IF e IS SqlCompsiteExpressionContext VAR ce
                    names:AddRange(ce:Names)
                ENDIF
                terms:Add( SqlParenExpressionContext{} { Open := o, Expr := e, Close := c } )
                LOOP
            CASE XTokenType.RPAREN
                done := TRUE
            CASE XTokenType.ID
                VAR idTokens := List<XToken>{}
                DO WHILE SELF:La2 == XTokenType.DOT .AND. SELF:La3 == XTokenType.ID
                    idTokens:Add(SELF:ConsumeAndGet())
                    idTokens:Add(SELF:ConsumeAndGet())
                ENDDO
                idTokens:Add(SELF:ConsumeAndGet())
                AddNameTerm(idTokens)
                LOOP
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
            IF hasComplexTerms
                THROW ArgumentException{i"Unsupported combine or compare operator within expression term","SQLParser"}
            ENDIF
            RETURN SqlCompsiteExpressionContext{} { Exprs := terms, Names := names }
        ELSEIF terms:Count == 1
            RETURN terms[0]
        ELSE // Empty term
            RETURN SqlSimpleExpressionContext{} { Tokens := tokens }
        ENDIF

END CLASS
END NAMESPACE
