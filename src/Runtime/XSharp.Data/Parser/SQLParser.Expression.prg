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
        LOCAL done := FALSE AS LOGIC
        LOCAL term := NULL AS SqlExpressionContext
        VAR parens := Stack<XTokenType>{}
        VAR tokens := List<XToken>{}

        IF SELF:La1 == XTokenType.LPAREN
            parens:Push(SELF:MatchingParen())
            VAR o := SELF:ConsumeAndGet()
            VAR e := SELF:ParseExpressionContext()
            IF parens:Pop() != SELF:La1
                THROW ArgumentException{i"Unmatched closing parenthesis: {SELF:La1}","SQLParser"}
            ENDIF
            VAR c := SELF:ConsumeAndGet()
            term := SqlParenExpressionContext{} { Open := o, Expr := e, Close := c }
        ENDIF

        DO WHILE ! SELF:Eos() .AND. ! done
            SWITCH SELF:La1
            CASE XTokenType.LPAREN
            CASE XTokenType.LBRKT
            CASE XTokenType.LCURLY
                parens:Push(SELF:MatchingParen())
            CASE XTokenType.RPAREN
            CASE XTokenType.RBRKT
            CASE XTokenType.RCURLY
                IF parens:Count == 0
                    done := TRUE
                ELSEIF parens:Pop() != SELF:La1
                    THROW ArgumentException{i"Unmatched closing parenthesis: {SELF:La1}","SQLParser"}
                ENDIF
            OTHERWISE
                IF SELF:CompareOp() .OR. SELF:CombineOp() .OR. SELF:Eoe()
                    IF parens:Count() == 0
                        done := TRUE
                    ENDIF
                ENDIF
            END SWITCH
            IF !done
                tokens:Add(SELF:ConsumeAndGet())
            ENDIF
        ENDDO
        IF term IS NOT NULL
            IF tokens:Count > 0
                VAR suffixExpr := SqlSimpleExpressionContext{} { Tokens := tokens }
                RETURN SqlCompsiteExpressionContext{} { Exprs := { term, suffixExpr } }
            ENDIF
            RETURN term
        ENDIF
        RETURN SqlSimpleExpressionContext{} { Tokens := tokens }

END CLASS
END NAMESPACE
