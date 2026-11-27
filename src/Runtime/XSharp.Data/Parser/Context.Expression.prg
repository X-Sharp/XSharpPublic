// Context.prg
// Created by    : nikos
// Creation Date : 11/15/2025 11:05:07 PM
// Created for   :
// WorkStation   : DESKTOP-TJFSDLK

USING System.Collections.Generic
USING System.Text
using System.Diagnostics
using XSharp.RDD.Support
using XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.Parsers

ABSTRACT CLASS SqlExpressionContext
    CONSTRUCTOR()
        RETURN

    ABSTRACT METHOD BuildString(sb AS StringBuilder) AS VOID
    OVERRIDE METHOD ToString() AS STRING
        VAR sb := StringBuilder{}
        SELF:BuildString(sb)
        RETURN sb:ToString()

    VIRTUAL METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        // Default implementation calls BuildString; override in derived classes as needed
        SELF:BuildString(sb)
        RETURN

    VIRTUAL METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        // Default implementation returns empty list; override in derived classes as needed
        RETURN List<STRING>{}

    METHOD ToResolvedString(tableAliases AS IDictionary<STRING,STRING>) AS STRING
        VAR sb := StringBuilder{}
        SELF:BuildStringWithFieldResolution(sb, tableAliases)
        RETURN sb:ToString()
END CLASS

CLASS SqlSimpleExpressionContext INHERIT SqlExpressionContext
    PROPERTY Tokens AS IList<XToken> AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        FOREACH VAR t IN Tokens
            sb:Append(t:Leadingws)
            sb:Append(t:Text)
        NEXT
END CLASS

CLASS SqlWildcardExpressionContext INHERIT SqlExpressionContext
    // An optional table alias restricts expansion to that FROM source.
    PROPERTY Table AS STRING AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        IF !String.IsNullOrEmpty(Table)
            sb:Append(Table)
            sb:Append(".")
        ENDIF
        sb:Append("*")
    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        SELF:BuildString(sb)
END CLASS

CLASS SqlNameExpressionContext INHERIT SqlSimpleExpressionContext
    PROPERTY Table AS STRING AUTO
    PROPERTY Name AS STRING AUTO
    CONSTRUCTOR()
        RETURN

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        IF !String.IsNullOrEmpty(Table)
            // If we have a table qualifier, convert TABLE.FIELD to TABLE->FIELD
            IF Tokens:Count > 0
                sb:Append(Tokens[0]:Leadingws)
            ENDIF
            LOCAL tableName AS STRING
            IF !tableAliases:TryGetValue(Table, OUT tableName)
                tableName := Table
            ENDIF
            sb:Append(tableName)
            sb:Append("->")
            sb:Append(Name)
        ELSE
            // For unqualified fields, we need to resolve them based on available tables
            VAR matchingTables := List<STRING>{}
            FOREACH VAR table IN tableAliases:Values
                IF FieldInTable(table, Name) != NIL
                    matchingTables:Add(table)
                ENDIF
            NEXT

            IF matchingTables:Count == 0
                // Field doesn't exist in any table - just output the name
                sb:Append(Name)
            ELSEIF matchingTables:Count == 1
                // Field exists in exactly one table - use that table
                sb:Append(matchingTables[0])
                sb:Append("->")
                sb:Append(Name)
            ELSE
                // Field exists in multiple tables - raise an error
                THROW Error{ei"Ambiguous field reference '{Name}' exists in multiple tables: {String.Join(\", \", matchingTables)}"}
            ENDIF
        ENDIF

    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        VAR result := List<STRING>{}
        // If we have a table qualifier, add that table to dependencies
        IF !String.IsNullOrEmpty(Table)
            LOCAL tableName AS STRING
            IF !tableAliases:TryGetValue(Table, OUT tableName)
                tableName := Table
            ENDIF
            result:Add(tableName)
        ELSE
            // For unqualified fields, find which tables contain this field
            VAR matchingTables := List<STRING>{}
            FOREACH VAR table IN tableAliases:Values
                IF FieldInTable(table, Name) != NIL
                    matchingTables:Add(table)
                ENDIF
            NEXT

            // Add all matching tables to dependencies
            result:AddRange(matchingTables)
        ENDIF
        RETURN result

    /// Helper method to check if a field exists in a table
    PRIVATE STATIC METHOD FieldInTable(tableName AS STRING, fieldName AS STRING) AS USUAL
        // If the table name matches the current workarea, check if the field exists
        IF Alias() == tableName
            RETURN FieldPos(fieldName) > 0
        ENDIF

        VAR area := RuntimeState.Workareas:FindAlias(tableName)
        IF area > 0
            // Return if the field exists
            VAR pos := (tableName)->FieldPos(fieldName)
            IF pos > 0
                RETURN (tableName)->DbStruct()[pos]
            ENDIF
        ENDIF

        RETURN NIL
END CLASS

/// Aggregate call represented in the expression tree so SELECT, HAVING, and
/// ORDER BY consumers can distinguish it from an ordinary scalar function.
CLASS SqlAggregateExpressionContext INHERIT SqlExpressionContext
    PROPERTY FunctionName AS STRING AUTO
    PROPERTY Argument AS SqlExpressionContext AUTO
    PROPERTY IsStar AS LOGIC AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        sb:Append(FunctionName)
        sb:Append("(")
        IF IsStar
            sb:Append("*")
        ELSEIF Argument != NULL
            Argument:BuildString(sb)
        ENDIF
        sb:Append(")")
    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        sb:Append(FunctionName)
        sb:Append("(")
        IF IsStar
            sb:Append("*")
        ELSEIF Argument != NULL
            Argument:BuildStringWithFieldResolution(sb, tableAliases)
        ENDIF
        sb:Append(")")
    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        IF Argument != NULL
            RETURN Argument:GetTableDependencies(tableAliases)
        ENDIF
        RETURN List<STRING>{}
END CLASS

CLASS SqlCompositeExpressionContext INHERIT SqlExpressionContext
    PROPERTY Exprs AS IList<SqlExpressionContext> AUTO
    PROPERTY Names AS IList<SqlNameExpressionContext> AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        FOREACH VAR e IN Exprs
            e:BuildString(sb)
        NEXT

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        FOREACH VAR e IN Exprs
            e:BuildStringWithFieldResolution(sb, tableAliases)
        NEXT

    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        VAR result := List<STRING>{}
        FOREACH VAR e IN Exprs
            VAR deps := e:GetTableDependencies(tableAliases)
            FOREACH VAR dep IN deps
                IF !result:Contains(dep)
                    result:Add(dep)
                ENDIF
            NEXT
        NEXT
        RETURN result
END CLASS

CLASS SqlParenExpressionContext INHERIT SqlExpressionContext
    PROPERTY Open AS XToken AUTO
    PROPERTY Expr AS SqlExpressionContext AUTO
    PROPERTY Close AS XToken AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        sb:Append(Open:Leadingws)
        sb:Append(Open:Text)
        Expr:BuildString(sb)
        sb:Append(Close:Leadingws)
        sb:Append(Close:Text)

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        sb:Append(Open:Leadingws)
        sb:Append(Open:Text)
        Expr:BuildStringWithFieldResolution(sb, tableAliases)
        sb:Append(Close:Leadingws)
        sb:Append(Close:Text)

    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        RETURN Expr:GetTableDependencies(tableAliases)
END CLASS

CLASS SqlBinaryExpressionContext INHERIT SqlExpressionContext
    PROPERTY Left AS SqlExpressionContext AUTO
    PROPERTY Op AS XToken AUTO
    PROPERTY Right AS SqlExpressionContext AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        Left:BuildString(sb)
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Right:BuildString(sb)

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        Left:BuildStringWithFieldResolution(sb, tableAliases)
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Right:BuildStringWithFieldResolution(sb, tableAliases)

    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        VAR result := List<STRING>{}
        VAR leftDeps := Left:GetTableDependencies(tableAliases)
        VAR rightDeps := Right:GetTableDependencies(tableAliases)

        // Add dependencies from left expression
        FOREACH VAR dep IN leftDeps
            IF !result:Contains(dep)
                result:Add(dep)
            ENDIF
        NEXT

        // Add dependencies from right expression
        FOREACH VAR dep IN rightDeps
            IF !result:Contains(dep)
                result:Add(dep)
            ENDIF
        NEXT

        RETURN result
END CLASS

CLASS SqlPrefixExpressionContext INHERIT SqlExpressionContext
    PROPERTY Op AS XToken AUTO
    PROPERTY Expr AS SqlExpressionContext AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        IF Op:Type == XTokenType.NOT .AND. Expr IS SqlInSubqueryExpressionContext VAR inSubquery
            inSubquery:BuildWithNegation(sb, NULL, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlInListExpressionContext VAR inList
            inList:BuildWithNegation(sb, NULL, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlParenExpressionContext VAR paren .AND. ;
               paren:Expr IS SqlInSubqueryExpressionContext VAR parenthesizedSubquery
            parenthesizedSubquery:BuildWithNegation(sb, NULL, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlParenExpressionContext VAR listParen .AND. ;
               listParen:Expr IS SqlInListExpressionContext VAR parenthesizedList
            parenthesizedList:BuildWithNegation(sb, NULL, TRUE)
            RETURN
        ENDIF
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Expr:BuildString(sb)

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        IF Op:Type == XTokenType.NOT .AND. Expr IS SqlInSubqueryExpressionContext VAR inSubquery
            inSubquery:BuildWithNegation(sb, tableAliases, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlInListExpressionContext VAR inList
            inList:BuildWithNegation(sb, tableAliases, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlParenExpressionContext VAR paren .AND. ;
               paren:Expr IS SqlInSubqueryExpressionContext VAR parenthesizedSubquery
            parenthesizedSubquery:BuildWithNegation(sb, tableAliases, TRUE)
            RETURN
        ELSEIF Op:Type == XTokenType.NOT .AND. Expr IS SqlParenExpressionContext VAR listParen .AND. ;
               listParen:Expr IS SqlInListExpressionContext VAR parenthesizedList
            parenthesizedList:BuildWithNegation(sb, tableAliases, TRUE)
            RETURN
        ENDIF
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Expr:BuildStringWithFieldResolution(sb, tableAliases)

    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        RETURN Expr:GetTableDependencies(tableAliases)
END CLASS

CLASS SqlLogicExpressionContext INHERIT SqlBinaryExpressionContext
    CONSTRUCTOR()
        RETURN
END CLASS

CLASS SqlCompareExpressionContext INHERIT SqlBinaryExpressionContext
    CONSTRUCTOR()
        RETURN
END CLASS

CLASS SqlExistsSubqueryExpressionContext INHERIT SqlExpressionContext
    PROPERTY Subquery AS FoxSelectContext AUTO
    PROPERTY HasRows AS LOGIC AUTO
    PROPERTY IsPrepared AS LOGIC AUTO

    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        IF !IsPrepared
            THROW System.InvalidOperationException{"EXISTS subquery has not been prepared for execution"}
        ENDIF
        sb:Append(IIF(HasRows, ".T.", ".F."))
END CLASS

CLASS SqlScalarSubqueryExpressionContext INHERIT SqlExpressionContext
    PROPERTY Subquery AS FoxSelectContext AUTO
    PROPERTY Value AS USUAL AUTO
    PROPERTY ResultFieldDefinition AS ARRAY AUTO
    PROPERTY IsPrepared AS LOGIC AUTO

    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        IF !IsPrepared
            THROW System.InvalidOperationException{"Scalar subquery has not been prepared for execution"}
        ENDIF
        IF IsNil(Value) .OR. Value:IsNull
            sb:Append(".NULL.")
        ELSE
            SqlInSubqueryExpressionContext.AppendValue(sb, Value)
        ENDIF
END CLASS

/// A quantified comparison against the single-column result of a SELECT.
CLASS SqlQuantifiedSubqueryExpressionContext INHERIT SqlExpressionContext
    PROPERTY ValueExpression AS SqlExpressionContext AUTO
    PROPERTY Op AS XToken AUTO
    PROPERTY Quantifier AS STRING AUTO
    PROPERTY Subquery AS FoxSelectContext AUTO
    PROPERTY Values AS IList<USUAL> AUTO

    CONSTRUCTOR()
        Values := NULL
        RETURN

    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        THROW System.InvalidOperationException{"Quantified subquery predicate must be evaluated as a WHERE predicate"}
END CLASS

/// An uncorrelated SELECT used as the value set of an IN predicate.
CLASS SqlInSubqueryExpressionContext INHERIT SqlExpressionContext
    PROPERTY ValueExpression AS SqlExpressionContext AUTO
    PROPERTY Subquery AS FoxSelectContext AUTO
    PROPERTY IsNegated AS LOGIC AUTO
    PROPERTY Values AS IList<USUAL> AUTO

    CONSTRUCTOR()
        Values := NULL
        RETURN

    METHOD BuildWithNegation(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>, negated AS LOGIC) AS VOID
        LOCAL previous := IsNegated AS LOGIC
        IsNegated := negated
        TRY
            IF tableAliases == NULL
                SELF:BuildString(sb)
            ELSE
                SELF:BuildStringWithFieldResolution(sb, tableAliases)
            ENDIF
        FINALLY
            IsNegated := previous
        END TRY

    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        SELF:BuildMembershipPredicate(sb, NULL)

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        SELF:BuildMembershipPredicate(sb, tableAliases)

    PRIVATE METHOD BuildMembershipPredicate(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        IF Values == NULL
            THROW InvalidOperationException{"IN subquery has not been prepared for execution"}
        ENDIF
        IF Values:Count == 0
            sb:Append(IIF(IsNegated, ".T.", ".F."))
            RETURN
        ENDIF
        IF IsNegated .AND. SELF:HasNullValue()
            sb:Append(".F.")
            RETURN
        ENDIF
        IF !SELF:HasNonNullValue()
            sb:Append(".F.")
            RETURN
        ENDIF
        // WHERE treats UNKNOWN as nonmatching. Emit an explicit Boolean result so
        // MacroCompiler's NOT/.NULL. coercion cannot admit NULL outer values.
        sb:Append("IIF(ISNULL(")
        SELF:AppendValueExpression(sb, tableAliases)
        sb:Append("), .F., IIF(INLIST(")
        SELF:AppendValueExpression(sb, tableAliases)
        sb:Append(", ")
        SELF:AppendNonNullValues(sb)
        sb:Append("), ")
        sb:Append(IIF(IsNegated, ".F., .T.))", ".T., .F.))"))

    PRIVATE METHOD AppendValueExpression(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        IF tableAliases == NULL
            ValueExpression:BuildString(sb)
        ELSE
            ValueExpression:BuildStringWithFieldResolution(sb, tableAliases)
        ENDIF
        RETURN

    PRIVATE METHOD HasNullValue() AS LOGIC
        FOREACH VAR value IN Values
            IF value:IsNull
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE

    PRIVATE METHOD HasNonNullValue() AS LOGIC
        FOREACH VAR value IN Values
            IF !value:IsNull
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE

    PRIVATE METHOD AppendNonNullValues(sb AS StringBuilder) AS VOID
        LOCAL first := TRUE AS LOGIC
        FOREACH VAR value IN Values
            IF value:IsNull
                LOOP
            ENDIF
            IF !first
                sb:Append(", ")
            ENDIF
            SqlInSubqueryExpressionContext.AppendValue(sb, value)
            first := FALSE
        NEXT
        RETURN

    STATIC METHOD AppendValue(sb AS StringBuilder, value AS USUAL) AS VOID
        IF value:IsNull
            sb:Append(".NULL.")
            RETURN
        ENDIF
        VAR boxed := (OBJECT)value
        SWITCH UsualType(boxed)
        CASE __UsualType.String
        CASE __UsualType.Memo
            sb:Append("'")
            sb:Append(((STRING)boxed):Replace("'", "''"))
            sb:Append("'")
        CASE __UsualType.Logic
            sb:Append(IIF((LOGIC)boxed, ".T.", ".F."))
        CASE __UsualType.Date
            sb:Append("{^")
            sb:Append(((DATE)boxed):ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture))
            sb:Append("}")
        CASE __UsualType.DateTime
            sb:Append("{^")
            sb:Append(((System.DateTime)boxed):ToString("yyyy-MM-dd HH:mm:ss.fff", System.Globalization.CultureInfo.InvariantCulture))
            sb:Append("}")
        OTHERWISE
            IF UsualType(boxed) == __UsualType.Byte .OR. UsualType(boxed) == __UsualType.ShortInt .OR. ;
               UsualType(boxed) == __UsualType.Word .OR. UsualType(boxed) == __UsualType.Long .OR. ;
               UsualType(boxed) == __UsualType.DWord .OR. UsualType(boxed) == __UsualType.Int64 .OR. ;
               UsualType(boxed) == __UsualType.UInt64 .OR. UsualType(boxed) == __UsualType.Real4 .OR. ;
               UsualType(boxed) == __UsualType.Real8 .OR. UsualType(boxed) == __UsualType.Float .OR. ;
               UsualType(boxed) == __UsualType.Decimal .OR. ;
               UsualType(boxed) == __UsualType.Currency
                sb:Append(Convert.ToString(boxed, System.Globalization.CultureInfo.InvariantCulture))
            ELSE
                THROW Error{"Unsupported value type in IN subquery result"}
            ENDIF
        END SWITCH
END CLASS

CLASS SqlInListExpressionContext INHERIT SqlExpressionContext
    PROPERTY ValueExpression AS SqlExpressionContext AUTO
    PROPERTY Items AS IList<SqlExpressionContext> AUTO
    PROPERTY IsNegated AS LOGIC AUTO
    CONSTRUCTOR()
        Items := List<SqlExpressionContext>{}
        RETURN
    METHOD BuildWithNegation(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>, negated AS LOGIC) AS VOID
        LOCAL previous := IsNegated AS LOGIC
        IsNegated := negated
        TRY
            SELF:BuildResolved(sb, tableAliases)
        FINALLY
            IsNegated := previous
        END TRY
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        SELF:BuildResolved(sb, NULL)
    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        SELF:BuildResolved(sb, tableAliases)
    PRIVATE METHOD BuildResolved(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        sb:Append("(!ISNULL(")
        IF tableAliases == NULL
            ValueExpression:BuildString(sb)
        ELSE
            ValueExpression:BuildStringWithFieldResolution(sb, tableAliases)
        ENDIF
        sb:Append(") .AND. ")
        IF IsNegated
            sb:Append("!")
        ENDIF
        sb:Append("INLIST(")
        IF tableAliases == NULL
            ValueExpression:BuildString(sb)
        ELSE
            ValueExpression:BuildStringWithFieldResolution(sb, tableAliases)
        ENDIF
        sb:Append(", ")
        LOCAL first := TRUE AS LOGIC
        LOCAL containsNull := FALSE AS LOGIC
        FOREACH VAR item IN Items
            IF !first
                sb:Append(", ")
            ENDIF
            IF tableAliases == NULL
                item:BuildString(sb)
            ELSE
                item:BuildStringWithFieldResolution(sb, tableAliases)
            ENDIF
            IF item:ToString():Trim():Equals(".NULL.", StringComparison.OrdinalIgnoreCase)
                containsNull := TRUE
            ENDIF
            first := FALSE
        NEXT
        sb:Append(")")
        IF IsNegated .AND. containsNull
            sb:Append(" .AND. .F.")
        ENDIF
        sb:Append(")")
    OVERRIDE METHOD GetTableDependencies(tableAliases AS IDictionary<STRING,STRING>) AS IList<STRING>
        VAR result := List<STRING>{}
        FOREACH VAR dep IN ValueExpression:GetTableDependencies(tableAliases)
            IF !result:Contains(dep)
                result:Add(dep)
            ENDIF
        NEXT
        FOREACH VAR item IN Items
            FOREACH VAR dep IN item:GetTableDependencies(tableAliases)
                IF !result:Contains(dep)
                    result:Add(dep)
                ENDIF
            NEXT
        NEXT
        RETURN result
END CLASS

END NAMESPACE


