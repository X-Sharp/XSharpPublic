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

CLASS SqlNameExpressionContext INHERIT SqlSimpleExpressionContext
    PROPERTY Table AS STRING AUTO
    PROPERTY Name AS STRING AUTO
    CONSTRUCTOR()
        RETURN

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
        // If we have a table qualifier, convert TABLE.FIELD to TABLE->FIELD
        IF !String.IsNullOrEmpty(Table)
            LOCAL tableName AS STRING
            IF !tableAliases:TryGetValue(Table, OUT tableName)
                tableName := Table
            ENDIF
            sb:Append(tableName)
            sb:Append("->")
            sb:Append(Name)
        ELSE
            // For unqualified fields, we need to resolve them based on available tables
            // First, check if the field exists in multiple tables (ambiguous)
            VAR matchingTables := List<STRING>{}

            // In a real implementation, we would check if the field exists in each table
            // For now, we'll simulate checking by using a helper function
            FOREACH VAR table IN tableAliases:Values
                IF FieldInTable(table, Name) != NIL
                    matchingTables:Add(table)
                ENDIF
            NEXT

            IF matchingTables:Count == 0
                // Field doesn't exist in any table - just output the field name
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
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Expr:BuildString(sb)

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, tableAliases AS IDictionary<STRING,STRING>) AS VOID
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

END NAMESPACE

