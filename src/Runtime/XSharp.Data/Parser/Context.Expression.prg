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

    VIRTUAL METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        // Default implementation calls BuildString; override in derived classes as needed
        SELF:BuildString(sb)
        RETURN
    METHOD ToResolvedString(availableTables AS IList<STRING>) AS STRING
        VAR sb := StringBuilder{}
        SELF:BuildStringWithFieldResolution(sb, availableTables)
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

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        // If we have a table qualifier, convert TABLE.FIELD to TABLE->FIELD
        IF !String.IsNullOrEmpty(Table)
            sb:Append(Table)
            sb:Append("->")
            sb:Append(Name)
        ELSE
            // For unqualified fields, we need to resolve them based on available tables
            // First, check if the field exists in multiple tables (ambiguous)
            VAR matchingTables := List<STRING>{}

            // In a real implementation, we would check if the field exists in each table
            // For now, we'll simulate checking by using a helper function
            FOREACH VAR table IN availableTables
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

CLASS SqlCompsiteExpressionContext INHERIT SqlExpressionContext
    PROPERTY Exprs AS IList<SqlExpressionContext> AUTO
    PROPERTY Names AS IList<SqlNameExpressionContext> AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        FOREACH VAR e IN Exprs
            e:BuildString(sb)
        NEXT

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        FOREACH VAR e IN Exprs
            e:BuildStringWithFieldResolution(sb, availableTables)
        NEXT
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

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        sb:Append(Open:Leadingws)
        sb:Append(Open:Text)
        Expr:BuildStringWithFieldResolution(sb, availableTables)
        sb:Append(Close:Leadingws)
        sb:Append(Close:Text)
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

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        Left:BuildStringWithFieldResolution(sb, availableTables)
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Right:BuildStringWithFieldResolution(sb, availableTables)
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

    OVERRIDE METHOD BuildStringWithFieldResolution(sb AS StringBuilder, availableTables AS IList<STRING>) AS VOID
        sb:Append(Op:Leadingws)
        sb:Append(Op:Text)
        Expr:BuildStringWithFieldResolution(sb, availableTables)
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
