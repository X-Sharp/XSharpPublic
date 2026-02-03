USING System.Collections.Generic
USING System.Text
using System.Diagnostics
using XSharp.RDD.Support
using XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.Parsers
    ENUM FoxAlterMode
        MEMBER AddColumn
        MEMBER AlterColumn
        MEMBER DropColumn
        MEMBER AlterTable
    END ENUM

[DebuggerDisplay("{Mode,nq} {Name,nq}")];
CLASS FoxAlterTableContext
    PROPERTY Name       as STRING AUTO
    PROPERTY Mode       AS FoxAlterMode AUTO
    PROPERTY ColumnInfo AS FoxColumnContext AUTO
    PROPERTY TableRules as STRING AUTO
END CLASS

[DebuggerDisplay("{Type,nq} {Name,nq}")];
CLASS FoxCreateTableContext
    PROPERTY Name       as STRING AUTO
    PROPERTY IsCursor   AS LOGIC AUTO
    PROPERTY Type       as STRING => IIF(IsCursor, "CURSOR","TABLE")
    PROPERTY Columns    as List<FoxColumnContext> AUTO
    PROPERTY CodePage   as LONG AUTO
    PROPERTY ArrayName  as STRING AUTO
    PROPERTY RuleExpression as SqlExpressionContext AUTO
    PROPERTY RuleText       as string Auto
    PROPERTY Free       AS LOGIC AUTO
    PROPERTY LongName   AS STRING AUTO
    CONSTRUCTOR()
        SELF:Columns := List<FoxColumnContext>{}
        RETURN

    OVERRIDE METHOD ToString() AS STRING
        var sb := StringBuilder{}
        sb:Append(Type)
        sb:Append(" ")
        sb:Append(Name)
        sb:Append(" ")
        if RuleExpression != NULL
            sb:Append("Check "+RuleExpression:ToString())
        ENDIF
        return sb:ToString()
        END CLASS

[DebuggerDisplay("{Name,nq} {FieldType}")];
CLASS FoxColumnContext INHERIT RddFieldInfo
    PROPERTY Foreign    as STRING AUTO
    PROPERTY ForeignTag AS STRING AUTO
    PROPERTY Table      AS FoxCreateTableContext AUTO
    CONSTRUCTOR ()
        SUPER("",DbFieldType.Unknown,0,0)
        RETURN

    OVERRIDE METHOD ToString() AS STRING
        var sb := StringBuilder{}
        sb:Append(SELF:Name)
        sb:Append(" ")
        sb:Append(SELF:FieldType:ToString())
        IF SELF:Length > 0
            sb:Append("(")
            sb:Append(SELF:Length)
            IF SELF:Decimals > 0
                sb:Append(",")
                sb:Append(SELF:Decimals)
            ENDIF
            sb:Append(")")

        ENDIF
        if SELF:IsNullable
            sb:Append(" Null")
        endif
        if SELF:IsUnique
            sb:Append(" Unique")
        endif
        if SELF:PrimaryKey
            sb:Append(" Primary key")
        endif
        if SELF:DefaultValue != NULL
            sb:Append(" Default " +SELF:DefaultValue:ToString())
        endif
        if SELF:RuleExpression != NULL
            sb:Append(" Check " +SELF:RuleExpression:ToString())
            if !String.IsNullOrEmpty(SELF:RuleText)
                sb:Append(" Error " +SELF:RuleText)
            endif
        endif
        return sb:ToString()

END CLASS

CLASS FoxDeleteContext
    PROPERTY TableName  AS STRING AUTO
    PROPERTY IsForce    AS LOGIC AUTO
    PROPERTY TableList  AS List<STRING> AUTO
    PROPERTY JoinList   AS List<STRING> AUTO
    PROPERTY WhereClause AS SqlExpressionContext AUTO
    CONSTRUCTOR()
        IsForce   := FALSE
        TableList := List<String>{}
        JoinList  := List<String>{}
        RETURN

END CLASS
CLASS FoxUpdateContext
    PROPERTY TableName   AS STRING AUTO
    PROPERTY ColumnList  AS List<STRING> AUTO
    PROPERTY ValueList   AS List<SqlExpressionContext> AUTO
    PROPERTY IsForce    AS LOGIC AUTO
    PROPERTY TableList  AS List<STRING> AUTO
    PROPERTY JoinList   AS List<STRING> AUTO
    PROPERTY WhereClause AS SqlExpressionContext AUTO
    CONSTRUCTOR()
        IsForce   := FALSE
        ColumnList := List<String>{}
        ValueList := List<SqlExpressionContext>{}
        TableList := List<String>{}
        JoinList  := List<String>{}
        RETURN

END CLASS
CLASS FoxSelectContext
    PROPERTY TopCount    AS STRING AUTO
    PROPERTY IsDistinct  AS LOGIC AUTO
    PROPERTY SelectList  AS List<SqlExpressionContext> AUTO
    PROPERTY TableList   AS List<STRING> AUTO
    PROPERTY JoinList    AS List<STRING> AUTO
    PROPERTY WhereClause AS SqlExpressionContext AUTO
    PROPERTY GroupByClause AS SqlExpressionContext AUTO
    PROPERTY HavingClause AS SqlExpressionContext AUTO
    PROPERTY OrderByClause AS SqlExpressionContext AUTO
    CONSTRUCTOR()
        SelectList := List<SqlExpressionContext>{}
        TableList := List<String>{}
        JoinList := List<String>{}
        RETURN

END CLASS
CLASS FoxInsertContext
    PROPERTY TableName   AS STRING AUTO
    PROPERTY ColumnList  AS List<STRING> AUTO
    PROPERTY ValueList   AS List<SqlExpressionContext> AUTO  // For VALUES clause
    PROPERTY SelectStmt  AS STRING AUTO        // For INSERT ... SELECT
    PROPERTY IsForce     AS LOGIC AUTO
    CONSTRUCTOR()
        ColumnList := List<String>{}
        ValueList := List<SqlExpressionContext>{}
        RETURN

END CLASS
END NAMESPACE

