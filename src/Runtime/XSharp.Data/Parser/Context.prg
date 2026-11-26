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

CLASS SqlFieldSelectionContext
    PROPERTY Expression AS SqlExpressionContext AUTO
    PROPERTY Alias AS STRING AUTO
    CONSTRUCTOR()
        RETURN
END CLASS

CLASS FoxSelectContext
    PROPERTY TopCount    AS STRING AUTO
    PROPERTY IsDistinct  AS LOGIC AUTO
    PROPERTY SelectList  AS List<SqlExpressionContext> AUTO
    PROPERTY FieldSelectionList AS List<SqlFieldSelectionContext> AUTO  // New property for field selections with aliases
    PROPERTY TableList   AS List<STRING> AUTO
    PROPERTY TableAliases AS Dictionary<STRING,STRING> AUTO
    PROPERTY DerivedTables AS Dictionary<STRING,FoxSelectContext> AUTO
    PROPERTY JoinList    AS List<STRING> AUTO
    PROPERTY Joins       AS List<FoxSelectJoinContext> AUTO
    PROPERTY TargetCursor AS STRING AUTO  // Property to store the target cursor name for INTO CURSOR
    PROPERTY TargetArray AS STRING AUTO   // Target variable name for INTO ARRAY
    PROPERTY TargetTable AS STRING AUTO   // Persistent table target for INTO TABLE
    PROPERTY WhereClause AS SqlExpressionContext AUTO
    PROPERTY GroupByClause AS SqlExpressionContext AUTO
    // GroupByClause remains the first item for callers compiled against the
    // original single-expression representation.
    PROPERTY GroupByItems AS List<SqlExpressionContext> AUTO
    PROPERTY HavingClause AS SqlExpressionContext AUTO
    PROPERTY OrderByClause AS List<FoxOrderByItem> AUTO
    PROPERTY UnionBranches AS List<FoxSelectContext> AUTO
    PROPERTY UnionAll AS List<LOGIC> AUTO
    // Runtime materialization used when LEFT JOIN requires NULL-extended rows.
    PROPERTY OuterJoinRows AS List<LONG[]> AUTO
    // Source record positions captured before LEFT JOIN execution changes work areas.
    PROPERTY WorkAreaRecordSnapshot AS Dictionary<STRING,LONG> AUTO
    CONSTRUCTOR()
        SelectList := List<SqlExpressionContext>{}
        FieldSelectionList := List<SqlFieldSelectionContext>{}  // Initialize the new property
        TableList := List<String>{}
        TableAliases := Dictionary<STRING,STRING>{StringComparer.OrdinalIgnoreCase}
        DerivedTables := Dictionary<STRING,FoxSelectContext>{StringComparer.OrdinalIgnoreCase}
        JoinList := List<String>{}
        Joins := List<FoxSelectJoinContext>{}
        OrderByClause := List<FoxOrderByItem>{}
        UnionBranches := List<FoxSelectContext>{}
        UnionAll := List<LOGIC>{}
        OuterJoinRows := NULL
        WorkAreaRecordSnapshot := NULL
        GroupByItems := List<SqlExpressionContext>{}
        RETURN

END CLASS

ENUM FoxSelectJoinKind
    MEMBER Inner
    MEMBER Cross
    MEMBER Left
END ENUM

CLASS FoxSelectJoinContext
    PROPERTY TableName AS STRING AUTO
    PROPERTY Alias AS STRING AUTO
    PROPERTY Kind AS FoxSelectJoinKind AUTO
    PROPERTY OnClause AS SqlExpressionContext AUTO
    // Set only on the execution copy used for NULL extension.
    PROPERTY NullRecordNumber AS LONG AUTO
    // True when this source was introduced by a comma, starting a new join group.
    PROPERTY IsCommaSource AS LOGIC AUTO
END CLASS

CLASS FoxOrderByItem
    PROPERTY Expression AS SqlExpressionContext AUTO
    PROPERTY Descending AS LOGIC AUTO
    CONSTRUCTOR(expression AS SqlExpressionContext, descending AS LOGIC)
        SELF:Expression := expression
        SELF:Descending := descending
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

