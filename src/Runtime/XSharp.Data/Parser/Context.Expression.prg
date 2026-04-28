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

CLASS SqlCompsiteExpressionContext INHERIT SqlExpressionContext
    PROPERTY Exprs AS IList<SqlExpressionContext> AUTO
    CONSTRUCTOR()
        RETURN
    OVERRIDE METHOD BuildString(sb AS StringBuilder) AS VOID
        FOREACH VAR e IN Exprs
            e:BuildString(sb)
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
