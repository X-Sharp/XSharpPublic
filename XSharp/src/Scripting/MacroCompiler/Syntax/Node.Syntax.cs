using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    abstract internal partial class Node
    {
        internal bool CompilerGenerated = false;
        internal Token Token = null;
        internal Node(Token t) { Token = t; }
    }
    abstract internal partial class Expr : Node
    {
        internal Expr(Token t) : base(t) { }
    }
    abstract internal partial class TypeExpr : Expr
    {
        internal TypeExpr(Token t) : base(t) { }
    }
    abstract internal partial class NameExpr : TypeExpr
    {
        internal string Name;
        internal int Arity;
        string lookupName = null;
        internal NameExpr(Token t, string name, int arity) : base(t) { Name = name; Arity = arity; }
        internal string LookupName { get { if (lookupName == null) lookupName = Arity == 0 ? Name : Name + '`' + Arity.ToString(); return lookupName; } }
        public override string ToString() { return Name; }
    }
    internal partial class SyntaxToken : Node
    {
        internal SyntaxToken(Token t) : base(t) { }
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
        internal TokenType Kind;
        internal NativeTypeExpr(Token t) : base(t) { Kind = t.type; }
        internal NativeTypeExpr(Token t, TokenType tt) : base(t) { Kind = tt; }
        public override string ToString() { return TokenText(Kind); }
    }
    internal partial class IdExpr : NameExpr
    {
        internal IdExpr(Token t): base(t, t.value, 0) { }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal Expr Expr;
        internal NameExpr Member;
        internal MemberAccessExpr(Expr e, NameExpr n) : base(n.Token) { Expr = e; Member = n; }
        public override string ToString() { return "(" + Expr.ToString() + ':' + Member.ToString() + ")"; }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal TypeExpr Expr;
        internal NameExpr Member;
        internal QualifiedNameExpr(TypeExpr e, NameExpr n) : base(n.Token, n.Name, n.Arity) { Expr = e; Member = n; }
        public override string ToString() { return "(" + Expr.ToString() + '.' + Member.ToString() + ")"; }
    }
    internal partial class AssignExpr : Expr
    {
        internal Expr Left;
        internal TokenType Kind;
        internal Expr Right;
        internal AssignExpr(Expr l, Token o, Expr r) : base(o) { Left = l; Kind = o.type; Right = r; }
        public override string ToString() { return "(" + Left.ToString() + TokenText(Kind) + Right.ToString() + ")"; }
    }
    internal partial class AssignOpExpr : AssignExpr
    {
        internal AssignOpExpr(Expr l, Token o, Expr r) : base(l, o, r) { }
    }
    internal partial class BinaryExpr : Expr
    {
        internal Expr Left;
        internal TokenType Kind;
        internal Expr Right;
        internal BinaryExpr(Expr l, Token o, Expr r) : base(o) { Left = l; Kind = o.type; Right = r; }
        public override string ToString() { return "(" + Left.ToString() + TokenText(Kind) + Right.ToString() + ")"; }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal BinaryLogicExpr(Expr l, Token o, Expr r) : base(l,o,r) {}
    }
    internal partial class UnaryExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal UnaryExpr(Expr e, Token o) : base(o) { Expr = e; Kind = o.type; }
        public override string ToString() { return "(" + TokenText(Kind) + Expr.ToString() + ")"; }
    }
    internal partial class PrefixExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal PrefixExpr(Expr e, Token o) : base(o) { Expr = e; Kind = o.type; }
        public override string ToString() { return "(" + TokenText(Kind) + Expr.ToString() + ")"; }
    }
    internal partial class PostfixExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal PostfixExpr(Expr e, Token o) : base(o) { Expr = e; Kind = o.type; }
        public override string ToString() { return "(" + Expr.ToString() + TokenText(Kind) + ")"; }
    }
    internal partial class LiteralExpr : Expr
    {
        internal TokenType Kind;
        internal string Value;
        internal LiteralExpr(Token t) : base(t) { Kind = t.type; Value = t.value; }
        internal LiteralExpr(Token t, TokenType tt) : base(t) { Kind = tt; Value = t.value; }
        public override string ToString() { return "'" + Value + "'"; }
    }
    internal partial class SelfExpr : Expr
    {
        internal SelfExpr(Token t) : base(t) { }
        public override string ToString() { return "(SELF)"; }
    }
    internal partial class SuperExpr : Expr
    {
        internal SuperExpr(Token t) : base(t) { }
        public override string ToString() { return "(SUPER)"; }
    }
    internal partial class CheckedExpr : Expr
    {
        internal Expr Expr;
        internal CheckedExpr(Expr e, Token t) : base(t) { Expr = e; }
        public override string ToString() { return "(CHECKED " + Expr.ToString() + ")"; }
    }
    internal partial class UncheckedExpr : Expr
    {
        internal Expr Expr;
        internal UncheckedExpr(Expr e, Token t) : base(t) { Expr = e; }
        public override string ToString() { return "(UNCHECKED " + Expr.ToString() + ")"; }
    }
    internal partial class TypeOfExpr : Expr
    {
        internal TypeExpr Type;
        internal TypeOfExpr(TypeExpr e, Token t) : base(t) { Type = e; }
        public override string ToString() { return "(TYPEOF " + Type.ToString() + ")"; }
    }
    internal partial class SizeOfExpr : Expr
    {
        internal TypeExpr Type;
        internal SizeOfExpr(TypeExpr e, Token t) : base(t) { Type = e; }
        public override string ToString() { return "(SIZEOF " + Type.ToString() + ")"; }
    }
    internal partial class DefaultExpr : Expr
    {
        internal TypeExpr Type;
        internal DefaultExpr(TypeExpr e, Token t) : base(t) { Type = e; }
        public override string ToString() { return "(DEFAULT " + Type.ToString() + ")"; }
    }
    internal partial class TypeCast : Expr
    {
        internal TypeExpr Type;
        internal Expr Expr;
        internal TypeCast(TypeExpr t, Expr e) : base(t?.Token ?? e.Token) { Type = t; Expr = e; }
        public override string ToString() { return "(" + Type.ToString() + ")" + Expr.ToString(); }
    }
    internal partial class TypeConversion : TypeCast
    {
        internal TypeConversion(TypeExpr t, Expr e) : base(t, e) { }
    }
    internal partial class IsExpr : Expr
    {
        internal Expr Expr;
        internal TypeExpr Type;
        internal IsExpr(Expr e, TypeExpr t, Token o) : base(o) { Expr = e; Type = t; }
        public override string ToString() { return "(" + Expr.ToString() + " IS " + Type.ToString() + ")"; }
    }
    internal partial class AsTypeExpr : Expr
    {
        internal Expr Expr;
        internal TypeExpr Type;
        internal AsTypeExpr(Expr e, TypeExpr t, Token o) : base(o) { Expr = e; Type = t; }
        public override string ToString() { return "(" + Expr.ToString() + " AS TYPE " + Type.ToString() + ")"; }
    }
    internal partial class MethodCallExpr : Expr
    {
        internal Expr Expr;
        internal ArgList Args;
        internal MethodCallExpr(Expr e, ArgList a) : base(e.Token) { Expr = e; Args = a; }
        public override string ToString() { return Expr.ToString() + "(" + Args.ToString() + ")"; }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal CtorCallExpr(TypeExpr e, ArgList a) : base(e,a) { }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
        internal ArrayAccessExpr(Expr e, ArgList a) : base(e, a) { }
    }
    internal partial class EmptyExpr : Expr
    {
        internal EmptyExpr(Token t) : base(t) { }
        public override string ToString() { return ""; }
    }
    internal partial class ExprList : Expr
    {
        internal IList<Expr> Exprs;
        internal ExprList(IList<Expr> e) : base(null) { Exprs = e; }
        public override string ToString() { var sb = new StringBuilder(); foreach (var e in Exprs) { if (sb.Length > 0) sb.Append(", "); sb.Append(e.ToString()); } return sb.ToString(); }
    }
    internal partial class LiteralArray : Expr
    {
        internal TypeExpr ElemType;
        internal ExprList Values;
        internal LiteralArray(ExprList values, TypeExpr elemType = null) : base(null) { ElemType = elemType;  Values = values; }
        public override string ToString() { return "{" + Values.ToString() + "}"; }
    }
    internal partial class IifExpr : Expr
    {
        internal Expr Cond;
        internal Expr True;
        internal Expr False;
        internal IifExpr(Expr cond, Expr trueExpr, Expr falseExpr, Token o) : base(o) { Cond = cond; True = trueExpr; False = falseExpr; }
        public override string ToString() { return "{IIF(" + Cond.ToString() + "," + True.ToString() + "," + False.ToString() + ")}"; }
    }
    internal partial class AliasExpr : Expr
    {
        internal Expr Alias;
        internal Expr Field;
        internal AliasExpr(Expr alias, Expr field, Token o) : base(o) { Alias = alias; Field = field; }
        public override string ToString() { return "{" + Alias.ToString() + "->" + Field.ToString() + ")}"; }
    }
    internal partial class Arg : Node
    {
        internal Expr Expr;
        internal Arg(Expr e) : base(e.Token) { Expr = e; }
        public override string ToString() { return Expr.ToString(); }
    }
    internal partial class ArgList : Node
    {
        internal IList<Arg> Args;
        internal ArgList(IList<Arg> a) : base(null) { Args = a; }
        public override string ToString() { var sb = new StringBuilder(); foreach (var a in Args) { if (sb.Length > 0) sb.Append(", "); sb.Append(a.ToString()); } return sb.ToString(); }
    }
    internal partial class Codeblock : Node
    {
        internal IList<IdExpr> Params;
        internal ExprList Body;
        internal Codeblock(IList<IdExpr> p, ExprList l) : base(null) { Params = p; Body = l; }
        string ParamsAsString() { if (Params == null) return ""; var sb = new StringBuilder(); foreach (var p in Params) { if (sb.Length > 0) sb.Append(", "); sb.Append(p.ToString()); } return sb.ToString(); }
        public override string ToString() { return "{|" + ParamsAsString() + "|" + Body.ToString() + "}"; }
    }
}
