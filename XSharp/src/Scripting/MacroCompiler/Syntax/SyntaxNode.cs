using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    internal partial class Node
    {
        internal bool CompilerGenerated = false;
    }
    internal partial class Expr : Node
    {
    }
    internal partial class StoreTemp : Expr
    {
        internal Expr Expr;
        internal StoreTemp(Expr e)
        {
            CompilerGenerated = true;
            Expr = e;
        }
    }
    internal partial class LoadTemp : Expr
    {
        internal Expr Expr;
        internal StoreTemp Temp;
        internal LoadTemp(Expr e, StoreTemp t)
        {
            CompilerGenerated = true;
            Expr = e;
            Temp = t;
        }
        internal LoadTemp(StoreTemp t)
        {
            CompilerGenerated = true;
            Expr = null;
            Temp = t;
        }
    }
    internal partial class TypeExpr : Expr
    {
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
        internal TokenType Kind;
        internal NativeTypeExpr(TokenType kind) { Kind = kind; }
        public override string ToString() { return TokenText(Kind); }
    }
    internal partial class NameExpr : TypeExpr
    {
        internal string Name;
        internal int Arity;
        string lookupName = null;
        internal NameExpr(string name, int arity) { Name = name; Arity = arity; }
        internal NameExpr(): this(null,0) { }
        internal string LookupName { get { if (lookupName == null) lookupName = Arity == 0 ? Name : Name + '`' + Arity.ToString(); return lookupName; } }
        public override string ToString() { return Name; }
    }
    internal partial class IdExpr : NameExpr
    {
        internal IdExpr(string n): base(n, 0) { }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal Expr Expr;
        internal NameExpr Member;
        internal MemberAccessExpr(Expr e, NameExpr n) { Expr = e; Member = n; }
        public override string ToString() { return "(" + Expr.ToString() + ':' + Member.ToString() + ")"; }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal NameExpr Expr;
        internal NameExpr Member;
        internal QualifiedNameExpr(NameExpr e, NameExpr n) { Expr = e; Member = n; }
        public override string ToString() { return "(" + Expr.ToString() + '.' + Member.ToString() + ")"; }
    }
    internal partial class AssignExpr : Expr
    {
        internal Expr Left;
        internal TokenType Kind;
        internal Expr Right;
        internal AssignExpr(Expr l, TokenType o, Expr r) { Left = l; Kind = o; Right = r; }
        public override string ToString() { return "(" + Left.ToString() + TokenText(Kind) + Right.ToString() + ")"; }
    }
    internal partial class AssignOpExpr : AssignExpr
    {
        internal AssignOpExpr(Expr l, TokenType o, Expr r) : base(l, o, r) { }
    }
    internal partial class BinaryExpr : Expr
    {
        internal Expr Left;
        internal TokenType Kind;
        internal Expr Right;
        internal BinaryExpr(Expr l, TokenType o, Expr r) { Left = l; Kind = o; Right = r; }
        public override string ToString() { return "(" + Left.ToString() + TokenText(Kind) + Right.ToString() + ")"; }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal BinaryLogicExpr(Expr l, TokenType o, Expr r) : base(l,o,r) {}
    }
    internal partial class UnaryExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal UnaryExpr(Expr e, TokenType o) { Expr = e; Kind = o; }
        public override string ToString() { return "(" + TokenText(Kind) + Expr.ToString() + ")"; }
    }
    internal partial class PrefixExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal PrefixExpr(Expr e, TokenType o) { Expr = e; Kind = o; }
        public override string ToString() { return "(" + TokenText(Kind) + Expr.ToString() + ")"; }
    }
    internal partial class PostfixExpr : Expr
    {
        internal Expr Expr;
        internal TokenType Kind;
        internal PostfixExpr(Expr e, TokenType o) { Expr = e; Kind = o; }
        public override string ToString() { return "(" + Expr.ToString() + TokenText(Kind) + ")"; }
    }
    internal partial class LiteralExpr : Expr
    {
        internal TokenType Kind;
        internal string Value;
        internal LiteralExpr(TokenType kind, string v) { Kind = kind; Value = v; }
        public override string ToString() { return "'" + Value + "'"; }
    }
    internal partial class SelfExpr : Expr
    {
        internal SelfExpr() { }
        public override string ToString() { return "(SELF)"; }
    }
    internal partial class SuperExpr : Expr
    {
        internal SuperExpr() { }
        public override string ToString() { return "(SUPER)"; }
    }
    internal partial class CheckedExpr : Expr
    {
        internal Expr Expr;
        internal CheckedExpr(Expr e) { Expr = e; }
        public override string ToString() { return "(CHECKED " + Expr.ToString() + ")"; }
    }
    internal partial class UncheckedExpr : Expr
    {
        internal Expr Expr;
        internal UncheckedExpr(Expr e) { Expr = e; }
        public override string ToString() { return "(UNCHECKED " + Expr.ToString() + ")"; }
    }
    internal partial class TypeOfExpr : Expr
    {
        internal TypeExpr Type;
        internal TypeOfExpr(TypeExpr t) { Type = t; }
        public override string ToString() { return "(TYPEOF " + Type.ToString() + ")"; }
    }
    internal partial class SizeOfExpr : Expr
    {
        internal TypeExpr Type;
        internal SizeOfExpr(TypeExpr t) { Type = t; }
        public override string ToString() { return "(SIZEOF " + Type.ToString() + ")"; }
    }
    internal partial class DefaultExpr : Expr
    {
        internal TypeExpr Type;
        internal DefaultExpr(TypeExpr t) { Type = t; }
        public override string ToString() { return "(DEFAULT " + Type.ToString() + ")"; }
    }
    internal partial class TypeCast : Expr
    {
        internal TypeExpr Type;
        internal Expr Expr;
        internal TypeCast(TypeExpr t, Expr e) { Type = t; Expr = e; }
        public override string ToString() { return "(" + Type.ToString() + ")" + Expr.ToString(); }
    }
    internal partial class TypeConversion : TypeCast
    {
        internal TypeConversion(TypeExpr t, Expr e) : base(t,e) { }
    }
    internal partial class IsExpr : Expr
    {
        internal Expr Expr;
        internal TypeExpr Type;
        internal IsExpr(Expr e, TypeExpr t) { Expr = e; Type = t; }
        public override string ToString() { return "(" + Expr.ToString() + " IS " + Type.ToString() + ")"; }
    }
    internal partial class AsTypeExpr : Expr
    {
        internal Expr Expr;
        internal TypeExpr Type;
        internal AsTypeExpr(Expr e, TypeExpr t) { Expr = e; Type = t; }
        public override string ToString() { return "(" + Expr.ToString() + " AS TYPE " + Type.ToString() + ")"; }
    }
    internal partial class MethodCallExpr : Expr
    {
        internal Expr Expr;
        internal ArgList Args;
        internal MethodCallExpr(Expr e, ArgList a) { Expr = e; Args = a; }
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
    internal partial class Arg : Node
    {
        internal Expr Expr;
        internal Arg(Expr e) { Expr = e; }
        public override string ToString() { return Expr.ToString(); }
    }
    internal partial class ExprList : Expr
    {
        internal IList<Expr> Exprs;
        internal ExprList(IList<Expr> e) { Exprs = e; }
        public override string ToString() { var sb = new StringBuilder(); foreach (var e in Exprs) { if (sb.Length > 0) sb.Append(", "); sb.Append(e.ToString()); } return sb.ToString(); }
    }
    internal partial class LiteralArray : Expr
    {
        internal ExprList Values;
        internal LiteralArray(ExprList values) { Values = values; }
        public override string ToString() { return "{" + Values.ToString() + "}"; }
    }
    internal partial class ArgList : Node
    {
        internal IList<Arg> Args;
        internal ArgList(IList<Arg> a) { Args = a; }
        public override string ToString() { var sb = new StringBuilder(); foreach (var a in Args) { if (sb.Length > 0) sb.Append(", "); sb.Append(a.ToString()); } return sb.ToString(); }
    }
    internal partial class Codeblock : Node
    {
        internal IList<IdExpr> Params;
        internal ExprList Body;
        internal Codeblock(IList<IdExpr> p, ExprList l) { Params = p; Body = l; }
        string ParamsAsString() { if (Params == null) return ""; var sb = new StringBuilder(); foreach (var p in Params) { if (sb.Length > 0) sb.Append(", "); sb.Append(p.ToString()); } return sb.ToString(); }
        public override string ToString() { return "{|" + ParamsAsString() + "|" + Body.ToString() + "}"; }
    }
}
