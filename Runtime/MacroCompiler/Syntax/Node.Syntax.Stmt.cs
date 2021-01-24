using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    internal partial class StmtBlock : Stmt
    {
        internal Stmt[] StmtList;
        internal StmtBlock(Token t, Stmt[] s) : base(t) { StmtList = s; }
        internal StmtBlock(Stmt[] s) : base(s.First().Token) { StmtList = s; }
        public override string ToString() => String.Join("\n", Array.ConvertAll(StmtList,(x) => x.ToString()));
    }
    internal partial class ExprStmt : Stmt
    {
        internal Expr Expr;
        internal ExprStmt(Token t, Expr e) : base(t) { Expr = e; }
        internal ExprStmt(Expr e) : this(e.Token, e) { }
        public override string ToString() => Expr.ToString();
    }
    internal partial class DeclStmt : Stmt
    {
        internal VarDecl[] VarDecls;
        internal DeclStmt(Token t, VarDecl[] v) : base(t) { VarDecls = v; }
        public override string ToString() => TokenText(Token.type) + " " + String.Join(", ", Array.ConvertAll(VarDecls, (x) => x.ToString()));
    }
    internal partial class VarDecl : Node
    {
        internal string Name;
        internal Expr[] ArraySub;
        internal TypeExpr Type;
        internal Expr Initializer;
        internal bool IsConst = false;
        internal bool IsDim = false;
        internal bool IsIsType = false;
        internal VarDecl(Token t, string n, Expr[] asub, TypeExpr a, Expr i) : base(t) { Name = n; ArraySub = asub; Type = a; Initializer = i; }
        internal VarDecl(Token t, Expr[] asub, TypeExpr a, Expr i) : this(t, t.value, asub, a, i) { }
        internal VarDecl(Token t, TypeExpr a) : this(t, t.value, null, a, null) { }
        internal VarDecl(Token t) : this(t, t.value, null, null, null) { }
        public override string ToString() => (IsConst?"CONST ":"") + (IsDim?"DIM ":"") + Name +
            (ArraySub?.Length > 0 ?"["+String.Join(", ",Array.ConvertAll(ArraySub,(x)=>x.ToString()))+"]":"") +
            (Initializer!=null?" := "+Initializer.ToString():"") +
            (Type!=null?(IsIsType?" IS ":" AS ")+Type.ToString():"");
    }
    internal partial class FieldDeclStmt : Stmt
    {
        internal Token[] Fields;
        internal Token Alias;
        internal FieldDeclStmt(Token[] fields, Token alias) : base(fields.First()) { Fields = fields; Alias = alias; }
        public override string ToString() => "FIELD " + String.Join(", ", Array.ConvertAll(Fields, (x) => x.value)) + Alias != null ? " IN " + Alias.value : "";
    }
    internal partial class EmptyStmt : Stmt
    {
        internal EmptyStmt(Token t) : base(t) { }
        public override string ToString() => "NOP";
    }
}
