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
    internal partial class ImpliedVarDecl : VarDecl
    {
        internal ImpliedVarDecl(Token t, Expr i) : base(t, null, null, i) { }
        public override string ToString() => (IsConst ? "CONST " : "") + Name + " := " + Initializer.ToString();
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
    internal partial class WhileStmt : Stmt
    {
        Expr Cond;
        Stmt Stmt;
        internal WhileStmt(Token t, Expr cond, Stmt s) : base(t) { Cond = cond; Stmt = s; }
        public override string ToString() => "WHILE " + Cond.ToString() + "\n  " + Stmt.ToString().Replace("\n","\n  ") + "\nEND WHILE";
    }
    internal partial class ForStmt : Stmt
    {
        AssignExpr AssignExpr;
        VarDecl ForDecl;
        Token Dir;
        Expr Final;
        Expr Step;
        Stmt Stmt;
        private ForStmt(Token t, Token dir, Expr final, Expr step, Stmt s) : base(t) { ForDecl = null; AssignExpr = null;  Dir = dir; Final = final; Step = step; Stmt = s; }
        internal ForStmt(Token t, AssignExpr a, Token dir, Expr final, Expr step, Stmt s) : this(t, dir, final, step, s) { AssignExpr = a; }
        internal ForStmt(Token t, VarDecl d, Token dir, Expr final, Expr step, Stmt s) : this(t, dir, final, step, s) { ForDecl = d; }
        public override string ToString() => "FOR " + (AssignExpr?.ToString() ?? ((ForDecl is ImpliedVarDecl ? "VAR " : "LOCAL ") + ForDecl.ToString())) + " " + Dir.type + " " + Final + (Step != null ? " STEP " + Step : "") + "\n  " + Stmt.ToString().Replace("\n", "\n  ") + "\nEND FOR";
    }
    internal partial class IfStmt : Stmt
    {
        Expr Cond;
        Stmt StmtIf;
        Stmt StmtElse;
        internal IfStmt(Token t, Expr cond, Stmt si, Stmt se) : base(t) { Cond = cond; StmtIf = si; StmtElse = se; }
        public override string ToString() => "IF " + Cond.ToString() + "\n  " + StmtIf.ToString().Replace("\n", "\n  ") + (StmtElse != null ? "\nELSE\n  " + StmtElse.ToString().Replace("\n", "\n  ") : "") + "\nEND WHILE";
    }
    internal partial class DoCaseStmt : Stmt
    {
        CaseBlock[] Cases;
        Stmt Otherwise;
        internal DoCaseStmt(Token t, CaseBlock[] cases, Stmt otherwise) : base(t) { Cases = cases; Otherwise = otherwise; }
        public override string ToString() => "DO CASE\n" + String.Join("\n", Array.ConvertAll(Cases, (x) => x.ToString())) + (Otherwise != null ? "\nOTHERWISE\n  " + Otherwise.ToString().Replace("\n", "\n  ") : "") + "\nEND CASE";
    }
    internal partial class CaseBlock : Node
    {
        Expr Cond;
        Stmt Stmt;
        internal CaseBlock(Token t, Expr cond, Stmt s) : base(t) { Cond = cond; Stmt = s; }
        public override string ToString() => "CASE " + Cond.ToString() + "\n  " + Stmt.ToString().Replace("\n", "\n  ");
    }
}
