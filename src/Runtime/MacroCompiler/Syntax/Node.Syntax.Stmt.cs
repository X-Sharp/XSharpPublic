using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    abstract internal partial class Stmt : Node
    {
        internal Stmt(Token t) : base(t) { }
    }
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
    internal partial class ExprResultStmt : ExprStmt
    {
        internal ExprResultStmt(Token t, Expr e) : base(t, e) { }
        internal ExprResultStmt(Expr e) : base(e.Token, e) { }
    }
    internal partial class ReturnStmt : ExprResultStmt
    {
        internal ReturnStmt(Token t, Expr e) : base(t, e) { }
        internal ReturnStmt(Expr e) : base(e) { }
        public override string ToString() { return "RETURN " + Expr?.ToString(); }
    }
    internal partial class DeclStmt : Stmt
    {
        internal VarDecl[] VarDecls;
        internal DeclStmt(Token t, VarDecl[] v) : base(t) { VarDecls = v; }
        public override string ToString() => TokenText(Token.Type) + " " + String.Join(", ", Array.ConvertAll(VarDecls, (x) => x.ToString()));
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
        internal VarDecl(Token t, Expr[] asub, TypeExpr a, Expr i) : this(t, t.Value, asub, a, i) { }
        internal VarDecl(Token t, TypeExpr a) : this(t, t.Value, null, a, null) { }
        internal VarDecl(Token t) : this(t, t.Value, null, null, null) { }
        public override string ToString() => (IsConst?"CONST ":"") + (IsDim?"DIM ":"") + Name +
            (ArraySub?.Length > 0 ?"["+String.Join(", ",Array.ConvertAll(ArraySub,(x)=>x.ToString()))+"]":"") +
            (Initializer!=null?" := "+Initializer.ToString():"") +
            (Type!=null?(IsIsType?" IS ":" AS ")+Type.ToString():"");
    }
    internal partial class MemVarDecl : VarDecl
    {
        internal MemVarDecl(Token t, Expr i) : base(t, null, null, i) { }
        internal MemVarDecl(Token t, Expr[] asub, TypeExpr a, Expr i) : base(t, asub, a, i) { }
    }
    internal partial class ImpliedVarDecl : VarDecl
    {
        internal ImpliedVarDecl(Token t, Expr i) : base(t, null, null, i) { }
        public override string ToString() => (IsConst ? "CONST " : "") + Name + (Initializer != null ? " := " + Initializer.ToString() : "");
    }
    internal partial class FieldDeclStmt : Stmt
    {
        internal Token[] Fields;
        internal Token Alias;
        internal FieldDeclStmt(Token[] fields, Token alias) : base(fields.First()) { Fields = fields; Alias = alias; }
        public override string ToString() => "FIELD " + String.Join(", ", Array.ConvertAll(Fields, (x) => x.Value)) + Alias != null ? " IN " + Alias.Value : "";
    }
    internal partial class EmptyStmt : Stmt
    {
        internal EmptyStmt(Token t) : base(t) { }
        public override string ToString() => "NOP";
    }
    internal partial class WhileStmt : Stmt
    {
        internal Expr Cond;
        internal Stmt Stmt;
        internal WhileStmt(Token t, Expr cond, Stmt s) : base(t) { Cond = cond; Stmt = s; }
        public override string ToString() => "DO WHILE " + Cond.ToString() + "\n  " + Stmt.ToString().Replace("\n","\n  ") + "\nENDDO";
    }
    internal partial class RepeatStmt : WhileStmt
    {
        internal RepeatStmt(Token t, Expr cond, Stmt s) : base(t, cond, s) { }
        public override string ToString() => "REPEAT\n  " + Stmt.ToString().Replace("\n", "\n  ") + "\nUNTIL " + Cond.ToString();
    }
    internal partial class ForBaseStmt : Stmt
    {
        internal ForBaseStmt(Token t) : base(t) { }
    }
    internal partial class ForBaseStmt : Stmt
    {
        internal Stmt Stmt;
    }
    internal partial class ForStmt : ForBaseStmt
    {
        internal AssignExpr AssignExpr;
        internal VarDecl ForDecl;
        internal Token Dir;
        internal Expr Final;
        internal Expr Step;
        private ForStmt(Token t, Token dir, Expr final, Expr step, Stmt s) : base(t) { ForDecl = null; AssignExpr = null;  Dir = dir; Final = final; Step = step; Stmt = s; }
        internal ForStmt(Token t, AssignExpr a, Token dir, Expr final, Expr step, Stmt s) : this(t, dir, final, step, s) { AssignExpr = a; }
        internal ForStmt(Token t, VarDecl d, Token dir, Expr final, Expr step, Stmt s) : this(t, dir, final, step, s) { ForDecl = d; }
        public override string ToString() => "FOR " + (AssignExpr?.ToString() ?? ((ForDecl is ImpliedVarDecl ? "VAR " : "LOCAL ") + ForDecl.ToString())) + " " + Dir.Type + " " + Final + (Step != null ? " STEP " + Step : "") + "\n  " + Stmt.ToString().Replace("\n", "\n  ") + "\nEND FOR";
    }
    internal partial class ForeachStmt : ForBaseStmt
    {
        internal VarDecl ForDecl;
        internal Expr Expr;
        internal ForeachStmt(Token t, VarDecl d, Expr e, Stmt s) : base(t) { ForDecl = d; Expr = e; Stmt = s; }
        public override string ToString() => "FOREACH " + (ForDecl is ImpliedVarDecl ? "VAR " : "") + ForDecl.ToString() + " IN " + Expr + "\n  " + Stmt.ToString().Replace("\n", "\n  ") + "\nEND FOREACH";
    }
    internal partial class IfStmt : Stmt
    {
        internal Expr Cond;
        internal Stmt StmtIf;
        internal Stmt StmtElse;
        internal IfStmt(Token t, Expr cond, Stmt si, Stmt se) : base(t) { Cond = cond; StmtIf = si; StmtElse = se; }
        public override string ToString() => "IF " + Cond.ToString() + "\n  " + StmtIf.ToString().Replace("\n", "\n  ") + (StmtElse != null ? "\nELSE\n  " + StmtElse.ToString().Replace("\n", "\n  ") : "") + "\nENDIF";
    }
    internal partial class DoCaseStmt : Stmt
    {
        internal CaseBlock[] Cases;
        internal Stmt Otherwise;
        internal DoCaseStmt(Token t, CaseBlock[] cases, Stmt otherwise) : base(t) { Cases = cases; Otherwise = otherwise; }
        public override string ToString() => "DO CASE\n" + String.Join("\n", Array.ConvertAll(Cases, (x) => x.ToString())) + (Otherwise != null ? "\nOTHERWISE\n  " + Otherwise.ToString().Replace("\n", "\n  ") : "") + "\nEND CASE";
    }
    internal partial class CaseBlock : Node
    {
        internal Expr Cond;
        internal Stmt Stmt;
        internal CaseBlock(Token t, Expr cond, Stmt s) : base(t) { Cond = cond; Stmt = s; }
        public override string ToString() => "CASE " + Cond.ToString() + "\n  " + Stmt.ToString().Replace("\n", "\n  ");
    }

    internal partial class SwitchStmt : Stmt
    {
        internal Expr Expr;
        internal SwitchBlock[] SwitchBlocks;
        internal SwitchStmt(Token t, Expr e, SwitchBlock[] sw) : base(t) { Expr = e; SwitchBlocks = sw; }
        public override string ToString() => "SWITCH\n" + String.Join("\n", Array.ConvertAll(SwitchBlocks, (x) => x.ToString())) + "\nEND SWITCH";
    }
    internal partial class SwitchBlock : Node
    {
        internal Stmt Stmt;
        internal SwitchBlock(Token t, Stmt s) : base(t) { Stmt = s; }
        public override string ToString() => Token.Type + "\n  " + Stmt?.ToString().Replace("\n", "\n  ");
    }
    internal partial class SwitchBlockExpr : SwitchBlock
    {
        internal Expr Expr;
        internal Expr When;
        internal SwitchBlockExpr(Token t, Expr e, Expr when, Stmt s) : base(t, s) { Expr = e; When = when; }
        public override string ToString() => Token.Type + " " + Expr.ToString() + (When != null ? "WHEN " + When : "") + "\n  " + Stmt?.ToString().Replace("\n", "\n  ");
    }
    internal partial class SwitchBlockType : SwitchBlock
    {
        Token Name;
        internal TypeExpr Type;
        internal Expr When;
        internal SwitchBlockType(Token t, Token n, TypeExpr type, Expr when, Stmt s) : base(t, s) { Name = n;  Type = type; When = when; }
        public override string ToString() => Token.Type + " " + Name + " AS " + Type + (When != null ? "WHEN " + When : "") + "\n  " + Stmt?.ToString().Replace("\n", "\n  ");
    }

    internal partial class ExitStmt : Stmt
    {
        internal ExitStmt(Token t) : base(t) { }
        public override string ToString() => "EXIT";
    }
    internal partial class LoopStmt : Stmt
    {
        internal LoopStmt(Token t) : base(t) { }
        public override string ToString() => "LOOP";
    }
    internal partial class ThrowStmt : Stmt
    {
        internal Expr Expr;
        internal ThrowStmt(Token t, Expr e) : base(t) { Expr = e; }
        public override string ToString() => "THROW" + (Expr != null ? " " + Expr.ToString() : "") + "\n";
    }
    internal partial class BreakStmt : ThrowStmt
    {
        internal BreakStmt(Token t, Expr e) : base(t, e) { }
        public override string ToString() => "BREAK" + (Expr != null ? " " + Expr.ToString() : "") + "\n";
    }
    internal partial class QMarkStmt : Stmt
    {
        internal Expr[] Exprs;
        internal QMarkStmt(Token t, Expr[] e) : base(t) { Exprs = e; }
        public override string ToString() => "? " + (Exprs != null ? String.Join(", ", Array.ConvertAll(Exprs, (x) => x.ToString())) : "") + "\n";
    }
    internal partial class QQMarkStmt : QMarkStmt
    {
        internal QQMarkStmt(Token t, Expr[] e) : base(t, e) { }
        public override string ToString() => "?" + base.ToString();
    }
    internal partial class TryStmt : Stmt
    {
        internal Stmt Stmt;
        internal CatchBlock[] Catches;
        internal FinallyBlock Finally;
        internal TryStmt(Token t, Stmt s, CatchBlock[] cb, FinallyBlock fb) : base(t) { Stmt = s; Catches = cb; Finally = fb; }
        public override string ToString() => "TRY\n  " + Stmt.ToString().Replace("\n", "\n  ") + (Catches != null ? String.Join("\n", Array.ConvertAll(Catches, (x) => x.ToString())) : "") + Finally?.ToString();
    }
    internal partial class CatchBlock : Node
    {
        internal Token Name;
        internal TypeExpr Type;
        internal Expr When;
        internal Stmt Stmt;
        internal CatchBlock(Token t, Token name, TypeExpr type, Expr when, Stmt s) : base(t) { Name = name; Type = type; When = when; Stmt = s; }
        public override string ToString() => "CATCH" + "\n  " + Stmt.ToString().Replace("\n", "\n  ");
    }
    internal partial class FinallyBlock : Node
    {
        internal Stmt Stmt;
        internal FinallyBlock(Token t, Stmt s) : base(t) { Stmt = s; }
        public override string ToString() => "FINALLY\n  " + Stmt.ToString().Replace("\n", "\n  ");
    }
    internal partial class SequenceStmt : Stmt
    {
        internal Stmt Stmt;
        internal Token Name;
        internal StmtBlock Recover;
        internal StmtBlock Finally;
        internal SequenceStmt(Token t, Stmt s, Token n, StmtBlock r, StmtBlock f) : base(t) { Stmt = s; Name = n; Recover = r; Finally = f; }
        public override string ToString() => "BEGIN SEQUENCE\n  " + Stmt.ToString().Replace("\n", "\n  ") +
            (Recover != null ? "RECOVER USING " + Name + "\n  " + String.Join("\n", Recover.ToString()) : "") +
            (Finally != null ? "FINALLY\n  " + String.Join("\n", Finally.ToString()) : "") +
            "END SEQUENCE";
    }

    internal partial class ScopeStmt : Stmt
    {
        internal Stmt Stmt;
        internal ScopeStmt(Token t, Stmt s) : base(t) { Stmt = s; }
        public override string ToString() => "BEGIN " + TokenText(Token.Type) + "\n  " + String.Join("\n", Stmt.ToString()) + "END " + TokenText(Token.Type);
    }

    internal partial class LockStmt : Stmt
    {
        internal Stmt Stmt;
        internal Expr Key;
        internal LockStmt(Token t, Expr key, Stmt s) : base(t) { Key = key; Stmt = s; }
        public override string ToString() => "BEGIN LOCK " + Key + "\n  " +  String.Join("\n", Stmt.ToString()) + "END LOCK";
    }

    internal partial class UsingStmt : Stmt
    {
        internal Stmt Stmt;
        internal Expr Expr = null;
        internal DeclStmt Decl = null;
        internal UsingStmt(Token t, Expr e, Stmt s) : base(t) { Expr = e; Stmt = s; }
        internal UsingStmt(Token t, DeclStmt decl, Stmt s) : base(t) { Decl = decl; Stmt = s; }
        public override string ToString() => "BEGIN USING " + (Decl != null ? Decl.ToString() : Expr.ToString()) + "\n  " + String.Join("\n", Stmt.ToString()) + "END USING";
    }
    internal partial class FixedStmt : Stmt
    {
        internal Stmt Stmt;
        internal DeclStmt Decl = null;
        internal FixedStmt(Token t, DeclStmt decl, Stmt s) : base(t) { Decl = decl; Stmt = s; }
        public override string ToString() => "BEGIN FIXED " + Decl + "\n  " + String.Join("\n", Stmt.ToString()) + "END FIXED";
    }
    internal partial class Script : Node
    {
        internal Stmt Body;
        internal Script(Token t, Stmt s) : base(t) { Body = s; }
        public override string ToString() => Body.ToString();
    }
}
