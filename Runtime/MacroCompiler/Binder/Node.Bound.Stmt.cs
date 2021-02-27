using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using static System.Diagnostics.Debug;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    abstract internal partial class Stmt : Node
    {
        internal bool RequireExceptionHandling = false;
    }
    internal partial interface ILoopableStmt
    {
    }
    internal partial interface IExitableStmt
    {
    }
    internal partial class StmtBlock : Stmt
    {
        internal override Node Bind(Binder b)
        {
            for(int i  = 0; i < StmtList.Length; i++)
            {
                b.BindStmt(ref StmtList[i]);
            }
            return null;
        }
    }
    internal partial class ExprStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            return null;
        }
    }
    internal partial class ExprResultStmt : ExprStmt
    {
        internal Node TargetEntity = null;
        internal override Node Bind(Binder b)
        {
            if (Expr != null)
            {
                b.Bind(ref Expr);
                if (Expr.Datatype.NativeType != NativeType.Void)
                {
                    b.Convert(ref Expr, b.ObjectType);
                }
            }
            Symbol = b.ObjectType;
            if (RequireExceptionHandling || !(b.Entity is Codeblock))
            {
                if (b.Entity == null)
                    throw Error(ErrorCode.NoBindTarget);
                TargetEntity = b.Entity;
            }
            return null;
        }
    }
    internal partial class ReturnStmt : ExprResultStmt
    {
    }
    internal partial class DeclStmt : Stmt
    {
        // TODO
    }
    internal partial class VarDecl : Node
    {
        internal LocalSymbol Var;
        internal override Node Bind(Binder b)
        {
            if (Type != null)
            {
                b.Bind(ref Type, BindAffinity.Type);
                Type.RequireType();
                Var = b.AddLocal(Name, Type.Symbol as TypeSymbol) ?? throw Error(ErrorCode.LocalSameName, Name);
            }
            else
            {
                Var = b.AddLocal(Name, b.ObjectType);
            }
            if (Initializer != null)
            {
                b.Bind(ref Initializer);
                Initializer.RequireGetAccess();
                b.Convert(ref Initializer, Var.Type);
                Initializer = AssignExpr.Bound(IdExpr.Bound(Var), Initializer, b.Options.Binding);
            }
            return null;
        }
    }
    internal partial class ImpliedVarDecl : VarDecl
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Initializer);
            Initializer.RequireGetAccess();
            Var = b.AddLocal(Name, Initializer.Datatype) ?? throw Error(ErrorCode.LocalSameName, Name);
            Initializer = AssignExpr.Bound(IdExpr.Bound(Var), Initializer, b.Options.Binding);
            return null;
        }
    }
    internal partial class FieldDeclStmt : Stmt
    {
        // TODO
    }
    internal partial class EmptyStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            return null;
        }
    }
    internal partial class WhileStmt : Stmt, ILoopableStmt, IExitableStmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Cond);
            Cond.RequireGetAccess();
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
    }
    internal partial class RepeatStmt : WhileStmt, ILoopableStmt, IExitableStmt
    {
        internal override Node Bind(Binder b) => base.Bind(b);
    }
    internal partial class ForStmt : Stmt, ILoopableStmt, IExitableStmt
    {
        Expr WhileExpr;
        Expr IncrExpr;
        internal override Node Bind(Binder b)
        {
            b.OpenScope();

            if (AssignExpr != null)
            {
                b.Bind(ref AssignExpr);
            }
            else
            {
                b.Bind(ref ForDecl);
                AssignExpr = ForDecl.Initializer as AssignExpr;
            }

            Expr Iter;
            Iter = AssignExpr.Left;
            Iter.RequireGetAccess();

            b.Bind(ref Final);
            Final.RequireGetAccess();

            if (Step != null)
            {
                b.Bind(ref Step);
                Step.RequireGetAccess();
            }
            else
            {
                Step = LiteralExpr.Bound(Constant.Create(1));
            }

            switch (Dir.type)
            {
                case TokenType.UPTO:
                    WhileExpr = BinaryExpr.Bound(Iter, Dir, Final, BinaryOperatorKind.LessThanOrEqual, b.Options.Binding);
                    IncrExpr = AssignOpExpr.Bound(Iter, Step, BinaryOperatorKind.Addition, b);
                    break;
                case TokenType.DOWNTO:
                    WhileExpr = BinaryExpr.Bound(Iter, Dir, Final, BinaryOperatorKind.GreaterThanOrEqual, b.Options.Binding);
                    IncrExpr = AssignOpExpr.Bound(Iter, Step, BinaryOperatorKind.Subtraction, b);
                    break;
                case TokenType.TO:
                    var step_pos = BinaryExpr.Bound(Step, Dir, LiteralExpr.Bound(Constant.Create(0)), BinaryOperatorKind.GreaterThanOrEqual, b.Options.Binding);
                    var whileExprUpTo = BinaryExpr.Bound(Iter, Dir, Final, BinaryOperatorKind.LessThanOrEqual, b.Options.Binding);
                    var whileExprDownTo = BinaryExpr.Bound(Iter, Dir, Final, BinaryOperatorKind.GreaterThanOrEqual, b.Options.Binding);
                    WhileExpr = IifExpr.Bound(step_pos, whileExprUpTo, whileExprDownTo, b.Options.Binding);
                    IncrExpr = AssignOpExpr.Bound(Iter, Step, BinaryOperatorKind.Addition, b);
                    break;
                default:
                    throw Error(ErrorCode.Internal);
            }
            b.Convert(ref WhileExpr, Compilation.Get(NativeType.Boolean));

            b.Bind(ref Stmt);

            b.CloseScope();
            return null;
        }
    }
    internal partial class ForeachStmt : Stmt, ILoopableStmt, IExitableStmt
    {
        // TODO
    }
    internal partial class IfStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Cond);
            Cond.RequireGetAccess();
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            b.BindStmt(ref StmtIf);
            b.CloseScope();

            b.OpenScope();
            b.BindStmt(ref StmtElse);
            b.CloseScope();

            return null;
        }
    }
    internal partial class DoCaseStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            for(int i = 0; i < Cases.Length; i++)
                b.Bind(ref Cases[i]);
            b.OpenScope();
            b.Bind(ref Otherwise);
            b.CloseScope();
            return null;
        }
    }
    internal partial class CaseBlock : Node
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Cond);
            Cond.RequireGetAccess();
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
    }

    internal partial class SwitchStmt : Stmt
    {
        // TODO
    }
    internal partial class SwitchBlock : Node
    {
        // TODO
    }
    internal partial class SwitchBlockExpr : SwitchBlock
    {
        // TODO
    }
    internal partial class SwitchBlockType : SwitchBlock
    {
        // TODO
    }
    internal partial class ExitStmt : Stmt
    {
        internal IExitableStmt Outer;
        internal override Node Bind(Binder b)
        {
            Outer = b.FindOuter<IExitableStmt>() ?? throw Error(ErrorCode.NoExitableStatement);
            return null;
        }
    }
    internal partial class LoopStmt : Stmt
    {
        internal ILoopableStmt Outer;
        internal override Node Bind(Binder b)
        {
            Outer = b.FindOuter<ILoopableStmt>() ?? throw Error(ErrorCode.NoLoopableStatement);
            return null;
        }
    }
    internal partial class BreakStmt : Stmt
    {
        // TODO
    }
    internal partial class ThrowStmt : Stmt
    {
        // TODO
    }
    internal partial class QMarkStmt : Stmt
    {
        // TODO
    }
    internal partial class QQMarkStmt : QMarkStmt
    {
        // TODO
    }
    internal partial class TryStmt : Stmt
    {
        // TODO
    }
    internal partial class CatchBlock : Node
    {
        // TODO
    }
    internal partial class FinallyBlock : Node
    {
        // TODO
    }
    internal partial class SequenceStmt : Stmt
    {
        // TODO
    }

    internal partial class ScopeStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
    }

    internal partial class LockStmt : Stmt
    {
        // TODO
    }

    internal partial class UsingStmt : Stmt
    {
        // TODO
    }
    internal partial class FixedStmt : Stmt
    {
        // TODO
    }
    internal partial class Script : Node
    {
        internal override Node Bind(Binder b)
        {
            b.Entity = this;
            Symbol = b.ObjectType;
            if (Body != null)
            {
                b.BindStmt(ref Body);
            }
            return null;
        }
    }
}
