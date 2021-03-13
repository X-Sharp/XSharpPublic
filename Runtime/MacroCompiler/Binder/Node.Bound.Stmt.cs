using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using static System.Diagnostics.Debug;
using System.Dynamic;

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
        internal override Node Bind(Binder b)
        {
            if (Token.type == TokenType.LPARAMETERS || Token.type == TokenType.PARAMETERS)
            {
                for (int i = 0; i < VarDecls.Length; i++)
                {
                    var argIdx = b.Options.ArrayBase + i;
                    var paramArray = IdExpr.Bound((b.Entity as Script).ParamArray);
                    VarDecls[i].Initializer =
                        IifExpr.Bound(
                            BinaryExpr.Bound(ArrayLengthExpr.Bound(paramArray), Token, LiteralExpr.Bound(Constant.Create(i)), BinaryOperatorKind.GreaterThan, b.Options.Binding),
                            ArrayAccessExpr.Bound(paramArray, ArgList.Bound(LiteralExpr.Bound(Constant.Create(argIdx))), b),
                            DefaultExpr.Bound(b, b.ObjectType),
                            b.Options.Binding);
                }
            }

            for (int i = 0; i < VarDecls.Length; i++)
            {
                b.Bind(ref VarDecls[i]);
            }
            return null;
        }
    }
    internal partial class VarDecl : Node
    {
        internal LocalSymbol Var => Symbol as LocalSymbol;
        internal override Node Bind(Binder b)
        {
            if (Type != null)
            {
                b.Bind(ref Type, BindAffinity.Type);
                Type.RequireType();
                Symbol = b.AddLocal(Name, Type.Symbol as TypeSymbol) ?? throw Error(ErrorCode.LocalSameName, Name);
            }
            else
            {
                Symbol = b.AddLocal(Name, b.ObjectType);
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
        internal static VarDecl Bound(LocalSymbol loc, Expr initializer, BindOptions opt)
        {
            return new VarDecl(null, null, null, null, AssignExpr.Bound(IdExpr.Bound(loc), initializer, opt)) { Symbol = loc };
        }
    }
    internal partial class MemVarDecl : VarDecl
    {
        internal override Node Bind(Binder b)
        {
            b.CreatesAutoVars = true;
            Symbol = b.AddMemvar(Name);
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
            Symbol = b.AddLocal(Name, Initializer.Datatype) ?? throw Error(ErrorCode.LocalSameName, Name);
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
    internal partial class ForBaseStmt: Stmt, ILoopableStmt, IExitableStmt
    {
        protected VarDecl IterDecl = null;
        protected Expr IterInit = null;
        protected Expr WhileExpr = null;
        protected Expr IncrExpr = null;
        protected VarDecl InnerDecl = null;
        protected bool Dispose = false;
    }
    internal partial class ForStmt : ForBaseStmt, ILoopableStmt, IExitableStmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();

            if (AssignExpr != null)
            {
                b.Bind(ref AssignExpr);
                IterInit = AssignExpr;
            }
            else
            {
                b.Bind(ref ForDecl);
                IterDecl = ForDecl;
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
    internal partial class ForeachStmt : ForBaseStmt, ILoopableStmt, IExitableStmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();

            b.Bind(ref Expr);
            Expr.RequireGetAccess();

            if (Expr.Datatype.IsArray && Expr.Datatype.ArrayRank == 1)
            {
                var array = b.Cache(ref Expr);
                var iter = b.AddLocal(Compilation.Get(NativeType.Int32));
                IterDecl = VarDecl.Bound(iter, LiteralExpr.Bound(Constant.Create(b.Options.ArrayBase)), b.Options.Binding);
                WhileExpr = BinaryExpr.Bound(IdExpr.Bound(iter), Token,
                    MethodCallExpr.Bound(array, Compilation.Get(WellKnownMembers.System_Array_get_Length), array, ArgList.Empty),
                    b.Options.ArrayZero ? BinaryOperatorKind.LessThan : BinaryOperatorKind.LessThanOrEqual, b.Options.Binding);
                IncrExpr = AssignOpExpr.Bound(IdExpr.Bound(iter), LiteralExpr.Bound(Constant.Create(1)), BinaryOperatorKind.Addition, b);
                ForDecl.Initializer = ArrayAccessExpr.Bound(array, new ArgList(new List<Arg>(1) { new Arg(IdExpr.Bound(iter)) }), b);
            }
            else
            {
                if (Expr.Datatype.IsUsualOrObject() && b.Options.Binding.HasFlag(BindOptions.AllowDynamic))
                {
                    b.Convert(ref Expr, Compilation.Get(NativeType.Array));
                }
                Expr e = b.Cache(ref Expr);
                var getIterSym = e.Datatype.GetEnumeratorGetter() ?? throw Error(ErrorCode.NoSuitableEnumerator);
                var getIter = MethodCallExpr.Bound(e, getIterSym, e, ArgList.Empty);
                var iter = b.AddLocal(getIter.Datatype);
                IterDecl = VarDecl.Bound(iter, getIter, b.Options.Binding);
                WhileExpr = MethodCallExpr.Bound(b, IdExpr.Bound(iter), SystemNames.MoveNext, ArgList.Empty);
                b.Convert(ref WhileExpr, Compilation.Get(NativeType.Boolean));
                ForDecl.Initializer = MethodCallExpr.Bound(b, IdExpr.Bound(iter), SystemNames.CurrentGetter, ArgList.Empty);
                Dispose = Compilation.Get(WellKnownTypes.System_IDisposable).IsAssignableFrom(getIter.Datatype);
                if (Dispose) RequireExceptionHandling = true;
            }

            b.Bind(ref ForDecl);
            InnerDecl = ForDecl;

            Expr Iter;
            Iter = (ForDecl.Initializer as AssignExpr).Left;
            Iter.RequireGetAccess();

            b.Bind(ref Stmt);

            b.CloseScope();
            return null;
        }
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
        internal LocalSymbol SwitchValue;
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Expr);
            SwitchValue = b.AddLocal(Expr.Datatype);
            bool hasDefault = false;
            SwitchBlock next = null;
            for (var i = SwitchBlocks.Length-1; i >= 0; i--)
            {
                b.Bind(ref SwitchBlocks[i]);
                var sb = SwitchBlocks[i];
                if (!(sb is SwitchBlockExpr)&&!(sb is SwitchBlockType))
                {
                    if (hasDefault)
                        throw Error(ErrorCode.MultipleDefaultInSwitch, sb.Token);
                    hasDefault = true;
                }
                if (sb.FallThrough)
                    sb.NextBlock = next;
                else
                    next = sb;
            }
            b.CloseScope();
            return null;
        }
    }
    internal partial class SwitchBlock : Node
    {
        internal bool FallThrough => Stmt == null;
        internal SwitchBlock NextBlock = null;
        internal override Node Bind(Binder b)
        {
            if (Stmt != null)
            {
                b.OpenScope();
                b.Bind(ref Stmt);
                b.CloseScope();
            }
            return null;
        }
    }
    internal partial class SwitchBlockExpr : SwitchBlock
    {
        Expr Cond;
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
            var s = b.FindOuter<SwitchStmt>() ?? throw Error(ErrorCode.Internal);
            Cond = BinaryExpr.Bound(Expr, Expr.Token, IdExpr.Bound(s.SwitchValue), BinaryOperatorKind.ExactEqual, b.Options.Binding);
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            if (When != null)
            {
                b.Bind(ref When);
                When.RequireGetAccess();
                b.Convert(ref When, Compilation.Get(NativeType.Boolean));
            }
            if (Stmt != null)
                b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
    }
    internal partial class SwitchBlockType : SwitchBlock
    {
        Expr Cond;
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Type, BindAffinity.Type);
            Type.RequireType();
            var v = b.AddLocal(Type.Symbol as TypeSymbol);
            var s = b.FindOuter<SwitchStmt>() ?? throw Error(ErrorCode.Internal);
            Cond = IsVarExpr.Bound(IdExpr.Bound(s.SwitchValue), Type, v);
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            if (When != null)
            {
                b.Bind(ref When);
                When.RequireGetAccess();
                b.Convert(ref When, Compilation.Get(NativeType.Boolean));
            }
            if (Stmt != null)
                b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
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
        internal virtual string QOutName => XSharpFunctionNames.QOut;
        Expr QOutCall;
        internal override Node Bind(Binder b)
        {
            var funcs = Compilation.Get(WellKnownTypes.XSharp_RT_Functions);
            var expr = IdExpr.Bound(b.Lookup(funcs, QOutName));
            var args = new ArgList(new List<Arg>(Exprs.Select(x => new Arg(x))));
            b.Bind(ref args);
            Expr self;
            var sym = b.BindMethodCall(expr, expr.Symbol, args, out self);
            if (self != null)
                throw Error(ErrorCode.Internal);
            QOutCall = MethodCallExpr.Bound(expr, sym, null, args);
            return null;
        }
    }
    internal partial class QQMarkStmt : QMarkStmt
    {
        internal override string QOutName => XSharpFunctionNames.QQOut;
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
        internal LocalSymbol PCount;
        internal ArgumentSymbol ParamArray;
        internal override Node Bind(Binder b)
        {
            b.Entity = this;

            ParamArray = b.AddParam(XSharpSpecialNames.ClipperArgs, Binder.ArrayOf(b.ObjectType));
            //b.AddConstant(XSharpSpecialNames.ClipperArgCount, Constant.Create(Params?.Count ?? 0));
            PCount = b.AddLocal(XSharpSpecialNames.ClipperPCount, Compilation.Get(NativeType.Int32));

            Symbol = b.ObjectType;
            if (Body != null)
            {
                b.BindStmt(ref Body);
            }
            return null;
        }
    }
}
