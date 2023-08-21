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
        internal static StmtBlock Bound(params Stmt[] s)
        {
            return new StmtBlock(s);
        }
    }
    internal partial class ExprStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            return null;
        }
        internal static ExprStmt Bound(Expr e)
        {
            return new ExprStmt(e);
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
                if (b.ResultType == null)
                {
                    b.ResultType = Expr.Datatype;
                }
                else if (!b.ResultType.IsVoid && !Expr.Datatype.IsVoid)
                {
                    b.Convert(ref Expr, b.ResultType);
                }
            }
            else if (b.ResultType == null)
            {
                b.ResultType = Compilation.Get(NativeType.Void);
            }
            if (!b.ResultType.IsVoid)
            {
                Symbol = b.ResultType;
                if (RequireExceptionHandling || !(b.Entity is Codeblock))
                {
                    if (b.Entity == null)
                        throw Error(ErrorCode.NoBindTarget);
                    TargetEntity = b.Entity;
                }
            }
            return null;
        }
    }
    internal partial class ReturnStmt : ExprResultStmt
    {
        internal override Node Bind(Binder b)
        {
            if (b.Entity is Script s && !s.AllowReturn)
                throw Error(ErrorCode.ReturnNotAllowed);
            return base.Bind(b);
        }
    }
    internal partial class DeclStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            if (Token.Type == TokenType.LPARAMETERS || Token.Type == TokenType.PARAMETERS)
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

            // TODO: Handle STATIC
            if (Token.Type == TokenType.STATIC)
                throw Error(ErrorCode.NotSupported, "STATIC");

            for (int i = 0; i < VarDecls.Length; i++)
            {
                b.Bind(ref VarDecls[i]);
            }
            return null;
        }
        internal static DeclStmt Bound(VarDecl v)
        {
            return new DeclStmt(null, new VarDecl[] { v });
        }
    }
    internal partial class VarDecl : Node
    {
        internal LocalSymbol Var => Symbol as LocalSymbol;
        internal override Node Bind(Binder b)
        {
            // TODO: Handle IS
            if (IsIsType)
                throw Type.Error(ErrorCode.NotSupported, "IS");

            // TODO: Handle DIM, array sub peroperly (according to full compiler)
            if (IsDim && ArraySub == null)
                throw Error(ErrorCode.Expected, "array specifier");
            bool isDim = IsDim && ArraySub != null;
            bool isArray = !IsDim && ArraySub != null;
            if (ArraySub != null)
            {
                for (int i = 0; i < ArraySub.Length; i++)
                {
                    b.Bind(ref ArraySub[i]);
                }
            }

            TypeSymbol t = b.ObjectType;
            if (Type != null)
            {
                b.Bind(ref Type, BindAffinity.Type);
                Type.RequireType();
                t = Type.Symbol as TypeSymbol;
            }
            if (isDim)
            {
                t = Binder.ArrayOf(t,ArraySub.Length);
            }
            else if (isArray && Type == null)
            {
                t = Compilation.Get(NativeType.Array);
            }
            Symbol = b.AddLocal(Name, t) ?? throw Error(ErrorCode.LocalSameName, Name);
            if (Initializer != null)
            {
                b.Bind(ref Initializer);
                Initializer.RequireGetAccess();
                if (IsConst)
                {
                    if (!Initializer.IsConstant)
                        throw Error(ErrorCode.ValueNotConst);
                    Var.SetConst();
                }
                b.Convert(ref Initializer, Var.Type);
                Initializer = InitExpr.Bound(IdExpr.Bound(Var), Initializer, b.Options.Binding);
            }
            else if (IsConst)
            {
                throw Error(ErrorCode.ConstWithoutInitializer);
            }
            else if (isDim)
            {
                Initializer = InitExpr.Bound(IdExpr.Bound(Var), CtorCallExpr.Bound(b, IdExpr.Bound(t), ArgList.Bound(ArraySub)), b.Options.Binding);
            }
            else if (isArray)
            {
                Initializer = InitExpr.Bound(IdExpr.Bound(Var), MethodCallExpr.Bound(b, null, Compilation.Get(WellKnownMembers.XSharp___Array___ArrayNew), null, ArgList.Bound(ArraySub)), b.Options.Binding);
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
            if (IsConst && Initializer is LiteralExpr c)
            {
                Symbol = b.AddConstant(Name, (Constant)c.Symbol) ?? throw Error(ErrorCode.LocalSameName, Name);
            }
            else
            {
                Symbol = b.AddLocal(Name, Initializer.Datatype) ?? throw Error(ErrorCode.LocalSameName, Name);
                if (IsConst)
                {
                    if (Initializer?.IsConstant != true)
                        throw Error(ErrorCode.ValueNotConst);
                    Var.SetConst();
                }
                Initializer = InitExpr.Bound(IdExpr.Bound(Var), Initializer, b.Options.Binding);
            }
            return null;
        }
    }
    internal partial class FieldDeclStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            foreach (var f in Fields)
            {
                b.AddFieldAlias(f.Value, Alias?.Value);
            }
            return null;
        }
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

            switch (Dir.Type)
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
        internal static IfStmt Bound(Expr cond, Stmt sTrue, Stmt sFalse)
        {
            return new IfStmt(null, cond, sTrue, sFalse);
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
    internal partial class ThrowStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            if (Expr != null)
            {
                b.Bind(ref Expr);
                Expr.RequireGetAccess();
                if (!Compilation.Get(WellKnownTypes.System_Exception).IsAssignableFrom(Expr.Datatype))
                    Expr.ThrowError(ErrorCode.TypeMustDeriveFrom, Compilation.Get(WellKnownTypes.System_Exception).FullName);
                b.Convert(ref Expr, Compilation.Get(NativeType.Object));
            }
            return null;
        }
    }
    internal partial class BreakStmt : ThrowStmt
    {
        internal override Node Bind(Binder b)
        {
            if (Expr != null)
            {
                b.Bind(ref Expr);
                Expr.RequireGetAccess();
            }
            else
            {
                if (b.Options.Dialect == XSharpDialect.FoxPro)
                    Expr = LiteralExpr.Bound(Constant.Create(false));
                else
                    Expr = LiteralExpr.Bound(Constant.CreateDefault(Compilation.Get(NativeType.Usual)));
            }
            var t = IdExpr.Bound(Compilation.Get(WellKnownTypes.XSharp_Internal_WrappedException));
            var args = ArgList.Bound(Expr);
            Expr = CtorCallExpr.Bound(b, t, args);
            b.Convert(ref Expr, Compilation.Get(NativeType.Object));
            return null;
        }
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
            Expr self, writeBack;
            var sym = b.BindMethodCall(expr, expr.Symbol, args, out self, out writeBack);
            if (self != null || writeBack != null)
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
        internal Node TargetEntity = null;
        internal override Node Bind(Binder b)
        {
            TargetEntity = b.Entity;
            b.OpenScope();
            b.Bind(ref Stmt);
            b.CloseScope();
            for (int i = 0; i < Catches.Length; i++)
                b.Bind(ref Catches[i]);
            if (Finally != null)
                b.Bind(ref Finally);
            return null;
        }
        internal static TryStmt Bound(Binder b, Stmt sb, Stmt fb)
        {
            return new TryStmt(null, sb, new CatchBlock[] { }, fb == null ? null : new FinallyBlock(null, fb)) { TargetEntity = b.Entity };
        }
        internal static TryStmt Bound(Binder b, Stmt sb, CatchBlock cb, Stmt fb)
        {
            if (cb == null)
                return Bound(b, sb, fb);
            return new TryStmt(null, sb, new CatchBlock[] { cb }, fb == null ? null : new FinallyBlock(null, fb)) { TargetEntity = b.Entity };
        }
    }
    internal partial class CatchBlock : Node
    {
        LocalSymbol ExVar;
        internal override Node Bind(Binder b)
        {
            // TODO: do not allow multiple unfiltered catch blocks of the same type
            b.OpenScope();
            if (Type != null)
            {
                b.Bind(ref Type, BindAffinity.Type);
                Type.RequireType();
                if (!Compilation.Get(WellKnownTypes.System_Exception).IsAssignableFrom(Type.Symbol as TypeSymbol))
                    Type.ThrowError(ErrorCode.TypeMustDeriveFrom, Compilation.Get(WellKnownTypes.System_Exception).FullName);
                ExVar = b.AddLocal(Name.Value, Type.Symbol as TypeSymbol);
            }
            else if (Name != null)
                ExVar = b.AddLocal(Name.Value, Compilation.Get(NativeType.Object));
            else
                ExVar = b.AddLocal(Compilation.Get(NativeType.Object));
            if (When != null)
            {
                // TODO: support filtered exceptions
                b.Bind(ref When);
                When.RequireGetAccess();
                When.ThrowError(ErrorCode.NotSupported, "WHEN");
            }
            b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
        internal static CatchBlock Bound(LocalSymbol exVar, Stmt s)
        {
            return s == null ? null : new CatchBlock(null, null, null, null, s) { ExVar = exVar };
        }
    }
    internal partial class FinallyBlock : Node
    {
        bool SaveAllowReturn(Binder b)
        {
            bool ar = false;
            if (b.Entity is Script s)
            {
                ar = s.AllowReturn;
                s.AllowReturn = false;
            }
            return ar;
        }
        void RestoreAllowReturn(Binder b, bool ar)
        {
            if (b.Entity is Script s)
                s.AllowReturn = ar;
        }
        internal override Node Bind(Binder b)
        {
            bool ar = SaveAllowReturn(b);
            b.OpenScope();
            b.Bind(ref Stmt);
            b.CloseScope();
            RestoreAllowReturn(b, ar);
            return null;
        }
    }
    internal partial class SequenceStmt : Stmt
    {
        LocalSymbol ExVar;
        bool SaveAllowReturn(Binder b)
        {
            bool ar = false;
            if (b.Entity is Script s)
            {
                ar = s.AllowReturn;
                s.AllowReturn = false;
            }
            return ar;
        }
        void RestoreAllowReturn(Binder b, bool ar)
        {
            if (b.Entity is Script s)
                s.AllowReturn = ar;
        }
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Stmt);
            Stmt = TryStmt.Bound(b,
                StmtBlock.Bound(
                    ExprStmt.Bound(MethodCallExpr.Bound(null, Compilation.Get(WellKnownMembers.XSharp_Internal_CompilerServices_EnterBeginSequence), null, ArgList.Empty)),
                    Stmt),
                ExprStmt.Bound(MethodCallExpr.Bound(null, Compilation.Get(WellKnownMembers.XSharp_Internal_CompilerServices_ExitBeginSequence), null, ArgList.Empty))
                );
            b.CloseScope();
            if (Recover != null)
            {
                b.OpenScope();
                var rv = b.AddLocal(Name.Value, b.ObjectType);
                b.Bind(ref Recover);
                ExVar = b.AddLocal(Compilation.Get(WellKnownTypes.System_Exception));
                Expr rvxw = MethodCallExpr.Bound(b, TypeCast.Bound(b, IdExpr.Bound(ExVar), Compilation.Get(WellKnownTypes.XSharp_Internal_WrappedException)), "get_Value", ArgList.Empty);
                Expr rvxe = TypeCast.Bound(b, IdExpr.Bound(ExVar), Compilation.Get(WellKnownTypes.XSharp_Error));
                Expr rvx = MethodCallExpr.Bound(null, Compilation.Get(WellKnownMembers.XSharp_Error_WrapRawException), null, ArgList.Bound(TypeCast.Bound(b, IdExpr.Bound(ExVar), Compilation.Get(WellKnownTypes.System_Exception))));
                var rvInit =
                    IifExpr.Bound(IsExpr.Bound(IdExpr.Bound(ExVar), IdExpr.Bound(Compilation.Get(WellKnownTypes.XSharp_Internal_WrappedException))),   TypeConversion.Bound(b, rvxw, Compilation.Get(NativeType.Usual)),
                        IifExpr.Bound(IsExpr.Bound(IdExpr.Bound(ExVar), IdExpr.Bound(Compilation.Get(WellKnownTypes.XSharp_Error))), TypeConversion.Bound(b, rvxe, Compilation.Get(NativeType.Usual)),
                            rvx,
                            b.Options.Binding),
                        b.Options.Binding);
                var rvdecl = DeclStmt.Bound(VarDecl.Bound(rv, rvInit, b.Options.Binding));
                Recover = StmtBlock.Bound(rvdecl, Recover);
                b.CloseScope();
            }
            if (Finally != null)
            {
                bool ar = SaveAllowReturn(b);
                b.OpenScope();
                b.Bind(ref Finally);
                b.CloseScope();
                RestoreAllowReturn(b, ar);
            }
            return TryStmt.Bound(b, Stmt, CatchBlock.Bound(ExVar, Recover), Finally);
        }
    }

    internal partial class ScopeStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            // TODO: UNSAFE
            if (Token.Type == TokenType.UNSAFE) throw Error(ErrorCode.NotSupported, Token.Type);

            // TODO: CHECKED
            if (Token.Type == TokenType.CHECKED) throw Error(ErrorCode.NotSupported, Token.Type);

            // TODO: UNCHECKED
            if (Token.Type == TokenType.UNCHECKED) throw Error(ErrorCode.NotSupported, Token.Type);

            b.OpenScope();
            b.Bind(ref Stmt);
            b.CloseScope();
            return null;
        }
    }

    internal partial class LockStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            b.Bind(ref Key);
            Key.RequireGetAccess();
            if (Key.Datatype.IsValueType)
                throw Error(ErrorCode.RequireReferenceType);
            b.Bind(ref Stmt);
            b.CloseScope();

            var k = b.AddLocal(Compilation.Get(NativeType.Object));
            b.Convert(ref Key, Compilation.Get(NativeType.Object));
            var kdecl = DeclStmt.Bound(VarDecl.Bound(k, Key, b.Options.Binding));

            var l = b.AddLocal(Compilation.Get(NativeType.Boolean));
            var ldecl = DeclStmt.Bound(VarDecl.Bound(l, LiteralExpr.Bound(Constant.Create(false)), b.Options.Binding));

            var enter = ExprStmt.Bound(MethodCallExpr.Bound(b, null, Compilation.Get(WellKnownMembers.System_Threading_Monitor_Enter), null, ArgList.Bound(IdExpr.Bound(k), IdExpr.Bound(l))));
            var exit = IfStmt.Bound(IdExpr.Bound(l),
                ExprStmt.Bound(MethodCallExpr.Bound(b, null, Compilation.Get(WellKnownMembers.System_Threading_Monitor_Exit), null, ArgList.Bound(IdExpr.Bound(k)))),
                null);

            return StmtBlock.Bound(kdecl, ldecl,
                TryStmt.Bound(b,
                    StmtBlock.Bound(enter, Stmt),
                    exit));
        }
    }

    internal partial class UsingStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            b.OpenScope();
            if (Expr != null)
            {
                b.Bind(ref Expr);
                Expr.RequireGetAccess();
            }
            else
                b.Bind(ref Decl);
            b.Bind(ref Stmt);
            b.CloseScope();

            var u = Decl != null ? Decl.VarDecls[0].Var : b.AddLocal(Expr.Datatype);
            var udecl = Decl ?? DeclStmt.Bound(VarDecl.Bound(u, Expr, b.Options.Binding));

            Expr du = AsTypeExpr.Bound(IdExpr.Bound(u), IdExpr.Bound(Compilation.Get(WellKnownTypes.System_IDisposable)));
            b.Cache(ref du);
            var cond = BinaryExpr.Bound(du, Expr.Token, LiteralExpr.Bound(Constant.Null), BinaryOperatorKind.NotEqual, b.Options.Binding);
            var exit = IfStmt.Bound(cond,
                ExprStmt.Bound(MethodCallExpr.Bound(b, null, Compilation.Get(WellKnownMembers.System_IDisposable_Dispose), du, ArgList.Empty)),
                null);

            return StmtBlock.Bound(udecl,
                TryStmt.Bound(b,
                    Stmt,
                    exit));
        }
    }
    internal partial class FixedStmt : Stmt
    {
        internal override Node Bind(Binder b)
        {
            // TODO: support FIXED when pointer support is added
            throw Error(ErrorCode.NotSupported, Token.Type);
        }
    }
    internal partial class Script : Node
    {
        internal LocalSymbol PCount;
        internal ArgumentSymbol ParamArray;
        internal bool AllowReturn = true;
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
