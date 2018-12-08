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

    abstract internal partial class Node
    {
        internal Symbol Symbol = null;
        internal virtual Node Bind(Binder b) { throw new NotImplementedException(); }
    }
    abstract internal partial class Expr : Node
    {
        internal TypeSymbol Datatype = null;
        internal BindAffinity Affinity = BindAffinity.Access;
        internal virtual Expr Cloned(Binder b) { return this; }
    }
    abstract internal partial class TypeExpr : Expr
    {
    }
    abstract internal partial class NameExpr : TypeExpr
    {
    }
    internal partial class CachedExpr : Expr
    {
        internal LocalSymbol Local;
        internal Expr Expr;
        CachedExpr(Binder b, Expr e) : base(e.Token)
        {
            CompilerGenerated = true;
            Expr = e;
            Local = b.AddLocal(Expr.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
        }
        internal static CachedExpr Bound(Binder b, Expr expr)
        {
            return new CachedExpr(b, expr);
        }
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = Binder.GetNativeTypeFromToken(Kind);
            return null;
        }
    }
    internal partial class IdExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.Lookup(null, Name);
            if (Symbol == null)
            {
                switch (b.Options.UndeclaredVariableResolution)
                {
                    case VariableResolution.Error:
                        throw new NotImplementedException();
                    case VariableResolution.GenerateLocal:
                        Symbol = b.AddVariable(Name, Compilation.Get(NativeType.Usual));
                        break;
                    case VariableResolution.TreatAsField:
                        return AliasExpr.Bound(Name);
                    case VariableResolution.TreatAsFieldOrMemvar:
                        return AutoVarExpr.Bound(Name);
                }
            }
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName);
            if (Symbol == null)
            {
                if (Affinity == BindAffinity.Invoke)
                    Binder.Convert(ref Expr, Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
                else
                    Binder.Convert(ref Expr, Compilation.Get(NativeType.Object));
                Symbol = new DynamicSymbol(Member.LookupName);
            }
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
        internal override Expr Cloned(Binder b)
        {
            b.Cache(ref Expr);
            return this;
        }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
    }
    internal partial class AssignExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Binder.Convert(ref Right, Left.Datatype);
            Symbol = Left.Symbol;
            Datatype = Left.Datatype;
            return null;
        }
    }
    internal partial class AssignOpExpr : AssignExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            var r = new BinaryExpr(Left.Cloned(b), Token, Right);
            r.Symbol = Binder.BinaryOperation(BinaryOperatorSymbol.OperatorKind(Kind), ref r.Left, ref r.Right);
            r.Datatype = (r.Symbol as TypedSymbol)?.Type;
            Right = r;
            Binder.Convert(ref Right, Left.Datatype);
            Symbol = Left.Symbol;
            Datatype = Left.Datatype;
            return null;
        }
    }
    internal partial class BinaryExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            if (BinaryOperatorSymbol.OperatorIsLogic(Kind))
            {
                Binder.Convert(ref Left, Compilation.Get(NativeType.Boolean));
                Binder.Convert(ref Right, Compilation.Get(NativeType.Boolean));
            }
            Symbol = Binder.BinaryOperation(BinaryOperatorSymbol.OperatorKind(Kind), ref Left, ref Right);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
        internal static BinaryExpr Bound(Expr Left, BinaryOperatorKind kind, bool logic, Expr Right)
        {
            var e = new BinaryExpr(Left, Left.Token, Right);
            if (logic)
            {
                Binder.Convert(ref Left, Compilation.Get(NativeType.Boolean));
                Binder.Convert(ref Right, Compilation.Get(NativeType.Boolean));
            }
            e.Symbol = Binder.BinaryOperation(kind, ref e.Left, ref e.Right);
            e.Datatype = (e.Symbol as TypedSymbol)?.Type;
            return e;
        }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Binder.Convert(ref Left, Compilation.Get(NativeType.Boolean));
            Binder.Convert(ref Right, Compilation.Get(NativeType.Boolean));
            Symbol = Binder.BinaryOperation(BinaryOperatorSymbol.OperatorKind(Kind), ref Left, ref Right);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
    }
    internal partial class UnaryExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            if (UnaryOperatorSymbol.OperatorIsLogic(Kind))
            {
                Binder.Convert(ref Expr, Compilation.Get(NativeType.Boolean));
            }
            Symbol = Binder.UnaryOperation(UnaryOperatorSymbol.OperatorKind(Kind), ref Expr);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
        internal static UnaryExpr Bound(Expr expr, UnaryOperatorKind kind)
        {
            var s = Binder.UnaryOperation(kind, ref expr);
            return new UnaryExpr(expr, expr.Token) { Symbol = s, Datatype = s?.Type };
        }
    }
    internal partial class PrefixExpr : Expr
    {
        Expr Left;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Left = Expr.Cloned(b);
            Expr = UnaryExpr.Bound(Expr, UnaryOperatorSymbol.OperatorKind(Kind));
            Binder.Convert(ref Expr, Left.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
            return null;
        }
    }
    internal partial class PostfixExpr : Expr
    {
        Expr Left;
        Expr Value;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Left = Expr.Cloned(b);
            Value = b.Cache(ref Expr);
            Expr = UnaryExpr.Bound(Expr, UnaryOperatorSymbol.OperatorKind(Kind));
            Binder.Convert(ref Expr, Left.Datatype);
            Symbol = Value.Symbol;
            Datatype = Value.Datatype;
            return null;
        }
    }
    internal partial class LiteralExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.CreateLiteral(Kind, Value);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
        internal static LiteralExpr Bound(Constant c)
        {
            return new LiteralExpr(Token.None) { Symbol = c, Datatype = c.Type };
        }
    }
    internal partial class SelfExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw new CompileFailure(ErrorCode.NotSupported, "SELF keyword");
        }
    }
    internal partial class SuperExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw new CompileFailure(ErrorCode.NotSupported, "SELF keyword");
        }
    }
    internal partial class CheckedExpr : Expr
    {
    }
    internal partial class UncheckedExpr : Expr
    {
    }
    internal partial class TypeOfExpr : Expr
    {
    }
    internal partial class SizeOfExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Type);
            Symbol = Type.Symbol;
            Datatype = Compilation.Get(NativeType.UInt32);
            return null;
        }
    }
    internal partial class DefaultExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Type);
            Symbol = Type.Symbol;
            Datatype = Symbol as TypeSymbol;
            return null;
        }
    }
    internal partial class TypeCast : Expr
    {
        internal override Node Bind(Binder b)
        {
            Expr.Bind(b);
            Type.Bind(b);
            Datatype = Type.Symbol as TypeSymbol;
            Symbol = Binder.Conversion(Expr, Datatype, allowExplicit: true);
            return null;
        }
        internal static TypeCast Bound(Expr e, TypeSymbol t) { return new TypeCast(null, e) { Datatype = t }; }
    }
    internal partial class TypeConversion : TypeCast
    {
        internal static TypeConversion Bound(Expr e, TypeSymbol t, ConversionSymbol conv) { return new TypeConversion(null, e) { Datatype = t, Symbol = conv }; }
    }
    internal partial class IsExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Type);
            Symbol = Type.Symbol;
            Datatype = Compilation.Get(NativeType.Boolean);
            return null;
        }
    }
    internal partial class AsTypeExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Type);
            Symbol = Type.Symbol;
            Datatype = Type.Symbol as TypeSymbol;
            return null;
        }
    }
    internal partial class MethodCallExpr : Expr
    {
        protected Expr Self = null;
        internal override Node Bind(Binder b)
        {
            Expr.Affinity = BindAffinity.Invoke;
            b.Bind(ref Expr);
            b.Bind(ref Args);
            if (Expr.Symbol is DynamicSymbol)
            {
                Symbol = Binder.LookupFullName(XSharpQualifiedFunctionNames.InternalSend) ?? Binder.LookupFullName(VulcanQualifiedFunctionNames.InternalSend);
                var a = new List<Arg>(3);
                a.Add(new Arg((Expr as MemberAccessExpr)?.Expr));
                a.Add(new Arg(LiteralExpr.Bound(Constant.Create((Expr.Symbol as DynamicSymbol).Name))));
                a.Add(new Arg(LiteralArray.Bound(Args.Args)));
                Args = new ArgList(a);
            }
            else
            {
                Self = (Expr as MemberAccessExpr)?.Expr;
                Symbol = b.BindCall(Self, Expr.Symbol, Args);
            }
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Args);
            Symbol = b.BindCall(null, Expr.Symbol.Lookup(".ctor"), Args);
            // TODO: nvk: Symbol can be null for value-type default .ctor (add ObjectInitSymbol?)
            Datatype = (Symbol as TypedSymbol)?.Type ?? (Expr.Symbol as TypeSymbol);
            return null;
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Args);
            Binder.Convert(ref Expr, Compilation.Get(NativeType.Array));
            if (!b.Options.ArrayZero)
                Args.ConvertArrayBase1();
            Self = Expr;
            var s = Self.Datatype.Lookup(SystemNames.IndexerName);
            Symbol = b.BindArrayAccess(Self, s, Args);
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
        internal override Expr Cloned(Binder b)
        {
            b.Cache(ref Expr);
            foreach (var arg in Args.Args) b.Cache(ref arg.Expr);
            return this;
        }
    }
    internal partial class EmptyExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = Constant.CreateDefault(Compilation.Get(NativeType.Usual));
            Datatype = (Symbol as TypedSymbol)?.Type;
            return null;
        }
    }
    internal partial class ExprList : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(Exprs);
            var e = Exprs.LastOrDefault();
            if (e != null)
            {
                Symbol = e.Symbol;
                Datatype = e.Datatype;
            }
            else
            {
                Datatype = Compilation.Get(NativeType.Void);
            }
            return null;
        }
    }
    internal partial class LiteralArray : Expr
    {
        internal override Node Bind(Binder b)
        {
            if (ElemType != null) b.Bind(ref ElemType);
            b.Bind(ref Values);
            if (ElemType != null)
                Convert(ElemType.Symbol as TypeSymbol);
            else
                Convert(Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object),
                    Compilation.Get(WellKnownTypes.XSharp___Array));
            return null;
        }
        internal static LiteralArray Bound(IList<Expr> values, TypeSymbol type = null)
        {
            var e = new LiteralArray(new ExprList(values));
            e.Convert(type ?? Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
            return e;
        }
        internal static LiteralArray Bound(IList<Arg> args, TypeSymbol type = null)
        {
            var values = new List<Expr>(args.Count);
            foreach(var a in args) values.Add(a.Expr);
            return Bound(values, type);
        }
        private void Convert(TypeSymbol et, TypeSymbol dt = null)
        {
            for (int i = 0; i < Values.Exprs.Count; i++)
            {
                var v = Values.Exprs[i];
                Binder.Convert(ref v, et);
                Values.Exprs[i] = v;
            }
            Symbol = et;
            Datatype = dt ?? Binder.ArrayOf(et);
        }
    }
    internal partial class IifExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Cond);
            b.Bind(ref True);
            b.Bind(ref False);
            Binder.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            Datatype = Binder.ConvertResult(ref True, ref False);
            return null;
        }
    }
    internal partial class AliasExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            if (Alias != null)
            {
                b.Bind(ref Alias);
                Binder.Convert(ref Alias, Compilation.Get(NativeType.String));
            }
            b.Bind(ref Field);
            Binder.Convert(ref Field, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
        internal static AliasExpr Bound(string fieldName)
        {
            return new AliasExpr(null, LiteralExpr.Bound(Constant.Create(fieldName)), Token.None) { Datatype = Compilation.Get(NativeType.Usual) };
        }
    }
    internal partial class AutoVarExpr : Expr
    {
        internal Expr Var;
        AutoVarExpr(Expr var) : base(var.Token) { Var = var; }
        public override string ToString() { return "{Var:" + Var.ToString() + "}"; }
        internal static AutoVarExpr Bound(string varName)
        {
            return new AutoVarExpr(LiteralExpr.Bound(Constant.Create(varName))) { Datatype = Compilation.Get(NativeType.Usual) };
        }
    }
    internal partial class Arg : Node
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            return null;
        }
    }
    internal partial class ArgList : Node
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(Args);
            return null;
        }
        internal void ConvertArrayBase1()
        {
            for (int i = 0; i < Args.Count; i++)
            {
                Args[i].Expr = BinaryExpr.Bound(Args[i].Expr, BinaryOperatorKind.Subtraction, false, LiteralExpr.Bound(Constant.Create(1)));
            }
        }
    }
    internal partial class Codeblock : Node
    {
        ArgumentSymbol ParamArray;
        internal override Node Bind(Binder b)
        {
            if (Params != null)
            {
                ParamArray = b.AddParam(Binder.ArrayOf(b.ObjectType));
                foreach (var p in Params)
                {
                    b.AddLocal(p.LookupName, b.ObjectType);
                    p.Bind(b);
                }
                // TODO: nvk: generate pcount?
            }
            if (Body != null)
            {
                b.Bind(ref Body);
                if (Body.Datatype.NativeType != NativeType.Void)
                {
                    Expr e = Body.Exprs.Last();
                    Binder.Convert(ref e, b.ObjectType);
                    Body.Exprs[Body.Exprs.Count - 1] = e;
                }
            }
            Symbol = b.ObjectType;
            return null;
        }
    }
}