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
        internal virtual Node Bind(Binder b) { throw new InternalError(); }
        internal CompilationError Error(ErrorCode e, params object[] args) => Compilation.Error(Token, e, args);
    }
    abstract internal partial class Expr : Node
    {
        internal TypeSymbol Datatype = null;
        internal BindAffinity Affinity = BindAffinity.Access;
        internal virtual Expr Cloned(Binder b) { return this; }
        internal TypeSymbol ThrowError(ErrorCode e, params object[] args) { throw Error(e, args); }
        internal TypeSymbol ThrowError(CompilationError e) { throw e; }
        internal void RequireValue()
        {
            if (Datatype == null)
                ThrowError(ErrorCode.NotAnExpression, Symbol);
        }
        internal void RequireType()
        {
            if (!(Symbol is TypeSymbol))
                ThrowError(ErrorCode.NotAType, Symbol);
        }
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
            Symbol = Binder.GetNativeTypeFromToken(Kind) ?? ThrowError(ErrorCode.NotSupported,Kind);
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
                        throw Error(ErrorCode.IdentifierNotFound, Name);
                    case VariableResolution.GenerateLocal:
                        Symbol = b.AddVariable(Name, Compilation.Get(NativeType.Usual));
                        break;
                    case VariableResolution.TreatAsField:
                        return AliasExpr.Bound(Name);
                    case VariableResolution.TreatAsFieldOrMemvar:
                        return AutoVarExpr.Bound(Name);
                }
            }
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName);
            if (Symbol == null)
            {
                if (Affinity == BindAffinity.Invoke)
                    b.Convert(ref Expr, Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
                else
                    b.Convert(ref Expr, Compilation.Get(NativeType.Object));
                Symbol = new DynamicSymbol(Member.LookupName);
            }
            Datatype = Symbol.Type();
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
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName) ?? ThrowError(Binder.LookupError(Expr, this));
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class AssignExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Left.RequireValue();
            Right.RequireValue();
            b.Convert(ref Right, Left.Datatype);
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
            Left.RequireValue();
            Right.RequireValue();
            Right = BinaryExpr.Bound(Left.Cloned(b), Token, Right, BinaryOperatorSymbol.OperatorKind(Kind), false, b.Options.Binding);
            b.Convert(ref Right, Left.Datatype);
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
            Left.RequireValue();
            Right.RequireValue();
            if (BinaryOperatorSymbol.OperatorIsLogic(Kind))
                Symbol = b.BindBinaryLogicOperation(this, BinaryOperatorSymbol.OperatorKind(Kind));
            else
                Symbol = b.BindBinaryOperation(this, BinaryOperatorSymbol.OperatorKind(Kind));
            Datatype = Symbol.Type();
            return null;
        }
        internal static BinaryExpr Bound(Expr Left, Token t, Expr Right, BinaryOperatorKind kind, bool logic, BindOptions options)
        {
            var e = new BinaryExpr(Left, t, Right);
            e.Symbol = Binder.BindBinaryOperation(e, kind, options);
            e.Datatype = e.Symbol.Type();
            return e;
        }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Left.RequireValue();
            Right.RequireValue();
            Symbol = b.BindBinaryLogicOperation(this, BinaryOperatorSymbol.OperatorKind(Kind));
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class UnaryExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
            if (UnaryOperatorSymbol.OperatorIsLogic(Kind))
                Symbol = b.BindUnaryLogicOperation(this, UnaryOperatorSymbol.OperatorKind(Kind));
            else
                Symbol = b.BindUnaryOperation(this, UnaryOperatorSymbol.OperatorKind(Kind));
            Datatype = Symbol.Type();
            return null;
        }
        internal static UnaryExpr Bound(Expr expr, UnaryOperatorKind kind, BindOptions options)
        {
            var e = new UnaryExpr(expr, expr.Token);
            e.Symbol = Binder.BindUnaryOperation(e, kind, options);
            e.Datatype = e.Symbol.Type();
            return e;
        }
    }
    internal partial class PrefixExpr : UnaryExpr
    {
        Expr Left;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
            Left = Expr.Cloned(b);
            Expr = Bound(Expr, UnaryOperatorSymbol.OperatorKind(Kind), b.Options.Binding);
            b.Convert(ref Expr, Left.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
            return null;
        }
    }
    internal partial class PostfixExpr : UnaryExpr
    {
        Expr Left;
        Expr Value;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
            Left = Expr.Cloned(b);
            Value = b.Cache(ref Expr);
            Expr = Bound(Expr, UnaryOperatorSymbol.OperatorKind(Kind), b.Options.Binding);
            b.Convert(ref Expr, Left.Datatype);
            Symbol = Value.Symbol;
            Datatype = Value.Datatype;
            return null;
        }
    }
    internal partial class LiteralExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.CreateLiteral(this, Value);
            Datatype = Symbol.Type();
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
            throw Error(ErrorCode.NotSupported, "SELF keyword");
        }
    }
    internal partial class SuperExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw Error(ErrorCode.NotSupported, "SUPER keyword");
        }
    }
    internal partial class CheckedExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw Error(ErrorCode.NotImplemented, "CHECKED expression");
        }
    }
    internal partial class UncheckedExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw Error(ErrorCode.NotImplemented, "UNCHECKED expression");
        }
    }
    internal partial class TypeOfExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw Error(ErrorCode.NotImplemented, "TYPEOF operator");
        }
    }
    internal partial class SizeOfExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Type);
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
            Datatype = Compilation.Get(NativeType.UInt32);
            return null;
        }
    }
    internal partial class DefaultExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Type);
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
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
            Type.RequireType();
            Datatype = Type.Symbol as TypeSymbol;
            Symbol = b.ExplicitConversion(Expr, Datatype);
            if (!(Symbol as ConversionSymbol).Exists)
                ThrowError(ErrorCode.NoConversion, Expr.Datatype, Type.Symbol);
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
            Expr.RequireValue();
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
            if (Expr.Datatype.IsValueType)
            {
                if (Binder.TypesMatch(Expr.Datatype, Type.Symbol as TypeSymbol))
                    return LiteralExpr.Bound(Constant.Create(true));
                else if (Binder.TypesMatch(Type.Symbol as TypeSymbol, Compilation.Get(WellKnownTypes.System_ValueType)))
                    return LiteralExpr.Bound(Constant.Create(true));
                else if (Binder.TypesMatch(Type.Symbol as TypeSymbol, Compilation.Get(NativeType.Object)))
                    return LiteralExpr.Bound(Constant.Create(true));
                return LiteralExpr.Bound(Constant.Create(false));
            }
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
            Expr.RequireValue();
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
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
            Symbol = b.BindMethodCall(Expr, Expr.Symbol, Args, out Self);
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireType();
            b.Bind(ref Args);
            Symbol = b.BindCtorCall(Expr, Expr.Symbol, Args);
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
            b.Bind(ref Args);
            b.Convert(ref Expr, Compilation.Get(NativeType.Array));
            Self = Expr;
            var s = Self.Datatype.Lookup(SystemNames.IndexerName);
            Symbol = b.BindArrayAccess(Self, s, Args);
            Datatype = Symbol.Type();
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
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class ExprList : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(Exprs);
            foreach(var expr in Exprs) expr.RequireValue();
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
            if (ElemType != null)
            {
                b.Bind(ref ElemType);
                ElemType.RequireType();
            }
            b.Bind(ref Values);
            if (ElemType != null)
                ConvertElements(ElemType.Symbol as TypeSymbol);
            else
                ConvertElements(Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object),
                    Compilation.Get(WellKnownTypes.XSharp___Array));
            return null;
        }
        internal static LiteralArray Bound(IList<Expr> values, TypeSymbol type = null)
        {
            var e = new LiteralArray(new ExprList(values));
            e.ConvertElements(type ?? Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
            return e;
        }
        internal static LiteralArray Bound(IList<Arg> args, TypeSymbol type = null)
        {
            var values = new List<Expr>(args.Count);
            foreach(var a in args) values.Add(a.Expr);
            return Bound(values, type);
        }
        private void ConvertElements(TypeSymbol et, TypeSymbol dt = null)
        {
            for (int i = 0; i < Values.Exprs.Count; i++)
            {
                var v = Values.Exprs[i];
                Binder.Convert(ref v, et, BindOptions.Default);
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
            Cond.RequireValue();
            True.RequireValue();
            False.RequireValue();
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            Datatype = b.ConvertResult(ref True, ref False);
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
                Alias.RequireValue();
                b.Convert(ref Alias, Compilation.Get(NativeType.String));
            }
            b.Bind(ref Field);
            Field.RequireValue();
            b.Convert(ref Field, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
        internal static AliasExpr Bound(string fieldName)
        {
            return new AliasExpr(null, LiteralExpr.Bound(Constant.Create(fieldName)), Token.None) { Datatype = Compilation.Get(NativeType.Usual) };
        }
    }
    internal partial class SubstrExpr : BinaryExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Left.RequireValue();
            Right.RequireValue();
            b.Convert(ref Left, Compilation.Get(NativeType.String));
            b.Convert(ref Right, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Boolean);
            return null;
        }
    }
    internal partial class AutoVarExpr : Expr
    {
        internal Expr Var;
        AutoVarExpr(Expr var) : base(var.Token) { Var = var; }
        public override string ToString() { return "{Var:" + Var.ToString() + "}"; }
        internal static AutoVarExpr Bound(string varName)
        {
            var e = LiteralExpr.Bound(Constant.Create(varName));
            return new AutoVarExpr(e) { Symbol = e.Symbol, Datatype = Compilation.Get(NativeType.Usual) };
        }
    }
    internal partial class Arg : Node
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireValue();
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
                    b.Convert(ref e, b.ObjectType);
                    Body.Exprs[Body.Exprs.Count - 1] = e;
                }
            }
            Symbol = b.ObjectType;
            return null;
        }
    }
}
