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
        internal virtual void RequireValue()
        {
            if (Symbol is SymbolList && Symbol.UniqueIdent() is Symbol s)
            {
                Symbol = s;
                Datatype = s.Type();
            }
            if (Datatype == null)
                ThrowError(ErrorCode.NotAnExpression, Symbol);
            if (Symbol is SymbolList)
                ThrowError(ErrorCode.NotAnExpression, Symbol);
        }
        internal virtual void RequireType()
        {
            if (Symbol is SymbolList)
                Symbol = Symbol.UniqueType();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Type");
            if (!(Symbol is TypeSymbol))
                ThrowError(ErrorCode.NotAType, Symbol);
        }
        internal virtual void RequireGetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression");
            if (!Symbol.HasGetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Get);
        }
        internal virtual void RequireSetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression");
            if (!Symbol.HasSetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Set);
        }
        internal virtual void RequireGetSetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression");
            if (!Symbol.HasGetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Get);
            if (!Symbol.HasSetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Set);
        }
        internal virtual void RequireRefAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression");
            if (!Symbol.HasRefAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Ref);
        }
    }
    abstract internal partial class TypeExpr : Expr
    {
        internal override void RequireValue()
        {
            if (Symbol is MethodBaseSymbol)
                ThrowError(ErrorCode.NotAnExpression, Symbol);
            base.RequireValue();
        }
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
        internal override void RequireValue() => Expr.RequireValue();
        internal override void RequireType() => Expr.RequireType();
        internal override void RequireGetAccess() => Expr.RequireGetAccess();
        internal override void RequireSetAccess() => Expr.RequireSetAccess();
        internal override void RequireGetSetAccess() => Expr.RequireGetSetAccess();
        internal override void RequireRefAccess() => Expr.RequireRefAccess();
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
        internal override Node Bind(Binder b)
        {
            if (Affinity != BindAffinity.Type)
            {
                Expr e = new IdExpr(Token);
                b.Bind(ref e, Affinity);
                return e;
            }
            Symbol = Binder.GetNativeTypeFromToken(Kind) ?? ThrowError(ErrorCode.NotSupported,Kind);
            return null;
        }
    }
    internal partial class IdExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.Lookup(Name);
            if (Affinity != BindAffinity.Invoke && Affinity != BindAffinity.Type)
            {
                if (Symbol.IsMethodOrMethodGroup())
                {
                    // IdExpr can't be a method
                    // TODO (nvk): If delegates are supprted this needs to be revised!
                    Symbol = null;
                }
                else if (Symbol is TypeSymbol)
                {
                    Symbol = null;
                }
                else if (Symbol is NamespaceSymbol)
                {
                    Symbol = null;
                }
                else if (Symbol is SymbolList)
                {
                    Symbol = Symbol.UniqueIdent();
                }
            }
            if (Symbol == null && Affinity != BindAffinity.Type)
            {
                if (Affinity == BindAffinity.Alias)
                {
                    return LiteralExpr.Bound(Constant.Create(Name));
                }
                else
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
                            if (Affinity == BindAffinity.Assign)
                                b.CreatesAutoVars = true;
                            return AutoVarExpr.Bound(Name);
                    }
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
            if (!b.Options.AllowDotAccess && Token.type == TokenType.DOT)
                ThrowError(ErrorCode.DotMemberAccess);
            b.Bind(ref Expr);
            Expr.RequireValue();
            Symbol = b.BindMemberAccess(ref Expr, ref Member, Affinity);
            Datatype = Symbol.Type();
            return null;
        }
        internal override Expr Cloned(Binder b)
        {
            b.Cache(ref Expr);
            return this;
        }
        internal static MemberAccessExpr Bound(Binder b, Expr expr, Expr member, BindAffinity affinity)
        {
            expr.RequireValue();
            var e = new MemberAccessExpr(expr, Token.None, member);
            e.Symbol = b.BindMemberAccess(ref e.Expr, ref e.Member, affinity);
            e.Datatype = e.Symbol.Type();
            return e;
        }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr, BindAffinity.Type);
            if (Expr.Symbol.UniqueType() is TypeSymbol t)
            {
                Expr.Symbol = t;
                Symbol = b.Lookup(t, Member.LookupName) ?? ThrowError(Binder.LookupError(Expr, this));
                Datatype = Symbol.Type();
            }
            if (Symbol == null)
            {
                if (b.Options.AllowDotAccess)
                {
                    if (Expr.Symbol.UniqueIdent() != null)
                        return MemberAccessExpr.Bound(b, Expr, Member, Affinity);
                    if (Expr is NameExpr aname && Member is NameExpr fname)
                        return AliasExpr.Bound(aname.LookupName, fname.LookupName);
                    Expr.RequireValue();
                }
                else
                    Expr.RequireType();
            }
            return null;
        }
        internal override Expr Cloned(Binder b)
        {
            if (!(Expr.Symbol is TypeSymbol))
                b.Cache(ref Expr);
            return this;
        }
    }
    internal partial class AssignExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left, BindAffinity.Assign);
            b.Bind(ref Right);
            Left.RequireSetAccess();
            Right.RequireGetAccess();
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
            b.Bind(ref Left, BindAffinity.Assign);
            b.Bind(ref Right);
            Left.RequireGetSetAccess();
            Right.RequireGetAccess();
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
            Left.RequireGetAccess();
            Right.RequireGetAccess();
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
            Left.RequireGetAccess();
            Right.RequireGetAccess();
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
            Expr.RequireGetAccess();
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
            b.Bind(ref Expr, BindAffinity.Assign);
            Expr.RequireGetSetAccess();
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
            b.Bind(ref Expr, BindAffinity.Assign);
            Expr.RequireGetSetAccess();
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
            b.Bind(ref Type, BindAffinity.Type);
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
            Datatype = Compilation.Get(NativeType.UInt32);
            return null;
        }
        internal override void RequireGetAccess() => base.RequireValue();
    }
    internal partial class DefaultExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Type, BindAffinity.Type);
            Type.RequireType();
            Symbol = Type.Symbol as TypeSymbol;
            Datatype = Symbol as TypeSymbol;
            return null;
        }
        internal override void RequireGetAccess() => base.RequireValue();
    }
    internal partial class TypeCast : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Type, BindAffinity.Type);
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
            b.Bind(ref Type, BindAffinity.Type);
            Expr.RequireGetAccess();
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
            b.Bind(ref Type, BindAffinity.Type);
            Expr.RequireGetAccess();
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
            b.Bind(ref Expr, BindAffinity.Invoke);
            b.Bind(ref Args);
            Symbol = b.BindMethodCall(Expr, Expr.Symbol, Args, out Self);
            Datatype = Symbol.Type();
            if (Self?.Datatype.IsValueType == true)
            {
                if ((Symbol as MethodSymbol).DeclaringType.IsValueType)
                {
                    if (!Symbol.HasRefAccess)
                    {
                        b.Cache(ref Self);
                    }
                    b.Convert(ref Self, Binder.ByRefOf(Self.Datatype));
                }
                else
                {
                    b.Convert(ref Self, Compilation.Get(NativeType.Object));
                }
            }
            return null;
        }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr, BindAffinity.Type);
            Expr.RequireType();
            b.Bind(ref Args);
            Symbol = b.BindCtorCall(Expr, Expr.Symbol, Args);
            Datatype = Symbol.Type();
            return null;
        }
    }
    internal partial class IntrinsicCallExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Args);
            switch (Kind)
            {
                case IntrinsicCallType.GetFParam:
                case IntrinsicCallType.GetMParam:
                    b.ConvertArrayBase(Args);
                    b.ConvertExplicit(ref Args.Args[0].Expr, Compilation.Get(NativeType.UInt32));
                    Symbol = b.Lookup(XSharpSpecialNames.ClipperArgs);
                    Datatype = Compilation.Get(NativeType.Object);
                    break;
                case IntrinsicCallType.Chr:
                    b.ConvertExplicit(ref Args.Args[0].Expr, Compilation.Get(NativeType.UInt32));
                    Symbol = Compilation.Get(WellKnownMembers.XSharp_Core_Functions_Chr);
                    Datatype = Symbol.Type();
                    break;
                default:
                    throw new InternalError();
            }
            return null;
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
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
        internal override void RequireSetAccess() => RequireGetAccess();
        internal override void RequireGetSetAccess() => RequireGetAccess();
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
                b.Bind(ref ElemType, BindAffinity.Type);
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
        internal override void RequireGetAccess() => base.RequireValue();
    }
    internal partial class IifExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Cond);
            b.Bind(ref True);
            b.Bind(ref False);
            Cond.RequireGetAccess();
            True.RequireGetAccess();
            False.RequireGetAccess();
            b.Convert(ref Cond, Compilation.Get(NativeType.Boolean));
            Datatype = b.ConvertResult(ref True, ref False);
            return null;
        }
        internal override void RequireGetAccess() => base.RequireValue();
    }
    internal partial class MemvarExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Var);
            Var.RequireGetAccess();
            b.Convert(ref Var, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
        internal override void RequireGetAccess() => RequireValue();
        internal override void RequireSetAccess() => RequireValue();
        internal override void RequireGetSetAccess() => RequireValue();
    }
    internal partial class AliasExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            if (Alias != null)
            {
                b.Bind(ref Alias);
                Alias.RequireGetAccess();
                var m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGetWa) as MethodSymbol;
                b.Convert(ref Alias, Binder.FindType(m.Parameters.Parameters[0].ParameterType));
            }
            b.Bind(ref Field);
            Field.RequireGetAccess();
            b.Convert(ref Field, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
        internal static AliasExpr Bound(string fieldName)
        {
            return new AliasExpr(null, LiteralExpr.Bound(Constant.Create(fieldName)), Token.None) { Datatype = Compilation.Get(NativeType.Usual) };
        }
        internal static AliasExpr Bound(string aliasName, string fieldName)
        {
            return new AliasExpr(
                LiteralExpr.Bound(Constant.Create(aliasName)),
                LiteralExpr.Bound(Constant.Create(fieldName)),
                Token.None)
                { Datatype = Compilation.Get(NativeType.Usual) };
        }
        internal static AliasExpr Bound(Expr field)
        {
            return new AliasExpr(null, field, Token.None) { Datatype = Compilation.Get(NativeType.Usual) };
        }
        internal override void RequireGetAccess() => RequireValue();
        internal override void RequireSetAccess() => RequireValue();
        internal override void RequireGetSetAccess() => RequireValue();
    }
    internal partial class AliasWaExpr : AliasExpr
    {
        internal override Node Bind(Binder b)
        {
            if (Alias != null)
            {
                b.Bind(ref Alias, BindAffinity.Alias);
                Alias.RequireGetAccess();
                b.Convert(ref Alias, Compilation.Get(NativeType.Usual));
            }
            b.Bind(ref Field, BindAffinity.AliasField);
            Datatype = Field.Datatype;
            return null;
        }
        internal override void RequireGetAccess() => Field.RequireGetAccess();
        internal override void RequireSetAccess() => Field.RequireSetAccess();
        internal override void RequireGetSetAccess() => Field.RequireGetSetAccess();
    }
    internal partial class RuntimeIdExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
            b.Convert(ref Expr, Compilation.Get(NativeType.String));
            switch (Affinity)
            {
                case BindAffinity.AliasField:
                    return AliasExpr.Bound(Expr);
                case BindAffinity.Member:
                    return Expr;
                default:
                    ThrowError(ErrorCode.InvalidRuntimeIdExpr);
                    break;
            }
            return null;
        }
        internal override void RequireGetAccess() => RequireValue();
        internal override void RequireSetAccess() => RequireValue();
        internal override void RequireGetSetAccess() => RequireValue();
    }
    internal partial class SubstrExpr : BinaryExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Left);
            b.Bind(ref Right);
            Left.RequireGetAccess();
            Right.RequireGetAccess();
            b.Convert(ref Left, Compilation.Get(NativeType.String));
            b.Convert(ref Right, Compilation.Get(NativeType.String));
            Symbol = Compilation.Get(WellKnownMembers.XSharp_Core_Functions_Instr);
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
        internal override void RequireGetAccess() => RequireValue();
        internal override void RequireSetAccess() => RequireValue();
        internal override void RequireGetSetAccess() => RequireValue();
    }
    internal partial class Arg : Node
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
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
        LocalSymbol PCount;
        ArgumentSymbol ParamArray;
        internal override Node Bind(Binder b)
        {
            if (Params != null)
            {
                foreach (var p in Params)
                {
                    b.AddLocal(p.LookupName, b.ObjectType);
                    p.Bind(b);
                }
            }
            ParamArray = b.AddParam(XSharpSpecialNames.ClipperArgs, Binder.ArrayOf(b.ObjectType));
            b.AddConstant(XSharpSpecialNames.ClipperArgCount, Constant.Create(Params?.Count ?? 0));
            PCount = b.AddLocal(XSharpSpecialNames.ClipperPCount, Compilation.Get(NativeType.Int32));
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
