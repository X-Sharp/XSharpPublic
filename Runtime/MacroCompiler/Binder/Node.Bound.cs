using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using static System.Diagnostics.Debug;
using System.Linq.Expressions;

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
                ThrowError(ErrorCode.NotFound, "Type", this.ToString());
            if (!(Symbol is TypeSymbol))
                ThrowError(ErrorCode.NotAType, Symbol);
        }
        internal virtual void RequireGetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression", this.ToString());
            if (!Symbol.HasGetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Get);
        }
        internal virtual void RequireSetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression", this.ToString());
            if (!Symbol.HasSetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Set);
        }
        internal virtual void RequireInitAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression", this.ToString());
            if (!Symbol.HasInitAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Init);
        }
        internal virtual void RequireGetSetAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression", this.ToString());
            if (!Symbol.HasGetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Get);
            if (!Symbol.HasSetAccess)
                throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Set);
        }
        internal virtual void RequireRefAccess()
        {
            RequireValue();
            if (Symbol == null)
                ThrowError(ErrorCode.NotFound, "Expression", this.ToString());
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
        static internal IdExpr Bound(Symbol sym)
        {
            var e = new IdExpr(Token.None);
            e.Symbol = sym;
            e.Datatype = sym.Type();
            return e;
        }
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
            if (!b.Options.AllowDotAccess && Token.Type == TokenType.DOT)
                ThrowError(ErrorCode.DotMemberAccess);
            b.Bind(ref Expr);
            Expr.RequireValue();
            if (Expr.Datatype.IsArray && Expr.Datatype.ArrayRank == 1 && Binder.LookupComparer.Equals((Member as IdExpr)?.Name, SystemNames.Length))
                return ArrayLengthExpr.Bound(Expr);
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
    internal partial class ArrayLengthExpr : MemberAccessExpr
    {
        internal ArrayLengthExpr(Expr array) : base(array, array.Token, null) { }
        internal override void RequireGetAccess() { }
        internal override void RequireSetAccess() => throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Set);
        internal override void RequireRefAccess() => throw Binder.AccessModeError(this, Symbol, Symbol.AccessMode.Ref);
        internal static ArrayLengthExpr Bound(Expr expr)
        {
            return new ArrayLengthExpr(expr) { Datatype = Compilation.Get(NativeType.Int32) };
        }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr, BindAffinity.Type);
            var s = Expr.Symbol.UniqueTypeOrNamespace();
            if (s is TypeSymbol t)
            {
                Expr.Symbol = t;
                Symbol = b.Lookup(t, Member.LookupName) ?? ThrowError(Binder.LookupError(Expr, this));
                Datatype = Symbol.Type();
            }
            else if (s is NamespaceSymbol ns)
            {
                Expr.Symbol = ns;
                Symbol = b.Lookup(ns, Member.LookupName) ?? ThrowError(Binder.LookupError(Expr, this));
                Datatype = Symbol.Type();
            }
            if (Symbol == null)
            {
                if (b.Options.AllowDotAccess)
                {
                    if (b.Options.UndeclaredVariableResolution != VariableResolution.TreatAsField)
                    {
                        Expr.Symbol = null; // re-bind Expr -- this is a hack!!!
                        b.Bind(ref Expr, Affinity);
                    }
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
        internal static AssignExpr Bound(Expr Left, Expr Right, BindOptions options)
        {
            Left.RequireSetAccess();
            Right.RequireGetAccess();
            Binder.Convert(ref Right, Left.Datatype, options);
            return new AssignExpr(Left, Left.Token, Right) { Symbol = Left.Symbol, Datatype = Left.Datatype };
        }
    }
    internal partial class InitExpr : AssignExpr
    {
        InitExpr(Expr l, Token o, Expr r) : base(l, o, r) { }
        internal static new InitExpr Bound(Expr Left, Expr Right, BindOptions options)
        {
            Left.RequireInitAccess();
            Right.RequireGetAccess();
            Binder.Convert(ref Right, Left.Datatype, options);
            return new InitExpr(Left, Left.Token, Right) { Symbol = Left.Symbol, Datatype = Left.Datatype };
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
            Right = BinaryExpr.Bound(Left.Cloned(b), Token, Right, BinaryOperatorSymbol.OperatorKind(Kind), b.Options.Binding);
            b.Convert(ref Right, Left.Datatype);
            Symbol = Left.Symbol;
            Datatype = Left.Datatype;
            return null;
        }
        internal static AssignOpExpr Bound(Expr Left, Expr Right, BinaryOperatorKind kind, Binder b)
        {
            Left.RequireGetSetAccess();
            Right.RequireGetAccess();
            Right = BinaryExpr.Bound(Left.Cloned(b), Left.Token, Right, kind, b.Options.Binding);
            b.Convert(ref Right, Left.Datatype);
            return new AssignOpExpr(Left, Left.Token, Right) { Symbol = Left.Symbol, Datatype = Left.Datatype };
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
        internal static BinaryExpr Bound(Expr Left, Token t, Expr Right, BinaryOperatorKind kind, BindOptions options)
        {
            Left.RequireGetAccess();
            Right.RequireGetAccess();
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
        internal static new BinaryExpr Bound(Expr Left, Token t, Expr Right, BinaryOperatorKind kind, BindOptions options)
        {
            Left.RequireGetAccess();
            Right.RequireGetAccess();
            var e = new BinaryExpr(Left, t, Right);
            e.Symbol = Binder.BindBinaryOperation(e, kind, options | BindOptions.Logic);
            e.Datatype = e.Symbol.Type();
            return e;
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
            if (b.Options.Dialect == XSharpDialect.FoxPro && (Type.Symbol as TypeSymbol).NativeType == NativeType.Usual)
                return LiteralExpr.Bound(Constant.Create(false));
            Symbol = Type.Symbol as TypeSymbol;
            Datatype = Symbol as TypeSymbol;
            return null;
        }
        internal static Expr Bound(Binder b, TypeSymbol type)
        {
            if (b.Options.Dialect == XSharpDialect.FoxPro && type.NativeType == NativeType.Usual)
            {
                Expr e = LiteralExpr.Bound(Constant.Create(false));
                b.Convert(ref e, type);
                return e;
            }
            return new DefaultExpr(null, null) { Symbol = type, Datatype = type };
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
        internal static TypeCast Bound(Binder b, Expr e, TypeSymbol t)
        {
            var s = b.ExplicitConversion(e, t);
            return new TypeCast(null, e) { Datatype = t, Symbol = s };
        }
    }
    internal partial class TypeConversion : TypeCast
    {
        internal static TypeConversion Bound(Expr e, TypeSymbol t, ConversionSymbol conv)
        {
            return new TypeConversion(null, e) { Datatype = t, Symbol = conv };
        }
        internal new static Expr Bound(Binder b, Expr e, TypeSymbol t)
        {
            b.Convert(ref e, t);
            return e;
        }
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
        internal static Expr Bound(Expr expr, TypeExpr type)
        {
            if (expr.Datatype.IsValueType)
            {
                if (Binder.TypesMatch(expr.Datatype, type.Symbol as TypeSymbol))
                    return LiteralExpr.Bound(Constant.Create(true));
                else if (Binder.TypesMatch(type.Symbol as TypeSymbol, Compilation.Get(WellKnownTypes.System_ValueType)))
                    return LiteralExpr.Bound(Constant.Create(true));
                else if (Binder.TypesMatch(type.Symbol as TypeSymbol, Compilation.Get(NativeType.Object)))
                    return LiteralExpr.Bound(Constant.Create(true));
                return LiteralExpr.Bound(Constant.Create(false));
            }
            return new IsExpr(expr, type, null) { Symbol = type.Symbol, Datatype = Compilation.Get(NativeType.Boolean) };
        }
        internal override void RequireGetAccess() { }
    }
    internal partial class IsVarExpr : IsExpr
    {
        internal LocalSymbol Var;
        internal bool? Check = null;
        internal static IsVarExpr Bound(Expr expr, TypeExpr type, LocalSymbol var)
        {
            bool? check = null;
            if (expr.Datatype.IsValueType)
            {
                if (Binder.TypesMatch(expr.Datatype, type.Symbol as TypeSymbol))
                    check = true;
                else if (Binder.TypesMatch(type.Symbol as TypeSymbol, Compilation.Get(WellKnownTypes.System_ValueType)))
                    check = true;
                else if (Binder.TypesMatch(type.Symbol as TypeSymbol, Compilation.Get(NativeType.Object)))
                    check = true;
                else
                    check = false;
            }
            return new IsVarExpr(expr, type, null, null) { Symbol = type.Symbol, Datatype = Compilation.Get(NativeType.Boolean), Var = var, Check = check };
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
        internal static Expr Bound(Expr expr, TypeExpr type)
        {
            return new AsTypeExpr(expr, type, null) { Symbol = type.Symbol, Datatype = type.Symbol as TypeSymbol };
        }
        internal override void RequireGetAccess() { }
    }
    internal partial class MethodCallExpr : Expr
    {
        protected Expr Self = null;
        protected Expr WriteBack = null;
        internal override Node Bind(Binder b)
        {
            if (b.Options.FoxParenArrayAccess && Expr is IdExpr id && (Args.Args.Count == 1 || Args.Args.Count == 2))
            {
                // Todo:
                // Validate also if the array indexes are or could be numeric
                // So valid are:
                // LONG, USUAL, OBJECT
                // When not, then we call the function always
                if (Affinity == BindAffinity.Assign)
                {
                    // transform to array access
                    var a = new ArrayAccessExpr(Expr, Args);
                    b.Bind(ref a, Affinity);
                    return a;
                }
                else
                {
                    Expr e = new IdExpr(id.Name);
                    b.Bind(ref e, BindAffinity.Invoke);
                    if (e.Symbol?.IsMethodOrMethodGroup() == true)
                    {
                        if (b.Options.UndeclaredVariableResolution == VariableResolution.TreatAsFieldOrMemvar)
                        {
                            // transform to call to __FoxArrayAccess()
                            var m = (Args.Args.Count == 1) ? WellKnownMembers.XSharp_VFP_Functions___FoxArrayAccess_1 : WellKnownMembers.XSharp_VFP_Functions___FoxArrayAccess_2;
                            Args.Args.Insert(0, new Arg(LiteralExpr.Bound(Constant.Create(id.Name))));
                            Args.Args.Insert(1, new Arg(new IdExpr(id.Name)));
                            b.Bind(ref Args);
                            if (Args.Args[1].Expr is AutoVarExpr av)
                                av.Safe = true;
                            return MethodCallExpr.Bound(b, null, Compilation.Get(m), null, Args);
                        }
                    }
                    else
                    {
                        // transform to array access
                        var a = new ArrayAccessExpr(Expr, Args);
                        b.Bind(ref a, Affinity);
                        return a;
                    }
                }
            }
            b.Bind(ref Expr, BindAffinity.Invoke);
            b.Bind(ref Args);
            Symbol = b.BindMethodCall(Expr, Expr.Symbol, Args, out Self, out WriteBack);
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
        internal static MethodCallExpr Bound(Expr e, Symbol sym, Expr self, ArgList args, Expr writeBack = null)
        {
            return new MethodCallExpr(e, args) { Symbol = sym, Datatype = sym.Type(), Self = self, WriteBack = writeBack };
        }
        internal static MethodCallExpr Bound(Binder b, Expr e, Symbol sym, Expr self, ArgList args)
        {
            Expr boundSelf;
            Expr writeBack;
            if (self != null)
                e = new MemberAccessExpr(self, null, null);
            var m = b.BindMethodCall(e, sym, args, out boundSelf, out writeBack);
            return new MethodCallExpr(e, args) { Symbol = m, Datatype = m.Type(), Self = boundSelf, WriteBack = writeBack };
        }
        internal static MethodCallExpr Bound(Binder b, Expr e, string name, ArgList args)
        {
            Expr m = new IdExpr(name);
            Expr self;
            Expr writeBack;
            var ms = b.BindMemberAccess(ref e, ref m, BindAffinity.Invoke);
            if (!(ms is MethodSymbol))
                throw e.Error(ErrorCode.Internal);
            var expr = new MemberAccessExpr(e, e.Token, m) { Symbol = ms };
            var sym = b.BindMethodCall(expr, ms, ArgList.Empty, out self, out writeBack);
            return Bound(e, sym, self, ArgList.Empty, writeBack);
        }
}
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr, BindAffinity.Type);
            Expr.RequireType();
            b.Bind(ref Args);
            Symbol = b.BindCtorCall(Expr, Expr.Symbol, Args, out WriteBack);
            Datatype = Symbol.Type();
            return null;
        }
        internal static CtorCallExpr Bound(Binder b, TypeExpr type, ArgList args)
        {
            Expr writeBack;
            var sym = b.BindCtorCall(type, type.Symbol, args, out writeBack);
            return new CtorCallExpr(type, args) { Symbol = sym, Datatype = sym.Type(), WriteBack = writeBack };
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
            if (Expr.Datatype.IsUsualOrObject())
            {
                b.Convert(ref Expr, Compilation.Get(NativeType.Array));
            }
            if (Binder.TypesMatch(Expr.Datatype,NativeType.Array) || Expr.Datatype.IsArray)
            {
                b.ConvertArrayBase(Args);
            }
            if (Expr.Datatype.IsArray && Expr.Datatype.ArrayRank == 1)
            {
                if (Args.Args.Count != 1)
                    throw Error(ErrorCode.WrongNumberIfIndices);
                return NativeArrayAccessExpr.Bound(Expr, Args);
            }
            Self = Expr;
            var s = Self.Datatype.Lookup(SystemNames.IndexerName);
            Symbol = b.BindArrayAccess(Self, s, Args);
            Datatype = Symbol.Type();
            if (Expr.Datatype.IsArray && Expr.Datatype != Datatype)
            {
                Expr conv = this;
                b.Convert(ref conv, Expr.Datatype.ElementType);
                return conv;
            }
            return null;
        }
        internal override Expr Cloned(Binder b)
        {
            b.Cache(ref Expr);
            foreach (var arg in Args.Args) b.Cache(ref arg.Expr);
            return this;
        }
        internal static ArrayAccessExpr Bound(Expr e, ArgList a, Binder b)
        {
            if (e.Datatype.IsUsualOrObject())
            {
                b.Convert(ref e, Compilation.Get(NativeType.Array));
            }
            if (Binder.TypesMatch(e.Datatype, NativeType.Array) || e.Datatype.IsArray)
            {
                b.ConvertArrayBase(a);
            }
            if (e.Datatype.IsArray && e.Datatype.ArrayRank == 1)
            {
                if (a.Args.Count != 1)
                    throw e.Error(ErrorCode.WrongNumberIfIndices);
                return NativeArrayAccessExpr.Bound(e, a);
            }
            var item = e.Datatype.Lookup(SystemNames.IndexerName);
            var sym = b.BindArrayAccess(e, item, a);
            return new ArrayAccessExpr(e, a) { Self = e, Symbol = sym, Datatype = sym.Type() };
        }
        internal override void RequireSetAccess() => RequireGetAccess();
        internal override void RequireGetSetAccess() => RequireGetAccess();
    }
    internal partial class NativeArrayAccessExpr : ArrayAccessExpr
    {
        NativeArrayAccessExpr(Expr e, ArgList a) : base(e, a) { }
        internal static NativeArrayAccessExpr Bound(Expr e, ArgList a)
        {
            return new NativeArrayAccessExpr(e, a) { Self = e, Symbol = e.Symbol, Datatype = e.Symbol.Type().ElementType };
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
        internal static IifExpr Bound(Expr cond, Expr t, Expr f, BindOptions opt)
        {
            cond.RequireGetAccess();
            t.RequireGetAccess();
            f.RequireGetAccess();
            Binder.Convert(ref cond, Compilation.Get(NativeType.Boolean), Binder.Conversion(cond, Compilation.Get(NativeType.Boolean), opt));
            var r = new IifExpr(cond, t, f, cond.Token);
            r.Datatype = Binder.ConvertResult(ref r.True, ref r.False, opt);
            return r;
        }
        internal override void RequireGetAccess() => base.RequireValue();
    }

    internal partial class MacroExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
            this.Symbol = Compilation.Get(WellKnownMembers.XSharp_RT_Functions_Evaluate);
            b.Convert(ref Expr, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
    }

    internal partial class MacroId : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Id);
            Id.RequireGetAccess();
            b.Convert(ref Id, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
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
                Symbol = m;
            }
            else
            {
                Symbol = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGet) as MethodSymbol;
            }
            b.Bind(ref Field);
            Field.RequireGetAccess();
            b.Convert(ref Field, Compilation.Get(NativeType.String));
            Datatype = Compilation.Get(NativeType.Usual);
            return null;
        }
        internal static AliasExpr Bound(string fieldName)
        {
            return new AliasExpr(null, LiteralExpr.Bound(Constant.Create(fieldName)), Token.None)
            {
                Symbol = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGet) as MethodSymbol,
                Datatype = Compilation.Get(NativeType.Usual)
            };
        }
        internal static AliasExpr Bound(string aliasName, string fieldName)
        {
            return new AliasExpr(
                LiteralExpr.Bound(Constant.Create(aliasName)),
                LiteralExpr.Bound(Constant.Create(fieldName)),
                Token.None)
                {
                    Symbol = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGetWa) as MethodSymbol,
                    Datatype = Compilation.Get(NativeType.Usual)
                };
        }
        internal static AliasExpr Bound(Expr field)
        {
            return new AliasExpr(null, field, Token.None)
            {
                Symbol = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGet) as MethodSymbol,
                Datatype = Compilation.Get(NativeType.Usual)
            };
        }
        internal override void RequireGetAccess() => RequireValue();
        internal override void RequireSetAccess() => RequireValue();
        internal override void RequireGetSetAccess() => RequireValue();
    }
    internal partial class AliasWaExpr : AliasExpr
    {
        Stmt Stmt;
        internal override Node Bind(Binder b)
        {
            Stmt = b.StmtStack.Peek();
            if (Alias != null)
            {
                b.Bind(ref Alias, BindAffinity.Alias);
                Alias.RequireGetAccess();
                b.Convert(ref Alias, Compilation.Get(NativeType.Usual));
                Stmt.RequireExceptionHandling = true;
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
            // check for macro expression
            // When & is followed by a (
            // then this is parsed a so called ParenExpression
            // We consider this to be a macro
            // unless it is prefixed by a ":" or a "->" token.
            // The & could be the first token in the macro
            // so check to make sure that there is a Prev token
            // There is Always a next token after the &, either an ID or a LPAREN
            bool macroCompile = false;
            TokenType prev = this.Token.Prev != null ? this.Token.Prev.Type : TokenType.WS;
            if (this.Token.Next.Type == TokenType.LPAREN
                && prev!= TokenType.COLON && prev != TokenType.ALIAS)
            {
                Expr = new MacroExpr(Expr, this.Token);
                macroCompile = true;
            }
            b.Bind(ref Expr);
            Expr.RequireGetAccess();
            if (macroCompile)
                b.Convert(ref Expr, Compilation.Get(NativeType.Usual));
            else
                b.Convert(ref Expr, Compilation.Get(NativeType.String));

            switch (Affinity)
            {
                case BindAffinity.AliasField:
                    return AliasExpr.Bound(Expr);
                case BindAffinity.Member:
                    return Expr;
                case BindAffinity.Access when macroCompile:
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
        internal bool Safe = false;
        AutoVarExpr(Expr var) : base(var.Token) { Var = var; }
        public override string ToString() { return "{Var:" + Var.ToString() + "}"; }
        internal static AutoVarExpr Bound(string varName, bool safe = false)
        {
            var e = LiteralExpr.Bound(Constant.Create(varName));
            return new AutoVarExpr(e) { Symbol = e.Symbol, Datatype = Compilation.Get(NativeType.Usual), Safe = safe };
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
        internal static ArgList Bound(params Expr[] argExprs)
        {
            return new ArgList(new List<Arg>(argExprs.Select(e => new Arg(e))));
        }
    }
    internal partial class Codeblock : Node
    {
        LocalSymbol PCount = null;
        ArgumentSymbol ParamArray = null;
        internal override Node Bind(Binder b)
        {
            b.Entity = this;
            if (Params != null)
            {
                foreach (var p in Params)
                {
                    b.AddLocal(p.LookupName, b.ObjectType,true);
                    p.Bind(b);
                }
            }
            ParamArray = b.AddParam(XSharpSpecialNames.ClipperArgs, Binder.ArrayOf(b.ObjectType));
            b.AddConstant(XSharpSpecialNames.ClipperArgCount, Constant.Create(Params?.Count ?? 0));
            PCount = b.AddLocal(XSharpSpecialNames.ClipperPCount, Compilation.Get(NativeType.Int32));
            if (Body != null)
            {
                b.BindStmt(ref Body);
            }
            return null;
        }
    }
    internal partial class TypedCodeblock : Codeblock
    {
        internal override Node Bind(Binder b)
        {
            b.Entity = this;
            if (Body != null)
            {
                b.BindStmt(ref Body);
            }
            b.GenerateDelegateTypeIfRequired();
            return null;
        }
        internal static TypedCodeblock Bound(Codeblock cb, Binder b)
        {
            var tcb = new TypedCodeblock(cb);
            if (b.ParameterTypes != null)
            {
                int a = 0;
                foreach (var p in b.ParameterTypes)
                {
                    string paramName = (cb.Params?.Count > a) ? cb.Params[a].LookupName : null;
                    ++a;
                    b.AddParam(paramName ?? ("__arg" + a), p);
                }
            }
            tcb.Bind(b);
            return tcb;
        }
    }
    internal partial class CodeblockExpr : Expr
    {
        Binder NestedBinder;
        int CbIndex;
        IList<XSharp.Codeblock> CbList;
        bool usualMacro;
        internal override Node Bind(Binder b)
        {
            NestedBinder = b.CreateNested();
            NestedBinder.Bind(ref Codeblock);
            CbIndex = b.AddNestedCodeblock(out Symbol);
            CbList = b.NestedCodeblocks;
            Datatype = Compilation.Get(WellKnownTypes.XSharp_Codeblock);
            usualMacro = b.ObjectType == Compilation.Get(NativeType.Usual);
            return null;
        }
        internal override void RequireGetAccess() => RequireValue();
    }
}
