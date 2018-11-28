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

    internal partial class Node
    {
        internal Symbol Symbol = null;
        internal virtual Node Bind(Binder b) { throw new NotImplementedException(); }
    }
    internal partial class Expr : Node
    {
        internal TypeSymbol Datatype = null;
        internal BindAffinity Affinity = BindAffinity.Access;
    }
    internal partial class StoreTemp : Expr
    {
        internal LocalSymbol Local;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Local = b.AddLocal(Expr.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
            return null;
        }
    }
    internal partial class LoadTemp : Expr
    {
        internal override Node Bind(Binder b)
        {
            Assert(Temp.Symbol != null);
            b.Bind(ref Expr);
            Symbol = Temp.Symbol;
            Datatype = Temp.Datatype;
            return null;
        }
    }
    internal partial class TypeExpr : Expr
    {
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = Binder.GetNativeTypeFromToken(Kind);
            return null;
        }
    }
    internal partial class NameExpr : TypeExpr
    {
    }
    internal partial class IdExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.Lookup(null, Name);
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
            var r = new BinaryExpr(Left, Kind, Right);
            r.Symbol = Binder.BinaryOperation(BinaryOperatorSymbol.OperatorKind(Kind), ref r.Left, ref r.Right);
            r.Datatype = (r.Symbol as BinaryOperatorSymbol)?.Type;
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
            Datatype = (Symbol as BinaryOperatorSymbol)?.Type;
            return null;
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
            Datatype = (Symbol as BinaryOperatorSymbol)?.Type;
            return null;
        }
    }
    internal partial class UnaryExpr : Expr
    {
        internal Expr Left;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Left = Expr;
            if (UnaryOperatorSymbol.OperatorIsLogic(Kind))
            {
                Binder.Convert(ref Left, Compilation.Get(NativeType.Boolean));
            }
            Symbol = Binder.UnaryOperation(UnaryOperatorSymbol.OperatorKind(Kind), ref Expr);
            Datatype = (Symbol as UnaryOperatorSymbol)?.Type;
            return null;
        }
    }
    internal partial class PrefixExpr : Expr
    {
        Expr Left;
        internal override Node Bind(Binder b)
        {
            var u = new UnaryExpr(Expr, Kind);
            Expr = u;
            b.Bind(ref Expr);
            Left = u.Left;
            Binder.Convert(ref Expr, (Expr as UnaryExpr).Left.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
            return null;
        }
    }
    internal partial class PostfixExpr : Expr
    {
        Expr Left;
        LoadTemp Temp;
        internal override Node Bind(Binder b)
        {
            var t = new StoreTemp(Expr);
            Expr = t;
            Temp = new LoadTemp(t);
            var u = new UnaryExpr(Expr, Kind);
            Expr = u;
            b.Bind(ref Expr);
            Left = u.Left;
            Binder.Convert(ref Expr, (Expr as UnaryExpr).Left.Datatype);
            Symbol = Expr.Symbol;
            Datatype = Expr.Datatype;
            return null;
        }
    }
    internal partial class LiteralExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = Binder.CreateLiteral(Kind, Value);
            Datatype = (Symbol as Constant)?.Type;
            return null;
        }
        internal static LiteralExpr Bound(Constant c)
        {
            var e = new LiteralExpr(TokenType.LAST, null);
            e.Symbol = c;
            e.Datatype = (e.Symbol as Constant)?.Type;
            return e;
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
        Expr Self = null;
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
                Symbol = b.BindCall(Expr, Args, out Self);
            }
            Datatype = ((MethodSymbol)Symbol)?.Type;
            return null;
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
    }
    internal partial class EmptyExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = Constant.CreateDefault(Compilation.Get(NativeType.Usual));
            Datatype = (Symbol as Constant)?.Type;
            return null;
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
            b.Bind(ref Values);
            Convert(Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
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
        private void Convert(TypeSymbol t)
        {
            for (int i = 0; i < Values.Exprs.Count; i++)
            {
                var v = Values.Exprs[i];
                Binder.Convert(ref v, t);
                Values.Exprs[i] = v;
            }
            Symbol = t;
            Datatype = Binder.ArrayOf(t);
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
        ParameterSymbol ParamArray;
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