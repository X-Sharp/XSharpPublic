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
            switch (Kind)
            {
                case TokenType.ARRAY:
                case TokenType.CODEBLOCK:
                case TokenType.DATE:
                case TokenType.FLOAT:
                case TokenType.PSZ:
                case TokenType.SYMBOL:
                    // TODO nvk: create special types
                    break;
                case TokenType.USUAL:
                    Symbol = Compilation.GetNativeType(NativeType.Usual);
                    break;
                case TokenType.BYTE:
                    Symbol = Compilation.GetNativeType(NativeType.Byte);
                    break;
                case TokenType.CHAR:
                    Symbol = Compilation.GetNativeType(NativeType.Char);
                    break;
                case TokenType.DATETIME:
                    Symbol = Compilation.GetNativeType(NativeType.DateTime);
                    break;
                case TokenType.DECIMAL:
                    Symbol = Compilation.GetNativeType(NativeType.Decimal);
                    break;
                case TokenType.DWORD:
                    Symbol = Compilation.GetNativeType(NativeType.UInt32);
                    break;
                case TokenType.DYNAMIC:
                    Symbol = Compilation.GetNativeType(NativeType.Object); // TODO nvk: special dynamic type?
                    break;
                case TokenType.INT:
                    Symbol = Compilation.GetNativeType(NativeType.Int32);
                    break;
                case TokenType.INT64:
                    Symbol = Compilation.GetNativeType(NativeType.Int64);
                    break;
                case TokenType.LOGIC:
                    Symbol = Compilation.GetNativeType(NativeType.Boolean);
                    break;
                case TokenType.LONGINT:
                    Symbol = Compilation.GetNativeType(NativeType.Int32);
                    break;
                case TokenType.OBJECT:
                    Symbol = Compilation.GetNativeType(NativeType.Object);
                    break;
                case TokenType.PTR:
                    Symbol = Compilation.GetNativeType(NativeType.Unknown); // TODO nvk: PTR type
                    break;
                case TokenType.REAL4:
                    Symbol = Compilation.GetNativeType(NativeType.Single);
                    break;
                case TokenType.REAL8:
                    Symbol = Compilation.GetNativeType(NativeType.Double);
                    break;
                case TokenType.SHORTINT:
                    Symbol = Compilation.GetNativeType(NativeType.Int16);
                    break;
                case TokenType.STRING:
                    Symbol = Compilation.GetNativeType(NativeType.String);
                    break;
                case TokenType.UINT64:
                    Symbol = Compilation.GetNativeType(NativeType.UInt64);
                    break;
                case TokenType.VOID:
                    Symbol = Compilation.GetNativeType(NativeType.Void);
                    break;
                case TokenType.WORD:
                    Symbol = Compilation.GetNativeType(NativeType.UInt16);
                    break;
                default:
                    break;
            }
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
            switch (Kind)
            {
                case TokenType.TRUE_CONST:
                    Symbol = Constant.Create(true);
                    break;
                case TokenType.FALSE_CONST:
                    Symbol = Constant.Create(false);
                    break;
                case TokenType.CHAR_CONST:
                    Symbol = Constant.Create(Literals.CharValue(Value));
                    break;
                case TokenType.STRING_CONST:
                case TokenType.ESCAPED_STRING_CONST:
                case TokenType.INTERPOLATED_STRING_CONST:
                    Symbol = Constant.Create(Literals.StringValue(Value));
                    break;
                case TokenType.SYMBOL_CONST:
                    Symbol = Constant.Create(Value.Substring(1).ToUpperInvariant());
                    break;
                case TokenType.HEX_CONST:
                    switch (Value.Last())
                    {
                        case 'U':
                        case 'u':
                            if (Value.Length > 8 + 3)
                                Symbol = Constant.Create(unchecked((ulong)Literals.HexValue(Value.Substring(2))));
                            else
                                Symbol = Constant.Create(unchecked((uint)Literals.HexValue(Value.Substring(2))));
                            break;
                        case 'L':
                        case 'l':
                            if (Value.Length > 8 + 3)
                                Symbol = Constant.Create(Literals.HexValue(Value.Substring(2)));
                            else
                                Symbol = Constant.Create(unchecked((int)Literals.HexValue(Value.Substring(2))));
                            break;
                        default:
                            {
                                long l = Literals.HexValue(Value.Substring(2));
                                if (l < 0)
                                    Symbol = Constant.Create(unchecked((ulong)l));
                                else if (l > uint.MaxValue)
                                    Symbol = Constant.Create(l);
                                else if (l > int.MaxValue)
                                    Symbol = Constant.Create(unchecked((uint)l));
                                else
                                    Symbol = Constant.Create(unchecked((int)l));
                            }
                            break;
                    }
                    break;
                case TokenType.BIN_CONST:
                    switch (Value.Last())
                    {
                        case 'U':
                        case 'u':
                            if (Value.Length > 32 + 3)
                                Symbol = Constant.Create(unchecked((ulong)Literals.BinValue(Value.Substring(2))));
                            else
                                Symbol = Constant.Create(unchecked((uint)Literals.BinValue(Value.Substring(2))));
                            break;
                        case 'L':
                        case 'l':
                            if (Value.Length > 32 + 3)
                                Symbol = Constant.Create(Literals.BinValue(Value.Substring(2)));
                            else
                                Symbol = Constant.Create(unchecked((int)Literals.BinValue(Value.Substring(2))));
                            break;
                        default:
                            {
                                long l = Literals.BinValue(Value.Substring(2));
                                if (l < 0)
                                    Symbol = Constant.Create(unchecked((ulong)l));
                                else if (l > uint.MaxValue)
                                    Symbol = Constant.Create(l);
                                else if (l > int.MaxValue)
                                    Symbol = Constant.Create(unchecked((uint)l));
                                else
                                    Symbol = Constant.Create(unchecked((int)l));
                            }
                            break;
                    }
                    break;
                case TokenType.REAL_CONST:
                    switch (Value.Last())
                    {
                        case 'M':
                        case 'm':
                            Symbol = Constant.Create(decimal.Parse(Value.Substring(0, Value.Length - 1), System.Globalization.CultureInfo.InvariantCulture));
                            break;
                        case 'S':
                        case 's':
                            Symbol = Constant.Create(float.Parse(Value.Substring(0, Value.Length - 1), System.Globalization.CultureInfo.InvariantCulture));
                            break;
                        case 'D':
                        case 'd':
                            Symbol = Constant.Create(double.Parse(Value.Substring(0, Value.Length - 1), System.Globalization.CultureInfo.InvariantCulture));
                            break;
                        default:
                            Symbol = Constant.Create(double.Parse(Value, System.Globalization.CultureInfo.InvariantCulture));
                            break;
                    }
                    break;
                case TokenType.INT_CONST:
                    switch (Value.Last())
                    {
                        case 'U':
                        case 'u':
                            try
                            {
                                ulong ul = ulong.Parse(Value.Substring(0, Value.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (ul > uint.MaxValue)
                                    Symbol = Constant.Create(ul);
                                else
                                    Symbol = Constant.Create(unchecked((uint)ul));
                            }
                            catch (OverflowException)
                            {
                                throw new Exception("Integer overflow");
                            }
                            break;
                        case 'L':
                        case 'l':
                            try
                            {
                                long l = long.Parse(Value.Substring(0, Value.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (l > int.MaxValue)
                                    Symbol = Constant.Create(l);
                                else
                                    Symbol = Constant.Create(unchecked((int)l));
                            }
                            catch (OverflowException)
                            {
                                throw new Exception("Integer overflow");
                            }
                            break;
                        default:
                            try
                            {
                                ulong un = 0;
                                long n = 0;
                                if (Value.First() != '-')
                                {
                                    un = ulong.Parse(Value, System.Globalization.CultureInfo.InvariantCulture);
                                    if (un <= long.MaxValue)
                                        n = unchecked((long)un);
                                }
                                else
                                {
                                    n = long.Parse(Value, System.Globalization.CultureInfo.InvariantCulture);
                                }
                                if (un > long.MaxValue)
                                    Symbol = Constant.Create(un);
                                else if (n > uint.MaxValue)
                                    Symbol = Constant.Create(n);
                                else if (n > int.MaxValue)
                                    Symbol = Constant.Create(unchecked((uint)n));
                                else
                                    Symbol = Constant.Create(unchecked((int)n));
                            }
                            catch (OverflowException)
                            {
                                throw new Exception("Integer overflow");
                            }
                            break;
                    }
                    break;
                case TokenType.NULL:
                    Symbol = Constant.Create((object)null);
                    break;
                case TokenType.DATE_CONST:
                case TokenType.NIL:
                case TokenType.NULL_ARRAY:
                case TokenType.NULL_CODEBLOCK:
                case TokenType.NULL_DATE:
                case TokenType.NULL_OBJECT:
                case TokenType.NULL_PSZ:
                case TokenType.NULL_PTR:
                case TokenType.NULL_STRING:
                case TokenType.NULL_SYMBOL:
                default:
                    throw new Exception("Unexpected literal kind");
            }
            Datatype = (Symbol as Constant)?.Type;
            return null;
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
    }
    internal partial class DefaultExpr : Expr
    {
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
    }
    internal partial class AsTypeExpr : Expr
    {
    }
    internal partial class MethodCallExpr : Expr
    {
        Expr Self = null;
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            b.Bind(ref Args);
            Symbol = b.BindCall(Expr, Args, out Self);
            Datatype = ((MethodSymbol)Symbol)?.Type;
            return null;
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
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
                Datatype = Compilation.GetNativeType(NativeType.Void);
            }
            return null;
        }
    }
    internal partial class LiteralArray : Expr
    {
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