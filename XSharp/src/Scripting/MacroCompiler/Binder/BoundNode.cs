using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler.Syntax
{
    using static TokenAttr;

    internal partial class Node
    {
        internal Symbol Symbol = null;
        internal virtual Node Bind(Binder b) { return null; }
    }
    internal partial class Expr: Node
    {
        internal Type Datatype = null;
    }
    internal partial class TypeExpr : Expr
    {
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
    }
    internal partial class NameExpr : TypeExpr
    {
    }
    internal partial class IdExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            Symbol = b.Lookup(null, Name);
            if (Symbol is MemberSymbol)
            {
                var m = (Symbol as MemberSymbol).Member;
                switch (m.MemberType)
                {
                    case MemberTypes.Field:
                        Datatype = (m as FieldInfo).FieldType;
                        break;
                    case MemberTypes.Event:
                        Datatype = (m as EventInfo).EventHandlerType;
                        break;
                    case MemberTypes.Property:
                        Datatype = (m as PropertyInfo).PropertyType;
                        break;
                }
            }
            return null;
        }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName);
            return null;
        }
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override Node Bind(Binder b)
        {
            b.Bind(ref Expr);
            Symbol = b.Lookup(Expr.Symbol, Member.LookupName);
            return null;
        }
    }
    internal partial class BinaryExpr : Expr
    {
    }
    internal partial class PrefixExpr : Expr
    {
    }
    internal partial class PostfixExpr : Expr
    {
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
                    Symbol = Constant.Create(true);
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
                    Symbol = Constant.Create<object>(null);
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
            throw new Exception("SELF not supported");
        }
    }
    internal partial class SuperExpr : Expr
    {
        internal override Node Bind(Binder b)
        {
            throw new Exception("SUPER not supported");
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
    }
    internal partial class TypeConversion : TypeCast
    {
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
            Datatype = (((MethodSymbol)Symbol)?.Member as MethodInfo)?.ReturnType;
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
        internal override Node Bind(Binder b)
        {
            if (Params != null)
            {
                foreach (var p in Params)
                {
                    b.Locals.Add(p.LookupName, new LocalSymbol(p.LookupName, b.ObjectType));
                }
            }
            if (Body != null)
                b.Bind(ref Body);
            Symbol = b.ObjectType;
            return null;
        }
    }
}