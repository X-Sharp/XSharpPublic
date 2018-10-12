using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler.Syntax
{
    using static CodeGen;

    internal partial class Node
    {
        internal virtual void Emit(ILGenerator ilg) { throw new NotImplementedException(); }
    }
    internal partial class Expr : Node
    {
        internal virtual void Emit(ILGenerator ilg, bool preserve) { }
        internal sealed override void Emit(ILGenerator ilg) { Emit(ilg, true); }
    }
    internal partial class StoreTemp : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Local.Declare(ilg);
            Local.EmitSet(ilg);
        }
    }
    internal partial class LoadTemp : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr?.Emit(ilg, false);
            if (preserve)
                Temp.Local.EmitGet(ilg);
        }
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
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                Symbol.EmitGet(ilg);
        }
    }
    internal partial class MemberAccessExpr : Expr
    {
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Symbol.EmitGet(ilg);
        }
    }
    internal partial class AssignExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Right.Emit(ilg);
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Left.Symbol.EmitSet(ilg);
        }
    }
    internal partial class AssignOpExpr : AssignExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Right.Emit(ilg);
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Left.Symbol.EmitSet(ilg);
        }
    }
    internal partial class BinaryExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Symbol is BinaryOperatorSymbol)
            {
                Left.Emit(ilg);
                Right.Emit(ilg);
                (Symbol as BinaryOperatorSymbol).Emit(this, Datatype, ilg);
            }
            else
                throw new NotImplementedException();
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class UnaryExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            if (Symbol is UnaryOperatorSymbolWithMethod)
            {
                var op = (UnaryOperatorSymbolWithMethod)Symbol;
                ilg.Emit(OpCodes.Call, op.Method.Method);
            }
            else if (Symbol is UnaryOperatorSymbol)
            {
                var op = (UnaryOperatorSymbol)Symbol;
                EmitUnaryOperator(ilg, op, Datatype); // TODO nkok: handle checked/unchecked
            }
            else
            {
                throw new NotImplementedException();
            }
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class PrefixExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            ilg.Emit(OpCodes.Dup);
            Left.Symbol.EmitSet(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class PostfixExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            Left.Symbol.EmitSet(ilg);
            Temp.Emit(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class LiteralExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
            {
                EmitLiteral(ilg, (Constant)Symbol);
            }
        }
    }
    internal partial class SelfExpr : Expr
    {
        // Not supported
    }
    internal partial class SuperExpr : Expr
    {
        // Not supported
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
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
            if (preserve)
            {
                ((ConversionSymbol)Symbol).Emit(Expr,Datatype,ilg);
            }
        }
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
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Args.Emit(ilg);
            var m = (MethodSymbol)Symbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve && Datatype.NativeType != NativeType.Void)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
    }
    internal partial class Arg : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            Expr.Emit(ilg, true);
        }
    }
    internal partial class ExprList : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            for (int i = 0; i < Exprs.Count-1; i++)
            {
                Exprs[i].Emit(ilg, false);
            }
            Exprs.LastOrDefault()?.Emit(ilg, preserve);
        }
    }
    internal partial class LiteralArray : Expr
    {
    }
    internal partial class ArgList : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            foreach (var a in Args)
            {
                a.Emit(ilg);
            }
        }
    }
    internal partial class Codeblock : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            if (Params != null)
            {
                foreach (var p in Params)
                {
                    LocalSymbol ls = (LocalSymbol)p.Symbol;
                    ls.Declare(ilg);

                    var idx = Constant.Create(ls.Index);
                    var skip = ilg.DefineLabel();
                    var lidx = Constant.Create(ls.Index);

                    ParamArray.EmitGet(ilg);
                    ilg.Emit(OpCodes.Ldlen);
                    ilg.Emit(OpCodes.Conv_I4);
                    EmitLiteral(ilg, lidx);
                    ilg.Emit(OpCodes.Cgt);
                    ilg.Emit(OpCodes.Brfalse_S, skip);

                    ParamArray.EmitGet(ilg);
                    EmitLiteral(ilg, lidx);
                    ilg.Emit(OpCodes.Ldelem_Ref);
                    ls.EmitSet(ilg);

                    ilg.MarkLabel(skip);
                }
            }
            bool isVoid = true;
            if (Body != null)
            {
                isVoid &= Body.Datatype.NativeType == NativeType.Void;
                Body.Emit(ilg,true);
            }
            if (isVoid)
            {
                EmitDefault(ilg,(TypeSymbol)Symbol);
            }
            ilg.Emit(OpCodes.Ret);
        }
    }
}
