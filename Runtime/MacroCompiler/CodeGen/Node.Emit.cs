using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler.Syntax
{
    using static CodeGen;

    abstract internal partial class Node
    {
        internal virtual void Emit(ILGenerator ilg) { throw new InternalError(); }
    }
    abstract internal partial class Expr : Node
    {
        internal virtual void Emit(ILGenerator ilg, bool preserve) { throw new InternalError(); }
        internal virtual void EmitSet(ILGenerator ilg, bool preserve) { throw new InternalError(); }
        internal virtual void EmitAddr(ILGenerator ilg) { throw new InternalError(); }
        internal sealed override void Emit(ILGenerator ilg) { Emit(ilg, true); }
    }
    abstract internal partial class TypeExpr : Expr
    {
    }
    abstract internal partial class NameExpr : TypeExpr
    {
    }
    internal partial class CachedExpr : Expr
    {
        bool emitted = false;
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (emitted)
            {
                if (preserve)
                    Local.EmitGet(ilg);
            }
            else
            {
                Expr.Emit(ilg);
                if (preserve)
                    ilg.Emit(OpCodes.Dup);
                Local.Declare(ilg);
                Local.EmitSet(ilg);
                emitted = true;
            }
        }
        internal override void EmitAddr(ILGenerator ilg)
        {
            if (!emitted)
            {
                Expr.Emit(ilg);
                Local.Declare(ilg);
                Local.EmitSet(ilg);
            }
            Local.EmitAddr(ilg);
        }
    }
    internal partial class NativeTypeExpr : TypeExpr
    {
    }
    internal partial class IdExpr : NameExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                Symbol.EmitGet(ilg);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Symbol.EmitSet(ilg);
        }
        internal override void EmitAddr(ILGenerator ilg)
        {
            Symbol.EmitAddr(ilg);
        }
    }
    internal partial class MemberAccessExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            Symbol.EmitGet(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Expr.Emit(ilg);
            Symbol.EmitSet(ilg);
        }
    }
    internal partial class ArrayLengthExpr : MemberAccessExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
            if (preserve)
            {
                ilg.Emit(OpCodes.Ldlen);
                ilg.Emit(OpCodes.Conv_I4);
            }
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve) => throw new InternalError();
    }
    internal partial class QualifiedNameExpr : NameExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Symbol.EmitGet(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            Symbol.EmitSet(ilg);
        }
    }
    internal partial class AssignExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Right.Emit(ilg);
            Left.EmitSet(ilg, preserve);
        }
    }
    internal partial class AssignOpExpr : AssignExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Right.Emit(ilg);
            Left.EmitSet(ilg, preserve);
        }
    }
    internal partial class BinaryExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Left.Emit(ilg);
            Right.Emit(ilg);
            Symbol.EmitGet(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class BinaryLogicExpr : BinaryExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Symbol is BinaryOperatorSymbol)
            {
                var lb = ilg.DefineLabel();
                var op = (Symbol as BinaryOperatorSymbol);
                Left.Emit(ilg);
                ilg.Emit(OpCodes.Dup);
                switch (op.Kind)
                {
                    case BinaryOperatorKind.And:
                        ilg.Emit(OpCodes.Brfalse, lb);
                        break;
                    case BinaryOperatorKind.Or:
                        ilg.Emit(OpCodes.Brtrue, lb);
                        break;
                    default:
                        throw new InternalError();
                }
                ilg.Emit(OpCodes.Pop);
                Right.Emit(ilg);
                ilg.MarkLabel(lb);
            }
            else
                throw new InternalError();
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class UnaryExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            Symbol.EmitGet(ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class PrefixExpr : UnaryExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            Left.EmitSet(ilg, preserve);
        }
    }
    internal partial class PostfixExpr : UnaryExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            Left.EmitSet(ilg, false);
            Value.Emit(ilg, preserve);
        }
    }
    internal partial class LiteralExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
            {
                ((Constant)Symbol).EmitGet(ilg);
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
        // TODO (nvk): to be implemented
    }
    internal partial class UncheckedExpr : Expr
    {
        // TODO (nvk): to be implemented
    }
    internal partial class TypeOfExpr : Expr
    {
        // TODO (nvk): to be implemented
    }
    internal partial class SizeOfExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ilg.Emit(OpCodes.Sizeof, (Symbol as TypeSymbol).Type);
        }
    }
    internal partial class DefaultExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                EmitDefault(ilg, (TypeSymbol)Symbol);
        }
    }
    internal partial class TypeCast : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            ((ConversionSymbol)Symbol).Emit(Expr, Datatype, ilg);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class TypeConversion : TypeCast
    {
    }
    internal partial class IsExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
            if (preserve)
            {
                ilg.Emit(OpCodes.Isinst, (Symbol as TypeSymbol).Type);
                ilg.Emit(OpCodes.Ldnull);
                ilg.Emit(OpCodes.Cgt_Un);
            }
        }
    }
    internal partial class IsVarExpr : IsExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
            if (preserve)
            {
                Var.Declare(ilg);
                if (Check == null)
                {
                    ilg.Emit(OpCodes.Isinst, (Symbol as TypeSymbol).Type);
                    ilg.Emit(OpCodes.Dup);
                    Var.EmitSet(ilg);
                    ilg.Emit(OpCodes.Ldnull);
                    ilg.Emit(OpCodes.Cgt_Un);
                }
                else
                {
                    Var.EmitSet(ilg);
                    if (Check == true)
                        ilg.Emit(OpCodes.Ldc_I4_1);
                    else
                        ilg.Emit(OpCodes.Ldc_I4_0);
                }
            }
        }
    }
    internal partial class AsTypeExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
            if (preserve)
            {
                ilg.Emit(OpCodes.Isinst, Datatype.Type);
            }
        }
    }
    internal partial class MethodCallExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Self != null) Self.Emit(ilg);
            Args.Emit(ilg);
            Symbol.EmitGet(ilg);
            if (WriteBack != null)
                WriteBack.Emit(ilg, false);
            if (!preserve && Datatype.NativeType != NativeType.Void)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class CtorCallExpr : MethodCallExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Args.Emit(ilg);
            Symbol.EmitGet(ilg);
            if (WriteBack != null)
                WriteBack.Emit(ilg, false);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class IntrinsicCallExpr : MethodCallExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            switch (Kind)
            {
                case IntrinsicCallType.GetFParam:
                case IntrinsicCallType.GetMParam:
                    Args.Emit(ilg);
                    EmitGetElemSafe(ilg, Symbol, Datatype);
                    break;
                case IntrinsicCallType.Chr:
                    Args.Emit(ilg);
                    Symbol.EmitGet(ilg);
                    break;
                default:
                    throw new InternalError();
            }
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class ArrayAccessExpr : MethodCallExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Self != null) Self.Emit(ilg);
            Args.Emit(ilg);
            var m = (PropertySymbol)Symbol;
            ilg.Emit(Self == null ? OpCodes.Call : OpCodes.Callvirt, m.Getter.Method);
            if (!preserve && Datatype.NativeType != NativeType.Void)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            var m = (PropertySymbol)Symbol;
            bool isVoid = m.Setter.Type.NativeType == NativeType.Void;
            if (preserve && isVoid)
                ilg.Emit(OpCodes.Dup);
            var t = ilg.DeclareLocal(Datatype.Type);
            ilg.Emit(OpCodes.Stloc, t.LocalIndex);
            if (Self != null) Self.Emit(ilg);
            if (m.ValueLast)
            {
                Args.Emit(ilg);
                ilg.Emit(OpCodes.Ldloc, t.LocalIndex);
            }
            else
            {
                ilg.Emit(OpCodes.Ldloc, t.LocalIndex);
                Args.Emit(ilg);
            }
            ilg.Emit(Self == null ? OpCodes.Call : OpCodes.Callvirt, m.Setter.Method);
            if (!preserve && !isVoid)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class NativeArrayAccessExpr : ArrayAccessExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Self != null) Self.Emit(ilg);
            Args.Emit(ilg);
            ilg.Emit(OpCodes.Ldelem, Datatype.Type);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ilg.Emit(OpCodes.Dup);
            var t = ilg.DeclareLocal(Datatype.Type);
            ilg.Emit(OpCodes.Stloc, t.LocalIndex);
            if (Self != null) Self.Emit(ilg);
            Args.Emit(ilg);
            ilg.Emit(OpCodes.Ldloc, t.LocalIndex);
            ilg.Emit(OpCodes.Stelem, Datatype.Type);
        }
    }
    internal partial class EmptyExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (preserve)
                ((Constant)Symbol).EmitGet(ilg);
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
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            EmitConstant_I4(ilg, Values.Exprs.Count);
            ilg.Emit(OpCodes.Newarr, (Symbol as TypeSymbol).Type);
            for (int i = 0; i < Values.Exprs.Count; i++)
            {
                ilg.Emit(OpCodes.Dup);
                EmitConstant_I4(ilg, i);
                Values.Exprs[i].Emit(ilg);
                EmitArrayStoreElem(ilg, Symbol as TypeSymbol);
            }
            if (Datatype.NativeType == NativeType.Array)
            {
                ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___Array_ctor) as ConstructorSymbol).Constructor);
            }
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class IifExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Cond.Emit(ilg);
            var lbFalse = ilg.DefineLabel();
            var lbTrue = ilg.DefineLabel();
            ilg.Emit(OpCodes.Brfalse, lbFalse);
            True.Emit(ilg);
            ilg.Emit(OpCodes.Br, lbTrue);
            ilg.MarkLabel(lbFalse);
            False.Emit(ilg);
            ilg.MarkLabel(lbTrue);

            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class MacroExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions_Evaluate) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }

    internal partial class MacroId
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Id.Emit(ilg);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___VarGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class MemvarExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Var.Emit(ilg);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___MemVarGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            var v = ilg.DeclareLocal(Compilation.Get(NativeType.Usual).Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            Var.Emit(ilg);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___MemVarPut) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class AliasExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            if (Alias != null)
                Alias.Emit(ilg);
            Field.Emit(ilg);
            MethodSymbol m;
            if (Alias != null)
                m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGetWa) as MethodSymbol;
            else
                m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            var v = ilg.DeclareLocal(Compilation.Get(NativeType.Usual).Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            if (Alias != null)
                Alias.Emit(ilg);
            Field.Emit(ilg);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            MethodSymbol m;
            if (Alias != null)
                m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldSetWa) as MethodSymbol;
            else
                m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldSet) as MethodSymbol;

            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class AliasWaExpr : AliasExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            var push = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___pushWorkarea) as MethodSymbol;
            var pop = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___popWorkarea) as MethodSymbol;
            var dirty = ilg.DeclareLocal(Compilation.Get(NativeType.Boolean).Type);
            Alias.Emit(ilg);
            ilg.Emit(OpCodes.Call, push.Method);
            ilg.Emit(OpCodes.Ldc_I4_1);
            ilg.Emit(OpCodes.Stloc, dirty.LocalIndex);
            Field.Emit(ilg, preserve);
            ilg.Emit(OpCodes.Ldc_I4_0);
            ilg.Emit(OpCodes.Stloc, dirty.LocalIndex);
            ilg.Emit(OpCodes.Call, pop.Method);
            Stmt.FinallyClauses.Add(() => {
                var skip = ilg.DefineLabel();
                ilg.Emit(OpCodes.Ldloc, dirty.LocalIndex);
                ilg.Emit(OpCodes.Brfalse_S, skip);
                ilg.Emit(OpCodes.Call, pop.Method);
                ilg.MarkLabel(skip);
            });
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            var push = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___pushWorkarea) as MethodSymbol;
            var pop = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___popWorkarea) as MethodSymbol;
            var dirty = ilg.DeclareLocal(Compilation.Get(NativeType.Boolean).Type);
            var v = ilg.DeclareLocal(Compilation.Get(NativeType.Usual).Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            Alias.Emit(ilg);
            ilg.Emit(OpCodes.Call, push.Method);
            ilg.Emit(OpCodes.Ldc_I4_1);
            ilg.Emit(OpCodes.Stloc, dirty.LocalIndex);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            Field.EmitSet(ilg, preserve);
            ilg.Emit(OpCodes.Ldc_I4_0);
            ilg.Emit(OpCodes.Stloc, dirty.LocalIndex);
            ilg.Emit(OpCodes.Call, pop.Method);
            Stmt.FinallyClauses.Add(() => {
                var skip = ilg.DefineLabel();
                ilg.Emit(OpCodes.Ldloc, dirty.LocalIndex);
                ilg.Emit(OpCodes.Brfalse_S, skip);
                ilg.Emit(OpCodes.Call, pop.Method);
                ilg.MarkLabel(skip);
            });
        }
        internal void EmitFinallyBlock() { }
    }
    internal partial class RuntimeIdExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Expr.Emit(ilg, preserve);
        }
    }
    internal partial class SubstrExpr : BinaryExpr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Left.Emit(ilg);
            Right.Emit(ilg);
            var m = Symbol as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class AutoVarExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            Var.Emit(ilg);
            var m = Compilation.Get(Safe ? WellKnownMembers.XSharp_RT_Functions___VarGetSafe : WellKnownMembers.XSharp_RT_Functions___VarGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
        internal override void EmitSet(ILGenerator ilg, bool preserve)
        {
            var v = ilg.DeclareLocal(Datatype.Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            Var.Emit(ilg);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            var m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___VarPut) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class Arg : Node
    {
        internal override void Emit(ILGenerator ilg)
        {
            Expr.Emit(ilg, true);
        }
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
                    lidx.EmitGet(ilg);
                    ilg.Emit(OpCodes.Cgt);
                    ilg.Emit(OpCodes.Brfalse, skip);

                    ParamArray.EmitGet(ilg);
                    lidx.EmitGet(ilg);
                    if (ls.Type.IsReferenceType)
                        ilg.Emit(OpCodes.Ldelem_Ref);
                    else
                        ilg.Emit(OpCodes.Ldelem,ls.Type.Type);
                    ls.EmitSet(ilg);

                    ilg.MarkLabel(skip);
                }
            }
            if (PCount != null)
            {
                PCount.Declare(ilg);
                ParamArray.EmitGet(ilg);
                ilg.Emit(OpCodes.Ldlen);
                ilg.Emit(OpCodes.Conv_I4);
                PCount.EmitSet(ilg);
            }
            Body.Emit(ilg);
            if (Symbol != null) Symbol.EmitGet(ilg);
            ilg.Emit(OpCodes.Ret);
        }
    }
    internal partial class CodeblockExpr : Expr
    {
        internal override void Emit(ILGenerator ilg, bool preserve)
        {
            // Emit nested codeblock
            var source = Token.ToString();
            if (usualMacro)
            {
                var dlg = NestedBinder.Emit(Codeblock, source) as UsualMacro.MacroCodeblockDelegate;
                if (NestedBinder.CreatesAutoVars)
                    CbList[CbIndex] = new UsualMacro.MacroMemVarCodeblock(dlg, NestedBinder.ParamCount, source, true);
                else
                    CbList[CbIndex] = new UsualMacro.MacroCodeblock(dlg, NestedBinder.ParamCount, source, true);
            }
            else
            {
                var dlg = NestedBinder.Emit(Codeblock, source) as ObjectMacro.MacroCodeblockDelegate;
                ICodeblock rtc = NestedBinder.CreatesAutoVars ? new ObjectMacro.MacroMemVarCodeblock(dlg, NestedBinder.ParamCount)
                                                              : new ObjectMacro.MacroCodeblock(dlg, NestedBinder.ParamCount);
                CbList[CbIndex] = new _Codeblock(rtc, source, true, false);
            }

            // Emit code to retrieve codeblock
            var lidx = Constant.Create(CbIndex);
            Symbol.EmitGet(ilg);
            lidx.EmitGet(ilg);
            ilg.Emit(OpCodes.Ldelem, Datatype.Type);
            if (!preserve)
                ilg.Emit(OpCodes.Pop);
        }
    }
}
