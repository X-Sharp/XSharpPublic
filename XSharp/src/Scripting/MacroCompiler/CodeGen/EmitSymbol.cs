using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler
{
    internal abstract partial class Symbol
    {
        internal virtual void EmitGet(ILGenerator ilg) { throw new NotImplementedException(); }
        internal virtual void EmitSet(ILGenerator ilg) { throw new NotImplementedException(); }
        internal virtual void EmitGetAddr(ILGenerator ilg) { throw new NotImplementedException(); }
    }
    internal abstract partial class TypedSymbol : Symbol
    {
    }
    internal partial class SymbolList : Symbol
    {
    }
    internal partial class ContainerSymbol : Symbol
    {
    }
    internal partial class NamespaceSymbol : ContainerSymbol
    {
    }
    internal partial class TypeSymbol : ContainerSymbol
    {
    }
    internal partial class LocalSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(OpCodes.Ldloc, Index); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(OpCodes.Stloc, Index); }
        internal override void EmitGetAddr(ILGenerator ilg) { ilg.Emit(OpCodes.Ldloca, Index); }
        internal void Declare(ILGenerator ilg)
        {
            var lb = ilg.DeclareLocal(Type.Type);
            Index = lb.LocalIndex;
        }
    }
    internal partial class ParameterSymbol : LocalSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(OpCodes.Ldarg, Index); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(OpCodes.Starg, Index); }
    }
    internal partial class DynamicSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            var m = (Binder.LookupFullName(XSharpQualifiedFunctionNames.IVarGet) ?? Binder.LookupFullName(VulcanQualifiedFunctionNames.IVarGet)) as MethodSymbol;
            ilg.Emit(OpCodes.Ldstr, Name);
            ilg.Emit(OpCodes.Call, m.Method);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            var m = (Binder.LookupFullName(XSharpQualifiedFunctionNames.IVarPut) ?? Binder.LookupFullName(VulcanQualifiedFunctionNames.IVarPut)) as MethodSymbol;
            var lo = ilg.DeclareLocal(Compilation.Get(NativeType.Object).Type);
            var lv = ilg.DeclareLocal(Type.Type);
            ilg.Emit(OpCodes.Stloc, lo.LocalIndex);
            ilg.Emit(OpCodes.Stloc, lv.LocalIndex);
            ilg.Emit(OpCodes.Ldloc, lo.LocalIndex);
            ilg.Emit(OpCodes.Ldstr, Name);
            ilg.Emit(OpCodes.Ldloc, lv.LocalIndex);
            ilg.Emit(OpCodes.Call, m.Method);
        }
    }
    internal partial class MemberSymbol : TypedSymbol
    {
    }
    internal partial class MethodBaseSymbol : MemberSymbol
    {
    }
    internal partial class MethodSymbol : MethodBaseSymbol
    {
    }
    internal partial class FieldSymbol : MemberSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            if (Field.IsStatic)
                ilg.Emit(OpCodes.Ldsfld, Field);
            else
                ilg.Emit(OpCodes.Ldfld, Field);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            if (Field.IsStatic)
                ilg.Emit(OpCodes.Stsfld, Field);
            else
                ilg.Emit(OpCodes.Stfld, Field);
        }
    }
    internal partial class EventSymbol : MemberSymbol
    {
    }
    internal partial class PropertySymbol : MemberSymbol
    {
    }
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
    }
}
