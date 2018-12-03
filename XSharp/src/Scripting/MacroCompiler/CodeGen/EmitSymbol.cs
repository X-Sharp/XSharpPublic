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
        internal virtual void EmitAddr(ILGenerator ilg) { throw new NotImplementedException(); }
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
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(Index < 256 ? OpCodes.Ldloc_S : OpCodes.Ldloc, Index); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(Index < 256 ? OpCodes.Stloc_S : OpCodes.Stloc, Index); }
        internal override void EmitAddr(ILGenerator ilg) { ilg.Emit(Index < 256 ? OpCodes.Ldloca_S : OpCodes.Ldloca, Index); }
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
        internal override void EmitAddr(ILGenerator ilg) { ilg.Emit(Index < 256 ? OpCodes.Ldarga_S : OpCodes.Ldarga, Index); }
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
            ilg.Emit(OpCodes.Pop);
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
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
    }
    internal partial class FieldSymbol : MemberSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(Field.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, Field); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(Field.IsStatic ? OpCodes.Stsfld : OpCodes.Stfld, Field); }
        internal override void EmitAddr(ILGenerator ilg) { ilg.Emit(Field.IsStatic ? OpCodes.Ldsflda : OpCodes.Ldflda, Field); }
    }
    internal partial class EventSymbol : MemberSymbol
    {
    }
    internal partial class PropertySymbol : MemberSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(Getter.IsStatic ? OpCodes.Call : OpCodes.Callvirt, Getter.Method); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(Setter.IsStatic ? OpCodes.Call : OpCodes.Callvirt, Setter.Method); }
    }
}
