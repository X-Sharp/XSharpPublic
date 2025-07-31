﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;
using XSharp.RDD;

namespace XSharp.MacroCompiler
{
    internal abstract partial class Symbol
    {
        internal virtual void EmitGet(ILGenerator ilg) { throw new InternalError(); }
        internal virtual void EmitSet(ILGenerator ilg) { throw new InternalError(); }
        internal virtual void EmitAddr(ILGenerator ilg) { throw new InternalError(); }
    }
    internal abstract partial class TypedSymbol : Symbol
    {
    }
    internal partial class SymbolList : Symbol
    {
    }
    internal abstract partial class ContainerSymbol : Symbol
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
        internal virtual void Declare(ILGenerator ilg)
        {
            var lb = ilg.DeclareLocal(Type.Type);
            Index = lb.LocalIndex;
        }
    }
    internal partial class ArgumentSymbol : LocalSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { ilg.Emit(OpCodes.Ldarg, Index); }
        internal override void EmitSet(ILGenerator ilg) { ilg.Emit(OpCodes.Starg, Index); }
        internal override void EmitAddr(ILGenerator ilg) { ilg.Emit(Index < 256 ? OpCodes.Ldarga_S : OpCodes.Ldarga, Index); }
    }
    internal partial class VariableSymbol : LocalSymbol
    {
        internal override void EmitGet(ILGenerator ilg) { if (Index < 0) Declare(ilg); base.EmitGet(ilg); }
        internal override void EmitSet(ILGenerator ilg) { if (Index < 0) Declare(ilg); base.EmitSet(ilg); }
        internal override void EmitAddr(ILGenerator ilg) { if (Index < 0) Declare(ilg); base.EmitAddr(ilg); }
    }
    internal partial class MemvarSymbol : LocalSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Ldstr, Name);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___MemVarGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            var v = ilg.DeclareLocal(Compilation.Get(NativeType.Usual).Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            ilg.Emit(OpCodes.Ldstr, Name);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            MethodSymbol m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions___MemVarPut) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            ilg.Emit(OpCodes.Pop);
        }
        internal override void Declare(ILGenerator ilg) { }
    }
    internal partial class FieldAliasSymbol : LocalSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            if (WorkArea != null)
                ilg.Emit(OpCodes.Ldstr, WorkArea);
            ilg.Emit(OpCodes.Ldstr, Name);
            MethodSymbol m = WorkArea != null ?
                  Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGetWa) as MethodSymbol
                : Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldGet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            var v = ilg.DeclareLocal(Compilation.Get(NativeType.Usual).Type);
            ilg.Emit(OpCodes.Stloc, v.LocalIndex);
            if (WorkArea != null)
                ilg.Emit(OpCodes.Ldstr, WorkArea);
            ilg.Emit(OpCodes.Ldstr, Name);
            ilg.Emit(OpCodes.Ldloc, v.LocalIndex);
            MethodSymbol m = WorkArea != null ?
                  Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldSetWa) as MethodSymbol
                : Compilation.Get(WellKnownMembers.XSharp_RT_Functions___FieldSet) as MethodSymbol;
            ilg.Emit(OpCodes.Call, m.Method);
            ilg.Emit(OpCodes.Pop);
        }
        internal override void Declare(ILGenerator ilg) { }
    }
    internal partial class DynamicSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            var m = Dialect == XSharpDialect.FoxPro  ?
                Compilation.Get(WellKnownMembers.XSharp_RT_Functions_IVarGetSelf) as MethodSymbol
                : Compilation.Get(WellKnownMembers.XSharp_RT_Functions_IVarGet) as MethodSymbol;
            ilg.Emit(OpCodes.Ldstr, Name);
            ilg.Emit(OpCodes.Call, m.Method);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            var m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions_IVarPut) as MethodSymbol;
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
    internal partial class DynamicExprSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            var m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions_IVarGet) as MethodSymbol;
            Name.Emit(ilg);
            ilg.Emit(OpCodes.Call, m.Method);
        }
        internal override void EmitSet(ILGenerator ilg)
        {
            var m = Compilation.Get(WellKnownMembers.XSharp_RT_Functions_IVarPut) as MethodSymbol;
            var lo = ilg.DeclareLocal(Compilation.Get(NativeType.Object).Type);
            var lv = ilg.DeclareLocal(Type.Type);
            ilg.Emit(OpCodes.Stloc, lo.LocalIndex);
            ilg.Emit(OpCodes.Stloc, lv.LocalIndex);
            ilg.Emit(OpCodes.Ldloc, lo.LocalIndex);
            Name.Emit(ilg);
            ilg.Emit(OpCodes.Ldloc, lv.LocalIndex);
            ilg.Emit(OpCodes.Call, m.Method);
            ilg.Emit(OpCodes.Pop);
        }
    }
    internal partial class ObjectInitializerSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            var l = ilg.DeclareLocal(Type.Type);
            ilg.Emit(l.LocalIndex < 256 ? OpCodes.Ldloca_S : OpCodes.Ldloca, l);
            ilg.Emit(OpCodes.Initobj, Type.Type);
            ilg.Emit(l.LocalIndex < 256 ? OpCodes.Ldloc_S : OpCodes.Ldloc, l);
        }
    }
    internal abstract partial class MemberSymbol : TypedSymbol
    {
    }
    internal abstract partial class MethodBaseSymbol : MemberSymbol
    {
    }
    internal partial class MethodSymbol : MethodBaseSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            if (DeclaringType.IsValueType)
                ilg.Emit(OpCodes.Call, Method);
            else
                ilg.Emit(IsVirtual ? OpCodes.Callvirt : OpCodes.Call, Method);
        }
    }
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Newobj, Constructor);
        }
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
