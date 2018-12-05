using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    using static CodeGen;

    internal abstract partial class Constant : TypedSymbol
    {
        internal abstract void Emit(ILGenerator ilg);
    }
    internal partial class ConstantWithValue<T> : Constant
    {
        internal override void Emit(ILGenerator ilg)
        {
            EmitLiteral(ilg, this);
        }
    }
    internal partial class ConstantVOFloat : ConstantWithValue<double>
    {
        internal override void Emit(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Ldc_R8, Double.Value);
            EmitConstant_I4(ilg, Length);
            EmitConstant_I4(ilg, Decimals);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___VOFloat_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantVODate : ConstantWithValue<DateTime>
    {
        internal override void Emit(ILGenerator ilg)
        {
            EmitConstant_I4(ilg, DateTime.Value.Year);
            EmitConstant_I4(ilg, DateTime.Value.Month);
            EmitConstant_I4(ilg, DateTime.Value.Day);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___VODate_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantVOSymbol : ConstantWithValue<string>
    {
        internal override void Emit(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Ldstr, String);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___Symbol_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantDefault : Constant
    {
        internal override void Emit(ILGenerator ilg)
        {
            EmitDefault(ilg, Type);
        }
    }
}