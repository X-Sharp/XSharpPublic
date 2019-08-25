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
    }
    internal partial class ConstantWithValue<T> : Constant
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            EmitLiteral(ilg, this);
        }
    }
    internal partial class ConstantVOFloat : ConstantWithValue<double>
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Ldc_R8, Double.Value);
            EmitConstant_I4(ilg, Length);
            EmitConstant_I4(ilg, Decimals);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___Float_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantVODate : ConstantWithValue<DateTime>
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            EmitConstant_I4(ilg, DateTime.Value.Year);
            EmitConstant_I4(ilg, DateTime.Value.Month);
            EmitConstant_I4(ilg, DateTime.Value.Day);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___Date_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantVOSymbol : ConstantWithValue<string>
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Ldstr, String);
            ilg.Emit(OpCodes.Newobj, (Compilation.Get(WellKnownMembers.XSharp___Symbol_ctor) as ConstructorSymbol).Constructor);
        }
    }
    internal partial class ConstantDefault : Constant
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            EmitDefault(ilg, Type);
        }
    }
}
