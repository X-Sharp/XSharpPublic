using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    internal abstract partial class Constant : TypedSymbol
    {
        internal Constant() : base(AccessMode.Get) { }

        internal static ConstantWithValue<T> Create<T>(T value, NativeType nt) { return new ConstantWithValue<T>(value, nt); }
        internal static ConstantWithValue<T> Create<T>(T value, TypeSymbol t) { return new ConstantWithValue<T>(value, t); }
        internal static ConstantWithValue<bool> Create(bool value) { return new ConstantWithValue<bool>(value, NativeType.Boolean); }
        internal static ConstantWithValue<int> Create(int value) { return new ConstantWithValue<int>(value, NativeType.Int32); }
        internal static ConstantWithValue<long> Create(long value) { return new ConstantWithValue<long>(value, NativeType.Int64); }
        internal static ConstantWithValue<uint> Create(uint value) { return new ConstantWithValue<uint>(value, NativeType.UInt32); }
        internal static ConstantWithValue<ulong> Create(ulong value) { return new ConstantWithValue<ulong>(value, NativeType.UInt64); }
        internal static ConstantWithValue<float> Create(float value) { return new ConstantWithValue<float>(value, NativeType.Single); }
        internal static ConstantWithValue<double> Create(double value) { return new ConstantWithValue<double>(value, NativeType.Double); }
        internal static ConstantWithValue<object> Create(object value) { return new ConstantWithValue<object>(value, NativeType.Object); }
        internal static ConstantWithValue<decimal> Create(decimal value) { return new ConstantWithValue<decimal>(value, NativeType.Decimal); }
        internal static ConstantWithValue<string> Create(string value) { return new ConstantWithValue<string>(value, NativeType.String); }
        internal static ConstantWithValue<DateTime> Create(DateTime value) { return new ConstantWithValue<DateTime>(value, NativeType.DateTime); }
        internal static ConstantVOFloat Create(double value, int length, int decimals) { return new ConstantVOFloat(value, length, decimals); }
        internal static ConstantVODate CreateVODate(int year, int month, int day) { return new ConstantVODate(year, month, day); }
        internal static ConstantVOSymbol CreateSymbol(string value) { return new ConstantVOSymbol(value); }
        internal static ConstantDefault CreateDefault(TypeSymbol type) { return new ConstantDefault(type); }

        internal static ConstantDefault Null { get { return CreateDefault(Compilation.Get(NativeType.Object)); } }
        internal static ConstantDefault Nil { get { return CreateDefault(Compilation.Get(NativeType.Usual)); } }

        internal virtual bool? Boolean { get; }
        internal virtual int? Int { get; }
        internal virtual long? Long { get; }
        internal virtual uint? UInt { get; }
        internal virtual ulong? ULong { get; }
        internal virtual float? Float { get; }
        internal virtual double? Double { get; }
        internal virtual decimal? Decimal { get; }
        internal virtual string String { get; }
        internal virtual DateTime? DateTime { get; }
    }
    internal partial class ConstantWithValue<T> : Constant
    {
        T Value;
        internal ConstantWithValue(T value, NativeType nt) { Value = value; Type = Compilation.Get(nt); }
        internal ConstantWithValue(T value, TypeSymbol t) { Value = value; Type = t; }

        internal override TypeSymbol Type { get; }

        internal override bool? Boolean { get { return unchecked(Value as bool?); } }
        internal override int? Int { get { return unchecked(Value as int?); } }
        internal override long? Long { get { return unchecked(Value as long?); } }
        internal override uint? UInt { get { return unchecked(Value as uint?); } }
        internal override ulong? ULong { get { return unchecked(Value as ulong?); } }
        internal override float? Float { get { return unchecked(Value as float?); } }
        internal override double? Double { get { return unchecked(Value as double?); } }
        internal override decimal? Decimal { get { return unchecked(Value as decimal?); } }
        internal override string String { get { return Value as string; } }
        internal override DateTime? DateTime { get { return Value as DateTime?; } }

        internal override string FullName { get { return Value.ToString(); } }
    }
    internal partial class ConstantVOFloat : ConstantWithValue<double>
    {
        int Length;
        int Decimals;
        internal ConstantVOFloat(double value, int length, int decimals) : base(value, NativeType.VOFloat)
        {
            Length = length;
            Decimals = decimals;
        }
    }
    internal partial class ConstantVODate: ConstantWithValue<DateTime>
    {
        internal ConstantVODate(int year, int month, int day) : base(new DateTime(year, month, day), NativeType.VODate) { }
    }
    internal partial class ConstantVOSymbol : ConstantWithValue<string>
    {
        internal ConstantVOSymbol(string value) : base(value, NativeType.Symbol) {}
    }
    internal partial class ConstantDefault : Constant
    {
        internal ConstantDefault(TypeSymbol type) { Type = type; }

        internal override TypeSymbol Type { get; }

        internal override string FullName { get { return "default(" + Type.FullName + ")"; } }
    }
}
