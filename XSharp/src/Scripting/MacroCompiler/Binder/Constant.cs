using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    internal class Constant : Symbol
    {
        internal static ConstantWithValue<T> Create<T>(T value) { return new ConstantWithValue<T>(value); }
        internal override Symbol Lookup(string name) { throw new NotImplementedException(); }

        internal virtual Type Type { get; }

        internal virtual int? Int { get; }
        internal virtual long? Long { get; }
        internal virtual float? Float { get; }
        internal virtual double? Double { get; }
        internal virtual string String { get; }
    }
    internal class ConstantWithValue<T> : Constant
    {
        T Value;
        internal ConstantWithValue(T value) { Value = value; }

        internal override Type Type { get { return typeof(T); } }

        internal override int? Int { get { return unchecked(Value as int?); } }
        internal override long? Long { get { return unchecked(Value as long?); } }
        internal override float? Float { get { return unchecked(Value as float?); } }
        internal override double? Double { get { return unchecked(Value as double?); } }
        internal override string String { get { return Value as string; } }

        public override string ToString() { return Value.ToString(); }
    }
}