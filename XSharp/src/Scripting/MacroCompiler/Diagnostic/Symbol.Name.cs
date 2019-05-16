using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    internal abstract partial class Symbol
    {
        internal abstract string FullName { get; }
        public override string ToString() => FullName;
    }
    internal abstract partial class TypedSymbol : Symbol
    {
    }
    internal partial class SymbolList : Symbol
    {
        internal override string FullName { get { return "<symbol group>"; } }
    }
    internal abstract partial class ContainerSymbol : Symbol
    {
    }
    internal partial class NamespaceSymbol : ContainerSymbol
    {
        internal override string FullName { get { return string.IsNullOrEmpty(ParentNamespace?.FullName) ? Name : ParentNamespace.FullName + "." + Name; } }
    }
    internal partial class TypeSymbol : ContainerSymbol
    {
        string namePrefix
        {
            get
            {
                var n = Namespace?.FullName ?? DeclaringType?.FullName;
                return string.IsNullOrEmpty(n) ? "" : n + ".";
            }
        }
        internal override string FullName { get { return Compilation.NativeTypeName(NativeType) ?? namePrefix + Type.Name; } }
    }
    internal partial class LocalSymbol : TypedSymbol
    {
        internal override string FullName { get { return Name ?? "<unnamed>"; } }
    }
    internal partial class ArgumentSymbol : LocalSymbol
    {
    }
    internal partial class VariableSymbol : LocalSymbol
    {
    }
    internal partial class DynamicSymbol : TypedSymbol
    {
        internal override string FullName { get { return Name; } }
    }
    internal partial class DynamicExprSymbol : TypedSymbol
    {
        internal override string FullName { get { return Name.ToString(); } }
    }
    internal partial class ObjectInitializerSymbol : TypedSymbol
    {
        internal override string FullName { get { return Type.FullName + "{}"; } }
    }
    internal abstract partial class MemberSymbol : TypedSymbol
    {
        internal override string FullName { get { return Type.FullName + "." + Member.Name; } }
    }
    internal partial class ParameterListSymbol : Symbol
    {
        internal override string FullName { get { return "<parameter-list>"; } }
    }
    internal abstract partial class MethodBaseSymbol : MemberSymbol
    {
    }
    internal partial class MethodSymbol : MethodBaseSymbol
    {
        internal override string FullName { get { return ContainingType.FullName + "." + Method.Name + "()"; } }
    }
    internal partial class FieldSymbol : MemberSymbol
    {
        internal override string FullName { get { return ContainingType.FullName + (Field.IsStatic ? "." : ":") + Field.Name; } }
    }
    internal partial class EventSymbol : MemberSymbol
    {
        internal override string FullName { get { return ContainingType.FullName + "." + Event.Name; } }
    }
    internal partial class PropertySymbol : MemberSymbol
    {
        internal override string FullName { get { return ContainingType.FullName + (IsStatic ? "." : ":") + Property.Name; } }
    }
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
        internal override string FullName { get { return Type.FullName + "{}"; } }
    }
}
