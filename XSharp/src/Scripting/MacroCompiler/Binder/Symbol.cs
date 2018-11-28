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
        internal abstract Symbol Lookup(string name);
    }
    internal abstract partial class TypedSymbol : Symbol
    {
        abstract internal TypeSymbol Type { get; }
    }
    internal partial class SymbolList : Symbol
    {
        internal MemberTypes SymbolTypes;
        internal List<Symbol> Symbols;
        internal SymbolList() { Symbols = new List<Symbol>(); }
        internal SymbolList(Symbol s): this() { Add(s); SymbolTypes = 0; }
        internal void Add(Symbol s) { Symbols.Add(s); SymbolTypes |= (s as MemberSymbol)?.Member.MemberType ?? 0; }
        internal override Symbol Lookup(string name)
        {
            foreach (var s in Symbols)
            {
                var v = s.Lookup(name);
                if (v != null)
                    return v;
            }
            return null;
        }
    }
    internal partial class ContainerSymbol : Symbol
    {
        internal Dictionary<string, Symbol> Members = new Dictionary<string, Symbol>(StringComparer.OrdinalIgnoreCase);
        internal ContainerSymbol() { }
        internal override Symbol Lookup(string name) { Symbol s; Members.TryGetValue(name, out s); return s; }
    }
    internal partial class NamespaceSymbol : ContainerSymbol
    {
        internal string Name;
        internal NamespaceSymbol(string name) { Name = name; }
        internal NamespaceSymbol() { Name = null; }
    }
    internal partial class TypeSymbol : ContainerSymbol
    {
        bool Cached = false;
        internal Type Type;
        internal NativeType NativeType;
        internal TypeSymbol(Type type) { Type = type; }
        internal void UpdateCache()
        {
            if (Cached)
                return;
            var flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public;
            if (Type.Assembly == Assembly.GetEntryAssembly())
            {
                flags |= BindingFlags.NonPublic;
            }
            foreach(var m in Type.GetMembers(flags))
            {
                Symbol s = null;
                if (Members.TryGetValue(m.Name, out s))
                {
                    if (!(s is SymbolList))
                    {
                        s = new SymbolList(s);
                        Members[m.Name] = s;
                    }
                    (s as SymbolList).Add(MemberSymbol.Create(m));
                }
                else
                    Members[m.Name] = MemberSymbol.Create(m);
            }
            Cached = true;
        }
        internal override Symbol Lookup(string name)
        {
            UpdateCache();
            return base.Lookup(name);
        }
        internal bool IsNullableType()
        {
            return Type.IsGenericType && Type.GetGenericTypeDefinition() == typeof(Nullable<>);
        }
        private TypeSymbol _nullableUnderlyingType;
        internal TypeSymbol NullableUnderlyingType
        {
            get
            {
                if (_nullableUnderlyingType == null)
                {
                    if (IsNullableType())
                        _nullableUnderlyingType = Binder.FindType(Type.GetGenericArguments()[0]);
                }
                return _nullableUnderlyingType;
            }
        }
    }
    internal partial class LocalSymbol : TypedSymbol
    {
        internal string Name;
        internal override TypeSymbol Type { get; }
        internal int Index = -1;
        internal LocalSymbol(string name, TypeSymbol type) { Name = name; Type = type; }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal partial class ParameterSymbol : LocalSymbol
    {
        internal ParameterSymbol(string name, TypeSymbol type, int index) : base(name, type) { Index = index; }
    }
    internal partial class DynamicSymbol : TypedSymbol
    {
        internal string Name;
        internal override TypeSymbol Type { get { return (Binder.LookupFullName(XSharpQualifiedFunctionNames.IVarGet) as MethodSymbol)?.Type ?? Compilation.Get(NativeType.Object); } }
        internal DynamicSymbol(string name) { Name = name; }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal partial class MemberSymbol : TypedSymbol
    {
        internal MemberInfo Member;
        internal override TypeSymbol Type { get; }
        internal MemberSymbol(MemberInfo member, TypeSymbol type) { Member = member; Type = type; }
        internal override Symbol Lookup(string name) { return null; }
        internal static MemberSymbol Create(MemberInfo member)
        {
            switch (member.MemberType)
            {
                case MemberTypes.Method:
                    return new MethodSymbol((MethodInfo)member);
                case MemberTypes.Field:
                    return new FieldSymbol((FieldInfo)member);
                case MemberTypes.Event:
                    return new EventSymbol((EventInfo)member);
                case MemberTypes.Property:
                    return new PropertySymbol((PropertyInfo)member);
                case MemberTypes.Constructor:
                    return new ConstructorSymbol((ConstructorInfo)member);
                default:
                    return new MemberSymbol(member, null);
            }
        }
    }
    internal partial class MethodBaseSymbol : MemberSymbol
    {
        internal MethodBase MethodBase { get { return (MethodBase)base.Member; } }
        bool _foundAttributes = false;
        CustomAttributeData _clipperAttr = null;
        string[] _clipperParams = null;
        bool _hasParamArray = false;
        //internal ParameterInfo[] Parameters { get { Interlocked.CompareExchange(ref _parameters, Method.GetParameters(), null); return _parameters; } }
        //ParameterInfo[] _parameters;
        internal MethodBaseSymbol(MethodBase method, TypeSymbol type) : base(method, type) { }
        internal bool IsClipper { get { FindAttributes(); return _clipperAttr != null; } }
        internal string[] ClipperParams { get { FindAttributes(); return _clipperParams; } }
        internal bool HasParamArray { get { FindAttributes(); return _hasParamArray; } }
        void FindAttributes()
        {
            if (!_foundAttributes)
            {
                if (MethodBase.CustomAttributes != null)
                {
                    foreach (var attr in MethodBase.CustomAttributes)
                    {
                        if (attr.AttributeType == Compilation.Get(WellKnownTypes.ClipperCallingConventionAttribute).Type)
                        {
                            _clipperAttr = attr;
                            _clipperParams = (string[])_clipperAttr?.ConstructorArguments[0].Value;
                        }
                    }
                }
                var attrs = ((MethodBase)Member).GetParameters().LastOrDefault()?.CustomAttributes;
                if (attrs != null)
                {
                    foreach (var attr in attrs)
                    {
                        if (attr.AttributeType == typeof(System.ParamArrayAttribute))
                        {
                            _hasParamArray = true;
                        }
                    }
                }
                _foundAttributes = true;
            }
        }
    }
    internal partial class MethodSymbol : MethodBaseSymbol
    {
        internal MethodInfo Method { get { return (MethodInfo)base.Member; } }
        internal MethodSymbol(MethodInfo method) : base(method, Binder.FindType(method.ReturnType)) { }
    }
    internal partial class FieldSymbol : MemberSymbol
    {
        internal FieldInfo Field { get { return (FieldInfo)base.Member; } }
        internal FieldSymbol(FieldInfo field) : base(field, Binder.FindType(field.FieldType)) { }
    }
    internal partial class EventSymbol : MemberSymbol
    {
        internal EventInfo Event { get { return (EventInfo)base.Member; } }
        internal EventSymbol(EventInfo evt) : base(evt, Binder.FindType(evt.EventHandlerType)) { }
    }
    internal partial class PropertySymbol : MemberSymbol
    {
        internal PropertyInfo Property { get { return (PropertyInfo)base.Member; } }
        internal PropertySymbol(PropertyInfo property) : base(property, Binder.FindType(property.PropertyType)) { }
    }
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
        internal ConstructorInfo Constructor { get { return (ConstructorInfo)base.Member; } }
        internal ConstructorSymbol(ConstructorInfo ctor) : base(ctor, Binder.FindType(ctor.DeclaringType)) { }
    }
}
