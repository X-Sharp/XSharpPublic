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
        [Flags]
        internal enum AccessMode
        {
            None = 0,
            Get = 1,
            Set = 2,
            Ref = 4,
            Init = 8,
            InitGet = Get | Init,
            GetSet = Get | Set | Init,
            All = Get | Set | Ref | Init,
        };
        internal AccessMode access_ = AccessMode.None;
        internal Symbol() { }
        internal Symbol(AccessMode access) { access_ = access; }
        internal abstract Symbol Lookup(string name);
        internal bool HasGetAccess { get => access_.HasFlag(AccessMode.Get); }
        internal bool HasSetAccess { get => access_.HasFlag(AccessMode.Set); }
        internal bool HasRefAccess { get => access_.HasFlag(AccessMode.Ref); }
        internal bool HasInitAccess { get => access_.HasFlag(AccessMode.Init); }
        internal static Symbol Join(Symbol s, Symbol t)
        {
            if (t != null)
            {
                if (s != null)
                {
                    if (!(s is SymbolList))
                        s = new SymbolList(s);
                    if (!(t is SymbolList))
                        (s as SymbolList).Add(t);
                    else
                    {
                        foreach (var ts in (t as SymbolList).Symbols)
                            (s as SymbolList).Add(ts);
                    }
                }
                else
                    return t;
            }
            return s;
        }
    }
    internal abstract partial class TypedSymbol : Symbol
    {
        internal TypedSymbol(AccessMode access) : base(access) { }
        internal override Symbol Lookup(string name) { throw new InternalError(); }
        abstract internal TypeSymbol Type { get; }
    }
    internal partial class SymbolList : Symbol
    {
        internal MemberTypes SymbolTypes = 0;
        internal List<Symbol> Symbols;
        internal SymbolList() { Symbols = new List<Symbol>(); }
        internal SymbolList(Symbol s): this() { Add(s); }
        internal void Add(Symbol s) { Symbols.Add(s); SymbolTypes |= (s as MemberSymbol)?.MemberType ?? 0; }
        internal bool HasMethod { get { return SymbolTypes.HasFlag(MemberTypes.Method); } }
        internal bool HasConstructor { get { return SymbolTypes.HasFlag(MemberTypes.Constructor); } }
        internal bool HasProperty { get { return SymbolTypes.HasFlag(MemberTypes.Property); } }
        internal bool HasField { get { return SymbolTypes.HasFlag(MemberTypes.Field); } }
        internal bool HasMethodBase { get { return HasMethod || HasConstructor; } }
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
    internal abstract partial class ContainerSymbol : Symbol
    {
        internal Dictionary<string, Symbol> Members = new Dictionary<string, Symbol>(Binder.LookupComparer);
        internal ContainerSymbol() { }
        internal override Symbol Lookup(string name) { Symbol s; Members.TryGetValue(name, out s); return s; }
    }
    internal partial class NamespaceSymbol : ContainerSymbol
    {
        internal string Name;
        internal NamespaceSymbol ParentNamespace;
        internal NamespaceSymbol(string name, NamespaceSymbol parent) { Name = name; ParentNamespace = parent; }
        internal NamespaceSymbol() { Name = null; ParentNamespace = null; }
    }
    internal partial class TypeSymbol : ContainerSymbol
    {
        bool Cached = false;
        internal bool IsFunctionsClass { get; set; }
        internal readonly Type Type;
        internal NativeType NativeType;
        internal TypeSymbol DeclaringType { get { return Binder.FindType(Type.DeclaringType); } }
        internal NamespaceSymbol Namespace { get { return Binder.LookupFullName(Type.Namespace) as NamespaceSymbol; } }
        internal TypeSymbol(Type type) { Type = type; IsFunctionsClass = type.Name.EndsWith("Functions"); }
        internal bool IsByRef { get { return Type.IsByRef; } }
        internal bool IsArray { get { return Type.IsArray; } }
        internal bool IsValueType { get { return Type.IsValueType; } }
        internal bool IsReferenceType { get { return Type.IsClass || Type.IsInterface; } }
        internal bool IsEnum { get { return Type.IsEnum; } }
        internal bool IsGenericType { get { return Type.IsGenericType; } }
        internal bool IsVoid { get { return NativeType == NativeType.Void; } }
        internal int ArrayRank { get { return Type.GetArrayRank(); } }
        internal TypeSymbol ElementType { get { return Type.HasElementType ? Binder.FindType(Type.GetElementType()) : null; } }
        internal TypeSymbol EnumUnderlyingType { get { return Type.IsEnum ? Binder.FindType(Type.GetEnumUnderlyingType()) : null; } }
        internal Dictionary<MemberInfo, Symbol> MemberTable = new Dictionary<MemberInfo, Symbol>();
        void AddMember(string name, Symbol ms)
        {
            Symbol s = null;
            if (Members.TryGetValue(name, out s))
            {
                if (!(s is SymbolList))
                {
                    s = new SymbolList(s);
                    Members[name] = s;
                }
                (s as SymbolList).Add(ms);
            }
            else
                Members[name] = ms;
        }
        static List<MemberInfo> GetMemberInfoList(Type t, BindingFlags flags)
        {
            List<MemberInfo> result = new List<MemberInfo>();
            result.AddRange(t.GetMembers(flags));
            var usedIfaces = new HashSet<Type>();
            var ifaces = new Stack<Type>();
            ifaces.Push(t);
            while (ifaces.Count > 0)
            {
                foreach(var iface in ifaces.Pop().GetInterfaces())
                {
                    if (!usedIfaces.Contains(iface))
                    {
                        usedIfaces.Add(iface);
                        result.AddRange(iface.GetMembers(flags));
                        ifaces.Push(iface);
                    }
                }
            }
            return result;
        }
        internal void UpdateCache()
        {
            if (Cached)
                return;
            var flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public;
            if (Type.Assembly == Assembly.GetEntryAssembly())
            {
                flags |= BindingFlags.NonPublic;
            }
            foreach (var m in GetMemberInfoList(Type, flags))
            {
                lock (this)
                {
                    var ms = MemberSymbol.Create(this, m);
                    if (ms != null)
                    {
                        if (! MemberTable.ContainsKey(m))
                        {
                            MemberTable.Add(m, ms);
                        }
                        AddMember(m.Name, ms);
                    }
                }
            }
            if (NativeType == NativeType.Array)
            {
                Symbol getter;
                Symbol setter;
                Members.TryGetValue(XSharpFunctionNames.GetElement, out getter);
                Members.TryGetValue(XSharpFunctionNames.SetElement, out setter);
                if (getter is SymbolList) getter = (getter as SymbolList).Symbols.Find(s => (s as MethodSymbol)?.Parameters.Parameters.LastOrDefault()?.ParameterType.IsArray == true);
                if (setter is SymbolList) setter = (setter as SymbolList).Symbols.Find(s => (s as MethodSymbol)?.Parameters.Parameters.LastOrDefault()?.ParameterType.IsArray == true);
                if (getter is MethodSymbol && setter is MethodSymbol)
                    AddMember(SystemNames.IndexerName, new PropertySymbol(this, getter as MethodSymbol, setter as MethodSymbol, false));
            }
            if (IsArray && ArrayRank > 1)
            {
                Symbol getters;
                Symbol setters;
                Members.TryGetValue(SystemNames.ArrayGetValue, out getters);
                Members.TryGetValue(SystemNames.ArraySetValue, out setters);
                var getter = (getters as SymbolList).Symbols.Find(s => { var t = (s as MethodSymbol)?.Parameters.Parameters.LastOrDefault()?.ParameterType; return t.IsArray == true && t.GetElementType() == typeof(int); });
                var setter = (setters as SymbolList).Symbols.Find(s => { var t = (s as MethodSymbol)?.Parameters.Parameters.LastOrDefault()?.ParameterType; return t.IsArray == true && t.GetElementType() == typeof(int); });
                if (getter is MethodSymbol && setter is MethodSymbol)
                    AddMember(SystemNames.IndexerName, new PropertySymbol(this, getter as MethodSymbol, setter as MethodSymbol, false));
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
        internal bool IsParam { get; set; }
        internal bool IsAuto { get; set; }
        internal bool IsConst => !HasSetAccess;
        internal LocalSymbol(string name, TypeSymbol type) : base(AccessMode.All) { Name = name; Type = type; }
        internal LocalSymbol(TypeSymbol type, int index) : this(null, type) { Index = index; }
        internal LocalSymbol(TypeSymbol type) : this(null, type) { }
        internal override Symbol Lookup(string name) { return null; }
        internal void SetConst() { access_ &= ~AccessMode.Set; }
    }
    internal partial class ArgumentSymbol : LocalSymbol
    {
        internal ArgumentSymbol(string name, TypeSymbol type, int index) : base(name, type) { Index = index; }
    }
    internal partial class VariableSymbol : LocalSymbol
    {
        internal VariableSymbol(string name, TypeSymbol type) : base(name, type) { }
    }
    internal partial class MemvarSymbol : LocalSymbol
    {
        internal MemvarSymbol(string name) : base(name, Compilation.Get(NativeType.Usual)) { access_ = AccessMode.GetSet; }
    }
    internal partial class FieldAliasSymbol : LocalSymbol
    {
        internal string WorkArea = null;
        internal FieldAliasSymbol(string name) : base(name, Compilation.Get(NativeType.Usual)) { access_ = AccessMode.GetSet; }
        internal FieldAliasSymbol(string name, string wa) : this(name) { WorkArea = wa; }
    }
    internal partial class DynamicSymbol : TypedSymbol
    {
        internal string Name;
        internal XSharpDialect Dialect;
        internal override TypeSymbol Type { get { return (Binder.LookupFullName(XSharpQualifiedFunctionNames.IVarGet) as MethodSymbol)?.Type ?? Compilation.Get(NativeType.Object); } }
        internal DynamicSymbol(string name, XSharpDialect dialect) : base(AccessMode.GetSet)
        {
            Name = name;
            Dialect = dialect;
        }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal partial class DynamicExprSymbol : TypedSymbol
    {
        internal Syntax.Expr Name;
        internal override TypeSymbol Type { get { return (Binder.LookupFullName(XSharpQualifiedFunctionNames.IVarGet) as MethodSymbol)?.Type ?? Compilation.Get(NativeType.Object); } }
        internal DynamicExprSymbol(Syntax.Expr name) : base(AccessMode.GetSet) { Name = name; }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal partial class ObjectInitializerSymbol : TypedSymbol
    {
        internal override TypeSymbol Type { get; }
        internal ObjectInitializerSymbol(TypeSymbol type) : base(AccessMode.Get) { Type = type; }
    }
    internal abstract partial class MemberSymbol : TypedSymbol
    {
        internal TypeSymbol ContainingType;
        internal MemberInfo Member;
        internal readonly MemberTypes MemberType;
        internal TypeSymbol DeclaringType { get => Binder.FindType(Member.DeclaringType); }
        internal override TypeSymbol Type { get; }
        internal MemberSymbol(TypeSymbol contType, MemberInfo member, TypeSymbol type, MemberTypes memberType, AccessMode access) : base(access)
        {
            ContainingType = contType;
            Member = member;
            Type = type;
            MemberType = memberType;
        }
        internal override Symbol Lookup(string name) { return null; }
        internal static Symbol Create(TypeSymbol contType, MemberInfo member)
        {
            switch (member.MemberType)
            {
                case MemberTypes.Method:
                    return new MethodSymbol(contType, (MethodInfo)member);
                case MemberTypes.Field:
                    {
                        // Do not store Globals and Defines from the Functions classes
                        if (contType.IsFunctionsClass)
                            return null;
                        var field = (FieldInfo)member;
                        if (field.IsLiteral)
                        {
                            return Constant.Create(
                                field.FieldType.IsEnum ?
                                    Convert.ChangeType(field.GetValue(null), field.FieldType.GetEnumUnderlyingType()) : field.GetValue(null),
                                Binder.FindType(field.FieldType));
                        }
                        return new FieldSymbol(contType, field);
                    }
                case MemberTypes.Event:
                    return new EventSymbol(contType, (EventInfo)member);
                case MemberTypes.Property:
                    return new PropertySymbol(contType, (PropertyInfo)member);
                case MemberTypes.Constructor:
                    return new ConstructorSymbol(contType, (ConstructorInfo)member);
                case MemberTypes.NestedType:
                    return new TypeSymbol(member as Type);
                default:
                    return null; // ignore unrecognized types
            }
        }
    }
    internal partial class ParameterListSymbol : Symbol
    {
        internal bool HasParamArray { get; }
        internal ParameterInfo[] Parameters;
        internal ParameterListSymbol(ParameterInfo[] parameters) : base()
        {
            Parameters = parameters;
            var attrs = Parameters.LastOrDefault()?.CustomAttributes;
            if (attrs != null)
            {
                foreach (var attr in attrs)
                {
                    if (attr.AttributeType == typeof(System.ParamArrayAttribute))
                    {
                        HasParamArray = true;
                    }
                }
            }
        }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal abstract partial class MethodBaseSymbol : MemberSymbol
    {
        internal MethodBase MethodBase { get { return (MethodBase)base.Member; } }
        bool _foundAttributes = false;
        CustomAttributeData _clipperAttr = null;
        string[] _clipperParams = null;
        ParameterListSymbol _parameters;
        internal MethodBaseSymbol(TypeSymbol contType, MethodBase method, TypeSymbol type) : base(contType, method, type, method.MemberType, AccessMode.Get) { }
        internal ParameterListSymbol Parameters { get { Interlocked.CompareExchange(ref _parameters, new ParameterListSymbol(MethodBase.GetParameters()), null); return _parameters; } }
        internal bool IsClipper { get { FindAttributes(); return _clipperAttr != null; } }
        internal string[] ClipperParams { get { FindAttributes(); return _clipperParams; } }
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
                            _clipperParams = ((IReadOnlyCollection<CustomAttributeTypedArgument>)_clipperAttr?.ConstructorArguments[0].Value)
                                .Select(a => (string)a.Value).ToArray();
                        }
                    }
                }
                _foundAttributes = true;
            }
        }
    }
    internal partial class MethodSymbol : MethodBaseSymbol
    {
        internal bool IsStatic { get { return Method.IsStatic; } }
        internal bool IsVirtual { get { return Method.IsVirtual; } }
        internal MethodInfo Method { get { return (MethodInfo)base.Member; } }
        internal MethodSymbol(TypeSymbol contType, MethodInfo method) : base(contType, method, Binder.FindType(method.ReturnType)) { }
    }
    internal partial class FieldSymbol : MemberSymbol
    {
        internal bool IsStatic { get { return Field.IsStatic; } }
        internal FieldInfo Field { get { return (FieldInfo)base.Member; } }
        internal FieldSymbol(TypeSymbol contType, FieldInfo field) : base(contType, field, Binder.FindType(field.FieldType), field.MemberType, AccessMode.All) { }
    }
    internal partial class EventSymbol : MemberSymbol
    {
        internal EventInfo Event { get { return (EventInfo)base.Member; } }
        internal EventSymbol(TypeSymbol contType, EventInfo evt) : base(contType, evt, Binder.FindType(evt.EventHandlerType), evt.MemberType, AccessMode.GetSet) { }
    }
    internal partial class PropertySymbol : MemberSymbol
    {
        internal bool IsStatic { get { return Getter?.IsStatic ?? Setter?.IsStatic ?? true; } }
        internal PropertyInfo Property { get { return (PropertyInfo)base.Member; } }
        internal MethodSymbol Getter { get; }
        internal MethodSymbol Setter { get; }
        internal readonly bool ValueLast = true;
        ParameterListSymbol _parameters;
        internal ParameterListSymbol Parameters
        {
            get
            {
                var p = Getter?.Method.GetParameters();
                if (p == null)
                {
                    p = Setter?.Method.GetParameters();
                    if (p != null)
                    {
                        Array.Resize(ref p, p.Length - 1);
                    }
                    else
                        p = new ParameterInfo[] { };
                }
                Interlocked.CompareExchange(ref _parameters, new ParameterListSymbol(p), null);
                return _parameters;
            }
        }
        internal PropertySymbol(TypeSymbol contType, PropertyInfo property) : base(contType, property, Binder.FindType(property.PropertyType), property.MemberType, AccessMode.GetSet)
        {
            Getter = Property.GetMethod?.IsPublic == true ? ContainingType.MemberTable[Property.GetMethod] as MethodSymbol : null;
            Setter = Property.SetMethod?.IsPublic == true ? ContainingType.MemberTable[Property.SetMethod] as MethodSymbol : null;
        }
        internal PropertySymbol(TypeSymbol contType, MethodSymbol getter, MethodSymbol setter, bool valueLast) : base(contType, null, Binder.FindType(getter.Method.ReturnType), MemberTypes.Property, AccessMode.GetSet)
        {
            Getter = getter;
            Setter = setter;
            ValueLast = valueLast;
        }
    }
    internal partial class ConstructorSymbol : MethodBaseSymbol
    {
        internal ConstructorInfo Constructor { get { return (ConstructorInfo)base.Member; } }
        internal ConstructorSymbol(TypeSymbol contType, ConstructorInfo ctor) : base(contType, ctor, Binder.FindType(ctor.DeclaringType)) { }
    }
}
