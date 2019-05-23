using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal enum BindAffinity
    {
        Access,
        Assign,
        AccessAndAssign,
        Invoke,
        Type,
        Alias,
        AliasField,
        Member,
    }

    [Flags]
    internal enum BindOptions
    {
        AllowDynamic = 1,
        AllowInexactComparisons = 2,
        AllowImplicitNarrowingConversions = 4,

        BoxUsual = 128,
        Explicit = 256,
        Special = 512,
        Logic = 1024,
        Cast = 2048,

        None = 0,
        Default = AllowDynamic | AllowInexactComparisons | AllowImplicitNarrowingConversions
    }

    internal partial class Binder
    {
        static NamespaceSymbol Global = null;
        static List<ContainerSymbol> Usings = null;
        static Dictionary<Type, TypeSymbol> TypeCache = null;

        internal static StringComparer LookupComprer = StringComparer.OrdinalIgnoreCase;

        internal MacroOptions Options;

        internal Dictionary<string, Symbol> LocalCache = new Dictionary<string, Symbol>(LookupComprer);
        internal List<LocalSymbol> Locals = new List<LocalSymbol>();
        internal List<ArgumentSymbol> Args = new List<ArgumentSymbol>();
        internal TypeSymbol ObjectType;
        internal Type DelegateType;

        internal bool CreatesAutoVars = false;

        protected Binder(Type objectType, Type delegateType, MacroOptions options)
        {
            Debug.Assert(delegateType.IsSubclassOf(typeof(Delegate)));
            BuildIndex();
            ObjectType = FindType(objectType);
            DelegateType = delegateType;
            Options = options;
        }

        internal static Binder<T, R> Create<T,R>(MacroOptions options) where R: class
        {
            return new Binder<T, R>(options);
        }

        static internal void BuildIndex()
        {
            if (Global != null && Usings != null && TypeCache != null)
                return;

            var global = new NamespaceSymbol();
            var usings = new List<ContainerSymbol>();
            var typeCache = new Dictionary<Type, TypeSymbol>();

            var usedSymbols = new HashSet<ContainerSymbol>();

            foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (a.IsDynamic)
                    continue;
                var most_visible = a == System.Reflection.Assembly.GetEntryAssembly();
                try
                { 
                var types = most_visible ? a.GetTypes() : a.GetExportedTypes();

                // Build type lookup dictionary
                foreach (var t in types.Where(t => !t.IsNested && !string.IsNullOrEmpty(t?.Name)))
                {
                    var n = global;
                    var nss = t.Namespace?.Split('.');
                    if (nss != null)
                    {
                        foreach (var ns in nss)
                        {
                            if (!string.IsNullOrEmpty(ns))
                            {
                                Symbol nn;
                                if (!n.Members.TryGetValue(ns, out nn) || (!(nn is NamespaceSymbol) && most_visible))
                                {
                                    nn = new NamespaceSymbol(ns,n);
                                    n.Members[ns] = nn;
                                }
                                if (!(nn is NamespaceSymbol))
                                    continue;
                                n = nn as NamespaceSymbol;
                            }
                        }
                    }

                    Func<ContainerSymbol, Type, TypeSymbol> add_type = null;

                    add_type = (ct, ty) =>
                    {
                        if (ty.IsNested)
                        {
                            ct = add_type(ct, ty.DeclaringType);
                            if (ct == null)
                                return null;
                        }
                        Symbol tv;
                        if (ct.Members.TryGetValue(ty.Name, out tv))
                        {
                            if ((tv as TypeSymbol)?.Type == ty)
                                return (TypeSymbol)tv;
                            if (!most_visible && tv is TypeSymbol)
                                return null;
                        }

                        var r = new TypeSymbol(ty);
                        ct.Members[ty.Name] = r;
                        typeCache[ty] = r;
                        return r;
                    };

                    var ts = add_type(n, t);

                    /*if (!t.IsNested && t.Name.Equals(XSharpSpecialNames.FunctionsClass, StringComparison.OrdinalIgnoreCase))
                    {
                        if (ts != null && !usedSymbols.Contains(ts))
                        {
                            usedSymbols.Add(ts);
                            usings.Add(ts);
                        }
                    }*/
                }
                }
                catch (Exception e)
                {
                    System.Diagnostics.Debug.WriteLine("Error loading types from " + a.CodeBase + "\r" + e.Message);
                }
            }

            global = System.Threading.Interlocked.CompareExchange(ref Global, global, null);
            typeCache = System.Threading.Interlocked.CompareExchange(ref TypeCache, typeCache, null);

            Compilation.InitializeNativeTypes();
            Compilation.InitializeWellKnownTypes();
            Compilation.InitializeWellKnownMembers();

            foreach (var ns in new string[]{OurNameSpaces.System, OurNameSpaces.XSharp, OurNameSpaces.Vulcan})
            {
                var s = LookupFullName(ns) as NamespaceSymbol;
                if (s != null && !usedSymbols.Contains(s))
                {
                    usedSymbols.Add(s);
                    usings.Add(s);
                }
            }

            var cla = Compilation.Get(WellKnownTypes.XSharp_Internal_ClassLibraryAttribute);
            var ina = Compilation.Get(WellKnownTypes.ImplicitNamespaceAttribute);
            if (cla != null && ina != null)
            {
                foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
                {
                    if (a.IsDynamic)
                        continue;
                    var most_visible = a == System.Reflection.Assembly.GetEntryAssembly();
                    if (a.CustomAttributes != null)
                    {
                        foreach (var attr in a.CustomAttributes)
                        {
                            if (attr.AttributeType == ina.Type)
                            {
                                var args = attr.ConstructorArguments;
                                if (args != null && args.Count == 1)
                                {
                                    // first element is the default namespace
                                    var ns = args[0].Value.ToString();
                                    if (!string.IsNullOrEmpty(ns))
                                    {
                                        var s = LookupFullName(ns) as NamespaceSymbol;
                                        if (s != null && !usedSymbols.Contains(s))
                                        {
                                            usedSymbols.Add(s);
                                            usings.Add(s);
                                        }
                                    }
                                }
                            }
                            else if (attr.AttributeType == cla.Type)
                            {
                                var args = attr.ConstructorArguments;
                                if (args != null && args.Count == 2)
                                {
                                    // first element is the Functions class
                                    var cls = args[0].Value.ToString();
                                    if (!string.IsNullOrEmpty(cls))
                                    {
                                        var s = LookupFullName(cls) as TypeSymbol;
                                        if (s != null && !usedSymbols.Contains(s))
                                        {
                                            usedSymbols.Add(s);
                                            usings.Add(s);
                                        }
                                    }
                                    // second element is the default namespace
                                    var ns = args[1].Value.ToString();
                                    if (!string.IsNullOrEmpty(ns))
                                    {
                                        var s = LookupFullName(ns) as NamespaceSymbol;
                                        if (s != null && !usedSymbols.Contains(s))
                                        {
                                            usedSymbols.Add(s);
                                            usings.Add(s);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            usings = System.Threading.Interlocked.CompareExchange(ref Usings, usings, null);
        }

        internal static TypeSymbol FindType(Type t)
        {
            if (t == null)
                return null;
            TypeSymbol v;
            if (!TypeCache.TryGetValue(t, out v))
            {
                v = new TypeSymbol(t);
                TypeCache[t] = v;
            }
            return v;
        }

        internal static TypeSymbol NullableType(TypeSymbol t)
        {
            if (t == null)
                return null;
            var nt = Nullable.GetUnderlyingType(t.Type);
            if (nt.IsValueType)
            {
                nt = typeof(Nullable<>).MakeGenericType(nt);
            }
            return FindType(nt);
        }

        internal static TypeSymbol ArrayOf(TypeSymbol t)
        {
            if (t == null)
                return null;
            var nt = t.Type.MakeArrayType();
            return FindType(nt);
        }

        internal static TypeSymbol PointerOf(TypeSymbol t)
        {
            if (t == null)
                return null;
            var nt = t.Type.MakePointerType();
            return FindType(nt);
        }

        internal static TypeSymbol ByRefOf(TypeSymbol t)
        {
            if (t == null)
                return null;
            var nt = t.Type.MakeByRefType();
            return FindType(nt);
        }

        internal Symbol Lookup(string name)
        {
            {
                Symbol v;
                LocalCache.TryGetValue(name, out v);
                if (v != null)
                    return v;
            }
            {
                Symbol v = LookupName(name);
                if (v != null)
                    return v;
            }
            return null;
        }

        internal Symbol Lookup(Symbol decl, string name)
        {
            if (decl != null)
                return decl.Lookup(name);
            else
                return Lookup(name);
        }

        internal static Symbol LookupName(string name)
        {
            {
                Symbol v = Global.Lookup(name);
                if (v != null)
                    return v;
            }
            {
                Symbol v = null;
                foreach (var u in Usings)
                {
                    Symbol t = u.Lookup(name);
                    if (t != null)
                    {
                        if (v != null)
                        {
                            if (!(v is SymbolList))
                                v = new SymbolList(v);
                            if (!(t is SymbolList))
                                (v as SymbolList).Add(t);
                            else
                            {
                                foreach (var ts in (t as SymbolList).Symbols)
                                    (v as SymbolList).Add(ts);
                            }
                        }
                        else
                            v = t;
                    }
                }
                return v;
            }
        }

        internal static Symbol ResolveSuffix(string fullname, Symbol type)
        {
            if (fullname?.EndsWith("[]") == true)
            {
                return ArrayOf(ResolveSuffix(fullname.Remove(fullname.Length - 2), type as TypeSymbol) as TypeSymbol);
            }
            else if (fullname?.EndsWith("*") == true)
            {
                return PointerOf(ResolveSuffix(fullname.Remove(fullname.Length - 1), type as TypeSymbol) as TypeSymbol);
            }
            else if (fullname?.EndsWith("&") == true)
            {
                return ByRefOf(ResolveSuffix(fullname.Remove(fullname.Length - 1), type as TypeSymbol) as TypeSymbol);
            }
            else if (fullname?.EndsWith("?") == true)
            {
                return NullableType(ResolveSuffix(fullname.Remove(fullname.Length - 1), type as TypeSymbol) as TypeSymbol);
            }
            else
                return type;
        }

        internal static Symbol LookupFullName(string[] qnames)
        {
            Symbol n = Global;
            if (qnames != null)
            {
                foreach (var id in qnames)
                {
                    n = n.Lookup(id);
                    if (n == null)
                        break;
                }
                return ResolveSuffix(qnames.LastOrDefault(), n);
            }
            return null;
        }

        internal static Symbol LookupFullName(string fullname)
        {
            if (fullname == null)
                return null;
            if (fullname.StartsWith("global::"))
            {
                fullname = fullname.Substring(fullname.LastIndexOf(':') + 1);
            }
            var t = LookupFullName(fullname.TrimEnd(new char[] {'[',']','*','&'}).Split('.'));
            return ResolveSuffix(fullname,t);
        }

        internal LocalSymbol AddLocal(TypeSymbol type)
        {
            return AddLocal(null, type);
        }

        internal ArgumentSymbol AddParam(TypeSymbol type)
        {
            return AddParam(null, type);
        }

        internal LocalSymbol AddLocal(string name, TypeSymbol type)
        {
            var local = new LocalSymbol(name, type);
            Locals.Add(local);
            if (!string.IsNullOrEmpty(name))
                LocalCache.Add(name, local);
            return local;
        }

        internal ArgumentSymbol AddParam(string name, TypeSymbol type)
        {
            var arg = new ArgumentSymbol(name, type, Args.Count);
            Args.Add(arg);
            if (!string.IsNullOrEmpty(name))
                LocalCache.Add(name, arg);
            return arg;
        }

        internal VariableSymbol AddVariable(string name, TypeSymbol type)
        {
            var variable = new VariableSymbol(name, type);
            Locals.Add(variable);
            if (!string.IsNullOrEmpty(name))
                LocalCache.Add(name, variable);
            return variable;
        }

        internal void AddConstant(string name, Constant c)
        {
            if (!string.IsNullOrEmpty(name))
                LocalCache.Add(name, c);
        }
    }

    internal class Binder<T,R> : Binder where R: class
    {
        internal Binder(MacroOptions options) : base(typeof(T),typeof(R), options) { }

        internal Codeblock Bind(Codeblock macro)
        {
            Bind(ref macro);
            return macro;
        }

        internal int ParamCount
        {
            get
            {
                int c = 0;
                foreach (var loc in LocalCache)
                    if (loc.Value is ArgumentSymbol)
                        c++;
                return c;
            }
        }
    }
}
