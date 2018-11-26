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
        Invoke,
    }

    internal partial class Binder
    {
        static NamespaceSymbol Global = null;
        static List<ContainerSymbol> Usings = null;
        static Dictionary<Type, TypeSymbol> TypeCache = null;

        internal bool DynamicUsual = true;

        internal Dictionary<string, LocalSymbol> LocalCache = new Dictionary<string, LocalSymbol>();
        internal List<LocalSymbol> Locals = new List<LocalSymbol>();
        internal List<ParameterSymbol> Params = new List<ParameterSymbol>();
        internal TypeSymbol ObjectType;
        internal Type DelegateType;

        protected Binder(Type objectType, Type delegateType)
        {
            Debug.Assert(delegateType.IsSubclassOf(typeof(Delegate)));
            BuildIndex();
            ObjectType = FindType(objectType);
            DelegateType = delegateType;
        }

        internal static Binder<T, R> Create<T,R>() where R: class
        {
            return new Binder<T, R>();
        }

        static internal void BuildIndex()
        {
            if (Global != null && Usings != null && TypeCache != null)
                return;

            var global = new NamespaceSymbol();
            var usings = new List<ContainerSymbol>();
            var typeCache = new Dictionary<Type, TypeSymbol>();

            bool usedDefaultNs = false;

            foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (a.IsDynamic)
                    continue;
                var most_visible = a == System.Reflection.Assembly.GetEntryAssembly();
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
                                    nn = new NamespaceSymbol(ns);
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

                    if (!t.IsNested && t.Name.Equals(XSharpSpecialNames.FunctionsClass, StringComparison.OrdinalIgnoreCase))
                    {
                        usings.Add(ts);
                        if (!usedDefaultNs 
                            && !string.IsNullOrEmpty(t.Namespace) 
                            && t.Namespace.Contains(".")
                            && t.Assembly == System.Reflection.Assembly.GetExecutingAssembly())
                        {
                            usings.Add(n);
                            usedDefaultNs = true;
                        }
                    }
                }
            }

            Symbol s;
            if (global.Members.TryGetValue("System",out s))
            {
                if (s is NamespaceSymbol)
                    usings.Add((NamespaceSymbol)s);
            }

            global = System.Threading.Interlocked.CompareExchange(ref Global, global, null);
            usings = System.Threading.Interlocked.CompareExchange(ref Usings, usings, null);
            typeCache = System.Threading.Interlocked.CompareExchange(ref TypeCache, typeCache, null);

            Compilation.InitializeNativeTypes();
            Compilation.InitializeWellKnownTypes();
            Compilation.InitializeWellKnownMembers();
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

        internal Symbol Lookup(Symbol decl, string name)
        {
            if (decl != null)
            {
                return decl.Lookup(name);
            }
            else
            {
                {
                    LocalSymbol v;
                    LocalCache.TryGetValue(name, out v);
                    if (v != null)
                        return v;
                }
                {
                    Symbol v = Lookup(name);
                    if (v != null)
                        return v;
                }
            }
            return null;
        }

        internal static Symbol Lookup(string name)
        {
            {
                Symbol v = Global.Lookup(name);
                if (v != null)
                    return v;
            }
            foreach (var u in Usings)
            {
                Symbol v = u.Lookup(name);
                if (v != null)
                    return v;
            }
            return null;
        }

        internal static Symbol LookupFullName(string fullname)
        {
            Symbol n = Global;
            if (fullname.StartsWith("global::"))
            {
                fullname = fullname.Substring(fullname.LastIndexOf(':') + 1);
            }
            var qnames = fullname.Split('.');
            if (qnames != null)
            {
                foreach (var id in qnames)
                {
                    n = n.Lookup(id);
                    if (n == null)
                        break;
                }
                return n;
            }
            return null;
        }

        internal LocalSymbol AddLocal(TypeSymbol type)
        {
            return AddLocal(null, type);
        }

        internal ParameterSymbol AddParam(TypeSymbol type)
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

        internal ParameterSymbol AddParam(string name, TypeSymbol type)
        {
            var param = new ParameterSymbol(name, type, Params.Count);
            Params.Add(param);
            if (!string.IsNullOrEmpty(name))
                LocalCache.Add(name, param);
            return param;
        }
    }

    internal class Binder<T,R> : Binder where R: class
    {
        internal Binder() : base(typeof(T),typeof(R)) { }

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
                    if (loc.Value is ParameterSymbol)
                        c++;
                return c;
            }
        }
    }
}
