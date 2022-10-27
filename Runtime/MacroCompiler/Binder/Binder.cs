using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler
{
    using Syntax;
    using System.IO;
    using System.Linq.Expressions;
    using System.Reflection;

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

    internal abstract partial class Binder
    {
        static NamespaceSymbol Global = null;
        static List<ContainerSymbol> Usings = null;
        static List<ContainerSymbol> RuntimeFunctions = null;
        static Dictionary<Type, TypeSymbol> TypeCache = null;
        static HashSet<Assembly> LoadedAssemblies = new HashSet<Assembly>();

        internal static StringComparer LookupComparer = StringComparer.OrdinalIgnoreCase;

        internal MacroOptions Options;

        internal Dictionary<string, Symbol> LocalCache = new Dictionary<string, Symbol>(LookupComparer);
        internal List<LocalSymbol> Locals = new List<LocalSymbol>();
        internal List<ArgumentSymbol> Args = new List<ArgumentSymbol>();
        internal TypeSymbol ObjectType;
        internal Type DelegateType;
        internal List<XSharp.Codeblock> NestedCodeblocks;
        internal bool CreatesAutoVars = false;
        internal Stack<Stmt> StmtStack = new Stack<Stmt>();
        internal Stack<int> ScopeStack = new Stack<int>();
        internal Node Entity = null;

        internal TypeSymbol ResultType = null;
        internal TypeSymbol[] ParameterTypes = null;

        static Binder()
        {
            AppDomain.CurrentDomain.AssemblyLoad += AssemblyLoadEventHandler;
        }

        protected Binder(Type objectType, Type delegateType, MacroOptions options)
        {
            Debug.Assert(delegateType == null || delegateType.IsSubclassOf(typeof(Delegate)));
            if (TypeCache == null)
            {
                lock (LoadedAssemblies)
                    if (TypeCache == null)
                        BuildIndex();
            }
            ObjectType = FindType(objectType);
            DelegateType = delegateType;
            Options = options;
            NestedCodeblocks = null;
            if (delegateType != null)
            {
                var mi = delegateType.GetMethod("Invoke");
                if (mi != null)
                {
                    ResultType = FindType(mi.ReturnType);
                    var parameters = mi.GetParameters();
                    ParameterTypes = parameters.Select(p => FindType(p.ParameterType)).ToArray();
                }
            }
        }

        internal static Binder<T> Create<T>(MacroOptions options, Type delegateType)
        {
            if (options?.GenerateAssembly == true)
                return new AssemblyBinder<T>(options, delegateType);
            if (options?.StrictTypedSignature == true)
                return new TypedBinder<T>(options, delegateType);
            return new Binder<T>(options, delegateType);
        }

        internal static Binder<T> Create<T,R>(MacroOptions options) where R: Delegate
        {
            if (options?.GenerateAssembly == true)
                return new AssemblyBinder<T>(options, typeof(Delegate) != typeof(R) ? typeof(R) : null);
            if (options?.StrictTypedSignature == true)
                return new TypedBinder<T>(options, typeof(Delegate) != typeof(R) ? typeof(R) : null);
            return new Binder<T, R>(options);
        }

        private static void AssemblyLoadEventHandler(object sender, AssemblyLoadEventArgs args)
        {
            lock (LoadedAssemblies)
            {
                if (TypeCache != null && !args.LoadedAssembly.IsDynamic && !LoadedAssemblies.Contains(args.LoadedAssembly))
                {
                    LoadedAssemblies.Add(args.LoadedAssembly);
                    UpdateIndex(args.LoadedAssembly);
                }
            }
        }

        static void UpdateTypeCache(NamespaceSymbol global, Dictionary<Type, TypeSymbol> typeCache, Assembly a)
        {
            if (!XSharp.RuntimeState.MacroCompilerIncludeAssemblyInCache(a))
            {
                return;
            }
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
                                    nn = new NamespaceSymbol(ns, n);
                                    n.Members[ns] = nn;
                                }
                                if (!(nn is NamespaceSymbol))
                                    continue;
                                n = nn as NamespaceSymbol;
                            }
                        }
                    }

                    TypeSymbol add_type(ContainerSymbol ct, Type ty)
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
        static void UpdateUsings(List<ContainerSymbol> usings, List<ContainerSymbol> rtFuncs, Assembly a, HashSet<ContainerSymbol> usedSymbols = null)
        {
            if (!XSharp.RuntimeState.MacroCompilerIncludeAssemblyInCache(a))
            {
                return;
            }
            if (usedSymbols == null)
            {
                usedSymbols = new HashSet<ContainerSymbol>();
                foreach (var s in usings)
                    if (!usedSymbols.Contains(s))
                        usedSymbols.Add(s);
            }
            var cla = Compilation.Get(WellKnownTypes.XSharp_Internal_ClassLibraryAttribute);
            var ina = Compilation.Get(WellKnownTypes.ImplicitNamespaceAttribute);
            bool isXsRuntime = a.IsInXSharpRuntime();
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
                                if (s != null)
                                {
                                    if (isXsRuntime && rtFuncs != null)
                                    {
                                        rtFuncs.Add(s);
                                    }
                                    else if (!usedSymbols.Contains(s))
                                    {
                                        usedSymbols.Add(s);
                                        usings.Add(s);
                                    }
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
        static internal void BuildIndex()
        {
            var global = new NamespaceSymbol();
            var usings = new List<ContainerSymbol>();
            var rtFuncs = new List<ContainerSymbol>();
            var typeCache = new Dictionary<Type, TypeSymbol>();

            var usedSymbols = new HashSet<ContainerSymbol>();

            var loadedAssemblies = new HashSet<Assembly>(AppDomain.CurrentDomain.GetAssemblies());
            loadedAssemblies.RemoveWhere(a => a.IsDynamic);

            do
            {
                foreach (var a in loadedAssemblies.ToArray())
                {
                    UpdateTypeCache(global, typeCache, a);
                }
                LoadedAssemblies.UnionWith(loadedAssemblies);

                loadedAssemblies = new HashSet<Assembly>(AppDomain.CurrentDomain.GetAssemblies());
                loadedAssemblies.RemoveWhere(a => a.IsDynamic);
                loadedAssemblies.ExceptWith(LoadedAssemblies);
            } while (loadedAssemblies.Count > 0);

            System.Threading.Interlocked.CompareExchange(ref Global, global, null);
            System.Threading.Interlocked.CompareExchange(ref TypeCache, typeCache, null);

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
                foreach (var a in LoadedAssemblies.ToArray())
                {
                    UpdateUsings(usings, rtFuncs, a, usedSymbols);
                }
            }

            System.Threading.Interlocked.CompareExchange(ref Usings, usings, null);
            System.Threading.Interlocked.CompareExchange(ref RuntimeFunctions, rtFuncs, null);
        }
        static void UpdateIndex(Assembly a)
        {
            UpdateTypeCache(Global, TypeCache, a);
            if (Usings != null)
                UpdateUsings(Usings, RuntimeFunctions, a);
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

        internal static TypeSymbol ArrayOf(TypeSymbol t, int rank)
        {
            if (t == null)
                return null;
            var nt = rank == 1 ? t.Type.MakeArrayType() : t.Type.MakeArrayType(rank);
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
            Symbol v = Global.Lookup(name);

            // prevent "Collection was modified" error
            foreach (var u in Usings.ToArray())
                v = Symbol.Join(v, u.Lookup(name));
            if (!v.HasFunctions())
            {
                // prevent "Collection was modified" error
                foreach (var u in RuntimeFunctions.ToArray())
                    v = Symbol.Join(v, u.Lookup(name));
            }
            return v;
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

        internal LocalSymbol AddLocal(string name, TypeSymbol type, bool isParam = false)
        {
            var local = new LocalSymbol(name, type);
            local.IsParam = isParam;
            Locals.Add(local);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, local);
            }
            return local;
        }

        internal LocalSymbol AddAutoLocal(string name, TypeSymbol type)
        {
            var res = AddLocal(name, type);
            res.IsAuto = true;
            return res;
        }
        internal ArgumentSymbol AddParam(string name, TypeSymbol type, bool first = false)
        {
            var arg = new ArgumentSymbol(name, type, first ? 0 : Args.Count);
            if (first)
            {
                for (int i = 0; i < Args.Count; i++) Args[i].Index += 1;
                Args.Insert(0,arg);
            }
            else
                Args.Add(arg);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, arg);
            }
            return arg;
        }

        internal ArgumentSymbol AddParam(string name, TypeSymbol type, int index)
        {
            var arg = new ArgumentSymbol(name, type, index);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, arg);
            }
            return arg;
        }

        internal VariableSymbol AddVariable(string name, TypeSymbol type)
        {
            var variable = new VariableSymbol(name, type);
            Locals.Add(variable);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, variable);
            }
            return variable;
        }

        internal MemvarSymbol AddMemvar(string name)
        {
            var variable = new MemvarSymbol(name);
            Locals.Add(variable);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, variable);
            }
            return variable;
        }
        internal FieldAliasSymbol AddFieldAlias(string name, string wa = null)
        {
            var variable = new FieldAliasSymbol(name, wa);
            Locals.Add(variable);
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, variable);
            }
            return variable;
        }

        internal Constant AddConstant(string name, Constant c)
        {
            if (!string.IsNullOrEmpty(name))
            {
                if (LocalCache.ContainsKey(name))
                    return null;
                LocalCache.Add(name, c);
            }
            return c;
        }

        internal int ParamCount
        {
            get
            {
                int c = 0;
                foreach (var loc in Locals)
                    if (loc.IsParam)
                        c++;
                return c;
            }
        }

        internal int OpenScope()
        {
            ScopeStack.Push(Locals.Count);
            return Locals.Count;
        }

        internal void CloseScope()
        {
            var scopeBase = ScopeStack.Pop();
            for (var i = scopeBase; i <Locals.Count; i++)
            {
                if (!string.IsNullOrEmpty(Locals[i].Name))
                {
                    LocalCache.Remove(Locals[i].Name);
                }
            }
        }

        internal T FindOuter<T>() where T: class
        {
            foreach(var s in StmtStack)
            {
                if (s is T ts)
                {
                    return ts;
                }
            }
            return null;
        }

        internal void GenerateDelegateTypeIfRequired()
        {
            if (DelegateType == null)
            {
                var types = new List<Type>();
                if (ParameterTypes != null)
                    foreach (var t in ParameterTypes)
                        types.Add(t.Type);
                types.Add(ResultType.Type ?? typeof(void));
                DelegateType = Expression.GetDelegateType(types.ToArray());
            }
        }

        internal int AddNestedCodeblock(out Symbol argSym)
        {
            if (NestedCodeblocks == null)
            {
                NestedCodeblocks = new List<XSharp.Codeblock>();
                argSym = AddParam(XSharpSpecialNames.NestedCodeblockArgs, ArrayOf(Compilation.Get(WellKnownTypes.XSharp_Codeblock)), true);
            }
            else
                argSym = Args[0];
            NestedCodeblocks.Add(null);
            return NestedCodeblocks.Count-1;
        }

        internal bool HasNestedCodeblocks { get => NestedCodeblocks != null; }

        internal abstract Binder CreateNested();

        internal abstract void GenerateMethod(string source);
        internal virtual byte[] GetAssemblyBytes() => null;
        internal abstract Delegate GenerateDelegate();
        internal abstract ILGenerator GetILGenerator();
    }

    internal class Binder<T> : Binder
    {
        internal Binder(MacroOptions options, Type delegateType) : base(typeof(T), delegateType, options) { }

        internal class NestedWrapper
        {
            internal delegate T EvalDelegate(XSharp.Codeblock[] cbs, params T[] args);

            XSharp.Codeblock[] nested_cbs_;
            EvalDelegate eval_func_;

            internal NestedWrapper(XSharp.Codeblock[] nested_cbs, EvalDelegate eval_func)
            {
                nested_cbs_ = nested_cbs;
                eval_func_ = eval_func;
            }

            internal T Eval(params T[] args)
            {
                return eval_func_(nested_cbs_, args);
            }
        }

        internal U Bind<U>(U macro) where U : Node
        {
            Bind(ref macro);
            return macro;
        }

        internal override Binder CreateNested()
        {
            return new Binder<T>(Options, DelegateType);
        }

        private DynamicMethod Method { get; set; }
        internal override void GenerateMethod(string source) => Method = CreateMethod(source);
        internal override Delegate GenerateDelegate() => CreateDelegate(Method);
        internal override ILGenerator GetILGenerator() => Method.GetILGenerator();

        protected virtual DynamicMethod CreateMethod(string source)
        {
            if (HasNestedCodeblocks)
                return new DynamicMethod(source, typeof(T), new Type[] { typeof(XSharp.Codeblock[]), typeof(T[]) });
            else
                return new DynamicMethod(source, typeof(T), new Type[] { typeof(T[]) });
        }

        protected virtual Delegate CreateDelegate(DynamicMethod dm)
        {
            if (HasNestedCodeblocks)
            {
                var eval_dlg = dm.CreateDelegate(typeof(NestedWrapper.EvalDelegate)) as NestedWrapper.EvalDelegate;
                return Delegate.CreateDelegate(DelegateType, new NestedWrapper(NestedCodeblocks.ToArray(), eval_dlg), "Eval", false);
            }
            else
                return dm.CreateDelegate(DelegateType);
        }
    }

    internal class Binder<T,R> : Binder<T> where R: Delegate
    {
        internal Binder(MacroOptions options) : base(options, typeof(R)) { }
        internal Binder(MacroOptions options, Type delegateType) : base(options, delegateType) { }

        internal override Binder CreateNested()
        {
            return new Binder<T,R>(Options);
        }
    }
    internal class TypedBinder<T> : Binder<T>
    {
        internal TypedBinder(MacroOptions options, Type delegateType) : base(options, delegateType) { }
        internal override Binder CreateNested()
        {
            return new Binder<T, Func<T[], T>>(Options);
        }
        protected override DynamicMethod CreateMethod(string source)
        {
            var par = ParameterTypes?.Select(p => p.Type).ToList() ?? new List<Type>();
            if (HasNestedCodeblocks)
                par.Insert(0, typeof(XSharp.Codeblock[]));
            return new DynamicMethod(source, ResultType.Type ?? typeof(void), par.ToArray());
        }
    }
    internal class AssemblyBinder<T> : TypedBinder<T>
    {
        internal AssemblyBinder(MacroOptions options, Type delegateType) : base(options, delegateType) { }

        internal string NameOfAssembly = "QueryAssembly";
        internal string NameOfClass = "QueryClass";
        internal string NameOfMethod = "QueryMethod";

        private string Name;
        private AssemblyBuilder Assembly;
        private ModuleBuilder AssemblyModule;
        private TypeBuilder MethodType;
        private MethodBuilder Method;
        private Type CreatedType;
        private byte[] AssemblyData;
        internal override void GenerateMethod(string source)
        {
            //create the builder
            AssemblyName assembly = new AssemblyName(NameOfAssembly);
            AppDomain appDomain = System.Threading.Thread.GetDomain();
            var tempName = Path.GetTempFileName();
            Assembly = appDomain.DefineDynamicAssembly(assembly, AssemblyBuilderAccess.Save, Path.GetDirectoryName(tempName));
            Name = Path.GetFileName(tempName);
            AssemblyModule = Assembly.DefineDynamicModule(assembly.Name, Name);

            //create the class
            MethodType = AssemblyModule.DefineType(NameOfClass, System.Reflection.TypeAttributes.Public | System.Reflection.TypeAttributes.Class, typeof(object));

            //create the method
            Method = MethodType.DefineMethod(NameOfMethod, System.Reflection.MethodAttributes.Public | System.Reflection.MethodAttributes.Static,
                    ResultType.Type ?? typeof(void),
                    ParameterTypes?.Select(p => p.Type).ToArray() ?? new Type[0]);
            //method.DefineParameter

            CreatedType = null;
        }
        internal override byte[] GetAssemblyBytes()
        {
            if (AssemblyData == null)
            {
                MethodType.CreateType();
                Assembly.Save(Name);
                AssemblyData = File.ReadAllBytes(AssemblyModule.FullyQualifiedName);
                File.Delete(AssemblyModule.FullyQualifiedName);
            }
            return AssemblyData;
        }
        internal override Delegate GenerateDelegate()
        {
            if (CreatedType == null)
            {
                var loadedAssembly = System.Reflection.Assembly.Load(GetAssemblyBytes());
                CreatedType = loadedAssembly.GetType(NameOfClass);
            }
            return CreatedType.GetMethod(NameOfMethod).CreateDelegate(DelegateType);
        }
        internal override ILGenerator GetILGenerator() => Method.GetILGenerator();

    }
}
