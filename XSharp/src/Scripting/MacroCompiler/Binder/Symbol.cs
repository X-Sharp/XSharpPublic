using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    internal abstract class Symbol
    {
        internal abstract Symbol Lookup(string name);
    }
    internal class SymbolList : Symbol
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
    internal class ContainerSymbol : Symbol
    {
        internal Dictionary<string, Symbol> Members = new Dictionary<string, Symbol>(StringComparer.OrdinalIgnoreCase);
        internal ContainerSymbol() { }
        internal override Symbol Lookup(string name) { Symbol s; Members.TryGetValue(name, out s); return s; }
    }
    internal class NamespaceSymbol : ContainerSymbol
    {
        internal string Name;
        internal NamespaceSymbol(string name) { Name = name; }
        internal NamespaceSymbol() { Name = null; }
    }
    internal class TypeSymbol : ContainerSymbol
    {
        bool Cached = false;
        internal Type Type;
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
    }
    internal class LocalSymbol : Symbol
    {
        internal string Name;
        internal TypeSymbol Type;
        internal LocalSymbol(string name, TypeSymbol type) { Name = name; Type = type; }
        internal override Symbol Lookup(string name) { return null; }
    }
    internal class MemberSymbol : Symbol
    {
        internal MemberInfo Member;
        internal MemberSymbol(MemberInfo member) { Member = member; }
        internal override Symbol Lookup(string name) { return null; }
        internal static MemberSymbol Create(MemberInfo member)
        {
            switch (member.MemberType)
            {
                case MemberTypes.Method:
                    return new MethodSymbol((MethodInfo)member);
                default:
                    return new MemberSymbol(member);
            }
        }
    }
    internal class MethodSymbol : MemberSymbol
    {
        internal MethodInfo Method { get { return (MethodInfo)base.Member; } }
        //internal ParameterInfo[] Parameters { get { Interlocked.CompareExchange(ref _parameters, Method.GetParameters(), null); return _parameters; } }
        //ParameterInfo[] _parameters;
        internal MethodSymbol(MethodInfo method) : base(method) { }
    }
}
