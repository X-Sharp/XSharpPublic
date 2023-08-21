using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;
using XSharp.MacroCompiler.Syntax;

namespace XSharp.MacroCompiler
{
    internal static class SymbolExtensions
    {
        internal static bool IsUsualOrObject(this TypeSymbol s) => s.NativeType == NativeType.Usual || s.NativeType == NativeType.Object;

        internal static bool IsSubclassOf(this TypeSymbol ts, TypeSymbol tb) => ts.Type.IsSubclassOf(tb.Type);

        internal static bool IsAssignableFrom(this TypeSymbol ts, TypeSymbol tb) => ts.Type.IsAssignableFrom(tb.Type);

        internal static bool IsMethodOrMethodGroup(this Symbol s)
        {
            if (s is MethodBaseSymbol)
                return true;
            if (s is SymbolList)
            {
                // we can have both types and methods in a list
                // for example when we search for Type
                //
                var list = s as SymbolList;
                foreach (var m in list.Symbols)
                {
                    if (m is MethodBaseSymbol)
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        internal static bool HasMethods(this Symbol s) => (s is MethodBaseSymbol) || (s as SymbolList)?.HasMethodBase == true;

        internal static bool HasFunctions(this Symbol s)
        {
            if ((s as MethodSymbol)?.IsStatic == true)
                return true;
            if (s is SymbolList)
            {
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if ((m as MethodSymbol)?.IsStatic == true)
                        return true;
                }
            }
            return false;
        }

        internal static bool IsInXSharpRuntime(this Assembly a) => a.GetName().Name.ToLower().StartsWith("xsharp.");

        internal static bool IsInXSharpRuntime(this MethodSymbol s) => s.Method.DeclaringType.Assembly.IsInXSharpRuntime();

        internal static Symbol UniqueIdent(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (!(m is MethodBaseSymbol) && !(m is TypeSymbol) && !(m is NamespaceSymbol))
                    {
                        if (u != null)
                            return null;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static Symbol UniqueType(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (m is TypeSymbol)
                    {
                        if (u != null)
                            return s;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static Symbol UniqueTypeOrNamespace(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (m is TypeSymbol || m is NamespaceSymbol)
                    {
                        if (u != null)
                            return s;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static TypeSymbol Type(this Symbol s) => (s as TypedSymbol)?.Type;

        internal static bool IsStaticMember(this Symbol s) => (s is FieldSymbol fs)? fs.IsStatic :
            (s is MethodSymbol ms) ? ms.IsStatic :
            (s is PropertySymbol ps) ? ps.IsStatic :
            false;

        internal static string MemberName(this Symbol s)
        {
            return
                (s as MemberSymbol)?.FullName
                ?? (s as SymbolList)?.Symbols.First().MemberName();
        }

        internal static bool IsConstant(this Symbol s) => (s as Constant)?.Type.IsUsualOrObject() == false;

        internal static bool Matches(this TypeSymbol s, TypeSymbol other) => Binder.TypesMatch(s, other);

        internal static bool IsEnumeratorGetter(this MethodSymbol s) => s.Parameters.Parameters.Length == 0 &&
            (s.Type.Matches(Compilation.Get(WellKnownTypes.System_Collections_IEnumerator)) || s.Type.DeclaringType.Matches(Compilation.Get(WellKnownTypes.System_Collections_Generic_IEnumerator_T1)));

        internal static MethodSymbol GetEnumeratorGetter(this Symbol s)
        {
            var members = s.Lookup(SystemNames.GetEnumerator);
            if (members is MethodSymbol singleGetter && singleGetter.IsEnumeratorGetter())
                return singleGetter;
            if (members is SymbolList memberList && memberList.HasMethod)
            {
                MethodSymbol getter = null;
                foreach (var member in memberList.Symbols)
                {
                    if (member is MethodSymbol method && method.IsEnumeratorGetter())
                    {
                        if (getter == null)
                            getter = method;
                        else if (!getter.Type.IsGenericType && method.Type.IsGenericType)
                            getter = method;
                        else if (getter.Type.IsGenericType && method.Type.IsGenericType)
                            return null;
                    }
                }
                return getter;
            }
            return null;
        }

        internal static bool IsIndirectRefConversion(this Symbol s) => (s is ConversionChain ch) && (ch.Conversion is ConversionByRef && ch.Previous is ConversionToTemp || ch.Conversion.IsIndirectRefConversion());
        internal static LocalSymbol IndirectRefConversionTempLocal(this Symbol s) => ((s as ConversionChain)?.Previous as ConversionToTemp)?.Local ?? (s as ConversionChain)?.Conversion.IndirectRefConversionTempLocal();

        internal static void AddExpr(ref Expr c, Expr e)
        {
            if (c == null)
                c = e;
            else if (c is ExprList el)
                el.Exprs.Add(e);
            else
                c = new ExprList(new Expr[] { c, e });
        }
    }
}
