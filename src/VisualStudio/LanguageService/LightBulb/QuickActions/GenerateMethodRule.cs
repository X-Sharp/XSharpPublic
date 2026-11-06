//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>What stands before the name under the caret.</summary>
    internal enum ReceiverKind
    {
        /// <summary>Nothing: a bare call <c>Foo()</c>.</summary>
        None,
        /// <summary><c>SELF:Foo()</c></summary>
        Self,
        /// <summary><c>SUPER:Foo()</c></summary>
        Super,
        /// <summary><c>oVar:Foo()</c> / <c>oVar.Foo()</c></summary>
        Variable,
        /// <summary>A chain, a call result, a literal ...</summary>
        Other,
    }

    /// <summary>Decides whether "Generate method" is offered for a call, and into which class.</summary>
    internal static class GenerateMethodRule
    {
        private const int MaxDepth = 20;

        /// <summary>Pseudo functions of the compiler: lexed as identifiers, never found as runtime functions.</summary>
        private static readonly HashSet<string> Intrinsics = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "PCount", "ArgCount", "_GetInst", "PCall", "PCallNative", "DoDefault", "_Args",
            "_GetMParam", "_GetFParam", "AltD", "SLen", "String2Psz", "Cast2Psz", "_SizeOf", "_TypeOf",
        };

        /// <summary>
        /// The source class to generate method <paramref name="name"/> into, or null. Offered for <c>SELF:Foo()</c>
        /// and bare calls in a class method (into the enclosing class) and for <c>oVar:Foo()</c> on a variable typed
        /// as a source class. Not offered when the name is no call, when the class or a base class already has a
        /// member of that name or defines NoMethod (unknown sends are caught on purpose), for DLL, unknown and
        /// late-bound receivers, and for a bare call when <paramref name="isKnownGlobal"/> knows a function or
        /// define of that name.
        /// </summary>
        internal static IXTypeSymbol Target(ReceiverKind kind, IXTypeSymbol receiverType, IXTypeSymbol enclosingClass,
            bool isCall, bool resolved, string name, Func<string, bool> isKnownGlobal, Func<IXTypeSymbol, IXTypeSymbol> baseOf, bool caseSensitive)
        {
            // a name the lookup resolves (extension methods, delegates, ...) exists already
            if (!isCall || resolved || string.IsNullOrEmpty(name))
                return null;
            IXTypeSymbol target;
            switch (kind)
            {
                case ReceiverKind.Self:
                    target = enclosingClass;
                    break;
                case ReceiverKind.None:
                    if (enclosingClass == null || Intrinsics.Contains(name) || isKnownGlobal(name))
                        return null;
                    target = enclosingClass;
                    break;
                case ReceiverKind.Variable:
                    target = receiverType;
                    break;
                default:
                    return null;
            }
            if (!(target is XSourceTypeSymbol))
                return null;
            var comparison = caseSensitive ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase;
            foreach (var type in SelfAndBases(target, baseOf))
            {
                if (type.Members.Any(m => string.Equals(m.Name, name, comparison) ||
                                          string.Equals(m.Name, "NoMethod", StringComparison.OrdinalIgnoreCase)))
                    return null;
            }
            return target;
        }

        /// <summary>Did the lookup find the name? In VO an unknown name comes back as an undeclared variable.</summary>
        internal static bool IsResolved(IList<IXSymbol> symbols)
        {
            return symbols != null && symbols.Any(s => s.Kind != Kind.Undeclared);
        }

        /// <summary>The type and its base types, nearest first; stops at cycles and after 20 levels.</summary>
        internal static IEnumerable<IXTypeSymbol> SelfAndBases(IXTypeSymbol type, Func<IXTypeSymbol, IXTypeSymbol> baseOf)
        {
            var seen = new HashSet<IXTypeSymbol>();
            for (int depth = 0; type != null && depth < MaxDepth && seen.Add(type); depth++)
            {
                yield return type;
                type = baseOf(type);
            }
        }
    }
}
