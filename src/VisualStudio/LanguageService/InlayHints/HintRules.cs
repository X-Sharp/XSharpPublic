//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using XSharp.LanguageService.FindReferences;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>Which hints are shown and what they say. A wrong hint is worse than none: everything unclear stays silent.</summary>
    internal static class HintRules
    {
        /// <summary>
        /// True when the argument does not tell what it means: a literal (also NIL, NULL_*, TRUE/FALSE, a signed
        /// number) or a constructor call <c>Type{...}</c>.
        /// </summary>
        internal static bool SaysNothing(IList<IToken> argument)
        {
            if (argument == null || argument.Count == 0)
                return false;
            if (argument.Count == 1)
                return XSharpLexer.IsLiteral(argument[0].Type);
            if (argument.Count == 2 && (argument[0].Type == XSharpLexer.MINUS || argument[0].Type == XSharpLexer.PLUS))
                return XSharpLexer.IsLiteral(argument[1].Type);
            return IsConstructor(argument);
        }

        /// <summary>The one member that fits the call, or null when none or several remain, or it declares no parameters.</summary>
        internal static IXMemberSymbol SingleFit(IList<IXSymbol> filtered, int argumentCount)
        {
            if (filtered == null || filtered.Count != 1 || !(filtered[0] is IXMemberSymbol member))
                return null;
            if (member.Parameters == null || member.Parameters.Count == 0)
                return null;
            return OverloadFilter.Accepts(member, argumentCount) ? member : null;
        }

        internal static IReadOnlyList<InlayHint> ParameterHints(CallCandidate call, IXMemberSymbol member)
        {
            var result = new List<InlayHint>();
            var parameters = member?.Parameters;
            if (parameters == null || parameters.Count == 0)
                return result;
            int offset = member.IsExtension && call.IsMemberAccess ? 1 : 0;
            for (int i = 0; i < call.Arguments.Count; i++)
            {
                var index = i + offset;
                // arguments beyond the last parameter (CLIPPER), or the 2nd+ argument of a PARAMS parameter
                if (index >= parameters.Count)
                    break;
                var argument = call.Arguments[i];
                var parameter = parameters[index];
                if (!SaysNothing(argument))
                    continue;
                result.Add(new InlayHint
                {
                    Kind = InlayHintKind.Parameter,
                    Position = argument[0].StartIndex,
                    Text = parameter.Name + ":",
                    Parameter = parameter,
                    Member = member
                });
            }
            return result;
        }

        /// <summary>True when the initializer already names the type: a constructor, a literal or a cast.</summary>
        internal static bool TypeIsVisible(IList<IToken> initializer)
        {
            if (initializer == null || initializer.Count == 0)
                return false;
            if (SaysNothing(initializer))
                return true;
            // (Type) expr
            if (initializer[0].Type == XSharpLexer.LPAREN)
            {
                int i = 1;
                while (i < initializer.Count && (IsTypeNamePart(initializer[i].Type)))
                    i++;
                return i > 1 && i < initializer.Count - 1 && initializer[i].Type == XSharpLexer.RPAREN;
            }
            return false;
        }

        /// <summary>
        /// False when the lookup did not resolve the type: the parser stores VAR or the initializer text as type
        /// name, and that stays when the lookup fails (<c>FOREACH VAR d IN &lt;DbFile&gt;{o}</c> gives "&lt;DbFile&gt;{o}").
        /// </summary>
        internal static bool IsResolvedTypeName(string typeName, IList<IToken> initializer)
        {
            if (string.IsNullOrWhiteSpace(typeName) || string.Equals(typeName, XLiterals.VarType, System.StringComparison.OrdinalIgnoreCase))
                return false;
            if (initializer == null || initializer.Count == 0)
                return true;
            var text = string.Concat(initializer.Select(t => t.Text));
            return !string.Equals(WithoutSpaces(typeName), WithoutSpaces(text), System.StringComparison.OrdinalIgnoreCase);
        }

        private static string WithoutSpaces(string text) => Regex.Replace(text, @"\s+", "");

        /// <summary>
        /// True when all overloads of the call return the same type. The lookup behind the variable type takes the
        /// first overload, not the one the arguments pick (<c>Math.Max(1, 2)</c> would say BYTE).
        /// </summary>
        internal static bool OverloadsAgreeOnType(IList<IXSymbol> candidates)
        {
            var types = (candidates ?? new List<IXSymbol>()).OfType<IXMemberSymbol>().Select(m => m.TypeName ?? "");
            return types.Distinct(System.StringComparer.OrdinalIgnoreCase).Count() <= 1;
        }

        /// <summary>
        /// True when a type argument does not resolve: a type parameter that the lookup did not substitute
        /// (<c>List&lt;TSource&gt;</c> from <c>ToList()</c>, <c>List&lt;T&gt;</c> from a member of a generic class).
        /// </summary>
        internal static bool HasOpenTypeArgument(string typeName, System.Func<string, bool> resolves)
        {
            foreach (var argument in TypeArguments(typeName))
            {
                if (argument.Contains("<"))
                {
                    if (HasOpenTypeArgument(argument, resolves))
                        return true;
                    continue;
                }
                var name = argument;
                while (name.EndsWith("[]"))
                    name = name.Substring(0, name.Length - 2);
                if (!resolves(name))
                    return true;
            }
            return false;
        }

        /// <summary>The top level type arguments of <c>Name&lt;A, B&lt;C&gt;&gt;</c>: "A" and "B&lt;C&gt;".</summary>
        private static IEnumerable<string> TypeArguments(string typeName)
        {
            var result = new List<string>();
            if (string.IsNullOrEmpty(typeName))
                return result;
            int open = typeName.IndexOf('<');
            int close = typeName.LastIndexOf('>');
            if (open < 0 || close < open)
                return result;
            int depth = 0, start = open + 1;
            for (int i = start; i < close; i++)
            {
                var c = typeName[i];
                if (c == '<')
                    depth++;
                else if (c == '>')
                    depth--;
                else if (c == ',' && depth == 0)
                {
                    result.Add(typeName.Substring(start, i - start).Trim());
                    start = i + 1;
                }
            }
            result.Add(typeName.Substring(start, close - start).Trim());
            return result.Where(a => a.Length > 0);
        }

        /// <summary>
        /// The type name the double-click edit writes. It must compile: a namespace is left out only where a USING
        /// or the enclosing namespace brings it in; otherwise the full name stays (also for nested types).
        /// </summary>
        internal static string EditTypeName(string fullName, IEnumerable<string> usings)
        {
            if (string.IsNullOrEmpty(fullName))
                return fullName;
            var known = new HashSet<string>(usings ?? Enumerable.Empty<string>(), System.StringComparer.OrdinalIgnoreCase);
            return QualifiedNames.Replace(fullName, m =>
            {
                var name = m.Value;
                int dot = name.LastIndexOf('.');
                return known.Contains(name.Substring(0, dot)) ? name.Substring(dot + 1) : name;
            });
        }

        private static readonly Regex QualifiedNames = new Regex(@"\b(?:\w+\.)+\w+", RegexOptions.Compiled);

        private static readonly Regex Namespaces = new Regex(@"\b(?:\w+\.)+(\w+)", RegexOptions.Compiled);

        /// <summary>The type name without namespaces, also inside generic arguments.</summary>
        internal static string ShortTypeName(string xsharpTypeName)
        {
            if (string.IsNullOrEmpty(xsharpTypeName))
                return xsharpTypeName;
            return Namespaces.Replace(xsharpTypeName, "$1");
        }

        private static bool IsTypeNamePart(int type)
        {
            return type == XSharpLexer.ID || type == XSharpLexer.DOT || XSharpLexer.IsType(type);
        }

        /// <summary><c>Name{...}</c> or <c>Ns.Name{...}</c> covering the whole argument.</summary>
        private static bool IsConstructor(IList<IToken> tokens)
        {
            int i = 0;
            while (i < tokens.Count && (tokens[i].Type == XSharpLexer.ID || tokens[i].Type == XSharpLexer.DOT))
                i++;
            if (i == 0 || i >= tokens.Count || tokens[i].Type != XSharpLexer.LCURLY)
                return false;
            int depth = 0;
            for (; i < tokens.Count; i++)
            {
                if (tokens[i].Type == XSharpLexer.LCURLY)
                    depth++;
                else if (tokens[i].Type == XSharpLexer.RCURLY && --depth == 0)
                    return i == tokens.Count - 1;
            }
            return false;
        }
    }
}
