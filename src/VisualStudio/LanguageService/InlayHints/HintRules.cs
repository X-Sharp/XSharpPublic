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
