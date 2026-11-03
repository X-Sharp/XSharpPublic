//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// The lookup returns every overload of a name. Find References narrows them down by the number of
    /// arguments at the call site, then by the argument types where they are obvious from the tokens.
    /// </summary>
    internal static class OverloadFilter
    {
        /// <summary>The number of arguments when the name is followed by an argument list, otherwise null.</summary>
        internal static int? ArgumentCount(IList<IToken> tokens, IToken name)
        {
            return Arguments(tokens, name)?.Count;
        }

        /// <summary>
        /// The tokens of each argument when the name is followed by an argument list <c>(...)</c> or, for a
        /// creation, <c>{...}</c>, otherwise null. An omitted argument (<c>Foo(1,,3)</c>) is an empty list.
        /// </summary>
        internal static IList<IList<IToken>> Arguments(IList<IToken> tokens, IToken name)
        {
            var index = name.TokenIndex >= 0 && name.TokenIndex < tokens.Count && tokens[name.TokenIndex] == name
                ? name.TokenIndex
                : tokens.IndexOf(name);
            if (index < 0)
                return null;
            var arguments = new List<IList<IToken>>();
            var current = new List<IToken>();
            int depth = 0;
            for (int i = index + 1; i < tokens.Count; i++)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                if (depth == 0)
                {
                    if (token.Type != XSharpLexer.LPAREN && token.Type != XSharpLexer.LCURLY)
                        return null;
                    depth = 1;
                    continue;
                }
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.LCURLY:
                        depth++;
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.RCURLY:
                        depth--;
                        if (depth == 0)
                        {
                            if (current.Count > 0 || arguments.Count > 0)
                                arguments.Add(current);
                            return arguments;
                        }
                        break;
                    case XSharpLexer.COMMA:
                        if (depth == 1)
                        {
                            arguments.Add(current);
                            current = new List<IToken>();
                            continue;
                        }
                        break;
                    case XSharpLexer.EOS:
                    case XSharpLexer.Eof:
                        return null;
                }
                current.Add(token);
            }
            return null;
        }

        /// <summary>The argument count for the identifier at (or just before) the caret position.</summary>
        internal static int? ArgumentCountAt(IList<IToken> tokens, int position)
        {
            return ArgumentsAt(tokens, position)?.Count;
        }

        /// <summary>The arguments for the identifier at (or just before) the caret position.</summary>
        internal static IList<IList<IToken>> ArgumentsAt(IList<IToken> tokens, int position)
        {
            var name = NameAt(tokens, position);
            return name == null ? null : Arguments(tokens, name);
        }

        /// <summary>
        /// The identifier at (or just before) the caret position, or the keyword that names a constructor
        /// (<c>SUPER</c>, <c>SELF</c>, <c>CONSTRUCTOR</c>).
        /// </summary>
        internal static IToken NameAt(IList<IToken> tokens, int position)
        {
            return tokens?.FirstOrDefault(t => IsName(t) && t.StartIndex <= position && position <= t.StopIndex + 1);
        }

        private static bool IsName(IToken token)
        {
            switch (token.Type)
            {
                case XSharpLexer.ID:
                case XSharpLexer.SUPER:
                case XSharpLexer.SELF:
                case XSharpLexer.CONSTRUCTOR:
                    return true;
                default:
                    return false;
            }
        }

        /// <summary>
        /// The system type name of an argument when it is obvious from its tokens: a literal, a constructor call
        /// <c>Foo{...}</c> or a single identifier (typed by <paramref name="identifierType"/>). Otherwise null.
        /// </summary>
        internal static string ArgumentType(IList<IToken> argument, Func<IToken, string> identifierType)
        {
            if (argument.Count == 0)
                return null;
            var first = argument[0];
            if (argument.Count == 2 && first.Type == XSharpLexer.MINUS)
                first = argument[1];
            else if (argument.Count > 1)
                return ConstructedType(argument);
            if (first.Type == XSharpLexer.ID)
                return Normalize(identifierType?.Invoke(first));
            return LiteralType(first);
        }

        /// <summary>The type of <c>Foo{...}</c> or <c>Ns.Foo{...}</c>, otherwise null.</summary>
        private static string ConstructedType(IList<IToken> argument)
        {
            int i = 0;
            var name = "";
            while (i < argument.Count && argument[i].Type == XSharpLexer.ID)
            {
                name += argument[i].Text;
                i++;
                if (i < argument.Count && argument[i].Type == XSharpLexer.DOT)
                {
                    name += ".";
                    i++;
                }
                else
                    break;
            }
            if (name.Length == 0 || i >= argument.Count || argument[i].Type != XSharpLexer.LCURLY ||
                argument[argument.Count - 1].Type != XSharpLexer.RCURLY)
                return null;
            // the closing brace must belong to the opening one: Foo{}:Bar{} is not a constructor call
            int depth = 0;
            for (int j = i; j < argument.Count - 1; j++)
            {
                if (argument[j].Type == XSharpLexer.LCURLY)
                    depth++;
                else if (argument[j].Type == XSharpLexer.RCURLY && --depth == 0)
                    return null;
            }
            return name;
        }

        private static string LiteralType(IToken token)
        {
            var text = token.Text.ToUpperInvariant();
            switch (token.Type)
            {
                case XSharpLexer.TRUE_CONST:
                case XSharpLexer.FALSE_CONST:
                    return KnownTypes.SystemBoolean;
                case XSharpLexer.INT_CONST:
                case XSharpLexer.HEX_CONST:
                case XSharpLexer.BIN_CONST:
                    return text.EndsWith("U") ? KnownTypes.SystemUInt32 : KnownTypes.SystemInt32;
                case XSharpLexer.REAL_CONST:
                    if (text.EndsWith("M"))
                        return KnownTypes.SystemDecimal;
                    return text.EndsWith("S") ? KnownTypes.SystemSingle : KnownTypes.SystemDouble;
                case XSharpLexer.DATE_CONST:
                case XSharpLexer.NULL_DATE:
                    return KnownTypes.XSharpDate;
                case XSharpLexer.DATETIME_CONST:
                    return KnownTypes.SystemDateTime;
                case XSharpLexer.SYMBOL_CONST:
                case XSharpLexer.NULL_SYMBOL:
                    return KnownTypes.XSharpSymbol;
                case XSharpLexer.CHAR_CONST:
                    return KnownTypes.SystemChar;
                case XSharpLexer.STRING_CONST:
                case XSharpLexer.ESCAPED_STRING_CONST:
                case XSharpLexer.INTERPOLATED_STRING_CONST:
                case XSharpLexer.TEXT_STRING_CONST:
                case XSharpLexer.BRACKETED_STRING_CONST:
                case XSharpLexer.NULL_STRING:
                    return KnownTypes.SystemString;
                default:
                    return null;
            }
        }

        private static string Normalize(string typeName)
        {
            return string.IsNullOrWhiteSpace(typeName) ? null : typeName.Trim().GetSystemTypeName(true);
        }

        private enum TypeCategory { Any, Integral, Floating, Char, Text, Logic, Date, Symbol, Other }

        private static TypeCategory Category(string systemTypeName)
        {
            switch (systemTypeName)
            {
                case KnownTypes.SystemObject:
                case KnownTypes.XSharpUsual:
                    return TypeCategory.Any;
                case KnownTypes.SystemByte:
                case KnownTypes.SystemSByte:
                case KnownTypes.SystemInt16:
                case KnownTypes.SystemUInt16:
                case KnownTypes.SystemInt32:
                case KnownTypes.SystemUInt32:
                case KnownTypes.SystemInt64:
                case KnownTypes.SystemUInt64:
                    return TypeCategory.Integral;
                case KnownTypes.SystemSingle:
                case KnownTypes.SystemDouble:
                case KnownTypes.SystemDecimal:
                case KnownTypes.XSharpFloat:
                case KnownTypes.XSharpVOFloat:
                case KnownTypes.XSharpCurrency:
                    return TypeCategory.Floating;
                case KnownTypes.SystemChar:
                    return TypeCategory.Char;
                case KnownTypes.SystemString:
                case KnownTypes.XSharpPSZ:
                    return TypeCategory.Text;
                case KnownTypes.SystemBoolean:
                case KnownTypes.XSharpWinBool:
                    return TypeCategory.Logic;
                case KnownTypes.XSharpDate:
                case KnownTypes.XSharpWinDate:
                case KnownTypes.XSharpVODate:
                case KnownTypes.SystemDateTime:
                    return TypeCategory.Date;
                case KnownTypes.XSharpSymbol:
                    return TypeCategory.Symbol;
                default:
                    return TypeCategory.Other;
            }
        }

        private enum Match { None, Compatible, Exact }

        /// <summary>
        /// Only built-in types of different kinds rule an overload out. Class types are never ruled out: the
        /// inheritance between them is not known here.
        /// </summary>
        private static Match Compare(string argumentType, string parameterType)
        {
            var parameter = Normalize(parameterType);
            if (parameter == null)
                return Match.Compatible;
            if (string.Equals(argumentType, parameter, StringComparison.OrdinalIgnoreCase) ||
                string.Equals(ShortName(argumentType), ShortName(parameter), StringComparison.OrdinalIgnoreCase))
                return Match.Exact;
            var argumentCategory = Category(argumentType);
            var parameterCategory = Category(parameter);
            if (argumentCategory == TypeCategory.Any || parameterCategory == TypeCategory.Any ||
                argumentCategory == TypeCategory.Other || parameterCategory == TypeCategory.Other ||
                argumentCategory == parameterCategory)
                return Match.Compatible;
            if (argumentCategory == TypeCategory.Integral && parameterCategory == TypeCategory.Floating)
                return Match.Compatible;
            if ((argumentCategory == TypeCategory.Integral && parameterCategory == TypeCategory.Char) ||
                (argumentCategory == TypeCategory.Char && parameterCategory == TypeCategory.Integral))
                return Match.Compatible;
            return Match.None;
        }

        private static string ShortName(string typeName)
        {
            var dot = typeName.LastIndexOf('.');
            return dot < 0 ? typeName : typeName.Substring(dot + 1);
        }

        /// <summary>The number of exact argument matches, or null when an argument rules the member out.</summary>
        private static int? Score(IXMemberSymbol member, IList<string> argumentTypes, int offset)
        {
            if (member.CallingConvention == CallingConvention.Clipper)
                return 0;
            var parameters = member.Parameters ?? new IXParameterSymbol[0];
            int exact = 0;
            for (int i = 0; i < argumentTypes.Count; i++)
            {
                var argumentType = argumentTypes[i];
                int p = i + offset;
                if (argumentType == null || p < 0 || parameters.Count == 0)
                    continue;
                string parameterType;
                if (p < parameters.Count && parameters[p].ParamType != ParamType.Params)
                    parameterType = parameters[p].TypeName;
                else if (parameters[parameters.Count - 1].ParamType == ParamType.Params)
                    parameterType = ElementType(parameters[parameters.Count - 1].TypeName, argumentType);
                else
                    continue;
                switch (Compare(argumentType, parameterType))
                {
                    case Match.None:
                        return null;
                    case Match.Exact:
                        exact++;
                        break;
                }
            }
            return exact;
        }

        /// <summary>A PARAMS array takes its elements, or the array itself.</summary>
        private static string ElementType(string arrayType, string argumentType)
        {
            if (arrayType == null || !arrayType.EndsWith("[]") || argumentType.EndsWith("[]"))
                return arrayType;
            return arrayType.Substring(0, arrayType.Length - 2);
        }

        /// <summary>
        /// Keeps the overloads whose parameter types accept the argument types, and among them the ones with the
        /// most exact matches. Unknown argument types (null) rule nothing out. When no overload accepts the
        /// arguments, all symbols are kept.
        /// </summary>
        internal static IList<IXSymbol> FilterByTypes(IList<IXSymbol> symbols, IList<string> argumentTypes)
        {
            if (symbols.Count < 2 || argumentTypes.All(t => t == null))
                return symbols;
            var scored = new List<(IXSymbol Symbol, int Exact)>();
            foreach (var symbol in symbols)
            {
                if (!(symbol is IXMemberSymbol member))
                {
                    scored.Add((symbol, 0));
                    continue;
                }
                var score = Score(member, argumentTypes, 0);
                if (member.IsExtension)
                {
                    // called on an instance: the first parameter is the instance
                    var asInstance = Score(member, argumentTypes, 1);
                    if (score == null || (asInstance != null && asInstance > score))
                        score = asInstance;
                }
                if (score != null)
                    scored.Add((symbol, score.Value));
            }
            if (scored.Count == 0)
                return symbols;
            var best = scored.Max(s => s.Exact);
            return scored.Where(s => s.Exact == best).Select(s => s.Symbol).ToList();
        }

        /// <summary>Narrows the overloads by the number of arguments, then by their types.</summary>
        internal static IList<IXSymbol> Filter(IList<IXSymbol> symbols, IList<IList<IToken>> arguments, Func<IToken, string> identifierType)
        {
            var byCount = Filter(symbols, arguments?.Count);
            if (arguments == null || byCount.Count < 2)
                return byCount;
            return FilterByTypes(byCount, arguments.Select(a => ArgumentType(a, identifierType)).ToList());
        }

        /// <summary>Can the member be called with this number of arguments?</summary>
        internal static bool Accepts(IXMemberSymbol member, int argumentCount)
        {
            // CLIPPER calling convention: any number of arguments
            if (member.CallingConvention == CallingConvention.Clipper)
                return true;
            var parameters = member.Parameters ?? new IXParameterSymbol[0];
            int total = parameters.Count;
            int required = parameters.Count(p => p.ParamType != ParamType.Params && string.IsNullOrEmpty(p.Value));
            bool hasParamArray = parameters.Any(p => p.ParamType == ParamType.Params);
            if (Fits(argumentCount, required, total, hasParamArray))
                return true;
            // an extension method can also be called with the instance as first argument
            return member.IsExtension && Fits(argumentCount, required - 1, total - 1, hasParamArray);
        }

        private static bool Fits(int count, int required, int total, bool hasParamArray)
        {
            return count >= required && (hasParamArray || count <= total);
        }

        /// <summary>
        /// Keeps the members that accept the number of arguments. Without a count, or when no member accepts it
        /// (for example when the parameters are unknown), all symbols are kept.
        /// </summary>
        internal static IList<IXSymbol> Filter(IList<IXSymbol> symbols, int? argumentCount)
        {
            if (argumentCount == null || symbols.Count < 2)
                return symbols;
            var kept = symbols.Where(s => !(s is IXMemberSymbol member) || Accepts(member, argumentCount.Value)).ToList();
            return kept.Count == 0 ? symbols : kept;
        }
    }
}
