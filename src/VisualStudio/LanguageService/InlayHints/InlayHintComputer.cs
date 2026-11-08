//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using XSharp.LanguageService.FindReferences;
using XSharp.Settings;
using XSharp.Support;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>The hints of one document snapshot.</summary>
    internal sealed class InlayHintSet
    {
        internal ITextSnapshot Snapshot { get; set; }
        internal IReadOnlyList<InlayHint> Hints { get; set; }
        /// <summary>The kinds that were computed (the options, or Alt+F1 held).</summary>
        internal bool Parameters { get; set; }
        internal bool Types { get; set; }
    }

    /// <summary>Resolves the candidates of a whole document against the code model. Runs in the background.</summary>
    internal static class InlayHintComputer
    {
        /// <summary>The hints for <paramref name="snapshot"/>, or null when the document has moved on.</summary>
        internal static InlayHintSet Compute(XDocument doc, XFile file, ITextSnapshot snapshot, bool parameters, bool types, CancellationToken ct)
        {
            if (doc == null || file == null || snapshot == null || (!parameters && !types))
                return null;
            IList<IToken> tokens;
            XSharpLineState lineState;
            // the classifier clears and refills the document in place on every lex
            lock (doc)
            {
                if (doc.SnapShot != snapshot || doc.Tokens == null)
                    return null;
                tokens = doc.Tokens.ToList();
                lineState = doc.LineState;
            }
            var watch = Stopwatch.StartNew();
            var document = PrivateDocument(doc.Buffer, tokens, snapshot, lineState);
            var hints = new List<InlayHint>();
            int calls = 0, declarations = 0;
            // No ModelWalker.Suspend() here: this runs after every parse, and a suspend makes the walker drop the
            // rest of its project and start it again. The hints are recomputed when the walk completes.
            if (parameters)
            {
                foreach (var call in HintCandidates.Calls(tokens))
                {
                    ct.ThrowIfCancellationRequested();
                    // only calls that could show a hint are worth a lookup
                    if (!call.Arguments.Any(HintRules.SaysNothing))
                        continue;
                    calls++;
                    var member = HintRules.SingleFit(Resolve(document, file, tokens, call.Name, out _, out _), call.Arguments.Count);
                    if (member != null)
                        hints.AddRange(HintRules.ParameterHints(call, member));
                }
            }
            if (types)
            {
                foreach (var decl in HintCandidates.Declarations(tokens))
                {
                    ct.ThrowIfCancellationRequested();
                    if (HintRules.TypeIsVisible(decl.Initializer))
                        continue;
                    declarations++;
                    var hint = TypeHint(document, file, tokens, decl);
                    if (hint != null)
                        hints.Add(hint);
                }
            }
            Logger.Information($"Inlay hints {file.FullPath}: {calls} calls, {declarations} declarations, {hints.Count} hints, {watch.ElapsedMilliseconds} ms");
            return new InlayHintSet { Snapshot = snapshot, Hints = hints.OrderBy(h => h.Position).ToList() };
        }

        private static InlayHint TypeHint(XDocument document, XFile file, IList<IToken> tokens, DeclarationCandidate decl)
        {
            // an initializer that ends in a call: the call must be unambiguous, as for the parameter names
            var callName = FinalCallName(decl.Initializer);
            if (callName != null)
            {
                var arguments = OverloadFilter.Arguments(decl.Initializer, callName);
                var filtered = Resolve(document, file, tokens, callName, out _, out var candidates);
                if (filtered.Count != 1 || (filtered[0] is IXMemberSymbol m && !OverloadFilter.Accepts(m, arguments?.Count ?? 0)))
                    return null;
                if (!HintRules.OverloadsAgreeOnType(candidates))
                    return null;
            }
            var symbols = Resolve(document, file, tokens, decl.Name, out var location, out _);
            if (!(symbols.FirstOrDefault() is IXVariableSymbol variable))
                return null;
            var typeName = variable.TypeName;
            if (!HintRules.IsResolvedTypeName(typeName, decl.Initializer))
                return null;
            if (location == null || HintRules.HasOpenTypeArgument(typeName, name => ResolvesToType(location, name)))
                return null;
            var shown = HintRules.ShortTypeName(typeName);
            var written = HintRules.EditTypeName(typeName, location.Usings);
            return new InlayHint
            {
                Kind = InlayHintKind.Type,
                Position = decl.Name.StopIndex + 1,
                Text = " AS " + shown,
                TypeName = shown,
                Type = location.FindType(typeName),
                Edit = TypeRewrite.For(decl, tokens, written, XLiterals.FormatKeyword)
            };
        }

        /// <summary>True when the name (also a keyword type such as STRING) is a type in the context of the location.</summary>
        private static bool ResolvesToType(XSharpSearchLocation location, string name)
        {
            if (location.Project == null)
                return false;
            return location.FindType(name.GetSystemTypeName(location.Project.ParseOptions.XSharpRuntime)) != null;
        }

        /// <summary>The name of the call that the initializer ends with (<c>o:Foo(1)</c>, <c>Bar{}</c>), otherwise null.</summary>
        internal static IToken FinalCallName(IList<IToken> initializer)
        {
            if (initializer == null || initializer.Count < 3)
                return null;
            var last = initializer[initializer.Count - 1].Type;
            int open, close;
            if (last == XSharpLexer.RPAREN)
            {
                open = XSharpLexer.LPAREN;
                close = XSharpLexer.RPAREN;
            }
            else if (last == XSharpLexer.RCURLY)
            {
                open = XSharpLexer.LCURLY;
                close = XSharpLexer.RCURLY;
            }
            else
                return null;
            int depth = 0;
            for (int i = initializer.Count - 1; i > 0; i--)
            {
                var type = initializer[i].Type;
                if (type == close)
                    depth++;
                else if (type == open && --depth == 0)
                {
                    var name = initializer[i - 1];
                    return name.Type == XSharpLexer.ID || XSharpLexer.IsPositionalKeyword(name.Type) ? name : null;
                }
            }
            return null;
        }

        /// <summary>The symbols the name resolves to, narrowed by the arguments of its call.</summary>
        private static IList<IXSymbol> Resolve(XDocument document, XFile file, IList<IToken> tokens, IToken name, out XSharpSearchLocation context, out IList<IXSymbol> candidates)
        {
            context = null;
            candidates = new List<IXSymbol>();
            try
            {
                var member = XSharpLookup.FindMemberAtPosition(name.StartIndex, file);
                var ns = XSharpTokenTools.FindNamespace(name.StartIndex, file);
                var location = new XSharpSearchLocation(document, file, member, document.SnapShot, name.Line - 1, name.StartIndex, ns?.FullName ?? "");
                var searchContext = location;
                context = location;
                var resolved = SymbolResolver.ForCreation(SymbolResolver.ResolveAt(ref location, out _, out _), tokens, name);
                candidates = resolved;
                var arguments = OverloadFilter.Arguments(tokens, name);
                if (arguments == null)
                    return resolved;
                return OverloadFilter.Filter(resolved, arguments, id => SymbolResolver.TypeOf(searchContext, id));
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Inlay hints: lookup failed");
                return new List<IXSymbol>();
            }
        }

        /// <summary>A document of its own: the classifier refills the buffer's document while we resolve.</summary>
        private static XDocument PrivateDocument(ITextBuffer buffer, IList<IToken> tokens, ITextSnapshot snapshot, XSharpLineState lineState)
        {
            var document = new XDocument(buffer, tokens, snapshot);
            XDocumentIndexer.BuildIndexes(tokens, out var lineTokens, out var identifiers, out var indexState);
            document.SetTokens(lineTokens);
            document.SetIdentifiers(identifiers);
            document.SetState(lineState ?? indexState, snapshot);
            return document;
        }
    }
}
