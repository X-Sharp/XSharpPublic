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
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using XSharp.Support;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Finds the references to a source symbol: name filter, lexer, then every candidate token is resolved
    /// with the Go To Definition lookup and compared with the target.
    /// </summary>
    internal static class FindReferencesEngine
    {
        /// <summary>The option "list late-bound sends and symbols" (Tools > Options > X# > Intellisense).</summary>
        internal static bool IncludeLateBound { get; set; } = true;

        /// <summary>
        /// Create the <paramref name="family"/> on the UI thread: it reads the target's (editor) types.
        /// <paramref name="options"/> says what is returned besides the sure places (late-bound sends and symbols,
        /// uncertain places, names in comments and strings) and how far the search reaches.
        /// </summary>
        internal static Task<FindReferencesStats> FindAsync(IXSymbol target, OverrideFamily family, ReferenceSearchOptions options,
            XSharpSearchLocation origin, ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)
        {
            return Task.Run(() => Find(target, family, options, origin, documents, sink, ct), ct);
        }

        private static FindReferencesStats Find(IXSymbol target, OverrideFamily family, ReferenceSearchOptions options, XSharpSearchLocation origin,
            ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)
        {
            var stats = new FindReferencesStats();
            var watch = Stopwatch.StartNew();
            ModelWalker.Suspend();
            try
            {
                var name = SearchName(target);
                TextInterval? limit = null;
                IEnumerable<XFile> files;
                if (target is XSourceVariableSymbol variable)
                {
                    // locals and parameters: only inside their member
                    if (variable.Parent is XSourceEntity owner)
                        limit = owner.Interval;
                    // the XFile of a local belongs to the open editor: search a private copy, parsing it here
                    // would replace the editor's entities from a background thread
                    var editorFile = variable.File ?? origin.File;
                    files = new[] { editorFile?.Project?.FindXFile(editorFile.FullPath) };
                }
                else
                {
                    // an override family is searched from its topmost member: its callers may live in any project
                    // that sees the base class
                    files = FilesInScope(family.TargetRoot ?? target, options.TransitiveProjects);
                }
                Action<XFile> failed = null;
                if (options.IncludeUncertain)
                {
                    failed = file => sink.Add(new[]
                    {
                        new ReferenceHit
                        {
                            FilePath = file.FullPath,
                            ProjectName = file.Project?.Name,
                            LineText = "",
                            Uncertainty = ReferenceUncertainty.FileNotRead,
                            Group = UncertainGroup(ReferenceUncertainty.FileNotRead, target)
                        }
                    });
                }
                SearchEach(files.Where(f => f != null), file =>
                {
                    stats.FilesInScope++;
                    var hits = SearchFile(target, family, options, name, file, limit, documents, stats, ct);
                    if (hits.Count > 0)
                    {
                        stats.Hits += hits.Count;
                        sink.Add(hits);
                    }
                }, stats, ct, failed);
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "FindReferences failed");
            }
            finally
            {
                ModelWalker.Resume();
                stats.Elapsed = watch.Elapsed;
                Logger.Information($"FindReferences {target.Kind} '{target.Name}' ({target.FullName}): {stats}");
            }
            return stats;
        }

        /// <summary>
        /// Runs the search for every item. A failing item (missing or locked file, lookup exception) is logged and
        /// counted in <see cref="FindReferencesStats.Failed"/>, passed to <paramref name="failed"/>, and the search goes
        /// on; cancellation ends it.
        /// </summary>
        internal static void SearchEach<T>(IEnumerable<T> items, Action<T> search, FindReferencesStats stats, CancellationToken ct,
            Action<T> failed = null)
        {
            foreach (var item in items)
            {
                ct.ThrowIfCancellationRequested();
                try
                {
                    search(item);
                }
                catch (OperationCanceledException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    stats.Failed++;
                    Logger.Exception(ex, $"FindReferences: search failed for {item}");
                    failed?.Invoke(item);
                }
            }
        }

        /// <summary>
        /// Only symbols declared in the solution's source can be searched; types and members of referenced
        /// assemblies cannot.
        /// </summary>
        internal static bool IsInSource(IXSymbol target)
        {
            return target is XSourceVariableSymbol || SourceFileOf(target) != null;
        }

        private static XFile SourceFileOf(IXSymbol target)
        {
            // members read from the database have no file: use the file of their type
            var file = (target as XSourceSymbol)?.File;
            if (file == null && target is IXMemberSymbol member)
                file = (member.ParentType as XSourceSymbol)?.File;
            return file;
        }

        /// <summary>
        /// The root and the projects that reference it; with <paramref name="transitive"/> also the projects that
        /// reference it through other projects. Each project once, the root first.
        /// </summary>
        internal static IList<T> ProjectsInScope<T>(T root, IEnumerable<T> all, Func<T, IEnumerable<T>> referencedProjects, bool transitive)
        {
            var result = new List<T> { root };
            var candidates = all.Where(p => !Equals(p, root)).ToList();
            bool added = true;
            while (added)
            {
                added = false;
                foreach (var project in candidates.ToList())
                {
                    var references = referencedProjects(project);
                    if (transitive ? references.Any(r => result.Contains(r)) : references.Contains(root))
                    {
                        result.Add(project);
                        candidates.Remove(project);
                        added = transitive;
                    }
                }
            }
            return result;
        }

        private static IEnumerable<XFile> FilesInScope(IXSymbol target, bool transitive)
        {
            var project = SourceFileOf(target)?.Project;
            if (project == null)
                return Enumerable.Empty<XFile>();
            var projects = ProjectsInScope(project, XSolution.Projects, p => p.ReferencedProjects, transitive);
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var result = new List<XFile>();
            foreach (var p in projects)
            {
                foreach (var path in p.SourceFiles)
                {
                    if (seen.Add(path))
                    {
                        var file = p.FindXFile(path);
                        if (file != null && file.XFileType == XFileType.SourceCode)
                            result.Add(file);
                    }
                }
            }
            return result;
        }

        private static List<ReferenceHit> SearchFile(IXSymbol target, OverrideFamily family, ReferenceSearchOptions options, string name, XFile file,
            TextInterval? limit, ReferenceDocumentSource documents, FindReferencesStats stats, CancellationToken ct)
        {
            var lateBound = (options.IncludeLateBound || options.IncludeUncertain) && CanBeLateBound(target);
            var uncertain = options.IncludeUncertain;
            var hits = new List<ReferenceHit>();
            // one text for filter, tokens and entities: an open buffer can change while we search
            var text = documents.ReadText(file);
            if (!ReferenceCandidateScanner.MayContain(text, name))
                return hits;
            var document = documents.CreateDocument(file, text);
            stats.FilesLexed++;
            var candidates = target.Kind == Kind.Constructor
                ? ReferenceCandidateScanner.FindConstructorTokens(document.Tokens, name)
                : ReferenceCandidateScanner.FindIdentifierTokens(document.Tokens, name);
            // names inside the expressions of interpolated strings: resolved on the tokens of their expression
            var allowDot = file.Project?.ParseOptions?.AllowDotForInstanceMembers ?? false;
            var interpolated = ReferenceCandidateScanner.FindInterpolatedTokens(document.Tokens, name,
                expression => ReferenceDocumentSource.Lex(file, expression), allowDot);
            var symbolLiterals = CollectsSymbolLiterals(target, options)
                ? ReferenceCandidateScanner.FindSymbolTokens(document.Tokens, name) : Array.Empty<IToken>();
            var textKinds = options.IncludeText | (uncertain ? TextTokenKinds.InactiveCode : TextTokenKinds.None);
            var inText = ReferenceCandidateScanner.FindWordInText(document.Tokens, name, textKinds);
            // the name only in a comment, a string or a longer word: no need to parse the file
            if (candidates.Count == 0 && interpolated.Count == 0 && symbolLiterals.Count == 0 && inText.Count == 0)
                return hits;
            if (candidates.Count > 0 || interpolated.Count > 0)
                ParseEntities(file, text);
            var snapshot = document.SnapShot;
            var declarationLines = new HashSet<int>();
            var lateBoundGroup = LateBoundGroup(target);
            void AddAt(int start, int length, bool isDefinition, string group,
                ReferenceUncertainty uncertainty = ReferenceUncertainty.None, TextTokenKinds textKind = TextTokenKinds.None)
            {
                var textLine = snapshot.GetLineFromPosition(start);
                hits.Add(new ReferenceHit
                {
                    FilePath = file.FullPath,
                    ProjectName = file.Project?.Name,
                    Line = textLine.LineNumber,
                    Column = start - textLine.Start.Position,
                    Length = length,
                    LineText = textLine.GetText(),
                    Classifications = ReferenceLineText.Classify(document.Tokens, textLine.Start.Position, textLine.End.Position),
                    IsDefinition = isDefinition,
                    Group = group,
                    Start = start,
                    Uncertainty = uncertainty,
                    InText = textKind
                });
            }
            void AddHit(IToken token, bool isDefinition, string group, ReferenceUncertainty uncertainty = ReferenceUncertainty.None)
            {
                AddAt(token.StartIndex, token.StopIndex - token.StartIndex + 1, isDefinition, group, uncertainty);
            }
            // one document per interpolated string, with the string replaced by the expression
            var expressionDocuments = new Dictionary<IList<IToken>, XDocument>();
            var all = candidates.Select(t => (Token: t, Hole: (InterpolatedCandidate)null))
                .Concat(interpolated.Select(c => (Token: c.Token, Hole: c)))
                .OrderBy(c => c.Token.StartIndex);
            foreach (var (token, hole) in all)
            {
                ct.ThrowIfCancellationRequested();
                if (limit.HasValue && !limit.Value.ContainsInclusive(token.StartIndex))
                    continue;
                stats.Candidates++;
                var line = token.Line - 1;
                // the name in a declaration: of the target (all parts of a partial type), or of something else
                var declared = hole == null ? DeclaredEntity(file.EntityList, token) : null;
                bool isDefinition = declared != null &&
                    (SymbolIdentity.IsSame(target, declared) || family.Contains(declared));
                IXSymbol group = declared;
                bool isReference = false;
                if (declared == null)
                {
                    var searchDocument = document;
                    if (hole != null && !expressionDocuments.TryGetValue(hole.Tokens, out searchDocument))
                    {
                        searchDocument = documents.CreateDocument(file, text, ReferenceCandidateScanner.Splice(document.Tokens, hole));
                        expressionDocuments.Add(hole.Tokens, searchDocument);
                    }
                    var tokens = searchDocument.Tokens;
                    var member = XSharpLookup.FindMemberAtPosition(token.StartIndex, file);
                    var ns = XSharpTokenTools.FindNamespace(token.StartIndex, file);
                    var location = new XSharpSearchLocation(searchDocument, file, member, searchDocument.SnapShot, line, token.StartIndex, ns?.FullName ?? "");
                    var context = location;
                    var resolved = SymbolResolver.ForCreation(SymbolResolver.ResolveAt(ref location, out _, out _), tokens, token);
                    var symbols = OverloadFilter.Filter(resolved, OverloadFilter.Arguments(tokens, token),
                        id => SymbolResolver.TypeOf(context, id));
                    if (symbols.Count == 0)
                    {
                        stats.Unresolved++;
                        // o:Name on a USUAL, an OBJECT or an unknown type: may reach the target at runtime
                        if (lateBound && ReferenceCandidateScanner.IsSend(tokens, token))
                            AddHit(token, false, lateBoundGroup, ReferenceUncertainty.LateBound);
                        else if (uncertain)
                            AddHit(token, false, UncertainGroup(ReferenceUncertainty.Unresolved, target), ReferenceUncertainty.Unresolved);
                        continue;
                    }
                    stats.Resolved++;
                    if (uncertain && IsAmbiguous(target, family, symbols))
                    {
                        AddHit(token, false, UncertainGroup(ReferenceUncertainty.AmbiguousOverload, target), ReferenceUncertainty.AmbiguousOverload);
                        continue;
                    }
                    var match = symbols.FirstOrDefault(s => SymbolIdentity.IsReferenceTo(target, s));
                    group = target;
                    if (match == null)
                    {
                        // a call that reaches an override of the target, or the member the target overrides
                        match = symbols.FirstOrDefault(s => family.Contains(s));
                        group = match;
                    }
                    isReference = match != null;
                    // locals and parameters: the first occurrence on their declaration line is the declaration
                    if (match is XSourceVariableSymbol local && local.Range.StartLine == line &&
                        !declarationLines.Contains(line))
                    {
                        declarationLines.Add(line);
                        isDefinition = true;
                    }
                }
                if (isDefinition || isReference)
                    AddHit(token, isDefinition, GroupOf(group ?? target));
            }
            // Send(o, #Name), IVarGet(o, #Name): the symbol names the member
            foreach (var token in symbolLiterals)
                AddHit(token, false, lateBoundGroup, ReferenceUncertainty.Symbol);
            // the name in comments and strings (asked for), and in excluded #ifdef code (never sure)
            foreach (var occurrence in inText)
            {
                if (limit.HasValue && !limit.Value.ContainsInclusive(occurrence.Start))
                    continue;
                if (occurrence.Kind == TextTokenKinds.InactiveCode)
                    AddAt(occurrence.Start, occurrence.Length, false, UncertainGroup(ReferenceUncertainty.InactiveCode, target),
                        ReferenceUncertainty.InactiveCode);
                else
                    AddAt(occurrence.Start, occurrence.Length, false, GroupOf(target), ReferenceUncertainty.None, occurrence.Kind);
            }
            return hits;
        }

        /// <summary>
        /// The arguments do not tell which overload is called: the target (or a member of its family) fits, and so
        /// does another member with the same name.
        /// </summary>
        internal static bool IsAmbiguous(IXSymbol target, OverrideFamily family, IList<IXSymbol> symbols)
        {
            bool Belongs(IXSymbol s) => SymbolIdentity.IsReferenceTo(target, s) || family.Contains(s);
            if (!symbols.Any(Belongs))
                return false;
            return symbols.OfType<IXMemberSymbol>().Any(m => !Belongs(m) &&
                string.Equals(m.Name, target.Name, StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>The group of the places that may refer to the target but are never changed by a rename.</summary>
        internal static string UncertainGroup(ReferenceUncertainty uncertainty, IXSymbol target)
        {
            switch (uncertainty)
            {
                case ReferenceUncertainty.LateBound:
                case ReferenceUncertainty.Symbol:
                    return LateBoundGroup(target);
                case ReferenceUncertainty.Unresolved:
                    return "Not changed: not resolved";
                case ReferenceUncertainty.AmbiguousOverload:
                    return "Not changed: ambiguous overload";
                case ReferenceUncertainty.InactiveCode:
                    return "Not changed: inactive code";
                case ReferenceUncertainty.FileNotRead:
                    return "Not changed: file not read";
            }
            return GroupOf(target);
        }

        /// <summary>
        /// The entity whose name the token is in its declaration, otherwise null. A declaration is not a call:
        /// its parameter list must not be read as arguments.
        /// </summary>
        internal static XSourceEntity DeclaredEntity(IList<XSourceEntity> entities, IToken token)
        {
            // the innermost entity that starts on the token's line
            var entity = entities?.LastOrDefault(e => e.IncludesPosition(token.StartIndex) &&
                e.Interval.Start <= token.StartIndex && LineOf(e) == token.Line);
            if (entity == null)
                return null;
            // a constructor is named by its keyword
            if (token.Type == XSharpLexer.CONSTRUCTOR)
                return entity.Kind == Kind.Constructor ? entity : null;
            var name = entity is IXTypeSymbol ? WithoutArity(entity.Name) : entity.Name;
            if (!string.Equals(name, token.Text, StringComparison.OrdinalIgnoreCase))
                return null;
            return entity;
        }

        /// <summary>The group header of a hit in the window: the declaration it belongs to.</summary>
        internal static string GroupOf(IXSymbol symbol)
        {
            // ACCESS, ASSIGN and PROPERTY with one name are one property
            var keyword = SymbolIdentity.IsPropertyLike(symbol.Kind) ? "PROPERTY" : symbol.KindKeyword;
            return $"{keyword} {symbol.FullName}".Trim();
        }
        /// <summary>
        /// Symbols #Name name members (Send, IVarGet) and, in VO code, classes and functions (CreateInstance(#Foo)).
        /// Find All References lists them for members when asked; a rename reports them for every named symbol.
        /// </summary>
        internal static bool CollectsSymbolLiterals(IXSymbol target, ReferenceSearchOptions options)
        {
            if (CanBeLateBound(target))
                return options.IncludeLateBound || options.IncludeUncertain;
            return options.IncludeUncertain && !(target is IXVariableSymbol);
        }

        /// <summary>Methods, properties and fields can be called late bound: <c>o:Name</c>, <c>#Name</c>.</summary>
        internal static bool CanBeLateBound(IXSymbol target)
        {
            return target is IXMemberSymbol &&
                (target.Kind == Kind.Method || target.Kind == Kind.Field || SymbolIdentity.IsPropertyLike(target.Kind));
        }

        /// <summary>The group of the sends and symbols that may reach the target at runtime.</summary>
        internal static string LateBoundGroup(IXSymbol target)
        {
            return $"Late bound :{target.Name} / #{target.Name}";
        }

        /// <summary>
        /// The name a file must contain: for a constructor (named .ctor) the name of its class; a generic type
        /// (stored as <c>Box`1</c>) without its arity.
        /// </summary>
        internal static string SearchName(IXSymbol target)
        {
            if (target.Kind == Kind.Constructor && target is IXMemberSymbol ctor)
            {
                var typeName = ctor.ParentType?.Name ?? ctor.DeclaringType;
                if (!string.IsNullOrEmpty(typeName))
                    return WithoutArity(typeName.Substring(typeName.LastIndexOf('.') + 1));
            }
            return target is IXTypeSymbol ? WithoutArity(target.Name) : target.Name;
        }

        /// <summary><c>Box`1</c> → <c>Box</c>: the name of a generic type as the source writes it.</summary>
        private static string WithoutArity(string name)
        {
            var tick = name?.IndexOf('`') ?? -1;
            return tick > 0 ? name.Substring(0, tick) : name;
        }

        /// <summary>1 based start line of an entity, comparable with IToken.Line (Range lines are 0 based).</summary>
        private static int LineOf(XSourceEntity entity)
        {
            return entity.Range.StartLine + 1;
        }

        /// <summary>
        /// XProject.FindXFile returns a fresh XFile without entities. XFile.Parse() would write the database and
        /// then clear the entities of a file that is not open, so parse without saving.
        /// </summary>
        private static void ParseEntities(XFile file, string text)
        {
            using (var walker = new SourceWalker(file, false))
            {
                walker.Parse(text, false);
            }
        }
    }
}
