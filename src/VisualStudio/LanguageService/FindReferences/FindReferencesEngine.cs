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
        /// <paramref name="includeLateBound"/> adds the sends <c>o:Name</c> that the lookup cannot resolve and the
        /// symbols <c>#Name</c> in a group of their own.
        /// </summary>
        internal static Task<FindReferencesStats> FindAsync(IXSymbol target, OverrideFamily family, bool includeLateBound,
            XSharpSearchLocation origin, ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)
        {
            return Task.Run(() => Find(target, family, includeLateBound && CanBeLateBound(target), origin, documents, sink, ct), ct);
        }

        private static FindReferencesStats Find(IXSymbol target, OverrideFamily family, bool lateBound, XSharpSearchLocation origin,
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
                    files = FilesInScope(family.TargetRoot ?? target);
                }
                SearchEach(files.Where(f => f != null), file =>
                {
                    stats.FilesInScope++;
                    var hits = SearchFile(target, family, lateBound, name, file, limit, documents, stats, ct);
                    if (hits.Count > 0)
                    {
                        stats.Hits += hits.Count;
                        sink.Add(hits);
                    }
                }, stats, ct);
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
        /// counted in <see cref="FindReferencesStats.Failed"/>, and the search goes on; cancellation ends it.
        /// </summary>
        internal static void SearchEach<T>(IEnumerable<T> items, Action<T> search, FindReferencesStats stats, CancellationToken ct)
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

        private static IEnumerable<XFile> FilesInScope(IXSymbol target)
        {
            var project = SourceFileOf(target)?.Project;
            if (project == null)
                return Enumerable.Empty<XFile>();
            var projects = XSolution.Projects.Where(p => p == project || p.ReferencedProjects.Contains(project));
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

        private static List<ReferenceHit> SearchFile(IXSymbol target, OverrideFamily family, bool lateBound, string name, XFile file,
            TextInterval? limit, ReferenceDocumentSource documents, FindReferencesStats stats, CancellationToken ct)
        {
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
            var symbolLiterals = lateBound ? ReferenceCandidateScanner.FindSymbolTokens(document.Tokens, name) : Array.Empty<IToken>();
            // the name only in a comment, a string or a longer word: no need to parse the file
            if (candidates.Count == 0 && interpolated.Count == 0 && symbolLiterals.Count == 0)
                return hits;
            ParseEntities(file, text);
            var snapshot = document.SnapShot;
            var declarationLines = new HashSet<int>();
            var lateBoundGroup = LateBoundGroup(target);
            void AddHit(IToken token, bool isDefinition, string group)
            {
                var textLine = snapshot.GetLineFromLineNumber(token.Line - 1);
                hits.Add(new ReferenceHit
                {
                    FilePath = file.FullPath,
                    ProjectName = file.Project?.Name,
                    Line = token.Line - 1,
                    Column = token.StartIndex - textLine.Start.Position,
                    Length = token.StopIndex - token.StartIndex + 1,
                    LineText = textLine.GetText(),
                    Classifications = ReferenceLineText.Classify(document.Tokens, textLine.Start.Position, textLine.End.Position),
                    IsDefinition = isDefinition,
                    Group = group
                });
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
                            AddHit(token, false, lateBoundGroup);
                        continue;
                    }
                    stats.Resolved++;
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
                AddHit(token, false, lateBoundGroup);
            return hits;
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
            if (!string.Equals(entity.Name, token.Text, StringComparison.OrdinalIgnoreCase))
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

        /// <summary>The name a file must contain: for a constructor (named .ctor) the name of its class.</summary>
        internal static string SearchName(IXSymbol target)
        {
            if (target.Kind == Kind.Constructor && target is IXMemberSymbol ctor)
            {
                var typeName = ctor.ParentType?.Name ?? ctor.DeclaringType;
                if (!string.IsNullOrEmpty(typeName))
                    return typeName.Substring(typeName.LastIndexOf('.') + 1);
            }
            return target.Name;
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
