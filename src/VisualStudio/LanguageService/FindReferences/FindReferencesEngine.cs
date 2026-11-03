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
        internal static Task<FindReferencesStats> FindAsync(IXSymbol target, XSharpSearchLocation origin,
            ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)
        {
            return Task.Run(() => Find(target, origin, documents, sink, ct), ct);
        }

        private static FindReferencesStats Find(IXSymbol target, XSharpSearchLocation origin,
            ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)
        {
            var stats = new FindReferencesStats();
            var watch = Stopwatch.StartNew();
            ModelWalker.Suspend();
            try
            {
                var name = target.Name;
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
                    files = FilesInScope(target);
                }
                SearchEach(files.Where(f => f != null), file =>
                {
                    stats.FilesInScope++;
                    var hits = SearchFile(target, name, file, limit, documents, stats, ct);
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

        private static IEnumerable<XFile> FilesInScope(IXSymbol target)
        {
            // members read from the database have no file: use the file of their type
            var targetFile = (target as XSourceSymbol)?.File;
            if (targetFile == null && target is IXMemberSymbol member)
                targetFile = (member.ParentType as XSourceSymbol)?.File;
            var project = targetFile?.Project;
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

        private static List<ReferenceHit> SearchFile(IXSymbol target, string name, XFile file, TextInterval? limit,
            ReferenceDocumentSource documents, FindReferencesStats stats, CancellationToken ct)
        {
            var hits = new List<ReferenceHit>();
            // one text for filter, tokens and entities: an open buffer can change while we search
            var text = documents.ReadText(file);
            if (!ReferenceCandidateScanner.MayContain(text, name))
                return hits;
            var document = documents.CreateDocument(file, text);
            ParseEntities(file, text);
            stats.FilesLexed++;
            var snapshot = document.SnapShot;
            var candidates = ReferenceCandidateScanner.FindIdentifierTokens(document.Tokens, name);
            var declarationLines = new HashSet<int>();
            foreach (var token in candidates)
            {
                ct.ThrowIfCancellationRequested();
                if (limit.HasValue && !limit.Value.ContainsInclusive(token.StartIndex))
                    continue;
                stats.Candidates++;
                var line = token.Line - 1;
                bool isDefinition = IsDeclaration(target, token, line, file);
                bool isReference = false;
                if (!isDefinition)
                {
                    var member = XSharpLookup.FindMemberAtPosition(token.StartIndex, file);
                    var ns = XSharpTokenTools.FindNamespace(token.StartIndex, file);
                    var location = new XSharpSearchLocation(document, file, member, snapshot, line, token.StartIndex, ns?.FullName ?? "");
                    var context = location;
                    var symbols = OverloadFilter.Filter(SymbolResolver.ResolveAt(ref location, out _, out _),
                        OverloadFilter.Arguments(document.Tokens, token), id => SymbolResolver.TypeOf(context, id));
                    if (symbols.Count == 0)
                    {
                        stats.Unresolved++;
                        continue;
                    }
                    stats.Resolved++;
                    var match = symbols.FirstOrDefault(s => SymbolIdentity.IsSame(target, s));
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
                {
                    var textLine = snapshot.GetLineFromLineNumber(line);
                    hits.Add(new ReferenceHit
                    {
                        FilePath = file.FullPath,
                        ProjectName = file.Project?.Name,
                        Line = line,
                        Column = token.StartIndex - textLine.Start.Position,
                        Length = token.StopIndex - token.StartIndex + 1,
                        LineText = textLine.GetText(),
                        Classifications = ReferenceLineText.Classify(document.Tokens, textLine.Start.Position, textLine.End.Position),
                        IsDefinition = isDefinition
                    });
                }
            }
            return hits;
        }

        /// <summary>The token is the name in a declaration of the target (all parts of a partial type).</summary>
        private static bool IsDeclaration(IXSymbol target, IToken token, int line, XFile file)
        {
            // the innermost entity that starts on the token's line
            var entity = file.EntityList.LastOrDefault(e => e.IncludesPosition(token.StartIndex) &&
                e.Interval.Start <= token.StartIndex && LineOf(file, e) == token.Line);
            if (entity == null)
                return false;
            if (!string.Equals(entity.Name, token.Text, StringComparison.OrdinalIgnoreCase))
                return false;
            return SymbolIdentity.IsSame(target, entity);
        }

        /// <summary>1 based start line of an entity, comparable with IToken.Line (Range lines are 0 based).</summary>
        private static int LineOf(XFile file, XSourceEntity entity)
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
