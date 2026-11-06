//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using XSharp.Settings;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [Name("XSharp Quick Actions")]
    [ContentType(XSharpConstants.LanguageName)]
    internal class QuickActionsProvider : ISuggestedActionsSourceProvider
    {
        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            if (XEditorSettings.DisableLightBulb || textView == null || textBuffer == null)
                return null;
            return new QuickActionsSource(textView, textBuffer);
        }
    }

    /// <summary>The actions that apply at one caret position: computed once in the background, shown on request.</summary>
    internal sealed class QuickActionOffers
    {
        internal QuickActionContext Context { get; set; }
        internal IReadOnlyList<string> UsingNamespaces { get; set; } = new string[0];
        internal XSourceTypeSymbol MethodTarget { get; set; }
        internal IReadOnlyList<IXMemberSymbol> Overrides { get; set; } = new IXMemberSymbol[0];
        internal bool Any => UsingNamespaces.Count > 0 || MethodTarget != null || Overrides.Count > 0;
    }

    internal sealed class QuickActionsSource : ISuggestedActionsSource
    {
        private readonly ITextView _view;
        private readonly ITextBuffer _buffer;
        private readonly object _lock = new object();
        private QuickActionOffers _last;
        private SnapshotPoint? _caret;

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;
#pragma warning restore CS0067

        internal QuickActionsSource(ITextView view, ITextBuffer buffer)
        {
            _view = view;
            _buffer = buffer;
            RememberCaret(view.Caret.Position);
            view.Caret.PositionChanged += CaretPositionChanged;
            view.Closed += ViewClosed;
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (XEditorSettings.DisableLightBulb)
                return Task.FromResult(false);
            return Task.Run(() => Offers(range, cancellationToken)?.Any ?? false, cancellationToken);
        }

        /// <summary>
        /// VS passes the extent of the caret line as range, not the caret, and calls the source off the UI thread,
        /// where the caret cannot be read. So the caret is remembered from its PositionChanged event (UI thread).
        /// </summary>
        private void RememberCaret(CaretPosition position)
        {
            var caret = position.Point.GetPoint(_buffer, position.Affinity);
            lock (_lock)
            {
                _caret = caret;
            }
        }

        private void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e) => RememberCaret(e.NewPosition);

        private void ViewClosed(object sender, EventArgs e)
        {
            _view.Caret.PositionChanged -= CaretPositionChanged;
            _view.Closed -= ViewClosed;
        }

        /// <summary>The remembered caret when it lies in <paramref name="range"/>, else the start of the range.</summary>
        private int PositionIn(SnapshotSpan range)
        {
            lock (_lock)
            {
                if (_caret.HasValue && _caret.Value.Snapshot == range.Snapshot &&
                    _caret.Value.Position >= range.Start.Position && _caret.Value.Position <= range.End.Position)
                    return _caret.Value.Position;
            }
            return range.Start.Position;
        }

        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (XEditorSettings.DisableLightBulb)
                return Enumerable.Empty<SuggestedActionSet>();
            var offers = Offers(range, cancellationToken);
            if (offers == null || !offers.Any)
                return Enumerable.Empty<SuggestedActionSet>();
            var sets = new List<SuggestedActionSet>();
            if (offers.UsingNamespaces.Count > 0)
                sets.Add(new SuggestedActionSet(offers.UsingNamespaces.Select(ns => (ISuggestedAction)new AddUsingAction(_view, offers.Context, ns)).ToArray()));
            if (offers.MethodTarget != null)
                sets.Add(new SuggestedActionSet(new ISuggestedAction[] { new GenerateMethodAction(_view, offers.Context, offers.MethodTarget) }));
            if (offers.Overrides.Count > 0)
                sets.Add(new SuggestedActionSet(new ISuggestedAction[] { new GenerateOverridesAction(_view, offers.Context, offers.Overrides) }));
            return sets;
        }

        /// <summary>The offers at the start of <paramref name="range"/>, from the cache when the version and position match.</summary>
        private QuickActionOffers Offers(SnapshotSpan range, CancellationToken ct)
        {
            var snapshot = range.Snapshot;
            var position = PositionIn(range);
            lock (_lock)
            {
                if (_last != null && _last.Context.Version == snapshot.Version.VersionNumber && _last.Context.Position == position)
                    return _last;
            }
            try
            {
                var context = QuickActionContext.Compute(_buffer, snapshot, position, ct);
                if (context == null || ct.IsCancellationRequested)
                {
                    if (XSettings.EnableLogging)
                        XSharp.Support.Logger.Information($"Quick actions: no context at {position} (document not parsed for version {snapshot.Version.VersionNumber}, or cancelled)");
                    return null;
                }
                var offers = Compute(context);
                if (XSettings.EnableLogging)
                {
                    XSharp.Support.Logger.Information($"Quick actions: range {range.Start.Position}-{range.End.Position}, caret {position}, name '{context.Name?.Text}', " +
                        $"receiver {context.Receiver}, symbols {context.Symbols.Count} [{string.Join(",", context.Symbols.Select(sym => sym.GetType().Name + ":" + sym.Kind))}], class '{context.EnclosingClass?.FullName}', " +
                        $"usings {offers.UsingNamespaces.Count}, method target '{offers.MethodTarget?.Name}', overrides {offers.Overrides.Count}");
                }
                lock (_lock)
                {
                    _last = offers;
                }
                return offers;
            }
            catch (OperationCanceledException)
            {
                return null;
            }
            catch (Exception e)
            {
                XSharp.Support.Logger.Exception(e, "Quick actions");
                return null;
            }
        }

        private static QuickActionOffers Compute(QuickActionContext context)
        {
            var offers = new QuickActionOffers { Context = context };
            var project = context.Project;
            ModelWalker.Suspend();
            try
            {
                if (context.Name != null && UsingCandidates.Applies(context.Receiver, context.Symbols))
                {
                    var name = context.Name.Text;
                    offers.UsingNamespaces = UsingCandidates.Namespaces(TypesNamed(project, name), name,
                        context.Usings.Concat(context.File.Usings ?? new List<string>()), context.CurrentNamespace, context.CaseSensitive);
                }
                if (context.Name != null)
                {
                    offers.MethodTarget = GenerateMethodRule.Target(context.Receiver, context.ReceiverType, context.EnclosingClass,
                        context.IsCall, GenerateMethodRule.IsResolved(context.Symbols), context.Name.Text, n => IsKnownGlobal(project, n), BaseOfIn(context), context.CaseSensitive) as XSourceTypeSymbol;
                }
                if (context.EnclosingClass != null)
                {
                    var candidates = OverrideCandidates.For(context.EnclosingClass, BaseOfIn(context), AllVirtual, context.CaseSensitive);
                    if (OverrideCandidates.WorthOffering(candidates))
                        offers.Overrides = candidates;
                }
            }
            finally
            {
                ModelWalker.Resume();
            }
            return offers;
        }

        /// <summary>
        /// The base of the enclosing class from its part in this file when the merged type cannot resolve it (the
        /// merged type comes from the database without the USINGs of the file).
        /// </summary>
        private static Func<IXTypeSymbol, IXTypeSymbol> BaseOfIn(QuickActionContext context)
        {
            return type =>
            {
                var result = BaseOf(type);
                if (result == null && type == context.EnclosingClass && context.EnclosingPart != null && context.EnclosingPart != type)
                    result = BaseOf(context.EnclosingPart);
                return result;
            };
        }

        /// <summary>BaseType is only set by ForceComplete (UI thread): resolve it like Find References does.</summary>
        internal static IXTypeSymbol BaseOf(IXTypeSymbol type) => type == null ? null : FindReferences.OverrideFamily.ResolveBase(type);

        /// <summary>Every method of a source class in a dialect with VO semantics is overridable.</summary>
        internal static bool AllVirtual(IXTypeSymbol type)
        {
            return type is XSourceTypeSymbol source && (source.File?.Project?.Dialect ?? XDialect.Core) != XDialect.Core;
        }

        private static bool IsKnownGlobal(XProject project, string name)
        {
            // the lookups throw instead of returning null when nothing is found
            return Found(() => project.FindFunction(name) != null) || Found(() => project.FindGlobalOrDefine(name) != null) ||
                   Found(() => project.FindFunctionsInAssemblyReferences(name)?.Count > 0) ||
                   Found(() => project.FindGlobalsInAssemblyReferences(name)?.Count > 0);
        }

        private static bool Found(Func<bool> lookup)
        {
            try
            {
                return lookup();
            }
            catch (Exception)
            {
                return false;
            }
        }

        /// <summary>The (namespace, name) of the types named <paramref name="name"/> in the project, the projects it references and its assemblies.</summary>
        private static IEnumerable<(string Namespace, string Name)> TypesNamed(XProject project, string name)
        {
            var result = new List<(string Namespace, string Name)>();
            foreach (var row in XDatabase.GetProjectTypes(name, project.DependentProjectList))
                result.Add((row.Namespace, row.TypeName));
            var assemblyIds = new HashSet<long>((project.DependentAssemblyList ?? "")
                .Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries)
                .Select(s => long.TryParse(s.Trim(), out var id) ? id : -1));
            if (assemblyIds.Count > 0)
            {
                // the query does not filter by assembly for a match on the simple name: filter here
                foreach (var row in XDatabase.GetAssemblyTypes(name, project.DependentAssemblyList))
                {
                    if (assemblyIds.Contains(row.IdAssembly))
                        result.Add((row.Namespace, row.TypeName));
                }
            }
            return result;
        }

        public void Dispose()
        {
            _view.Caret.PositionChanged -= CaretPositionChanged;
            _view.Closed -= ViewClosed;
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            telemetryId = Guid.Empty;
            return false;
        }
    }
}
