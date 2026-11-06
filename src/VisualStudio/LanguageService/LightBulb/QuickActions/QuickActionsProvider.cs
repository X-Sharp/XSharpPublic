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

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;
#pragma warning restore CS0067

        internal QuickActionsSource(ITextView view, ITextBuffer buffer)
        {
            _view = view;
            _buffer = buffer;
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (XEditorSettings.DisableLightBulb)
                return Task.FromResult(false);
            return Task.Run(() => Offers(range, cancellationToken)?.Any ?? false, cancellationToken);
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
            var position = range.Start.Position;
            lock (_lock)
            {
                if (_last != null && _last.Context.Version == snapshot.Version.VersionNumber && _last.Context.Position == position)
                    return _last;
            }
            try
            {
                var context = QuickActionContext.Compute(_buffer, snapshot, position, ct);
                if (context == null || ct.IsCancellationRequested)
                    return null;
                var offers = Compute(context);
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
                if (context.Name != null && context.Symbols.Count == 0)
                {
                    var name = context.Name.Text;
                    offers.UsingNamespaces = UsingCandidates.Namespaces(TypesNamed(project, name), name,
                        context.Usings.Concat(context.File.Usings ?? new List<string>()), context.CurrentNamespace, context.CaseSensitive);
                }
                if (context.Name != null)
                {
                    offers.MethodTarget = GenerateMethodRule.Target(context.Receiver, context.ReceiverType, context.EnclosingClass,
                        context.IsCall, context.Symbols.Count > 0, context.Name.Text, n => IsKnownGlobal(project, n), BaseOf, context.CaseSensitive) as XSourceTypeSymbol;
                }
                if (context.EnclosingClass != null)
                {
                    var candidates = OverrideCandidates.For(context.EnclosingClass, BaseOf, AllVirtual, context.CaseSensitive);
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

        internal static IXTypeSymbol BaseOf(IXTypeSymbol type) => type?.BaseType;

        /// <summary>Every method of a source class in a dialect with VO semantics is overridable.</summary>
        internal static bool AllVirtual(IXTypeSymbol type)
        {
            return type is XSourceTypeSymbol source && (source.File?.Project?.Dialect ?? XDialect.Core) != XDialect.Core;
        }

        private static bool IsKnownGlobal(XProject project, string name)
        {
            return project.FindFunction(name) != null || project.FindGlobalOrDefine(name) != null ||
                   project.FindFunctionsInAssemblyReferences(name).Count > 0 || project.FindGlobalsInAssemblyReferences(name).Count > 0;
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
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            telemetryId = Guid.Empty;
            return false;
        }
    }
}
